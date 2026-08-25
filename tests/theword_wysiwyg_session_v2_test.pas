unit theword_wysiwyg_session_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTheWordWysiwygSessionV2Test = class(TTestCase)
  published
    procedure AppliesSevenSemanticsWithoutExposingTagsAsText;
    procedure UndoRedoRestoresOntSnapshots;
    procedure TextReplacementUsesUTF16Offsets;
    procedure FullVisibleDeletionRemovesPresentationWrappersAndIsUndoable;
    procedure PartialDeletionPrunesOnlyFullyCoveredNonemptyWrappers;
    procedure FullVisibleDeletionRejectsProtectedOntObjectsAtomically;
    procedure RejectedReplacementDoesNotPolluteUndo;
    procedure OpaqueAndLexicalObjectsProtectTextEdits;
    procedure VisualAdapterCombinesOverlappingStyles;
    procedure VisualAdapterRendersScriptAndStrikeStyles;
    procedure VisualAdapterRendersHtmlSemanticStyles;
    procedure InsertionStyleMatchesSemanticReplacementBoundaries;
    procedure VisualAdapterRendersImportedFontColorsAndPreservesRaw;
    procedure InsertionStyleMatchesFirstClassFontAnchors;
    procedure VisualAdapterRendersOldTestamentQuoteBoldBlack;
    procedure VisualAdapterMapsSyntheticProtectedSeparators;
    procedure ParagraphMarkIsSyntheticMappedAndNonInteractive;
    procedure ParagraphMarkEditingPreservesRawUndoAndProvenance;
    procedure TitleRendersAsProtectedChapterHeading;
    procedure TitleCannotBeEditedFormattedOrCutVisually;
    procedure TitleCommandsAreAtomicUndoableAndPreserveBodyTrace;
    procedure TitleWrapUnwrapArePresentationOnlyAndUndoable;
    procedure TitleWrapPartialFormattingIsOneUndoCommand;
    procedure TitleFormattingSupportsSemanticColorAndSize;
    procedure TitlePendingInsertionFormattingIsAtomic;
    procedure TitleWrapRejectsCrossingAndProtectedContent;
    procedure MultipleTitleLevelsAndMalformedTitleRemainExplicit;
    procedure TerminalEmptyTitleKeepsOffsetMapReversible;
    procedure ProtectedAnchorsAllowAdjacentButNotCrossingEdits;
    procedure RawFallbackReentersTheSameCodecSession;
    procedure SemanticToggleRemovesAnySliceForAllKinds;
    procedure SemanticToggleAppliesAcrossMixedCoverageAndMerges;
    procedure SemanticToggleHandlesAdjacentNestedAndCrossingMarks;
    procedure SemanticToggleUsesUtf16BoundariesAtomically;
    procedure SemanticToggleRejectsTitleAndAllowsBodyBoundary;
    procedure SemanticToggleUndoRedoPreservesOntObjects;
    procedure NonSemanticEventOrderControlsSemanticOnlyClassification;
    procedure RejectedSemanticToggleCannotBeRedone;
    procedure VisualEditsProduceExactOriginTrace;
    procedure SemanticToggleDoesNotChangeOriginTrace;
    procedure UndoRedoRestoresOriginTrace;
    procedure CutAndPasteTrackDeletedAndInsertedUnits;
    procedure RawVisibleChangeUsesConservativeTrace;
    procedure IdenticalRetypeRemainsDirtyUntilUndo;
    procedure RawProtectsLexicalObjectsButAllowsExternalEdits;
    procedure TranslatorNoteCommandsAreAtomicAndPreserveProvenance;
    procedure TranslatorNoteMarkerResolvesSavedDraftWithStableId;
    procedure TranslatorNoteAnchorsProtectTextEditing;
    procedure VisualAdapterResolvesNestedFontDimensions;
    procedure VisualAdapterComposesTitleAndRelativeFontSizes;
    procedure FontAndSemanticCrossingsStayBalanced;
    procedure FontOnlyRawChangeIsSemanticPresentation;
    procedure FontColorSelectionPreservesTopologyAndIsAtomicPresentation;
    procedure RelativeFontSizeIsAtomicUndoableAndProtected;
  end;

implementation

uses
  Graphics, text_direction, text_edit_provenance, verse_visual_document,
  theword_ont_codec_v2,
  theword_verse_visual_adapter_v2, theword_wysiwyg_session_v2;

function SemanticMask(const ARaw: UnicodeString;
  AKind: TTheWordSemanticKind): UnicodeString;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Mark: TTheWordSemanticMark;
  MarkIndex, Offset: Integer;
begin
  Result := '';
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(ARaw);
  try
    SetLength(Result, Length(Document.VisibleText));
    for Offset := 1 to Length(Result) do
      Result[Offset] := '0';
    for MarkIndex := 0 to Document.MarkCount - 1 do
    begin
      Mark := Document.Marks[MarkIndex];
      if Mark.Kind <> AKind then
        Continue;
      for Offset := Mark.StartOffset to Mark.EndOffset - 1 do
        Result[Offset + 1] := '1';
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  AppliesSevenSemanticsWithoutExposingTagsAsText;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('abcdefg');
  try
    AssertTrue(Session.ApplySemantic(twsmItalic, 0, 1));
    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 1, 1));
    AssertTrue(Session.ApplySemantic(twsmOldTestamentQuote, 2, 1));
    AssertTrue(Session.ApplySemantic(twsmUnderline, 3, 1));
    AssertTrue(Session.ApplySemantic(twsmStrikethrough, 4, 1));
    AssertTrue(Session.ApplySemantic(twsmSuperscript, 5, 1));
    AssertTrue(Session.ApplySemantic(twsmSubscript, 6, 1));
    AssertEquals(UnicodeString('abcdefg'), Session.VisibleText);
    AssertEquals(UnicodeString('<FI>a<Fi><FR>b<Fr><FO>c<Fo><FU>d<Fu>' +
      '<s>e</s><sup>f</sup><sub>g</sub>'), Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.UndoRedoRestoresOntSnapshots;
const
  ORIGINAL: UnicodeString = '<FI>ab<Fi><x data="1">cd';
var
  Session: TTheWordWysiwygSessionV2;
  Changed: UnicodeString;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL);
  try
    AssertTrue(Session.ApplySemantic(twsmUnderline, 0, 2));
    Changed := Session.RawText;
    AssertTrue(Pos('<x data="1">', Changed) > 0);
    AssertTrue(Session.Undo);
    AssertEquals(ORIGINAL, Session.RawText);
    AssertTrue(Session.Redo);
    AssertEquals(Changed, Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.TextReplacementUsesUTF16Offsets;
var
  Session: TTheWordWysiwygSessionV2;
  Emoji: UnicodeString;
begin
  Emoji := UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00));
  Session := TTheWordWysiwygSessionV2.Create('abcd');
  try
    AssertTrue(Session.ReplaceText(1, 2, Emoji));
    AssertEquals(UnicodeString('a') + Emoji + 'd', Session.VisibleText);
    AssertEquals(4, Length(Session.VisibleText));
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('abcd'), Session.VisibleText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  FullVisibleDeletionRemovesPresentationWrappersAndIsUndoable;
const
  ORIGINAL: UnicodeString =
    '<FI>Teste <i>com</i> <font color=red size=+1>' +
    '<b>formatação</b></font>.<Fi>';
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL);
  try
    AssertTrue('deleting all visible text must be accepted',
      Session.ReplaceText(0, Length(Session.VisibleText), ''));
    AssertEquals('the visual surface is empty', UnicodeString(''),
      Session.VisibleText);
    AssertEquals('fully-covered presentation tags must not survive',
      UnicodeString(''), Session.RawText);

    AssertTrue('full deletion creates one undoable edit', Session.Undo);
    AssertEquals('undo restores text and original tag spelling', ORIGINAL,
      Session.RawText);
    AssertTrue('the deletion can be redone', Session.Redo);
    AssertEquals('redo removes text and wrappers again', UnicodeString(''),
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  PartialDeletionPrunesOnlyFullyCoveredNonemptyWrappers;
const
  PARTIAL_RAW: UnicodeString = '<FI>abcd<Fi>';
  EXACT_RAW: UnicodeString = '<FI>ab<Fi>';
  EMPTY_MARKS_RAW: UnicodeString =
    '<FU><Fu>a<FI><Fi>bc<font color=red></font>' +
    'd<font size=+1></font>';
  EMPTY_MARKS_AFTER_DELETE: UnicodeString =
    '<FU><Fu>a<FI><Fi><font color=red></font>' +
    'd<font size=+1></font>';
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(PARTIAL_RAW);
  try
    AssertTrue('partial deletion inside FI must be accepted',
      Session.ReplaceText(1, 2, ''));
    AssertEquals('the remaining FI range stays formatted',
      UnicodeString('<FI>ad<Fi>'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(EXACT_RAW);
  try
    AssertTrue('deleting the exact FI payload must be accepted',
      Session.ReplaceText(0, Length(Session.VisibleText), ''));
    AssertEquals('a newly emptied FI wrapper is removed', UnicodeString(''),
      Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(EMPTY_MARKS_RAW);
  try
    AssertTrue('deletion between original empty marks must be accepted',
      Session.ReplaceText(1, 2, ''));
    AssertEquals('original empty marks outside and on both boundaries remain',
      EMPTY_MARKS_AFTER_DELETE, Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  FullVisibleDeletionRejectsProtectedOntObjectsAtomically;
const
  NOTE_RAW: UnicodeString = '<FI>a<RF>note<Rf>b<Fi>';
  LEXICAL_RAW: UnicodeString = 'word<WG123><WTN-NSM><wt>';
  OPAQUE_RAW: UnicodeString = 'a<unknown x="1">b';
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(NOTE_RAW);
  try
    AssertFalse('a full deletion cannot cross an RF anchor',
      Session.ReplaceText(0, Length(Session.VisibleText), ''));
    AssertEquals('RF rejection is atomic', NOTE_RAW, Session.RawText);
    AssertFalse('RF rejection creates no undo entry', Session.CanUndo);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(LEXICAL_RAW);
  try
    AssertFalse('a full deletion cannot remove a protected lexical word',
      Session.ReplaceText(0, Length(Session.VisibleText), ''));
    AssertEquals('lexical rejection is atomic', LEXICAL_RAW,
      Session.RawText);
    AssertFalse('lexical rejection creates no undo entry', Session.CanUndo);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(OPAQUE_RAW);
  try
    AssertFalse('a full deletion cannot cross an opaque ONT object',
      Session.ReplaceText(0, Length(Session.VisibleText), ''));
    AssertEquals('opaque rejection is atomic', OPAQUE_RAW, Session.RawText);
    AssertFalse('opaque rejection creates no undo entry', Session.CanUndo);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  RejectedReplacementDoesNotPolluteUndo;
var
  Session: TTheWordWysiwygSessionV2;
  Malformed: UnicodeString;
  Rejected: Boolean;
begin
  Malformed := UnicodeString(WideChar($D800));
  Session := TTheWordWysiwygSessionV2.Create('abcd');
  try
    Rejected := False;
    try
      Session.ReplaceText(1, 0, Malformed);
    except
      on E: ETheWordOntCodec do
        Rejected := True;
    end;
    AssertTrue('malformed UTF-16 replacement must be rejected', Rejected);
    AssertEquals(UnicodeString('abcd'), Session.RawText);
    AssertFalse('a rejected edit must not create undo history', Session.CanUndo);
  finally
    Session.Free;
  end;
end;
procedure TTheWordWysiwygSessionV2Test.
  OpaqueAndLexicalObjectsProtectTextEdits;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('a<unknown x="1"><WG123>b');
  try
    AssertFalse(Session.CanReplaceText(1, 0));
    AssertFalse(Session.ReplaceText(0, 2, 'z'));
    AssertEquals(UnicodeString('ab'), Session.VisibleText);
    AssertEquals(UnicodeString('a<unknown x="1"><WG123>b'),
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterCombinesOverlappingStyles;
var
  Codec: ITheWordOntCodecV2;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
  Style: TVisualTextStyle;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse('<FI>ab<Fi>cd');
  try
    Source.AddSemanticMark(twsmWordsOfJesus, 1, 4);
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 7);
    AssertEquals(UnicodeString('abcd'), Visual.Text);
    AssertEquals(QWord(7), Visual.Revision);
    AssertTrue(Visual.SpanCount >= 3);
    Style := Visual.Spans[1].Style;
    AssertTrue(vtsfItalic in Style.Flags);
    AssertTrue(Style.HasForegroundColor);
    AssertEquals(LongInt(clRed), Style.ForegroundColor);
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterRendersScriptAndStrikeStyles;
var
  Codec: ITheWordOntCodecV2;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(
    '<s><sup>a</sup></s><sub>b</sub>');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 8);
    AssertEquals(UnicodeString('ab'), Visual.Text);
    AssertEquals('script tags split the visual runs', 2, Visual.SpanCount);
    AssertTrue('the first run is struck',
      vtsfStrikethrough in Visual.Spans[0].Style.Flags);
    AssertTrue('the first run is superscript',
      vtsfSuperscript in Visual.Spans[0].Style.Flags);
    AssertFalse('the first run is not subscript',
      vtsfSubscript in Visual.Spans[0].Style.Flags);
    AssertTrue('the second run is subscript',
      vtsfSubscript in Visual.Spans[1].Style.Flags);
    AssertFalse('the second run is not superscript',
      vtsfSuperscript in Visual.Spans[1].Style.Flags);
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterRendersHtmlSemanticStyles;
var
  Codec: ITheWordOntCodecV2;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(
    '<B>bold</b><I>italic</i><U>under</u>');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 9);
    AssertEquals(UnicodeString('bolditalicunder'), Visual.Text);
    AssertEquals('HTML semantic tags produce three visual runs', 3,
      Visual.SpanCount);
    AssertTrue('HTML bold maps to vtsfBold',
      vtsfBold in Visual.Spans[0].Style.Flags);
    AssertEquals(UnicodeString('html-bold'), Visual.Spans[0].Annotation);
    AssertTrue('HTML italic maps to vtsfItalic',
      vtsfItalic in Visual.Spans[1].Style.Flags);
    AssertEquals(UnicodeString('html-italic'), Visual.Spans[1].Annotation);
    AssertTrue('HTML underline maps to vtsfUnderline',
      vtsfUnderline in Visual.Spans[2].Style.Flags);
    AssertEquals(UnicodeString('html-underline'), Visual.Spans[2].Annotation);
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  InsertionStyleMatchesSemanticReplacementBoundaries;
const
  RAW: UnicodeString = '<sup>ab</sup>X<sub>cd</sub>';
var
  Session: TTheWordWysiwygSessionV2;
  Style: TVisualTextStyle;
begin
  Session := TTheWordWysiwygSessionV2.Create(RAW);
  try
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Session.Document, 0,
      Style));
    AssertFalse('sup start inserts outside the tag',
      vtsfSuperscript in Style.Flags);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Session.Document, 1,
      Style));
    AssertTrue('strict interior inherits superscript',
      vtsfSuperscript in Style.Flags);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Session.Document, 2,
      Style));
    AssertFalse('sup end inserts outside the tag',
      vtsfSuperscript in Style.Flags);

    AssertTrue(TryResolveTheWordInsertionVisualStyle(Session.Document, 3,
      Style));
    AssertFalse('sub start inserts outside the tag',
      vtsfSubscript in Style.Flags);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Session.Document, 4,
      Style));
    AssertTrue('strict interior inherits subscript',
      vtsfSubscript in Style.Flags);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Session.Document, 5,
      Style));
    AssertFalse('sub end inserts outside the tag',
      vtsfSubscript in Style.Flags);

    AssertTrue(Session.ReplaceText(1, 0, 'S'));
    AssertEquals('the same strict-interior rule formats typed text',
      UnicodeString('<sup>aSb</sup>X<sub>cd</sub>'), Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterRendersImportedFontColorsAndPreservesRaw;
const
  REAL_RAW: UnicodeString =
    'os quais <font color=green>(tais que)</font> justica pagarao ' +
    '<font color=green>(sofrerao punicao)</font> destruicao';
  NESTED_RAW: UnicodeString =
    '<font color=red>A<font color=green>B</font>C</font>' +
    '<font color="#123456">D</font>';
var
  Codec: ITheWordOntCodecV2;
  Source: TTheWordVerseDocument;
  Span: TVisualSpan;
  Visual: IVerseVisualDocument;

  procedure AssertColorAt(const AText: UnicodeString;
    AExpectedColor: LongInt);
  var
    Offset: Integer;
  begin
    Offset := Pos(AText, Visual.Text) - 1;
    AssertTrue('colored text exists in the visual document', Offset >= 0);
    AssertTrue('colored text resolves to a visual span',
      Visual.TryGetSpanAt(Offset, Span));
    AssertTrue('font color becomes an explicit visual foreground',
      Span.Style.HasForegroundColor);
    AssertEquals('font color keeps its TColor/BGR value', AExpectedColor,
      Span.Style.ForegroundColor);
  end;

begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(REAL_RAW);
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 23);
    AssertColorAt('(tais que)', LongInt(clGreen));
    AssertColorAt('(sofrerao punicao)', LongInt(clGreen));
    AssertEquals('rendering never canonicalizes imported font lexemes',
      REAL_RAW, Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse(NESTED_RAW);
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 24);
    AssertColorAt('A', LongInt(clRed));
    AssertColorAt('B', LongInt(clGreen));
    AssertColorAt('C', LongInt(clRed));
    AssertColorAt('D', LongInt($00563412));
    AssertEquals('nested and hexadecimal font tags remain byte-for-byte raw',
      NESTED_RAW, Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  InsertionStyleMatchesFirstClassFontAnchors;
const
  RAW: UnicodeString = '<font color=red>ab</font>';
var
  Session: TTheWordWysiwygSessionV2;
  Style: TVisualTextStyle;
begin
  Session := TTheWordWysiwygSessionV2.Create(RAW);
  try
    AssertTrue(TryResolveTheWordInsertionVisualStyle(
      Session.Document, 0, Style));
    AssertFalse('OPEN anchor does not leak its following color',
      Style.HasForegroundColor);
    AssertTrue('typing exactly at a first-class OPEN anchor is editable',
      Session.CanReplaceText(0, 0));
    AssertTrue('ReplaceText accepts a first-class OPEN anchor',
      Session.ReplaceText(0, 0, 'X'));
    AssertEquals('OPEN insertion stays outside the font range',
      'X' + RAW, Session.RawText);

    AssertTrue(TryResolveTheWordInsertionVisualStyle(
      Session.Document, 2, Style));
    AssertTrue('strictly interior caret inherits the font color',
      Style.HasForegroundColor);
    AssertEquals('strictly interior caret is red', LongInt(clRed),
      Style.ForegroundColor);
    AssertTrue('typing strictly inside the font range is editable',
      Session.CanReplaceText(2, 0));
    AssertTrue(Session.ReplaceText(2, 0, 'X'));
    AssertEquals('interior insertion is serialized inside the original tags',
      'X<font color=red>aXb</font>', Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(RAW);
  try
    AssertTrue(TryResolveTheWordInsertionVisualStyle(
      Session.Document, 2, Style));
    AssertFalse('CLOSE anchor does not inherit its preceding color',
      Style.HasForegroundColor);
    AssertTrue('typing exactly at a first-class CLOSE anchor is editable',
      Session.CanReplaceText(2, 0));
    AssertTrue('ReplaceText accepts a first-class CLOSE anchor',
      Session.ReplaceText(2, 0, 'X'));
    AssertEquals('CLOSE insertion stays outside the font range',
      '<font color=red>ab</font>X', Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterRendersOldTestamentQuoteBoldBlack;
const
  FO_ONLY: UnicodeString = '<FO>ab<Fo>';
  COMBINED: UnicodeString = '<FO><FI><FU>ab<Fu><Fi><Fo>';
  FO_OUTSIDE_FR: UnicodeString = '<FO><FR>ab<Fr><Fo>';
  FR_OUTSIDE_FO: UnicodeString = '<FR><FO>ab<Fo><Fr>';
var
  Codec: ITheWordOntCodecV2;
  Source: TTheWordVerseDocument;
  Style: TVisualTextStyle;
  Visual: IVerseVisualDocument;

  procedure AssertVisualStyle(const ARaw: UnicodeString;
    AExpectedColor: TColor; const AExpectedFlags: TVisualTextStyleFlags);
  begin
    Source := Codec.DecodeVerse(ARaw);
    try
      Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 8);
      AssertEquals(UnicodeString('ab'), Visual.Text);
      AssertTrue('semantic span has an explicit foreground color',
        Visual.Spans[0].Style.HasForegroundColor);
      AssertEquals('semantic foreground color', LongInt(AExpectedColor),
        Visual.Spans[0].Style.ForegroundColor);
      Style := Visual.Spans[0].Style;
      AssertTrue('semantic style flags', Style.Flags = AExpectedFlags);
      AssertTrue('strict-interior insertion style resolves',
        TryResolveTheWordInsertionVisualStyle(Source, 1, Style));
      AssertTrue('insertion semantic style flags',
        Style.Flags = AExpectedFlags);
      AssertTrue('insertion has an explicit semantic foreground',
        Style.HasForegroundColor);
      AssertEquals('insertion semantic foreground',
        LongInt(AExpectedColor), Style.ForegroundColor);
      AssertEquals('visualization preserves the original ONT', ARaw,
        Codec.EncodeVerse(Source));
    finally
      Source.Free;
      Source := nil;
      Visual := nil;
    end;
  end;

begin
  Codec := TTheWordOntCodecV2.Create;
  AssertVisualStyle(FO_ONLY, clBlack, [vtsfBold]);
  AssertVisualStyle(COMBINED, clBlack,
    [vtsfBold, vtsfItalic, vtsfUnderline]);
  { FR supplies the color where it overlaps FO, while FO still supplies
    bold. The result must not depend on which wrapper is outermost. }
  AssertVisualStyle(FO_OUTSIDE_FR, clRed, [vtsfBold]);
  AssertVisualStyle(FR_OUTSIDE_FO, clRed, [vtsfBold]);
end;

procedure TTheWordWysiwygSessionV2Test.
  TitleRendersAsProtectedChapterHeading;
const
  RAW: UnicodeString = '<TS>Prefácio<Ts>Revelação';
var
  Codec: ITheWordOntCodecV2;
  DocumentLength, DocumentOffset, LayoutOffset, TitleLength: Integer;
  Source: TTheWordVerseDocument;
  Span: TVisualSpan;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(RAW);
  try
    TitleLength := Length(UnicodeString('Prefácio'));
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 11);
    AssertEquals(UnicodeString('Prefácio') + #10 + 'Revelação',
      Visual.Text);
    Span := Visual.Spans[0];
    AssertTrue('simple title text is interactive', Span.Interactive);
    AssertEquals('title has a dedicated visual role', Ord(vsrTitleContent),
      Ord(Span.Role));
    AssertTrue('default title is bold', vtsfBold in Span.Style.Flags);
    AssertEquals('default title grows by two points', 2,
      Span.Style.FontSizeDelta);
    AssertTrue('default title uses chapter heading color',
      Span.Style.HasForegroundColor);
    AssertEquals('default title annotation', UnicodeString('title-level-1'),
      Span.Annotation);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, TitleLength,
      twoaBefore, LayoutOffset));
    AssertEquals('title closes before synthetic newline', TitleLength,
      LayoutOffset);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, TitleLength,
      twoaAfter, LayoutOffset));
    AssertEquals('body begins after synthetic newline', TitleLength + 1,
      LayoutOffset);
    AssertTrue(TryTheWordLayoutRangeToDocument(Visual, TitleLength + 1,
      Length(UnicodeString('Revelação')), DocumentOffset, DocumentLength));
    AssertEquals('body document offset follows title text', TitleLength,
      DocumentOffset);
    AssertEquals(Length(UnicodeString('Revelação')), DocumentLength);
    AssertEquals('visualization never changes raw title', RAW,
      Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TitleCannotBeEditedFormattedOrCutVisually;
const
  RAW: UnicodeString = '<TS>Prefácio<Ts>Revelação';
var
  InsertedLength: Integer;
  Payload: string;
  Plain: UnicodeString;
  Session: TTheWordWysiwygSessionV2;
  TitleLength: Integer;
  UsedPrivate: Boolean;
begin
  TitleLength := Length(UnicodeString('Prefácio'));
  Session := TTheWordWysiwygSessionV2.Create(RAW);
  try
    AssertFalse('title text is protected from replacement',
      Session.CanReplaceText(0, TitleLength));
    AssertTrue('title text can receive WYSIWYG presentation formatting',
      Session.CanApplySemantic(0, TitleLength));
    AssertTrue(Session.ApplySemantic(twsmItalic, 0, TitleLength));
    AssertEquals(UnicodeString('<TS><FI>Prefácio<Fi><Ts>Revelação'),
      Session.RawText);
    AssertTrue(Session.Undo);
    AssertEquals(RAW, Session.RawText);
    AssertFalse('visual Cut cannot remove a title',
      Session.CutSelection(0, TitleLength, Plain, Payload));
    AssertFalse('rejected Cut creates no undo entry', Session.CanUndo);
    AssertTrue('first body word can be copied without its closing title tag',
      Session.CopySelection(TitleLength, Length(UnicodeString('Revelação')),
        Plain, Payload));
    AssertEquals(UnicodeString('Revelação'), Plain);
    AssertEquals('clipboard payload excludes structural title lexemes', 0,
      Pos('<Ts>', Payload));
    AssertTrue('first body character can be cut without deleting Ts',
      Session.CutSelection(TitleLength, 1, Plain, Payload));
    AssertEquals(UnicodeString('<TS>Prefácio<Ts>evelação'), Session.RawText);
    AssertTrue(Session.Undo);
    AssertEquals(RAW, Session.RawText);
    AssertTrue('paste at the body boundary keeps Ts before inserted text',
      Session.Paste(TitleLength, 0, '', 'X', InsertedLength,
        UsedPrivate));
    AssertEquals(1, InsertedLength);
    AssertFalse(UsedPrivate);
    AssertEquals(UnicodeString('<TS>Prefácio<Ts>XRevelação'),
      Session.RawText);
    AssertTrue(Session.Undo);
    AssertEquals(RAW, Session.RawText);
    AssertTrue('body is editable immediately after closing Ts',
      Session.CanReplaceText(TitleLength, 1));
    AssertTrue('body accepts semantic formatting',
      Session.ApplySemantic(twsmItalic, TitleLength,
        Length(UnicodeString('Revelação'))));
    AssertEquals(UnicodeString('<TS>Prefácio<Ts><FI>Revelação<Fi>'),
      Session.RawText);
    AssertTrue('semantic-only body edit keeps existing Associations safe',
      Session.ChangesAreSemanticOnly);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TitleCommandsAreAtomicUndoableAndPreserveBodyTrace;
var
  CurrentStart: Integer;
  Session: TTheWordWysiwygSessionV2;
  Title: TTheWordTitleRange;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create(
    'left<TS1>Head<Ts>right');
  try
    AssertTrue(Session.TryGetTitle(0, Title));
    AssertTrue(Title.Editable);
    AssertTrue(Session.ReplaceTitleText(Title.OpenOrdinal, 0, 4,
      'Heading'));
    AssertEquals(UnicodeString('left<TS1>Heading<Ts>right'),
      Session.RawText);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('unchanged body suffix keeps its origin after title growth',
        Trace.OriginMap.TryProjectOriginalRange(8, 5, CurrentStart));
      AssertEquals(11, CurrentStart);
    finally
      Trace.Free;
    end;
    AssertTrue('one title text edit creates one undo command',
      Session.Undo);
    AssertEquals(UnicodeString('left<TS1>Head<Ts>right'),
      Session.RawText);
    AssertFalse(Session.CanUndo);
    AssertTrue(Session.Redo);
    AssertEquals(UnicodeString('left<TS1>Heading<Ts>right'),
      Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    'left<TS1>Head<Ts>right');
  try
    AssertTrue(Session.TryGetTitle(0, Title));
    AssertTrue(Session.ChangeTitleLevel(Title.OpenOrdinal, 2));
    AssertEquals(UnicodeString('left<TS2>Head<Ts>right'),
      Session.RawText);
    AssertTrue('level-only change is presentation and keeps Associations',
      Session.ChangesAreSemanticOnly);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('level-only change leaves body origins untouched',
        Trace.IsBaselineIdentity);
    finally
      Trace.Free;
    end;
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('left<TS1>Head<Ts>right'),
      Session.RawText);
    AssertTrue(Session.Redo);
    AssertEquals(UnicodeString('left<TS2>Head<Ts>right'),
      Session.RawText);
    AssertTrue(Session.TryGetTitle(0, Title));
    AssertTrue(Session.RemoveTitle(Title.OpenOrdinal));
    AssertEquals(UnicodeString('left right'), Session.RawText);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('body suffix survives removal of the title',
        Trace.OriginMap.TryProjectOriginalRange(8, 5, CurrentStart));
      AssertEquals(5, CurrentStart);
    finally
      Trace.Free;
    end;
    AssertTrue('remove is one undo command', Session.Undo);
    AssertEquals(UnicodeString('left<TS2>Head<Ts>right'),
      Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('left right');
  try
    AssertTrue(Session.CanInsertTitle(5));
    AssertTrue(Session.InsertTitle(5, 3, 'Heading'));
    AssertEquals(UnicodeString('left <TS3>Heading<Ts>right'),
      Session.RawText);
    AssertTrue(Session.TryGetTitle(0, Title));
    AssertEquals(3, Title.Level);
    AssertTrue('insert is one undo command', Session.Undo);
    AssertEquals(UnicodeString('left right'), Session.RawText);
    AssertFalse(Session.CanUndo);
    AssertTrue(Session.Redo);
    AssertEquals(UnicodeString('left <TS3>Heading<Ts>right'),
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TitleWrapUnwrapArePresentationOnlyAndUndoable;
const
  ORIGINAL: UnicodeString = '<FI>Head<Fi> body';
  WRAPPED: UnicodeString = '<TS2><FI>Head<Fi><Ts> body';
var
  OpenOrdinal: Integer;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 23);
  try
    AssertTrue(Session.CanWrapTitle(0, 4));
    AssertTrue(Session.WrapTitle(0, 4, 2, OpenOrdinal));
    AssertTrue(OpenOrdinal >= 0);
    AssertEquals(WRAPPED, Session.RawText);
    AssertEquals(UnicodeString('Head body'), Session.VisibleText);
    AssertTrue('title wrapping is presentation-only',
      Session.ChangesAreSemanticOnly);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('wrapping preserves the origin trace identity',
        Trace.IsBaselineIdentity);
    finally
      Trace.Free;
    end;

    AssertTrue(Session.Undo);
    AssertEquals(ORIGINAL, Session.RawText);
    AssertFalse('wrap is exactly one undo command', Session.CanUndo);
    AssertTrue(Session.Redo);
    AssertEquals(WRAPPED, Session.RawText);
    AssertTrue(Session.UnwrapTitle(OpenOrdinal));
    AssertEquals('unwrap retains title content and formatting', ORIGINAL,
      Session.RawText);
    AssertTrue('unwrap is also presentation-only',
      Session.ChangesAreSemanticOnly);
    AssertTrue(Session.Undo);
    AssertEquals('unwrap is exactly one command', WRAPPED,
      Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('<TS1>A<Ts>B body');
  try
    AssertTrue(Session.WrapTitle(1, 1, 3, OpenOrdinal));
    AssertEquals('consecutive titles serialize without nesting',
      UnicodeString('<TS1>A<Ts><TS3>B<Ts> body'), Session.RawText);
  finally
    Session.Free;
  end;

end;

procedure TTheWordWysiwygSessionV2Test.
  TitleWrapPartialFormattingIsOneUndoCommand;
const
  ORIGINAL: UnicodeString = '<b>ABCD</b> EF';
  WRAPPED: UnicodeString = '<b>AB</b><TS2><b>CD</b> E<Ts>F';
var
  OpenOrdinal: Integer;
  Session: TTheWordWysiwygSessionV2;
  Title: TTheWordTitleRange;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 41);
  try
    AssertTrue('session preflight accepts supported partial formatting',
      Session.CanWrapTitle(2, 4));
    AssertTrue(Session.WrapTitle(2, 4, 2, OpenOrdinal));
    AssertEquals('the session publishes only balanced split markup',
      WRAPPED, Session.RawText);
    AssertEquals(UnicodeString('ABCD EF'), Session.VisibleText);
    AssertEquals('both pieces retain bold presentation',
      2, Session.Document.MarkCount);
    AssertTrue(Session.TryGetTitle(OpenOrdinal, Title));
    AssertTrue(Title.Editable);
    AssertTrue('title-only fission remains a presentation edit',
      Session.ChangesAreSemanticOnly);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('fission does not change UTF-16 origin provenance',
        Trace.IsBaselineIdentity);
    finally
      Trace.Free;
    end;

    AssertTrue(Session.Undo);
    AssertEquals('one undo restores literal input and topology',
      ORIGINAL, Session.RawText);
    AssertFalse('wrap and all fission are one atomic command',
      Session.CanUndo);
    AssertTrue(Session.Redo);
    AssertEquals('redo restores the complete balanced transformation',
      WRAPPED, Session.RawText);
    AssertEquals(2, Session.Document.MarkCount);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TitleFormattingSupportsSemanticColorAndSize;
var
  Index: Integer;
  Session: TTheWordWysiwygSessionV2;
  Title: TTheWordTitleRange;
begin
  Session := TTheWordWysiwygSessionV2.Create('<TS1>Abc<Ts> body');
  try
    AssertTrue(Session.TryGetTitle(0, Title));
    AssertTrue(Title.Editable);
    AssertTrue(Session.CanApplySemantic(0, 3));
    AssertTrue(Session.ApplySemantic(twsmHtmlBold, 0, 3));
    AssertEquals(UnicodeString('<TS1><b>Abc</b><Ts> body'),
      Session.RawText);

    AssertTrue(Session.ApplyFontColor(0, 3, True, clRed));
    AssertTrue('font edit preserves imported title spelling',
      Pos('<TS1>', Session.RawText) > 0);
    AssertTrue(Session.AdjustRelativeFontSize(0, 3, 1));
    AssertTrue('combined font edit preserves imported title spelling',
      Pos('<TS1>', Session.RawText) > 0);
    AssertTrue(Session.TryGetTitle(0, Title));
    AssertTrue('formatted title remains editable', Title.Editable);
    AssertEquals('one combined font range', 1,
      Session.Document.FontMarkCount);
    Index := 0;
    AssertTrue(Session.Document.FontMarks[Index].HasColor);
    AssertEquals(clRed, Session.Document.FontMarks[Index].ResolvedColor);
    AssertTrue(Session.Document.FontMarks[Index].HasSize);
    AssertEquals(1, Session.Document.FontMarks[Index].RelativeSize);
    AssertTrue('all title formatting remains presentation-only',
      Session.ChangesAreSemanticOnly);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('<TS>Head<Ts>');
  try
    AssertTrue('an imported terminal title accepts semantic formatting',
      Session.ApplySemantic(twsmHtmlItalic, 0, 4));
    AssertTrue('an imported terminal title accepts font color',
      Session.ApplyFontColor(0, 4, True, clRed));
    AssertTrue('an imported terminal title accepts relative size',
      Session.AdjustRelativeFontSize(0, 4, 1));
    AssertTrue('terminal formatting keeps the original title wrapper',
      Pos('<TS>', Session.RawText) > 0);
    AssertTrue(Session.TryGetTitle(0, Title));
    AssertTrue('a formatted terminal title remains editable', Title.Editable);
    AssertEquals(1, Session.Document.FontMarkCount);
    AssertTrue(Session.Document.FontMarks[0].HasColor);
    AssertTrue(Session.Document.FontMarks[0].HasSize);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TitlePendingInsertionFormattingIsAtomic;
var
  BadUtf16: UnicodeString;
  NewOrdinal, OriginalOrdinal: Integer;
  Session: TTheWordWysiwygSessionV2;
  Span: TVisualSpan;
  Title: TTheWordTitleRange;
  Trace: TVerseEditTrace;
  Visual: IVerseVisualDocument;
begin
  Session := TTheWordWysiwygSessionV2.Create(
    '<TS1><font color=red size=+1>ab</font><Ts>body');
  try
    AssertTrue(Session.TryGetTitle(0, Title));
    OriginalOrdinal := Title.OpenOrdinal;
    AssertTrue('pending formatting inserts inside a title',
      Session.ReplaceTitleTextWithPendingFormatting(
        OriginalOrdinal, 1, 0, 'XY', True, 1, 2,
        True, False, 0, NewOrdinal));
    AssertTrue('restored title has a usable identity', NewOrdinal >= 0);
    AssertEquals(UnicodeString('aXYbbody'), Session.VisibleText);
    AssertTrue(Session.TryGetTitle(NewOrdinal, Title));
    AssertEquals('title grows with inserted text', 4,
      Title.EndOffset - Title.StartOffset);
    AssertTrue('original title spelling is preserved',
      Pos('<TS1>', Session.RawText) > 0);
    AssertTrue('original title close is preserved',
      Pos('<Ts>', Session.RawText) > 0);

    Visual := BuildTheWordVerseVisualDocument(Session.Document,
      tdLeftToRight, 91);
    AssertTrue(Visual.TryGetSpanAt(1, Span));
    AssertEquals('TS1 + pending size +2 composes to +6 pt',
      6, Span.Style.FontSizeDelta);
    AssertTrue('Default color reveals TS1 structural gray',
      Span.Style.HasForegroundColor);
    AssertEquals(THEWORD_TITLE_LEVEL1_COLOR,
      Span.Style.ForegroundColor);
    AssertTrue(Visual.TryGetSpanAt(0, Span));
    AssertEquals('surrounding imported +1 remains +4 pt with TS1',
      4, Span.Style.FontSizeDelta);
    AssertEquals('surrounding imported red remains red',
      LongInt(clRed), Span.Style.ForegroundColor);

    AssertTrue('formatted title insertion is one undo command',
      Session.Undo);
    AssertEquals(
      UnicodeString('<TS1><font color=red size=+1>ab</font><Ts>body'),
      Session.RawText);
    AssertFalse('formatted insertion adds exactly one undo entry',
      Session.CanUndo);

    AssertTrue(Session.TryGetTitle(0, Title));
    AssertFalse('markup delimiter is rejected in pending title typing',
      Session.ReplaceTitleTextWithPendingFormatting(
        Title.OpenOrdinal, 1, 0, '<b>', True, 1, 2,
        False, False, 0, NewOrdinal));
    AssertFalse('attribute delimiter is rejected in pending title typing',
      Session.ReplaceTitleTextWithPendingFormatting(
        Title.OpenOrdinal, 1, 0, '|', True, 1, 2,
        False, False, 0, NewOrdinal));
    AssertFalse('line break is rejected in pending title typing',
      Session.ReplaceTitleTextWithPendingFormatting(
        Title.OpenOrdinal, 1, 0, #10, True, 1, 2,
        False, False, 0, NewOrdinal));
    BadUtf16 := UnicodeString(WideChar($D800));
    AssertFalse('unpaired surrogate is rejected atomically',
      Session.ReplaceTitleTextWithPendingFormatting(
        Title.OpenOrdinal, 1, 0, BadUtf16, True, 1, 2,
        False, False, 0, NewOrdinal));
    AssertEquals('all rejected input leaves raw text untouched',
      UnicodeString('<TS1><font color=red size=+1>ab</font><Ts>body'),
      Session.RawText);
    AssertFalse('rejected pending input creates no undo entry',
      Session.CanUndo);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('rejected input leaves provenance untouched',
        Trace.IsBaselineIdentity);
    finally
      Trace.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TitleWrapRejectsCrossingAndProtectedContent;
var
  OpenOrdinal: Integer;
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('<TS>Head<Ts>body');
  try
    AssertFalse('selection crossing an existing title is rejected',
      Session.CanWrapTitle(3, 2));
    AssertFalse(Session.WrapTitle(3, 2, 1, OpenOrdinal));
    AssertEquals(UnicodeString('<TS>Head<Ts>body'), Session.RawText);
    AssertFalse(Session.CanUndo);
    AssertFalse('formatting cannot cross a title boundary',
      Session.CanApplySemantic(3, 2));
    AssertFalse(Session.ApplyFontColor(3, 2, True, clRed));
    AssertFalse(Session.AdjustRelativeFontSize(3, 2, 1));
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('word<WG123>');
  try
    AssertFalse('protected lexical content cannot be wrapped',
      Session.CanWrapTitle(0, 4));
    AssertFalse(Session.WrapTitle(0, 4, 1, OpenOrdinal));
    AssertEquals(UnicodeString('word<WG123>'), Session.RawText);
    AssertFalse(Session.CanUndo);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    '<TS>A<RF>note<Rf>B<Ts> body');
  try
    AssertFalse('title with a translator note remains protected',
      Session.CanApplySemantic(0, 2));
    AssertFalse(Session.ApplySemantic(twsmItalic, 0, 2));
    AssertEquals(UnicodeString('<TS>A<RF>note<Rf>B<Ts> body'),
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  MultipleTitleLevelsAndMalformedTitleRemainExplicit;
var
  Codec: ITheWordOntCodecV2;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse('<TS1>A<Ts><TS2>B<Ts>C');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 12);
    AssertEquals(UnicodeString('A') + #10 + 'B' + #10 + 'C', Visual.Text);
    AssertTrue(vtsfBold in Visual.Spans[0].Style.Flags);
    AssertFalse(vtsfItalic in Visual.Spans[0].Style.Flags);
    AssertEquals(2, Visual.Spans[0].Style.FontSizeDelta);
    AssertTrue(Visual.Spans[0].Interactive);
    AssertEquals(UnicodeString('title-level-1'),
      Visual.Spans[0].Annotation);
    AssertEquals(UnicodeString('<TS1>A<Ts><TS2>B<Ts>C'),
      Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('<TS>Unclosed body');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 13);
    AssertEquals('malformed title text is never silently hidden',
      UnicodeString('Unclosed body'), Visual.Text);
    AssertFalse('malformed title body requires Raw editing',
      Visual.Spans[0].Interactive);
    AssertEquals(UnicodeString('malformed-title'), Visual.Spans[0].Annotation);
    AssertEquals(UnicodeString('<TS>Unclosed body'),
      Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TerminalEmptyTitleKeepsOffsetMapReversible;
const
  RAW: UnicodeString = 'body<TS><Ts>';
var
  Codec: ITheWordOntCodecV2;
  DocumentOffset, LayoutOffset: Integer;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(RAW);
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 14);
    AssertEquals('terminal empty title emits its structural line break',
      UnicodeString('body') + #10, Visual.Text);

    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 4, twoaBefore,
      LayoutOffset));
    AssertEquals('before terminal title break', 4, LayoutOffset);
    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, LayoutOffset,
      DocumentOffset));
    AssertEquals('before affinity reverses to document end', 4,
      DocumentOffset);

    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 4, twoaAfter,
      LayoutOffset));
    AssertEquals('after terminal title break is a valid layout offset',
      Length(Visual.Text), LayoutOffset);
    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, LayoutOffset,
      DocumentOffset));
    AssertEquals('after affinity reverses to document end', 4,
      DocumentOffset);
    AssertEquals('raw terminal title remains literal', RAW,
      Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterMapsSyntheticProtectedSeparators;
var
  Codec: ITheWordOntCodecV2;
  DocumentLength, DocumentOffset, LayoutOffset: Integer;
  Hebrew: UnicodeString;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;

  Source := Codec.DecodeVerse('existe<x>NO');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 1);
    AssertEquals(UnicodeString('existe NO'), Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 6, twoaBefore,
      LayoutOffset));
    AssertEquals('document anchor before separator', 6, LayoutOffset);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 6, twoaAfter,
      LayoutOffset));
    AssertEquals('document anchor after separator', 7, LayoutOffset);
    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, 6,
      DocumentOffset));
    AssertEquals('layout before separator maps to anchor', 6,
      DocumentOffset);
    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, 7,
      DocumentOffset));
    AssertEquals('layout after separator maps to anchor', 6,
      DocumentOffset);
    AssertTrue(TryTheWordLayoutRangeToDocument(Visual, 7, 2,
      DocumentOffset, DocumentLength));
    AssertEquals('right word starts at document anchor', 6,
      DocumentOffset);
    AssertEquals('right word document length', 2, DocumentLength);
    AssertTrue(TryTheWordLayoutRangeToDocument(Visual, 6, 1,
      DocumentOffset, DocumentLength));
    AssertEquals('synthetic separator has no document width', 0,
      DocumentLength);
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('a||b');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 2);
    AssertEquals('adjacent structural objects add one separator',
      UnicodeString('a b'), Visual.Text);
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('a <x>b');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 3);
    AssertEquals('existing whitespace is not duplicated',
      UnicodeString('a b'), Visual.Text);
  finally
    Source.Free;
  end;

  Hebrew := UnicodeString(WideChar($05D1)) +
    UnicodeString(WideChar($05B0)) + UnicodeString(WideChar($05BC));
  Source := Codec.DecodeVerse(Hebrew + '<x>gloss');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdRightToLeft, 4);
    AssertEquals('mixed Hebrew and gloss remain visibly separated',
      Hebrew + ' gloss', Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, Length(Hebrew),
      twoaAfter, LayoutOffset));
    AssertEquals('Hebrew anchor accounts for synthetic layout unit',
      Length(Hebrew) + 1, LayoutOffset);
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  ParagraphMarkIsSyntheticMappedAndNonInteractive;
const
  RAW: UnicodeString = 'alpha<CM>beta';
var
  Codec: ITheWordOntCodecV2;
  DocumentLength, DocumentOffset, LayoutOffset, LogicalIndex: Integer;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(RAW);
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 15);
    AssertEquals('CM is exposed as a pilcrow only in the visual layout',
      UnicodeString('alpha¶beta'), Visual.Text);
    AssertEquals('rendering keeps the literal ONT source untouched', RAW,
      Codec.EncodeVerse(Source));

    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 5, twoaBefore,
      LayoutOffset));
    AssertEquals('document caret before CM precedes the pilcrow', 5,
      LayoutOffset);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 5, twoaAfter,
      LayoutOffset));
    AssertEquals('document caret after CM follows the pilcrow', 6,
      LayoutOffset);

    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, 5,
      DocumentOffset));
    AssertEquals('pilcrow start maps to the CM document anchor', 5,
      DocumentOffset);
    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, 6,
      DocumentOffset));
    AssertEquals('pilcrow end maps to the same zero-width anchor', 5,
      DocumentOffset);
    AssertTrue(TryTheWordLayoutRangeToDocument(Visual, 5, 1,
      DocumentOffset, DocumentLength));
    AssertEquals('pilcrow selection starts at the CM anchor', 5,
      DocumentOffset);
    AssertEquals('synthetic pilcrow has no document width', 0,
      DocumentLength);

    AssertFalse('pilcrow must not participate in editing or selection',
      Visual.TryGetInteractiveLogicalIndexAt(5, LogicalIndex));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('alpha<CM><CM>beta');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 16);
    AssertEquals('each consecutive CM has its own visual marker',
      UnicodeString('alpha¶¶beta'), Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 5, twoaAfter,
      LayoutOffset));
    AssertEquals('after affinity crosses both consecutive pilcrows', 7,
      LayoutOffset);
    AssertTrue(TryTheWordLayoutRangeToDocument(Visual, 5, 2,
      DocumentOffset, DocumentLength));
    AssertEquals('consecutive pilcrows share the CM document anchor', 5,
      DocumentOffset);
    AssertEquals('consecutive pilcrows remain zero-width in the document', 0,
      DocumentLength);
    AssertFalse('first consecutive pilcrow is not interactive',
      Visual.TryGetInteractiveLogicalIndexAt(5, LogicalIndex));
    AssertFalse('second consecutive pilcrow is not interactive',
      Visual.TryGetInteractiveLogicalIndexAt(6, LogicalIndex));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('<CM>body');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 17);
    AssertEquals('CM at document start remains visible',
      UnicodeString('¶body'), Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 0, twoaBefore,
      LayoutOffset));
    AssertEquals('before initial CM is layout start', 0, LayoutOffset);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 0, twoaAfter,
      LayoutOffset));
    AssertEquals('after initial CM follows its pilcrow', 1, LayoutOffset);
    AssertFalse('initial pilcrow is not interactive',
      Visual.TryGetInteractiveLogicalIndexAt(0, LogicalIndex));
    AssertEquals('initial CM raw source remains literal',
      UnicodeString('<CM>body'), Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('body<CM>');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 18);
    AssertEquals('CM at document end remains visible',
      UnicodeString('body¶'), Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 4, twoaBefore,
      LayoutOffset));
    AssertEquals('before terminal CM precedes its pilcrow', 4,
      LayoutOffset);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 4, twoaAfter,
      LayoutOffset));
    AssertEquals('after terminal CM is layout end', 5, LayoutOffset);
    AssertFalse('terminal pilcrow is not interactive',
      Visual.TryGetInteractiveLogicalIndexAt(4, LogicalIndex));
    AssertEquals('terminal CM raw source remains literal',
      UnicodeString('body<CM>'), Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('<TS>Head<Ts><CM>body');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 20);
    AssertEquals('title break is emitted before a following pilcrow',
      UnicodeString('Head' + #10 + '¶body'), Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 4, twoaAfter,
      LayoutOffset));
    AssertEquals('body caret follows the adjacent pilcrow', 6,
      LayoutOffset);
    AssertFalse('pilcrow after title remains non-interactive',
      Visual.TryGetInteractiveLogicalIndexAt(5, LogicalIndex));
    AssertEquals('reordered synthetic layout preserves raw ONT',
      UnicodeString('<TS>Head<Ts><CM>body'), Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('body<CM><TS>Head<Ts>');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 21);
    AssertEquals('CM before a title remains before its title break',
      UnicodeString('body¶' + #10 + 'Head'), Visual.Text);
    AssertFalse('prefix pilcrow remains non-interactive',
      Visual.TryGetInteractiveLogicalIndexAt(4, LogicalIndex));
    AssertEquals('prefix CM and title retain original raw order',
      UnicodeString('body<CM><TS>Head<Ts>'), Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('body<CM><TS><Ts><CM>tail');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 22);
    AssertEquals('empty title partitions CMs by their event ordinals',
      UnicodeString('body¶' + #10 + '¶tail'), Visual.Text);
    AssertFalse('CM before empty title remains non-interactive',
      Visual.TryGetInteractiveLogicalIndexAt(4, LogicalIndex));
    AssertFalse('CM after empty title remains non-interactive',
      Visual.TryGetInteractiveLogicalIndexAt(6, LogicalIndex));
    AssertEquals('empty title boundary preserves literal raw order',
      UnicodeString('body<CM><TS><Ts><CM>tail'), Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('<CM><CM>');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 19);
    AssertEquals('empty visible document still exposes every CM',
      UnicodeString('¶¶'), Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 0, twoaBefore,
      LayoutOffset));
    AssertEquals('empty document starts before both pilcrows', 0,
      LayoutOffset);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 0, twoaAfter,
      LayoutOffset));
    AssertEquals('empty document after affinity crosses both pilcrows', 2,
      LayoutOffset);
    AssertFalse('first empty-document pilcrow is not interactive',
      Visual.TryGetInteractiveLogicalIndexAt(0, LogicalIndex));
    AssertFalse('second empty-document pilcrow is not interactive',
      Visual.TryGetInteractiveLogicalIndexAt(1, LogicalIndex));
    AssertEquals('empty-document CM source remains literal',
      UnicodeString('<CM><CM>'), Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('alpha<cm>beta');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 20);
    AssertEquals('wrong-case cm remains an unknown protected tag', 0,
      Pos(UnicodeString(WideChar($00B6)), Visual.Text));
    AssertEquals('wrong-case cm raw source remains literal',
      UnicodeString('alpha<cm>beta'), Codec.EncodeVerse(Source));
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  ParagraphMarkEditingPreservesRawUndoAndProvenance;
var
  CurrentStart: Integer;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create('alpha beta', 51);
  try
    AssertTrue('Enter inserts one CM at the document caret',
      Session.InsertParagraphMark(5, 0));
    AssertEquals(UnicodeString('alpha<CM> beta'), Session.RawText);
    AssertEquals('CM never becomes visible document text',
      UnicodeString('alpha beta'), Session.VisibleText);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('structural-only insertion keeps exact provenance',
        Trace.IsBaselineIdentity);
      AssertEquals(51, Trace.TokenizerVersion);
    finally
      Trace.Free;
    end;

    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('alpha beta'), Session.RawText);
    AssertTrue(Session.Redo);
    AssertEquals(UnicodeString('alpha<CM> beta'), Session.RawText);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('redo restores the same exact provenance',
        Trace.IsBaselineIdentity);
    finally
      Trace.Free;
    end;

    AssertTrue('Backspace removes the adjacent CM',
      Session.RemoveParagraphMark(5, True));
    AssertEquals(UnicodeString('alpha beta'), Session.RawText);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('removing a zero-width object does not touch text origins',
        Trace.IsBaselineIdentity);
    finally
      Trace.Free;
    end;
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('alpha beta');
  try
    AssertTrue('Enter replaces selected visible text with one CM atomically',
      Session.InsertParagraphMark(0, 5));
    AssertEquals(UnicodeString('<CM> beta'), Session.RawText);
    AssertEquals(UnicodeString(' beta'), Session.VisibleText);
    Trace := Session.CloneEditTrace;
    try
      AssertFalse('removed selection is absent from the origin map',
        Trace.OriginMap.TryProjectOriginalRange(0, 5, CurrentStart));
      AssertTrue('untouched suffix keeps its exact origin',
        Trace.OriginMap.TryProjectOriginalRange(5, 5, CurrentStart));
      AssertEquals(0, CurrentStart);
    finally
      Trace.Free;
    end;
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('alpha beta'), Session.RawText);
    AssertTrue(Session.Redo);
    AssertEquals(UnicodeString('<CM> beta'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('a<CM><CM>b');
  try
    AssertTrue('Backspace removes only the last consecutive CM',
      Session.RemoveParagraphMark(1, True));
    AssertEquals(UnicodeString('a<CM>b'), Session.RawText);
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('a<CM><CM>b'), Session.RawText);
    AssertTrue(Session.Redo);
    AssertEquals(UnicodeString('a<CM>b'), Session.RawText);
    AssertTrue('Delete removes the one remaining CM',
      Session.RemoveParagraphMark(1, False));
    AssertEquals(UnicodeString('ab'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('alpha<CM> beta');
  try
    AssertTrue('selection deletion removes its CM and visible text together',
      Session.DeleteSelectionWithParagraphMarks(0, 6, [5]));
    AssertEquals(UnicodeString('beta'), Session.RawText);
    Trace := Session.CloneEditTrace;
    try
      AssertFalse('selected text is removed from provenance',
        Trace.OriginMap.TryProjectOriginalRange(0, 6, CurrentStart));
      AssertTrue('untouched suffix keeps exact provenance',
        Trace.OriginMap.TryProjectOriginalRange(6, 4, CurrentStart));
      AssertEquals(0, CurrentStart);
    finally
      Trace.Free;
    end;
    AssertTrue(Session.Undo);
    AssertEquals('combined deletion is one undo step',
      UnicodeString('alpha<CM> beta'), Session.RawText);
    AssertTrue(Session.Redo);
    AssertEquals(UnicodeString('beta'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('<CM><CM>body');
  try
    AssertTrue('one selected pilcrow removes one CM only',
      Session.DeleteSelectionWithParagraphMarks(0, 0, [0]));
    AssertEquals(UnicodeString('<CM>body'), Session.RawText);
    AssertTrue('two selected pilcrows remove both remaining occurrences',
      Session.Undo);
    AssertTrue(Session.DeleteSelectionWithParagraphMarks(0, 0, [0, 0]));
    AssertEquals(UnicodeString('body'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('a<CM><x>b');
  try
    AssertFalse('another protected object makes combined deletion atomic',
      Session.DeleteSelectionWithParagraphMarks(0, 2, [1]));
    AssertEquals(UnicodeString('a<CM><x>b'), Session.RawText);
    AssertFalse('failed combined deletion creates no undo entry',
      Session.CanUndo);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('<TS>Head<Ts>body');
  try
    AssertFalse('Enter cannot insert CM inside a protected title',
      Session.InsertParagraphMark(1, 0));
    AssertEquals(UnicodeString('<TS>Head<Ts>body'), Session.RawText);
    AssertTrue('Enter remains available at the title/body boundary',
      Session.InsertParagraphMark(4, 0));
    AssertEquals(UnicodeString('<TS>Head<Ts><CM>body'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    'word<WG123><WTN-NSM><wt>');
  try
    AssertFalse('Enter cannot split a protected lexical object',
      Session.InsertParagraphMark(2, 0));
    AssertEquals(UnicodeString('word<WG123><WTN-NSM><wt>'),
      Session.RawText);
    AssertTrue('Enter is allowed exactly after a lexical word',
      Session.InsertParagraphMark(4, 0));
    AssertEquals('CM follows every ordered lexical postfix event',
      UnicodeString('word<WG123><WTN-NSM><wt><CM>'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('a<x>b');
  try
    AssertFalse('Enter cannot share an anchor with another opaque object',
      Session.InsertParagraphMark(1, 0));
    AssertFalse('Delete cannot remove a non-CM opaque object',
      Session.RemoveParagraphMark(1, False));
    AssertEquals(UnicodeString('a<x>b'), Session.RawText);
    AssertFalse('rejected structural edits create no undo history',
      Session.CanUndo);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  ProtectedAnchorsAllowAdjacentButNotCrossingEdits;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('a|b');
  try
    AssertFalse('caret cannot insert on structural separator',
      Session.CanReplaceText(1, 0));
    AssertFalse('edit cannot cross structural separator',
      Session.CanReplaceText(0, 2));
    AssertTrue('right-hand text remains editable',
      Session.CanReplaceText(1, 1));
    AssertTrue(Session.ReplaceText(1, 1, 'B'));
    AssertEquals(UnicodeString('a|B'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('a<x>b');
  try
    AssertFalse('caret cannot insert on opaque separator',
      Session.CanReplaceText(1, 0));
    AssertFalse('edit cannot cross opaque separator',
      Session.CanReplaceText(0, 2));
    AssertTrue('text adjacent to opaque object remains editable',
      Session.CanReplaceText(1, 1));
    AssertTrue(Session.ReplaceText(1, 1, 'B'));
    AssertEquals(UnicodeString('a<x>B'), Session.RawText);
    AssertTrue('semantic formatting adjacent to opaque object is allowed',
      Session.CanApplySemantic(1, 1));
    AssertTrue(Session.ApplySemantic(twsmItalic, 1, 1));
    AssertEquals(UnicodeString('a<x><FI>B<Fi>'), Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  RawFallbackReentersTheSameCodecSession;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('word');
  try
    AssertTrue(Session.ReplaceRawText('<FU>word<Fu><opaque>'));
    AssertEquals(UnicodeString('word'), Session.VisibleText);
    AssertEquals(Ord(twlcProtectedRegions), Ord(Session.Capability));
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('word'), Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  SemanticToggleRemovesAnySliceForAllKinds;
const
  STARTS: array[0..3] of Integer = (0, 0, 2, 4);
  LENGTHS: array[0..3] of Integer = (6, 2, 2, 2);
var
  CaseIndex: Integer;
  CloseTag, Expected, OpenTag, Raw: UnicodeString;
  Kind: TTheWordSemanticKind;
  Session: TTheWordWysiwygSessionV2;
begin
  for Kind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
  begin
    OpenTag := TheWordCanonicalOpen(Kind);
    CloseTag := TheWordCanonicalClose(Kind);
    Raw := OpenTag + 'abcdef' + CloseTag;
    for CaseIndex := 0 to High(STARTS) do
    begin
      case CaseIndex of
        0: Expected := 'abcdef';
        1: Expected := 'ab' + OpenTag + 'cdef' + CloseTag;
        2: Expected := OpenTag + 'ab' + CloseTag + 'cd' + OpenTag +
          'ef' + CloseTag;
        3: Expected := OpenTag + 'abcd' + CloseTag + 'ef';
      end;
      Session := TTheWordWysiwygSessionV2.Create(Raw);
      try
        AssertTrue('covered selection must remove semantic kind ' +
          IntToStr(Ord(Kind)), Session.ApplySemantic(Kind,
            STARTS[CaseIndex], LENGTHS[CaseIndex]));
        AssertEquals('toggle must subtract the selected interval', Expected,
          Session.RawText);
        AssertEquals('semantic toggle never changes visible text',
          UnicodeString('abcdef'), Session.VisibleText);
      finally
        Session.Free;
      end;
    end;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  SemanticToggleAppliesAcrossMixedCoverageAndMerges;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('<FR>ab<Fr>cd');
  try
    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 1, 3));
    AssertEquals('a partially covered selection becomes wholly formatted',
      UnicodeString('<FR>abcd<Fr>'), Session.RawText);
    AssertEquals(UnicodeString('1111'),
      SemanticMask(Session.RawText, twsmWordsOfJesus));
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  SemanticToggleHandlesAdjacentNestedAndCrossingMarks;
var
  Changed: UnicodeString;
  Restored, Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(
    '<FI>ab<Fi><FI>cd<Fi>');
  try
    AssertTrue('adjacent marks form continuous coverage',
      Session.ApplySemantic(twsmItalic, 1, 2));
    AssertEquals(UnicodeString('<FI>a<Fi>bc<FI>d<Fi>'), Session.RawText);
    AssertEquals(UnicodeString('1001'),
      SemanticMask(Session.RawText, twsmItalic));
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    '<FI>a<FI>bc<Fi>d<Fi>');
  try
    AssertTrue('nested marks are evaluated by effective coverage',
      Session.ApplySemantic(twsmItalic, 1, 2));
    AssertEquals(UnicodeString('<FI>a<Fi>bc<FI>d<Fi>'), Session.RawText);
    AssertEquals(UnicodeString('1001'),
      SemanticMask(Session.RawText, twsmItalic));
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    '<FR>ab<FI>cd<Fr>ef<Fi>');
  try
    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 1, 2));
    Changed := Session.RawText;
    AssertEquals('only the requested kind is subtracted',
      UnicodeString('100100'), SemanticMask(Changed, twsmWordsOfJesus));
    AssertEquals('crossing italic coverage remains unchanged',
      UnicodeString('001111'), SemanticMask(Changed, twsmItalic));
    Restored := TTheWordWysiwygSessionV2.Create(Changed);
    try
      AssertEquals('crossing serialization is stable after decode', Changed,
        Restored.RawText);
    finally
      Restored.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  SemanticToggleUsesUtf16BoundariesAtomically;
var
  Emoji, Original: UnicodeString;
  Session: TTheWordWysiwygSessionV2;
begin
  Emoji := UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00));
  Original := '<FR>A' + Emoji + 'B<Fr>';
  Session := TTheWordWysiwygSessionV2.Create(Original);
  try
    AssertFalse('a semantic boundary cannot split a surrogate pair',
      Session.CanApplySemantic(2, 1));
    AssertFalse('rejected UTF-16 selection is a normal failed command',
      Session.ApplySemantic(twsmWordsOfJesus, 2, 1));
    AssertEquals('rejected UTF-16 toggle is atomic', Original,
      Session.RawText);
    AssertFalse('rejected UTF-16 toggle creates no undo entry',
      Session.CanUndo);

    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 1, 2));
    AssertEquals(UnicodeString('<FR>A<Fr>') + Emoji + '<FR>B<Fr>',
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  SemanticToggleRejectsTitleAndAllowsBodyBoundary;
const
  ORIGINAL: UnicodeString = '<TS>Head<Ts><FR>abcdef<Fr>';
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL);
  try
    AssertFalse('selection crossing title and body is protected',
      Session.CanApplySemantic(3, 2));
    AssertFalse(Session.ApplySemantic(twsmWordsOfJesus, 3, 2));
    AssertEquals(ORIGINAL, Session.RawText);
    AssertFalse(Session.CanUndo);

    AssertTrue('body starts at the same document offset as closing Ts',
      Session.ApplySemantic(twsmWordsOfJesus, 4, 2));
    AssertEquals(UnicodeString('<TS>Head<Ts>ab<FR>cdef<Fr>'),
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  SemanticToggleUndoRedoPreservesOntObjects;
const
  ORIGINAL: UnicodeString =
    '<X z="1"><FR>ab<WG123><WTN-NSM><wt>|cd<Fr><fi>tail</fi>';
  EXPECTED: UnicodeString =
    '<X z="1"><FR>a<Fr>b<WG123><WTN-NSM><wt>|c<FR>d<Fr><fi>tail</fi>';
var
  Changed: UnicodeString;
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL);
  try
    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 1, 2));
    Changed := Session.RawText;
    AssertEquals('unaffected ONT objects remain literal and ordered',
      EXPECTED, Changed);
    AssertEquals(UnicodeString('abcdtail'), Session.VisibleText);
    AssertEquals(UnicodeString('10010000'),
      SemanticMask(Changed, twsmWordsOfJesus));
    AssertTrue(Session.ChangesAreSemanticOnly);
    AssertTrue(CanCommitLegacyWysiwygChangeV2(True,
      Session.ChangesAreSemanticOnly));

    AssertTrue('one undo restores the literal source snapshot', Session.Undo);
    AssertEquals(ORIGINAL, Session.RawText);
    AssertFalse('toggle was exactly one command', Session.CanUndo);
    AssertTrue(Session.Redo);
    AssertEquals(Changed, Session.RawText);
    AssertEquals(UnicodeString('10010000'),
      SemanticMask(Session.RawText, twsmWordsOfJesus));
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  NonSemanticEventOrderControlsSemanticOnlyClassification;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('word<WG123><X>');
  try
    AssertTrue(Session.ReplaceRawText('word<X><WG123>'));
    AssertFalse('reordering lexical and opaque events is not semantic-only',
      Session.ChangesAreSemanticOnly);
    AssertFalse('Associations block reordered non-semantic ONT objects',
      CanCommitLegacyWysiwygChangeV2(True,
        Session.ChangesAreSemanticOnly));
    AssertTrue('without Associations the raw change remains allowed',
      CanCommitLegacyWysiwygChangeV2(False,
        Session.ChangesAreSemanticOnly));
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('word<WG123><X>');
  try
    AssertTrue(Session.ApplySemantic(twsmItalic, 0, 4));
    AssertTrue('visual toggle preserves the global non-semantic event order',
      Session.ChangesAreSemanticOnly);
    AssertTrue('semantic toggle remains committable with Associations',
      CanCommitLegacyWysiwygChangeV2(True,
        Session.ChangesAreSemanticOnly));
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.RejectedSemanticToggleCannotBeRedone;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('<FR>abcdef<Fr>');
  try
    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 2, 2));
    AssertTrue(Session.CanUndo);
    AssertTrue(Session.RejectLastChange);
    AssertEquals(UnicodeString('<FR>abcdef<Fr>'), Session.RawText);
    AssertFalse('a rejected command must not remain undoable',
      Session.CanUndo);
    AssertFalse('a rejected command must not be available through redo',
      Session.CanRedo);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.VisualEditsProduceExactOriginTrace;
var
  CurrentStart: Integer;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create('one two three', 42);
  try
    AssertTrue(Session.ReplaceText(4, 3, 'TWO'));
    Trace := Session.CloneEditTrace;
    try
      AssertEquals(42, Trace.TokenizerVersion);
      AssertEquals(Ord(vetmExact), Ord(Trace.Mode));
      AssertEquals(UnicodeString('one two three'), Trace.OriginalText);
      AssertEquals(UnicodeString('one TWO three'), Trace.CurrentText);
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(0, 4,
        CurrentStart));
      AssertEquals(0, CurrentStart);
      AssertFalse(Trace.OriginMap.TryProjectOriginalRange(4, 3,
        CurrentStart));
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(7, 6,
        CurrentStart));
      AssertEquals(7, CurrentStart);
    finally
      Trace.Free;
    end;
    Session.AcceptChanges;
    AssertFalse(Session.Dirty);
    Trace := Session.CloneEditTrace;
    try
      AssertEquals(UnicodeString('one TWO three'), Trace.OriginalText);
      AssertTrue(Trace.IsBaselineIdentity);
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(4, 3,
        CurrentStart));
    finally
      Trace.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  SemanticToggleDoesNotChangeOriginTrace;
var
  CurrentStart: Integer;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create('word', 7);
  try
    AssertTrue(Session.ApplySemantic(twsmItalic, 0, 4));
    Trace := Session.CloneEditTrace;
    try
      AssertEquals(Ord(vetmExact), Ord(Trace.Mode));
      AssertEquals(UnicodeString('word'), Trace.OriginalText);
      AssertEquals(UnicodeString('word'), Trace.CurrentText);
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(0, 4,
        CurrentStart));
      AssertEquals(0, CurrentStart);
    finally
      Trace.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.UndoRedoRestoresOriginTrace;
var
  CurrentStart: Integer;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create('abc def', 3);
  try
    AssertTrue(Session.ReplaceText(0, 3, 'ABC'));
    AssertTrue(Session.Undo);
    Trace := Session.CloneEditTrace;
    try
      AssertEquals(UnicodeString('abc def'), Trace.CurrentText);
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(0, 3,
        CurrentStart));
    finally
      Trace.Free;
    end;

    AssertTrue(Session.Redo);
    Trace := Session.CloneEditTrace;
    try
      AssertEquals(UnicodeString('ABC def'), Trace.CurrentText);
      AssertFalse(Trace.OriginMap.TryProjectOriginalRange(0, 3,
        CurrentStart));
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(3, 4,
        CurrentStart));
    finally
      Trace.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  CutAndPasteTrackDeletedAndInsertedUnits;
var
  CurrentStart, InsertedLength: Integer;
  Payload: string;
  PlainText: UnicodeString;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
  UsedPrivatePayload: Boolean;
begin
  Session := TTheWordWysiwygSessionV2.Create('one two', 11);
  try
    AssertTrue(Session.CutSelection(0, 3, PlainText, Payload));
    AssertEquals(UnicodeString('one'), PlainText);
    Trace := Session.CloneEditTrace;
    try
      AssertFalse(Trace.OriginMap.TryProjectOriginalRange(0, 3,
        CurrentStart));
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(3, 4,
        CurrentStart));
      AssertEquals(0, CurrentStart);
    finally
      Trace.Free;
    end;

    AssertTrue(Session.Paste(0, 0, Payload, PlainText, InsertedLength,
      UsedPrivatePayload));
    AssertTrue(UsedPrivatePayload);
    AssertEquals(3, InsertedLength);
    AssertEquals(UnicodeString('one two'), Session.VisibleText);
    Trace := Session.CloneEditTrace;
    try
      AssertFalse('pasted text is new even with the private payload',
        Trace.OriginMap.TryProjectOriginalRange(0, 3, CurrentStart));
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(3, 4,
        CurrentStart));
      AssertEquals(3, CurrentStart);
    finally
      Trace.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  RawVisibleChangeUsesConservativeTrace;
var
  CurrentStart: Integer;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create('prefix middle suffix', 15);
  try
    AssertTrue(Session.ReplaceRawText('<FI>prefix changed suffix<Fi>'));
    Trace := Session.CloneEditTrace;
    try
      AssertEquals(Ord(vetmRawConservative), Ord(Trace.Mode));
      AssertEquals(UnicodeString('prefix changed suffix'),
        Trace.CurrentText);
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(0, 7,
        CurrentStart));
      AssertFalse(Trace.OriginMap.TryProjectOriginalRange(7, 6,
        CurrentStart));
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(13, 7,
        CurrentStart));
      AssertEquals(14, CurrentStart);
    finally
      Trace.Free;
    end;

    AssertTrue(Session.ReplaceRawText(
      '<FU><FI>prefix changed suffix<Fi><Fu>'));
    Trace := Session.CloneEditTrace;
    try
      AssertEquals('semantic-only Raw change keeps the existing map mode',
        Ord(vetmRawConservative), Ord(Trace.Mode));
      AssertTrue(Trace.OriginMap.TryProjectOriginalRange(13, 7,
        CurrentStart));
    finally
      Trace.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  IdenticalRetypeRemainsDirtyUntilUndo;
var
  CurrentStart: Integer;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create('same', 8);
  try
    AssertTrue(Session.ReplaceText(0, 4, 'same'));
    AssertEquals(UnicodeString('same'), Session.RawText);
    AssertTrue('provenance makes an identical manual retype dirty',
      Session.Dirty);
    Trace := Session.CloneEditTrace;
    try
      AssertFalse(Trace.OriginMap.TryProjectOriginalRange(0, 4,
        CurrentStart));
    finally
      Trace.Free;
    end;
    AssertTrue(Session.Undo);
    AssertFalse(Session.Dirty);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  RawProtectsLexicalObjectsButAllowsExternalEdits;
const
  ORIGINAL: UnicodeString = 'one<WG123><WTN-NSM><wt> two';
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 8);
  try
    AssertTrue('baseline lexical objects are unchanged',
      Session.ProtectedLexicalObjectsUnchanged);
    AssertTrue(Session.ReplaceRawText(
      'new one<WG123><WTN-NSM><wt> two'));
    AssertTrue('insertion before a protected word only shifts its anchor',
      Session.ProtectedLexicalObjectsUnchanged);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 8);
  try
    AssertTrue(Session.ReplaceRawText(
      'one<WG123><WTN-NSM><wt> TWO'));
    AssertTrue('text outside the lexical word remains editable',
      Session.ProtectedLexicalObjectsUnchanged);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 8);
  try
    AssertTrue(Session.ReplaceRawText(
      'ONE<WG123><WTN-NSM><wt> two'));
    AssertFalse('the word carrying Strong/Morph remains protected',
      Session.ProtectedLexicalObjectsUnchanged);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 8);
  try
    AssertTrue(Session.ReplaceRawText(
      'one<WG124><WTN-NSM><wt> two'));
    AssertFalse('lexical raw payload cannot be changed',
      Session.ProtectedLexicalObjectsUnchanged);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 8);
  try
    AssertTrue(Session.ReplaceRawText(
      'one<WTN-NSM><WG123><wt> two'));
    AssertFalse('lexical order cannot be changed',
      Session.ProtectedLexicalObjectsUnchanged);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TranslatorNoteCommandsAreAtomicAndPreserveProvenance;
const
  ORIGINAL: UnicodeString = 'word<WG123><WTN-NSM><wt> tail';
  INSERTED: UnicodeString =
    'word<WG123><WTN-NSM><wt><RF q=*><b>note</b><Rf> tail';
  UPDATED: UnicodeString =
    'word<WG123><WTN-NSM><wt><RF q="A B"><i>changed</i><Rf> tail';
var
  FirstId, NoteId, RevisionBefore, SecondId: QWord;
  Note: TTheWordTranslatorNote;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL, 73);
  try
    AssertFalse('RF cannot split a protected lexical word',
      Session.CanInsertTranslatorNote(2));
    AssertTrue(Session.CanInsertTranslatorNote(4));
    AssertTrue(Session.InsertTranslatorNote(4, '*', '<B>note</B>',
      NoteId));
    AssertTrue(NoteId <> 0);
    AssertEquals('one structural edit advances one revision', QWord(1),
      Session.Revision);
    AssertEquals(INSERTED, Session.RawText);
    AssertEquals(UnicodeString('word tail'), Session.VisibleText);
    AssertEquals(1, Session.Document.TranslatorNoteCount);
    AssertFalse('RF is a non-semantic ONT object',
      Session.ChangesAreSemanticOnly);
    AssertTrue('lexical identity remains unchanged',
      Session.ProtectedLexicalObjectsUnchanged);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('RF insertion leaves every text origin untouched',
        Trace.IsBaselineIdentity);
      AssertEquals(73, Trace.TokenizerVersion);
    finally
      Trace.Free;
    end;

    AssertTrue(Session.UpdateTranslatorNote(NoteId, 'A B',
      '<i>changed</i>'));
    AssertEquals('update is one more revision', QWord(2),
      Session.Revision);
    AssertEquals(UPDATED, Session.RawText);
    AssertTrue('one undo restores the complete previous note', Session.Undo);
    AssertEquals(INSERTED, Session.RawText);
    Trace := Session.CloneEditTrace;
    try
      AssertTrue('undo restores exact unchanged provenance',
        Trace.IsBaselineIdentity);
    finally
      Trace.Free;
    end;
    AssertTrue(Session.Redo);
    AssertEquals(UPDATED, Session.RawText);

    NoteId := Session.Document.TranslatorNotes[0].Id;
    AssertTrue(Session.RemoveTranslatorNote(NoteId));
    AssertEquals(ORIGINAL, Session.RawText);
    AssertEquals(0, Session.Document.TranslatorNoteCount);
    AssertTrue('one undo restores the removed note atomically', Session.Undo);
    AssertEquals(UPDATED, Session.RawText);
    AssertEquals(1, Session.Document.TranslatorNoteCount);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    'a<RF   q="x" >same<Rf>b');
  try
    NoteId := Session.Document.TranslatorNotes[0].Id;
    RevisionBefore := Session.Revision;
    AssertTrue('saving an unchanged note is a successful no-op',
      Session.UpdateTranslatorNote(NoteId, 'x', 'same'));
    AssertEquals('no-op does not advance revision', RevisionBefore,
      Session.Revision);
    AssertFalse('no-op does not create undo history', Session.CanUndo);
    AssertEquals('no-op preserves imported RF literally',
      'a<RF   q="x" >same<Rf>b', Session.RawText);
    AssertTrue(Session.UpdateTranslatorNote(NoteId, 'x', '<i>new</i>'));
    AssertEquals('session body update preserves RF opening',
      'a<RF   q="x" ><i>new</i><Rf>b', Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('a b');
  try
    AssertTrue(Session.InsertTranslatorNote(1, '*', '<b>x</b>',
      FirstId));
    AssertTrue(Session.InsertTranslatorNote(1, '*', '<b>x</b>',
      SecondId));
    AssertTrue('identical notes at one anchor have distinct IDs',
      FirstId <> SecondId);
    AssertEquals(2, Session.Document.TranslatorNoteCount);
    AssertTrue(Session.UpdateTranslatorNote(SecondId, '*',
      '<i>second</i>'));
    AssertTrue(Session.TryGetTranslatorNote(FirstId, Note));
    AssertEquals('first identical note remains untouched',
      '<b>x</b>', Note.BodyRaw);
    AssertTrue(Session.TryGetTranslatorNote(SecondId, Note));
    AssertEquals('the newly inserted identical note is targeted',
      '<i>second</i>', Note.BodyRaw);
    AssertTrue(Session.RemoveTranslatorNote(SecondId));
    AssertEquals(1, Session.Document.TranslatorNoteCount);
    AssertTrue(Session.TryGetTranslatorNote(FirstId, Note));
    AssertEquals('<b>x</b>', Note.BodyRaw);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TranslatorNoteMarkerResolvesSavedDraftWithStableId;
var
  FirstId, MarkerId, SecondId: QWord;
  FoundFirstMarker, FoundSecondMarker: Boolean;
  Index: Integer;
  Note: TTheWordTranslatorNote;
  Session: TTheWordWysiwygSessionV2;
  Span: TVisualSpan;
  Visual: IVerseVisualDocument;
begin
  Session := TTheWordWysiwygSessionV2.Create('A B C');
  try
    AssertTrue(Session.InsertTranslatorNote(4, '',
      '<b>saved draft</b>', FirstId));
    Visual := BuildTheWordVerseVisualDocument(Session.Document,
      tdLeftToRight, Session.Revision);
    MarkerId := 0;
    for Index := 0 to Visual.SpanCount - 1 do
    begin
      Span := Visual.Spans[Index];
      if Span.Role = vsrTranslatorNote then
      begin
        MarkerId := Span.ObjectId;
        Break;
      end;
    end;
    AssertTrue('inserted RF produces a marker ObjectId', MarkerId <> 0);
    AssertTrue('marker keeps the ID returned by Session.Insert',
      MarkerId = FirstId);
    AssertTrue('marker ObjectId resolves through the live session',
      Session.TryGetTranslatorNote(MarkerId, Note));
    AssertEquals('resolved marker reloads the saved draft body',
      '<b>saved draft</b>', Note.BodyRaw);

    AssertTrue(Session.InsertTranslatorNote(4, '',
      '<i>second note</i>', SecondId));
    AssertTrue('same-anchor notes retain distinct stable IDs',
      SecondId <> FirstId);
    AssertTrue(Session.TryGetTranslatorNote(FirstId, Note));
    AssertEquals('<b>saved draft</b>', Note.BodyRaw);
    AssertTrue(Session.TryGetTranslatorNote(SecondId, Note));
    AssertEquals('<i>second note</i>', Note.BodyRaw);

    Visual := BuildTheWordVerseVisualDocument(Session.Document,
      tdLeftToRight, Session.Revision);
    FoundFirstMarker := False;
    FoundSecondMarker := False;
    for Index := 0 to Visual.SpanCount - 1 do
    begin
      Span := Visual.Spans[Index];
      if Span.Role <> vsrTranslatorNote then
        Continue;
      AssertTrue('every rebuilt marker resolves through Session',
        Session.TryGetTranslatorNote(Span.ObjectId, Note));
      AssertTrue('no rebuilt marker resolves an empty draft',
        Note.BodyRaw <> '');
      if Span.ObjectId = FirstId then
        FoundFirstMarker := True
      else if Span.ObjectId = SecondId then
        FoundSecondMarker := True;
    end;
    AssertTrue('first same-anchor marker remains present',
      FoundFirstMarker);
    AssertTrue('second same-anchor marker remains present',
      FoundSecondMarker);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  TranslatorNoteAnchorsProtectTextEditing;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('a<RF>note<Rf>b');
  try
    AssertEquals(UnicodeString('ab'), Session.VisibleText);
    AssertTrue('caret can insert immediately before the right-sticky RF',
      Session.CanReplaceText(1, 0));
    AssertFalse('replacement cannot cross the RF anchor',
      Session.CanReplaceText(0, 2));
    AssertTrue(Session.ReplaceText(1, 0, 'X'));
    AssertEquals(UnicodeString('aX<RF>note<Rf>b'), Session.RawText);
    AssertEquals('RF shifts after text inserted at its anchor', 2,
      Session.Document.TranslatorNotes[0].OffsetUTF16);

    AssertTrue('text beginning immediately after shifted RF remains editable',
      Session.CanReplaceText(2, 1));
    AssertTrue(Session.ReplaceText(2, 1, 'B'));
    AssertEquals(UnicodeString('aX<RF>note<Rf>B'), Session.RawText);
    AssertEquals(2, Session.Document.TranslatorNotes[0].OffsetUTF16);

    AssertTrue('an edit strictly before RF shifts its anchor safely',
      Session.ReplaceText(0, 0, 'X'));
    AssertEquals(UnicodeString('XaX<RF>note<Rf>B'), Session.RawText);
    AssertEquals(3, Session.Document.TranslatorNotes[0].OffsetUTF16);
    AssertTrue('shifted anchor remains right-sticky',
      Session.CanReplaceText(3, 0));
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterResolvesNestedFontDimensions;
const
  RAW: UnicodeString =
    '<font color=red size=+1>aa' +
    '<font color=green>bb</font>' +
    '<font size=-1>cc</font>' +
    '<font size=+0>ee</font>dd</font>';
  PRIORITY_RAW: UnicodeString =
    '<font color=blue><FR>x<Fr><FO>y<Fo></font>';
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Span: TVisualSpan;
  Style: TVisualTextStyle;
  Visual: IVerseVisualDocument;

  procedure AssertStyleAt(AOffset, ASize: Integer; AColor: LongInt);
  begin
    AssertTrue(Visual.TryGetSpanAt(AOffset, Span));
    AssertEquals('font size delta', ASize, Span.Style.FontSizeDelta);
    AssertTrue(Span.Style.HasForegroundColor);
    AssertEquals('font color', AColor, Span.Style.ForegroundColor);
  end;

begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(RAW);
  try
    AssertEquals(4, Document.FontMarkCount);
    Visual := BuildTheWordVerseVisualDocument(Document, tdLeftToRight, 31);
    AssertStyleAt(0, 2, LongInt(clRed));
    AssertStyleAt(2, 2, LongInt(clGreen));
    AssertStyleAt(4, -2, LongInt(clRed));
    AssertStyleAt(6, 0, LongInt(clRed));
    AssertStyleAt(8, 2, LongInt(clRed));
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Document, 3, Style));
    AssertEquals(2, Style.FontSizeDelta);
    AssertEquals(LongInt(clGreen), Style.ForegroundColor);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Document, 5, Style));
    AssertEquals(-2, Style.FontSizeDelta);
    AssertEquals(LongInt(clRed), Style.ForegroundColor);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Document, 7, Style));
    AssertEquals('explicit +0 replaces outer size', 0, Style.FontSizeDelta);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Document, 9, Style));
    AssertEquals('outer size restores after +0 close', 2, Style.FontSizeDelta);
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Document, 0, Style));
    AssertEquals('font boundary does not leak size', 0, Style.FontSizeDelta);
    AssertTrue('font boundary remains editable',
      Document.CanReplaceVisibleText(0, 0));
    AssertTrue(TryResolveTheWordInsertionVisualStyle(Document, 10, Style));
    AssertEquals('closing boundary restores default size', 0,
      Style.FontSizeDelta);
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(PRIORITY_RAW);
  try
    Visual := BuildTheWordVerseVisualDocument(Document, tdLeftToRight, 32);
    AssertTrue(Visual.TryGetSpanAt(0, Span));
    AssertEquals('FR overrides font blue', LongInt(clRed),
      Span.Style.ForegroundColor);
    AssertTrue(Visual.TryGetSpanAt(1, Span));
    AssertEquals('FO overrides font color outside FR', LongInt(clBlack),
      Span.Style.ForegroundColor);
  finally
    Document.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterComposesTitleAndRelativeFontSizes;
const
  RAW: UnicodeString =
    '<TS><font size=+1>aa</font><font size=-1>bb</font><Ts>' +
    '<TS2><font size=+1>cc</font><font size=-1>dd</font><Ts>' +
    '<font size=+1>ee</font><font size=-1>ff</font>' +
    '<TS3><font size=+1>gg</font><Ts>';
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Span: TVisualSpan;
  Style: TVisualTextStyle;
  Visual: IVerseVisualDocument;

  procedure AssertVisualSize(const AText: UnicodeString;
    AExpectedDelta: Integer);
  var
    Offset: Integer;
  begin
    Offset := Pos(AText, Visual.Text) - 1;
    AssertTrue('visual text exists: ' + UTF8Encode(AText), Offset >= 0);
    AssertTrue('visual span resolves: ' + UTF8Encode(AText),
      Visual.TryGetSpanAt(Offset, Span));
    AssertEquals('visual font delta: ' + UTF8Encode(AText),
      AExpectedDelta, Span.Style.FontSizeDelta);
  end;

  procedure AssertInsertionSize(AOffset, AExpectedDelta: Integer;
    const AExpectedFlags: TVisualTextStyleFlags);
  begin
    AssertTrue('insertion style resolves',
      TryResolveTheWordInsertionVisualStyle(Document, AOffset, Style));
    AssertEquals('insertion font delta', AExpectedDelta,
      Style.FontSizeDelta);
    AssertTrue('insertion title flags', Style.Flags = AExpectedFlags);
  end;

begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(RAW);
  try
    Visual := BuildTheWordVerseVisualDocument(Document,
      tdLeftToRight, 41);
    AssertVisualSize('aa', 4);
    AssertVisualSize('bb', 0);
    AssertVisualSize('cc', 3);
    AssertVisualSize('dd', -1);
    AssertVisualSize('ee', 2);
    AssertVisualSize('ff', -2);
    AssertVisualSize('gg', 2);

    AssertInsertionSize(1, 4, [vtsfBold]);
    AssertInsertionSize(3, 0, [vtsfBold]);
    AssertInsertionSize(5, 3, [vtsfBold, vtsfItalic]);
    AssertInsertionSize(7, -1, [vtsfBold, vtsfItalic]);
    AssertInsertionSize(9, 2, []);
    AssertInsertionSize(11, -2, []);
    AssertInsertionSize(13, 2, [vtsfItalic]);
  finally
    Document.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  FontAndSemanticCrossingsStayBalanced;
var
  Codec: ITheWordOntCodecV2;
  Restored: TTheWordVerseDocument;
  Session: TTheWordWysiwygSessionV2;
  Raw: UnicodeString;

  procedure AssertRestores(const AExpectedText: UnicodeString);
  begin
    Raw := Session.RawText;
    Restored := Codec.DecodeVerse(Raw);
    try
      AssertEquals(AExpectedText, Restored.VisibleText);
      AssertEquals(0, Restored.OpaqueItemCount);
      AssertEquals(1, Restored.FontMarkCount);
      AssertEquals('synthetic reopen merges to one logical semantic mark',
        1, Restored.MarkCount);
      AssertEquals('no-op after re-decode is stable', Raw,
        Codec.EncodeVerse(Restored));
    finally
      Restored.Free;
    end;
  end;

begin
  Codec := TTheWordOntCodecV2.Create;
  Session := TTheWordWysiwygSessionV2.Create(
    '<font color=red>x</font> y');
  try
    AssertTrue(Session.ApplySemantic(twsmHtmlBold, 0, 3));
    AssertTrue('enclosing semantic closes and reopens at font boundary',
      Pos('</b></font><b>', Session.RawText) > 0);
    AssertRestores('x y');
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    '<font color=red>x</font> y');
  try
    AssertTrue(Session.ApplySemantic(twsmHtmlBold, 0, 1));
    AssertFalse('same anchors need no synthetic crossing',
      Pos('</b></font><b>', Session.RawText) > 0);
    AssertRestores('x y');
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    '<font color=red>xy</font> z');
  try
    AssertTrue(Session.ApplySemantic(twsmHtmlBold, 1, 3));
    AssertTrue('real crossing is split at font close',
      Pos('</b></font><b>', Session.RawText) > 0);
    AssertRestores('xy z');
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  FontOnlyRawChangeIsSemanticPresentation;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('ab');
  try
    AssertTrue(Session.ReplaceRawText('<font size=+1>ab</font>'));
    AssertTrue(Session.Dirty);
    AssertTrue('font presentation is semantic-only',
      Session.ChangesAreSemanticOnly);
    AssertTrue('associations permit presentation-only font changes',
      CanCommitLegacyWysiwygChangeV2(True, Session.ChangesAreSemanticOnly));
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  FontColorSelectionPreservesTopologyAndIsAtomicPresentation;
const
  ORIGINAL: UnicodeString =
    '<font size="+1" name=''Tahoma'' color=red>abcd</font>';
  TARGET_COLOR = $00563412;
var
  Changed: UnicodeString;
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Session: TTheWordWysiwygSessionV2;

  procedure AssertEffectiveAt(AOffset: Integer; AExpectedColor: LongInt);
  var
    BestColorOrdinal: Integer;
    BestNameOrdinal: Integer;
    BestSizeOrdinal: Integer;
    EffectiveColor: LongInt;
    EffectiveName: UnicodeString;
    EffectiveSize: Integer;
    Index: Integer;
    Mark: TTheWordFontMark;
  begin
    BestColorOrdinal := -1;
    BestNameOrdinal := -1;
    BestSizeOrdinal := -1;
    EffectiveColor := -1;
    EffectiveName := '';
    EffectiveSize := 0;
    for Index := 0 to Document.FontMarkCount - 1 do
    begin
      Mark := Document.FontMarks[Index];
      if (Mark.StartOffset > AOffset) or (Mark.EndOffset <= AOffset) then
        Continue;
      if Mark.HasColor and (Mark.OpenOrdinal > BestColorOrdinal) then
      begin
        BestColorOrdinal := Mark.OpenOrdinal;
        EffectiveColor := Mark.ResolvedColor;
      end;
      if Mark.HasSize and (Mark.OpenOrdinal > BestSizeOrdinal) then
      begin
        BestSizeOrdinal := Mark.OpenOrdinal;
        EffectiveSize := Mark.RelativeSize;
      end;
      if Mark.HasFontName and (Mark.OpenOrdinal > BestNameOrdinal) then
      begin
        BestNameOrdinal := Mark.OpenOrdinal;
        EffectiveName := Mark.FontNameRaw;
      end;
    end;
    AssertTrue('effective color exists', BestColorOrdinal >= 0);
    AssertEquals('effective color', AExpectedColor, EffectiveColor);
    AssertTrue('relative size remains present', BestSizeOrdinal >= 0);
    AssertEquals('relative size remains +1', 1, EffectiveSize);
    AssertTrue('font name remains present', BestNameOrdinal >= 0);
    AssertEquals('font name remains unchanged', 'Tahoma',
      UTF8Encode(EffectiveName));
  end;

begin
  Codec := TTheWordOntCodecV2.Create;
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL);
  Document := nil;
  try
    AssertTrue('selection color is applied through the session',
      Session.ApplyFontColor(1, 2, True, TARGET_COLOR));
    Changed := Session.RawText;
    AssertTrue('the raw topology changes', Changed <> ORIGINAL);
    AssertEquals('visible text is preserved', UnicodeString('abcd'),
      Session.VisibleText);
    AssertTrue('font-only mutation is presentation-only',
      Session.ChangesAreSemanticOnly);
    AssertTrue('associations accept the presentation-only change',
      CanCommitLegacyWysiwygChangeV2(True,
        Session.ChangesAreSemanticOnly));

    Document := Codec.DecodeVerse(Changed);
    AssertEquals('changed raw remains decodable', UnicodeString('abcd'),
      Document.VisibleText);
    AssertEquals('no malformed topology is introduced', 0,
      Document.OpaqueItemCount);
    AssertEquals('re-encoding the rebuilt topology is stable', Changed,
      Codec.EncodeVerse(Document));
    AssertTrue('the imported font name lexeme is retained',
      Pos('name=''Tahoma''', Changed) > 0);
    AssertEffectiveAt(0, $000000FF);
    AssertEffectiveAt(1, TARGET_COLOR);
    AssertEffectiveAt(2, TARGET_COLOR);
    AssertEffectiveAt(3, $000000FF);

    AssertTrue('selection command creates one undo entry', Session.CanUndo);
    AssertTrue(Session.Undo);
    AssertEquals('one undo restores the exact imported raw', ORIGINAL,
      Session.RawText);
    AssertFalse('color topology did not create auxiliary undo entries',
      Session.CanUndo);
    AssertTrue(Session.Redo);
    AssertEquals('redo restores the exact rebuilt topology', Changed,
      Session.RawText);
  finally
    Document.Free;
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  RelativeFontSizeIsAtomicUndoableAndProtected;
const
  ORIGINAL: UnicodeString = '<font size=+1 color=red>ab</font>c';
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL);
  try
    AssertTrue(Session.AdjustRelativeFontSize(0, 1, 1));
    AssertEquals('<font size=+2 color=red>a</font>' +
      '<font size=+1 color=red>b</font>c', Session.RawText);
    AssertTrue(Session.ChangesAreSemanticOnly);
    AssertTrue(CanCommitLegacyWysiwygChangeV2(True,
      Session.ChangesAreSemanticOnly));
    AssertTrue(Session.Undo);
    AssertEquals(ORIGINAL, Session.RawText);
    AssertFalse('one selection command creates one undo entry',
      Session.CanUndo);
    AssertTrue(Session.Redo);
    AssertEquals('<font size=+2 color=red>a</font>' +
      '<font size=+1 color=red>b</font>c', Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create(
    '<font size=+1>ab</font>');
  try
    AssertTrue(Session.ReplaceTextWithRelativeFontSize(
      1, 0, 'x', 1, 0));
    AssertEquals('<font size=+1>a</font>x' +
      '<font size=+1>b</font>', Session.RawText);
    AssertTrue('pending typing is one command', Session.Undo);
    AssertEquals('<font size=+1>ab</font>', Session.RawText);
    AssertFalse(Session.CanUndo);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('<TS>head<Ts>body');
  try
    AssertTrue('an editable title accepts relative font size',
      Session.AdjustRelativeFontSize(0, 1, 1));
    AssertEquals(UnicodeString(
      '<TS><font size=+1>h</font>ead<Ts>body'), Session.RawText);
    AssertTrue(Session.CanUndo);
  finally
    Session.Free;
  end;
end;

initialization
  RegisterTest(TTheWordWysiwygSessionV2Test);

end.
