unit theword_wysiwyg_session_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTheWordWysiwygSessionV2Test = class(TTestCase)
  published
    procedure AppliesFourSemanticsWithoutExposingTagsAsText;
    procedure UndoRedoRestoresOntSnapshots;
    procedure TextReplacementUsesUTF16Offsets;
    procedure RejectedReplacementDoesNotPolluteUndo;
    procedure OpaqueAndLexicalObjectsProtectTextEdits;
    procedure VisualAdapterCombinesOverlappingStyles;
    procedure VisualAdapterMapsSyntheticProtectedSeparators;
    procedure TitleRendersAsProtectedChapterHeading;
    procedure TitleCannotBeEditedFormattedOrCutVisually;
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
  end;

implementation

uses
  Graphics, text_direction, verse_visual_document, theword_ont_codec_v2,
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
  AppliesFourSemanticsWithoutExposingTagsAsText;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('abcd');
  try
    AssertTrue(Session.ApplySemantic(twsmItalic, 0, 1));
    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 1, 1));
    AssertTrue(Session.ApplySemantic(twsmOldTestamentQuote, 2, 1));
    AssertTrue(Session.ApplySemantic(twsmUnderline, 3, 1));
    AssertEquals(UnicodeString('abcd'), Session.VisibleText);
    AssertEquals(UnicodeString('<FI>a<Fi><FR>b<Fr><FO>c<Fo><FU>d<Fu>'),
      Session.RawText);
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
    AssertFalse('title span is not interactive', Span.Interactive);
    AssertTrue('default title is bold', vtsfBold in Span.Style.Flags);
    AssertEquals('default title grows by two points', 2,
      Span.Style.FontSizeDelta);
    AssertTrue('default title uses chapter heading color',
      Span.Style.HasForegroundColor);
    AssertEquals('default title annotation', UnicodeString('title-level-0'),
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
    AssertFalse('title text cannot receive FI/FR/FO/FU',
      Session.CanApplySemantic(0, TitleLength));
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
    AssertTrue(vtsfItalic in Visual.Spans[0].Style.Flags);
    AssertEquals(1, Visual.Spans[0].Style.FontSizeDelta);
    AssertFalse(Visual.Spans[0].Interactive);
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
    AssertTrue('malformed title does not masquerade as a protected heading',
      Visual.Spans[0].Interactive);
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

initialization
  RegisterTest(TTheWordWysiwygSessionV2Test);

end.
