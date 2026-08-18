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
  end;

implementation

uses
  Graphics, text_direction, verse_visual_document, theword_ont_codec_v2,
  theword_verse_visual_adapter_v2, theword_wysiwyg_session_v2;

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

initialization
  RegisterTest(TTheWordWysiwygSessionV2Test);

end.
