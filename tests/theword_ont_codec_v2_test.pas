unit theword_ont_codec_v2_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, theword_ont_codec_v2, ONTParser,
  ONTTokenizer, Syntagm, theword_translator_note_body_v2,
  theword_inline_format_v2, theword_font_size_mutation_v2,
  text_direction, verse_visual_document, theword_verse_visual_adapter_v2;

type
  TTheWordOntCodecV2Tests = class(TTestCase)
  private
    FCodec: ITheWordOntCodecV2;
    procedure AssertUnicodeEquals(const AMessage: string;
      const AExpected, AActual: UnicodeString);
    procedure AssertRawBytesEqual(const AMessage: string;
      const AExpected, AActual: RawByteString);
    function FindMark(ADocument: TTheWordVerseDocument;
      AKind: TTheWordSemanticKind): TTheWordSemanticMark;
    function InlineSpanText(const ARaw: UnicodeString;
      const ASpanUTF16: TTheWordUTF16SpanV2): UnicodeString;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SemanticTagsAreStrictlyCaseSensitive;
    procedure HtmlSemanticTagsAreCaseInsensitiveAndSafe;
    procedure AllSevenSemanticPairsBecomeMarks;
    procedure LessThanWithoutClosingBracketRemainsText;
    procedure UnknownAndMalformedTagsAreOpaqueAndLiteral;
    procedure TitleRangesCollectAllLevelsInSourceOrder;
    procedure TitleRangesUseNestedStackAndIgnoreMalformedCase;
    procedure MalformedTitlesAreProtectedWithoutTitlePresentation;
    procedure TitleRangesProtectTextButAllowBodyAtEnd;
    procedure TitleEditingOperationsAreAtomicAndLossless;
    procedure TitleWrapUnwrapPreservesTextFormattingAndRaw;
    procedure TitleWrapOrdersConsecutiveAndFontBoundaries;
    procedure TitleWrapSplitsPartialFormattingUtf16AndNesting;
    procedure SemanticEditOutsideTitlePreservesLiteralRaw;
    procedure StrongAndMorphologyItemsKeepOrderAndRawLexemes;
    procedure MalformedStrongIsOpaque;
    procedure NewSemanticMarksUseCanonicalLexemes;
    procedure ImportedAndNewMarksSerializeTogether;
    procedure SyntagmBoundaryIsInvisibleStructuralMetadata;
    procedure SyntagmBoundaryProtectionAndOffsetsSurviveEditing;
    procedure ReplaceVisibleTextUpdatesSemanticOffsets;
    procedure ReplaceVisibleTextIsUtf16Safe;
    procedure ReplaceRefusesProtectedLexicalAndOpaqueContent;
    procedure CrossingMarksSerializeBalancedAndDeterministically;
    procedure CrossingSemanticTransitionsKeepLexicalTagsWithWord;
    procedure CrossingSemanticTransitionStopsLexicalPromotionAtOpaque;
    procedure UnmodifiedCrossingMarkupRoundTripsLiterally;
    procedure ImportedSemanticEventOrderSurvivesOutsideEdits;
    procedure OpaqueEventOrdinalSurvivesSemanticEdit;
    procedure ModifiedEncodeDecodeRestoresTheDocument;
    procedure ModulePreservesBomNewlinesAndOrderedRecords;
    procedure ActiveRulesRequireRawEditing;
    procedure DisabledRulesDoNotRequireRawEditing;
    procedure UnknownVerseTagsProduceProtectedRegions;
    procedure EditingVerseKeepsUntouchedModuleRecordsLiteral;
    procedure MixedLineEndingsAreReportedAndRoundTrip;
    procedure NonUtf8InputIsReportedAndRoundTripsUnchanged;
    procedure AsciiDeclaredWindows1252OverridesUtf8Heuristic;
    procedure Windows1252BytesDecodeAndRoundTrip;
    procedure UnsupportedDeclaredCharsetBlocksEditing;
    procedure DeclaredCodePageRejectsUnrepresentableEdit;
    procedure TranslatorNotesAreInvisibleAndRoundTripLiterally;
    procedure TranslatorNoteBodyCapabilitiesMatchTheWordSubset;
    procedure NestedAndUnclosedTranslatorNotesAreRawOnlyAndHidden;
    procedure TranslatorNoteCrudPreservesLexicalOrderAndVisibleText;
    procedure InlineFormatSimpleTagsHonorCaseRules;
    procedure InlineFormatFontAttributesAreLossless;
    procedure InlineFormatColorsResolveToBgr;
    procedure InlineFormatSizesAndMalformedTagsAreRejected;
    procedure InlineFormatFontNameSpansUseUtf16CodeUnits;
    procedure FontMarksAreFirstClassLosslessAndStrict;
    procedure FontMarkLexemesOwnPairedEventSerialization;
    procedure FontRangesShiftUtf16AndKeepLexicalPostfixes;
    procedure FontTopologyBuilderIsPureUtf16SafeAndCrossingAware;
    procedure FontTopologyBuilderPreservesSameAnchorAffinity;
    procedure FontTopologyBuilderPreservesImportedZeroOutsideSelection;
    procedure FontTopologyBuilderPreservesNestedSeamPayload;
  end;

implementation

const
  BOM: RawByteString = #$EF#$BB#$BF;

procedure TTheWordOntCodecV2Tests.SetUp;
begin
  inherited SetUp;
  FCodec := TTheWordOntCodecV2.Create;
end;

procedure TTheWordOntCodecV2Tests.TearDown;
begin
  FCodec := nil;
  inherited TearDown;
end;

procedure TTheWordOntCodecV2Tests.AssertUnicodeEquals(const AMessage: string;
  const AExpected, AActual: UnicodeString);
begin
  AssertTrue(AMessage + ': expected "' + UTF8Encode(AExpected) +
    '" but got "' + UTF8Encode(AActual) + '"', AExpected = AActual);
end;

procedure TTheWordOntCodecV2Tests.AssertRawBytesEqual(const AMessage: string;
  const AExpected, AActual: RawByteString);
var
  Index: Integer;
begin
  AssertEquals(AMessage + ' length', Length(AExpected), Length(AActual));
  for Index := 1 to Length(AExpected) do
    if Byte(AExpected[Index]) <> Byte(AActual[Index]) then
      Fail(Format('%s: byte %d expected %.2x but got %.2x',
        [AMessage, Index, Byte(AExpected[Index]), Byte(AActual[Index])]));
end;

function TTheWordOntCodecV2Tests.FindMark(
  ADocument: TTheWordVerseDocument; AKind: TTheWordSemanticKind):
  TTheWordSemanticMark;
var
  Index: Integer;
begin
  for Index := 0 to ADocument.MarkCount - 1 do
    if ADocument.Marks[Index].Kind = AKind then
      Exit(ADocument.Marks[Index]);
  Fail('Semantic mark not found: ' + IntToStr(Ord(AKind)));
  Result := nil;
end;

function TTheWordOntCodecV2Tests.InlineSpanText(const ARaw: UnicodeString;
  const ASpanUTF16: TTheWordUTF16SpanV2): UnicodeString;
begin
  Result := Copy(ARaw, ASpanUTF16.StartUTF16,
    ASpanUTF16.LengthUTF16);
end;

procedure TTheWordOntCodecV2Tests.SemanticTagsAreStrictlyCaseSensitive;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := '<FI>known<Fi> <fi>lowercase</fi>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', 'known lowercase',
      Document.VisibleText);
    AssertEquals('known mark count', 1, Document.MarkCount);
    AssertEquals('unknown lowercase tokens', 2, Document.OpaqueItemCount);
    AssertEquals('capability', Ord(twlcProtectedRegions),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeVerse(Document)));
    AssertUnicodeEquals('literal round trip', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.HtmlSemanticTagsAreCaseInsensitiveAndSafe;
var
  Document: TTheWordVerseDocument;
  Mark: TTheWordSemanticMark;
  Raw: UnicodeString;
begin
  Raw := '<FI>fi<Fi><B>bold <I>both</i> tail</b><U>under</U>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', 'fibold both tailunder',
      Document.VisibleText);
    AssertEquals('FI and HTML marks stay distinct', 4, Document.MarkCount);

    Mark := FindMark(Document, twsmItalic);
    AssertUnicodeEquals('FI remains distinct', '<FI>', Mark.OpenLexeme);

    Mark := FindMark(Document, twsmHtmlBold);
    AssertUnicodeEquals('bold opening lexeme is preserved', '<B>',
      Mark.OpenLexeme);
    AssertUnicodeEquals('bold closing lexeme is preserved', '</b>',
      Mark.CloseLexeme);

    Mark := FindMark(Document, twsmHtmlItalic);
    AssertUnicodeEquals('nested italic opening is preserved', '<I>',
      Mark.OpenLexeme);
    AssertUnicodeEquals('nested italic closing is preserved', '</i>',
      Mark.CloseLexeme);

    AssertUnicodeEquals('case-insensitive nested markup is a no-op',
      Raw, FCodec.EncodeVerse(Document));
    Document.ReplaceVisibleText(Length(Document.VisibleText), 0, '!');
    AssertUnicodeEquals('outside edit preserves imported spellings',
      Raw + '!', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Raw := '<S>strike</s><SuP>up</sUP><SUB>down</SuB>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertEquals('S SUP SUB are uniformly case-insensitive', 3,
      Document.MarkCount);
    AssertEquals(0, Document.OpaqueItemCount);
    AssertUnicodeEquals('mixed-case S SUP SUB no-op is literal', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  Document := FCodec.DecodeVerse('bold italic under');
  try
    Mark := Document.AddSemanticMark(twsmHtmlBold, 0, 4);
    AssertUnicodeEquals('canonical bold opening', '<b>', Mark.OpenLexeme);
    AssertUnicodeEquals('canonical bold closing', '</b>', Mark.CloseLexeme);
    Mark := Document.AddSemanticMark(twsmHtmlItalic, 5, 11);
    AssertUnicodeEquals('canonical italic opening', '<i>', Mark.OpenLexeme);
    AssertUnicodeEquals('canonical italic closing', '</i>', Mark.CloseLexeme);
    Mark := Document.AddSemanticMark(twsmHtmlUnderline, 12, 17);
    AssertUnicodeEquals('canonical underline opening', '<u>',
      Mark.OpenLexeme);
    AssertUnicodeEquals('canonical underline closing', '</u>',
      Mark.CloseLexeme);
    AssertUnicodeEquals('canonical HTML serialization',
      '<b>bold</b> <i>italic</i> <u>under</u>',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Raw := 'a<b class="x">b</b x><i/>c<u />d</i>e';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('unsupported HTML remains outside visible text',
      'abcde', Document.VisibleText);
    AssertEquals('attributes and self-closing tags are opaque', 5,
      Document.OpaqueItemCount);
    AssertEquals('unmatched known close is malformed',
      Ord(twokMalformedTag), Ord(Document.OpaqueItems[4].Kind));
    AssertUnicodeEquals('opaque HTML remains literal', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Raw := 'A<RF><B>note <I>body</i></B><Rf>B';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('RF body is isolated from verse semantics', 'AB',
      Document.VisibleText);
    AssertEquals('RF body creates no verse semantic marks', 0,
      Document.MarkCount);
    AssertEquals('one isolated RF body', 1, Document.TranslatorNoteCount);
    AssertUnicodeEquals('RF body stays literal',
      '<B>note <I>body</i></B>',
      Document.TranslatorNotes[0].BodyRaw);
    AssertUnicodeEquals('RF isolation round trips literally', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.AllSevenSemanticPairsBecomeMarks;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := '<FI>i<Fi><FR>r<Fr><FO>o<Fo><FU>u<Fu>' +
    '<s>s</s><sup>p</sup><sub>b</sub>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', 'irouspb', Document.VisibleText);
    AssertEquals('mark count', 7, Document.MarkCount);
    AssertEquals('italic', Ord(twsmItalic), Ord(Document.Marks[0].Kind));
    AssertEquals('red words', Ord(twsmWordsOfJesus),
      Ord(Document.Marks[1].Kind));
    AssertEquals('old testament quote', Ord(twsmOldTestamentQuote),
      Ord(Document.Marks[2].Kind));
    AssertEquals('underline', Ord(twsmUnderline),
      Ord(Document.Marks[3].Kind));
    AssertEquals('strikethrough', Ord(twsmStrikethrough),
      Ord(Document.Marks[4].Kind));
    AssertEquals('superscript', Ord(twsmSuperscript),
      Ord(Document.Marks[5].Kind));
    AssertEquals('subscript', Ord(twsmSubscript),
      Ord(Document.Marks[6].Kind));
    AssertEquals('italic starts at zero', 0,
      Document.Marks[0].StartOffset);
    AssertEquals('italic ends at one', 1, Document.Marks[0].EndOffset);
    AssertEquals('no opaque items', 0, Document.OpaqueItemCount);
    AssertEquals('capability', Ord(twlcFullWysiwyg),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeVerse(Document)));
    AssertUnicodeEquals('round trip', Raw, FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.LessThanWithoutClosingBracketRemainsText;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := 'plain <FI and still text';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', Raw, Document.VisibleText);
    AssertEquals('no opaque token', 0, Document.OpaqueItemCount);
    AssertEquals('full capability', Ord(twlcFullWysiwyg),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeVerse(Document)));
    AssertUnicodeEquals('round trip', Raw, FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.UnknownAndMalformedTagsAreOpaqueAndLiteral;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := '<X data=">">a<Fi>b<FI>c';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', 'abc', Document.VisibleText);
    AssertEquals('unknown plus unmatched close and open', 3,
      Document.OpaqueItemCount);
    AssertEquals('unknown kind', Ord(twokUnknownTag),
      Ord(Document.OpaqueItems[0].Kind));
    AssertEquals('unmatched close kind', Ord(twokMalformedTag),
      Ord(Document.OpaqueItems[1].Kind));
    AssertEquals('unclosed open kind', Ord(twokMalformedTag),
      Ord(Document.OpaqueItems[2].Kind));
    AssertUnicodeEquals('literal round trip', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.TitleRangesCollectAllLevelsInSourceOrder;
var
  Document: TTheWordVerseDocument;
  Ranges: TTheWordTitleRangeArray;
begin
  Document := FCodec.DecodeVerse(
    '<TS>A<Ts> <TS0>B<Ts> <TS1>C<Ts> <TS2>D<Ts> <TS3>E<Ts>');
  try
    AssertEquals('all title levels collected', 5,
      CollectTheWordTitleRanges(Document, Ranges));
    AssertEquals('TS start', 0, Ranges[0].StartOffset);
    AssertEquals('TS end', 1, Ranges[0].EndOffset);
    AssertEquals('TS default level', 1, Ranges[0].Level);
    AssertEquals('TS open ordinal', 0, Ranges[0].OpenOrdinal);
    AssertEquals('TS close ordinal', 1, Ranges[0].CloseOrdinal);
    AssertUnicodeEquals('TS lexeme retained', '<TS>',
      Ranges[0].OpenLexeme);
    AssertTrue('simple TS title is editable', Ranges[0].Editable);
    AssertTrue('simple TS0 title is editable', Ranges[1].Editable);
    AssertTrue('simple TS1 title is editable', Ranges[2].Editable);
    AssertEquals('TS0 level', 0, Ranges[1].Level);
    AssertEquals('TS1 level', 1, Ranges[2].Level);
    AssertEquals('TS2 level', 2, Ranges[3].Level);
    AssertEquals('TS3 level', 3, Ranges[4].Level);
    AssertEquals('last title start follows visible separators', 8,
      Ranges[4].StartOffset);
    AssertEquals('last title end', 9, Ranges[4].EndOffset);
    AssertTrue('ranges retain source order',
      (Ranges[0].OpenOrdinal < Ranges[1].OpenOrdinal) and
      (Ranges[1].OpenOrdinal < Ranges[2].OpenOrdinal) and
      (Ranges[2].OpenOrdinal < Ranges[3].OpenOrdinal) and
      (Ranges[3].OpenOrdinal < Ranges[4].OpenOrdinal));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.TitleRangesUseNestedStackAndIgnoreMalformedCase;
var
  Document: TTheWordVerseDocument;
  Ranges: TTheWordTitleRangeArray;
  Raw: UnicodeString;
begin
  Raw := '<TS>A<TS2>B<Ts>C<Ts> <ts>x</ts> <TS4>y<Ts> <TS>open';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertEquals('balanced and malformed title regions are delimited', 5,
      CollectTheWordTitleRanges(Document, Ranges));
    AssertEquals('outer title is first by opening order', 0,
      Ranges[0].StartOffset);
    AssertEquals('outer title includes nested title text', 3,
      Ranges[0].EndOffset);
    AssertEquals('outer level', 1, Ranges[0].Level);
    AssertFalse('nested outer title remains protected', Ranges[0].Editable);
    AssertFalse('nested inner title remains protected', Ranges[1].Editable);
    AssertEquals('inner title start', 1, Ranges[1].StartOffset);
    AssertEquals('inner title end', 2, Ranges[1].EndOffset);
    AssertEquals('inner level', 2, Ranges[1].Level);
    AssertEquals('wrong-case title is a protected sentinel', -1,
      Ranges[2].Level);
    AssertFalse('wrong-case title body is not editable', Ranges[2].Editable);
    AssertEquals(4, Ranges[2].StartOffset);
    AssertEquals(5, Ranges[2].EndOffset);
    AssertEquals('unsupported TS level is a protected sentinel', -1,
      Ranges[3].Level);
    AssertFalse('unsupported TS body is not editable', Ranges[3].Editable);
    AssertEquals(6, Ranges[3].StartOffset);
    AssertEquals(7, Ranges[3].EndOffset);
    AssertEquals('unclosed title is a protected sentinel', -1,
      Ranges[4].Level);
    AssertFalse('unclosed title body is not editable', Ranges[4].Editable);
    AssertEquals(8, Ranges[4].StartOffset);
    AssertEquals(12, Ranges[4].EndOffset);
    AssertFalse('wrong-case title requires Raw editing',
      Document.CanReplaceVisibleText(4, 1));
    AssertFalse('unsupported title level requires Raw editing',
      Document.CanReplaceVisibleText(6, 1));
    AssertFalse('unclosed title requires Raw editing',
      Document.CanReplaceVisibleText(8, 4));
    AssertTrue('text safely outside malformed pairs remains editable',
      Document.CanReplaceVisibleText(3, 1));
    AssertTrue('ranges retain source order',
      (Ranges[0].OpenOrdinal < Ranges[1].OpenOrdinal) and
      (Ranges[1].OpenOrdinal < Ranges[2].OpenOrdinal) and
      (Ranges[2].OpenOrdinal < Ranges[3].OpenOrdinal) and
      (Ranges[3].OpenOrdinal < Ranges[4].OpenOrdinal));
    AssertEquals('all title-looking tokens remain opaque', 9,
      Document.OpaqueItemCount);
    AssertUnicodeEquals('malformed and wrong-case lexemes remain literal',
      Raw, FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  MalformedTitlesAreProtectedWithoutTitlePresentation;
var
  Document: TTheWordVerseDocument;
  Span: TVisualSpan;
  Visual: IVerseVisualDocument;
begin
  Document := FCodec.DecodeVerse('pre <TS4>bad<Ts> tail');
  try
    Visual := BuildTheWordVerseVisualDocument(Document, tdLeftToRight, 1);
    AssertUnicodeEquals('malformed title adds no title line breaks',
      'pre bad tail', Visual.Text);
    AssertTrue('malformed body remains represented visually',
      Visual.TryGetSpanAt(4, Span));
    AssertFalse('malformed body is protected in visual mode',
      Span.Interactive);
    AssertEquals('malformed body uses protected title-content role',
      Ord(vsrTitleContent), Ord(Span.Role));
    AssertTrue('malformed body is identified without title styling',
      Pos('malformed-title', Span.Annotation) > 0);
    AssertTrue('following safe body remains represented visually',
      Visual.TryGetSpanAt(8, Span));
    AssertTrue('following safe body remains editable', Span.Interactive);
    AssertEquals('following body is ordinary content', Ord(vsrContent),
      Ord(Span.Role));
  finally
    Visual := nil;
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.TitleRangesProtectTextButAllowBodyAtEnd;
var
  Document: TTheWordVerseDocument;
  Ranges: TTheWordTitleRangeArray;
  Rejected: Boolean;
begin
  Document := FCodec.DecodeVerse('<TS>Heading<Ts>body');
  try
    AssertEquals('one protected title', 1,
      CollectTheWordTitleRanges(Document, Ranges));
    AssertFalse('exact title replacement is protected',
      Document.CanReplaceVisibleText(0, 7));
    AssertFalse('insertion inside title is protected',
      Document.CanReplaceVisibleText(3, 0));
    AssertFalse('range crossing title end is protected',
      Document.CanReplaceVisibleText(6, 2));

    Rejected := False;
    try
      Document.ReplaceVisibleText(0, 7, 'Other');
    except
      on E: ETheWordOntProtectedEdit do
        Rejected := True;
    end;
    AssertTrue('exact title replacement raises protected edit', Rejected);
    AssertFalse('rejected title replacement is atomic', Document.Modified);

    AssertTrue('body begins at the title end offset',
      Document.CanReplaceVisibleText(7, 0));
    Document.ReplaceVisibleText(7, 0, 'New ');
    AssertUnicodeEquals('body insertion does not enter title',
      '<TS>Heading<Ts>New body', FCodec.EncodeVerse(Document));
    AssertEquals('title end remains before inserted body', 1,
      CollectTheWordTitleRanges(Document, Ranges));
    AssertEquals('title range did not absorb body insertion', 7,
      Ranges[0].EndOffset);
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.TitleEditingOperationsAreAtomicAndLossless;
var
  Document: TTheWordVerseDocument;
  OpenOrdinal: Integer;
  Ranges: TTheWordTitleRangeArray;
  Replacement: UnicodeString;
  UnicodeLeft, UnicodeRight: UnicodeString;
begin
  Document := FCodec.DecodeVerse('left<TS1>Head<Ts>right');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Ranges[0].Editable);
    AssertUnicodeEquals('imported TS1 lexeme is retained', '<TS1>',
      Ranges[0].OpenLexeme);
    AssertTrue(Document.ReplaceTitleVisibleText(Ranges[0].OpenOrdinal,
      0, 4, 'Title'));
    AssertUnicodeEquals('title text edit keeps imported lexemes',
      'left<TS1>Title<Ts>right', FCodec.EncodeVerse(Document));
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('same effective level is a literal no-op',
      Document.ChangeTitleLevel(Ranges[0].OpenOrdinal, 1));
    AssertUnicodeEquals('TS1 survives a level-1 no-op',
      'left<TS1>Title<Ts>right', FCodec.EncodeVerse(Document));
    AssertTrue(Document.ChangeTitleLevel(Ranges[0].OpenOrdinal, 2));
    AssertUnicodeEquals('changed level uses canonical TS2',
      'left<TS2>Title<Ts>right', FCodec.EncodeVerse(Document));
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Document.RemoveTitle(Ranges[0].OpenOrdinal, Replacement));
    AssertUnicodeEquals('touching body fragments receive one separator',
      'left right', FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('reported replacement matches inserted separator',
      ' ', Replacement);
  finally
    Document.Free;
  end;
  Document := FCodec.DecodeVerse('foo<TS>Title<Ts>,bar');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Document.RemoveTitle(Ranges[0].OpenOrdinal, Replacement));
    AssertUnicodeEquals('punctuation on the right needs no separator',
      'foo,bar', FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('punctuation removal reports no separator', '',
      Replacement);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('foo,<TS>Title<Ts>bar');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Document.RemoveTitle(Ranges[0].OpenOrdinal, Replacement));
    AssertUnicodeEquals('punctuation on the left needs no separator',
      'foo,bar', FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('left punctuation reports no separator', '',
      Replacement);
  finally
    Document.Free;
  end;

  UnicodeLeft := 'caf' + UnicodeString(WideChar($00E9));
  UnicodeRight := UnicodeString(WideChar($00F3)) + 'rg' +
    WideChar($00E3) + 'o';
  Document := FCodec.DecodeVerse(UnicodeLeft + '<TS>Title<Ts>' + UnicodeRight);
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Document.RemoveTitle(Ranges[0].OpenOrdinal, Replacement));
    AssertUnicodeEquals('Unicode letters on both sides receive one separator',
      UnicodeLeft + ' ' + UnicodeRight, FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('Unicode word join reports exactly one separator',
      ' ', Replacement);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('left right');
  try
    AssertTrue('title can be inserted at an empty safe boundary',
      Document.InsertTitle(5, 3, 'Heading', OpenOrdinal));
    AssertTrue(OpenOrdinal >= 0);
    AssertUnicodeEquals('new level 3 title uses canonical tags at caret',
      'left <TS3>Heading<Ts>right', FCodec.EncodeVerse(Document));
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Ranges[0].Editable);
    AssertEquals(OpenOrdinal, Ranges[0].OpenOrdinal);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('left<TS><Ts>right');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('a simple empty title remains explicitly removable',
      Ranges[0].Editable);
    AssertTrue(Document.RemoveTitle(Ranges[0].OpenOrdinal, Replacement));
    AssertUnicodeEquals('empty wrapper removal joins touching words once',
      'left right', FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('empty wrapper reports the joining space', ' ',
      Replacement);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<TS><Ts>');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Document.RemoveTitle(Ranges[0].OpenOrdinal, Replacement));
    AssertUnicodeEquals('standalone empty wrapper is serialized away', '',
      FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('standalone empty wrapper needs no separator', '',
      Replacement);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('a <TS0>Legacy<Ts> b');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertEquals('legacy TS0 keeps its distinct imported level', 0,
      Ranges[0].Level);
    AssertUnicodeEquals('legacy TS0 round-trips literally',
      'a <TS0>Legacy<Ts> b', FCodec.EncodeVerse(Document));
    AssertTrue(Document.ChangeTitleLevel(Ranges[0].OpenOrdinal, 1));
    AssertUnicodeEquals('editing TS0 to level 1 uses canonical TS',
      'a <TS>Legacy<Ts> b', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<TS>A<TS2>B<Ts>C<Ts> body');
  try
    AssertEquals(2, CollectTheWordTitleRanges(Document, Ranges));
    AssertFalse(Ranges[0].Editable);
    AssertFalse(Ranges[1].Editable);
    AssertFalse('nested title cannot be edited',
      Document.ReplaceTitleVisibleText(Ranges[0].OpenOrdinal, 0, 1, 'X'));
    AssertFalse('nested title cannot change level',
      Document.ChangeTitleLevel(Ranges[1].OpenOrdinal, 3));
    AssertFalse('nested title cannot be removed',
      Document.RemoveTitle(Ranges[0].OpenOrdinal, Replacement));
    AssertUnicodeEquals('rejected nested edits are atomic',
      '<TS>A<TS2>B<Ts>C<Ts> body', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<TS><i>Styled</i><Ts> body');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('well-formed formatting inside a title stays editable',
      Ranges[0].Editable);
    AssertTrue(Document.ReplaceTitleVisibleText(Ranges[0].OpenOrdinal,
      0, 6, 'Plain'));
    AssertUnicodeEquals('editing formatted title text retains its style',
      '<TS><i>Plain</i><Ts> body', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  TitleWrapUnwrapPreservesTextFormattingAndRaw;
const
  ORIGINAL: UnicodeString =
    'pre <FI><font color=red>A</font>B<Fi> post';
  WRAPPED: UnicodeString =
    'pre <TS2><FI><font color=red>A</font>B<Fi><Ts> post';
var
  Document: TTheWordVerseDocument;
  OpenOrdinal: Integer;
  Ranges: TTheWordTitleRangeArray;
begin
  Document := FCodec.DecodeVerse(ORIGINAL);
  try
    AssertTrue('a formatted existing selection can become a title',
      Document.CanWrapTitle(4, 2));
    AssertTrue(Document.WrapTitle(4, 2, 2, OpenOrdinal));
    AssertUnicodeEquals('wrapping changes tags only', 'pre AB post',
      Document.VisibleText);
    AssertUnicodeEquals('TS encloses semantic and font wrappers exactly',
      WRAPPED, FCodec.EncodeVerse(Document));
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('wrapped formatted title remains editable',
      Ranges[0].Editable);
    AssertEquals(OpenOrdinal, Ranges[0].OpenOrdinal);
    AssertTrue(Document.UnwrapTitle(OpenOrdinal));
    AssertUnicodeEquals('unwrap preserves all selected content and lexemes',
      ORIGINAL, FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('unwrap never changes visible text', 'pre AB post',
      Document.VisibleText);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('A<WG123>');
  try
    AssertFalse('lexical postfix remains protected from title wrapping',
      Document.CanWrapTitle(0, 1));
    AssertFalse(Document.WrapTitle(0, 1, 1, OpenOrdinal));
    AssertUnicodeEquals('rejected protected wrap is atomic', 'A<WG123>',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    '<FI><font color=red><TS>AB<Ts>C</font><Fi>');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('semantic and font marks may envelop an editable title',
      Ranges[0].Editable);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<TS>AB<Ts>C');
  try
    Document.AddSemanticMark(twsmItalic, 1, 3);
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertFalse('a newly authored partial semantic crossing protects title',
      Ranges[0].Editable);
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  TitleWrapOrdersConsecutiveAndFontBoundaries;
var
  Document: TTheWordVerseDocument;
  OpenOrdinal: Integer;
  Ranges: TTheWordTitleRangeArray;
begin
  Document := FCodec.DecodeVerse('A<TS1>B<Ts>C');
  try
    AssertTrue(Document.WrapTitle(0, 1, 2, OpenOrdinal));
    AssertUnicodeEquals('new close precedes the next title opener',
      '<TS2>A<Ts><TS1>B<Ts>C', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<TS1>A<Ts>BC');
  try
    AssertTrue(Document.WrapTitle(1, 1, 2, OpenOrdinal));
    AssertUnicodeEquals('new open follows the previous title closer',
      '<TS1>A<Ts><TS2>B<Ts>C', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('   body');
  try
    AssertFalse('a whitespace-only selection cannot become a title',
      Document.CanWrapTitle(0, 3));
    AssertFalse(Document.WrapTitle(0, 3, 1, OpenOrdinal));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('Head');
  try
    AssertFalse('a new title must leave ordinary Bible text after it',
      Document.CanWrapTitle(0, 4));
    AssertFalse('legacy insertion at the terminal caret follows the same rule',
      Document.CanInsertTitle(4));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<TS>Head<Ts>');
  try
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('an imported terminal title remains editable',
      Document.ChangeTitleLevel(Ranges[0].OpenOrdinal, 2));
    AssertUnicodeEquals('imported terminal title level changes in place',
      '<TS2>Head<Ts>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    '<font color=red>L</font>A<font size=+1>R</font>');
  try
    AssertTrue(Document.WrapTitle(1, 1, 1, OpenOrdinal));
    AssertUnicodeEquals('left font closes before TS and right font opens after',
      '<font color=red>L</font><TS>A<Ts><font size=+1>R</font>',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<font color=red>A</font> body');
  try
    AssertTrue(Document.WrapTitle(0, 1, 1, OpenOrdinal));
    AssertUnicodeEquals('font belonging to selection stays inside TS',
      '<TS><font color=red>A</font><Ts> body',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<font color=red>ABC</font>');
  try
    AssertTrue(Document.WrapTitle(1, 1, 1, OpenOrdinal));
    AssertUnicodeEquals('font enveloping selection stays outside TS',
      '<font color=red>A<TS>B<Ts>C</font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  TitleWrapSplitsPartialFormattingUtf16AndNesting;
const
  FONT_OPEN: UnicodeString =
    '<font color="Red" size=+1 name=''Tahoma''>';
var
  Document: TTheWordVerseDocument;
  Emoji, ExpectedRaw, Raw: UnicodeString;
  Index, OpenOrdinal: Integer;
  Ranges: TTheWordTitleRangeArray;
begin
  Document := FCodec.DecodeVerse('<B>ABCD</b> EF');
  try
    AssertTrue('a supported semantic mark may cross the left TS boundary',
      Document.CanWrapTitle(2, 4));
    AssertTrue(Document.WrapTitle(2, 4, 2, OpenOrdinal));
    AssertUnicodeEquals('semantic lexemes are cloned around the TS seam',
      '<B>AB</b><TS2><B>CD</b> E<Ts>F',
      FCodec.EncodeVerse(Document));
    AssertEquals('the crossing bold mark is split into two ranges',
      2, Document.MarkCount);
    for Index := 0 to Document.MarkCount - 1 do
    begin
      AssertEquals(Ord(twsmHtmlBold), Ord(Document.Marks[Index].Kind));
      AssertUnicodeEquals('opening spelling survives fission', '<B>',
        Document.Marks[Index].OpenLexeme);
      AssertUnicodeEquals('closing spelling survives fission', '</b>',
        Document.Marks[Index].CloseLexeme);
      AssertTrue('import provenance survives fission',
        Document.Marks[Index].Imported);
    end;
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('the resulting title is balanced and editable',
      Ranges[0].Editable);
  finally
    Document.Free;
  end;

  Raw := '<font color=red><i>ABCD</i>E</font>F G';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertTrue('nested semantic and font ranges may share a partial seam',
      Document.CanWrapTitle(2, 4));
    AssertTrue('nested semantic/font seam is wrapped transactionally',
      Document.WrapTitle(2, 4, 2, OpenOrdinal));
    ExpectedRaw := '<font color=red><i>AB</i></font>' +
      '<TS2><font color=red><i>CD</i>E</font>F<Ts> G';
    AssertUnicodeEquals(
      'decoder must not recombine a semantic mark across a TS boundary',
      ExpectedRaw, FCodec.EncodeVerse(Document));
    AssertEquals(2, Document.MarkCount);
    AssertEquals(2, Document.FontMarkCount);
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue('nested split result remains a normal editable title',
      Ranges[0].Editable);
  finally
    Document.Free;
  end;

  Raw := 'A<i>B' + FONT_OPEN + 'CDEF</font>G</i> H';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertTrue('nested semantic and font presentation may cross TS',
      Document.CanWrapTitle(1, 4));
    AssertTrue(Document.WrapTitle(1, 4, 1, OpenOrdinal));
    ExpectedRaw := 'A<i><TS>B' + FONT_OPEN + 'CDE</font><Ts>' +
      FONT_OPEN + 'F</font>G</i> H';
    AssertUnicodeEquals('font is split while the enclosing italic remains',
      ExpectedRaw, FCodec.EncodeVerse(Document));
    AssertEquals('enclosing italic remains one semantic range',
      1, Document.MarkCount);
    AssertEquals(Ord(twsmHtmlItalic), Ord(Document.Marks[0].Kind));
    AssertEquals('combined font attributes are cloned on both sides',
      2, Document.FontMarkCount);
    for Index := 0 to Document.FontMarkCount - 1 do
    begin
      AssertUnicodeEquals('all original font attributes remain literal',
        FONT_OPEN, Document.FontMarks[Index].OpenLexeme);
      AssertTrue(Document.FontMarks[Index].HasColor);
      AssertTrue(Document.FontMarks[Index].HasSize);
      AssertEquals(1, Document.FontMarks[Index].RelativeSize);
      AssertTrue(Document.FontMarks[Index].HasFontName);
    end;
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Ranges[0].Editable);
  finally
    Document.Free;
  end;

  Emoji := UnicodeString(WideChar($D83D)) + WideChar($DE00);
  Raw := '<i>A' + Emoji + 'B</i> CD';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertFalse('a title boundary cannot split a surrogate pair',
      Document.CanWrapTitle(2, 3));
    AssertTrue('UTF-16 selection around the complete emoji is valid',
      Document.CanWrapTitle(1, 4));
    AssertTrue(Document.WrapTitle(1, 4, 3, OpenOrdinal));
    ExpectedRaw := '<i>A</i><TS3><i>' + Emoji + 'B</i> <Ts>CD';
    AssertUnicodeEquals('UTF-16 fission keeps balanced raw and exact text',
      ExpectedRaw, FCodec.EncodeVerse(Document));
    AssertUnicodeEquals('emoji and visible code units are unchanged',
      'A' + Emoji + 'B CD', Document.VisibleText);
    AssertEquals(2, Document.MarkCount);
    AssertEquals(1, CollectTheWordTitleRanges(Document, Ranges));
    AssertTrue(Ranges[0].Editable);
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.SemanticEditOutsideTitlePreservesLiteralRaw;
var
  Document: TTheWordVerseDocument;
begin
  Document := FCodec.DecodeVerse('<TS1>Heading<Ts> body');
  try
    Document.AddSemanticMark(twsmItalic, 8, 12);
    AssertUnicodeEquals('literal title surrounds regenerated body markup',
      '<TS1>Heading<Ts> <FI>body<Fi>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.StrongAndMorphologyItemsKeepOrderAndRawLexemes;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := 'word<WG123><WH456s><WTV-AAI-3S l="lemma"><wt>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', 'word', Document.VisibleText);
    AssertEquals('lexical item count', 4, Document.LexicalItemCount);
    AssertEquals('greek strong', Ord(twlkStrongGreek),
      Ord(Document.LexicalItems[0].Kind));
    AssertEquals('hebrew strong', Ord(twlkStrongHebrew),
      Ord(Document.LexicalItems[1].Kind));
    AssertEquals('morphology', Ord(twlkMorphology),
      Ord(Document.LexicalItems[2].Kind));
    AssertEquals('word terminator', Ord(twlkWordTokenEnd),
      Ord(Document.LexicalItems[3].Kind));
    AssertUnicodeEquals('strong raw', '<WG123>',
      Document.LexicalItems[0].RawLexeme);
    AssertUnicodeEquals('strong value', '123',
      Document.LexicalItems[0].Value);
    AssertUnicodeEquals('morph raw', '<WTV-AAI-3S l="lemma">',
      Document.LexicalItems[2].RawLexeme);
    AssertTrue('ordinals retain source order',
      (Document.LexicalItems[0].Ordinal < Document.LexicalItems[1].Ordinal) and
      (Document.LexicalItems[1].Ordinal < Document.LexicalItems[2].Ordinal) and
      (Document.LexicalItems[2].Ordinal < Document.LexicalItems[3].Ordinal));
    AssertEquals('known lexical metadata is editable', Ord(twlcFullWysiwyg),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeVerse(Document)));
    AssertUnicodeEquals('literal round trip', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.MalformedStrongIsOpaque;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := 'word<WGabc><WH><WT>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', 'word', Document.VisibleText);
    AssertEquals('no malformed lexical items', 0, Document.LexicalItemCount);
    AssertEquals('all malformed tags are opaque', 3,
      Document.OpaqueItemCount);
    AssertEquals('malformed strong kind', Ord(twokMalformedTag),
      Ord(Document.OpaqueItems[0].Kind));
    AssertEquals('protected capability', Ord(twlcProtectedRegions),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeVerse(Document)));
    AssertUnicodeEquals('literal round trip', Raw, FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.NewSemanticMarksUseCanonicalLexemes;
var
  Document: TTheWordVerseDocument;
  Mark: TTheWordSemanticMark;
begin
  Document := FCodec.DecodeVerse('plain');
  try
    Mark := Document.AddSemanticMark(twsmUnderline, 0, 5);
    AssertFalse('new mark is not imported', Mark.Imported);
    AssertUnicodeEquals('canonical open', '<FU>', Mark.OpenLexeme);
    AssertUnicodeEquals('canonical close', '<Fu>', Mark.CloseLexeme);
    AssertUnicodeEquals('canonical output', '<FU>plain<Fu>',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ImportedAndNewMarksSerializeTogether;
var
  Document: TTheWordVerseDocument;
begin
  Document := FCodec.DecodeVerse('<FI>x<Fi> y');
  try
    Document.AddSemanticMark(twsmWordsOfJesus, 2, 3);
    AssertUnicodeEquals('existing lexemes and new canonical lexemes',
      '<FI>x<Fi> <FR>y<Fr>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.SyntagmBoundaryIsInvisibleStructuralMetadata;
var
  Document, Unterminated: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := '<FI>n|ele<Fi> a||b';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('boundaries are absent from visible text', 'nele ab',
      Document.VisibleText);
    AssertEquals('three explicit boundaries', 3,
      Document.StructuralItemCount);
    AssertEquals('first boundary offset', 1,
      Document.StructuralItems[0].Offset);
    AssertEquals('adjacent boundary offset one', 6,
      Document.StructuralItems[1].Offset);
    AssertEquals('adjacent boundary offset two', 6,
      Document.StructuralItems[2].Offset);
    AssertEquals('structural kind', Ord(twstSyntagmBoundary),
      Ord(Document.StructuralItems[0].Kind));
    AssertUnicodeEquals('structural lexeme retained', '|',
      Document.StructuralItems[0].RawLexeme);
    AssertEquals('known boundary remains full WYSIWYG', Ord(twlcFullWysiwyg),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeVerse(Document)));
    AssertUnicodeEquals('unmodified boundaries round trip literally', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Unterminated := FCodec.DecodeVerse('x<broken|tail');
  try
    AssertUnicodeEquals('tail after unterminated less-than remains text',
      'x<broken|tail', Unterminated.VisibleText);
    AssertEquals('pipe in unterminated text is not reinterpreted', 0,
      Unterminated.StructuralItemCount);
  finally
    Unterminated.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.SyntagmBoundaryProtectionAndOffsetsSurviveEditing;
var
  Document, Restored: TTheWordVerseDocument;
  Encoded: UnicodeString;
  Rejected: Boolean;
begin
  Document := FCodec.DecodeVerse('n|ele');
  try
    Rejected := False;
    try
      Document.ReplaceVisibleText(0, 2, 'x');
    except
      on E: ETheWordOntProtectedEdit do
        Rejected := True;
    end;
    AssertTrue('edit crossing Syntagm boundary rejected', Rejected);
    AssertFalse('rejected boundary edit is atomic', Document.Modified);

    Document.ReplaceVisibleText(0, 0, 'um ');
    AssertUnicodeEquals('safe prefix edit', 'um nele', Document.VisibleText);
    AssertEquals('boundary offset shifted with visible text', 4,
      Document.StructuralItems[0].Offset);
    Encoded := FCodec.EncodeVerse(Document);
    AssertUnicodeEquals('boundary lexeme reemitted at shifted anchor',
      'um n|ele', Encoded);

    Restored := FCodec.DecodeVerse(Encoded);
    try
      AssertUnicodeEquals('restored visible text', Document.VisibleText,
        Restored.VisibleText);
      AssertEquals('restored structural item count', 1,
        Restored.StructuralItemCount);
      AssertEquals('restored structural offset', 4,
        Restored.StructuralItems[0].Offset);
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ReplaceVisibleTextUpdatesSemanticOffsets;
var
  Document, Restored: TTheWordVerseDocument;
  Mark: TTheWordSemanticMark;
  Encoded: UnicodeString;
begin
  Document := FCodec.DecodeVerse('<FI>hello<Fi> world');
  try
    Document.ReplaceVisibleText(1, 3, 'i');
    AssertUnicodeEquals('replacement text', 'hio world',
      Document.VisibleText);
    Mark := FindMark(Document, twsmItalic);
    AssertEquals('mark start after inner replacement', 0,
      Mark.StartOffset);
    AssertEquals('mark end after inner replacement', 3,
      Mark.EndOffset);

    Document.ReplaceVisibleText(0, 0, 'Say ');
    AssertUnicodeEquals('inserted prefix', 'Say hio world',
      Document.VisibleText);
    AssertEquals('mark start shifted', 4, Mark.StartOffset);
    AssertEquals('mark end shifted', 7, Mark.EndOffset);
    Encoded := FCodec.EncodeVerse(Document);
    AssertUnicodeEquals('semantic output follows updated offsets',
      'Say <FI>hio<Fi> world', Encoded);

    Restored := FCodec.DecodeVerse(Encoded);
    try
      AssertUnicodeEquals('restored visible text', Document.VisibleText,
        Restored.VisibleText);
      Mark := FindMark(Restored, twsmItalic);
      AssertEquals('restored mark start', 4, Mark.StartOffset);
      AssertEquals('restored mark end', 7, Mark.EndOffset);
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ReplaceVisibleTextIsUtf16Safe;
var
  Document: TTheWordVerseDocument;
  Text, Emoji, BadReplacement: UnicodeString;
  Rejected: Boolean;
begin
  SetLength(Emoji, 2);
  Emoji[1] := WideChar($D83D);
  Emoji[2] := WideChar($DE00);
  Text := 'A' + Emoji + 'B';
  Document := FCodec.DecodeVerse(Text);
  try
    Rejected := False;
    try
      Document.ReplaceVisibleText(2, 0, 'x');
    except
      on E: ETheWordOntCodec do
        Rejected := True;
    end;
    AssertTrue('cannot insert inside a surrogate pair', Rejected);
    AssertFalse('failed edit is atomic', Document.Modified);

    SetLength(BadReplacement, 1);
    BadReplacement[1] := WideChar($D800);
    Rejected := False;
    try
      Document.ReplaceVisibleText(1, 2, BadReplacement);
    except
      on E: ETheWordOntCodec do
        Rejected := True;
    end;
    AssertTrue('unpaired replacement surrogate rejected', Rejected);
    AssertFalse('second failed edit is atomic', Document.Modified);

    Document.ReplaceVisibleText(1, 2, 'Z');
    AssertUnicodeEquals('whole surrogate pair can be replaced', 'AZB',
      Document.VisibleText);
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ReplaceRefusesProtectedLexicalAndOpaqueContent;
var
  Document: TTheWordVerseDocument;
  Rejected: Boolean;
begin
  Document := FCodec.DecodeVerse('word<WG123> tail');
  try
    Rejected := False;
    try
      Document.ReplaceVisibleText(1, 1, 'X');
    except
      on E: ETheWordOntProtectedEdit do
        Rejected := True;
    end;
    AssertTrue('lexically annotated word is protected', Rejected);

    Rejected := False;
    try
      Document.ReplaceVisibleText(4, 0, 's');
    except
      on E: ETheWordOntProtectedEdit do
        Rejected := True;
    end;
    AssertTrue('insertion into lexical anchor is protected', Rejected);
    AssertFalse('rejected lexical edits are atomic', Document.Modified);
    AssertUnicodeEquals('raw still literal after rejection',
      'word<WG123> tail', FCodec.EncodeVerse(Document));

    Document.ReplaceVisibleText(0, 0, 'A ');
    AssertEquals('safe edit shifts lexical anchor', 6,
      Document.LexicalItems[0].Offset);
    AssertUnicodeEquals('lexical raw survives safe shift',
      'A word<WG123> tail', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('a<X>b');
  try
    Rejected := False;
    try
      Document.ReplaceVisibleText(0, 2, 'z');
    except
      on E: ETheWordOntProtectedEdit do
        Rejected := True;
    end;
    AssertTrue('edit crossing opaque object rejected', Rejected);
    AssertFalse('rejected opaque edit is atomic', Document.Modified);
    Document.ReplaceVisibleText(0, 0, 'z');
    AssertEquals('safe edit shifts opaque anchor', 2,
      Document.OpaqueItems[0].Offset);
    AssertUnicodeEquals('opaque raw survives safe shift', 'za<X>b',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.CrossingMarksSerializeBalancedAndDeterministically;
var
  Document, Restored: TTheWordVerseDocument;
  Encoded: UnicodeString;
  Mark: TTheWordSemanticMark;
begin
  Document := FCodec.DecodeVerse('abc');
  try
    Document.AddSemanticMark(twsmItalic, 0, 2);
    Document.AddSemanticMark(twsmWordsOfJesus, 1, 3);
    Encoded := FCodec.EncodeVerse(Document);
    AssertUnicodeEquals('crossing range is closed and reopened',
      '<FI>a<FR>b<Fr><Fi><FR>c<Fr>', Encoded);
    AssertUnicodeEquals('serialization is deterministic', Encoded,
      FCodec.EncodeVerse(Document));

    Restored := FCodec.DecodeVerse(Encoded);
    try
      AssertEquals('reopened fragments normalize to two marks', 2,
        Restored.MarkCount);
      Mark := FindMark(Restored, twsmItalic);
      AssertEquals('italic start', 0, Mark.StartOffset);
      AssertEquals('italic end', 2, Mark.EndOffset);
      Mark := FindMark(Restored, twsmWordsOfJesus);
      AssertEquals('red start restored', 1, Mark.StartOffset);
      AssertEquals('red end restored', 3, Mark.EndOffset);
      AssertUnicodeEquals('serialized form itself round trips literally',
        Encoded, FCodec.EncodeVerse(Restored));
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.CrossingSemanticTransitionsKeepLexicalTagsWithWord;
const
  LEXICAL: UnicodeString = '<WG123><WTV-AAI-3S l="lemma"><wt>';
var
  Document: TTheWordVerseDocument;
  Encoded, Expected: UnicodeString;
  EndingKind, ReopenedKind: TTheWordSemanticKind;
  Index: Integer;
  Parser: TONTParser;
  Parsed: TSyntagmList;
  ParsedWord: TSyntagm;
begin
  Parser := TONTParser.Create;
  try
    for ReopenedKind := Low(TTheWordSemanticKind) to
      High(TTheWordSemanticKind) do
    begin
      if ReopenedKind = High(TTheWordSemanticKind) then
        EndingKind := Low(TTheWordSemanticKind)
      else
        EndingKind := TTheWordSemanticKind(Ord(ReopenedKind) + 1);

      Document := FCodec.DecodeVerse('ab' + LEXICAL + '<X>|c');
      try
        Document.AddSemanticMark(EndingKind, 0, 2);
        Document.AddSemanticMark(ReopenedKind, 1, 3);
        Encoded := FCodec.EncodeVerse(Document);
        Expected := TheWordCanonicalOpen(EndingKind) + 'a' +
          TheWordCanonicalOpen(ReopenedKind) + 'b' + LEXICAL +
          TheWordCanonicalClose(ReopenedKind) +
          TheWordCanonicalClose(EndingKind) + '<X>|' +
          TheWordCanonicalOpen(ReopenedKind) + 'c' +
          TheWordCanonicalClose(ReopenedKind);
        AssertUnicodeEquals('lexical events precede regenerated crossing ' +
          IntToStr(Ord(ReopenedKind)), Expected, Encoded);

        Parsed := Parser.ParseLine(UTF8Encode(Encoded), nil);
        try
          ParsedWord := nil;
          for Index := 0 to Parsed.Count - 1 do
            if (Parsed[Index].Kind = skSyntagm) and
               (Parsed[Index].Text = 'b') then
            begin
              ParsedWord := Parsed[Index];
              Break;
            end;
          AssertTrue('lexical owner word remains parsed',
            Assigned(ParsedWord));
          AssertEquals('Strong remains attached to word', 1,
            ParsedWord.Strong.Count);
          AssertEquals('morphology remains attached to word', 1,
            ParsedWord.Morph.Count);
        finally
          for Index := Parsed.Count - 1 downto 0 do
            Parsed[Index].Free;
          Parsed.Free;
        end;
      finally
        Document.Free;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.CrossingSemanticTransitionStopsLexicalPromotionAtOpaque;
const
  LEXICAL: UnicodeString = '<WG123><WTV-AAI-3S l="lemma"><wt>';

  procedure AssertParsedLexicalCounts(const AEncoded: UnicodeString;
    AStrongCount, AMorphCount: Integer);
  var
    Index: Integer;
    Parser: TONTParser;
    Parsed: TSyntagmList;
    ParsedWord: TSyntagm;
  begin
    Parser := TONTParser.Create;
    try
      Parsed := Parser.ParseLine(UTF8Encode(AEncoded), nil);
      try
        ParsedWord := nil;
        for Index := 0 to Parsed.Count - 1 do
          if (Parsed[Index].Kind = skSyntagm) and
             (Parsed[Index].Text = 'b') then
          begin
            ParsedWord := Parsed[Index];
            Break;
          end;
        if not Assigned(ParsedWord) then
          Fail('lexical owner word was not parsed');
        AssertEquals('Strong attachment count', AStrongCount,
          ParsedWord.Strong.Count);
        AssertEquals('morphology attachment count', AMorphCount,
          ParsedWord.Morph.Count);
      finally
        for Index := Parsed.Count - 1 downto 0 do
          Parsed[Index].Free;
        Parsed.Free;
      end;
    finally
      Parser.Free;
    end;
  end;

var
  Document: TTheWordVerseDocument;
  Encoded: UnicodeString;
begin
  Document := FCodec.DecodeVerse('ab<X>' + LEXICAL + 'c');
  try
    AssertTrue(Document.ToggleSemanticMark(twsmWordsOfJesus, 0, 2));
    AssertTrue(Document.ToggleSemanticMark(twsmItalic, 1, 3));
    Encoded := FCodec.EncodeVerse(Document);
    AssertUnicodeEquals('opaque boundary retains later lexical order',
      '<FR>a<FI>b<Fi><Fr><X>' + LEXICAL + '<FI>c<Fi>', Encoded);
    AssertParsedLexicalCounts(Encoded, 0, 0);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('ab' + LEXICAL + '<X>c');
  try
    AssertTrue(Document.ToggleSemanticMark(twsmWordsOfJesus, 0, 2));
    AssertTrue(Document.ToggleSemanticMark(twsmItalic, 1, 3));
    Encoded := FCodec.EncodeVerse(Document);
    AssertUnicodeEquals('leading lexical prefix remains attached',
      '<FR>a<FI>b' + LEXICAL + '<Fi><Fr><X><FI>c<Fi>', Encoded);
    AssertParsedLexicalCounts(Encoded, 1, 1);
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.UnmodifiedCrossingMarkupRoundTripsLiterally;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := '<FI>a<FR>b<Fi>c<Fr>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertEquals('crossing source has two semantic marks', 2,
      Document.MarkCount);
    AssertUnicodeEquals('unmodified source remains code-unit literal', Raw,
      FCodec.EncodeVerse(Document));
    Document.ReplaceVisibleText(1, 1, 'b');
    AssertFalse('no-op replacement keeps original representation',
      Document.Modified);
    AssertUnicodeEquals('no-op still preserves original crossing order', Raw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ImportedSemanticEventOrderSurvivesOutsideEdits;
const
  RAW_CASES: array[0..3] of UnicodeString = (
    '<FI>a<Fi><FI>b<Fi>',
    '<FR>a<Fr><FR>b<Fr>',
    '<FO>a<Fo><FO>b<Fo>',
    '<FU>a<Fu><FU>b<Fu>'
  );
var
  CaseIndex: Integer;
  Document: TTheWordVerseDocument;
begin
  for CaseIndex := Low(RAW_CASES) to High(RAW_CASES) do
  begin
    Document := FCodec.DecodeVerse(RAW_CASES[CaseIndex]);
    try
      AssertEquals('adjacent imported runs remain distinct', 2,
        Document.MarkCount);
      Document.ReplaceVisibleText(Length(Document.VisibleText), 0, '!');
      AssertUnicodeEquals('outside edit preserves imported event stream',
        RAW_CASES[CaseIndex] + '!', FCodec.EncodeVerse(Document));
    finally
      Document.Free;
    end;
  end;
end;

procedure TTheWordOntCodecV2Tests.OpaqueEventOrdinalSurvivesSemanticEdit;
var
  Document: TTheWordVerseDocument;
begin
  Document := FCodec.DecodeVerse('<FI>a<Fi><X> b');
  try
    Document.ReplaceVisibleText(Length(Document.VisibleText), 0, '!');
    AssertUnicodeEquals('outside edit does not move opaque object',
      '<FI>a<Fi><X> b!', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<FI>a<Fi><X> b');
  try
    Document.AddSemanticMark(twsmUnderline, 2, 3);
    AssertUnicodeEquals('new mark regenerates locally after opaque event',
      '<FI>a<Fi><X> <FU>b<Fu>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<FI><X>ab<Fi>');
  try
    Document.AddSemanticMark(twsmUnderline, 1, 2);
    AssertUnicodeEquals('nested edit keeps imported ordinal before opaque',
      '<FI><X>a<FU>b<Fu><Fi>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ModifiedEncodeDecodeRestoresTheDocument;
var
  Document, Restored: TTheWordVerseDocument;
  Encoded: UnicodeString;
  Kind: TTheWordSemanticKind;
  ExpectedMark, RestoredMark: TTheWordSemanticMark;
begin
  Document := FCodec.DecodeVerse(
    'word<WG123> <FI>a|<FR>b<Fi>c<Fr> <X>end');
  try
    Document.ReplaceVisibleText(0, 0, 'A ');
    Encoded := FCodec.EncodeVerse(Document);
    Restored := FCodec.DecodeVerse(Encoded);
    try
      AssertUnicodeEquals('visible text restored', Document.VisibleText,
        Restored.VisibleText);
      AssertEquals('mark count restored', Document.MarkCount,
        Restored.MarkCount);
      for Kind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
        if Kind in [twsmItalic, twsmWordsOfJesus] then
        begin
          ExpectedMark := FindMark(Document, Kind);
          RestoredMark := FindMark(Restored, Kind);
          AssertEquals('mark start restored', ExpectedMark.StartOffset,
            RestoredMark.StartOffset);
          AssertEquals('mark end restored', ExpectedMark.EndOffset,
            RestoredMark.EndOffset);
        end;
      AssertEquals('lexical count restored', Document.LexicalItemCount,
        Restored.LexicalItemCount);
      AssertEquals('lexical offset restored', Document.LexicalItems[0].Offset,
        Restored.LexicalItems[0].Offset);
      AssertUnicodeEquals('lexical raw restored',
        Document.LexicalItems[0].RawLexeme,
        Restored.LexicalItems[0].RawLexeme);
      AssertEquals('opaque count restored', Document.OpaqueItemCount,
        Restored.OpaqueItemCount);
      AssertEquals('opaque offset restored', Document.OpaqueItems[0].Offset,
        Restored.OpaqueItems[0].Offset);
      AssertUnicodeEquals('opaque raw restored',
        Document.OpaqueItems[0].RawLexeme,
        Restored.OpaqueItems[0].RawLexeme);
      AssertEquals('structural count restored', Document.StructuralItemCount,
        Restored.StructuralItemCount);
      AssertEquals('structural offset restored',
        Document.StructuralItems[0].Offset,
        Restored.StructuralItems[0].Offset);
      AssertUnicodeEquals('structural raw restored',
        Document.StructuralItems[0].RawLexeme,
        Restored.StructuralItems[0].RawLexeme);
      AssertUnicodeEquals('restored serialization is literal', Encoded,
        FCodec.EncodeVerse(Restored));
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ModulePreservesBomNewlinesAndOrderedRecords;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
begin
  Bytes := BOM + '<FI>x<Fi>' + #13#10 +
    'description=First' + #13#10 +
    'description=Second' + #13#10 +
    '#verse.rule="a" "b"' + #13#10 +
    '.tag.rule="a" "b"' + #13#10 +
    '  continuation text' + #13#10 +
    'charset=utf-8' + #13#10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertTrue('BOM detected', Module.HasBom);
    AssertEquals('encoding', Ord(twoeUtf8), Ord(Module.Encoding));
    AssertEquals('newline', Ord(twleCRLF), Ord(Module.NewLineStyle));
    AssertUnicodeEquals('preferred newline', #13#10,
      Module.PreferredNewLine);
    AssertUnicodeEquals('declared charset', 'utf-8',
      Module.DeclaredCharset);
    AssertEquals('record count', 6, Module.RecordCount);
    AssertUnicodeEquals('first repeated description', 'description=First',
      Module.Records[0].RawText);
    AssertUnicodeEquals('second repeated description', 'description=Second',
      Module.Records[1].RawText);
    AssertEquals('hash-disabled record', Ord(tworkDisabled),
      Ord(Module.Records[2].Kind));
    AssertEquals('dot-disabled record', Ord(tworkDisabled),
      Ord(Module.Records[3].Kind));
    AssertEquals('continuation record', Ord(tworkContinuation),
      Ord(Module.Records[4].Kind));
    AssertTrue('unmodified module is exact', FCodec.EncodeModule(Module) = Bytes);
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.ActiveRulesRequireRawEditing;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
begin
  Bytes := 'verse' + #10 + 'verse.rule="from" "to"' + #10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertTrue('active rule classified', Module.Records[0].IsActiveRule);
    AssertEquals('raw-only capability', Ord(twlcRawOnly),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeModule(Module)));
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.DisabledRulesDoNotRequireRawEditing;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
begin
  Bytes := 'verse' + #10 + '#verse.rule="from" "to"' + #10 +
    '.tag.rule="from" "to"' + #10 +
    ';verse.rule="from" "to"' + #10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertFalse('hash rule disabled', Module.Records[0].IsActiveRule);
    AssertFalse('dot rule disabled', Module.Records[1].IsActiveRule);
    AssertFalse('semicolon rule disabled', Module.Records[2].IsActiveRule);
    AssertEquals('public classifier shares semicolon semantics',
      Ord(tworkDisabled),
      Ord(TheWordClassifyOntRecord(';tag.rule="from" "to"')));
    AssertEquals('full capability', Ord(twlcFullWysiwyg),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeModule(Module)));
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.UnknownVerseTagsProduceProtectedRegions;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
begin
  Bytes := '<custom>x</custom>' + #10 + 'description=sample' + #10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertEquals('protected capability', Ord(twlcProtectedRegions),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeModule(Module)));
    AssertTrue('unknown tags remain exact', FCodec.EncodeModule(Module) = Bytes);
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.EditingVerseKeepsUntouchedModuleRecordsLiteral;
var
  Module: TTheWordOntModule;
  Bytes, Expected: RawByteString;
begin
  Bytes := BOM + 'word' + #13#10 + 'description=One' + #13#10 +
    'description=Two' + #13#10;
  Expected := BOM + '<FO>word<Fo>' + #13#10 +
    'description=One' + #13#10 + 'description=Two' + #13#10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    Module.Verses[0].AddSemanticMark(twsmOldTestamentQuote, 0, 4);
    AssertTrue('module reports edit', Module.Modified);
    AssertTrue('records remain literal', FCodec.EncodeModule(Module) = Expected);
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.MixedLineEndingsAreReportedAndRoundTrip;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
begin
  Bytes := 'verse' + #13#10 + 'description=sample' + #10 +
    'lang=en' + #13;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertEquals('mixed newline metadata', Ord(twleMixed),
      Ord(Module.NewLineStyle));
    AssertTrue('mixed newlines preserved', FCodec.EncodeModule(Module) = Bytes);
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.NonUtf8InputIsReportedAndRoundTripsUnchanged;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
begin
  Bytes := 'caf' + RawByteString(#$E9) + #13#10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertEquals('legacy encoding metadata', Ord(twoeSystemAnsi),
      Ord(Module.Encoding));
    AssertTrue('legacy bytes preserved', FCodec.EncodeModule(Module) = Bytes);
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.AsciiDeclaredWindows1252OverridesUtf8Heuristic;
var
  Module: TTheWordOntModule;
  Bytes, Expected: RawByteString;
  Replacement: UnicodeString;
begin
  Bytes := 'plain' + #13#10 + 'charset=windows-1252' + #13#10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertEquals('declared code page wins for ASCII payload',
      Ord(twoeDeclaredCodePage), Ord(Module.Encoding));
    AssertEquals('windows-1252 code page', 1252, Module.CodePage);
    AssertTrue('declared code page can re-encode', Module.CanReencode);
    AssertTrue('verse is editable', Module.Verses[0].CanEdit);
    AssertTrue('unmodified ASCII module is byte-exact',
      FCodec.EncodeModule(Module) = Bytes);

    Replacement := 'price ';
    Replacement := Replacement + WideChar($20AC);
    Module.Verses[0].ReplaceVisibleText(0, 5, Replacement);
    Expected := 'price ' + RawByteString(#$80) + #13#10 +
      'charset=windows-1252' + #13#10;
    AssertRawBytesEqual('edited Euro is emitted as CP1252 byte 80',
      Expected, FCodec.EncodeModule(Module));
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.Windows1252BytesDecodeAndRoundTrip;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
begin
  Bytes := 'caf' + RawByteString(#$E9) + #10 +
    'charset=cp1252' + #10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertEquals('declared non-UTF8 encoding', Ord(twoeDeclaredCodePage),
      Ord(Module.Encoding));
    AssertEquals('declared code page', 1252, Module.CodePage);
    AssertUnicodeEquals('CP1252 verse decoded', 'caf' + WideChar($00E9),
      Module.Verses[0].VisibleText);
    AssertRawBytesEqual('CP1252 fixture remains byte-exact', Bytes,
      FCodec.EncodeModule(Module));
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.UnsupportedDeclaredCharsetBlocksEditing;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
  Raised: Boolean;
begin
  Bytes := 'plain' + #10 + 'charset=x-custom' + #10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    AssertEquals('unsupported declaration is explicit',
      Ord(twoeUnsupportedDeclared), Ord(Module.Encoding));
    AssertFalse('unsupported charset cannot re-encode', Module.CanReencode);
    AssertFalse('verse editing is blocked', Module.Verses[0].CanEdit);
    AssertEquals('unsupported module is raw-only', Ord(twlcRawOnly),
      Ord(TTheWordCapabilityAnalyzer.AnalyzeModule(Module)));
    AssertTrue('unmodified unsupported module remains byte-exact',
      FCodec.EncodeModule(Module) = Bytes);

    Raised := False;
    try
      Module.Verses[0].ReplaceVisibleText(0, 0, 'x');
    except
      on E: ETheWordOntEncodingError do
        Raised := True;
    end;
    AssertTrue('text edit is rejected', Raised);

    Raised := False;
    try
      Module.Verses[0].AddSemanticMark(twsmItalic, 0, 1);
    except
      on E: ETheWordOntEncodingError do
        Raised := True;
    end;
    AssertTrue('semantic edit is rejected', Raised);
    AssertFalse('rejected edits leave module unmodified', Module.Modified);
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.DeclaredCodePageRejectsUnrepresentableEdit;
var
  Module: TTheWordOntModule;
  Bytes: RawByteString;
  Emoji: UnicodeString;
  Raised: Boolean;
begin
  Bytes := 'plain' + #10 + 'charset=windows-1252' + #10;
  Module := FCodec.DecodeModule(Bytes, 1);
  try
    Emoji := UnicodeString(WideChar($D83D)) + WideChar($DE00);
    Raised := False;
    try
      Module.Verses[0].ReplaceVisibleText(0, 0, Emoji);
    except
      on E: ETheWordOntEncodingError do
        Raised := True;
    end;
    AssertTrue('unrepresentable Unicode edit is rejected', Raised);
    AssertFalse('failed edit is atomic', Module.Modified);
    AssertTrue('source remains byte-exact after rejected edit',
      FCodec.EncodeModule(Module) = Bytes);
  finally
    Module.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  TranslatorNotesAreInvisibleAndRoundTripLiterally;
const
  RAW: UnicodeString =
    'A<RF   q="a&amp;b" ><b>note</b><p>line<CL><Rf>B';
var
  Document: TTheWordVerseDocument;
  Note: TTheWordTranslatorNote;
begin
  Document := FCodec.DecodeVerse(RAW);
  try
    AssertUnicodeEquals('RF body is excluded from visible text', 'AB',
      Document.VisibleText);
    AssertEquals('one RF block', 1, Document.TranslatorNoteCount);
    Note := Document.TranslatorNotes[0];
    AssertEquals('anchor is a zero-based UTF-16 offset', 1,
      Note.OffsetUTF16);
    AssertUnicodeEquals('q follows theWord literal attribute semantics',
      'a&amp;b', Note.Q);
    AssertUnicodeEquals('body stays available to the note editor',
      '<b>note</b><p>line<CL>', Note.BodyRaw);
    AssertEquals(Ord(twlcFullWysiwyg), Ord(Note.Capability));
    AssertTrue(Note.Imported);
    AssertFalse(Note.Modified);
    AssertUnicodeEquals('untouched imported note is byte-for-byte literal',
      RAW, FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    'A<  RF   q="x" >note<Rf>B');
  try
    AssertUnicodeEquals('whitespace RF body is hidden', 'AB',
      Document.VisibleText);
    AssertEquals('whitespace RF remains one note', 1,
      Document.TranslatorNoteCount);
    AssertUnicodeEquals('whitespace RF marker is parsed', 'x',
      Document.TranslatorNotes[0].Q);
    AssertUnicodeEquals('whitespace RF opening stays literal',
      '<  RF   q="x" >', Document.TranslatorNotes[0].OpenLexeme);
    AssertUnicodeEquals('whitespace RF round-trips literally',
      'A<  RF   q="x" >note<Rf>B', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('A<  RF>note<  Rf >tail');
  try
    AssertUnicodeEquals('whitespace RF closing hides only its body',
      'Atail', Document.VisibleText);
    AssertEquals('whitespace closing remains one RF', 1,
      Document.TranslatorNoteCount);
    AssertUnicodeEquals('whitespace RF closing stays literal',
      '<  Rf >', Document.TranslatorNotes[0].CloseLexeme);
    AssertUnicodeEquals('whitespace RF closing round-trips literally',
      'A<  RF>note<  Rf >tail', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  TranslatorNoteBodyCapabilitiesMatchTheWordSubset;
const
  SAFE: UnicodeString =
    '<b>bold</b><font color="#FF0000">red</font>' +
    '<a href="https://example.test/?a=1&b=2">link</a><p><CL>' +
    '<span style="background:#AABBCC">bg</span>';
var
  Canonical: UnicodeString;
  Capability: TTheWordLayerCapability;
  Body: TTheWordTranslatorNoteBodyDocument;
  ColorValue: LongInt;
begin
  Body := TTheWordTranslatorNoteBodyCodec.Decode(SAFE);
  try
    AssertEquals(Ord(twnbcFullWysiwyg), Ord(Body.Capability));
    AssertEquals('safe body exposes stable editor tokens', 14,
      Body.TokenCount);
    AssertEquals(Ord(twnbtkTagOpen), Ord(Body.Tokens[6].Kind));
    AssertEquals(Ord(twnbLink), Ord(Body.Tokens[6].TagKind));
    AssertUnicodeEquals('ampersand is literal in theWord attributes',
      'https://example.test/?a=1&b=2', Body.Tokens[6].Href);
    AssertEquals(Ord(twnbSpanBackground),
      Ord(Body.Tokens[11].TagKind));
    AssertUnicodeEquals('theWord background value is exposed',
      '#AABBCC', Body.Tokens[11].BackgroundColor);
  finally
    Body.Free;
  end;
  AssertTrue(TryCanonicalizeTheWordTranslatorNoteBody(SAFE,
    Canonical, Capability));
  AssertEquals(Ord(twlcFullWysiwyg), Ord(Capability));
  AssertUnicodeEquals('safe subset has deterministic serialization',
    SAFE, Canonical);

  AssertEquals('Bible semantic tags are preserved protected regions',
    Ord(twlcProtectedRegions),
    Ord(AnalyzeTheWordTranslatorNoteBody('<FI>x<Fi>')));
  AssertFalse('protected body is not normalized by WYSIWYG canonicalizer',
    TryCanonicalizeTheWordTranslatorNoteBody('<FI>x<Fi>',
      Canonical, Capability));
  AssertUnicodeEquals('protected body remains literal', '<FI>x<Fi>',
    Canonical);
  AssertEquals('unknown tags require raw editing', Ord(twlcRawOnly),
    Ord(AnalyzeTheWordTranslatorNoteBody('<mystery>x</mystery>')));

  AssertEquals('unknown font attributes require Raw editing',
    Ord(twlcRawOnly), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<font color=red x=1>text</font>')));
  AssertFalse('unknown font attributes are never discarded',
    TryCanonicalizeTheWordTranslatorNoteBody(
      '<font color=red x=1>text</font>', Canonical, Capability));
  AssertUnicodeEquals('literal source remains unchanged',
    '<font color=red x=1>text</font>', Canonical);
  AssertEquals('unknown link attributes require Raw editing',
    Ord(twlcRawOnly), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<a href=https://example.test title=x>link</a>')));
  AssertEquals('unknown span attributes require Raw editing',
    Ord(twlcRawOnly), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<span style="background:red" x=1>text</span>')));
  AssertEquals('duplicate known attributes require Raw editing',
    Ord(twlcRawOnly), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<font color=red color=blue>text</font>')));
  AssertEquals('theWord named font colors are editable visually',
    Ord(twlcFullWysiwyg), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<font color="red">text</font>')));
  Body := TTheWordTranslatorNoteBodyCodec.Decode(
    '<font color="ReD">text</font>');
  try
    AssertEquals('named colors are case-insensitive',
      Ord(twnbcFullWysiwyg), Ord(Body.Capability));
    AssertUnicodeEquals('untouched named color remains literal',
      '<font color="ReD">text</font>',
      TTheWordTranslatorNoteBodyCodec.Encode(Body));
  finally
    Body.Free;
  end;
  AssertEquals('unknown color names stay outside Visual editing',
    Ord(twlcProtectedRegions), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<font color="not-a-color">text</font>')));
  AssertEquals('unsupported extended HTML names stay outside Visual editing',
    Ord(twlcProtectedRegions), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<font color="orange">text</font>')));
  AssertEquals('theWord theme-dependent colors stay outside Visual editing',
    Ord(twlcProtectedRegions), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<font color="defclr5">text</font>')));
  AssertEquals('theWord named backgrounds use the same safe color subset',
    Ord(twlcFullWysiwyg), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<span style="background:red">text</span>')));
  AssertTrue('theWord silver color is recognized',
    TryTheWordTranslatorNoteHtmlColor('silver', ColorValue));
  AssertEquals('theWord silver has its legacy gray value',
    LongInt($00808080), ColorValue);
  AssertEquals('absolute font sizes are not reinterpreted as relative',
    Ord(twlcProtectedRegions), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<font size="14">text</font>')));
  AssertEquals('short background colors remain protected literally',
    Ord(twlcProtectedRegions), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<span style="background:#ABC">text</span>')));
  AssertEquals('links unsafe for the modal require Raw editing',
    Ord(twlcRawOnly), Ord(AnalyzeTheWordTranslatorNoteBody(
    '<a href="https://example.test/a b">link</a>')));
  AssertEquals('free RTF payload always requires Raw editing',
    Ord(twlcRawOnly), Ord(AnalyzeTheWordTranslatorNoteBody(
    UnicodeString(#123#92'rtf1'#92'ansi note}'))));
end;

procedure TTheWordOntCodecV2Tests.
  NestedAndUnclosedTranslatorNotesAreRawOnlyAndHidden;
var
  Document: TTheWordVerseDocument;
begin
  Document := FCodec.DecodeVerse(
    'a<RF q=x>outer<RF>inner<Rf>tail<Rf>b');
  try
    AssertUnicodeEquals('first RF close delimits malformed nested body',
      'atailb',
      Document.VisibleText);
    AssertEquals(1, Document.TranslatorNoteCount);
    AssertEquals(Ord(twlcRawOnly),
      Ord(Document.TranslatorNotes[0].Capability));
    AssertUnicodeEquals('malformed nested body stops at first close',
      'outer<RF>inner', Document.TranslatorNotes[0].BodyRaw);
    AssertUnicodeEquals('nested RF round-trips without repair',
      'a<RF q=x>outer<RF>inner<Rf>tail<Rf>b',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('a<RF>unfinished <b>note');
  try
    AssertUnicodeEquals('unclosed RF body does not leak into Bible text',
      'a', Document.VisibleText);
    AssertEquals(1, Document.TranslatorNoteCount);
    AssertEquals(Ord(twlcRawOnly),
      Ord(Document.TranslatorNotes[0].Capability));
    AssertUnicodeEquals('unclosed source stays literal until Raw edit',
      'a<RF>unfinished <b>note', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  TranslatorNoteCrudPreservesLexicalOrderAndVisibleText;
const
  ORIGINAL: UnicodeString = 'word<WG123><WTN-NSM><wt> tail';
var
  Document, Restored: TTheWordVerseDocument;
  Note, Found: TTheWordTranslatorNote;
  NoteId: QWord;
  Raw: UnicodeString;
begin
  Document := FCodec.DecodeVerse(ORIGINAL);
  try
    AssertFalse('note cannot split the protected lexical word',
      Document.CanInsertTranslatorNote(2));
    AssertTrue('note is valid after all lexical postfixes',
      Document.CanInsertTranslatorNote(4));
    Note := Document.InsertTranslatorNote(4, '*', '<B>note</B>');
    AssertTrue(Assigned(Note));
    NoteId := Note.Id;
    AssertTrue(Document.TryGetTranslatorNote(NoteId, Found));
    AssertTrue(Found = Note);
    AssertUnicodeEquals('note insertion never changes visible text',
      'word tail', Document.VisibleText);
    AssertFalse(
      'lexical postfix still protects insertion at the shared note anchor',
      Document.CanReplaceVisibleText(4, 0));
    AssertFalse('text replacement cannot cross a note anchor',
      Document.CanReplaceVisibleText(3, 2));
    AssertFalse('new note body cannot contain whitespace-style nested RF',
      Assigned(Document.InsertTranslatorNote(4, '',
        '<  RF>nested<Rf>')));
    AssertFalse('new note body cannot contain whitespace-style RF close',
      Assigned(Document.InsertTranslatorNote(4, '',
        'nested<  Rf >tail')));
    Raw := FCodec.EncodeVerse(Document);
    AssertUnicodeEquals('RF follows Strong/morphology postfix data',
      'word<WG123><WTN-NSM><wt><RF q=*><b>note</b><Rf> tail', Raw);

    Restored := FCodec.DecodeVerse(Raw);
    try
      AssertUnicodeEquals('save/reopen keeps Bible text', 'word tail',
        Restored.VisibleText);
      AssertEquals(1, Restored.TranslatorNoteCount);
      AssertTrue(Restored.UpdateTranslatorNote(
        Restored.TranslatorNotes[0].Id, 'A B', '<i>changed</i>'));
      AssertUnicodeEquals('changed note is serialized canonically',
        'word<WG123><WTN-NSM><wt><RF q="A B"><i>changed</i><Rf> tail',
        FCodec.EncodeVerse(Restored));
      AssertTrue(Restored.RemoveTranslatorNote(
        Restored.TranslatorNotes[0].Id));
      AssertUnicodeEquals('removing RF restores all surrounding ONT',
        ORIGINAL, FCodec.EncodeVerse(Restored));
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    'A<RF   q="x" ><b>old</b><Rf>B');
  try
    AssertTrue('body update succeeds without changing marker',
      Document.UpdateTranslatorNote(Document.TranslatorNotes[0].Id,
        'x', '<i>new</i>'));
    AssertUnicodeEquals('body update preserves imported RF opening',
      'A<RF   q="x" ><i>new</i><Rf>B', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('a<RF>note<Rf>b');
  try
    AssertTrue('collapsed insertion at RF anchor is right-sticky',
      Document.CanReplaceVisibleText(1, 0));
    Document.ReplaceVisibleText(1, 0, 'X');
    AssertUnicodeEquals('inserted text precedes and shifts RF',
      'aX<RF>note<Rf>b', FCodec.EncodeVerse(Document));
    AssertEquals('RF anchor moves after inserted text', 2,
      Document.TranslatorNotes[0].OffsetUTF16);
    AssertFalse('replacement crossing RF remains protected',
      Document.CanReplaceVisibleText(1, 2));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    'A<RF q=x future=1>old<Rf>B');
  try
    AssertEquals(Ord(twlcRawOnly),
      Ord(Document.TranslatorNotes[0].Capability));
    AssertTrue('Raw body edit preserves unknown opening attributes',
      Document.UpdateTranslatorNote(Document.TranslatorNotes[0].Id,
        'x', 'new'));
    AssertUnicodeEquals('unknown RF attributes remain literal',
      'A<RF q=x future=1>new<Rf>B', FCodec.EncodeVerse(Document));
    AssertEquals('unknown opening keeps note Raw-only after body edit',
      Ord(twlcRawOnly), Ord(Document.TranslatorNotes[0].Capability));
    AssertFalse('typed marker change cannot discard unknown attributes',
      Document.UpdateTranslatorNote(Document.TranslatorNotes[0].Id,
        'y', 'new'));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('A<FI> B<Fi>');
  try
    AssertTrue(Assigned(Document.InsertTranslatorNote(1, '', 'note')));
    AssertUnicodeEquals('RF precedes a semantic opening at its anchor',
      'A<RF>note<Rf><FI> B<Fi>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    'A<WG123><WTN-NSM><wt><FI> B<Fi>');
  try
    AssertTrue(Assigned(Document.InsertTranslatorNote(1, '', 'note')));
    AssertUnicodeEquals(
      'RF follows lexical postfixes and precedes the next semantic opening',
      'A<WG123><WTN-NSM><wt><RF>note<Rf><FI> B<Fi>',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.InlineFormatSimpleTagsHonorCaseRules;
var
  ErrorText: UnicodeString;
  Kind: TTheWordInlineFormatTagKindV2;
  Tag: TTheWordInlineFormatTagV2;
begin
  AssertTrue(TryClassifyTheWordSimpleInlineFormatNameV2('B', Kind));
  AssertEquals(Ord(twiftv2Bold), Ord(Kind));
  AssertTrue(TryClassifyTheWordSimpleInlineFormatNameV2('i', Kind));
  AssertEquals(Ord(twiftv2Italic), Ord(Kind));
  AssertTrue(TryClassifyTheWordSimpleInlineFormatNameV2('U', Kind));
  AssertEquals(Ord(twiftv2Underline), Ord(Kind));
  AssertTrue(TryClassifyTheWordSimpleInlineFormatNameV2('s', Kind));
  AssertEquals(Ord(twiftv2Strikeout), Ord(Kind));
  AssertTrue(TryClassifyTheWordSimpleInlineFormatNameV2('SuP', Kind));
  AssertEquals(Ord(twiftv2Superscript), Ord(Kind));
  AssertTrue(TryClassifyTheWordSimpleInlineFormatNameV2('sUb', Kind));
  AssertEquals(Ord(twiftv2Subscript), Ord(Kind));
  AssertFalse(TryClassifyTheWordSimpleInlineFormatNameV2('font', Kind));

  AssertTrue(TryParseTheWordInlineFormatTagV2('<B>', Tag, ErrorText));
  AssertEquals(Ord(twiftv2Bold), Ord(Tag.Kind));
  AssertFalse(Tag.IsClosing);
  AssertUnicodeEquals('simple tag spelling is lossless', 'B', Tag.NameRaw);
  AssertUnicodeEquals('simple canonical name', 'b', Tag.CanonicalName);
  AssertTrue(TryParseTheWordInlineFormatTagV2('</sUP>', Tag, ErrorText));
  AssertTrue(Tag.IsClosing);
  AssertEquals(Ord(twiftv2Superscript), Ord(Tag.Kind));
  AssertTrue(TryParseTheWordInlineFormatTagV2('</font>', Tag, ErrorText));
  AssertEquals(Ord(twiftv2Font), Ord(Tag.Kind));
  AssertTrue(Tag.IsClosing);
  AssertFalse(TryParseTheWordInlineFormatTagV2('<FONT color=red>',
    Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2('</FONT>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2('</font >', Tag, ErrorText));
end;

procedure TTheWordOntCodecV2Tests.InlineFormatFontAttributesAreLossless;
const
  RAW: UnicodeString =
    '<font  color = " ReD " size = +01 name=''A B''   >';
var
  Attribute: TTheWordInlineFormatAttributeV2;
  ErrorText: UnicodeString;
  Tag: TTheWordInlineFormatTagV2;
begin
  AssertTrue(TryParseTheWordInlineFormatTagV2(RAW, Tag, ErrorText));
  AssertUnicodeEquals('full tag raw', RAW, Tag.Raw);
  AssertUnicodeEquals('raw span', RAW,
    InlineSpanText(Tag.Raw, Tag.RawSpanUTF16));
  AssertUnicodeEquals('tag name span', 'font',
    InlineSpanText(Tag.Raw, Tag.NameSpanUTF16));
  AssertEquals('three attributes in source order', 3,
    Length(Tag.Attributes));

  Attribute := Tag.Attributes[0];
  AssertUnicodeEquals('color full attribute span', '  color = " ReD "',
    InlineSpanText(Tag.Raw, Attribute.FullSpanUTF16));
  AssertUnicodeEquals('color raw', '  color = " ReD "', Attribute.Raw);
  AssertUnicodeEquals('color leading separator', '  ',
    InlineSpanText(Tag.Raw, Attribute.LeadingSeparatorSpanUTF16));
  AssertUnicodeEquals('color name span', 'color',
    InlineSpanText(Tag.Raw, Attribute.NameSpanUTF16));
  AssertUnicodeEquals('color trivia before equals', ' ',
    InlineSpanText(Tag.Raw, Attribute.BeforeEqualsTriviaSpanUTF16));
  AssertUnicodeEquals('color equals span', '=',
    InlineSpanText(Tag.Raw, Attribute.EqualsSpanUTF16));
  AssertUnicodeEquals('color trivia after equals', ' ',
    InlineSpanText(Tag.Raw, Attribute.AfterEqualsTriviaSpanUTF16));
  AssertUnicodeEquals('color lexical value', '" ReD "',
    InlineSpanText(Tag.Raw, Attribute.LexicalValueSpanUTF16));
  AssertUnicodeEquals('color payload value', ' ReD ',
    InlineSpanText(Tag.Raw, Attribute.PayloadValueSpanUTF16));
  AssertUnicodeEquals('color raw value', ' ReD ', Attribute.ValueRaw);
  AssertUnicodeEquals('color canonical value', 'red',
    Attribute.CanonicalValue);
  AssertTrue(Attribute.WasQuoted);
  AssertEquals('double quote is preserved', Ord('"'),
    Ord(Attribute.QuoteChar));

  Attribute := Tag.Attributes[1];
  AssertUnicodeEquals('size full attribute', ' size = +01',
    InlineSpanText(Tag.Raw, Attribute.FullSpanUTF16));
  AssertFalse(Attribute.WasQuoted);
  AssertUnicodeEquals('unquoted size lexical value', '+01',
    InlineSpanText(Tag.Raw, Attribute.LexicalValueSpanUTF16));
  AssertUnicodeEquals('size canonical value', '+1',
    Attribute.CanonicalValue);

  Attribute := Tag.Attributes[2];
  AssertUnicodeEquals('name raw', ' name=''A B''', Attribute.Raw);
  AssertTrue(Attribute.WasQuoted);
  AssertEquals('single quote is preserved', Ord(''''),
    Ord(Attribute.QuoteChar));
  AssertUnicodeEquals('name payload', 'A B',
    InlineSpanText(Tag.Raw, Attribute.PayloadValueSpanUTF16));
  AssertUnicodeEquals('closing trivia', '   ',
    InlineSpanText(Tag.Raw, Tag.ClosingTriviaSpanUTF16));
  AssertUnicodeEquals('lossless font name', 'A B', Tag.FontNameRaw);

  AssertTrue(TryParseTheWordInlineFormatTagV2(
    '<font name="Times New Roman" color=blue size=''-4''>',
    Tag, ErrorText));
  AssertUnicodeEquals('permuted name', 'Times New Roman', Tag.FontNameRaw);
  AssertEquals('permuted size', -4, Tag.RelativeSize);
  AssertEquals('permuted color', LongInt($00FF0000), Tag.ResolvedColor);
  AssertTrue(TryParseTheWordInlineFormatTagV2(
    '<font name=Noto/Sans>', Tag, ErrorText));
  AssertUnicodeEquals('slash is valid inside an unquoted font name',
    'Noto/Sans', Tag.FontNameRaw);
end;

procedure TTheWordOntCodecV2Tests.InlineFormatColorsResolveToBgr;
var
  CanonicalValue, ColorNameRaw, ErrorText: UnicodeString;
  ColorValue: LongInt;
  Tag: TTheWordInlineFormatTagV2;
begin
  AssertTrue(TryResolveTheWordInlineColorV2('ReD', ColorValue,
    ColorNameRaw, CanonicalValue));
  AssertEquals('red is COLORREF/BGR', LongInt($000000FF), ColorValue);
  AssertUnicodeEquals('mixed-case raw named color', 'ReD', ColorNameRaw);
  AssertUnicodeEquals('canonical named color', 'red', CanonicalValue);
  AssertTrue(TryResolveTheWordInlineColorV2('silver', ColorValue,
    ColorNameRaw, CanonicalValue));
  AssertEquals('legacy silver matches gray', LongInt($00808080),
    ColorValue);
  AssertTrue(TryResolveTheWordInlineColorV2('YeLLoW', ColorValue,
    ColorNameRaw, CanonicalValue));
  AssertEquals('yellow remains available despite its six letters',
    LongInt($0000FFFF), ColorValue);
  AssertTrue(TryResolveTheWordInlineColorV2('#cc3300', ColorValue,
    ColorNameRaw, CanonicalValue));
  AssertEquals('#CC3300 is converted from RGB to BGR',
    LongInt($000033CC), ColorValue);
  AssertUnicodeEquals('hash hex canonical form', '#CC3300',
    CanonicalValue);
  AssertTrue(TryResolveTheWordInlineColorV2('CC3300', ColorValue,
    ColorNameRaw, CanonicalValue));
  AssertEquals('bare CC3300 is converted from RGB to BGR',
    LongInt($000033CC), ColorValue);
  AssertUnicodeEquals('bare hex canonical form', '#CC3300',
    CanonicalValue);
  AssertTrue(TryParseTheWordInlineFormatTagV2(
    '<font color="  #cC3300  ">', Tag, ErrorText));
  AssertEquals('quoted color payload is trimmed only for resolution',
    LongInt($000033CC), Tag.ResolvedColor);
  AssertUnicodeEquals('quoted color payload stays raw', '  #cC3300  ',
    Tag.Attributes[0].ValueRaw);
  AssertFalse(TryResolveTheWordInlineColorV2('#ABC', ColorValue,
    ColorNameRaw, CanonicalValue));
  AssertFalse(TryResolveTheWordInlineColorV2('orange', ColorValue,
    ColorNameRaw, CanonicalValue));
end;

procedure TTheWordOntCodecV2Tests.
  InlineFormatSizesAndMalformedTagsAreRejected;
var
  ErrorText: UnicodeString;
  Tag: TTheWordInlineFormatTagV2;
begin
  AssertTrue(TryParseTheWordInlineFormatTagV2('<font size=+0>',
    Tag, ErrorText));
  AssertEquals(0, Tag.RelativeSize);
  AssertEquals(Ord('+'), Ord(Tag.SizeSignRaw));
  AssertTrue(TryParseTheWordInlineFormatTagV2('<font size=-0>',
    Tag, ErrorText));
  AssertEquals(0, Tag.RelativeSize);
  AssertEquals(Ord('-'), Ord(Tag.SizeSignRaw));
  AssertTrue(TryParseTheWordInlineFormatTagV2('<font size=-4>',
    Tag, ErrorText));
  AssertEquals(-4, Tag.RelativeSize);
  AssertTrue(TryParseTheWordInlineFormatTagV2('<font size=+10>',
    Tag, ErrorText));
  AssertEquals(10, Tag.RelativeSize);
  AssertTrue(TryParseTheWordInlineFormatTagV2('<font size=+01>',
    Tag, ErrorText));
  AssertUnicodeEquals('leading zero spelling is raw', '+01',
    Tag.Attributes[0].ValueRaw);
  AssertUnicodeEquals('leading zero value is canonicalized', '+1',
    Tag.Attributes[0].CanonicalValue);
  AssertTrue('terminal close without trivia is accepted',
    TryParseTheWordInlineFormatTagV2('<font color=red>', Tag, ErrorText));
  AssertEquals('absent closing trivia has zero UTF-16 length', 0,
    Tag.ClosingTriviaSpanUTF16.LengthUTF16);
  AssertTrue('terminal close after trivia is accepted',
    TryParseTheWordInlineFormatTagV2('<font color=red   >', Tag, ErrorText));
  AssertUnicodeEquals('closing trivia remains lossless', '   ',
    InlineSpanText(Tag.Raw, Tag.ClosingTriviaSpanUTF16));

  AssertFalse(TryParseTheWordInlineFormatTagV2('<font>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2('<font size=14>',
    Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2('<font size=-5>',
    Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2('<font size=+11>',
    Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font size=+999999999999999999999999>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font color=red color=blue>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font color=red x=1>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font COLOR=red>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font color="">', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font color="red>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font color="red"size=+1>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font color=red>junk>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font name="bad<name">', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font color=red/>', Tag, ErrorText));
  AssertFalse(TryParseTheWordInlineFormatTagV2(
    '<font name="line' + UnicodeString(#10) + 'break">',
    Tag, ErrorText));
end;

procedure TTheWordOntCodecV2Tests.
  InlineFormatFontNameSpansUseUtf16CodeUnits;
var
  Emoji, ErrorText, Raw: UnicodeString;
  Attribute: TTheWordInlineFormatAttributeV2;
  Tag: TTheWordInlineFormatTagV2;
begin
  Emoji := UnicodeString(WideChar($D83D)) + WideChar($DE00);
  Raw := '<font name="A' + Emoji + 'B">';
  AssertTrue(TryParseTheWordInlineFormatTagV2(Raw, Tag, ErrorText));
  AssertUnicodeEquals('non-BMP full raw is lossless', Raw, Tag.Raw);
  AssertUnicodeEquals('non-BMP font name is lossless', 'A' + Emoji + 'B',
    Tag.FontNameRaw);
  Attribute := Tag.Attributes[0];
  AssertEquals('payload length counts UTF-16 code units', 4,
    Attribute.PayloadValueSpanUTF16.LengthUTF16);
  AssertUnicodeEquals('payload span preserves surrogate pair',
    'A' + Emoji + 'B',
    InlineSpanText(Tag.Raw, Attribute.PayloadValueSpanUTF16));
  AssertUnicodeEquals('lexical span includes quotes around surrogate pair',
    '"A' + Emoji + 'B"',
    InlineSpanText(Tag.Raw, Attribute.LexicalValueSpanUTF16));
  AssertUnicodeEquals('full attribute span includes its separator',
    ' name="A' + Emoji + 'B"',
    InlineSpanText(Tag.Raw, Attribute.FullSpanUTF16));
end;

procedure TTheWordOntCodecV2Tests.FontMarksAreFirstClassLosslessAndStrict;
const
  RAW: UnicodeString =
    '<font  size=+1 color=" ReD " name=''A B''   >x</font>';
  INVALID: array[0..5] of UnicodeString = (
    '<font color=red color=blue>x</font>',
    '<font size=14>x</font>',
    '<font size=+11>x</font>',
    '<FONT color=red>x</font>',
    '<font color=red>x</FONT>',
    '<font color=red>x');
var
  Attribute: TTheWordInlineFormatAttributeV2;
  Document: TTheWordVerseDocument;
  Index: Integer;
  Mark: TTheWordFontMark;
  NestedRaw: UnicodeString;
begin
  Document := FCodec.DecodeVerse(RAW);
  try
    AssertEquals(1, Document.FontMarkCount);
    AssertEquals(0, Document.OpaqueItemCount);
    Mark := Document.FontMarks[0];
    AssertEquals(0, Mark.StartOffset);
    AssertEquals(1, Mark.EndOffset);
    AssertTrue(Mark.Imported);
    AssertTrue(Mark.HasColor);
    AssertEquals(LongInt($000000FF), Mark.ResolvedColor);
    AssertUnicodeEquals('color raw', ' ReD ', Mark.ColorRaw);
    AssertTrue(Mark.HasSize);
    AssertEquals(1, Mark.RelativeSize);
    AssertEquals(Ord('+'), Ord(Mark.SizeSignRaw));
    AssertTrue(Mark.HasFontName);
    AssertUnicodeEquals('font name', 'A B', Mark.FontNameRaw);
    AssertUnicodeEquals('open raw', Copy(RAW, 1, Length(Mark.OpenLexeme)),
      Mark.OpenLexeme);
    Attribute := Mark.OpeningData.Attributes[0];
    AssertUnicodeEquals('attribute span remains relative to open raw',
      Attribute.Raw, InlineSpanText(Mark.OpenLexeme,
      Attribute.FullSpanUTF16));
    AssertUnicodeEquals('no-op is literal', RAW, FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<font color=red></font>');
  try
    AssertEquals('zero-length font is a mark', 1, Document.FontMarkCount);
    AssertEquals(0, Document.FontMarks[0].StartOffset);
    AssertEquals(0, Document.FontMarks[0].EndOffset);
    AssertEquals(0, Document.OpaqueItemCount);
  finally
    Document.Free;
  end;

  NestedRaw := '<font color=red>A<font x=1>B</font>C</font>';
  Document := FCodec.DecodeVerse(NestedRaw);
  try
    AssertEquals('invalid nested font cannot steal outer close', 1,
      Document.FontMarkCount);
    AssertEquals(0, Document.FontMarks[0].StartOffset);
    AssertEquals(3, Document.FontMarks[0].EndOffset);
    AssertTrue(Document.FontMarks[0].HasColor);
    AssertEquals(LongInt($000000FF), Document.FontMarks[0].ResolvedColor);
    AssertEquals('invalid nested open and close stay opaque', 2,
      Document.OpaqueItemCount);
    AssertFalse('invalid nested open remains protected',
      Document.CanReplaceVisibleText(1, 0));
    AssertFalse('invalid nested close remains protected',
      Document.CanReplaceVisibleText(2, 0));
    AssertUnicodeEquals('nested-invalid no-op stays literal', NestedRaw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  NestedRaw := '<font color=red>A<font color=blue/>B</font>';
  Document := FCodec.DecodeVerse(NestedRaw);
  try
    AssertEquals('self-closing invalid font does not consume outer close', 1,
      Document.FontMarkCount);
    AssertEquals(0, Document.FontMarks[0].StartOffset);
    AssertEquals(2, Document.FontMarks[0].EndOffset);
    AssertEquals('self-closing font remains one opaque lexeme', 1,
      Document.OpaqueItemCount);
    AssertUnicodeEquals('nested self-closing no-op stays literal', NestedRaw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  NestedRaw := '<font color=red><b>x</font>y</b>';
  Document := FCodec.DecodeVerse(NestedRaw);
  try
    AssertEquals('imported font crossing semantic is not first-class', 0,
      Document.FontMarkCount);
    AssertTrue('crossing font pair stays protected',
      Document.OpaqueItemCount >= 2);
    AssertUnicodeEquals('crossing font-semantic remains literal', NestedRaw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  NestedRaw := '<b><font color=red>x</b>y</font>';
  Document := FCodec.DecodeVerse(NestedRaw);
  try
    AssertEquals('reverse imported crossing is not first-class', 0,
      Document.FontMarkCount);
    AssertTrue('reverse crossing stays protected',
      Document.OpaqueItemCount >= 2);
    AssertUnicodeEquals('reverse crossing remains literal', NestedRaw,
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  for Index := Low(INVALID) to High(INVALID) do
  begin
    Document := FCodec.DecodeVerse(INVALID[Index]);
    try
      AssertEquals('invalid font never becomes a mark', 0,
        Document.FontMarkCount);
      AssertTrue('invalid font stays protected',
        Document.OpaqueItemCount > 0);
      AssertUnicodeEquals('invalid source stays literal', INVALID[Index],
        FCodec.EncodeVerse(Document));
      AssertFalse('invalid anchor remains protected',
        Document.CanReplaceVisibleText(0, 0));
    finally
      Document.Free;
    end;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  FontMarkLexemesOwnPairedEventSerialization;
const
  ORIGINAL: UnicodeString = '<font color=red>x</font>';
  UPDATED_OPEN: UnicodeString =
    '<font  size = "+2" color=''BLUE'' name="New Face" >';
var
  Document, Restored: TTheWordVerseDocument;
  Encoded, OriginalClose, OriginalOpen: UnicodeString;
begin
  Document := FCodec.DecodeVerse(ORIGINAL);
  try
    AssertEquals(1, Document.FontMarkCount);
    AssertFalse('decoded document starts unmodified', Document.Modified);
    OriginalOpen := Document.FontMarks[0].OpenLexeme;
    OriginalClose := Document.FontMarks[0].CloseLexeme;
    AssertTrue('identical lexemes are a successful no-op',
      Document.UpdateFontMarkLexemes(Document.FontMarks[0], OriginalOpen,
        OriginalClose));
    AssertFalse('identical update does not mark document modified',
      Document.Modified);
    AssertTrue('identical update preserves imported state',
      Document.FontMarks[0].Imported);
    AssertUnicodeEquals('identical update remains literal', ORIGINAL,
      FCodec.EncodeVerse(Document));

    AssertTrue('valid API update owns both paired lexemes',
      Document.UpdateFontMarkLexemes(Document.FontMarks[0], UPDATED_OPEN,
        '</font>'));
    Encoded := UPDATED_OPEN + 'x</font>';
    AssertUnicodeEquals('encoder reads paired lexemes from font mark',
      Encoded, FCodec.EncodeVerse(Document));
    AssertTrue('valid update marks document modified', Document.Modified);
    AssertFalse('updated mark is no longer imported',
      Document.FontMarks[0].Imported);
    AssertUnicodeEquals('opening data follows updated lexeme', UPDATED_OPEN,
      Document.FontMarks[0].OpeningData.Raw);
    AssertTrue('updated mark has color', Document.FontMarks[0].HasColor);
    AssertEquals('updated blue is COLORREF/BGR', LongInt($00FF0000),
      Document.FontMarks[0].ResolvedColor);
    AssertUnicodeEquals('updated color raw spelling', 'BLUE',
      Document.FontMarks[0].ColorRaw);
    AssertTrue('updated mark has size', Document.FontMarks[0].HasSize);
    AssertEquals('updated relative size', 2,
      Document.FontMarks[0].RelativeSize);
    AssertTrue('updated mark has font name',
      Document.FontMarks[0].HasFontName);
    AssertUnicodeEquals('updated font name', 'New Face',
      Document.FontMarks[0].FontNameRaw);

    AssertFalse('invalid open rejects the whole update',
      Document.UpdateFontMarkLexemes(Document.FontMarks[0],
        '<font COLOR=green>', '</font>'));
    AssertFalse('invalid close rejects the whole update',
      Document.UpdateFontMarkLexemes(Document.FontMarks[0],
        '<font color=green>', '</FONT>'));
    AssertUnicodeEquals('failed updates preserve open', UPDATED_OPEN,
      Document.FontMarks[0].OpenLexeme);
    AssertUnicodeEquals('failed updates preserve close', '</font>',
      Document.FontMarks[0].CloseLexeme);
    AssertUnicodeEquals('failed updates preserve encoding', Encoded,
      FCodec.EncodeVerse(Document));

    Restored := FCodec.DecodeVerse(Encoded);
    try
      AssertEquals('re-decoded pair remains first class', 1,
        Restored.FontMarkCount);
      AssertEquals('re-decoded pair has no opaque orphan', 0,
        Restored.OpaqueItemCount);
      AssertUnicodeEquals('re-decoded pair remains literal', Encoded,
        FCodec.EncodeVerse(Restored));
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  FontRangesShiftUtf16AndKeepLexicalPostfixes;
var
  Document, Restored: TTheWordVerseDocument;
  Emoji, Encoded: UnicodeString;
  Mark: TTheWordFontMark;
begin
  Emoji := UnicodeString(WideChar($D83D)) + WideChar($DE00);
  Document := FCodec.DecodeVerse('A<font color=red>B' + Emoji +
    'C</font>D');
  try
    Mark := Document.FontMarks[0];
    AssertEquals(1, Mark.StartOffset);
    AssertEquals(5, Mark.EndOffset);
    AssertTrue('start anchor is editable',
      Document.CanReplaceVisibleText(1, 0));
    AssertFalse('surrogate cannot be split',
      Document.CanReplaceVisibleText(3, 1));
    Document.ReplaceVisibleText(0, 0, 'X');
    AssertEquals(2, Mark.StartOffset);
    AssertEquals(6, Mark.EndOffset);
    Document.ReplaceVisibleText(3, 2, 'Z');
    AssertEquals(2, Mark.StartOffset);
    AssertEquals(5, Mark.EndOffset);
    Document.ReplaceVisibleText(Length(Document.VisibleText), 0, '!');
    AssertEquals(2, Mark.StartOffset);
    AssertEquals(5, Mark.EndOffset);
    AssertUnicodeEquals('shifted range keeps imported lexemes',
      'XA<font color=red>BZC</font>D!', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('A<font color=red>BC</font>D');
  try
    AssertTrue('insertion at font start is allowed',
      Document.CanReplaceVisibleText(1, 0));
    Document.ReplaceVisibleText(1, 0, 'X');
    AssertUnicodeEquals('start insertion stays outside font',
      'AX<font color=red>BC</font>D', FCodec.EncodeVerse(Document));
    AssertEquals(2, Document.FontMarks[0].StartOffset);
    AssertEquals(4, Document.FontMarks[0].EndOffset);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('A<font color=red>BC</font>D');
  try
    AssertTrue('insertion at font end is allowed',
      Document.CanReplaceVisibleText(3, 0));
    Document.ReplaceVisibleText(3, 0, 'X');
    AssertUnicodeEquals('end insertion stays outside font',
      'A<font color=red>BC</font>XD', FCodec.EncodeVerse(Document));
    AssertEquals(1, Document.FontMarks[0].StartOffset);
    AssertEquals(3, Document.FontMarks[0].EndOffset);
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('A<font color=red>BC</font>D');
  try
    AssertTrue('replacement crossing both font boundaries is allowed',
      Document.CanReplaceVisibleText(0, 4));
    Document.ReplaceVisibleText(0, 4, 'Q');
    AssertUnicodeEquals('crossing replacement follows semantic range rules',
      '<font color=red>Q</font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<font color=red>ab</font>');
  try
    AssertTrue(Document.InsertParagraphMark(0));
    AssertUnicodeEquals('paragraph at font start stays outside',
      '<CM><font color=red>ab</font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<font color=red>ab</font>');
  try
    AssertTrue(Document.InsertParagraphMark(2));
    AssertUnicodeEquals('paragraph at font end stays outside',
      '<font color=red>ab</font><CM>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<font color=red>ab</font>');
  try
    AssertTrue('translator note at font start is allowed',
      Assigned(Document.InsertTranslatorNote(0, '', 'note')));
    AssertUnicodeEquals('translator note at font start stays outside',
      '<RF>note<Rf><font color=red>ab</font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  Document := FCodec.DecodeVerse('<font color=red>ab</font>');
  try
    AssertTrue('translator note at font end is allowed',
      Assigned(Document.InsertTranslatorNote(2, '', 'note')));
    AssertUnicodeEquals('translator note at font end stays outside',
      '<font color=red>ab</font><RF>note<Rf>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  Document := FCodec.DecodeVerse(
    '<font color=red>a<RF>n<Rf></font>');
  try
    AssertTrue('font close permits insertion beside RF postfix',
      Document.CanReplaceVisibleText(1, 0));
    Document.ReplaceVisibleText(1, 0, 'X');
    AssertUnicodeEquals('RF remains left of font close by ordinal',
      '<font color=red>a<RF>n<Rf></font>X', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    '<font color=red>word<WG123><wt></font>');
  try
    AssertFalse('lexical postfix still protects collapsed insertion',
      Document.CanReplaceVisibleText(4, 0));
    AssertUnicodeEquals('protected postfix remains before font close',
      '<font color=red>word<WG123><wt></font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    '<font color=red>a</font><font color=blue>b</font>');
  try
    Document.ReplaceVisibleText(1, 0, 'X');
    AssertUnicodeEquals('adjacent font boundary inserts between ranges',
      '<font color=red>a</font>X<font color=blue>b</font>',
      FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse('<font color=red></font>');
  try
    Document.ReplaceVisibleText(0, 0, 'X');
    AssertUnicodeEquals('zero-length font uses opening-boundary affinity',
      'X<font color=red></font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  Document := FCodec.DecodeVerse(
    '<RF>n<Rf><font color=red>x</font>');
  try
    Document.ReplaceVisibleText(0, 0, 'X');
    AssertUnicodeEquals('RF before font opener stays left of insertion',
      '<RF>n<Rf>X<font color=red>x</font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(
    'word<WG123><wt><font color=red>x</font>');
  try
    AssertFalse('WG postfix still protects shared font start anchor',
      Document.CanReplaceVisibleText(4, 0));
    AssertUnicodeEquals('WG stays left of font opener',
      'word<WG123><wt><font color=red>x</font>', FCodec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
  Document := FCodec.DecodeVerse(
    '<font color=red>head word<WG123><wt></font> tail');
  try
    Document.ReplaceVisibleText(1, 1, 'E');
    Document.ReplaceVisibleText(Length(Document.VisibleText), 0, '!');
    Encoded := FCodec.EncodeVerse(Document);
    AssertUnicodeEquals('lexical postfixes precede font close',
      '<font color=red>hEad word<WG123><wt></font> tail!', Encoded);
    Restored := FCodec.DecodeVerse(Encoded);
    try
      AssertEquals(1, Restored.FontMarkCount);
      AssertEquals(0, Restored.OpaqueItemCount);
      AssertEquals(2, Restored.LexicalItemCount);
      AssertEquals(Ord(twlkStrongGreek),
        Ord(Restored.LexicalItems[0].Kind));
      AssertUnicodeEquals('strong value', '123',
        Restored.LexicalItems[0].Value);
      AssertEquals(9, Restored.LexicalItems[0].Offset);
      AssertEquals(Ord(twlkWordTokenEnd),
        Ord(Restored.LexicalItems[1].Kind));
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  FontTopologyBuilderIsPureUtf16SafeAndCrossingAware;
var
  Candidate, Emoji, OriginalRaw, Raw: UnicodeString;
  Document, Restored: TTheWordVerseDocument;
  Index: Integer;
  OriginalModified: Boolean;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Emoji := UnicodeString(WideChar($D83D)) + WideChar($DE00);
  Raw := '<font size=''+1'' color=" ReD " name=''A B'' >A<FI>B' +
    '<font size=+2>C<WG123>D' + Emoji +
    '</font>E<Fi>F</font>';
  Document := FCodec.DecodeVerse(Raw);
  try
    OriginalRaw := Document.OriginalRaw;
    OriginalModified := Document.Modified;
    AssertTrue('partial range plan crosses semantic, nested font and emoji',
      TryPlanTheWordRelativeFontSizeV2(Document, 1, 6, -1, Plan));
    AssertTrue('candidate builds without mutating the document',
      Document.TryBuildRawWithFontTopology(Plan, Candidate));
    AssertUnicodeEquals('source raw remains owned and literal', OriginalRaw,
      Document.OriginalRaw);
    AssertEquals('source Modified remains unchanged', OriginalModified,
      Document.Modified);
    AssertTrue('no explicit positive zero is authored',
      Pos('size=+0', LowerCase(Candidate)) = 0);
    AssertTrue('outer color spelling survives split',
      Pos('color=" ReD "', Candidate) > 0);
    AssertTrue('outer font name spelling survives split',
      Pos('name=''A B''', Candidate) > 0);
    AssertTrue('nested size substitutes the outer size',
      Pos('size=+1>C<WG123>D', Candidate) > 0);
    AssertTrue('lexical lexeme is still literal',
      Pos('<WG123>', Candidate) > 0);

    Restored := FCodec.DecodeVerse(Candidate);
    try
      AssertUnicodeEquals('UTF-16 visible text and emoji survive',
        Document.VisibleText, Restored.VisibleText);
      AssertEquals('semantic topology survives crossing', Document.MarkCount,
        Restored.MarkCount);
      AssertEquals('WG object survives', Document.LexicalItemCount,
        Restored.LexicalItemCount);
      AssertEquals('WG affinity survives', Document.LexicalItems[0].Offset,
        Restored.LexicalItems[0].Offset);
      AssertUnicodeEquals('WG literal survives',
        Document.LexicalItems[0].RawLexeme,
        Restored.LexicalItems[0].RawLexeme);
      for Index := 0 to Restored.FontMarkCount - 1 do
        AssertFalse('decoded candidate has no explicit zero',
          Restored.FontMarks[Index].HasSize and
          (Restored.FontMarks[Index].RelativeSize = 0));
    finally
      Restored.Free;
    end;

    { Reconciliation failure is also pure and clears an unusable candidate. }
    Plan[0].Actions[0].Mark.OpenOrdinal := 2147483647;
    Candidate := 'sentinel';
    AssertFalse('stale plan is rejected',
      Document.TryBuildRawWithFontTopology(Plan, Candidate));
    AssertUnicodeEquals('failure clears candidate', '', Candidate);
    AssertUnicodeEquals('failure leaves raw untouched', OriginalRaw,
      Document.OriginalRaw);
    AssertEquals('failure leaves Modified untouched', OriginalModified,
      Document.Modified);
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  FontTopologyBuilderPreservesSameAnchorAffinity;
  procedure AssertLegacyStrong(const ARaw, AWord: UnicodeString);
  var
    Index: Integer;
    Parser: TONTParser;
    Parsed: TSyntagmList;
    ParsedWord: TSyntagm;
  begin
    Parser := TONTParser.Create;
    try
      Parsed := Parser.ParseLine(UTF8Encode(ARaw), nil);
      try
        ParsedWord := nil;
        for Index := 0 to Parsed.Count - 1 do
          if (Parsed[Index].Kind = skSyntagm) and
             (Parsed[Index].Text = AWord) then
          begin
            ParsedWord := Parsed[Index];
            Break;
          end;
        AssertTrue('legacy lexical owner is parsed', Assigned(ParsedWord));
        AssertEquals('legacy Strong remains attached', 1,
          ParsedWord.Strong.Count);
      finally
        for Index := Parsed.Count - 1 downto 0 do Parsed[Index].Free;
        Parsed.Free;
      end;
    finally
      Parser.Free;
    end;
  end;
var
  Candidate, Raw: UnicodeString;
  Document, Restored: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Raw := '<font size=+1>word<WG456></font>x';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(Document, 0, 4, 1, Plan));
    AssertTrue(Document.TryBuildRawWithFontTopology(Plan, Candidate));
    AssertUnicodeEquals('WG remains before font close',
      '<font size=+2>word<WG456></font>x', Candidate);
    AssertUnicodeEquals('close-side source identity is pure', Raw,
      Document.OriginalRaw);
    AssertLegacyStrong(Candidate, 'word');
    Restored := FCodec.DecodeVerse(Candidate);
    try
      AssertUnicodeEquals('close-side candidate round trips', Candidate,
        FCodec.EncodeVerse(Restored));
      AssertUnicodeEquals('close-side WG literal', '<WG456>',
        Restored.LexicalItems[0].RawLexeme);
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;

  Raw := 'word<WG123><font size=+1>x</font>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(Document, 4, 1, 1, Plan));
    AssertTrue(Document.TryBuildRawWithFontTopology(Plan, Candidate));
    AssertUnicodeEquals('WG remains before font open',
      'word<WG123><font size=+2>x</font>', Candidate);
    AssertUnicodeEquals('open-side source identity is pure', Raw,
      Document.OriginalRaw);
    AssertLegacyStrong(Candidate, 'word');
    Restored := FCodec.DecodeVerse(Candidate);
    try
      AssertUnicodeEquals('open-side candidate round trips', Candidate,
        FCodec.EncodeVerse(Restored));
      AssertEquals('open-side WG offset', 4,
        Restored.LexicalItems[0].Offset);
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;

  Raw := '<font size=+1>a</font><FI><RF q=*>note<Rf><Fi>' +
    '<font color=blue><ZZ></font><font color=red>b</font>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(Document, 0, 1, 1, Plan));
    AssertTrue(Document.TryBuildRawWithFontTopology(Plan, Candidate));
    AssertUnicodeEquals('zero wrappers, RF and opaque retain anchor order',
      '<font size=+2>a</font><FI><RF q=*>note<Rf><Fi>' +
      '<font color=blue><ZZ></font><font color=red>b</font>', Candidate);
    AssertUnicodeEquals('same-anchor source identity is pure', Raw,
      Document.OriginalRaw);
    Restored := FCodec.DecodeVerse(Candidate);
    try
      AssertEquals('zero semantic survives', 1, Restored.MarkCount);
      AssertEquals('zero font survives', 3, Restored.FontMarkCount);
      AssertEquals('RF survives', 1, Restored.TranslatorNoteCount);
      AssertEquals('opaque survives', 1, Restored.OpaqueItemCount);
      AssertUnicodeEquals('RF body survives', 'note',
        Restored.TranslatorNotes[0].BodyRaw);
      AssertUnicodeEquals('opaque literal survives', '<ZZ>',
        Restored.OpaqueItems[0].RawLexeme);
      AssertUnicodeEquals('same-anchor candidate round trips', Candidate,
        FCodec.EncodeVerse(Restored));
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  FontTopologyBuilderPreservesImportedZeroOutsideSelection;
const
  RAW: UnicodeString =
    '<font size=+0 color=blue>a</font><font size=+1>b</font>' +
    '<RF q=*>size=+0<Rf><ZZ size=+0>';
var
  Candidate: UnicodeString;
  Document, Restored: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Document := FCodec.DecodeVerse(RAW);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 1, 1, 1, Plan));
    AssertTrue(Document.TryBuildRawWithFontTopology(
      Plan, Candidate));
    AssertTrue('imported +0 outside the selection is preserved',
      Pos('<font size=+0 color=blue>a</font>', Candidate) > 0);
    AssertTrue('size-looking translator note data is preserved',
      Pos('<RF q=*>size=+0<Rf>', Candidate) > 0);
    AssertTrue('size-looking opaque data is preserved',
      Pos('<ZZ size=+0>', Candidate) > 0);
    Restored := FCodec.DecodeVerse(Candidate);
    try
      AssertEquals(2, Restored.FontMarkCount);
      AssertTrue(Restored.FontMarks[0].HasSize);
      AssertEquals(0, Restored.FontMarks[0].RelativeSize);
      AssertTrue(Restored.FontMarks[0].HasColor);
      AssertTrue(Restored.FontMarks[1].HasSize);
      AssertEquals(2, Restored.FontMarks[1].RelativeSize);
      AssertUnicodeEquals('candidate remains lossless',
        Candidate, FCodec.EncodeVerse(Restored));
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

procedure TTheWordOntCodecV2Tests.
  FontTopologyBuilderPreservesNestedSeamPayload;
const
  FONT_OUTER: UnicodeString =
    '<font size=+1 color=red><FI>a<RF q=*>n<Rf>b<Fi></font>';
  SEMANTIC_OUTER: UnicodeString =
    '<FI><font size=+1 color=red>a<RF q=*>n<Rf>b</font><Fi>';
var
  Candidate: UnicodeString;
  Document, Restored: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Document := FCodec.DecodeVerse(FONT_OUTER);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 1, 1, 1, Plan));
    AssertTrue(Document.TryBuildRawWithFontTopology(
      Plan, Candidate));
    AssertUnicodeEquals('payload remains inside persistent font and FI',
      '<font size=+1 color=red><FI>a<RF q=*>n<Rf><Fi></font>' +
      '<font size=+2 color=red><FI>b<Fi></font>', Candidate);
    Restored := FCodec.DecodeVerse(Candidate);
    try
      AssertEquals(1, Restored.TranslatorNoteCount);
      AssertEquals(1, Restored.MarkCount);
      AssertEquals(2, Restored.FontMarkCount);
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;

  Document := FCodec.DecodeVerse(SEMANTIC_OUTER);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 1, 1, 1, Plan));
    AssertTrue(Document.TryBuildRawWithFontTopology(
      Plan, Candidate));
    AssertUnicodeEquals('semantic outer wrapper stays continuous',
      '<FI><font size=+1 color=red>a<RF q=*>n<Rf></font>' +
      '<font size=+2 color=red>b</font><Fi>', Candidate);
    Restored := FCodec.DecodeVerse(Candidate);
    try
      AssertEquals(1, Restored.TranslatorNoteCount);
      AssertEquals(1, Restored.MarkCount);
      AssertEquals(2, Restored.FontMarkCount);
    finally
      Restored.Free;
    end;
  finally
    Document.Free;
  end;
end;

initialization
  RegisterTest(TTheWordOntCodecV2Tests);

end.
