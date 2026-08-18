unit theword_ont_codec_v2_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, theword_ont_codec_v2;

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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SemanticTagsAreStrictlyCaseSensitive;
    procedure AllFourSemanticPairsBecomeMarks;
    procedure LessThanWithoutClosingBracketRemainsText;
    procedure UnknownAndMalformedTagsAreOpaqueAndLiteral;
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

procedure TTheWordOntCodecV2Tests.AllFourSemanticPairsBecomeMarks;
var
  Document: TTheWordVerseDocument;
  Raw: UnicodeString;
begin
  Raw := '<FI>i<Fi><FR>r<Fr><FO>o<Fo><FU>u<Fu>';
  Document := FCodec.DecodeVerse(Raw);
  try
    AssertUnicodeEquals('visible text', 'irou', Document.VisibleText);
    AssertEquals('mark count', 4, Document.MarkCount);
    AssertEquals('italic', Ord(twsmItalic), Ord(Document.Marks[0].Kind));
    AssertEquals('red words', Ord(twsmWordsOfJesus),
      Ord(Document.Marks[1].Kind));
    AssertEquals('old testament quote', Ord(twsmOldTestamentQuote),
      Ord(Document.Marks[2].Kind));
    AssertEquals('underline', Ord(twsmUnderline),
      Ord(Document.Marks[3].Kind));
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

initialization
  RegisterTest(TTheWordOntCodecV2Tests);

end.
