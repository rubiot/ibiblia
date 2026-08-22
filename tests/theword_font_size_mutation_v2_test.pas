unit theword_font_size_mutation_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  theword_ont_codec_v2, theword_inline_format_v2,
  theword_font_size_mutation_v2;

type
  TTheWordFontSizeMutationV2Test = class(TTestCase)
  private
    function CountActions(
      const ALeaf: TTheWordFontSizeLeafPlanV2;
      AKind: TTheWordFontSizeActionKindV2): Integer;
  published
    procedure UniformAndMixedLeavesPlanIndependently;
    procedure NestedZeroRemovesEverySizeAncestor;
    procedure ColorOnlyInnerBecomesNonzeroCarrier;
    procedure ExplicitPositiveZeroIsLeafScoped;
    procedure ExplicitNegativeZeroIsLeafScoped;
    procedure Utf16BoundariesRejectSplitSurrogates;
    procedure InvalidSelectionsAndDeltasReturnEmptyPlan;
    procedure LimitsAndLateOverflowAreAtomic;
    procedure TitlesAndProtectedRangesBlockButLexicalDoesNot;
    procedure RewriteReparsesAndPreservesAttributeLexemes;
    procedure RewriteRejectsMalformedAndReparsesExactLexeme;
  end;

implementation

function TTheWordFontSizeMutationV2Test.CountActions(
  const ALeaf: TTheWordFontSizeLeafPlanV2;
  AKind: TTheWordFontSizeActionKindV2): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to High(ALeaf.Actions) do
    if ALeaf.Actions[Index].Kind = AKind then
      Inc(Result);
end;

procedure TTheWordFontSizeMutationV2Test.
  UniformAndMixedLeavesPlanIndependently;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse('word');
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 4, 1, Plan));
    AssertEquals(1, Length(Plan));
    AssertEquals(0, Plan[0].CurrentEffectiveSize);
    AssertEquals(1, Plan[0].TargetEffectiveSize);
    AssertTrue(Plan[0].CarrierIsNew);
    AssertEquals(1, CountActions(Plan[0], twfsakAddSize));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(
    '<font size=-3>a</font><font size=+1>b</font>');
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 2, -1, Plan));
    AssertEquals(2, Length(Plan));
    AssertEquals(-4, Plan[0].TargetEffectiveSize);
    AssertEquals(0, Plan[1].TargetEffectiveSize);
    AssertEquals(1, CountActions(Plan[0], twfsakSetSize));
    AssertEquals(1, CountActions(Plan[1], twfsakRemoveSize));
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontSizeMutationV2Test.
  NestedZeroRemovesEverySizeAncestor;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(
    '<font size=+2 color=red>a' +
    '<font size=+1>b</font>c</font>');
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 1, 1, -1, Plan));
    AssertEquals(1, Length(Plan));
    AssertEquals(1, Plan[0].CurrentEffectiveSize);
    AssertEquals(0, Plan[0].TargetEffectiveSize);
    AssertEquals(2,
      CountActions(Plan[0], twfsakRemoveSize));
    AssertEquals(0,
      CountActions(Plan[0], twfsakSetSize));
    AssertTrue(
      Plan[0].Carrier.OpenOrdinal >
      Plan[0].Actions[0].Mark.OpenOrdinal);
    AssertEquals(
      Plan[0].Carrier.OpenOrdinal,
      Plan[0].Actions[1].Mark.OpenOrdinal);
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontSizeMutationV2Test.
  ColorOnlyInnerBecomesNonzeroCarrier;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(
    '<font size=+2 color=red>a' +
    '<font color=blue>b</font>c</font>');
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 1, 1, 1, Plan));
    AssertEquals(3, Plan[0].TargetEffectiveSize);
    AssertEquals(1,
      CountActions(Plan[0], twfsakRemoveSize));
    AssertEquals(1,
      CountActions(Plan[0], twfsakAddSize));
    AssertTrue(
      Plan[0].Carrier.OpenOrdinal >
      Plan[0].Actions[0].Mark.OpenOrdinal);
    AssertEquals(
      Plan[0].Carrier.OpenOrdinal,
      Plan[0].Actions[1].Mark.OpenOrdinal);
    AssertEquals(3, Plan[0].Actions[1].TargetSize);
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontSizeMutationV2Test.
  ExplicitPositiveZeroIsLeafScoped;
const
  RAW: UnicodeString = '<font size=+0>abcd</font>';
var
  Action: TTheWordFontSizeMarkActionV2;
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(RAW);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 1, 2, 1, Plan));
    AssertEquals(1, Length(Plan));
    AssertEquals(1, Plan[0].StartUTF16);
    AssertEquals(2, Plan[0].LengthUTF16);
    AssertEquals(0, Plan[0].CurrentEffectiveSize);
    AssertTrue(Plan[0].CurrentHadExplicitSize);
    AssertEquals(Ord('+'),
      Ord(Plan[0].CurrentExplicitZeroSign));
    AssertEquals(1, Plan[0].TargetEffectiveSize);
    AssertFalse(Plan[0].CarrierIsNew);
    AssertEquals(1, Length(Plan[0].Actions));

    Action := Plan[0].Actions[0];
    AssertEquals(Ord(twfsakSetSize), Ord(Action.Kind));
    AssertEquals(Plan[0].Carrier.OpenOrdinal,
      Action.Mark.OpenOrdinal);
    AssertEquals(0, Action.Mark.StartUTF16);
    AssertEquals(4, Action.Mark.EndUTF16);
    AssertEquals(1, Action.LeafStartUTF16);
    AssertEquals(2, Action.LeafLengthUTF16);
    AssertTrue(Action.OriginalHadSize);
    AssertTrue(Action.OriginalWasExplicitZero);
    AssertEquals(Ord('+'), Ord(Action.OriginalZeroSign));
    AssertEquals(1, Action.TargetSize);
    AssertEquals(RAW, Codec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontSizeMutationV2Test.
  ExplicitNegativeZeroIsLeafScoped;
const
  RAW: UnicodeString = '<font size=-0>wxyz</font>';
var
  Action: TTheWordFontSizeMarkActionV2;
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(RAW);
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 2, 1, -1, Plan));
    AssertEquals(0, Plan[0].CurrentEffectiveSize);
    AssertTrue(Plan[0].CurrentHadExplicitSize);
    AssertEquals(Ord('-'),
      Ord(Plan[0].CurrentExplicitZeroSign));
    AssertEquals(-1, Plan[0].TargetEffectiveSize);
    AssertEquals(1, Length(Plan[0].Actions));

    Action := Plan[0].Actions[0];
    AssertEquals(Ord(twfsakSetSize), Ord(Action.Kind));
    AssertEquals(Plan[0].Carrier.OpenOrdinal,
      Action.Mark.OpenOrdinal);
    AssertEquals(2, Action.LeafStartUTF16);
    AssertEquals(1, Action.LeafLengthUTF16);
    AssertTrue(Action.OriginalWasExplicitZero);
    AssertEquals(Ord('-'), Ord(Action.OriginalZeroSign));
    AssertEquals(-1, Action.TargetSize);
    AssertEquals(RAW, Codec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontSizeMutationV2Test.
  Utf16BoundariesRejectSplitSurrogates;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Emoji: UnicodeString;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Emoji := UnicodeString(WideChar($D83D)) +
    WideChar($DE00);
  Document := Codec.DecodeVerse('a' + Emoji + 'b');
  try
    AssertFalse(
      'end boundary cannot split the high surrogate',
      TryPlanTheWordRelativeFontSizeV2(
        Document, 1, 1, 1, Plan));
    AssertEquals(0, Length(Plan));

    AssertFalse(
      'start boundary cannot begin at the low surrogate',
      TryPlanTheWordRelativeFontSizeV2(
        Document, 2, 1, 1, Plan));
    AssertEquals(0, Length(Plan));

    AssertTrue(
      'the complete non-BMP pair is a valid selection',
      TryPlanTheWordRelativeFontSizeV2(
        Document, 1, 2, 1, Plan));
    AssertEquals(1, Length(Plan));
    AssertEquals(2, Plan[0].LengthUTF16);
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontSizeMutationV2Test.
  InvalidSelectionsAndDeltasReturnEmptyPlan;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse('abc');
  try
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 1, 0, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 1, 2, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 0, 1, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, -1, 1, 1, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 4, 0, 1, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 2, 2, 1, Plan));
    AssertEquals(0, Length(Plan));
  finally
    Document.Free;
  end;
end;
procedure TTheWordFontSizeMutationV2Test.
  LimitsAndLateOverflowAreAtomic;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse('<font size=-3>x</font>');
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 1, -1, Plan));
    AssertEquals(-3, Plan[0].CurrentEffectiveSize);
    AssertEquals(-4, Plan[0].TargetEffectiveSize);
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('<font size=+9>x</font>');
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 1, 1, Plan));
    AssertEquals(9, Plan[0].CurrentEffectiveSize);
    AssertEquals(10, Plan[0].TargetEffectiveSize);
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('<font size=+10>x</font>');
  try
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 1, 1, Plan));
    AssertEquals(0, Length(Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('<font size=-4>x</font>');
  try
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 1, -1, Plan));
    AssertEquals(0, Length(Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(
    '<font size=+1>a</font><font size=+10>b</font>');
  try
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 2, 1, Plan));
    AssertEquals(
      'a later overflowing leaf discards the earlier valid plan',
      0, Length(Plan));
  finally
    Document.Free;
  end;
end;
procedure TTheWordFontSizeMutationV2Test.
  TitlesAndProtectedRangesBlockButLexicalDoesNot;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse('<TS>Head<Ts> body');
  try
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 1, 2, 1, Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('a<X>b');
  try
    AssertFalse(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 2, 1, Plan));
    AssertTrue(
      'semantic point policy does not block an endpoint',
      TryPlanTheWordRelativeFontSizeV2(
        Document, 0, 1, 1, Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(
    'word<WG123><WTN-NSM><wt> tail');
  try
    AssertTrue(TryPlanTheWordRelativeFontSizeV2(
      Document, 0, 4, 1, Plan));
    AssertEquals(1, Length(Plan));
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontSizeMutationV2Test.
  RewriteReparsesAndPreservesAttributeLexemes;
var
  Needed: Boolean;
  OpenLexeme, Rewritten: UnicodeString;
begin
  OpenLexeme :=
    '<font size=''+1'' color = " ReD " name=''A B''   >';
  AssertTrue(TryRewriteTheWordFontSizeOpenV2(
    OpenLexeme, True, 2, Rewritten, Needed));
  AssertTrue(Needed);
  AssertEquals(
    '<font size=''+2'' color = " ReD " name=''A B''   >',
    Rewritten);

  AssertTrue(TryRewriteTheWordFontSizeOpenV2(
    OpenLexeme, False, 0, Rewritten, Needed));
  AssertEquals(
    'first multi-attribute size is removed without disturbing followers',
    '<font color = " ReD " name=''A B''   >',
    Rewritten);

  OpenLexeme :=
    '<font color=red size = +1 name="N"   >';
  AssertTrue(TryRewriteTheWordFontSizeOpenV2(
    OpenLexeme, False, 0, Rewritten, Needed));
  AssertEquals('<font color=red name="N"   >', Rewritten);

  OpenLexeme :=
    '<font color=red name="N" size=+1   >';
  AssertTrue(TryRewriteTheWordFontSizeOpenV2(
    OpenLexeme, False, 0, Rewritten, Needed));
  AssertEquals('<font color=red name="N"   >', Rewritten);

  OpenLexeme := '<font size=-0>';
  AssertTrue(TryRewriteTheWordFontSizeOpenV2(
    OpenLexeme, False, 0, Rewritten, Needed));
  AssertFalse(Needed);
  AssertEquals('', Rewritten);

  OpenLexeme := '<font color = "red"   >';
  AssertTrue(TryRewriteTheWordFontSizeOpenV2(
    OpenLexeme, True, -1, Rewritten, Needed));
  AssertEquals(
    '<font color = "red" size=-1   >', Rewritten);
end;

procedure TTheWordFontSizeMutationV2Test.
  RewriteRejectsMalformedAndReparsesExactLexeme;
const
  UPPERCASE_RAW: UnicodeString =
    '<font SIZE=+1>x</font>';
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Needed: Boolean;
  Rewritten: UnicodeString;
begin
  AssertFalse(
    'malformed input is rejected by the internal reparse',
    TryRewriteTheWordFontSizeOpenV2(
      '<font color=red size="+1"', True, 2,
      Rewritten, Needed));
  AssertFalse(
    'uppercase SIZE is outside the lowercase parser contract',
    TryRewriteTheWordFontSizeOpenV2(
      '<font SIZE=+1>', True, 2,
      Rewritten, Needed));

  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(UPPERCASE_RAW);
  try
    AssertEquals(0, Document.FontMarkCount);
    AssertTrue(
      'unsupported uppercase SIZE remains protected opaque markup',
      Document.OpaqueItemCount > 0);
    AssertEquals(UPPERCASE_RAW, Codec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  AssertTrue(
    'the exact valid lowercase lexeme is reparsed before using its spans',
    TryRewriteTheWordFontSizeOpenV2(
      '<font color=red size="+1">', True, 2,
      Rewritten, Needed));
  AssertTrue(Needed);
  AssertEquals('<font color=red size="+2">', Rewritten);
end;
initialization
  RegisterTest(TTheWordFontSizeMutationV2Test);

end.
