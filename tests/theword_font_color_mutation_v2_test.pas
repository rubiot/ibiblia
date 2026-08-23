unit theword_font_color_mutation_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  theword_ont_codec_v2, theword_font_color_mutation_v2;

type
  TTheWordFontColorMutationV2Test = class(TTestCase)
  private
    function CountActions(
      const ALeaf: TTheWordFontColorLeafPlanV2;
      AKind: TTheWordFontColorActionKindV2): Integer;
  published
    procedure UniformAutomaticAndLiteralNoOp;
    procedure MixedAndNestedLeavesUseInnermostCarrier;
    procedure AutomaticRemovesEveryColorAncestor;
    procedure PartialSelectionKeepsLeafScopedIdentities;
    procedure TitlesAndProtectedObjectsRejectAtomically;
    procedure Utf16AndInvalidColorsRejectAtomically;
    procedure RewritePreservesEveryUnchangedAttributeLexeme;
    procedure RewriteNoOpAndCanonicalRgbAreStable;
  end;

implementation

function TTheWordFontColorMutationV2Test.CountActions(
  const ALeaf: TTheWordFontColorLeafPlanV2;
  AKind: TTheWordFontColorActionKindV2): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to High(ALeaf.Actions) do
    if ALeaf.Actions[Index].Kind = AKind then
      Inc(Result);
end;

procedure TTheWordFontColorMutationV2Test.
  UniformAutomaticAndLiteralNoOp;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontColorLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse('word');
  try
    AssertTrue(TryPlanTheWordFontColorV2(
      Document, 0, 4, True, $000000FF, Plan));
    AssertEquals(1, Length(Plan));
    AssertFalse(Plan[0].CurrentHasExplicitColor);
    AssertTrue(Plan[0].TargetHasExplicitColor);
    AssertEquals($000000FF, Plan[0].TargetEffectiveColor);
    AssertTrue(Plan[0].CarrierIsNew);
    AssertEquals(1, CountActions(Plan[0], twfcakAddColor));
    AssertTrue(TheWordFontColorPlanHasChangesV2(Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(
    '<font color = "red" size=+1>word</font>');
  try
    AssertTrue(TryPlanTheWordFontColorV2(
      Document, 0, 4, True, $000000FF, Plan));
    AssertEquals(1, Length(Plan));
    AssertTrue(Plan[0].CurrentHasExplicitColor);
    AssertEquals($000000FF, Plan[0].CurrentEffectiveColor);
    AssertEquals(0, Length(Plan[0].Actions));
    AssertFalse(TheWordFontColorPlanHasChangesV2(Plan));

    AssertTrue(TryPlanTheWordFontColorV2(
      Document, 0, 4, False, 0, Plan));
    AssertEquals(1, CountActions(Plan[0], twfcakRemoveColor));
    AssertFalse(Plan[0].Actions[0].TargetHasColor);
    AssertEquals('red', Plan[0].Actions[0].OriginalColorRaw);
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontColorMutationV2Test.
  MixedAndNestedLeavesUseInnermostCarrier;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontColorLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(
    '<font color=red size=+1>a' +
    '<font name=''N''>b</font>c</font>' +
    '<font color=blue>d</font>');
  try
    AssertTrue(TryPlanTheWordFontColorV2(
      Document, 0, 4, True, $00008000, Plan));
    AssertEquals(4, Length(Plan));

    AssertEquals(1, CountActions(Plan[0], twfcakSetColor));
    AssertEquals(1, CountActions(Plan[1], twfcakRemoveColor));
    AssertEquals(1, CountActions(Plan[1], twfcakAddColor));
    AssertEquals(1, CountActions(Plan[2], twfcakSetColor));
    AssertEquals(1, CountActions(Plan[3], twfcakSetColor));

    AssertTrue(Plan[1].Carrier.OpenOrdinal >
      Plan[1].Actions[0].Mark.OpenOrdinal);
    AssertEquals(Plan[1].Carrier.OpenOrdinal,
      Plan[1].Actions[1].Mark.OpenOrdinal);
    AssertFalse(Plan[1].Actions[1].OriginalHadColor);
    AssertEquals($00008000, Plan[1].Actions[1].TargetColor);
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontColorMutationV2Test.
  AutomaticRemovesEveryColorAncestor;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontColorLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(
    '<font color=red size=+1>a' +
    '<font color=blue name=N>b</font>c</font>');
  try
    AssertTrue(TryPlanTheWordFontColorV2(
      Document, 1, 1, False, 0, Plan));
    AssertEquals(1, Length(Plan));
    AssertTrue(Plan[0].CurrentHasExplicitColor);
    AssertEquals($00FF0000, Plan[0].CurrentEffectiveColor);
    AssertFalse(Plan[0].TargetHasExplicitColor);
    AssertEquals(2, CountActions(Plan[0], twfcakRemoveColor));
    AssertTrue(Plan[0].Actions[0].Mark.OpenOrdinal <
      Plan[0].Actions[1].Mark.OpenOrdinal);
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontColorMutationV2Test.
  PartialSelectionKeepsLeafScopedIdentities;
var
  Action: TTheWordFontColorMarkActionV2;
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontColorLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(
    '<font size=''+1'' color="red" name=''A B''>abcd</font>');
  try
    AssertTrue(TryPlanTheWordFontColorV2(
      Document, 1, 2, True, $00563412, Plan));
    AssertEquals(1, Length(Plan));
    AssertEquals(1, Plan[0].StartUTF16);
    AssertEquals(2, Plan[0].LengthUTF16);
    AssertEquals(1, Length(Plan[0].Actions));
    Action := Plan[0].Actions[0];
    AssertEquals(Ord(twfcakSetColor), Ord(Action.Kind));
    AssertEquals(0, Action.Mark.StartUTF16);
    AssertEquals(4, Action.Mark.EndUTF16);
    AssertEquals(1, Action.LeafStartUTF16);
    AssertEquals(2, Action.LeafLengthUTF16);
    AssertTrue(Action.OriginalHadColor);
    AssertEquals($000000FF, Action.OriginalColor);
    AssertEquals($00563412, Action.TargetColor);
    AssertEquals(
      '<font size=''+1'' color="red" name=''A B''>abcd</font>',
      Codec.EncodeVerse(Document));
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontColorMutationV2Test.
  TitlesAndProtectedObjectsRejectAtomically;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Plan: TTheWordFontColorLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse('<TS>Head<Ts> body');
  try
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 1, 2, True, $000000FF, Plan));
    AssertEquals(0, Length(Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('a<X>b');
  try
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 0, 2, True, $000000FF, Plan));
    AssertEquals(0, Length(Plan));
    AssertTrue('an opaque object at the endpoint is not crossed',
      TryPlanTheWordFontColorV2(
        Document, 0, 1, True, $000000FF, Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('a|b');
  try
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 0, 2, True, $000000FF, Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('a<RF q=*>note<Rf>b');
  try
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 0, 2, True, $000000FF, Plan));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse('word<WG123><WTN-NSM><wt> tail');
  try
    AssertTrue('postfix lexical objects remain attached to the word',
      TryPlanTheWordFontColorV2(
        Document, 0, 4, True, $000000FF, Plan));
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontColorMutationV2Test.
  Utf16AndInvalidColorsRejectAtomically;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Emoji: UnicodeString;
  Plan: TTheWordFontColorLeafPlanArrayV2;
begin
  Codec := TTheWordOntCodecV2.Create;
  Emoji := UnicodeString(WideChar($D83D)) + WideChar($DE00);
  Document := Codec.DecodeVerse('a' + Emoji + 'b');
  try
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 1, 1, True, $000000FF, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 2, 1, True, $000000FF, Plan));
    AssertEquals(0, Length(Plan));
    AssertTrue(TryPlanTheWordFontColorV2(
      Document, 1, 2, True, $000000FF, Plan));

    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 0, 1, True, -1, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 0, 1, True, $01000000, Plan));
    AssertEquals(0, Length(Plan));
    AssertFalse(TryPlanTheWordFontColorV2(
      Document, 0, 0, True, $000000FF, Plan));
    AssertEquals(0, Length(Plan));
  finally
    Document.Free;
  end;
end;

procedure TTheWordFontColorMutationV2Test.
  RewritePreservesEveryUnchangedAttributeLexeme;
var
  Needed: Boolean;
  OpenLexeme, Rewritten: UnicodeString;
begin
  OpenLexeme :=
    '<font size=''+1'' color = " ReD " name=''A B''   >';
  AssertTrue(TryRewriteTheWordFontColorOpenV2(
    OpenLexeme, True, $00563412, Rewritten, Needed));
  AssertTrue(Needed);
  AssertEquals(
    '<font size=''+1'' color = "#123456" name=''A B''   >',
    Rewritten);

  AssertTrue(TryRewriteTheWordFontColorOpenV2(
    OpenLexeme, False, 0, Rewritten, Needed));
  AssertTrue(Needed);
  AssertEquals('<font size=''+1'' name=''A B''   >', Rewritten);

  OpenLexeme := '<font color=red>';
  AssertTrue(TryRewriteTheWordFontColorOpenV2(
    OpenLexeme, False, 0, Rewritten, Needed));
  AssertFalse(Needed);
  AssertEquals('', Rewritten);

  OpenLexeme := '<font size = +2 name="N"   >';
  AssertTrue(TryRewriteTheWordFontColorOpenV2(
    OpenLexeme, True, $00563412, Rewritten, Needed));
  AssertTrue(Needed);
  AssertEquals(
    '<font size = +2 name="N" color=#123456   >', Rewritten);
end;

procedure TTheWordFontColorMutationV2Test.
  RewriteNoOpAndCanonicalRgbAreStable;
var
  Needed: Boolean;
  Rewritten: UnicodeString;
begin
  AssertEquals('#123456', TheWordCanonicalFontColorV2($00563412));
  AssertEquals('#FF0000', TheWordCanonicalFontColorV2($000000FF));
  AssertEquals('', TheWordCanonicalFontColorV2(-1));

  AssertTrue(TryRewriteTheWordFontColorOpenV2(
    '<font color = " ReD " size=+1>', True, $000000FF,
    Rewritten, Needed));
  AssertTrue(Needed);
  AssertEquals('same resolved color preserves the literal opening tag',
    '<font color = " ReD " size=+1>', Rewritten);

  AssertTrue(TryRewriteTheWordFontColorOpenV2(
    '<font size=+1>', False, 0, Rewritten, Needed));
  AssertTrue(Needed);
  AssertEquals('<font size=+1>', Rewritten);

  AssertFalse(TryRewriteTheWordFontColorOpenV2(
    '<font color=red', True, $000000FF, Rewritten, Needed));
  AssertFalse(TryRewriteTheWordFontColorOpenV2(
    '<font size=+1>', True, -1, Rewritten, Needed));
end;

initialization
  RegisterTest(TTheWordFontColorMutationV2Test);

end.
