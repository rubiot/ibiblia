unit theword_effective_inline_style_v2_review_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface
uses fpcunit, testregistry;
type
  TEffectiveInlineReviewTests = class(TTestCase)
  published
    procedure InnerScriptWinsAndClosingRestoresOuter;
    procedure PredecodedDocumentIsNotOwned;
  end;

implementation
uses theword_ont_codec_v2, theword_effective_inline_style_v2;

procedure TEffectiveInlineReviewTests.InnerScriptWinsAndClosingRestoresOuter;
var P: TTheWordEffectiveInlineProjectionV2; S: TTheWordEffectiveInlineStyleV2;
begin
  P := TTheWordEffectiveInlineProjectionV2.Resolve('<sup>a<sub>b</sub>c</sup>');
  try
    AssertTrue(P.TryStyleAtVisibleUTF16(0, S));
    AssertTrue(tweifv2Superscript in S.Flags); AssertFalse(tweifv2Subscript in S.Flags);
    AssertTrue(P.TryStyleAtVisibleUTF16(1, S));
    AssertFalse(tweifv2Superscript in S.Flags); AssertTrue(tweifv2Subscript in S.Flags);
    AssertTrue(P.TryStyleAtVisibleUTF16(2, S));
    AssertTrue(tweifv2Superscript in S.Flags); AssertFalse(tweifv2Subscript in S.Flags);
  finally P.Free end;
end;

procedure TEffectiveInlineReviewTests.PredecodedDocumentIsNotOwned;
var C: ITheWordOntCodecV2; D: TTheWordVerseDocument;
  P: TTheWordEffectiveInlineProjectionV2; S: TTheWordEffectiveInlineStyleV2;
begin
  C := TTheWordOntCodecV2.Create; D := C.DecodeVerse('<FR>x<Fr>');
  try
    P := TTheWordEffectiveInlineProjectionV2.Resolve(D);
    try
      AssertTrue(P.TryStyleAtVisibleUTF16(0, S));
      AssertEquals(LongInt($000000FF), S.ExplicitColor);
    finally P.Free end;
    AssertEquals(UnicodeString('x'), D.VisibleText);
  finally D.Free end;
end;

initialization RegisterTest(TEffectiveInlineReviewTests);
end.
