unit theword_effective_inline_style_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry;

type
  TTheWordEffectiveInlineStyleV2Tests = class(TTestCase)
  published
    procedure NestedFontAttributesRestoreIndependentlyAndZeroIsExplicit;
    procedure RelativeSizeLimitsUseTwoPointSteps;
    procedure SemanticPrecedenceAndHtmlCaseInsensitivity;
    procedure MalformedInnerFontDoesNotInvalidateValidOuter;
    procedure NameOnlyFontIsNeutral;
    procedure Utf16OffsetsCountEmojiAsSurrogatePair;
    procedure ExplicitColorFadeUsesExactWhiteBlend;
    procedure AddedWordsIsDistinctFromHtmlItalic;
    procedure CloneSurvivesSourceReleaseWithRunsAndNotes;
  end;

implementation

uses
  SysUtils, theword_effective_inline_style_v2;

function RequiredStyleAt(AProjection: TTheWordEffectiveInlineProjectionV2;
  AOffsetUTF16: Integer): TTheWordEffectiveInlineStyleV2;
begin
  if not AProjection.TryStyleAtVisibleUTF16(AOffsetUTF16, Result) then
    raise EAssertionFailedError.CreateFmt(
      'No style run contains visible UTF-16 offset %d', [AOffsetUTF16]);
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  NestedFontAttributesRestoreIndependentlyAndZeroIsExplicit;
const
  RAW: UnicodeString =
    '<font color=red size=+1>aa' +
    '<font color=green>bb</font>' +
    '<font size=-1>cc</font>' +
    '<font size=+0>ee</font>dd</font>';
var
  Projection: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(RAW);
  try
    AssertEquals(UnicodeString('aabbcceedd'), Projection.VisibleText);
    Style := RequiredStyleAt(Projection, 0);
    AssertEquals('outer red', LongInt($000000FF), Style.ExplicitColor);
    AssertEquals('outer +1 is two points', 2, Style.FontSizeDeltaPoints);
    Style := RequiredStyleAt(Projection, 2);
    AssertEquals('inner color replaces color only', LongInt($00008000),
      Style.ExplicitColor);
    AssertEquals('inner color inherits outer size', 2,
      Style.FontSizeDeltaPoints);
    Style := RequiredStyleAt(Projection, 4);
    AssertEquals('inner size inherits outer color', LongInt($000000FF),
      Style.ExplicitColor);
    AssertEquals('inner size replaces rather than adds', -2,
      Style.FontSizeDeltaPoints);
    Style := RequiredStyleAt(Projection, 6);
    AssertTrue('+0 remains an explicit size override', Style.HasSize);
    AssertEquals('+0 returns to the consumer base', 0,
      Style.FontSizeDeltaPoints);
    Style := RequiredStyleAt(Projection, 8);
    AssertEquals('closing restores outer size', 2,
      Style.FontSizeDeltaPoints);
  finally
    Projection.Free;
  end;
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  RelativeSizeLimitsUseTwoPointSteps;
var
  Projection: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(
    '<font size=+10>a</font><font size=-4>b</font>');
  try
    Style := RequiredStyleAt(Projection, 0);
    AssertEquals('+10 limit', 20, Style.FontSizeDeltaPoints);
    Style := RequiredStyleAt(Projection, 1);
    AssertEquals('-4 limit', -8, Style.FontSizeDeltaPoints);
  finally
    Projection.Free;
  end;
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  SemanticPrecedenceAndHtmlCaseInsensitivity;
var
  Projection: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(
    '<font color=blue><FR><B>x</b><FO>y<Fo><Fr>' +
    '<FO><FR>z<Fr><Fo></font>');
  try
    Style := RequiredStyleAt(Projection, 0);
    AssertTrue('uppercase HTML B is recognized', tweifv2Bold in Style.Flags);
    AssertEquals('FR overrides font color', LongInt($000000FF),
      Style.ExplicitColor);
    Style := RequiredStyleAt(Projection, 1);
    AssertTrue('FO implies bold with FR outside',
      tweifv2Bold in Style.Flags);
    AssertEquals('FR outside FO keeps words-of-Jesus red',
      LongInt($000000FF), Style.ExplicitColor);
    Style := RequiredStyleAt(Projection, 2);
    AssertTrue('FO implies bold with FO outside',
      tweifv2Bold in Style.Flags);
    AssertEquals('FR inside FO keeps words-of-Jesus red',
      LongInt($000000FF), Style.ExplicitColor);
  finally
    Projection.Free;
  end;
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  MalformedInnerFontDoesNotInvalidateValidOuter;
var
  Projection: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
  Offset: Integer;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(
    '<font color=red>ok<font color=bogus>x</font>y</font>');
  try
    AssertEquals(UnicodeString('okxy'), Projection.VisibleText);
    for Offset := 0 to 3 do
    begin
      Style := RequiredStyleAt(Projection, Offset);
      AssertTrue('valid outer remains explicit', Style.HasExplicitColor);
      AssertEquals('malformed inner is style-neutral', LongInt($000000FF),
        Style.ExplicitColor);
    end;
  finally
    Projection.Free;
  end;
end;

procedure TTheWordEffectiveInlineStyleV2Tests.NameOnlyFontIsNeutral;
var
  Projection: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(
    '<font name="Gentium Plus">x</font>');
  try
    Style := RequiredStyleAt(Projection, 0);
    AssertTrue('name-only font has no flags', Style.Flags = []);
    AssertFalse('font family is preserve-only', Style.HasExplicitColor);
    AssertFalse('font family does not imply size', Style.HasSize);
  finally
    Projection.Free;
  end;
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  Utf16OffsetsCountEmojiAsSurrogatePair;
const
  EMOJI: UnicodeString = #$D83D#$DE00;
var
  Projection: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(
    EMOJI + '<FR>x<Fr>');
  try
    AssertEquals('emoji occupies two UTF-16 code units', 3,
      Length(Projection.VisibleText));
    Style := RequiredStyleAt(Projection, 1);
    AssertFalse('both emoji code units are outside FR',
      Style.HasExplicitColor);
    Style := RequiredStyleAt(Projection, 2);
    AssertTrue('x begins after both surrogate code units',
      Style.HasExplicitColor);
    AssertEquals(LongInt($000000FF), Style.ExplicitColor);
  finally
    Projection.Free;
  end;
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  ExplicitColorFadeUsesExactWhiteBlend;
begin
  AssertEquals('black fades to #A3A3A3', LongInt($00A3A3A3),
    FadeTheWordExplicitColorV2($00000000));
  AssertEquals('red fades to #FFA3A3 in RGB', LongInt($00A3A3FF),
    FadeTheWordExplicitColorV2($000000FF));
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  AddedWordsIsDistinctFromHtmlItalic;
var
  Projection: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(
    '<FI>a<Fi><i>b</i>');
  try
    Style := RequiredStyleAt(Projection, 0);
    AssertTrue(tweifv2Italic in Style.Flags);
    AssertTrue(tweifv2AddedWords in Style.Flags);
    Style := RequiredStyleAt(Projection, 1);
    AssertTrue(tweifv2Italic in Style.Flags);
    AssertFalse(tweifv2AddedWords in Style.Flags);
  finally
    Projection.Free;
  end;
end;

procedure TTheWordEffectiveInlineStyleV2Tests.
  CloneSurvivesSourceReleaseWithRunsAndNotes;
var
  Projection, ProjectionClone: TTheWordEffectiveInlineProjectionV2;
  Style: TTheWordEffectiveInlineStyleV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(
    '<font color=red size=+1>x</font><RF q="7">body<Rf>');
  ProjectionClone := Projection.Clone;
  AssertTrue('clone is a distinct projection instance',
    ProjectionClone <> Projection);
  Projection.Free;
  Projection := nil;
  try
    AssertEquals(UnicodeString('x'), ProjectionClone.VisibleText);
    AssertEquals(1, ProjectionClone.RunCount);
    Style := RequiredStyleAt(ProjectionClone, 0);
    AssertEquals(LongInt($000000FF), Style.ExplicitColor);
    AssertEquals(2, Style.FontSizeDeltaPoints);
    AssertEquals(1, ProjectionClone.TranslatorNoteCount);
    AssertEquals(UnicodeString('7'),
      ProjectionClone.TranslatorNotes[0].Q);
    AssertEquals(UnicodeString('body'),
      ProjectionClone.TranslatorNotes[0].BodyRaw);
  finally
    ProjectionClone.Free;
  end;
end;

initialization
  RegisterTest(TTheWordEffectiveInlineStyleV2Tests);

end.
