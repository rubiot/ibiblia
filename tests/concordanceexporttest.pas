unit concordanceexporttest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, fpcunit, testregistry, ConcordanceExport;

type
  TConcordanceExportTest = class(TTestCase)
  published
    procedure AnalyticalOutputKeepsSurfaceFormsAndExactCounts;
    procedure SyntheticOutputAggregatesFormsByStrong;
    procedure TranslationsAreOrderedByFrequency;
    procedure ReferencesKeepMultiplicityAndOptionalContext;
    procedure VerseContextHighlightsTheExactTranslation;
    procedure VerseContextHighlightsSeparatedAssociatedTerms;
    procedure VerseContextHighlightsAssociatedRepeatedWordOnly;
    procedure RepeatedAssociationsMergeTheirExactHighlightRanges;
    procedure RelevantExcerptKeepsShortVerseWhole;
    procedure RelevantExcerptCropsBothEdgesAtWordBoundaries;
    procedure RelevantExcerptUsesAvailableContextAtVerseEdges;
    procedure RelevantExcerptRebasesUnicodeHighlightRanges;
    procedure RelevantExcerptPreservesAllMergedHighlights;
    procedure RelevantExcerptFallsBackToFullVerseForInvalidRange;
    procedure InvalidHighlightRangeDoesNotUseTextFallback;
    procedure RtfEscapingPreservesUnicodeAndControlCharacters;
    procedure TargetMorphemesKeepLegacyVerbJoining;
  end;

implementation

function ExtractRenderedReferenceContext(const Rtf: string): string;
var
  ContextEnd: Integer;
  ContextStart: Integer;
  Separator: string;
begin
  Result := '';
  Separator := UnicodeToRtf(' — ');
  ContextStart := Pos(Separator, Rtf);
  if ContextStart = 0 then
    Exit;
  Inc(ContextStart, Length(Separator));
  ContextEnd := PosEx('\par ', Rtf, ContextStart);
  if ContextEnd = 0 then
    ContextEnd := Length(Rtf) + 1;
  Result := Copy(Rtf, ContextStart, ContextEnd - ContextStart);
end;

function RenderedTextLength(const Rtf: string): Integer;
var
  Index: Integer;
begin
  Result := 0;
  Index := 1;
  while Index <= Length(Rtf) do
  begin
    if Copy(Rtf, Index, Length('{\b\ul ')) = '{\b\ul ' then
    begin
      Inc(Index, Length('{\b\ul '));
      Continue;
    end;
    if Rtf[Index] = '}' then
    begin
      Inc(Index);
      Continue;
    end;
    if (Rtf[Index] = '\') and (Index < Length(Rtf)) then
    begin
      if Rtf[Index + 1] = 'u' then
      begin
        Inc(Index, 2);
        while (Index <= Length(Rtf)) and
              (Rtf[Index] in ['-', '0'..'9']) do
          Inc(Index);
        if (Index <= Length(Rtf)) and (Rtf[Index] = '?') then
          Inc(Index);
        Inc(Result);
        Continue;
      end;
      if Rtf[Index + 1] in ['\', '{', '}'] then
      begin
        Inc(Index, 2);
        Inc(Result);
        Continue;
      end;
    end;
    Inc(Index);
    Inc(Result);
  end;
end;

procedure TConcordanceExportTest.
  AnalyticalOutputKeepsSurfaceFormsAndExactCounts;
var
  Concordance: TConcordance;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemAnalytical;
    Concordance.AddOccurrence('logos<WG3056><WTN-NSM>', 'word',
      '44,1,1', '', 2, 1);
    Concordance.AddOccurrence('logou<WG3056><WTN-GSM>', 'of word',
      '44,1,2', '', 1, 2);

    Rtf := Concordance.GetStrongRtf('logos', 'G3056');
    AssertTrue('The Strong lemma must be rendered as a section heading',
      Pos('{\fs28 ', Rtf) > 0);
    AssertTrue('Inflected forms must be one point larger than body text',
      Pos('{\fs26 ', Rtf) > 0);
    AssertTrue('Subsequent inflected forms need extra vertical separation',
      Pos('\pard\li240\sb60\sa40', Rtf) > 0);
    AssertTrue('The first analytical form needs paragraph shading',
      Pos('\pard\li240\sa40\shading10000\cfpat16\cbpat16 ', Rtf) > 0);
    AssertTrue('Subsequent analytical forms need paragraph shading',
      Pos('\pard\li240\sb60\sa40\shading10000\cfpat16\cbpat16 ',
        Rtf) > 0);
    AssertTrue(Pos('logos', Rtf) > 0);
    AssertTrue(Pos('logou', Rtf) > 0);
    AssertTrue(Pos('3 occurrences', Rtf) > 0);
    AssertTrue(Pos('2 verses', Rtf) > 0);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.SyntheticOutputAggregatesFormsByStrong;
var
  Concordance: TConcordance;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.AddOccurrence('<WG3056><WTN-NSM>', 'word',
      '44,1,1', '', 2, 1);
    Concordance.AddOccurrence('<WG3056><WTN-GSM>', 'word',
      '44,1,2', '', 3, 2);

    Rtf := Concordance.GetStrongRtf('logos', 'G3056');
    AssertTrue(Pos('word', Rtf) > 0);
    AssertTrue(Pos('5 occurrences', Rtf) > 0);
    AssertTrue(Pos('2 verses', Rtf) > 0);
    AssertEquals('Synthetic output must not add analytical paragraph shading',
      0, Pos('\shading10000\cfpat16\cbpat16 ', Rtf));
    AssertEquals('The synthetic output must render the translation once',
      Pos('word', Rtf), RPos('word', Rtf));
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.TranslationsAreOrderedByFrequency;
var
  Concordance: TConcordance;
  CommonPosition: Integer;
  RarePosition: Integer;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.AddOccurrence('<WG3056>', 'rare', '44,1,1', '', 1, 1);
    Concordance.AddOccurrence('<WG3056>', 'common', '44,1,2', '', 4, 2);
    Rtf := Concordance.GetStrongRtf('logos', 'G3056');
    CommonPosition := Pos('common', Rtf);
    RarePosition := Pos('rare', Rtf);
    AssertTrue((CommonPosition > 0) and (RarePosition > CommonPosition));
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.VerseContextHighlightsTheExactTranslation;
var
  Concordance: TConcordance;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.IncludeVerseContext := True;
    Ranges := nil;
    SetLength(Ranges, 2);
    Ranges[0].StartIndex := 9;
    Ranges[0].Length := 3;
    Ranges[1].StartIndex := 15;
    Ranges[1].Length := 3;
    Concordance.AddOccurrence('<WG1>', '<font>ser</font>', '44,1,1',
      'Servir, ser e SER.', 2, 1, Ranges);
    Rtf := Concordance.GetStrongRtf('alpha', 'G1');

    AssertTrue(Pos('{\b\ul ser}', Rtf) > 0);
    AssertTrue(Pos('{\b\ul SER}', Rtf) > 0);
    AssertEquals('A translation must not be highlighted inside another word',
      0, Pos('{\b\ul Ser}', Rtf));
    AssertEquals('Markup tags must not leak into the concordance',
      0, Pos('&lt;font&gt;', Rtf));
    AssertEquals(0, Pos('<font>', Rtf));
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  VerseContextHighlightsSeparatedAssociatedTerms;
var
  Concordance: TConcordance;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.IncludeVerseContext := True;
    Ranges := nil;
    SetLength(Ranges, 2);
    Ranges[0].StartIndex := 20;
    Ranges[0].Length := 4;
    Ranges[1].StartIndex := 34;
    Ranges[1].Length := 7;
    Concordance.AddOccurrence('<WG3596>', 'indo;caminho', '44,10,9',
      'E no dia seguinte, indo eles seu caminho, subiu Pedro.', 1, 1,
      Ranges);
    Rtf := Concordance.GetStrongRtf('hodoiporeo', 'G3596');

    AssertTrue(Pos('{\b\ul indo} eles seu {\b\ul caminho}', Rtf) > 0);
    AssertEquals('Only associated terms may be highlighted', 0,
      Pos('{\b\ul eles}', Rtf));
    AssertEquals('Only associated terms may be highlighted', 0,
      Pos('{\b\ul seu}', Rtf));
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  VerseContextHighlightsAssociatedRepeatedWordOnly;
var
  Concordance: TConcordance;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemAnalytical;
    Concordance.IncludeVerseContext := True;
    Ranges := nil;
    SetLength(Ranges, 2);
    Ranges[0].StartIndex := 14;
    Ranges[0].Length := 2;
    Ranges[1].StartIndex := 24;
    Ranges[1].Length := Length('lençol');
    Concordance.AddOccurrence('othone<WG3607>', 'um;lençol', '44,10,11',
      'um vaso como um grande lençol', 1, 1, Ranges);
    Rtf := Concordance.GetStrongRtf('othone', 'G3607');

    AssertTrue(Pos('um vaso como {\b\ul um} grande {\b\ul len', Rtf) > 0);
    AssertEquals('The equal but unassociated article must remain plain', 0,
      Pos('{\b\ul um} vaso', Rtf));
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  RepeatedAssociationsMergeTheirExactHighlightRanges;
var
  Concordance: TConcordance;
  FirstRange: TConcordanceHighlightRangeArray;
  SecondRange: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemAnalytical;
    Concordance.IncludeVerseContext := True;
    FirstRange := nil;
    SetLength(FirstRange, 1);
    FirstRange[0].StartIndex := 1;
    FirstRange[0].Length := 2;
    SecondRange := nil;
    SetLength(SecondRange, 1);
    SecondRange[0].StartIndex := 14;
    SecondRange[0].Length := 2;

    Concordance.AddOccurrence('article<WG1>', 'um', '44,10,11',
      'um vaso como um grande lencol', 1, 1, FirstRange);
    Concordance.AddOccurrence('article<WG1>', 'um', '44,10,11',
      'um vaso como um grande lencol', 1, 1, SecondRange);
    Rtf := Concordance.GetStrongRtf('article', 'G1');

    AssertTrue(Pos('{\b\ul um} vaso como {\b\ul um} grande', Rtf) > 0);
    AssertTrue('The reference must preserve both association occurrences',
      Pos('\u215?2', Rtf) > 0);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.RelevantExcerptKeepsShortVerseWhole;
var
  Concordance: TConcordance;
  Context: string;
  ContextRtf: string;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.IncludeVerseContext := True;
    AssertEquals('The compatibility property must retain full-verse mode',
      Ord(ccmFullVerse), Ord(Concordance.ContextMode));
    Concordance.ContextMode := ccmRelevantExcerpt;
    Concordance.ContextMaxCharacters := 120;
    Context := 'Uma frase curta mantém a palavra destacada.';
    Ranges := nil;
    SetLength(Ranges, 1);
    Ranges[0].StartIndex := Pos('palavra', Context);
    Ranges[0].Length := Length('palavra');

    Concordance.AddOccurrence('<WG1>', 'palavra', '44,1,1',
      Context, 1, 1, Ranges);
    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    ContextRtf := ExtractRenderedReferenceContext(Rtf);

    AssertEquals('A short verse must not receive ellipses',
      0, Pos('\u8230?', ContextRtf));
    AssertTrue(Pos('Uma frase curta', ContextRtf) > 0);
    AssertTrue(Pos('{\b\ul palavra}', ContextRtf) > 0);
    AssertTrue(Pos('destacada.', ContextRtf) > 0);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  RelevantExcerptCropsBothEdgesAtWordBoundaries;
var
  Concordance: TConcordance;
  Context: string;
  ContextRtf: string;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.ContextMode := ccmRelevantExcerpt;
    Concordance.ContextMaxCharacters := 40;
    Context := 'alpha bravo charlie delta echo foxtrot target golf hotel ' +
      'india juliet kilo lima';
    Ranges := nil;
    SetLength(Ranges, 1);
    Ranges[0].StartIndex := Pos('target', Context);
    Ranges[0].Length := Length('target');

    Concordance.AddOccurrence('<WG1>', 'target', '44,1,1',
      Context, 1, 1, Ranges);
    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    ContextRtf := ExtractRenderedReferenceContext(Rtf);

    AssertTrue('The visible excerpt must honor the configured limit',
      RenderedTextLength(ContextRtf) <= 40);
    AssertEquals('A central excerpt needs a leading ellipsis',
      1, Pos('\u8230? ', ContextRtf));
    AssertTrue('A central excerpt needs a trailing ellipsis',
      RPos(' \u8230?', ContextRtf) > 1);
    AssertEquals(0, Pos('alpha', ContextRtf));
    AssertEquals(0, Pos('lima', ContextRtf));
    AssertTrue(Pos('{\b\ul target}', ContextRtf) > 0);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  RelevantExcerptUsesAvailableContextAtVerseEdges;
var
  Concordance: TConcordance;
  Context: string;
  ContextRtf: string;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.ContextMode := ccmRelevantExcerpt;
    Concordance.ContextMaxCharacters := 40;
    Context := 'target alpha bravo charlie delta echo foxtrot golf hotel ' +
      'india juliet kilo lima';
    Ranges := nil;
    SetLength(Ranges, 1);
    Ranges[0].StartIndex := 1;
    Ranges[0].Length := Length('target');
    Concordance.AddOccurrence('<WG1>', 'target', '44,1,1',
      Context, 1, 1, Ranges);

    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    ContextRtf := ExtractRenderedReferenceContext(Rtf);
    AssertEquals('An excerpt at the start needs no leading ellipsis',
      0, Pos('\u8230? ', ContextRtf));
    AssertTrue('An excerpt at the start needs a trailing ellipsis',
      RPos(' \u8230?', ContextRtf) > 0);
    AssertTrue(RenderedTextLength(ContextRtf) <= 40);
  finally
    Concordance.Free;
  end;

  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.ContextMode := ccmRelevantExcerpt;
    Concordance.ContextMaxCharacters := 40;
    Context := 'alpha bravo charlie delta echo foxtrot golf hotel india ' +
      'juliet kilo lima omega';
    SetLength(Ranges, 1);
    Ranges[0].StartIndex := Pos('omega', Context);
    Ranges[0].Length := Length('omega');
    Concordance.AddOccurrence('<WG1>', 'omega', '44,1,1',
      Context, 1, 1, Ranges);

    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    ContextRtf := ExtractRenderedReferenceContext(Rtf);
    AssertEquals('An excerpt at the end needs a leading ellipsis',
      1, Pos('\u8230? ', ContextRtf));
    AssertEquals('An excerpt at the end needs no trailing ellipsis',
      0, RPos(' \u8230?', ContextRtf));
    AssertTrue(RenderedTextLength(ContextRtf) <= 40);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  RelevantExcerptRebasesUnicodeHighlightRanges;
var
  Concordance: TConcordance;
  Context: string;
  ContextRtf: string;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
  Target: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.ContextMode := ccmRelevantExcerpt;
    Concordance.ContextMaxCharacters := 40;
    Target := 'coração';
    Context := 'ação bênção esperança caminho distante ' + Target +
      ' próximo bênção oração esperança distante final';
    Ranges := nil;
    SetLength(Ranges, 1);
    Ranges[0].StartIndex := Pos(Target, Context);
    Ranges[0].Length := Length(Target);
    Concordance.AddOccurrence('<WG1>', Target, '44,1,1',
      Context, 1, 1, Ranges);

    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    ContextRtf := ExtractRenderedReferenceContext(Rtf);
    AssertTrue('The UTF-8 target must remain exactly highlighted',
      Pos('{\b\ul ' + UnicodeToRtf(Target) + '}', ContextRtf) > 0);
    AssertTrue('Unicode characters count as characters, not bytes',
      RenderedTextLength(ContextRtf) <= 40);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  RelevantExcerptPreservesAllMergedHighlights;
var
  Concordance: TConcordance;
  Context: string;
  ContextRtf: string;
  FirstRange: TConcordanceHighlightRangeArray;
  Rtf: string;
  SecondRange: TConcordanceHighlightRangeArray;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemAnalytical;
    Concordance.ContextMode := ccmRelevantExcerpt;
    Concordance.ContextMaxCharacters := 40;
    Context := 'prefix first one two three four five six seven eight nine ' +
      'ten eleven twelve second suffix';
    FirstRange := nil;
    SetLength(FirstRange, 1);
    FirstRange[0].StartIndex := Pos('first', Context);
    FirstRange[0].Length := Length('first');
    SecondRange := nil;
    SetLength(SecondRange, 1);
    SecondRange[0].StartIndex := Pos('second', Context);
    SecondRange[0].Length := Length('second');

    Concordance.AddOccurrence('pair<WG1>', 'pair', '44,1,1',
      Context, 1, 1, FirstRange);
    Concordance.AddOccurrence('pair<WG1>', 'pair', '44,1,1',
      Context, 1, 1, SecondRange);
    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    ContextRtf := ExtractRenderedReferenceContext(Rtf);

    AssertTrue(Pos('{\b\ul first}', ContextRtf) > 0);
    AssertTrue(Pos('{\b\ul second}', ContextRtf) > 0);
    AssertTrue('All highlights take precedence over the configured limit',
      RenderedTextLength(ContextRtf) > 40);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  RelevantExcerptFallsBackToFullVerseForInvalidRange;
var
  Concordance: TConcordance;
  Context: string;
  ContextRtf: string;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.ContextMode := ccmRelevantExcerpt;
    Concordance.ContextMaxCharacters := 40;
    Context := 'alpha bravo charlie delta echo foxtrot target golf hotel ' +
      'india juliet kilo lima omega';
    Ranges := nil;
    SetLength(Ranges, 1);
    Ranges[0].StartIndex := 999;
    Ranges[0].Length := Length('target');
    Concordance.AddOccurrence('<WG1>', 'target', '44,1,1',
      Context, 1, 1, Ranges);

    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    ContextRtf := ExtractRenderedReferenceContext(Rtf);
    AssertTrue(Pos('alpha', ContextRtf) > 0);
    AssertTrue(Pos('omega', ContextRtf) > 0);
    AssertEquals('An unsafe fallback must not add ellipses',
      0, Pos('\u8230?', ContextRtf));
    AssertEquals('An unsafe fallback must not guess a highlight',
      0, Pos('{\b\ul ', ContextRtf));
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.InvalidHighlightRangeDoesNotUseTextFallback;
var
  Concordance: TConcordance;
  Ranges: TConcordanceHighlightRangeArray;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.IncludeVerseContext := True;
    Ranges := nil;
    SetLength(Ranges, 1);
    Ranges[0].StartIndex := 99;
    Ranges[0].Length := 2;
    Concordance.AddOccurrence('<WG3607>', 'um;lençol', '44,10,11',
      'um vaso como um grande lençol', 1, 1, Ranges);
    Rtf := Concordance.GetStrongRtf('othone', 'G3607');

    AssertEquals('Invalid positions must not trigger textual highlighting',
      0, Pos('{\b\ul ', Rtf));
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  ReferencesKeepMultiplicityAndOptionalContext;
var
  Concordance: TConcordance;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.IncludeVerseContext := True;
    Concordance.AddOccurrence('<WG3056>', 'word', '44,1,1',
      'A {short} context', 2, 1);
    Rtf := Concordance.GetStrongRtf('logos', 'G3056');
    AssertTrue(Pos('2 occurrences', Rtf) > 0);
    AssertTrue(Pos('1 verse', Rtf) > 0);
    AssertTrue(Pos('\u215?2', Rtf) > 0);
    AssertTrue(Pos('A \{short\} context', Rtf) > 0);
  finally
    Concordance.Free;
  end;
end;

procedure TConcordanceExportTest.
  RtfEscapingPreservesUnicodeAndControlCharacters;
var
  Rtf: string;
begin
  Rtf := UnicodeToRtf('a\{b}' + LineEnding + UTF8Encode(WideChar($03BB)));
  AssertTrue(Pos('a\\\{b\}', Rtf) > 0);
  AssertTrue(Pos('\line ', Rtf) > 0);
  AssertTrue(Pos('\u955?', Rtf) > 0);
end;

procedure TConcordanceExportTest.TargetMorphemesKeepLegacyVerbJoining;
var
  Concordance: TConcordance;
  Rtf: string;
begin
  Concordance := TConcordance.Create;
  try
    Concordance.Mode := cemSynthetic;
    Concordance.AddOccurrence('<WG1>', 'amar-ei', '44,1,1', '', 1, 1);
    Rtf := Concordance.GetStrongRtf('alpha', 'G1');
    AssertTrue(Pos('amarei', Rtf) > 0);
    AssertEquals(0, Pos('amar-ei', Rtf));
  finally
    Concordance.Free;
  end;
end;

initialization
  RegisterTest(TConcordanceExportTest);

end.
