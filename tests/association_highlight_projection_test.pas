unit association_highlight_projection_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, StrUtils, fpcunit, testregistry, ConcordanceExport, Verse,
  SuggestionManager, syntagm, verse_render_buffer,
  association_highlight_projection;

type
  TAssociationHighlightProjectionTest = class(TTestCase)
  published
    procedure VisibleProjectionOmitsRFAndTitlesAndRebasesTrimmedText;
    procedure AssociationProjectionUsesExactIndexesAndDeduplicates;
    procedure AssociationProjectionFailsClosedForAnyInvalidIndex;
    procedure RangeUnionSortsMergesAndValidatesAtomically;
    procedure MatchingProjectionFiltersExactPairsAndUnionsAlternatives;
    procedure MetadataProjectionMatchesCanonicalMemoryPair;
  end;

implementation

function RequiredSyntagmIndex(AVerse: TVerse; const AText: string;
  AOccurrence: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to AVerse.Syntagms.Count - 1 do
    if AVerse.Syntagms[Index].Text = AText then
    begin
      Dec(AOccurrence);
      if AOccurrence = 0 then
        Exit(Index);
    end;
  raise Exception.CreateFmt('Syntagm occurrence not found: %s', [AText]);
end;

procedure TAssociationHighlightProjectionTest.
  VisibleProjectionOmitsRFAndTitlesAndRebasesTrimmedText;
var
  Context: string;
  Ranges: TConcordanceHighlightRangeArray;
  CliticIndex: Integer;
  TargetIndex: Integer;
  VerseModel: TVerse;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.TokenizerVersion := 1;
    VerseModel.Text :=
      '  Antes <RF>nota<Rf><TS1>título<Ts> coração amá-lo depois  ';
    TargetIndex := RequiredSyntagmIndex(VerseModel, 'coração', 1);
    CliticIndex := RequiredSyntagmIndex(VerseModel, '-lo', 1);

    BuildVisibleDestinationProjection(VerseModel, Context, Ranges);

    AssertEquals('visible destination context',
      'Antes  coração amá-lo depois', Context);
    AssertEquals('trimmed UTF-8 byte offset', Pos('coração', Context),
      Ranges[TargetIndex].StartIndex);
    AssertEquals('UTF-8 byte length',
      Length(UTF8Encode(UnicodeString('coração'))),
      Ranges[TargetIndex].Length);
    AssertEquals('hyphenated clitic offset', Pos('-lo', Context),
      Ranges[CliticIndex].StartIndex);
    AssertEquals('hyphenated clitic byte length', Length('-lo'),
      Ranges[CliticIndex].Length);
  finally
    VerseModel.Free;
  end;
end;

procedure TAssociationHighlightProjectionTest.
  AssociationProjectionUsesExactIndexesAndDeduplicates;
var
  Context: string;
  Data: TAssociationPairData;
  Projected: TConcordanceHighlightRangeArray;
  SyntagmRanges: TConcordanceHighlightRangeArray;
  LargeIndex: Integer;
  SecondArticleIndex: Integer;
  SheetIndex: Integer;
  VerseModel: TVerse;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.TokenizerVersion := 1;
    VerseModel.Text := 'um vaso como um grande lençol';
    SecondArticleIndex := RequiredSyntagmIndex(VerseModel, 'um', 2);
    LargeIndex := RequiredSyntagmIndex(VerseModel, 'grande', 1);
    SheetIndex := RequiredSyntagmIndex(VerseModel, 'lençol', 1);
    BuildVisibleDestinationProjection(VerseModel, Context, SyntagmRanges);

    SetLength(Data.TargetSyntagmIndexes, 4);
    Data.TargetSyntagmIndexes[0] := SheetIndex;
    Data.TargetSyntagmIndexes[1] := SecondArticleIndex;
    Data.TargetSyntagmIndexes[2] := LargeIndex;
    Data.TargetSyntagmIndexes[3] := SecondArticleIndex;

    AssertTrue(TryProjectAssociationHighlightRanges(Data, Context,
      SyntagmRanges, Projected));
    AssertEquals('duplicate target index is removed', 3, Length(Projected));
    AssertEquals('ranges are sorted by exact occurrence',
      PosEx('um', Context, 2), Projected[0].StartIndex);
    AssertEquals(Length('um'), Projected[0].Length);
    AssertEquals(Pos('grande', Context), Projected[1].StartIndex);
    AssertEquals(Length('grande'), Projected[1].Length);
    AssertEquals(Pos('lençol', Context), Projected[2].StartIndex);
    AssertEquals(Length(UTF8Encode(UnicodeString('lençol'))),
      Projected[2].Length);
  finally
    VerseModel.Free;
  end;
end;

procedure TAssociationHighlightProjectionTest.
  MatchingProjectionFiltersExactPairsAndUnionsAlternatives;
const
  CONTEXT = 'palavra entre palavra sobra';
var
  AssociationData: TAssociationPairDataArray;
  HighlightRanges: TConcordanceHighlightRangeArray;
  Matches: TTranslationPairMatchArray;
  SyntagmRanges: TConcordanceHighlightRangeArray;
begin
  AssociationData := nil;
  HighlightRanges := nil;
  Matches := nil;
  SyntagmRanges := nil;
  SetLength(SyntagmRanges, 3);
  SyntagmRanges[0].StartIndex := Pos('palavra', CONTEXT);
  SyntagmRanges[0].Length := Length('palavra');
  SyntagmRanges[1].StartIndex := PosEx('palavra', CONTEXT, 2);
  SyntagmRanges[1].Length := Length('palavra');
  SyntagmRanges[2].StartIndex := Pos('sobra', CONTEXT);
  SyntagmRanges[2].Length := Length('sobra');

  SetLength(AssociationData, 4);
  AssociationData[0].SourcePhrase := 'logos<WG3056><WTN-NSM>';
  AssociationData[0].TargetPhrase := 'palavra';
  SetLength(AssociationData[0].TargetSyntagmIndexes, 1);
  AssociationData[0].TargetSyntagmIndexes[0] := 0;
  AssociationData[1].SourcePhrase := AssociationData[0].SourcePhrase;
  AssociationData[1].TargetPhrase := AssociationData[0].TargetPhrase;
  SetLength(AssociationData[1].TargetSyntagmIndexes, 1);
  AssociationData[1].TargetSyntagmIndexes[0] := 1;
  AssociationData[2].SourcePhrase := 'logou<WG3056><WTN-GSM>';
  AssociationData[2].TargetPhrase := 'palavra';
  SetLength(AssociationData[2].TargetSyntagmIndexes, 1);
  AssociationData[2].TargetSyntagmIndexes[0] := 1;
  AssociationData[3].SourcePhrase := 'unselected<WG1>';
  AssociationData[3].TargetPhrase := 'palavra';
  SetLength(AssociationData[3].TargetSyntagmIndexes, 1);
  AssociationData[3].TargetSyntagmIndexes[0] := 2;

  SetLength(Matches, 2);
  Matches[0].SourcePhrase := AssociationData[0].SourcePhrase;
  Matches[0].TargetPhrase := AssociationData[0].TargetPhrase;
  Matches[1].SourcePhrase := AssociationData[2].SourcePhrase;
  Matches[1].TargetPhrase := AssociationData[2].TargetPhrase;

  AssertTrue(TryProjectMatchingAssociationHighlightRanges(AssociationData,
    Matches, CONTEXT, SyntagmRanges, HighlightRanges));
  AssertEquals('identical and morphology-specific pairs are consolidated',
    2, Length(HighlightRanges));
  AssertEquals(SyntagmRanges[0].StartIndex,
    HighlightRanges[0].StartIndex);
  AssertEquals(SyntagmRanges[1].StartIndex,
    HighlightRanges[1].StartIndex);

  AssociationData[2].TargetSyntagmIndexes[0] := 99;
  AssertFalse(TryProjectMatchingAssociationHighlightRanges(AssociationData,
    Matches, CONTEXT, SyntagmRanges, HighlightRanges));
  AssertEquals('one invalid selected association clears all ranges', 0,
    Length(HighlightRanges));
end;

procedure TAssociationHighlightProjectionTest.
  MetadataProjectionMatchesCanonicalMemoryPair;
var
  AssociationData: TAssociationPairDataArray;
  Context: string;
  CorruptionDetails: string;
  HighlightRanges: TConcordanceHighlightRangeArray;
  Matches: TTranslationPairMatchArray;
  RenderBuffer: TVerseRenderBuffer;
  SyntagmRanges: TConcordanceHighlightRangeArray;
begin
  AssociationData := nil;
  HighlightRanges := nil;
  Matches := nil;
  SyntagmRanges := nil;
  RenderBuffer := TVerseRenderBuffer.Create;
  try
    RenderBuffer.SetTokenizerVersion(1);
    AssertTrue(RenderBuffer.TryExtractAssociationPairData(
      'logos<WG3056><WTN-NSM>', 'a palavra final',
      '<par a="0" b="2">', plMetaData, AssociationData,
      CorruptionDetails));
    AssertEquals(1, Length(AssociationData));
    AssertEquals('<wg3056><wtn-nsm>',
      AssociationData[0].SourcePhrase);
    AssertEquals('palavra', AssociationData[0].TargetPhrase);

    BuildVisibleDestinationProjection(RenderBuffer.DestinationVerse,
      Context, SyntagmRanges);
    SetLength(Matches, 1);
    Matches[0].SourcePhrase := '<wg3056><wtn-nsm>';
    Matches[0].TargetPhrase := 'palavra';
    AssertTrue(TryProjectMatchingAssociationHighlightRanges(
      AssociationData, Matches, Context, SyntagmRanges,
      HighlightRanges));
    AssertEquals(1, Length(HighlightRanges));
    AssertEquals(Pos('palavra', Context),
      HighlightRanges[0].StartIndex);
    AssertEquals(Length('palavra'), HighlightRanges[0].Length);
  finally
    RenderBuffer.Free;
  end;
end;

procedure TAssociationHighlightProjectionTest.
  AssociationProjectionFailsClosedForAnyInvalidIndex;
var
  Data: TAssociationPairData;
  Projected: TConcordanceHighlightRangeArray;
  SyntagmRanges: TConcordanceHighlightRangeArray;
begin
  SetLength(SyntagmRanges, 1);
  SyntagmRanges[0].StartIndex := 1;
  SyntagmRanges[0].Length := 4;
  SetLength(Data.TargetSyntagmIndexes, 2);
  Data.TargetSyntagmIndexes[0] := 0;
  Data.TargetSyntagmIndexes[1] := 99;

  AssertFalse(TryProjectAssociationHighlightRanges(Data, 'word',
    SyntagmRanges, Projected));
  AssertEquals('no partial range may escape', 0, Length(Projected));
end;

procedure TAssociationHighlightProjectionTest.
  RangeUnionSortsMergesAndValidatesAtomically;
var
  Left: TConcordanceHighlightRangeArray;
  Right: TConcordanceHighlightRangeArray;
  Union: TConcordanceHighlightRangeArray;
begin
  SetLength(Left, 2);
  Left[0].StartIndex := 5;
  Left[0].Length := 3;
  Left[1].StartIndex := 2;
  Left[1].Length := 4;
  SetLength(Right, 2);
  Right[0] := Left[1];
  Right[1].StartIndex := 9;
  Right[1].Length := 2;

  AssertTrue(TryUnionAssociationHighlightRanges('abcdefghij', Left,
    Right, Union));
  AssertEquals(2, Length(Union));
  AssertEquals(2, Union[0].StartIndex);
  AssertEquals(6, Union[0].Length);
  AssertEquals(9, Union[1].StartIndex);
  AssertEquals(2, Union[1].Length);

  Right[1].StartIndex := 99;
  AssertFalse(TryUnionAssociationHighlightRanges('abcdefghij', Left,
    Right, Union));
  AssertEquals('invalid input clears the whole union', 0, Length(Union));
end;

initialization
  RegisterTest(TAssociationHighlightProjectionTest);

end.
