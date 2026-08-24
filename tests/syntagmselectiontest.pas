unit syntagmselectiontest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Verse, syntagm, syntagmselection,
  ai_association_flow;

type
  TSyntagmSelectionTest = class(TTestCase)
  private
    function CreateWords(const AWords: array of string): TStringList;
    procedure AssertIndexesEqual(const Expected: array of Integer; const Actual: TSyntagmIndexArray);
  published
    procedure TestExactSequencePrefersContiguousWords;
    procedure TestFallbackSelectsOrderedGap;
    procedure TestPartialPreservesRequestedOrder;
    procedure TestAssociatedSyntagmsAreExcluded;
    procedure TestJoinConnectedAddsPunctuation;
    procedure TestJoinConnectedDoesNotCrossSpaces;
    procedure TestJoinConnectedFalseKeepsOnlyWords;
    procedure TestItalicRequirementSelectsAddedOccurrence;
    procedure TestRegularRequirementSkipsAddedOccurrence;
    procedure TestMixedRequirementsSelectMatchingSequence;
    procedure TestExistingResolverRemainsStyleNeutral;
    procedure TestStyledMaterializationDistinguishesRepeatedWords;
    procedure TestStyledMaterializationSkipsInvalidMarkers;
  end;

implementation

function TSyntagmSelectionTest.CreateWords(const AWords: array of string): TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := Low(AWords) to High(AWords) do
    Result.Add(AWords[I]);
end;

procedure TSyntagmSelectionTest.AssertIndexesEqual(const Expected: array of Integer;
  const Actual: TSyntagmIndexArray);
var
  I: Integer;
begin
  AssertEquals('Index count', Length(Expected), Length(Actual));
  for I := Low(Expected) to High(Expected) do
    AssertEquals(Format('Index %d', [I]), Expected[I], Actual[I]);
end;

procedure TSyntagmSelectionTest.TestExactSequencePrefersContiguousWords;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['minha', 'locucao']);
  try
    Verse.Text := 'minha querida locucao minha locucao';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words, False, False);
    AssertIndexesEqual([6, 8], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestFallbackSelectsOrderedGap;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['minha', 'locucao']);
  try
    Verse.Text := 'minha querida locucao';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words, False, False);
    AssertIndexesEqual([0, 4], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestPartialPreservesRequestedOrder;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['a', 'missing', 'b']);
  try
    Verse.Text := 'x a y b';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words, True, False);
    AssertIndexesEqual([2, 6], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestAssociatedSyntagmsAreExcluded;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['word1', 'word2']);
  try
    Verse.Text := 'word1 word2 word1 word2';
    Verse.Syntagms[0].Pairs.Add(Verse.Syntagms[0]);
    Verse.Syntagms[2].Pairs.Add(Verse.Syntagms[2]);
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words, False, False);
    AssertIndexesEqual([4, 6], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestJoinConnectedAddsPunctuation;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['word1', 'word2']);
  try
    Verse.Text := 'word1,word2';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words, False, True);
    AssertIndexesEqual([0, 1, 2], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestJoinConnectedDoesNotCrossSpaces;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['word1', 'word2']);
  try
    Verse.Text := 'word1 , word2';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words, False, True);
    AssertIndexesEqual([0, 4], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestJoinConnectedFalseKeepsOnlyWords;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['word1', 'word2']);
  try
    Verse.Text := 'word1 - word2';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words, False, False);
    AssertIndexesEqual([0, 4], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestItalicRequirementSelectsAddedOccurrence;
var
  Verse: TVerse;
  Words: TStringList;
  Requirements: TSyntagmStyleRequirementArray;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['ele']);
  SetLength(Requirements, 1);
  Requirements[0] := ssrItalic;
  try
    Verse.Text := '<FI>ele<Fi> disse a ele';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words,
      Requirements, False, False);
    AssertEquals('one added occurrence', 1, Length(ResultIndexes));
    AssertTrue('selected occurrence is italic',
      Verse.Syntagms[ResultIndexes[0]].IsItalic);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestRegularRequirementSkipsAddedOccurrence;
var
  Verse: TVerse;
  Words: TStringList;
  Requirements: TSyntagmStyleRequirementArray;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['ele']);
  SetLength(Requirements, 1);
  Requirements[0] := ssrRegular;
  try
    Verse.Text := '<FI>ele<Fi> disse a ele';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words,
      Requirements, False, False);
    AssertEquals('one regular occurrence', 1, Length(ResultIndexes));
    AssertFalse('selected occurrence is not italic',
      Verse.Syntagms[ResultIndexes[0]].IsItalic);
    AssertEquals('selected the final ele', 'ele',
      Verse.Syntagms[ResultIndexes[0]].Text);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestMixedRequirementsSelectMatchingSequence;
var
  Verse: TVerse;
  Words: TStringList;
  Requirements: TSyntagmStyleRequirementArray;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['ele', 'disse']);
  SetLength(Requirements, 2);
  Requirements[0] := ssrItalic;
  Requirements[1] := ssrRegular;
  try
    Verse.Text := '<FI>ele<Fi> disse ele disse';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words,
      Requirements, False, False);
    AssertEquals('two matching occurrences', 2, Length(ResultIndexes));
    AssertTrue(Verse.Syntagms[ResultIndexes[0]].IsItalic);
    AssertFalse(Verse.Syntagms[ResultIndexes[1]].IsItalic);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestExistingResolverRemainsStyleNeutral;
var
  Verse: TVerse;
  Words: TStringList;
  ResultIndexes: TSyntagmIndexArray;
begin
  Verse := TVerse.Create();
  Words := CreateWords(['ele']);
  try
    Verse.Text := '<FI>ele<Fi> disse a ele';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Syntagms, Words,
      False, False);
    AssertEquals('one occurrence', 1, Length(ResultIndexes));
    AssertTrue('old resolver still selects first text match regardless of style',
      Verse.Syntagms[ResultIndexes[0]].IsItalic);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestStyledMaterializationDistinguishesRepeatedWords;
var
  SourceVerse, TargetVerse: TVerse;
  Association: TAssociationResult;
begin
  SourceVerse := TVerse.Create('test-source');
  TargetVerse := TVerse.Create('test-target');
  Association := TAssociationResult.Create;
  try
    SourceVerse.PairedVerse := TargetVerse;
    TargetVerse.PairedVerse := SourceVerse;
    SourceVerse.Text := 'added regular';
    TargetVerse.Text := '<FI>ele<Fi> disse a ele';
    SetLength(Association.Pairs, 2);
    Association.Pairs[0].OriginalPhrase := 'added';
    Association.Pairs[0].TranslatedPhrase := '[ele]';
    Association.Pairs[1].OriginalPhrase := 'regular';
    Association.Pairs[1].TranslatedPhrase := 'ele';

    AssertEquals('both pairs applied', 2,
      ApplyAssociationPairs(Association, SourceVerse, True));
    AssertEquals(1, SourceVerse.Syntagms[0].Pairs.Count);
    AssertTrue('bracketed phrase selects the italic occurrence',
      SourceVerse.Syntagms[0].Pairs[0].IsItalic);
    AssertEquals(1, SourceVerse.Syntagms[2].Pairs.Count);
    AssertFalse('unbracketed phrase selects the regular occurrence',
      SourceVerse.Syntagms[2].Pairs[0].IsItalic);
  finally
    SourceVerse.PairedVerse := nil;
    TargetVerse.PairedVerse := nil;
    Association.Free;
    TargetVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestStyledMaterializationSkipsInvalidMarkers;
var
  SourceVerse, TargetVerse: TVerse;
  Association: TAssociationResult;
begin
  SourceVerse := TVerse.Create('test-source');
  TargetVerse := TVerse.Create('test-target');
  Association := TAssociationResult.Create;
  try
    SourceVerse.PairedVerse := TargetVerse;
    TargetVerse.PairedVerse := SourceVerse;
    SourceVerse.Text := 'source';
    TargetVerse.Text := '<FI>target<Fi>';
    SetLength(Association.Pairs, 1);
    Association.Pairs[0].OriginalPhrase := 'source';
    Association.Pairs[0].TranslatedPhrase := '[target';

    AssertEquals('invalid marked pair is ignored', 0,
      ApplyAssociationPairs(Association, SourceVerse, True));
    AssertEquals('source remains unassociated', 0,
      SourceVerse.Syntagms[0].Pairs.Count);
  finally
    SourceVerse.PairedVerse := nil;
    TargetVerse.PairedVerse := nil;
    Association.Free;
    TargetVerse.Free;
    SourceVerse.Free;
  end;
end;

initialization
  RegisterTest(TSyntagmSelectionTest);

end.