unit syntagmselectiontest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Verse, syntagm, syntagmselection;

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
    Verse.Texto := 'minha querida locucao minha locucao';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Sintagmas, Words, False, False);
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
    Verse.Texto := 'minha querida locucao';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Sintagmas, Words, False, False);
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
    Verse.Texto := 'x a y b';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Sintagmas, Words, True, False);
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
    Verse.Texto := 'word1 word2 word1 word2';
    Verse.Sintagmas[0].Pairs.Add(Verse.Sintagmas[0]);
    Verse.Sintagmas[2].Pairs.Add(Verse.Sintagmas[2]);
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Sintagmas, Words, False, False);
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
    Verse.Texto := 'word1,word2';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Sintagmas, Words, False, True);
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
    Verse.Texto := 'word1 , word2';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Sintagmas, Words, False, True);
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
    Verse.Texto := 'word1 - word2';
    ResultIndexes := ResolveSyntagmSelectionIndexes(Verse.Sintagmas, Words, False, False);
    AssertIndexesEqual([0, 4], ResultIndexes);
  finally
    Words.Free;
    Verse.Free;
  end;
end;

initialization
  RegisterTest(TSyntagmSelectionTest);

end.