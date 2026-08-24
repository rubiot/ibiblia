unit syntagmselectiontest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Verse, syntagm, syntagmselection,
  ai_association_flow, ONTTokenizer;

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
    procedure TestGreekElisionAssociationAcceptsApostropheVariants;
    procedure TestGreekElisionAssociationRejectsMissingApostrophe;
    procedure TestGreekElisionTokenizer3SelectsSeparatedApostrophe;
    procedure TestGreekElisionStrongTagKeepsSeparatedKoronis;
    procedure TestGreekElisionDoesNotReuseAssociatedApostrophe;
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

procedure TSyntagmSelectionTest.TestGreekElisionAssociationAcceptsApostropheVariants;

  procedure AssertVariantApplies(const VariantName, OriginalPhrase: string);
  var
    SourceVerse, TargetVerse: TVerse;
    Association: TAssociationResult;
    AppliedCount, AssociatedLexicalCount, I: Integer;
  begin
    SourceVerse := TVerse.Create('test-source');
    TargetVerse := TVerse.Create('test-target');
    Association := TAssociationResult.Create;
    try
      SourceVerse.PairedVerse := TargetVerse;
      TargetVerse.PairedVerse := SourceVerse;
      SourceVerse.Text := 'οὕτω τὸ κατ᾽ ἐμὲ πρόθυμον';
      TargetVerse.Text := 'assim quanto a mim pronto';
      SetLength(Association.Pairs, 1);
      Association.Pairs[0].OriginalPhrase := OriginalPhrase;
      Association.Pairs[0].TranslatedPhrase := 'quanto a mim';

      AppliedCount := ApplyAssociationPairs(Association, SourceVerse);
      AssertEquals(VariantName + ': association pair must materialize',
        1, AppliedCount);

      AssociatedLexicalCount := 0;
      for I := 0 to SourceVerse.Syntagms.Count - 1 do
        if (SourceVerse.Syntagms[I].Kind = skSyntagm) and
           (SourceVerse.Syntagms[I].Pairs.Count > 0) then
          Inc(AssociatedLexicalCount);
      AssertEquals(VariantName +
        ': the complete elided phrase must be selected, not only one word',
        2, AssociatedLexicalCount);
    finally
      SourceVerse.PairedVerse := nil;
      TargetVerse.PairedVerse := nil;
      Association.Free;
      TargetVerse.Free;
      SourceVerse.Free;
    end;
  end;

begin
  AssertVariantApplies('Greek koronis U+1FBD', 'κατ᾽ ἐμὲ');
  AssertVariantApplies('ASCII apostrophe', 'κατ'' ἐμὲ');
  AssertVariantApplies('right single quotation mark U+2019', 'κατ’ ἐμὲ');
  AssertVariantApplies('modifier letter apostrophe U+02BC', 'κατʼ ἐμὲ');
end;

procedure TSyntagmSelectionTest.TestGreekElisionAssociationRejectsMissingApostrophe;
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
    SourceVerse.TokenizerVersion := 3;
    SourceVerse.Text := 'οὕτω κατ᾽ ἐμὲ πρόθυμον';
    TargetVerse.Text := 'assim quanto a mim pronto';
    SetLength(Association.Pairs, 1);
    Association.Pairs[0].OriginalPhrase := 'κατ ἐμὲ';
    Association.Pairs[0].TranslatedPhrase := 'quanto a mim';

    AssertEquals('omitting the elision apostrophe must not match', 0,
      ApplyAssociationPairs(Association, SourceVerse));
    AssertFalse('source must remain unassociated',
      SourceVerse.HasAssociations);
    AssertFalse('target must remain unassociated',
      TargetVerse.HasAssociations);
  finally
    SourceVerse.PairedVerse := nil;
    TargetVerse.PairedVerse := nil;
    Association.Free;
    TargetVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestGreekElisionTokenizer3SelectsSeparatedApostrophe;
var
  SourceVerse, TargetVerse: TVerse;
  Association: TAssociationResult;
  Words: TStringList;
  Indexes: TSyntagmIndexArray;
  ApostropheSyntagm: TSyntagm;
  ApostropheSelected: Boolean;
  AssociatedLexicalCount, I: Integer;
begin
  SourceVerse := TVerse.Create('test-source');
  TargetVerse := TVerse.Create('test-target');
  Association := TAssociationResult.Create;
  Words := CreateWords(['κατ''', 'ἐμὲ']);
  ApostropheSyntagm := nil;
  try
    SourceVerse.PairedVerse := TargetVerse;
    TargetVerse.PairedVerse := SourceVerse;
    SourceVerse.TokenizerVersion := 3;
    SourceVerse.Text := 'οὕτω κατ’ ἐμὲ πρόθυμον';
    TargetVerse.Text := 'assim quanto a mim pronto';

    Indexes := ResolveSyntagmSelectionIndexes(SourceVerse.Syntagms, Words,
      False, True);
    ApostropheSelected := False;
    for I := 0 to High(Indexes) do
      if SourceVerse.Syntagms[Indexes[I]].Text = '’' then
        ApostropheSelected := True;
    AssertTrue('JoinConnected must select the separated U+2019 apostrophe',
      ApostropheSelected);

    SetLength(Association.Pairs, 1);
    Association.Pairs[0].OriginalPhrase := 'κατ'' ἐμὲ';
    Association.Pairs[0].TranslatedPhrase := 'quanto a mim';
    AssertEquals('tokenizer v3 pair must materialize completely', 1,
      ApplyAssociationPairs(Association, SourceVerse));

    AssociatedLexicalCount := 0;
    for I := 0 to SourceVerse.Syntagms.Count - 1 do
    begin
      if (SourceVerse.Syntagms[I].Kind = skSyntagm) and
         (SourceVerse.Syntagms[I].Pairs.Count > 0) then
        Inc(AssociatedLexicalCount);
      if SourceVerse.Syntagms[I].Text = '’' then
        ApostropheSyntagm := SourceVerse.Syntagms[I];
    end;
    AssertEquals('both lexical words must be associated', 2,
      AssociatedLexicalCount);
    AssertTrue('the separated apostrophe must exist as its own syntagm',
      Assigned(ApostropheSyntagm));
    AssertEquals('the separated apostrophe is punctuation', Ord(skPunctuation),
      Ord(ApostropheSyntagm.Kind));
    AssertTrue('the separated apostrophe must belong to the association',
      ApostropheSyntagm.Pairs.Count > 0);
  finally
    Words.Free;
    SourceVerse.PairedVerse := nil;
    TargetVerse.PairedVerse := nil;
    Association.Free;
    TargetVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestGreekElisionStrongTagKeepsSeparatedKoronis;
var
  SourceVerse, TargetVerse: TVerse;
  Association: TAssociationResult;
  BaseSyntagm, KoronisSyntagm: TSyntagm;
  AssociatedWordCount, I: Integer;
begin
  SourceVerse := TVerse.Create('test-source');
  TargetVerse := TVerse.Create('test-target');
  Association := TAssociationResult.Create;
  BaseSyntagm := nil;
  KoronisSyntagm := nil;
  try
    SourceVerse.PairedVerse := TargetVerse;
    TargetVerse.PairedVerse := SourceVerse;
    SourceVerse.TokenizerVersion := 3;
    SourceVerse.Text := 'οὕτω κατ<WG2596>᾽ ἐμὲ πρόθυμον';
    TargetVerse.Text := 'assim quanto a mim pronto';
    SetLength(Association.Pairs, 1);
    Association.Pairs[0].OriginalPhrase := 'κατ'' ἐμὲ';
    Association.Pairs[0].TranslatedPhrase := 'quanto a mim';

    AssertEquals('metadata between the base and koronis must not split matching',
      1, ApplyAssociationPairs(Association, SourceVerse));

    AssociatedWordCount := 0;
    for I := 0 to SourceVerse.Syntagms.Count - 1 do
    begin
      if (SourceVerse.Syntagms[I].Kind = skSyntagm) and
         (SourceVerse.Syntagms[I].Text <> '᾽') and
         (SourceVerse.Syntagms[I].Pairs.Count > 0) then
        Inc(AssociatedWordCount);
      if SourceVerse.Syntagms[I].Text = 'κατ' then
        BaseSyntagm := SourceVerse.Syntagms[I]
      else if SourceVerse.Syntagms[I].Text = '᾽' then
        KoronisSyntagm := SourceVerse.Syntagms[I];
    end;
    AssertEquals('both lexical words must be associated', 2,
      AssociatedWordCount);
    AssertTrue('the elided base must be found', Assigned(BaseSyntagm));
    AssertTrue('the Strong metadata must remain on the base',
      BaseSyntagm.HasStrongs);
    AssertEquals('G2596', BaseSyntagm.Strong[0]);
    AssertTrue('the separated koronis must be found', Assigned(KoronisSyntagm));
    AssertEquals('the koronis keeps its legacy syntagm kind', Ord(skSyntagm),
      Ord(KoronisSyntagm.Kind));
    AssertTrue('the separated koronis must belong to the association',
      KoronisSyntagm.Pairs.Count > 0);
  finally
    SourceVerse.PairedVerse := nil;
    TargetVerse.PairedVerse := nil;
    Association.Free;
    TargetVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSyntagmSelectionTest.TestGreekElisionDoesNotReuseAssociatedApostrophe;
var
  SourceVerse, TargetVerse: TVerse;
  Association: TAssociationResult;
  Words: TStringList;
  Indexes: TSyntagmIndexArray;
  BaseSyntagm, ApostropheSyntagm, EmoiSyntagm,
    ExistingTargetSyntagm: TSyntagm;
  I: Integer;
begin
  SourceVerse := TVerse.Create('test-source');
  TargetVerse := TVerse.Create('test-target');
  Association := TAssociationResult.Create;
  Words := CreateWords(['κατ''', 'ἐμὲ']);
  BaseSyntagm := nil;
  ApostropheSyntagm := nil;
  EmoiSyntagm := nil;
  ExistingTargetSyntagm := nil;
  try
    SourceVerse.PairedVerse := TargetVerse;
    TargetVerse.PairedVerse := SourceVerse;
    SourceVerse.TokenizerVersion := 3;
    SourceVerse.Text := 'κατ’ ἐμὲ';
    TargetVerse.Text := 'existing quanto a mim';

    for I := 0 to SourceVerse.Syntagms.Count - 1 do
      if SourceVerse.Syntagms[I].Text = 'κατ' then
        BaseSyntagm := SourceVerse.Syntagms[I]
      else if SourceVerse.Syntagms[I].Text = '’' then
        ApostropheSyntagm := SourceVerse.Syntagms[I]
      else if SourceVerse.Syntagms[I].Text = 'ἐμὲ' then
        EmoiSyntagm := SourceVerse.Syntagms[I];
    for I := 0 to TargetVerse.Syntagms.Count - 1 do
      if TargetVerse.Syntagms[I].Text = 'existing' then
        ExistingTargetSyntagm := TargetVerse.Syntagms[I];

    AssertTrue('the elided base must be present', Assigned(BaseSyntagm));
    AssertTrue('the adjacent apostrophe must be present',
      Assigned(ApostropheSyntagm));
    AssertTrue('the following Greek word must be present',
      Assigned(EmoiSyntagm));
    AssertTrue('the pre-existing target must be present',
      Assigned(ExistingTargetSyntagm));

    ApostropheSyntagm.AddToSelection;
    ExistingTargetSyntagm.AddToSelection;
    SourceVerse.AssociateSyntagms;
    SourceVerse.ClearSelection;
    TargetVerse.ClearSelection;

    AssertEquals('the Greek base starts free', 0, BaseSyntagm.Pairs.Count);
    AssertEquals('the following Greek word starts free', 0,
      EmoiSyntagm.Pairs.Count);
    AssertEquals('the apostrophe has one pre-existing pair', 1,
      ApostropheSyntagm.Pairs.Count);
    AssertTrue('the apostrophe points to its pre-existing target',
      ApostropheSyntagm.Pairs[0] = ExistingTargetSyntagm);
    AssertEquals('the pre-existing target has one reciprocal pair', 1,
      ExistingTargetSyntagm.Pairs.Count);
    AssertTrue('the pre-existing target points back to the apostrophe',
      ExistingTargetSyntagm.Pairs[0] = ApostropheSyntagm);

    Indexes := ResolveSyntagmSelectionIndexes(SourceVerse.Syntagms, Words,
      False, True);
    AssertEquals('the resolver must not reuse an associated apostrophe', 0,
      Length(Indexes));

    SetLength(Association.Pairs, 1);
    Association.Pairs[0].OriginalPhrase := 'κατ'' ἐμὲ';
    Association.Pairs[0].TranslatedPhrase := 'quanto a mim';
    AssertEquals('materialization must reject the unavailable apostrophe', 0,
      ApplyAssociationPairs(Association, SourceVerse));

    AssertEquals('the Greek base remains free', 0, BaseSyntagm.Pairs.Count);
    AssertEquals('the following Greek word remains free', 0,
      EmoiSyntagm.Pairs.Count);
    AssertEquals('the apostrophe keeps exactly its original pair', 1,
      ApostropheSyntagm.Pairs.Count);
    AssertTrue('the apostrophe still points to its original target',
      ApostropheSyntagm.Pairs[0] = ExistingTargetSyntagm);
    AssertEquals('the target keeps exactly its reciprocal pair', 1,
      ExistingTargetSyntagm.Pairs.Count);
    AssertTrue('the target still points back to the apostrophe',
      ExistingTargetSyntagm.Pairs[0] = ApostropheSyntagm);
    AssertEquals('the apostrophe gains no source siblings', 0,
      ApostropheSyntagm.Siblings.Count);
    AssertEquals('the target gains no destination siblings', 0,
      ExistingTargetSyntagm.Siblings.Count);
    for I := 0 to TargetVerse.Syntagms.Count - 1 do
      if (TargetVerse.Syntagms[I].Kind = skSyntagm) and
         (TargetVerse.Syntagms[I] <> ExistingTargetSyntagm) then
        AssertEquals('new target words must remain unassociated', 0,
          TargetVerse.Syntagms[I].Pairs.Count);
  finally
    Words.Free;
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