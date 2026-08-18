unit ibibliatest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fpcunit, testutils, testregistry, Verse, Syntagm,
  ONTTokenizer, theword_ont_codec_v2, theword_wysiwyg_session_v2,
  text_edit_provenance;

type

  TRenderFailingVerse = class(TVerse)
  private
    FFailRender: Boolean;
    FRenderAttempts: Integer;
  public
    procedure Render; override;
    property FailRender: Boolean read FFailRender write FFailRender;
    property RenderAttempts: Integer read FRenderAttempts;
  end;

  TPair = array of array of string;
  TPairs = array of TPair;

  { TVerseTests }

  TVerseTests = class(TTestCase)
  private
    verse1: TVerse;
    verse2: TVerse;
    FVerseChangeCount: Integer;

    procedure VerseChanged;
    procedure Associate(const source: array of const; const dest: array of const);
    procedure AssociateByText(const pair: TPair);
    procedure AssociatePairs(const pairs: TPairs);
    procedure AssertPairs(const pairs: TPairs);
    procedure AssertPair(const pair: TPair);
    procedure AssertPair(const source, dest: TSyntagmList);
    procedure AssertSyntagmListEquals(const list1, list2: TSyntagmList);
    procedure AssertSyntagmListEquals(const list: TSyntagmList; const values: array of const);
    procedure AssertPairsAndSiblings(syntagm: TSyntagm; const pairs, siblings: array of const);
    function FindIndexOf(verse: TVerse; word: string): Integer;
    function CreateStringList(const AWords: array of string): TStringList; // Helper
    procedure AssertSemanticFormattingRoundTripPreservesAssociation(
      const OpenTag, CloseTag: string);
    procedure AssertSemanticToggleSplitPreservesAssociation(
      AKind: TTheWordSemanticKind);
    procedure AssertSemanticPunctuationTogglePreservesAssociation(
      AKind: TTheWordSemanticKind);
    procedure AssertSemanticApplyPreservesAssociation(
      AKind: TTheWordSemanticKind);
    procedure AssertTracedSemanticRoundTripPreservesAssociations(
      AKind: TTheWordSemanticKind);
    procedure AssertSelectionEqualsIndices(const ExpectedIndices: array of Integer); // Helper
    function CollectTokenizerTokens(version: Integer; const AText: string): TStringList;
    procedure AssertTokenizerTokens(version: Integer; const AText: string; const Expected: array of string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure SetTexto;
    procedure SyntagmsParsing;
    procedure StrongsParsing;
    procedure MorphologyParsing;
    procedure AssociationOneToOne;
    procedure AssociationOneToMany;
    procedure AssociationManyToOne;
    procedure AssociationManyToMany;
    procedure ReplaceSourceTextSameText;
    procedure ReplaceDestinationTextSameText;
    procedure ReplaceSourceTextChangeBeginning;
    procedure ReplaceDestinationTextChangeBeginning;
    procedure ReplaceSourceTextChangeEnd;
    procedure ReplaceDestinationTextChangeEnd;
    procedure ReplaceSourceTextChangeMiddle;
    procedure ReplaceDestinationTextChangeMiddle;
    procedure ReplaceTextTwice;
    procedure ReplaceSourceTextVerticalBar;
    procedure ReplaceDestinationTextVerticalBar;
    procedure ReplaceDestinationTextManyWords;
    procedure ReplaceSourceTextAddPunctuation;
    procedure SemanticFIFormattingPreservesAssociation;
    procedure SemanticFRFormattingPreservesAssociation;
    procedure SemanticFOFormattingPreservesAssociation;
    procedure SemanticFUFormattingPreservesAssociation;
    procedure SemanticFIToggleSplitPreservesAssociation;
    procedure SemanticFRToggleSplitPreservesAssociation;
    procedure SemanticFOToggleSplitPreservesAssociation;
    procedure SemanticFUToggleSplitPreservesAssociation;
    procedure SemanticFIPunctuationTogglePreservesAssociation;
    procedure SemanticFRPunctuationTogglePreservesAssociation;
    procedure SemanticFOPunctuationTogglePreservesAssociation;
    procedure SemanticFUPunctuationTogglePreservesAssociation;
    procedure SemanticFIApplyPreservesAssociation;
    procedure SemanticFRApplyPreservesAssociation;
    procedure SemanticFOApplyPreservesAssociation;
    procedure SemanticFUApplyPreservesAssociation;
    procedure SemanticTogglePreflightProtectsAssociationIdentity;
    procedure SemanticTogglePreflightRejectsSplitWhitespace;
    procedure TracedEditPreservesUntouchedRepeatedWord;
    procedure TracedInsertionsPreserveShiftedAssociations;
    procedure TracedSplitAndMergeRemoveOnlyAffectedMembers;
    procedure TracedEditShrinksManyToManyAssociation;
    procedure TracedEditPreservesPunctuationAfterProtectedTitle;
    procedure TracedEditProjectsStructuralBoundaries;
    procedure TracedEditPairsRoundTripWithShiftedIndexes;
    procedure TracedRawEditDoesNotStealExactRepeatedWord;
    procedure TracedEditProjectionFailureIsAtomic;
    procedure TracedUnicodeReplacementPreservesUntouchedLexicalWord;
    procedure TracedEditRejectsTouchedStrongWordAtomically;
    procedure TracedEditCanPopulateEmptyVerse;
    procedure TracedSemanticRoundTripPreservesAssociations;
    procedure TracedCutPasteDoesNotTransferAssociation;
    procedure TracedSemanticSplitRemovesOnlyAffectedAssociation;
    procedure TracedLegacyFallbackValidatesTraceAndProtectedWords;
    procedure TracedLegacyFallbackUsesHistoricalHeuristic;
    procedure TracedEditRenderFailureRemainsCommitted;
    procedure TracedFallbackRenderFailureRemainsCommitted;
    procedure TokenizerVersion1KeepsCurlyQuotesInsideWords;
    procedure TokenizerVersion2KeepsCurlyQuotesInsideWords;
    procedure TokenizerVersion1KeepsHebrewMaqafInsideWord;
    procedure TokenizerVersions2And3TreatHebrewMaqafAsPunctuation;
    procedure TokenizerVersionsKeepAsciiApostropheInsideWords;
    procedure TokenizerVersion3TreatsCurlyQuotesAsPunctuation;
    procedure VerseTokenizerVersionOverridesGlobalPreferredVersion;
    procedure RepeatedModifiedAssignmentsNotifyEveryEdit;
    // New tests for FindAndSelectSyntagms
    procedure TestFindAndSelectSyntagms_SimpleSequence;
    procedure TestFindAndSelectSyntagms_NotFound;
    procedure TestFindAndSelectSyntagms_PartialTrue_SomeFound;
    procedure TestFindAndSelectSyntagms_PartialFalse_NotAllFound;
    procedure TestFindAndSelectSyntagms_AvoidAlreadySelected_Sequential;
    procedure TestFindAndSelectSyntagms_JoinConnected_DoesNotCrossSpaces;
    procedure TestFindAndSelectSyntagms_EmptyWordsList;
    procedure TestFindAndSelectSyntagms_SequentialSkipsSelected;
    procedure TestFindAndSelectSyntagms_AnyOrderSkipsSelected_FullMatch;
    procedure TestFindAndSelectSyntagms_AnyOrder_PartialTrue_SkipsSelected;
    procedure TestFindAndSelectSyntagms_SequenceWithIntermediateNonWords;
    procedure TestFindAndSelectSyntagms_AnyOrderWithDuplicatesInSearchList;
  end;

implementation

procedure TRenderFailingVerse.Render;
begin
  Inc(FRenderAttempts);
  if FFailRender then
    raise Exception.Create('deliberate post-commit render failure');
  inherited Render;
end;

procedure TVerseTests.VerseChanged;
begin
  Inc(FVerseChangeCount);
end;

procedure TVerseTests.TestHookUp;
begin
  AssertTrue(verse1.PairedVerse = verse2);
  AssertTrue(verse2.PairedVerse = verse1);
end;

procedure TVerseTests.SetTexto;
var
  sample: string;
begin
  sample := 'Sample<WG1><WTPREP> verse<WTPREP l="verse"> <FI>text<Fi>.';
  verse1.Text := sample;
  AssertEquals(sample, verse1.XML);
  AssertEquals('Sample verse <FI>text<Fi>.', verse1.Text);
  AssertEquals('Sample verse text.', verse1.PlainText);
end;

procedure TVerseTests.SyntagmsParsing;
begin
  verse1.Text := 'Sample<WG1><WTPREP> verse<WH100><WH11><WTPREP l="verse"> <FI>text<Fi> x|z.';
  AssertTrue(verse1.Syntagms[0].Text = 'Sample');
  AssertTrue(verse1.Syntagms[1].Text = ' ');
  AssertTrue(verse1.Syntagms[2].Text = 'verse');
  AssertTrue(verse1.Syntagms[3].Text = ' ');
  AssertTrue(verse1.Syntagms[4].Text = '<FI>');
  AssertTrue(verse1.Syntagms[5].Text = 'text');
  AssertTrue(verse1.Syntagms[6].Text = '<Fi>');
  AssertTrue(verse1.Syntagms[7].Text = ' ');
  AssertTrue(verse1.Syntagms[8].Text = 'x');
  AssertTrue(verse1.Syntagms[9].Text = 'z');
  AssertTrue(verse1.Syntagms[10].Text = '.');
end;

procedure TVerseTests.StrongsParsing;
var
  s: TSyntagm;
begin
  verse1.Text := 'word';
  s := verse1.Syntagms.First;
  AssertTrue(s.HasStrongs = false);

  verse1.Text := 'word<WG1>';
  s := verse1.Syntagms.First;
  AssertTrue(s.HasStrongs = true);
  AssertEquals(1, s.Strong.Count);
  AssertEquals('G1', s.Strong[0]);

  verse1.Text := 'verse<WH100><WH11>';
  s := verse1.Syntagms.First;
  AssertEquals(2, s.Strong.Count);
  AssertEquals('H100', s.Strong[0]);
  AssertEquals('H11', s.Strong[1]);
end;

procedure TVerseTests.MorphologyParsing;
var
  s: TSyntagm;
begin
  verse1.Text := 'word';
  s := verse1.Syntagms.First;
  AssertEquals(0, s.Morph.Count);

  verse1.Text := 'word<WTPREP>';
  s := verse1.Syntagms.First;
  AssertEquals(1, s.Morph.Count);
  AssertEquals('PREP', s.Morph[0]);

  verse1.Text := 'word<WTPREP l="prep">';
  s := verse1.Syntagms.First;
  AssertEquals(1, s.Morph.Count);
  AssertEquals('PREP l="prep"', s.Morph[0]);

  verse1.Text := 'word<WG1><WTV-AA3S><WG33><WTPREP>';
  s := verse1.Syntagms.First;
  AssertEquals(2, s.Morph.Count);
  AssertEquals('V-AA3S', s.Morph[0]);
  AssertEquals('PREP', s.Morph[1]);

  verse1.Text := 'word<WTPREP l="prep">';
  AssertEquals('prep', verse1.Syntagms.First.Lemma);

  verse1.Text := 'word<WTPREP le="λόγος">';
  AssertEquals('λόγος', verse1.Syntagms.First.Lemma);

  verse1.Text := 'word<WTPREP lemma="מִלָּה">';
  AssertEquals('מִלָּה', verse1.Syntagms.First.Lemma);

  verse1.Text :=
    'word<WTPREP l="ignored" le="first"><WTN lemma="second">';
  AssertEquals('first;second', verse1.Syntagms.First.Lemma);

  verse1.Text := 'word<WTPREP le="unterminated>';
  AssertEquals('', verse1.Syntagms.First.Lemma);
end;

procedure TVerseTests.RepeatedModifiedAssignmentsNotifyEveryEdit;
begin
  verse1.OnVerseChanged := @VerseChanged;
  verse1.Modified := True;
  verse1.Modified := True;

  AssertEquals('Every completed edit must refresh the net pending state', 2,
    FVerseChangeCount);
  verse1.Modified := False;
  AssertEquals('Clearing the internal hint is not itself an edit', 2,
    FVerseChangeCount);
  verse1.Modified := True;
  AssertEquals(3, FVerseChangeCount);
end;

procedure TVerseTests.AssociationOneToOne;
var
  original, translation: TSyntagm;
begin
  verse1.Text := 'original';
  verse2.Text := 'translation';
  Associate([0], [0]);

  original    := verse1.Syntagms.First;
  translation := verse2.Syntagms.First;

  AssertPairsAndSiblings(original,    [translation], []);
  AssertPairsAndSiblings(translation, [original   ], []);
end;

procedure TVerseTests.AssociationOneToMany;
var
  original, translation1, translation2: TSyntagm;
begin
  verse1.Text := 'original';
  verse2.Text := 'translation1 translation2';
  Associate([0], [0, 2]);

  original     := verse1.Syntagms[0];
  translation1 := verse2.Syntagms[0];
  translation2 := verse2.Syntagms[2];

  AssertPairsAndSiblings(original,     [translation1, translation2], []);
  AssertPairsAndSiblings(translation1, [original], [translation2]);
  AssertPairsAndSiblings(translation2, [original], [translation1]);
end;

procedure TVerseTests.AssociationManyToOne;
var
  original1, original2, translation1: TSyntagm;
begin
  verse1.Text := 'original1 original2';
  verse2.Text := 'translation1';
  Associate([0,2], [0]);

  original1    := verse1.Syntagms[0];
  original2    := verse1.Syntagms[2];
  translation1 := verse2.Syntagms[0];

  AssertPairsAndSiblings(original1,    [translation1], [original2]);
  AssertPairsAndSiblings(original2,    [translation1], [original1]);
  AssertPairsAndSiblings(translation1, [original1, original2], []);
end;

procedure TVerseTests.AssociationManyToMany;
var
  original1, original2, translation1, translation2: TSyntagm;
begin
  verse1.Text := 'original1 original2';
  verse2.Text := 'translation1 translation2';
  Associate([0,2], [0,2]);

  original1    := verse1.Syntagms[0];
  original2    := verse1.Syntagms[2];
  translation1 := verse2.Syntagms[0];
  translation2 := verse2.Syntagms[2];

  AssertPairsAndSiblings(original1,    [translation1, translation2], [original2]);
  AssertPairsAndSiblings(original2,    [translation1, translation2], [original1]);
  AssertPairsAndSiblings(translation1, [original1, original2], [translation2]);
  AssertPairsAndSiblings(translation2, [original1, original2], [translation1]);
end;

procedure TVerseTests.ReplaceSourceTextSameText;
var
  original1, original2, translation1, translation2: TSyntagm;
begin
  verse1.Text := 'original1 original2';
  verse2.Text := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Syntagms[0];
  original2    := verse1.Syntagms[2];
  translation1 := verse2.Syntagms[0];
  translation2 := verse2.Syntagms[2];

  verse1.ChangeText('original1 original2');

  AssertPairsAndSiblings(original1,    [translation1], []);
  AssertPairsAndSiblings(original2,    [translation2], []);
  AssertPairsAndSiblings(translation1, [original1   ], []);
  AssertPairsAndSiblings(translation2, [original2   ], []);
end;

procedure TVerseTests.ReplaceDestinationTextSameText;
var
  original1, original2, translation1, translation2: TSyntagm;
begin
  verse1.Text := 'translation1 translation2';
  verse2.Text := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  translation1 := verse1.Syntagms[0];
  translation2 := verse1.Syntagms[2];
  original1    := verse2.Syntagms[0];
  original2    := verse2.Syntagms[2];

  verse2.ChangeText('original1 original2');

  AssertPairsAndSiblings(original1,    [translation1], []);
  AssertPairsAndSiblings(original2,    [translation2], []);
  AssertPairsAndSiblings(translation1, [original1   ], []);
  AssertPairsAndSiblings(translation2, [original2   ], []);
end;

procedure TVerseTests.AssertSemanticFormattingRoundTripPreservesAssociation(
  const OpenTag, CloseTag: string);
var
  SourceSyntagm: TSyntagm;
  TargetSyntagm: TSyntagm;
  FormattedText: string;
begin
  verse1.Text := 'source';
  verse2.Text := 'prefix target suffix';
  Associate([0], [2]);

  SourceSyntagm := verse1.Syntagms[0];
  TargetSyntagm := verse2.Syntagms[2];
  FormattedText := 'prefix ' + OpenTag + 'target' + CloseTag + ' suffix';

  verse2.ChangeText(FormattedText);

  AssertEquals('semantic markup must be retained', FormattedText, verse2.XML);
  AssertTrue('applying semantic markup must retain the target syntagm object',
    verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);

  verse2.ChangeText('prefix target suffix');

  AssertTrue('removing semantic markup must retain the target syntagm object',
    verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);
end;

procedure TVerseTests.AssertSemanticToggleSplitPreservesAssociation(
  AKind: TTheWordSemanticKind);
var
  CloseTag, Expected, OpenTag, Raw: UnicodeString;
  UpdatedPairs: string;
  PrefixSyntagm, SourceSyntagm, SuffixSyntagm, TargetSyntagm: TSyntagm;
  ReloadedSource, ReloadedTarget: TVerse;
  ReloadedSourceSyntagm, ReloadedTargetSyntagm: TSyntagm;
  Session: TTheWordWysiwygSessionV2;
  ReloadedSourceIndex, ReloadedTargetIndex, TargetIndex: Integer;
begin
  OpenTag := TheWordCanonicalOpen(AKind);
  CloseTag := TheWordCanonicalClose(AKind);
  Raw := OpenTag + 'prefix target<WG123><WTN-NSM><wt> suffix' + CloseTag;
  Expected := OpenTag + 'prefix ' + CloseTag +
    'target<WG123><WTN-NSM><wt>' + OpenTag + ' suffix' + CloseTag;

  verse1.Text := 'source';
  verse2.Text := UTF8Encode(Raw);
  TargetIndex := FindIndexOf(verse2, 'target');
  PrefixSyntagm := verse2.Syntagms[FindIndexOf(verse2, 'prefix')];
  TargetSyntagm := verse2.Syntagms[TargetIndex];
  SuffixSyntagm := verse2.Syntagms[FindIndexOf(verse2, 'suffix')];
  if AKind = twsmItalic then
    AssertTrue('fixture target must initially be italic',
      TargetSyntagm.IsItalic);
  AssertEquals('fixture target must have its Strong payload', 1,
    TargetSyntagm.Strong.Count);
  AssertEquals('G123', TargetSyntagm.Strong[0]);
  AssertEquals('fixture target must have its morphology payload', 1,
    TargetSyntagm.Morph.Count);
  AssertEquals('N-NSM', TargetSyntagm.Morph[0]);
  Associate([0], [TargetIndex]);
  SourceSyntagm := verse1.Syntagms[0];

  Session := TTheWordWysiwygSessionV2.Create(Raw);
  try
    AssertTrue(Session.ApplySemantic(AKind,
      Length(UnicodeString('prefix ')), Length(UnicodeString('target'))));
    AssertEquals(Expected, Session.RawText);
    AssertTrue('semantic split must be safe for legacy Associations',
      Session.ChangesAreSemanticOnly);
    AssertTrue(CanCommitLegacyWysiwygChangeV2(True,
      Session.ChangesAreSemanticOnly));

    verse2.ChangeText(UTF8Encode(Session.RawText));
    AssertEquals(UTF8Encode(Expected), verse2.XML);
  finally
    Session.Free;
  end;

  AssertTrue('prefix Syntagm identity must survive semantic split',
    verse2.Syntagms.IndexOf(PrefixSyntagm) >= 0);
  AssertTrue('associated target Syntagm identity must survive semantic split',
    verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertTrue('suffix Syntagm identity must survive semantic split',
    verse2.Syntagms.IndexOf(SuffixSyntagm) >= 0);
  if AKind = twsmItalic then
  begin
    AssertTrue('prefix must retain refreshed italic presentation',
      PrefixSyntagm.IsItalic);
    AssertFalse('target must adopt the parsed non-italic presentation',
      TargetSyntagm.IsItalic);
    AssertTrue('suffix must retain refreshed italic presentation',
      SuffixSyntagm.IsItalic);
  end;
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);
  AssertEquals('semantic split must retain the associated Strong payload', 1,
    TargetSyntagm.Strong.Count);
  AssertEquals('G123', TargetSyntagm.Strong[0]);
  AssertEquals('semantic split must retain the associated morphology', 1,
    TargetSyntagm.Morph.Count);
  AssertEquals('N-NSM', TargetSyntagm.Morph[0]);

  { Extra semantic tags legitimately shift the persisted Syntagm indexes.
    Verify the real contract by loading the updated text and updated pairs in
    a fresh Verse Pair. }
  UpdatedPairs := verse1.Pairs;
  AssertTrue('updated Association serialization must not be empty',
    UpdatedPairs <> '');
  ReloadedSource := TVerse.Create;
  ReloadedTarget := TVerse.Create;
  try
    ReloadedSource.PairedVerse := ReloadedTarget;
    ReloadedSource.Text := verse1.XML;
    ReloadedTarget.Text := verse2.XML;
    ReloadedSourceIndex := FindIndexOf(ReloadedSource, 'source');
    ReloadedTargetIndex := FindIndexOf(ReloadedTarget, 'target');
    ReloadedSourceSyntagm := ReloadedSource.Syntagms[ReloadedSourceIndex];
    ReloadedTargetSyntagm := ReloadedTarget.Syntagms[ReloadedTargetIndex];

    ReloadedSource.Pairs := UpdatedPairs;

    AssertPairsAndSiblings(ReloadedSourceSyntagm,
      [ReloadedTargetSyntagm], []);
    AssertPairsAndSiblings(ReloadedTargetSyntagm,
      [ReloadedSourceSyntagm], []);
    AssertEquals('reloaded target must retain its Strong payload', 1,
      ReloadedTargetSyntagm.Strong.Count);
    AssertEquals('G123', ReloadedTargetSyntagm.Strong[0]);
    AssertEquals('reloaded target must retain its morphology payload', 1,
      ReloadedTargetSyntagm.Morph.Count);
    AssertEquals('N-NSM', ReloadedTargetSyntagm.Morph[0]);
  finally
    ReloadedSource.Free;
    ReloadedTarget.Free;
  end;
end;

procedure TVerseTests.AssertSemanticPunctuationTogglePreservesAssociation(
  AKind: TTheWordSemanticKind);
var
  CloseTag, Expected, OpenTag, Raw: UnicodeString;
  PunctuationIndex, ReloadedSourceIndex, ReloadedTargetIndex: Integer;
  ReloadedSource, ReloadedTarget: TVerse;
  ReloadedSourceSyntagm, ReloadedTargetSyntagm: TSyntagm;
  Session: TTheWordWysiwygSessionV2;
  SourceSyntagm, TargetSyntagm: TSyntagm;
  UpdatedPairs: string;
begin
  OpenTag := TheWordCanonicalOpen(AKind);
  CloseTag := TheWordCanonicalClose(AKind);
  Raw := OpenTag + 'word!' + CloseTag;
  Expected := OpenTag + 'word' + CloseTag + '!';

  verse1.Text := 'source';
  verse2.Text := UTF8Encode(Raw);
  PunctuationIndex := FindIndexOf(verse2, '!');
  Associate([0], [PunctuationIndex]);
  SourceSyntagm := verse1.Syntagms[0];
  TargetSyntagm := verse2.Syntagms[PunctuationIndex];

  Session := TTheWordWysiwygSessionV2.Create(Raw);
  try
    AssertTrue(Session.ApplySemantic(AKind, Length(UnicodeString('word')), 1));
    AssertEquals(Expected, Session.RawText);
    AssertTrue(Session.ChangesAreSemanticOnly);
    AssertTrue(CanCommitLegacyWysiwygChangeV2(True,
      Session.ChangesAreSemanticOnly));
    verse2.ChangeText(UTF8Encode(Session.RawText));
  finally
    Session.Free;
  end;

  AssertTrue('associated punctuation identity must survive semantic toggle',
    verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);

  UpdatedPairs := verse1.Pairs;
  ReloadedSource := TVerse.Create;
  ReloadedTarget := TVerse.Create;
  try
    ReloadedSource.PairedVerse := ReloadedTarget;
    ReloadedSource.Text := verse1.XML;
    ReloadedTarget.Text := verse2.XML;
    ReloadedSourceIndex := FindIndexOf(ReloadedSource, 'source');
    ReloadedTargetIndex := FindIndexOf(ReloadedTarget, '!');
    ReloadedSourceSyntagm := ReloadedSource.Syntagms[ReloadedSourceIndex];
    ReloadedTargetSyntagm := ReloadedTarget.Syntagms[ReloadedTargetIndex];
    ReloadedSource.Pairs := UpdatedPairs;
    AssertPairsAndSiblings(ReloadedSourceSyntagm,
      [ReloadedTargetSyntagm], []);
    AssertPairsAndSiblings(ReloadedTargetSyntagm,
      [ReloadedSourceSyntagm], []);
  finally
    ReloadedSource.Free;
    ReloadedTarget.Free;
  end;
end;

procedure TVerseTests.AssertSemanticApplyPreservesAssociation(
  AKind: TTheWordSemanticKind);
var
  CloseTag, Expected, OpenTag, Raw: UnicodeString;
  ReloadedSource, ReloadedTarget: TVerse;
  ReloadedSourceIndex, ReloadedTargetIndex, TargetIndex: Integer;
  ReloadedSourceSyntagm, ReloadedTargetSyntagm: TSyntagm;
  Session: TTheWordWysiwygSessionV2;
  SourceSyntagm, TargetSyntagm: TSyntagm;
  UpdatedPairs: string;
begin
  OpenTag := TheWordCanonicalOpen(AKind);
  CloseTag := TheWordCanonicalClose(AKind);
  Raw := 'prefix target<WG123><WTN-NSM><wt> suffix';
  Expected := 'prefix ' + OpenTag +
    'target<WG123><WTN-NSM><wt>' + CloseTag + ' suffix';

  verse1.Text := 'source';
  verse2.Text := UTF8Encode(Raw);
  TargetIndex := FindIndexOf(verse2, 'target');
  Associate([0], [TargetIndex]);
  SourceSyntagm := verse1.Syntagms[0];
  TargetSyntagm := verse2.Syntagms[TargetIndex];

  Session := TTheWordWysiwygSessionV2.Create(Raw);
  try
    AssertTrue(Session.ApplySemantic(AKind,
      Length(UnicodeString('prefix ')), Length(UnicodeString('target'))));
    AssertEquals(Expected, Session.RawText);
    AssertTrue(Session.ChangesAreSemanticOnly);
    AssertTrue(CanCommitLegacyWysiwygChangeV2(True,
      Session.ChangesAreSemanticOnly));
    AssertTrue(verse2.CanChangeTextPreservingAssociationIdentity(
      UTF8Encode(Session.RawText)));
    verse2.ChangeText(UTF8Encode(Session.RawText));
  finally
    Session.Free;
  end;

  AssertTrue('associated target identity must survive semantic apply',
    verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);
  AssertEquals(1, TargetSyntagm.Strong.Count);
  AssertEquals('G123', TargetSyntagm.Strong[0]);
  AssertEquals(1, TargetSyntagm.Morph.Count);
  AssertEquals('N-NSM', TargetSyntagm.Morph[0]);
  if AKind = twsmItalic then
    AssertTrue('associated target must adopt italic presentation immediately',
      TargetSyntagm.IsItalic);

  UpdatedPairs := verse1.Pairs;
  ReloadedSource := TVerse.Create;
  ReloadedTarget := TVerse.Create;
  try
    ReloadedSource.PairedVerse := ReloadedTarget;
    ReloadedSource.Text := verse1.XML;
    ReloadedTarget.Text := verse2.XML;
    ReloadedSourceIndex := FindIndexOf(ReloadedSource, 'source');
    ReloadedTargetIndex := FindIndexOf(ReloadedTarget, 'target');
    ReloadedSourceSyntagm := ReloadedSource.Syntagms[ReloadedSourceIndex];
    ReloadedTargetSyntagm := ReloadedTarget.Syntagms[ReloadedTargetIndex];
    ReloadedSource.Pairs := UpdatedPairs;
    AssertPairsAndSiblings(ReloadedSourceSyntagm,
      [ReloadedTargetSyntagm], []);
    AssertPairsAndSiblings(ReloadedTargetSyntagm,
      [ReloadedSourceSyntagm], []);
    AssertEquals(1, ReloadedTargetSyntagm.Strong.Count);
    AssertEquals('G123', ReloadedTargetSyntagm.Strong[0]);
    AssertEquals(1, ReloadedTargetSyntagm.Morph.Count);
    AssertEquals('N-NSM', ReloadedTargetSyntagm.Morph[0]);
  finally
    ReloadedSource.Free;
    ReloadedTarget.Free;
  end;
end;

procedure TVerseTests.AssertTracedSemanticRoundTripPreservesAssociations(
  AKind: TTheWordSemanticKind);
const
  RAW: UnicodeString = 'prefix target<WG123><WTN-NSM>! suffix';
var
  Impact: TAssociationEditImpact;
  PunctuationOffset, TargetOffset: Integer;
  Session: TTheWordWysiwygSessionV2;
  SourcePunctuation, SourceWord, TargetPunctuation, TargetWord: TSyntagm;
  Trace: TVerseEditTrace;

  procedure ApplySessionToVerse(AEditingSession: TTheWordWysiwygSessionV2);
  begin
    Trace := AEditingSession.CloneEditTrace;
    try
      AssertTrue('the traced semantic save must apply',
        verse2.TryApplyTracedEdit(UTF8Encode(AEditingSession.RawText),
          Trace, Impact));
    finally
      Trace.Free;
    end;
    AssertEquals('semantic-only save preserves both associated members', 2,
      Impact.PreservedMembers);
    AssertEquals('semantic-only save removes no associated member', 0,
      Impact.RemovedMembers);
  end;

  procedure AssertLiveAssociationAndLexicalState;
  begin
    AssertTrue('associated word object must remain in the live Verse',
      verse2.Syntagms.IndexOf(TargetWord) >= 0);
    AssertTrue('associated punctuation object must remain in the live Verse',
      verse2.Syntagms.IndexOf(TargetPunctuation) >= 0);
    AssertPairsAndSiblings(SourceWord, [TargetWord], []);
    AssertPairsAndSiblings(TargetWord, [SourceWord], []);
    AssertPairsAndSiblings(SourcePunctuation, [TargetPunctuation], []);
    AssertPairsAndSiblings(TargetPunctuation, [SourcePunctuation], []);
    AssertEquals('Strong must survive the traced semantic save', 1,
      TargetWord.Strong.Count);
    AssertEquals('G123', TargetWord.Strong[0]);
    AssertEquals('morphology must survive the traced semantic save', 1,
      TargetWord.Morph.Count);
    AssertEquals('N-NSM', TargetWord.Morph[0]);
  end;

begin
  verse1.Text := 'sourceWord sourcePunctuation';
  verse2.Text := UTF8Encode(RAW);
  TargetOffset := Length(UnicodeString('prefix '));
  PunctuationOffset := TargetOffset + Length(UnicodeString('target'));
  TargetWord := verse2.Syntagms[2];
  TargetPunctuation := verse2.Syntagms[3];
  Associate([0], [2]);
  Associate([2], [3]);
  SourceWord := verse1.Syntagms[0];
  SourcePunctuation := verse1.Syntagms[2];

  Session := TTheWordWysiwygSessionV2.Create(RAW,
    verse2.TokenizerVersion);
  try
    AssertTrue(Session.ApplySemantic(AKind, TargetOffset,
      Length(UnicodeString('target'))));
    AssertTrue(Session.ApplySemantic(AKind, PunctuationOffset, 1));
    ApplySessionToVerse(Session);
  finally
    Session.Free;
  end;
  AssertLiveAssociationAndLexicalState;

  { Toggle the exact same whole-Syntagm ranges again. The adjacent marks may
    have merged, so this also covers the close/reopen split used by removal. }
  Session := TTheWordWysiwygSessionV2.Create(UTF8Decode(verse2.XML),
    verse2.TokenizerVersion);
  try
    AssertTrue(Session.ApplySemantic(AKind, TargetOffset,
      Length(UnicodeString('target'))));
    AssertTrue(Session.ApplySemantic(AKind, PunctuationOffset, 1));
    ApplySessionToVerse(Session);
  finally
    Session.Free;
  end;
  AssertEquals('removing the semantic mark restores the original ONT',
    UTF8Encode(RAW), verse2.XML);
  AssertLiveAssociationAndLexicalState;
end;

procedure TVerseTests.SemanticFIFormattingPreservesAssociation;
begin
  AssertSemanticFormattingRoundTripPreservesAssociation('<FI>', '<Fi>');
end;

procedure TVerseTests.SemanticFRFormattingPreservesAssociation;
begin
  AssertSemanticFormattingRoundTripPreservesAssociation('<FR>', '<Fr>');
end;

procedure TVerseTests.SemanticFOFormattingPreservesAssociation;
begin
  AssertSemanticFormattingRoundTripPreservesAssociation('<FO>', '<Fo>');
end;

procedure TVerseTests.SemanticFUFormattingPreservesAssociation;
begin
  AssertSemanticFormattingRoundTripPreservesAssociation('<FU>', '<Fu>');
end;

procedure TVerseTests.SemanticFIToggleSplitPreservesAssociation;
begin
  AssertSemanticToggleSplitPreservesAssociation(twsmItalic);
end;

procedure TVerseTests.SemanticFRToggleSplitPreservesAssociation;
begin
  AssertSemanticToggleSplitPreservesAssociation(twsmWordsOfJesus);
end;

procedure TVerseTests.SemanticFOToggleSplitPreservesAssociation;
begin
  AssertSemanticToggleSplitPreservesAssociation(twsmOldTestamentQuote);
end;

procedure TVerseTests.SemanticFUToggleSplitPreservesAssociation;
begin
  AssertSemanticToggleSplitPreservesAssociation(twsmUnderline);
end;

procedure TVerseTests.SemanticFIPunctuationTogglePreservesAssociation;
begin
  AssertSemanticPunctuationTogglePreservesAssociation(twsmItalic);
end;

procedure TVerseTests.SemanticFRPunctuationTogglePreservesAssociation;
begin
  AssertSemanticPunctuationTogglePreservesAssociation(twsmWordsOfJesus);
end;

procedure TVerseTests.SemanticFOPunctuationTogglePreservesAssociation;
begin
  AssertSemanticPunctuationTogglePreservesAssociation(twsmOldTestamentQuote);
end;

procedure TVerseTests.SemanticFUPunctuationTogglePreservesAssociation;
begin
  AssertSemanticPunctuationTogglePreservesAssociation(twsmUnderline);
end;

procedure TVerseTests.SemanticFIApplyPreservesAssociation;
begin
  AssertSemanticApplyPreservesAssociation(twsmItalic);
end;

procedure TVerseTests.SemanticFRApplyPreservesAssociation;
begin
  AssertSemanticApplyPreservesAssociation(twsmWordsOfJesus);
end;

procedure TVerseTests.SemanticFOApplyPreservesAssociation;
begin
  AssertSemanticApplyPreservesAssociation(twsmOldTestamentQuote);
end;

procedure TVerseTests.SemanticFUApplyPreservesAssociation;
begin
  AssertSemanticApplyPreservesAssociation(twsmUnderline);
end;

procedure TVerseTests.TracedSemanticRoundTripPreservesAssociations;
var
  Kind: TTheWordSemanticKind;
begin
  for Kind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
    AssertTracedSemanticRoundTripPreservesAssociations(Kind);
end;

procedure TVerseTests.SemanticTogglePreflightProtectsAssociationIdentity;
const
  ORIGINAL: string = '<FR>prefix target suffix<Fr>';
  VALID: string = '<FR>prefix <Fr>target<FR> suffix<Fr>';
  INVALID: string = '<FR>prefix ta<Fr>rg<FR>et suffix<Fr>';
var
  SourceSyntagm, TargetSyntagm: TSyntagm;
  TargetIndex: Integer;
begin
  verse1.Text := 'source';
  verse2.Text := ORIGINAL;
  TargetIndex := FindIndexOf(verse2, 'target');
  Associate([0], [TargetIndex]);
  SourceSyntagm := verse1.Syntagms[0];
  TargetSyntagm := verse2.Syntagms[TargetIndex];

  AssertTrue('tags placed at Syntagm boundaries preserve identity',
    verse2.CanChangeTextPreservingAssociationIdentity(VALID));
  AssertEquals('preflight is read-only', ORIGINAL, verse2.XML);
  verse2.ChangeText(VALID);
  AssertEquals(VALID, verse2.XML);
  AssertTrue(verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);

  AssertFalse('semantic tags cannot split an associated Syntagm',
    verse2.CanChangeTextPreservingAssociationIdentity(INVALID));
  AssertEquals('rejected preflight leaves XML intact', VALID, verse2.XML);
  AssertTrue('rejected preflight leaves the target object intact',
    verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);
end;

procedure TVerseTests.SemanticTogglePreflightRejectsSplitWhitespace;
const
  ORIGINAL: string = '<FR>prefix  target suffix<Fr>';
  SPLIT_WHITESPACE: string =
    '<FR>prefix <Fr><FR> target suffix<Fr>';
var
  SourceSyntagm, TargetSyntagm: TSyntagm;
  TargetIndex: Integer;
begin
  verse1.Text := 'source';
  verse2.Text := ORIGINAL;
  TargetIndex := FindIndexOf(verse2, 'target');
  Associate([0], [TargetIndex]);
  SourceSyntagm := verse1.Syntagms[0];
  TargetSyntagm := verse2.Syntagms[TargetIndex];

  AssertFalse('a semantic boundary cannot split one whitespace unit',
    verse2.CanChangeTextPreservingAssociationIdentity(SPLIT_WHITESPACE));
  AssertEquals('rejected whitespace preflight leaves XML intact', ORIGINAL,
    verse2.XML);
  AssertTrue('rejected whitespace preflight leaves target identity intact',
    verse2.Syntagms.IndexOf(TargetSyntagm) >= 0);
  AssertPairsAndSiblings(SourceSyntagm, [TargetSyntagm], []);
  AssertPairsAndSiblings(TargetSyntagm, [SourceSyntagm], []);
end;

procedure TVerseTests.TracedEditPreservesUntouchedRepeatedWord;
var
  FirstSource, FirstTarget, SecondSource: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source1 source2';
  verse2.Text := 'same same';
  Associate([0], [0]);
  Associate([2], [2]);
  FirstSource := verse1.Syntagms[0];
  SecondSource := verse1.Syntagms[2];
  FirstTarget := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity('same same',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(5, 4, 'changed');
    AssertTrue(verse2.TryApplyTracedEdit('same changed', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertTrue('the untouched repeated occurrence keeps object identity',
    verse2.Syntagms[0] = FirstTarget);
  AssertPairsAndSiblings(FirstSource, [FirstTarget], []);
  AssertPairsAndSiblings(FirstTarget, [FirstSource], []);
  AssertPairsAndSiblings(SecondSource, [], []);
  AssertPairsAndSiblings(verse2.Syntagms[2], [], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(1, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedInsertionsPreserveShiftedAssociations;
var
  FirstSource, FirstTarget, SecondSource, SecondTarget: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source1 source2';
  verse2.Text := 'one two';
  Associate([0], [0]);
  Associate([2], [2]);
  FirstSource := verse1.Syntagms[0];
  SecondSource := verse1.Syntagms[2];
  FirstTarget := verse2.Syntagms[0];
  SecondTarget := verse2.Syntagms[2];

  Trace := TVerseEditTrace.CreateIdentity('one two',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 0, 'new ');
    Trace.ApplyReplacement(7, 0, ',');
    AssertEquals(UnicodeString('new one, two'), Trace.CurrentText);
    AssertTrue(verse2.TryApplyTracedEdit('new one, two', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertTrue(verse2.Syntagms[2] = FirstTarget);
  AssertTrue(verse2.Syntagms[5] = SecondTarget);
  AssertPairsAndSiblings(FirstSource, [FirstTarget], []);
  AssertPairsAndSiblings(SecondSource, [SecondTarget], []);
  AssertEquals(2, Impact.PreservedMembers);
  AssertEquals(0, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedSplitAndMergeRemoveOnlyAffectedMembers;
var
  KeepSource, KeepTarget, LeftSource, RightSource: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'target keep';
  verse2.Text := 'target keep';
  Associate([0], [0]);
  Associate([2], [2]);
  KeepSource := verse1.Syntagms[2];
  KeepTarget := verse2.Syntagms[2];

  Trace := TVerseEditTrace.CreateIdentity('target keep',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(3, 0, ' ');
    AssertTrue(verse2.TryApplyTracedEdit('tar get keep', Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertTrue(verse2.Syntagms[4] = KeepTarget);
  AssertPairsAndSiblings(KeepSource, [KeepTarget], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(1, Impact.RemovedMembers);

  verse1.Text := 'left right keep';
  verse2.Text := 'left right keep';
  Associate([0], [0]);
  Associate([2], [2]);
  Associate([4], [4]);
  LeftSource := verse1.Syntagms[0];
  RightSource := verse1.Syntagms[2];
  KeepSource := verse1.Syntagms[4];
  KeepTarget := verse2.Syntagms[4];

  Trace := TVerseEditTrace.CreateIdentity('left right keep',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(4, 1, '');
    AssertTrue(verse2.TryApplyTracedEdit('leftright keep', Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertPairsAndSiblings(LeftSource, [], []);
  AssertPairsAndSiblings(RightSource, [], []);
  AssertTrue(verse2.Syntagms[2] = KeepTarget);
  AssertPairsAndSiblings(KeepSource, [KeepTarget], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(2, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedEditShrinksManyToManyAssociation;
var
  SourceA, SourceB, TargetY: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'a b';
  verse2.Text := 'x y';
  Associate([0, 2], [0, 2]);
  SourceA := verse1.Syntagms[0];
  SourceB := verse1.Syntagms[2];
  TargetY := verse2.Syntagms[2];

  Trace := TVerseEditTrace.CreateIdentity('x y',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 1, 'z');
    AssertTrue(verse2.TryApplyTracedEdit('z y', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertPairsAndSiblings(SourceA, [TargetY], [SourceB]);
  AssertPairsAndSiblings(SourceB, [TargetY], [SourceA]);
  AssertPairsAndSiblings(TargetY, [SourceA, SourceB], []);
  AssertPairsAndSiblings(verse2.Syntagms[0], [], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(1, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedEditPreservesPunctuationAfterProtectedTitle;
var
  Punctuation, Source: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source';
  verse2.Text := '<TS>Heading<Ts>word!';
  Source := verse1.Syntagms[0];
  Punctuation := verse2.Syntagms[2];
  Associate([0], [2]);

  Trace := TVerseEditTrace.CreateIdentity('Headingword!',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(7, 0, 'new ');
    AssertTrue(verse2.TryApplyTracedEdit(
      '<TS>Heading<Ts>new word!', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertTrue(verse2.Syntagms[4] = Punctuation);
  AssertPairsAndSiblings(Source, [Punctuation], []);
  AssertPairsAndSiblings(Punctuation, [Source], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(0, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedEditProjectsStructuralBoundaries;
var
  CompactA, CompactB, SpacedA, SpacedB: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source1 source2';
  verse2.Text := 'a|b';
  Associate([0], [0]);
  Associate([2], [1]);
  CompactA := verse2.Syntagms[0];
  CompactB := verse2.Syntagms[1];

  Trace := TVerseEditTrace.CreateIdentity('ab',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 0, 'x ');
    AssertTrue(verse2.TryApplyTracedEdit('x a|b', Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertTrue(verse2.Syntagms[2] = CompactA);
  AssertTrue(verse2.Syntagms[3] = CompactB);
  AssertEquals(2, Impact.PreservedMembers);

  verse1.Text := 'source1 source2';
  verse2.Text := 'a | b';
  Associate([0], [0]);
  Associate([2], [2]);
  SpacedA := verse2.Syntagms[0];
  SpacedB := verse2.Syntagms[2];

  Trace := TVerseEditTrace.CreateIdentity('a  b',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 0, 'x ');
    AssertTrue(verse2.TryApplyTracedEdit('x a | b', Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertTrue(verse2.Syntagms[2] = SpacedA);
  AssertTrue(verse2.Syntagms[4] = SpacedB);
  AssertEquals(2, Impact.PreservedMembers);
end;

procedure TVerseTests.TracedEditPairsRoundTripWithShiftedIndexes;
var
  Impact: TAssociationEditImpact;
  Pairs: string;
  ReloadedSource, ReloadedTarget: TVerse;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source1 source2';
  verse2.Text := 'one two';
  Associate([0], [0]);
  Associate([2], [2]);

  Trace := TVerseEditTrace.CreateIdentity('one two',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 0, 'new ');
    Trace.ApplyReplacement(7, 0, ',');
    AssertTrue(verse2.TryApplyTracedEdit('new one, two', Trace, Impact));
  finally
    Trace.Free;
  end;
  Pairs := verse1.Pairs;

  ReloadedSource := TVerse.Create;
  ReloadedTarget := TVerse.Create;
  try
    ReloadedSource.PairedVerse := ReloadedTarget;
    ReloadedSource.Text := verse1.XML;
    ReloadedTarget.Text := verse2.XML;
    ReloadedSource.Pairs := Pairs;
    AssertPairsAndSiblings(ReloadedSource.Syntagms[0],
      [ReloadedTarget.Syntagms[2]], []);
    AssertPairsAndSiblings(ReloadedSource.Syntagms[2],
      [ReloadedTarget.Syntagms[5]], []);
  finally
    ReloadedSource.Free;
    ReloadedTarget.Free;
  end;
end;

procedure TVerseTests.TracedRawEditDoesNotStealExactRepeatedWord;
var
  FirstSource, LastSource, LastTarget: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'first last';
  verse2.Text := 'repeat old repeat';
  Associate([0], [0]);
  Associate([2], [4]);
  FirstSource := verse1.Syntagms[0];
  LastSource := verse1.Syntagms[2];
  LastTarget := verse2.Syntagms[4];

  Trace := TVerseEditTrace.CreateIdentity('repeat old repeat',
    verse2.TokenizerVersion);
  try
    Trace.ApplyRawVisibleChange('gone repeat');
    AssertEquals(Ord(vetmRawConservative), Ord(Trace.Mode));
    AssertTrue(verse2.TryApplyTracedEdit('gone repeat', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertPairsAndSiblings(FirstSource, [], []);
  AssertTrue(verse2.Syntagms[2] = LastTarget);
  AssertPairsAndSiblings(LastSource, [LastTarget], []);
  AssertPairsAndSiblings(LastTarget, [LastSource], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(1, Impact.RemovedMembers);
  AssertFalse(Impact.UsedLegacyFallback);
end;

procedure TVerseTests.TracedEditProjectionFailureIsAtomic;
var
  OriginalSource, OriginalTarget: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source';
  verse2.Text := 'target';
  Associate([0], [0]);
  OriginalSource := verse1.Syntagms[0];
  OriginalTarget := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity('target',
    verse2.TokenizerVersion);
  try
    AssertFalse(verse2.TryApplyTracedEdit('changed', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertEquals('target', verse2.XML);
  AssertTrue(Impact.ErrorMessage <> '');
  AssertTrue(verse2.Syntagms[0] = OriginalTarget);
  AssertPairsAndSiblings(OriginalSource, [OriginalTarget], []);
  AssertPairsAndSiblings(OriginalTarget, [OriginalSource], []);
end;

procedure TVerseTests.TracedUnicodeReplacementPreservesUntouchedLexicalWord;
var
  Emoji, Hebrew: UnicodeString;
  HebrewSource, HebrewTarget, KeepSource, KeepTarget: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  Emoji := UnicodeString(#$D83D) + UnicodeString(#$DE00);
  verse1.Text := 'emoji keep';
  verse2.Text := UTF8Encode(Emoji + ' keep<WG1><WTN-NSM>');
  Associate([0], [0]);
  Associate([2], [2]);
  KeepSource := verse1.Syntagms[2];
  KeepTarget := verse2.Syntagms[2];

  Trace := TVerseEditTrace.CreateIdentity(Emoji + ' keep',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 2, 'X');
    AssertTrue(verse2.TryApplyTracedEdit(
      'X keep<WG1><WTN-NSM>', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertTrue(verse2.Syntagms[2] = KeepTarget);
  AssertPairsAndSiblings(KeepSource, [KeepTarget], []);
  AssertEquals(1, KeepTarget.Strong.Count);
  AssertEquals('G1', KeepTarget.Strong[0]);
  AssertEquals(1, KeepTarget.Morph.Count);
  AssertEquals('N-NSM', KeepTarget.Morph[0]);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(1, Impact.RemovedMembers);

  Hebrew := UTF8Decode('שָׁלוֹם');
  verse1.Text := 'source';
  verse2.Text := UTF8Encode(Hebrew);
  Associate([0], [0]);
  HebrewSource := verse1.Syntagms[0];
  HebrewTarget := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity(Hebrew,
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 0, 'novo ');
    AssertTrue(verse2.TryApplyTracedEdit(
      UTF8Encode(Trace.CurrentText), Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertTrue(verse2.Syntagms[2] = HebrewTarget);
  AssertPairsAndSiblings(HebrewSource, [HebrewTarget], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(0, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedEditRejectsTouchedStrongWordAtomically;
var
  SourceWord, TargetWord: TSyntagm;
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source';
  verse2.Text := 'word<WG1>';
  Associate([0], [0]);
  SourceWord := verse1.Syntagms[0];
  TargetWord := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity('word',
    verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 4, 'term');
    AssertFalse(verse2.TryApplyTracedEdit('term<WG1>', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertEquals('word<WG1>', verse2.XML);
  AssertTrue(Pos('Strong', Impact.ErrorMessage) > 0);
  AssertTrue(verse2.Syntagms[0] = TargetWord);
  AssertPairsAndSiblings(SourceWord, [TargetWord], []);
  AssertPairsAndSiblings(TargetWord, [SourceWord], []);
end;

procedure TVerseTests.TracedEditCanPopulateEmptyVerse;
var
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  Trace := TVerseEditTrace.CreateIdentity('', verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 0, 'first text');
    AssertTrue(verse2.TryApplyTracedEdit('first text', Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertEquals('first text', verse2.XML);
  AssertEquals(3, verse2.Syntagms.Count);
  AssertEquals(0, Impact.PreservedMembers);
  AssertEquals(0, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedCutPasteDoesNotTransferAssociation;
var
  CopySource, CopyTarget, StaySource, StayTarget: TSyntagm;
  Impact: TAssociationEditImpact;
  InsertedLength: Integer;
  Payload: string;
  PlainText: UnicodeString;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
  UsedPrivatePayload: Boolean;
begin
  verse1.Text := 'copySource staySource';
  verse2.Text := 'copy stay';
  Associate([0], [0]);
  Associate([2], [2]);
  CopySource := verse1.Syntagms[0];
  StaySource := verse1.Syntagms[2];
  CopyTarget := verse2.Syntagms[0];
  StayTarget := verse2.Syntagms[2];

  Session := TTheWordWysiwygSessionV2.Create('copy stay',
    verse2.TokenizerVersion);
  Trace := nil;
  try
    AssertTrue(Session.CutSelection(0, 4, PlainText, Payload));
    AssertEquals('copy', UTF8Encode(PlainText));
    AssertTrue(Session.Paste(0, 0, Payload, '', InsertedLength,
      UsedPrivatePayload));
    AssertTrue(UsedPrivatePayload);
    AssertEquals(4, InsertedLength);
    AssertEquals('the text can be restored while its origin remains new',
      'copy stay', UTF8Encode(Session.RawText));
    AssertTrue('cut and paste of the same text must remain a traced edit',
      Session.Dirty);

    Trace := Session.CloneEditTrace;
    AssertTrue(verse2.TryApplyTracedEdit(UTF8Encode(Session.RawText),
      Trace, Impact));
  finally
    Trace.Free;
    Session.Free;
  end;

  AssertTrue('the pasted occurrence must be a new Syntagm object',
    verse2.Syntagms[0] <> CopyTarget);
  AssertPairsAndSiblings(CopySource, [], []);
  AssertPairsAndSiblings(verse2.Syntagms[0], [], []);
  AssertTrue('the untouched word must retain object identity',
    verse2.Syntagms[2] = StayTarget);
  AssertPairsAndSiblings(StaySource, [StayTarget], []);
  AssertPairsAndSiblings(StayTarget, [StaySource], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(1, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedSemanticSplitRemovesOnlyAffectedAssociation;
var
  Impact: TAssociationEditImpact;
  KeepSource, KeepTarget, WordSource, WordTarget: TSyntagm;
  Session: TTheWordWysiwygSessionV2;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'wordSource keepSource';
  verse2.Text := 'word keep';
  Associate([0], [0]);
  Associate([2], [2]);
  WordSource := verse1.Syntagms[0];
  KeepSource := verse1.Syntagms[2];
  WordTarget := verse2.Syntagms[0];
  KeepTarget := verse2.Syntagms[2];

  Session := TTheWordWysiwygSessionV2.Create('word keep',
    verse2.TokenizerVersion);
  Trace := nil;
  try
    AssertTrue('formatting an interior slice must be accepted by the editor',
      Session.ApplySemantic(twsmWordsOfJesus, 1, 2));
    Trace := Session.CloneEditTrace;
    AssertTrue('the reconciler must apply the semantic split',
      verse2.TryApplyTracedEdit(UTF8Encode(Session.RawText), Trace, Impact));
  finally
    Trace.Free;
    Session.Free;
  end;

  AssertTrue('the structurally split word must not retain its old object',
    verse2.Syntagms.IndexOf(WordTarget) < 0);
  AssertPairsAndSiblings(WordSource, [], []);
  AssertTrue('the untouched word must retain its object and Association',
    verse2.Syntagms.IndexOf(KeepTarget) >= 0);
  AssertPairsAndSiblings(KeepSource, [KeepTarget], []);
  AssertPairsAndSiblings(KeepTarget, [KeepSource], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(1, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedLegacyFallbackValidatesTraceAndProtectedWords;
var
  Impact: TAssociationEditImpact;
  SourceWord, TargetWord: TSyntagm;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source';
  verse2.Text := 'target';
  Associate([0], [0]);
  SourceWord := verse1.Syntagms[0];
  TargetWord := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity('stale', verse2.TokenizerVersion);
  try
    Trace.MarkLegacyFallback;
    AssertFalse('fallback must not bypass a stale trace baseline',
      verse2.TryApplyTracedEdit('changed', Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertEquals('target', verse2.XML);
  AssertTrue(Impact.ErrorMessage <> '');
  AssertPairsAndSiblings(SourceWord, [TargetWord], []);

  verse1.DisassociatePairs;
  verse1.Text := 'source';
  verse2.Text := 'word<WG1>';
  Associate([0], [0]);
  SourceWord := verse1.Syntagms[0];
  TargetWord := verse2.Syntagms[0];
  Trace := TVerseEditTrace.CreateIdentity('word', verse2.TokenizerVersion);
  try
    Trace.ApplyRawVisibleChange('term');
    Trace.MarkLegacyFallback;
    AssertFalse('fallback must not guess across a protected Strong word',
      verse2.TryApplyTracedEdit('term<WG1>', Trace, Impact));
  finally
    Trace.Free;
  end;
  AssertEquals('word<WG1>', verse2.XML);
  AssertTrue(Pos('Strong', Impact.ErrorMessage) > 0);
  AssertPairsAndSiblings(SourceWord, [TargetWord], []);
end;

procedure TVerseTests.TracedLegacyFallbackUsesHistoricalHeuristic;
var
  Impact: TAssociationEditImpact;
  SourceWord, TargetWord: TSyntagm;
  Trace: TVerseEditTrace;
begin
  verse1.Text := 'source';
  verse2.Text := 'old';
  Associate([0], [0]);
  SourceWord := verse1.Syntagms[0];
  TargetWord := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity('old', verse2.TokenizerVersion);
  try
    Trace.ApplyRawVisibleChange('new old');
    Trace.MarkLegacyFallback;
    AssertTrue(verse2.TryApplyTracedEdit('new old', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertTrue(Impact.UsedLegacyFallback);
  AssertTrue('the historical fallback may retain an unchanged occurrence',
    verse2.Syntagms[2] = TargetWord);
  AssertPairsAndSiblings(SourceWord, [TargetWord], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(0, Impact.RemovedMembers);
end;

procedure TVerseTests.TracedEditRenderFailureRemainsCommitted;
var
  AttemptsBeforeSave: Integer;
  FailingVerse: TRenderFailingVerse;
  Impact: TAssociationEditImpact;
  SourceWord, TargetWord: TSyntagm;
  Trace: TVerseEditTrace;
begin
  verse1.PairedVerse := nil;
  FreeAndNil(verse2);
  FailingVerse := TRenderFailingVerse.Create;
  verse2 := FailingVerse;
  verse1.PairedVerse := verse2;

  verse1.Text := 'source';
  verse2.Text := 'target';
  Associate([0], [0]);
  SourceWord := verse1.Syntagms[0];
  TargetWord := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity('target', verse2.TokenizerVersion);
  try
    Trace.ApplyReplacement(0, 0, 'new ');
    AttemptsBeforeSave := FailingVerse.RenderAttempts;
    FailingVerse.FailRender := True;
    AssertTrue('a presentation failure after commit must not reject the save',
      verse2.TryApplyTracedEdit('new target', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertEquals('new target', verse2.XML);
  AssertEquals('', Impact.ErrorMessage);
  AssertTrue(verse2.Syntagms[2] = TargetWord);
  AssertPairsAndSiblings(SourceWord, [TargetWord], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(0, Impact.RemovedMembers);
  AssertEquals(AttemptsBeforeSave + 1, FailingVerse.RenderAttempts);

  FailingVerse.FailRender := False;
  Application.ProcessMessages;
  AssertTrue('the committed Verse must receive a deferred render retry',
    FailingVerse.RenderAttempts >= AttemptsBeforeSave + 2);
end;

procedure TVerseTests.TracedFallbackRenderFailureRemainsCommitted;
var
  AttemptsBeforeSave: Integer;
  FailingVerse: TRenderFailingVerse;
  Impact: TAssociationEditImpact;
  SourceWord, TargetWord: TSyntagm;
  Trace: TVerseEditTrace;
begin
  verse1.PairedVerse := nil;
  FreeAndNil(verse2);
  FailingVerse := TRenderFailingVerse.Create;
  verse2 := FailingVerse;
  verse1.PairedVerse := verse2;

  verse1.Text := 'source';
  verse2.Text := 'old';
  Associate([0], [0]);
  SourceWord := verse1.Syntagms[0];
  TargetWord := verse2.Syntagms[0];

  Trace := TVerseEditTrace.CreateIdentity('old', verse2.TokenizerVersion);
  try
    Trace.ApplyRawVisibleChange('new old');
    Trace.MarkLegacyFallback;
    AttemptsBeforeSave := FailingVerse.RenderAttempts;
    FailingVerse.FailRender := True;
    AssertTrue('a fallback render failure must not reject a committed save',
      verse2.TryApplyTracedEdit('new old', Trace, Impact));
  finally
    Trace.Free;
  end;

  AssertEquals('new old', verse2.XML);
  AssertEquals('', Impact.ErrorMessage);
  AssertTrue(Impact.UsedLegacyFallback);
  AssertTrue(verse2.Syntagms[2] = TargetWord);
  AssertPairsAndSiblings(SourceWord, [TargetWord], []);
  AssertEquals(1, Impact.PreservedMembers);
  AssertEquals(0, Impact.RemovedMembers);
  AssertEquals(AttemptsBeforeSave + 1, FailingVerse.RenderAttempts);

  FailingVerse.FailRender := False;
  Application.ProcessMessages;
  AssertTrue('the fallback commit must receive a deferred render retry',
    FailingVerse.RenderAttempts >= AttemptsBeforeSave + 2);
end;

procedure TVerseTests.ReplaceSourceTextChangeBeginning;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Text := 'original1 original2';
  verse2.Text := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Syntagms[0];
  original2    := verse1.Syntagms[2];
  translation1 := verse2.Syntagms[0];
  translation2 := verse2.Syntagms[2];

  //                   0         2         4
  verse1.ChangeText('original3 original1 original2');

  neworiginal3 := verse1.Syntagms[0];
  neworiginal1 := verse1.Syntagms[2];
  neworiginal2 := verse1.Syntagms[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceDestinationTextChangeBeginning;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Text := 'translation1 translation2';
  verse2.Text := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse2.Syntagms[0];
  original2    := verse2.Syntagms[2];
  translation1 := verse1.Syntagms[0];
  translation2 := verse1.Syntagms[2];

  //                   0         2         4
  verse2.ChangeText('original3 original1 original2');

  neworiginal3 := verse2.Syntagms[0];
  neworiginal1 := verse2.Syntagms[2];
  neworiginal2 := verse2.Syntagms[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceSourceTextChangeEnd;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Text := 'original1 original2';
  verse2.Text := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Syntagms[0];
  original2    := verse1.Syntagms[2];
  translation1 := verse2.Syntagms[0];
  translation2 := verse2.Syntagms[2];

  //                   0         2         4
  verse1.ChangeText('original1 original2 original3');

  neworiginal1 := verse1.Syntagms[0];
  neworiginal2 := verse1.Syntagms[2];
  neworiginal3 := verse1.Syntagms[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceDestinationTextChangeEnd;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Text := 'translation1 translation2';
  verse2.Text := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  translation1 := verse1.Syntagms[0];
  translation2 := verse1.Syntagms[2];
  original1    := verse2.Syntagms[0];
  original2    := verse2.Syntagms[2];

  //                   0         2         4
  verse2.ChangeText('original1 original2 original3');

  neworiginal1 := verse2.Syntagms[0];
  neworiginal2 := verse2.Syntagms[2];
  neworiginal3 := verse2.Syntagms[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceSourceTextChangeMiddle;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Text := 'original1 original2';
  verse2.Text := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Syntagms[0];
  original2    := verse1.Syntagms[2];
  translation1 := verse2.Syntagms[0];
  translation2 := verse2.Syntagms[2];

  //                   0         2         4
  verse1.ChangeText('original1 original3 original2');

  neworiginal1 := verse1.Syntagms[0];
  neworiginal3 := verse1.Syntagms[2];
  neworiginal2 := verse1.Syntagms[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceDestinationTextChangeMiddle;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Text := 'translation1 translation2';
  verse2.Text := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  translation1 := verse1.Syntagms[0];
  translation2 := verse1.Syntagms[2];
  original1    := verse2.Syntagms[0];
  original2    := verse2.Syntagms[2];

  //                   0         2         4
  verse2.ChangeText('original1 original3 original2');

  neworiginal1 := verse2.Syntagms[0];
  neworiginal3 := verse2.Syntagms[2];
  neworiginal2 := verse2.Syntagms[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceTextTwice;
var
  s, d: TSyntagmList;
begin           // 0 2 4 6
  verse1.Text := 'a b c d';
  verse2.Text := 'a b c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([4],[4]);
  Associate([6],[6]);
                    // 0 2 4 6 8
  verse2.ChangeText('a b e c d');

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[4], [d[6]], []);
  AssertPairsAndSiblings(s[6], [d[8]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[4], [],     []);
  AssertPairsAndSiblings(d[6], [s[4]], []);
  AssertPairsAndSiblings(d[8], [s[6]], []);
                    // 0 2 4 6 8
  verse2.ChangeText('a b c e d');

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[6], [d[8]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[6], [    ], []);
  AssertPairsAndSiblings(d[8], [s[6]], []);
end;

procedure TVerseTests.TokenizerVersion1KeepsCurlyQuotesInsideWords;
begin
  AssertTokenizerTokens(1, '‘You shall not eat of every tree of the garden’?”',
    ['‘You', 'shall', 'not', 'eat', 'of', 'every', 'tree', 'of', 'the', 'garden’', '?”']);
end;

procedure TVerseTests.TokenizerVersion2KeepsCurlyQuotesInsideWords;
begin
  AssertTokenizerTokens(2, '‘You shall not eat of every tree of the garden’?”',
    ['‘You', 'shall', 'not', 'eat', 'of', 'every', 'tree', 'of', 'the', 'garden’', '?', '”']);
end;

procedure TVerseTests.TokenizerVersion1KeepsHebrewMaqafInsideWord;
begin
  AssertTokenizerTokens(1, 'דבר־יהוה', ['דבר־יהוה']);
end;

procedure TVerseTests.TokenizerVersions2And3TreatHebrewMaqafAsPunctuation;
begin
  AssertTokenizerTokens(2, 'דבר־יהוה', ['דבר', '־', 'יהוה']);
  AssertTokenizerTokens(3, 'דבר־יהוה', ['דבר', '־', 'יהוה']);
end;

procedure TVerseTests.TokenizerVersionsKeepAsciiApostropheInsideWords;
begin
  AssertTokenizerTokens(1, 'don''t stop', ['don''t', 'stop']);
  AssertTokenizerTokens(2, 'don''t stop', ['don''t', 'stop']);
  AssertTokenizerTokens(3, 'don''t stop', ['don''t', 'stop']);
end;

procedure TVerseTests.TokenizerVersion3TreatsCurlyQuotesAsPunctuation;
begin
  AssertTokenizerTokens(3, '‘You shall not eat of every tree of the garden’?”',
    ['‘', 'You', 'shall', 'not', 'eat', 'of', 'every', 'tree', 'of', 'the', 'garden', '’', '?', '”']);
end;

procedure TVerseTests.VerseTokenizerVersionOverridesGlobalPreferredVersion;
var
  previousPreferredVersion: Integer;
begin
  previousPreferredVersion := TTokenizerFactory.PreferredVersion;
  try
    TTokenizerFactory.PreferredVersion := 1;
    verse1.TokenizerVersion := 3;

    verse1.Text := '‘quoted’';

    AssertEquals(3, verse1.Syntagms.Count);
    AssertEquals('‘', verse1.Syntagms[0].Text);
    AssertEquals('quoted', verse1.Syntagms[1].Text);
    AssertEquals('’', verse1.Syntagms[2].Text);
  finally
    TTokenizerFactory.PreferredVersion := previousPreferredVersion;
  end;
end;

procedure TVerseTests.ReplaceSourceTextVerticalBar;
var
  s, d: TSyntagmList;
begin           // 0 2 3 5
  verse1.Text := 'a b|c d';
                // 0 2 4 6
  verse2.Text := 'a b c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([3],[4]);
  Associate([5],[6]);

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [d[4]], []);
  AssertPairsAndSiblings(s[4], [],     []);
  AssertPairsAndSiblings(s[5], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[3]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[5]], []);
                    // 0 2 4 6
  verse1.ChangeText('a b c d');

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[6]], []);
end;

procedure TVerseTests.ReplaceDestinationTextVerticalBar;
var
  s, d: TSyntagmList;
begin
                // 0 2 4 6
  verse1.Text := 'a b c d';
                // 0 2 3 5
  verse2.Text := 'a b|c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([4],[3]);
  Associate([6],[5]);

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[3]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[5]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [s[4]], []);
  AssertPairsAndSiblings(d[4], [],     []);
  AssertPairsAndSiblings(d[5], [s[6]], []);
                    // 0 2 4 6
  verse2.ChangeText('a b c d');

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[6]], []);
end;

procedure TVerseTests.ReplaceDestinationTextManyWords;
begin
  // disabled
  Exit;

  verse1.Text := '(καὶ ἡ ζωὴ ἐφανερώθη· καὶ ἑωράκαμεν, καὶ μαρτυροῦμεν, καὶ ἀπαγγέλλομεν ὑμῖν τὴν ζωὴν τὴν αἰώνιον, ἥτις ἦν πρὸς τὸν πατέρα, καὶ ἐφανερώθη ἡμῖν·)';
  verse2.Text := '(E a vida foi manifestada, e nós a vimos, e testificamos dela, e vos anunciamos a vida eterna, que estava com o Pai, e nos foi manifestada);';

  AssociatePairs([
    [['καὶ'], ['E']],
    [['ἡ'], ['a']],
    [['ζωὴ'], ['vida']],
    [['ἐφανερώθη·'], ['foi', 'manifestada']],
    [['καὶ'], ['e']],
    [['ἑωράκαμεν'], ['nós', 'vimos']],
    [['καὶ'], ['e']],
    [['μαρτυροῦμεν'], ['testificamos']],
    [['καὶ'], ['e']],
    [['ἀπαγγέλλομεν'], ['anunciamos']],
    [['ὑμῖν'], ['vos']],
    [['τὴν'], ['a']],
    [['ζωὴν'], ['vida']],
    [['αἰώνιον'], ['eterna']],
    [['ἥτις'], ['que']],
    [['ἦν'], ['eestava']],
    [['πρὸς'], ['com']],
    [['τὸν'], ['o']],
    [['πατέρα'], ['Pai']],
    [['καὶ'], ['e']],
    [['ἐφανερώθη'], ['foi', 'manifestada']],
    [['ἡμῖν'], ['nos']]
  ]);

  verse2.ChangeText('(Porque a vida foi manifestada, e nós <FI>a<Fi> vimos, e testificamos, e vos anunciamos a vida eterna, que estava com o Pai, e nos foi manifestada.)');

  AssertPairs([
    //[['καὶ'], ['E']], association lost
    [['ἡ'], ['a']],
    [['ζωὴ'], ['vida']],
    [['ἐφανερώθη·'], ['foi', 'manifestada']],
    [['καὶ'], ['e']],
    [['ἑωράκαμεν'], ['nós', 'vimos']],
    [['καὶ'], ['e']],
    [['μαρτυροῦμεν'], ['testificamos']],
    [['καὶ'], ['e']],
    [['ἀπαγγέλλομεν'], ['anunciamos']],
    [['ὑμῖν'], ['vos']],
    [['τὴν'], ['a']],
    [['ζωὴν'], ['vida']],
    [['αἰώνιον'], ['eterna']],
    [['ἥτις'], ['que']],
    [['ἦν'], ['eestava']],
    [['πρὸς'], ['com']],
    [['τὸν'], ['o']],
    [['πατέρα'], ['Pai']],
    [['καὶ'], ['e']],
    [['ἐφανερώθη'], ['foi', 'manifestada']],
    [['ἡμῖν'], ['nos']]
  ]);

end;

procedure TVerseTests.ReplaceSourceTextAddPunctuation;
var
  s, d: TSyntagmList;
begin
                // 0 2 4 6
  verse1.Text := 'a b c d';
  verse2.Text := 'a b c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([4],[4]);
  Associate([6],[6]);

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[6]], []);

                    // 0 2  5 7
  verse1.ChangeText('a b, c d');

  s := verse1.Syntagms;
  d := verse2.Syntagms;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [],     []);
  AssertPairsAndSiblings(s[5], [d[4]], []);
  AssertPairsAndSiblings(s[6], [],     []);
  AssertPairsAndSiblings(s[7], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[5]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[7]], []);
end;

function TVerseTests.CreateStringList(const AWords: array of string): TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := Low(AWords) to High(AWords) do
    Result.Add(AWords[I]);
end;

function TVerseTests.CollectTokenizerTokens(version: Integer; const AText: string): TStringList;
var
  tokenizer: TONTTokenizer;
  token: TSyntagmToken;
begin
  Result := TStringList.Create;
  tokenizer := TTokenizerFactory.CreateTokenizer(version, AText);
  try
    while tokenizer.ReadSyntagm(token) <> skNull do
      if token.Kind <> skWhitespace then
        Result.Add(token.Value);
  finally
    tokenizer.Destroy;
  end;
end;

procedure TVerseTests.AssertTokenizerTokens(version: Integer; const AText: string;
  const Expected: array of string);
var
  tokens: TStringList;
  i: Integer;
begin
  tokens := CollectTokenizerTokens(version, AText);
  try
    AssertEquals(Format('Tokenizer v%d token count', [version]), Length(Expected), tokens.Count);
    for i := Low(Expected) to High(Expected) do
      AssertEquals(Format('Tokenizer v%d token %d', [version, i]), Expected[i], tokens[i]);
  finally
    tokens.Free;
  end;
end;

procedure TVerseTests.AssertSelectionEqualsIndices(const ExpectedIndices: array of Integer);
var
  I: Integer;
  ExpectedSyntagmPointers: array of Variant; // Using Variant to hold pointers for array of const
  ExpectedSyntagmsList: TSyntagmList;
begin
  // Check if verse1 and its Sintagmas are assigned
  AssertNotNull('verse1 is not initialized in AssertSelectionEqualsIndices', verse1);
  AssertNotNull('verse1.Syntagms is not initialized in AssertSelectionEqualsIndices', verse1.Syntagms);

  // Create a list of expected TSyntagm objects
  ExpectedSyntagmsList := TSyntagmList.Create;
  try
    for I := Low(ExpectedIndices) to High(ExpectedIndices) do
    begin
      if (ExpectedIndices[I] >= 0) and (ExpectedIndices[I] < verse1.Syntagms.Count) then
        ExpectedSyntagmsList.Add(verse1.Syntagms[ExpectedIndices[I]])
      else
        Fail(Format('Expected index %d is out of bounds for verse1.Syntagms (count: %d)', [ExpectedIndices[I], verse1.Syntagms.Count]));
    end;

    // Assert counts first
    AssertEquals(Format('Selection count mismatch. Expected %d, Got %d.', [ExpectedSyntagmsList.Count, verse1.Selection.Count]),
      ExpectedSyntagmsList.Count, verse1.Selection.Count);

    // Assert each item if counts match
    if ExpectedSyntagmsList.Count = verse1.Selection.Count then
    begin
      for I := 0 to ExpectedSyntagmsList.Count - 1 do
      begin
        AssertTrue(Format('Selected syntagm at selection index %d does not match. Expected syntagm text: "%s" (original index %d), Got: "%s" (original index %d)',
                 [I, ExpectedSyntagmsList[I].Text, verse1.Syntagms.IndexOf(ExpectedSyntagmsList[I]),
                  verse1.Selection[I].Text, verse1.Syntagms.IndexOf(verse1.Selection[I]) ]),
                  ExpectedSyntagmsList[I] = verse1.Selection[I]);
      end;
    end;
  finally
    ExpectedSyntagmsList.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_SimpleSequence;
var
  Words: TStringList;
begin
  verse1.Text := 'This is a test verse';
  // FSintagmas: 0:This 1:  2:is 3:  4:a 5:  6:test 7:  8:verse
  Words := CreateStringList(['test', 'verse']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, False, False);
    AssertSelectionEqualsIndices([6, 8]); // "test", "verse"
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_NotFound;
var
  Words: TStringList;
begin
  verse1.Text := 'This is a test verse';
  Words := CreateStringList(['invalid', 'words']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, False, False);
    AssertSelectionEqualsIndices([]);
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_PartialTrue_SomeFound;
var
  Words: TStringList;
begin
  verse1.Text := 'one two three';
  // FSintagmas: 0:one 1:  2:two 3:  4:three
  Words := CreateStringList(['one', 'four']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, True, False);
    // Any-order search: finds "one". "four" not found. Partial=True, so "one" is selected.
    AssertSelectionEqualsIndices([0]); // "one"
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_PartialFalse_NotAllFound;
var
  Words: TStringList;
begin
  verse1.Text := 'one two three';
  Words := CreateStringList(['one', 'four']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, False, False);
    AssertSelectionEqualsIndices([]); // Not all found, Partial=False
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_AvoidAlreadySelected_Sequential;
var
  Words: TStringList;
begin
  verse1.Text := 'test one test two';
  // FSintagmas: 0:test 1:  2:one 3:  4:test 5:  6:two
  verse1.ClearSelection;
  verse1.Syntagms[0].AddToSelection; // Pre-select first "test"

  Words := CreateStringList(['test', 'two']);
  try
    verse1.FindAndSelectSyntagms(Words, False, False);
    // Current selection must be ignored and replaced by the resolved phrase.
    AssertSelectionEqualsIndices([4, 6]);
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_JoinConnected_DoesNotCrossSpaces;
var
  Words: TStringList;
begin
  verse1.Text := 'word1 , word2';
  Words := CreateStringList(['word1', 'word2']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, False, True); // JoinConnected = True
    AssertSelectionEqualsIndices([0, 4]); // spaces break connection even when punctuation exists
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_EmptyWordsList;
begin
  verse1.Text := 'This is a test';
  verse1.ClearSelection;
  verse1.FindAndSelectSyntagms(nil, False, False); // Test with nil
  AssertSelectionEqualsIndices([]);

  verse1.ClearSelection;
  verse1.FindAndSelectSyntagms(CreateStringList([]), False, False); // Test with empty TStringList
  AssertSelectionEqualsIndices([]);
end;

procedure TVerseTests.TestFindAndSelectSyntagms_SequentialSkipsSelected;
var
  Words: TStringList;
begin
  verse1.Text := 'a b c a b d';
  // Indices:    0 1 2 3 4 5 6 7 8 9 10
  // Syntagms:   a ' ' b ' ' c ' ' a ' ' b ' '  d
  // Word Idx:   0     2     4     6     8     10
  verse1.ClearSelection;
  verse1.Syntagms[0].AddToSelection; // Select first 'a'
  verse1.Syntagms[2].AddToSelection; // Select first 'b'

  Words := CreateStringList(['a', 'b']);
  try
    verse1.FindAndSelectSyntagms(Words, False, False);
    AssertSelectionEqualsIndices([0, 2]);
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_AnyOrderSkipsSelected_FullMatch;
var
  Words: TStringList;
begin
  verse1.Text := 'x y z x y w';
  // Indices:    0 1 2 3 4 5 6 7 8 9 10
  // Syntagms:   x ' ' y ' ' z ' ' x ' ' y ' '  w
  // Word Idx:   0     2     4     6     8     10
  verse1.ClearSelection;
  verse1.Syntagms[0].AddToSelection; // Select first 'x'

  Words := CreateStringList(['x', 'y']); // Search for 'x' and 'y'
  try
    verse1.FindAndSelectSyntagms(Words, False, False);
    AssertSelectionEqualsIndices([0, 2]);
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_AnyOrder_PartialTrue_SkipsSelected;
var
  Words: TStringList;
begin
  verse1.Text := 'x y z x y w';
  // Word Idx:   0     2     4     6     8     10
  verse1.ClearSelection;
  verse1.Syntagms[0].AddToSelection; // Select first 'x' (idx 0)
  verse1.Syntagms[8].AddToSelection; // Select second 'y' (idx 8)

  Words := CreateStringList(['x', 'y', 'missing']);
  try
    verse1.FindAndSelectSyntagms(Words, True, False); // Partial = True
    AssertSelectionEqualsIndices([0, 2]);
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_SequenceWithIntermediateNonWords;
var
  Words: TStringList;
begin
  // Test if "word1 <tag> word2" can find sequence ["word1", "word2"]
  // Current implementation of sequential search in FindAndSelectSyntagms:
  //   for J := 0 to Words.Count - 1 do
  //     Syntagm := FSintagmas[I + J];
  // This implies direct adjacency in FSintagmas list for sequential matches.
  // This test will clarify that behavior. If FSintagmas[I+J] is not a word syntagm, it will fail.
  // The current code only checks Syntagm.Kind = skSyntagm.
  // If FSintagmas[I+J] is a space or tag, it won't match Words[J] unless Words[J] is that space/tag.
  // So, this test should behave as if direct adjacency of *word syntagms* is required.

  verse1.Text := 'word1<TAG_HERE>word2'; // Parsed as: word1 (0), <TAG_HERE> (1), word2 (2)
  Words := CreateStringList(['word1', 'word2']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, False, False);
    // Sequential search for "word1", "word2":
    // I=0: FSintagmas[0]="word1" (match), FSintagmas[1]="<TAG_HERE>" (no match for "word2") -> Fails
    // Any-order search: finds "word1" (idx 0), "word2" (idx 2)
    AssertSelectionEqualsIndices([0, 2]);
  finally
    Words.Free;
  end;

  // Test with space
  verse1.Text := 'word1 word2'; // Parsed as: word1 (0), ' ' (1), word2 (2)
  Words := CreateStringList(['word1', 'word2']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, False, False);
    // Sequential search:
    // I=0: FSintagmas[0]="word1" (match), FSintagmas[1]=" " (no match for "word2") -> Fails
    // Any-order search: finds "word1" (idx 0), "word2" (idx 2)
    AssertSelectionEqualsIndices([0, 2]);
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.TestFindAndSelectSyntagms_AnyOrderWithDuplicatesInSearchList;
var
  Words: TStringList;
begin
  verse1.Text := 'apple banana apple orange apple';
  // Indices:    0     1 2      3 4     5 6      7 8     9
  // Syntagms:   apple ' ' banana ' ' apple ' ' orange ' ' apple
  // Word Idx:   0         2         4          6          8
  Words := CreateStringList(['apple', 'apple']);
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, False, False);
    // Sequential search for ['apple', 'apple'] will fail because of space.
    // Any-order search:
    // For Words[0] ('apple'): finds FSintagmas[0]
    // For Words[1] ('apple'): finds FSintagmas[4] (because FSintagmas[0] is now in FoundSyntagms due to IndexOf check)
    AssertSelectionEqualsIndices([0, 4]);
  finally
    Words.Free;
  end;

  // Test with Partial = True and one more duplicate than available
  // Words.Clear; // This was causing AV as Words was already freed.
  Words := TStringList.Create; // Re-create Words
  Words.Add('apple'); Words.Add('apple'); Words.Add('apple'); Words.Add('apple'); // 4 apples
  try
    verse1.ClearSelection;
    verse1.FindAndSelectSyntagms(Words, True, False); // Partial = True
    // Any-order:
    // Word 1 ('apple'): finds idx 0
    // Word 2 ('apple'): finds idx 4
    // Word 3 ('apple'): finds idx 8
    // Word 4 ('apple'): no more apples
    // Partial = True, so [0,4,8] are selected
    AssertSelectionEqualsIndices([0, 4, 8]);
  finally
    Words.Free;
  end;
end;

procedure TVerseTests.Associate(const source: array of const;
  const dest: array of const);
var
  i: integer;
begin
  verse1.ClearSelection;
  verse2.ClearSelection;

  for i := 0 to High(source) do
    verse1.Syntagms[source[i].VInteger].AddToSelection;

  for i := 0 to High(dest) do
    verse2.Syntagms[dest[i].VInteger].AddToSelection;

  verse1.AssociateSyntagms;
end;

procedure TVerseTests.AssociateByText(const pair: TPair);
var
  word: string;
  i, s: Integer;
  words: array of string;
begin
  verse1.ClearSelection;
  verse2.ClearSelection;

  AssertTrue(Length(pair) = 2);
  words := pair[0];
  for i:=0 to Length(words)-1 do
  begin
    word := string(words[i]);
    s := FindIndexOf(verse1, word);
    if s = -1 then
       raise Exception.Create('Word not found in source text: ' + word);
    verse1.Syntagms[s].AddToSelection;
  end;
  words := pair[1];
  for i:=0 to Length(words)-1 do
  begin
    word := string(words[i]);
    s := FindIndexOf(verse2, word);
    if s = -1 then
       raise Exception.Create('Word not found in destination text: ' + word);
    verse2.Syntagms[s].AddToSelection;
  end;

  verse1.AssociateSyntagms;
end;

procedure TVerseTests.AssociatePairs(const pairs: TPairs);
var
  pair: TPair;
begin
  for pair in pairs do
    AssociateByText(pair);
end;

procedure TVerseTests.AssertPairs(const pairs: TPairs);
var
  pair: TPair;
begin
  for pair in pairs do
    AssertPair(pair);
end;

procedure TVerseTests.AssertPair(const pair: TPair);
var
  s: TSyntagm;
  word: string;
  i, idx: integer;
  srcList, dstList, siblings: TSyntagmList;
  src: array of string;
  dst: array of string;
begin
  AssertTrue(Length(pair) = 2);
  src := pair[0];
  dst := pair[1];
  AssertTrue(Length(src) > 0);
  AssertTrue(Length(dst) > 0);

  try
    srcList := TSyntagmList.Create;
    dstList := TSyntagmList.Create;
    for word in src do
      srcList.Add(verse1.Syntagms[FindIndexOf(verse1, word)]);
    AssertEquals(srcList.Count, Length(src));
    for word in dst do
      dstList.Add(verse2.Syntagms[FindIndexOf(verse2, word)]);
    AssertEquals(dstList.Count, Length(dst));

    siblings := TSyntagmList.Create();
    siblings.AddList(srcList);
    for i:=0 to srcList.Count-1 do
    begin
      s := srcList[i];
      // checking siblings
      idx := srcList.IndexOf(s);
      siblings.Delete(idx);
      AssertSyntagmListEquals(s.Siblings, siblings);
      siblings.Insert(idx, s);
      // checking pairs
      AssertSyntagmListEquals(s.Pairs, dstList);
    end;
  finally
    srcList.Free;
    dstList.Free;
    siblings.Free;
  end;
end;

procedure TVerseTests.AssertPair(const source, dest: TSyntagmList);
var
  s: TSyntagm;
begin
end;

procedure TVerseTests.AssertSyntagmListEquals(const list1, list2: TSyntagmList);
var
  i: integer;
begin
  AssertEquals(list1.Count, list2.Count);
  for i := 0 to list1.Count-1 do
    AssertTrue(list1[i] = list2[i]);
end;

procedure TVerseTests.AssertSyntagmListEquals(const list: TSyntagmList; const values: array of const);
var
  i: integer;
begin
  AssertEquals(High(values) + 1, list.Count);
  for i := 0 to High(values) do
    AssertTrue(Format('[%s]=[%s]', [TSyntagm(values[i].VPointer).RawText, list[i].RawText]),
               TSyntagm(values[i].VPointer) = list[i]);
end;

procedure TVerseTests.AssertPairsAndSiblings(syntagm: TSyntagm; const pairs, siblings: array of const);
begin
  AssertSyntagmListEquals(syntagm.Pairs, pairs);
  AssertSyntagmListEquals(syntagm.Siblings, siblings);
end;

function TVerseTests.FindIndexOf(verse: TVerse; word: string): Integer;
var
  list: TSyntagmList;
  i: Integer;
begin
  list := verse.Syntagms;
  Result := -1;

  for i := 0 to list.Count - 1 do
  begin
    if not list[i].IsAssociated and (list[i].Text = word) then
    begin
      if Result <> -1 then
        raise Exception.Create(Format('Duplicated word in verse', [word, Result, i]));
      result := i;
    end;
  end;

  if Result = -1 then
    raise Exception.Create(Format('Word [%s] not found in verse', [word]));
end;

procedure TVerseTests.SetUp;
begin
  verse1 := TVerse.Create();
  verse2 := TVerse.Create();
  verse1.PairedVerse := verse2;
  FVerseChangeCount := 0;
end;

procedure TVerseTests.TearDown;
begin
  verse1.Destroy;
  verse2.Destroy;
end;

initialization

  RegisterTest(TVerseTests);
end.

