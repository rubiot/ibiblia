unit versiculotest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Verse, syntagm;

type
  TVersiculoTest = class(TTestCase)
  published
    procedure TestInit;
    procedure TestFindByIndex;
    procedure TestFindAndSelectSyntagms_Basic;
    procedure TestFindAndSelectSyntagms_Partial;
    procedure TestFindAndSelectSyntagms_JoinConnected;
    procedure TestFindAndSelectSyntagms_JoinConnected_False;
    procedure TestFindAndSelectSyntagms_AvoidAssociated;
    // New comprehensive tests for association prevention
    procedure TestMultipleAssociations_NoReuse;
    procedure TestMultipleAssociations_SequentialSearch;
    procedure TestMultipleAssociations_AnyOrderSearch;
    procedure TestMultipleAssociations_WithVersePair;
    procedure TestMultipleAssociations_PartialMatch;
    // Migrated from aiassociatortest.pas
    procedure TestFindAndSelectSyntagms_Brackets;
    procedure TestRetokenizePreservesAssociationsAcrossTokenizerVersions;
    procedure TestDisassociatePairs_ToleratesAsymmetricSiblingLists;
    procedure TestAssociateSyntagms_ReverseVisualSelectionUsesLogicalOrder;
    procedure TestAssociation_Greek;
    procedure TestAssociation_Hebrew;
  end;

implementation

procedure TVersiculoTest.TestInit;
var
  v: TVerse;
begin
  v := TVerse.Create();
  try
    v.Text := 'This is a test verse';
    AssertEquals('Init', 'This is a test verse', v.Text);
  finally
    v.Free;
  end;
end;

procedure TVersiculoTest.TestFindByIndex;
var
  v: TVerse;
begin
  v := TVerse.Create();
  try
    v.Text := 'This is a test verse';
    AssertEquals('Index 0', 'This', v.Syntagms[0].Text);
    AssertEquals('Index 2', 'is', v.Syntagms[2].Text);
    AssertEquals('Index 4', 'a', v.Syntagms[4].Text);
    AssertEquals('Index 6', 'test', v.Syntagms[6].Text);
    AssertEquals('Index 8', 'verse', v.Syntagms[8].Text);
    //AssertEquals('Index 10', nil, v.Syntagms[10].Text);
  finally
    v.Free;
  end;
end;

procedure TVersiculoTest.TestFindAndSelectSyntagms_Basic;
var
  v: TVerse;
  words: TStringList;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'This is a test verse with some test words';
    words.Add('test'); words.Add('words');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, False, False);
    AssertEquals('Selected count', 2, v.Selection.Count);
    AssertEquals('Selected 0', 'test', v.Selection[0].Text);
    AssertEquals('Selected 1', 'words', v.Selection[1].Text);
  finally
    words.Free;
    v.Free;
  end;
end;

procedure TVersiculoTest.TestFindAndSelectSyntagms_Partial;
var
  v: TVerse;
  words: TStringList;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'This is a test verse with some test words';
    words.Add('invalid'); words.Add('words');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, True, True);
    AssertEquals('Partial selected count', 1, v.Selection.Count);
    AssertEquals('Partial selected 0', 'words', v.Selection[0].Text);
  finally
    words.Free;
    v.Free;
  end;
end;

procedure TVersiculoTest.TestFindAndSelectSyntagms_JoinConnected;
var
  v: TVerse;
  words: TStringList;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'וְהָאָ֗רֶץ הָיְתָ֥ה תֹ֨הוּ֙ וָבֹ֔הוּ וְחֹ֖שֶׁךְ עַל־פְּנֵ֣י תְה֑וֹם';
    words.Add('עַל'); words.Add('פְּנֵ֣י');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, False, True);
    AssertTrue('JoinConnected selected count >= 3', v.Selection.Count >= 3);
    AssertEquals('JoinConnected selected 0', 'עַל', v.Selection[0].Text);
    AssertEquals('JoinConnected selected 1', '־', v.Selection[1].Text);
    AssertEquals('JoinConnected selected 2', 'פְּנֵ֣י', v.Selection[2].Text);
  finally
    words.Free;
    v.Free;
  end;
end;

procedure TVersiculoTest.TestFindAndSelectSyntagms_JoinConnected_False;
var
  v: TVerse;
  words: TStringList;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'וְהָאָ֗רֶץ הָיְתָ֥ה תֹ֨הוּ֙ וָבֹ֔הוּ וְחֹ֖שֶׁךְ עַל־פְּנֵ֣י תְה֑וֹם';
    words.Add('עַל'); words.Add('פְּנֵ֣י');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, False, False);
    AssertEquals('JoinConnected False selected count', 2, v.Selection.Count);
    AssertEquals('JoinConnected False selected 0', 'עַל', v.Selection[0].Text);
    AssertEquals('JoinConnected False selected 1', 'פְּנֵ֣י', v.Selection[1].Text);
  finally
    words.Free;
    v.Free;
  end;
end;

procedure TVersiculoTest.TestFindAndSelectSyntagms_AvoidAssociated;
var
  v: TVerse;
  words: TStringList;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'This is a test verse with some test words';
    words.Add('test'); words.Add('verse');
    v.ClearSelection;
    // Mark the first occurrence of 'test' as associated
    v.Syntagms[6].IsAssociated:=True;
    v.FindAndSelectSyntagms(words, False, False);
    AssertEquals('AvoidAssociated selected count', 2, v.Selection.Count);
    AssertEquals('AvoidAssociated selected 0', 'test', v.Selection[0].Text); // Should be the second 'test'
    AssertEquals('AvoidAssociated selected 1', 'verse', v.Selection[1].Text);
  finally
    words.Free;
    v.Free;
  end;
end;

{ Test that simulates the real-world scenario: multiple AI-suggested associations
  should not reuse the same syntagm. This test creates two association pairs and
  verifies that syntagms from the first pair are not available for the second pair. }
procedure TVersiculoTest.TestMultipleAssociations_NoReuse;
var
  vOrig, vTrans: TVerse;
  words1, words2: TStringList;
  firstSyntagm: TSyntagm;
begin
  vOrig := TVerse.Create();
  vTrans := TVerse.Create();
  words1 := TStringList.Create;
  words2 := TStringList.Create;
  try
    // Set up verses - note "called" appears twice
    vOrig.Text := 'And God called the dry land Earth and called the gathering';
    vTrans.Text := 'E Deus chamou a terra seca Terra e chamou o ajuntamento';
    vOrig.PairedVerse := vTrans;
    vTrans.PairedVerse := vOrig;
    
    // First association: "And God called"
    words1.Add('And'); words1.Add('God'); words1.Add('called');
    vOrig.ClearSelection; vTrans.ClearSelection;
    vOrig.FindAndSelectSyntagms(words1, False, False);
    AssertEquals('First pair - original selected', 3, vOrig.Selection.Count);
    
    // Store reference to first syntagms before association
    firstSyntagm := vOrig.Selection[0];
    AssertEquals('First syntagm is And', 'And', firstSyntagm.Text);
    AssertTrue('First syntagm has no pairs yet', not Assigned(firstSyntagm.Pairs) or (firstSyntagm.Pairs.Count = 0));
    
    words1.Clear;
    words1.Add('E'); words1.Add('Deus'); words1.Add('chamou');
    vTrans.FindAndSelectSyntagms(words1, False, False);
    AssertEquals('First pair - translated selected', 3, vTrans.Selection.Count);
    
    // Create association (marks syntagms with Pairs)
    vOrig.AssociateSyntagms;
    AssertTrue('First syntagm now has pairs', Assigned(firstSyntagm.Pairs) and (firstSyntagm.Pairs.Count > 0));
    vOrig.ClearSelection; vTrans.ClearSelection;
    
    // Second association: "and called the" - should skip first "And" and first "called"
    // The second "called" should still be available (it's a different syntagm instance)
    words2.Add('and'); words2.Add('called'); words2.Add('the');
    vOrig.FindAndSelectSyntagms(words2, False, False);
    
    // Should find 3 words: second "and", second "called", and "the"
    AssertEquals('Second pair found 3 words', 3, vOrig.Selection.Count);
    
    // Verify that the first "And" is not in the second selection
    AssertTrue('Second selection does not contain first And', 
               vOrig.Selection.IndexOf(firstSyntagm) = -1);
    
    // Verify the syntagms selected are different instances
    AssertEquals('Found lowercase and', 'and', vOrig.Selection[0].Text);
    AssertEquals('Found second called', 'called', vOrig.Selection[1].Text);
    AssertEquals('Found the', 'the', vOrig.Selection[2].Text);
    
    // Verify the "called" we found is NOT the first one (which is at index 4)
    AssertTrue('Found different called instance', vOrig.Selection[1] <> vOrig.Syntagms[4]);
  finally
    words1.Free;
    words2.Free;
    vOrig.Free;
    vTrans.Free;
  end;
end;

{ Test sequential search specifically - ensures IsAssociated check works in sequential mode }
procedure TVersiculoTest.TestMultipleAssociations_SequentialSearch;
var
  v: TVerse;
  words: TStringList;
  syn1, syn2, syn3: TSyntagm;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'word1 word2 word3 word1 word2 word3';
    
    // First pass: select word1, word2, word3 (first occurrence)
    words.Add('word1'); words.Add('word2'); words.Add('word3');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, False, False);
    AssertEquals('First pass selected', 3, v.Selection.Count);
    
    syn1 := v.Selection[0];
    syn2 := v.Selection[1];
    syn3 := v.Selection[2];
    
    // Mark as associated by creating fake Pairs
    syn1.Pairs.Add(syn1); // Self-reference to mark as associated
    syn2.Pairs.Add(syn2);
    syn3.Pairs.Add(syn3);
    v.ClearSelection;
    
    // Second pass: try to select word1, word2, word3 again
    // Should find the SECOND occurrence (sequential search)
    v.FindAndSelectSyntagms(words, False, False);
    AssertEquals('Second pass selected', 3, v.Selection.Count);
    
    // Verify we got different syntagm instances (second occurrence)
    AssertTrue('Different word1', v.Selection[0] <> syn1);
    AssertTrue('Different word2', v.Selection[1] <> syn2);
    AssertTrue('Different word3', v.Selection[2] <> syn3);
    
    AssertEquals('Second word1 text', 'word1', v.Selection[0].Text);
    AssertEquals('Second word2 text', 'word2', v.Selection[1].Text);
    AssertEquals('Second word3 text', 'word3', v.Selection[2].Text);
  finally
    words.Free;
    v.Free;
  end;
end;

{ Test any-order search specifically - ensures IsAssociated check works in any-order mode }
procedure TVersiculoTest.TestMultipleAssociations_AnyOrderSearch;
var
  v: TVerse;
  words: TStringList;
  syn1, syn3: TSyntagm;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'word1 word2 word3 word4 word5';

    // First pass: ordered fallback should select word1 and word3, skipping word2.
    words.Add('word1'); words.Add('word3');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, False, False);
    AssertEquals('First pass selected', 2, v.Selection.Count);

    syn1 := v.Selection[0];
    syn3 := v.Selection[1];
    syn1.Pairs.Add(syn1); // Mark as associated
    syn3.Pairs.Add(syn3);
    v.ClearSelection;

    // Second pass: try to select word1 and word3 again.
    // Should fail because both are already associated
    v.FindAndSelectSyntagms(words, False, False);
    AssertEquals('Second pass found nothing', 0, v.Selection.Count);

    // Third pass: try with partial=True (should still find nothing)
    v.FindAndSelectSyntagms(words, True, False);
    AssertEquals('Third pass found nothing even with Partial', 0, v.Selection.Count);
  finally
    words.Free;
    v.Free;
  end;
end;

{ Test with actual verse pair - simulates real association workflow }
procedure TVersiculoTest.TestMultipleAssociations_WithVersePair;
var
  vOrig, vTrans: TVerse;
  words1Orig, words1Trans, words2Orig, words2Trans: TStringList;
begin
  vOrig := TVerse.Create();
  vTrans := TVerse.Create();
  words1Orig := TStringList.Create;
  words1Trans := TStringList.Create;
  words2Orig := TStringList.Create;
  words2Trans := TStringList.Create;
  try
    vOrig.Text := 'And God saw that it was good';
    vTrans.Text := 'E Deus viu que era bom';
    vOrig.PairedVerse := vTrans;
    vTrans.PairedVerse := vOrig;
    
    // === PAIR 1: "And God saw" <-> "E Deus viu" ===
    words1Orig.Add('And'); words1Orig.Add('God'); words1Orig.Add('saw');
    words1Trans.Add('E'); words1Trans.Add('Deus'); words1Trans.Add('viu');
    
    vOrig.ClearSelection; vTrans.ClearSelection;
    vOrig.FindAndSelectSyntagms(words1Orig, False, False);
    vTrans.FindAndSelectSyntagms(words1Trans, False, False);
    
    AssertEquals('Pair 1 - orig selected', 3, vOrig.Selection.Count);
    AssertEquals('Pair 1 - trans selected', 3, vTrans.Selection.Count);
    
    // Create first association
    vOrig.AssociateSyntagms;
    vOrig.ClearSelection; vTrans.ClearSelection;
    
    // === PAIR 2: "that it was good" <-> "que era bom" ===
    words2Orig.Add('that'); words2Orig.Add('it'); words2Orig.Add('was'); words2Orig.Add('good');
    words2Trans.Add('que'); words2Trans.Add('era'); words2Trans.Add('bom');
    
    vOrig.FindAndSelectSyntagms(words2Orig, False, False);
    vTrans.FindAndSelectSyntagms(words2Trans, False, False);
    
    AssertEquals('Pair 2 - orig selected', 4, vOrig.Selection.Count);
    AssertEquals('Pair 2 - trans selected', 3, vTrans.Selection.Count);
    
    // Verify that Pair 2 doesn't contain any syntagms from Pair 1
    AssertEquals('Pair 2 - that', 'that', vOrig.Selection[0].Text);
    AssertEquals('Pair 2 - it', 'it', vOrig.Selection[1].Text);
    AssertEquals('Pair 2 - was', 'was', vOrig.Selection[2].Text);
    AssertEquals('Pair 2 - good', 'good', vOrig.Selection[3].Text);
    
    // Create second association
    vOrig.AssociateSyntagms;
    
    // Verify all syntagms in both pairs have Pairs
    AssertTrue('And has pairs', Assigned(vOrig.Syntagms[0].Pairs) and (vOrig.Syntagms[0].Pairs.Count > 0));
    AssertTrue('God has pairs', Assigned(vOrig.Syntagms[2].Pairs) and (vOrig.Syntagms[2].Pairs.Count > 0));
    AssertTrue('saw has pairs', Assigned(vOrig.Syntagms[4].Pairs) and (vOrig.Syntagms[4].Pairs.Count > 0));
    AssertTrue('that has pairs', Assigned(vOrig.Syntagms[6].Pairs) and (vOrig.Syntagms[6].Pairs.Count > 0));
    AssertTrue('it has pairs', Assigned(vOrig.Syntagms[8].Pairs) and (vOrig.Syntagms[8].Pairs.Count > 0));
    AssertTrue('was has pairs', Assigned(vOrig.Syntagms[10].Pairs) and (vOrig.Syntagms[10].Pairs.Count > 0));
    AssertTrue('good has pairs', Assigned(vOrig.Syntagms[12].Pairs) and (vOrig.Syntagms[12].Pairs.Count > 0));
  finally
    words1Orig.Free; words1Trans.Free;
    words2Orig.Free; words2Trans.Free;
    vOrig.Free; vTrans.Free;
  end;
end;

{ Test partial match with already associated syntagms }
procedure TVersiculoTest.TestMultipleAssociations_PartialMatch;
var
  v: TVerse;
  words: TStringList;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.Text := 'word1 word2 word3 word4 word5';
    
    // Mark word2 and word4 as already associated by adding to Pairs
    v.Syntagms[2].Pairs.Add(v.Syntagms[2]);  // word2
    v.Syntagms[6].Pairs.Add(v.Syntagms[6]);  // word4
    
    // Try to find word2, word3, word4 with Partial=True
    words.Add('word2'); words.Add('word3'); words.Add('word4');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, True, False);
    
    // Should only find word3 (word2 and word4 are already associated)
    AssertEquals('Partial found only available word', 1, v.Selection.Count);
    AssertEquals('Found word3', 'word3', v.Selection[0].Text);
    
    // Verify word2 and word4 were NOT selected
    AssertTrue('word2 not in selection', v.Selection.IndexOf(v.Syntagms[2]) = -1);
    AssertTrue('word4 not in selection', v.Selection.IndexOf(v.Syntagms[6]) = -1);
  finally
    words.Free;
    v.Free;
  end;
end;

procedure TVersiculoTest.TestFindAndSelectSyntagms_Brackets;
var
  v: TVerse;
  words: TStringList;
begin
  v := TVerse.Create();
  words := TStringList.Create;
  try
    v.TokenizerVersion := 1;
    v.Text := 'In [the] beginning';
    words.Add('In'); words.Add('[the]'); words.Add('beginning');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, False, False);

    AssertEquals('Tokenizer v1 brackets selected count', 3, v.Selection.Count);
    AssertEquals('Tokenizer v1 brackets selected 1', '[the]', v.Selection[1].Text);

    v.TokenizerVersion := 2;
    v.Text := 'In [the] beginning';
    words.Clear;
    words.Add('In'); words.Add('[the]'); words.Add('beginning');
    v.ClearSelection;
    v.FindAndSelectSyntagms(words, False, False);

    AssertEquals('Tokenizer v2 brackets no longer match as one token', 0, v.Selection.Count);
  finally
    words.Free;
    v.Free;
  end;
end;

procedure TVersiculoTest.TestRetokenizePreservesAssociationsAcrossTokenizerVersions;
var
  vOrig, vDest: TVerse;
begin
  vOrig := TVerse.Create();
  vDest := TVerse.Create();
  try
    vOrig.TokenizerVersion := 1;
    vDest.TokenizerVersion := 1;
    vOrig.Text := 'alpha beta gamma';
    vDest.Text := 'one [two] three';
    vOrig.PairedVerse := vDest;
    vDest.PairedVerse := vOrig;

    vOrig.Syntagms[0].AddToSelection;
    vDest.Syntagms[0].AddToSelection;
    vOrig.AssociateSyntagms;
    vOrig.ClearSelection;
    vDest.ClearSelection;

    vOrig.Syntagms[4].AddToSelection;
    vDest.Syntagms[4].AddToSelection;
    vOrig.AssociateSyntagms;
    vOrig.ClearSelection;
    vDest.ClearSelection;

    AssertEquals('Tokenizer v1 pairs', '<par a="0" b="0"><par a="4" b="4">', vOrig.Pairs);

    vOrig.TokenizerVersion := 2;
    vDest.TokenizerVersion := 2;
    vDest.RetokenizePreservingAssociations;
    vOrig.RetokenizePreservingAssociations;

    AssertEquals('Tokenizer v2 token count', 7, vDest.Syntagms.Count);
    AssertEquals('Tokenizer v2 migrated pairs', '<par a="0" b="0"><par a="4" b="6">', vOrig.Pairs);
    AssertEquals('Association after brackets preserved', 'three', vOrig.Syntagms[4].Pairs[0].Text);
  finally
    vOrig.Free;
    vDest.Free;
  end;
end;

procedure TVersiculoTest.TestDisassociatePairs_ToleratesAsymmetricSiblingLists;
var
  vOrig, vDest: TVerse;
  leftSyn, rightSyn, destSyn: TSyntagm;
begin
  vOrig := TVerse.Create();
  vDest := TVerse.Create();
  try
    vOrig.Text := 'alpha beta';
    vDest.Text := 'one';
    vOrig.PairedVerse := vDest;
    vDest.PairedVerse := vOrig;

    leftSyn := vOrig.Syntagms[0];
    rightSyn := vOrig.Syntagms[2];
    destSyn := vDest.Syntagms[0];

    leftSyn.AddToSelection;
    rightSyn.AddToSelection;
    destSyn.AddToSelection;
    vOrig.AssociateSyntagms;
    vOrig.ClearSelection;
    vDest.ClearSelection;

    rightSyn.Siblings.Remove(leftSyn);

    leftSyn.DisassociatePairs;

    AssertEquals('Left syntagm pairs cleared', 0, leftSyn.Pairs.Count);
    AssertEquals('Left syntagm siblings cleared', 0, leftSyn.Siblings.Count);
    AssertEquals('Right syntagm pairs cleared', 0, rightSyn.Pairs.Count);
    AssertEquals('Right syntagm siblings cleared', 0, rightSyn.Siblings.Count);
    AssertEquals('Destination syntagm pairs cleared', 0, destSyn.Pairs.Count);
  finally
    vOrig.Free;
    vDest.Free;
  end;
end;

procedure TVersiculoTest.
  TestAssociateSyntagms_ReverseVisualSelectionUsesLogicalOrder;
var
  vOrig, vDest: TVerse;
  sourceAlpha, sourceBeta, sourceGamma: TSyntagm;
  targetUm, targetDois, targetTres: TSyntagm;
begin
  vOrig := TVerse.Create;
  vDest := TVerse.Create;
  try
    vOrig.Text := 'alpha beta gamma';
    vDest.Text := 'um dois tres';
    vOrig.PairedVerse := vDest;
    vDest.PairedVerse := vOrig;

    sourceAlpha := vOrig.Syntagms[0];
    sourceBeta := vOrig.Syntagms[2];
    sourceGamma := vOrig.Syntagms[4];
    targetUm := vDest.Syntagms[0];
    targetDois := vDest.Syntagms[2];
    targetTres := vDest.Syntagms[4];

    { Simulate clicking the visual RTL order, rightmost/last logical item first. }
    sourceGamma.AddToSelection;
    sourceBeta.AddToSelection;
    sourceAlpha.AddToSelection;
    targetTres.AddToSelection;
    targetDois.AddToSelection;
    targetUm.AddToSelection;

    AssertTrue('Source selection records click order',
      vOrig.Selection[0] = sourceGamma);
    AssertTrue('Target selection records click order',
      vDest.Selection[0] = targetTres);

    vOrig.AssociateSyntagms;

    AssertEquals('Source selection remains transient and complete', 3,
      vOrig.Selection.Count);
    AssertTrue('Source selection keeps reverse visual click order 0',
      vOrig.Selection[0] = sourceGamma);
    AssertTrue('Source selection keeps reverse visual click order 1',
      vOrig.Selection[1] = sourceBeta);
    AssertTrue('Source selection keeps reverse visual click order 2',
      vOrig.Selection[2] = sourceAlpha);
    AssertEquals('Target selection remains transient and complete', 3,
      vDest.Selection.Count);
    AssertTrue('Target selection keeps reverse visual click order 0',
      vDest.Selection[0] = targetTres);
    AssertTrue('Target selection keeps reverse visual click order 1',
      vDest.Selection[1] = targetDois);
    AssertTrue('Target selection keeps reverse visual click order 2',
      vDest.Selection[2] = targetUm);

    AssertTrue('Source pairs start in target logical order',
      sourceAlpha.Pairs[0] = targetUm);
    AssertTrue('Source pairs continue in target logical order',
      sourceAlpha.Pairs[1] = targetDois);
    AssertTrue('Source pairs end in target logical order',
      sourceAlpha.Pairs[2] = targetTres);
    AssertTrue('Every source syntagm receives the same logical pair order',
      (sourceBeta.Pairs[0] = targetUm) and
      (sourceGamma.Pairs[2] = targetTres));

    AssertTrue('Alpha siblings are logical beta, gamma',
      (sourceAlpha.Siblings[0] = sourceBeta) and
      (sourceAlpha.Siblings[1] = sourceGamma));
    AssertTrue('Beta siblings are logical alpha, gamma',
      (sourceBeta.Siblings[0] = sourceAlpha) and
      (sourceBeta.Siblings[1] = sourceGamma));
    AssertTrue('Gamma siblings are logical alpha, beta',
      (sourceGamma.Siblings[0] = sourceAlpha) and
      (sourceGamma.Siblings[1] = sourceBeta));

    AssertTrue('Target pairs start in source logical order',
      targetUm.Pairs[0] = sourceAlpha);
    AssertTrue('Target pairs continue in source logical order',
      targetUm.Pairs[1] = sourceBeta);
    AssertTrue('Target pairs end in source logical order',
      targetUm.Pairs[2] = sourceGamma);
    AssertTrue('Target siblings remain in logical order',
      (targetTres.Siblings[0] = targetUm) and
      (targetTres.Siblings[1] = targetDois));

    AssertEquals('Association serialization uses logical model indexes',
      '<par a="0,2,4" b="0,2,4">', vOrig.Pairs);
  finally
    vOrig.Free;
    vDest.Free;
  end;
end;

procedure TVersiculoTest.TestAssociation_Greek;
var
  v1, v2: TVerse;
  procedure Assoc(const Orig, Trans: string);
  var
    w1, w2: TStringList;
  begin
    w1 := TStringList.Create; w2 := TStringList.Create;
    try
      w1.Delimiter := ' '; w1.DelimitedText := Orig;
      w2.Delimiter := ' '; w2.DelimitedText := Trans;
      v1.ClearSelection; v2.ClearSelection;
      v1.FindAndSelectSyntagms(w1);
      v2.FindAndSelectSyntagms(w2);
      v1.AssociateSyntagms;
    finally
      w1.Free; w2.Free;
    end;
  end;
begin
  v1 := TVerse.Create();
  v2 := TVerse.Create();
  try
    v1.Text := 'Βίβλος γενέσεως ἸΗΣΟΥ Χριστοῦ, υἱοῦ Δαβὶδ, υἱοῦ Ἁβραάμ.';
    v2.Text := 'LIVRO da geração de Jesus Cristo, filho de Davi, filho de Abraão.';
    v1.PairedVerse := v2; v2.PairedVerse := v1;
    
    Assoc('Βίβλος', 'LIVRO');
    Assoc('γενέσεως', 'da geração');
    Assoc('ἸΗΣΟΥ', 'de Jesus');
    Assoc('Χριστοῦ', 'Cristo');
    Assoc('υἱοῦ', 'filho de');
    Assoc('Δαβὶδ', 'Davi');
    Assoc('υἱοῦ', 'filho de');
    Assoc('Ἁβραάμ', 'Abraão');
    
    AssertEquals('Greek associations', '<par a="0" b="0"><par a="2" b="2,4"><par a="4" b="6,8"><par a="6" b="10"><par a="9" b="13,15"><par a="11" b="17"><par a="14" b="20,22"><par a="16" b="24">', v1.Pairs);
  finally
    v1.Free; v2.Free;
  end;
end;

procedure TVersiculoTest.TestAssociation_Hebrew;
var
  v1, v2: TVerse;
  procedure Assoc(const Orig, Trans: string);
  var
    w1, w2: TStringList;
  begin
    w1 := TStringList.Create; w2 := TStringList.Create;
    try
      w1.Delimiter := ' ';
      w1.DelimitedText := Orig.Replace('־', ' ').Replace('׃', ' '); // Split on Maqqef and handle Sof Pasuq
      w2.Delimiter := ' ';
      w2.DelimitedText := Trans;
      v1.ClearSelection; v2.ClearSelection;
      v1.FindAndSelectSyntagms(w1, True, True); // Use JoinConnected for Hebrew
      v2.FindAndSelectSyntagms(w2, True, True);
      v1.AssociateSyntagms;
    finally
      w1.Free; w2.Free;
    end;
  end;
begin
  v1 := TVerse.Create();
  v2 := TVerse.Create();
  try
    v1.Text := 'וְהָאָ֗רֶץ הָיְתָ֥ה תֹ֨הוּ֙ וָבֹ֔הוּ וְחֹ֖שֶׁךְ עַל־פְּנֵ֣י תְה֑וֹם וְר֣וּחַ אֱלֹהִ֔ים מְרַחֶ֖פֶת עַל־פְּנֵ֥י הַמָּֽיִם׃';
    v2.Text := 'E a terra era sem forma e vazia; e [havia] trevas sobre a face do abismo; e o Espírito de Deus se movia sobre a face das águas.';
    v1.PairedVerse := v2; v2.PairedVerse := v1;
    
    Assoc('וְהָאָ֗רֶץ', 'E a terra');
    Assoc('הָיְתָ֥ה', 'era');
    Assoc('תֹ֨הוּ֙', 'sem forma');
    Assoc('וָבֹ֔הוּ', 'e vazia');
    Assoc('וְחֹ֖שֶׁךְ', 'e havia trevas');
    Assoc('עַל־פְּנֵ֣י', 'sobre a face');
    Assoc('תְה֑וֹם', 'do abismo');
    Assoc('וְר֣וּחַ', 'e o Espírito');
    Assoc('אֱלֹהִ֔ים', 'de Deus');
    Assoc('מְרַחֶ֖פֶת', 'se movia');
    Assoc('עַל־פְּנֵ֥י', 'sobre a face');
    Assoc('הַמָּֽיִם׃', 'das águas');
    
    AssertEquals('Hebrew associations', '<par a="0" b="0,2,4"><par a="2" b="6"><par a="4" b="8,10"><par a="6" b="12,14"><par a="8" b="17,20,23"><par a="10,11,12" b="25,27,29"><par a="14" b="31,33"><par a="16" b="36,38,40"><par a="18" b="42,44"><par a="20" b="46,48"><par a="22,23,24" b="50,52,54"><par a="26" b="56,58">', v1.Pairs);
  finally
    v1.Free; v2.Free;
  end;
end;

initialization
  RegisterTest(TVersiculoTest);

end.
