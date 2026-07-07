unit ai_processing_batch_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ai_flows, ai_processing_batch,
  PatchFile;

type
  TAIProcessingBatchTests = class(TTestCase)
  published
    procedure CreatePatchAppliesOnlyAcceptedFields;
    procedure CreatePatchSkipsRejectedChanges;
    procedure ApplyAcceptedResultKeepsUnownedFieldsUnchanged;
    procedure AcceptVerseProposalIncludesChangesInPatch;
    procedure RejectVerseProposalExcludesChangesFromPatch;
    procedure SelectedIndexesLimitAcceptedVerseApplication;
    procedure AssociationProposalAppliesOnlyPairsAndComments;
    procedure TranslationProposalAppliesOnlyDestinationText;
    procedure CommentsDefaultDecisionMergesCurrentAndAI;
    procedure KeepCurrentCommentsStillAppliesOtherChanges;
    procedure UseAICommentsReplacesCurrentComments;
    procedure EmptyAICommentsDoNotCreateCommentChange;
    procedure RejectedVerseDoesNotApplyCommentsDecision;
  end;

implementation

procedure TAIProcessingBatchTests.CreatePatchAppliesOnlyAcceptedFields;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  try
    OriginalData.Add('40,1,1', 'source old', 'destination old', 'pairs old', 'comments old', 1);
    Batch.Add('40,1,1', 'source new', 'destination new', 'pairs new', 'comments new', 2);
    Batch.SetFieldAccepted(0, arfDestinationText, True);
    Batch.SetFieldAccepted(0, arfComments, True);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('source old', Patch.SourceText[0]);
      AssertEquals('destination new', Patch.DestinationText[0]);
      AssertEquals('pairs old', Patch.Pairs[0]);
      AssertEquals('comments old<br/><br/>comments new', Patch.Comments[0]);
      AssertEquals('1', Patch.Status[0]);
    finally
      Patch.Free;
    end;
  finally
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.CreatePatchSkipsRejectedChanges;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  try
    OriginalData.Add('40,1,1', 'source old', 'destination old', 'pairs old', 'comments old', 1);
    Batch.Add('40,1,1', 'source new', 'destination new', 'pairs new', 'comments new', 2);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(0, AppliedCount);
      AssertEquals(0, Patch.Reference.Count);
    finally
      Patch.Free;
    end;
  finally
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.ApplyAcceptedResultKeepsUnownedFieldsUnchanged;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  FlowResult: TAIFlowResult;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source original', 'destination original',
      'pairs old', 'comments old', 1);
    Batch.Add('40,1,1', 'source original', 'destination original',
      'pairs old', 'comments old', 1);

    FlowResult.SetAcceptedPairs('pairs new');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.AcceptAllChangedFields(OriginalData, 0);

    AssertFalse(Batch.FieldChanged(OriginalData, 0, arfSourceText));
    AssertFalse(Batch.FieldChanged(OriginalData, 0, arfDestinationText));
    AssertTrue(Batch.FieldChanged(OriginalData, 0, arfAssociations));
    AssertFalse(Batch.FieldChanged(OriginalData, 0, arfComments));
    AssertFalse(Batch.IsFieldAccepted(0, arfSourceText));
    AssertFalse(Batch.IsFieldAccepted(0, arfDestinationText));
    AssertTrue(Batch.IsFieldAccepted(0, arfAssociations));
    AssertFalse(Batch.IsFieldAccepted(0, arfComments));
    AssertEquals(1, Batch.AcceptedFieldChanges(OriginalData, 0));
  finally
    FlowResult.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.AcceptVerseProposalIncludesChangesInPatch;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  try
    OriginalData.Add('40,1,1', 'source old', 'destination old',
      'pairs old', 'comments old', 1);
    Batch.Add('40,1,1', 'source old', 'destination new',
      'pairs old', 'comments old', 1);

    Batch.AcceptVerseProposal(OriginalData, 0);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('destination new', Patch.DestinationText[0]);
    finally
      Patch.Free;
    end;
  finally
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.RejectVerseProposalExcludesChangesFromPatch;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  try
    OriginalData.Add('40,1,1', 'source old', 'destination old',
      'pairs old', 'comments old', 1);
    Batch.Add('40,1,1', 'source old', 'destination new',
      'pairs old', 'comments old', 1);

    Batch.AcceptVerseProposal(OriginalData, 0);
    Batch.RejectVerseProposal(0);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(0, AppliedCount);
      AssertEquals(0, Patch.Reference.Count);
    finally
      Patch.Free;
    end;
  finally
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.SelectedIndexesLimitAcceptedVerseApplication;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  SelectedIndexes: TAIBatchIndexList;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  SelectedIndexes := TAIBatchIndexList.Create;
  try
    OriginalData.Add('40,1,1', 'source 1', 'destination 1',
      'pairs old 1', 'comments old 1', 1);
    OriginalData.Add('40,1,2', 'source 2', 'destination 2',
      'pairs old 2', 'comments old 2', 1);
    Batch.Add('40,1,1', 'source 1', 'destination new 1',
      'pairs old 1', 'comments old 1', 1);
    Batch.Add('40,1,2', 'source 2', 'destination new 2',
      'pairs old 2', 'comments old 2', 1);

    Batch.AcceptVerseProposal(OriginalData, 0);
    Batch.AcceptVerseProposal(OriginalData, 1);
    SelectedIndexes.Add('1');

    Patch := Batch.CreatePatch(OriginalData, SelectedIndexes, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('40,1,2', Patch.Reference[0]);
      AssertEquals('destination new 2', Patch.DestinationText[0]);
      AssertEquals(1, Batch.CountAcceptedVerses(OriginalData, SelectedIndexes));
    finally
      Patch.Free;
    end;
  finally
    SelectedIndexes.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.AssociationProposalAppliesOnlyPairsAndComments;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  FlowResult: TAIFlowResult;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source old', 'destination old',
      'pairs old', 'comments old', 1);
    Batch.Add('40,1,1', 'source old', 'destination old',
      'pairs old', 'comments old', 1);

    FlowResult.SetAcceptedPairs('pairs new');
    FlowResult.SetAcceptedComments('comments new');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.AcceptVerseProposal(OriginalData, 0);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('source old', Patch.SourceText[0]);
      AssertEquals('destination old', Patch.DestinationText[0]);
      AssertEquals('pairs new', Patch.Pairs[0]);
      AssertEquals('comments old<br/><br/>comments new', Patch.Comments[0]);
      AssertEquals('1', Patch.Status[0]);
      AssertFalse(Batch.IsFieldAccepted(0, arfSourceText));
      AssertFalse(Batch.IsFieldAccepted(0, arfDestinationText));
      AssertTrue(Batch.IsFieldAccepted(0, arfAssociations));
      AssertTrue(Batch.IsFieldAccepted(0, arfComments));
    finally
      Patch.Free;
    end;
  finally
    FlowResult.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.TranslationProposalAppliesOnlyDestinationText;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  FlowResult: TAIFlowResult;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source old', 'destination old',
      'pairs old', 'comments old', 1);
    Batch.Add('40,1,1', 'source old', 'destination old',
      'pairs old', 'comments old', 1);

    FlowResult.SetAcceptedDestinationText('destination new');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.AcceptVerseProposal(OriginalData, 0);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('source old', Patch.SourceText[0]);
      AssertEquals('destination new', Patch.DestinationText[0]);
      AssertEquals('pairs old', Patch.Pairs[0]);
      AssertEquals('comments old', Patch.Comments[0]);
      AssertEquals('1', Patch.Status[0]);
      AssertFalse(Batch.IsFieldAccepted(0, arfSourceText));
      AssertTrue(Batch.IsFieldAccepted(0, arfDestinationText));
      AssertFalse(Batch.IsFieldAccepted(0, arfAssociations));
      AssertFalse(Batch.IsFieldAccepted(0, arfComments));
    finally
      Patch.Free;
    end;
  finally
    FlowResult.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.CommentsDefaultDecisionMergesCurrentAndAI;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  FlowResult: TAIFlowResult;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);
    Batch.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);

    FlowResult.SetAcceptedComments('AI comments');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.AcceptVerseProposal(OriginalData, 0);

    AssertEquals(Ord(acdMerge), Ord(Batch.CommentsDecision[0]));
    AssertEquals('current comments<br/><br/>AI comments',
      Batch.EffectiveComments(OriginalData, 0));

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('current comments<br/><br/>AI comments', Patch.Comments[0]);
    finally
      Patch.Free;
    end;
  finally
    FlowResult.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.KeepCurrentCommentsStillAppliesOtherChanges;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  FlowResult: TAIFlowResult;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);
    Batch.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);

    FlowResult.SetAcceptedPairs('pairs new');
    FlowResult.SetAcceptedComments('AI comments');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.SetCommentsDecision(0, acdKeepCurrent);
    Batch.AcceptVerseProposal(OriginalData, 0);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('pairs new', Patch.Pairs[0]);
      AssertEquals('current comments', Patch.Comments[0]);
      AssertFalse(Batch.IsFieldAccepted(0, arfComments));
    finally
      Patch.Free;
    end;
  finally
    FlowResult.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.UseAICommentsReplacesCurrentComments;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  FlowResult: TAIFlowResult;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);
    Batch.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);

    FlowResult.SetAcceptedComments('AI comments');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.SetCommentsDecision(0, acdUseAI);
    Batch.AcceptVerseProposal(OriginalData, 0);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(1, AppliedCount);
      AssertEquals('AI comments', Patch.Comments[0]);
      AssertTrue(Batch.IsFieldAccepted(0, arfComments));
    finally
      Patch.Free;
    end;
  finally
    FlowResult.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.EmptyAICommentsDoNotCreateCommentChange;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  FlowResult: TAIFlowResult;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);
    Batch.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);

    FlowResult.SetAcceptedComments('');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.AcceptVerseProposal(OriginalData, 0);

    Patch := Batch.CreatePatch(OriginalData, nil, AppliedCount);
    try
      AssertEquals(0, AppliedCount);
      AssertEquals('current comments', Batch.EffectiveComments(OriginalData, 0));
      AssertFalse(Batch.IsFieldAccepted(0, arfComments));
    finally
      Patch.Free;
    end;
  finally
    FlowResult.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

procedure TAIProcessingBatchTests.RejectedVerseDoesNotApplyCommentsDecision;
var
  OriginalData: TAIProcessingBatch;
  Batch: TAIProcessingBatch;
  SelectedIndexes: TAIBatchIndexList;
  FlowResult: TAIFlowResult;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  OriginalData := TAIProcessingBatch.Create;
  Batch := TAIProcessingBatch.Create;
  SelectedIndexes := TAIBatchIndexList.Create;
  FlowResult := TAIFlowResult.Create;
  try
    OriginalData.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);
    Batch.Add('40,1,1', 'source', 'destination',
      'pairs old', 'current comments', 1);

    FlowResult.SetAcceptedComments('AI comments');
    Batch.ApplyAcceptedResult(0, FlowResult);
    Batch.SetCommentsDecision(0, acdUseAI);
    Batch.AcceptVerseProposal(OriginalData, 0);
    Batch.RejectVerseProposal(0);

    Patch := Batch.CreatePatch(OriginalData, SelectedIndexes, AppliedCount);
    try
      AssertEquals(0, AppliedCount);
      AssertEquals(0, Patch.Reference.Count);
    finally
      Patch.Free;
    end;
  finally
    FlowResult.Free;
    SelectedIndexes.Free;
    Batch.Free;
    OriginalData.Free;
  end;
end;

initialization
  RegisterTest(TAIProcessingBatchTests);

end.
