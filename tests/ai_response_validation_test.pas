unit ai_response_validation_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Verse, Syntagm, ai_flows,
  ai_association_flow, ai_translation_flow, ai_batch_work_item,
  ai_processing_batch, PatchFile;

type
  TAIResponseValidationTests = class(TTestCase)
  private
    FSource, FDestination: TVerse;
    FThrowOnNextChange: Boolean;
    procedure RaiseOnceAfterMutation;
    procedure AddOriginalAssociation;
    procedure AssertNoAcceptedFields(Parsed: TAIFlowResult);
    procedure AssertApplicationRejected(const Flow: IAIVerseFlow;
      Parsed: TAIFlowResult; AVerse: TVerse);
    procedure AssertNoPatch(Parsed: TAIFlowResult; const OriginalPairs: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AssociationParserRejectsInvalidEnvelopes;
    procedure AssociationParserKeepsValidPairsAmongMalformedEntries;
    procedure AssociationEmptyDataRequiresEmptyVersePair;
    procedure AssociationRejectedTemporaryResultCannotEraseExistingPairs;
    procedure AssociationFailureAfterMutationRestoresPairs;
    procedure AssociationMixedMatchesPublishesOnlyUsefulPairs;
    procedure AssociationEmptyVersePairIsNeutral;
    procedure TranslationPlainRejectsDestructiveEmptyResponses;
    procedure TranslationApplyRejectsEmptyResultWithoutParseContext;
    procedure TranslationPlainEmptyVersePairIsNeutral;
    procedure TranslationCombinedRejectsEmptyWithExistingDestination;
    procedure TranslationCombinedFailureRestoresExistingTextAndPairs;
    procedure EmptyResponsePreservesDestinationNotesAndMetadata;
  end;

implementation

procedure FreeVersePair(var Source, Destination: TVerse);
begin
  if Assigned(Source) then Source.PairedVerse := nil;
  if Assigned(Destination) then Destination.PairedVerse := nil;
  FreeAndNil(Destination);
  FreeAndNil(Source);
end;

procedure TAIResponseValidationTests.SetUp;
begin
  inherited SetUp;
  FSource := TVerse.Create;
  FDestination := TVerse.Create;
  FSource.Text := 'source next';
  FDestination.Text := 'target other';
  FSource.ReferenceID := '1,1,1';
  FSource.Reference := 'Genesis 1:1';
  FSource.PairedVerse := FDestination;
  FDestination.PairedVerse := FSource;
  FThrowOnNextChange := False;
end;

procedure TAIResponseValidationTests.TearDown;
begin
  FSource.OnVerseChanged := nil;
  FreeVersePair(FSource, FDestination);
  inherited TearDown;
end;

procedure TAIResponseValidationTests.RaiseOnceAfterMutation;
begin
  if not FThrowOnNextChange then Exit;
  FThrowOnNextChange := False;
  raise Exception.Create('Injected failure after association mutation');
end;

procedure TAIResponseValidationTests.AddOriginalAssociation;
var
  Words: TStringList;
begin
  Words := TStringList.Create;
  try
    Words.Add('source');
    FSource.FindAndSelectSyntagms(Words);
    Words[0] := 'target';
    FDestination.FindAndSelectSyntagms(Words);
    FSource.AssociateSyntagms;
    FSource.ClearSelection;
    FDestination.ClearSelection;
    AssertTrue('Fixture must contain an existing association', FSource.Pairs <> '');
  finally
    Words.Free;
  end;
end;

procedure TAIResponseValidationTests.AssertNoAcceptedFields(Parsed: TAIFlowResult);
begin
  AssertFalse('Source text must not be published', Parsed.AcceptedSourceTextSet);
  AssertFalse('Destination text must not be published', Parsed.AcceptedDestinationTextSet);
  AssertFalse('Associations must not be published', Parsed.AcceptedPairsSet);
  AssertFalse('Comments must not be published', Parsed.AcceptedCommentsSet);
  AssertFalse('Status must not be published', Parsed.AcceptedStatusSet);
end;

procedure TAIResponseValidationTests.AssertApplicationRejected(
  const Flow: IAIVerseFlow; Parsed: TAIFlowResult; AVerse: TVerse);
var
  Failed: Boolean;
begin
  Failed := False;
  try
    Flow.ApplyResult(Parsed, AVerse);
  except
    on E: Exception do Failed := True;
  end;
  AssertTrue('Invalid proposal must fail before publishing accepted fields', Failed);
  AssertNoAcceptedFields(Parsed);
end;

procedure TAIResponseValidationTests.AssertNoPatch(Parsed: TAIFlowResult;
  const OriginalPairs: string);
var
  Original, Proposal: TAIProcessingBatch;
  Patch: TPatchFile;
  AppliedCount: Integer;
begin
  Original := TAIProcessingBatch.Create;
  Proposal := TAIProcessingBatch.Create;
  Patch := nil;
  try
    Original.Add('1,1,1', FSource.Text, FDestination.Text, OriginalPairs, '', 0);
    Proposal.Add('1,1,1', FSource.Text, FDestination.Text, OriginalPairs, '', 0);
    Proposal.ApplyAcceptedResult(0, Parsed);
    Proposal.AcceptVerseProposal(Original, 0);
    Patch := Proposal.CreatePatch(Original, nil, AppliedCount);
    AssertEquals('Rejected or neutral response cannot produce a patch', 0, AppliedCount);
    AssertEquals(0, Patch.Reference.Count);
    AssertEquals(OriginalPairs, Proposal.Pairs[0]);
  finally
    Patch.Free;
    Proposal.Free;
    Original.Free;
  end;
end;

procedure TAIResponseValidationTests.AssociationParserRejectsInvalidEnvelopes;
const
  Invalid: array[0..6] of string = (
    '', '{}', '[]', 'null', '{"data":null}', '{"data":{}}',
    '{"data":[],"comments":42}');
var
  I: Integer;
  Parsed: TAssociationResult;
begin
  for I := Low(Invalid) to High(Invalid) do
  begin
    Parsed := ParseAssociationJson(Invalid[I]);
    try
      AssertTrue('Envelope must be rejected: ' + Invalid[I], Parsed.ParseError <> '');
      AssertNoAcceptedFields(Parsed);
    finally
      Parsed.Free;
    end;
  end;
end;

procedure TAIResponseValidationTests.AssociationParserKeepsValidPairsAmongMalformedEntries;
var
  Parsed: TAssociationResult;
begin
  Parsed := ParseAssociationJson('```json' + LineEnding +
    '{"data":[null,{},[],["source"],["source",42],["","target"],' +
    '["next"," "],["source","target"],["next","other"]],"comments":"review"}' +
    LineEnding + '```');
  try
    AssertEquals('', Parsed.ParseError);
    AssertEquals(2, Length(Parsed.Pairs));
    AssertEquals('source', Parsed.Pairs[0].OriginalPhrase);
    AssertEquals('target', Parsed.Pairs[0].TranslatedPhrase);
    AssertEquals('next', Parsed.Pairs[1].OriginalPhrase);
    AssertEquals('review', Parsed.Comments);
    AssertNoAcceptedFields(Parsed);
  finally
    Parsed.Free;
  end;
end;

procedure TAIResponseValidationTests.AssociationEmptyDataRequiresEmptyVersePair;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  I: Integer;
begin
  Flow := TAIAssociationFlow.Create;
  for I := 0 to 2 do
  begin
    case I of
      1: FSource.Text := '';
      2: begin FSource.Text := 'source'; FDestination.Text := ''; end;
    end;
    Parsed := Flow.ParseResponse('{"data":[]}', FSource);
    try
      AssertTrue('A non-empty side requires useful associations', Parsed.ParseError <> '');
      AssertApplicationRejected(Flow, Parsed, FSource);
    finally
      Parsed.Free;
    end;
  end;
end;

procedure TAIResponseValidationTests.AssociationRejectedTemporaryResultCannotEraseExistingPairs;
const
  Responses: array[0..3] of string = (
    '{}', '[]', '{"data":[]}', '{"data":[["missing","absent"]]}');
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  Data: TVerseData;
  Work: TAIBatchWorkItem;
  Source, Destination: TVerse;
  I: Integer;
begin
  AddOriginalAssociation;
  Data := Default(TVerseData);
  Data.ReferenceID := FSource.ReferenceID;
  Data.ReferenceName := FSource.Reference;
  Data.OriginalText := FSource.Text;
  Data.TranslatedText := FDestination.Text;
  Data.Associations := FSource.Pairs;
  Work := TAIBatchWorkItem.Create(FSource.TokenizerVersion, Data);
  Source := nil; Destination := nil;
  Flow := TAIAssociationFlow.Create;
  try
    Work.CreateTempVersePair(Source, Destination);
    AssertEquals('Batch proposal starts without original associations', '', Source.Pairs);
    for I := Low(Responses) to High(Responses) do
    begin
      Parsed := Flow.ParseResponse(Responses[I], Source);
      try
        AssertApplicationRejected(Flow, Parsed, Source);
        AssertNoPatch(Parsed, Data.Associations);
        AssertEquals(Data.Associations, FSource.Pairs);
      finally
        Parsed.Free;
      end;
    end;
  finally
    FreeVersePair(Source, Destination);
    Work.Free;
  end;
end;

procedure TAIResponseValidationTests.AssociationFailureAfterMutationRestoresPairs;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  OriginalPairs: string;
begin
  AddOriginalAssociation;
  OriginalPairs := FSource.Pairs;
  Flow := TAIAssociationFlow.Create(DEFAULT_ASSOCIATION_PROMPT, True);
  Parsed := Flow.ParseResponse(
    '{"data":[["next","other"]],"comments":"must not be published"}', FSource);
  try
    AssertEquals('', Parsed.ParseError);
    FThrowOnNextChange := True;
    FSource.OnVerseChanged := @RaiseOnceAfterMutation;
    AssertApplicationRejected(Flow, Parsed, FSource);
    AssertFalse('The injected error must occur after a real mutation', FThrowOnNextChange);
    AssertEquals('Existing associations must be restored', OriginalPairs, FSource.Pairs);
    AssertNoPatch(Parsed, OriginalPairs);
  finally
    FSource.OnVerseChanged := nil;
    Parsed.Free;
  end;
end;

procedure TAIResponseValidationTests.AssociationMixedMatchesPublishesOnlyUsefulPairs;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
begin
  Flow := TAIAssociationFlow.Create(DEFAULT_ASSOCIATION_PROMPT, True);
  Parsed := Flow.ParseResponse(
    '{"data":[["missing","absent"],["source","target"],["next",42]],"comments":"review"}',
    FSource);
  try
    AssertEquals('', Parsed.ParseError);
    Flow.ApplyResult(Parsed, FSource);
    AssertTrue(Parsed.AcceptedPairsSet);
    AssertTrue(Parsed.AcceptedPairs <> '');
    AssertEquals(FSource.Pairs, Parsed.AcceptedPairs);
    AssertTrue(Parsed.AcceptedCommentsSet);
    AssertEquals('review', Parsed.AcceptedComments);
    AssertEquals(1, Length(FSource.GetAssociationPairData(plText)));
  finally
    Parsed.Free;
  end;
end;

procedure TAIResponseValidationTests.AssociationEmptyVersePairIsNeutral;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
begin
  FSource.Text := ''; FDestination.Text := '';
  Flow := TAIAssociationFlow.Create;
  Parsed := Flow.ParseResponse('{"data":[]}', FSource);
  try
    AssertEquals('', Parsed.ParseError);
    Flow.ApplyResult(Parsed, FSource);
    AssertNoAcceptedFields(Parsed);
    AssertNoPatch(Parsed, '');
  finally
    Parsed.Free;
  end;
end;

procedure TAIResponseValidationTests.TranslationPlainRejectsDestructiveEmptyResponses;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  I: Integer;
  OriginalText: string;
begin
  Flow := TAITranslationFlow.Create(DEFAULT_TRANSLATION_PROMPT, True, False);
  for I := 0 to 2 do
  begin
    case I of
      1: FSource.Text := '';
      2: begin FSource.Text := 'source'; FDestination.Text := ''; end;
    end;
    OriginalText := FDestination.Text;
    Parsed := Flow.ParseResponse(' ' + LineEnding, FSource);
    try
      AssertTrue('Empty translation must fail for a non-empty side', Parsed.ParseError <> '');
      AssertApplicationRejected(Flow, Parsed, FSource);
      AssertEquals(OriginalText, FDestination.Text);
      AssertNoPatch(Parsed, '');
    finally
      Parsed.Free;
    end;
  end;
end;

procedure TAIResponseValidationTests.TranslationApplyRejectsEmptyResultWithoutParseContext;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  Combined: Boolean;
  OriginalText: string;
begin
  OriginalText := FDestination.Text;
  for Combined := False to True do
  begin
    Flow := TAITranslationFlow.Create(DEFAULT_TRANSLATION_PROMPT, True, Combined);
    if Combined then
      Parsed := Flow.ParseResponse('{"translation":"","data":[]}', nil)
    else
      Parsed := Flow.ParseResponse('', nil);
    try
      AssertEquals('Parsing without verse context may allow a neutral empty value', '', Parsed.ParseError);
      AssertApplicationRejected(Flow, Parsed, FSource);
      AssertEquals(OriginalText, FDestination.Text);
    finally
      Parsed.Free;
    end;
  end;
end;

procedure TAIResponseValidationTests.TranslationPlainEmptyVersePairIsNeutral;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
begin
  FSource.Text := ''; FDestination.Text := '';
  Flow := TAITranslationFlow.Create(DEFAULT_TRANSLATION_PROMPT, False, False);
  Parsed := Flow.ParseResponse('', FSource);
  try
    AssertEquals('', Parsed.ParseError);
    Flow.ApplyResult(Parsed, FSource);
    AssertNoPatch(Parsed, '');
  finally
    Parsed.Free;
  end;
end;

procedure TAIResponseValidationTests.TranslationCombinedRejectsEmptyWithExistingDestination;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  OriginalText: string;
begin
  FSource.Text := '';
  OriginalText := FDestination.Text;
  Flow := TAITranslationFlow.Create(DEFAULT_TRANSLATION_PROMPT, True, True);
  Parsed := Flow.ParseResponse('{"translation":"","data":[]}', FSource);
  try
    AssertTrue(Parsed.ParseError <> '');
    AssertApplicationRejected(Flow, Parsed, FSource);
    AssertEquals(OriginalText, FDestination.Text);
    AssertNoPatch(Parsed, '');
  finally
    Parsed.Free;
  end;
end;

procedure TAIResponseValidationTests.TranslationCombinedFailureRestoresExistingTextAndPairs;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  OriginalText, OriginalPairs: string;
begin
  AddOriginalAssociation;
  OriginalText := FDestination.Text;
  OriginalPairs := FSource.Pairs;
  Flow := TAITranslationFlow.Create(DEFAULT_TRANSLATION_PROMPT, True, True);
  Parsed := Flow.ParseResponse(
    '{"translation":"new destination","data":[["our","new"]]}', FSource);
  try
    AssertEquals('Literal substring parses but is not a selectable syntagm', '', Parsed.ParseError);
    AssertApplicationRejected(Flow, Parsed, FSource);
    AssertEquals(OriginalText, FDestination.Text);
    AssertEquals(OriginalPairs, FSource.Pairs);
    AssertNoPatch(Parsed, OriginalPairs);
  finally
    Parsed.Free;
  end;
end;

procedure TAIResponseValidationTests.EmptyResponsePreservesDestinationNotesAndMetadata;
const
  DestinationTexts: array[0..2] of string = (
    '<RF>stored note<Rf>', '<WG123>', '<FI><Fi>');
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  TextIndex, FlowIndex: Integer;
  Response: string;
begin
  FSource.Text := '';
  for TextIndex := Low(DestinationTexts) to High(DestinationTexts) do
  begin
    FDestination.Text := DestinationTexts[TextIndex];
    for FlowIndex := 0 to 2 do
    begin
      case FlowIndex of
        0: begin
          Flow := TAIAssociationFlow.Create;
          Response := '{"data":[]}';
        end;
        1: begin
          Flow := TAITranslationFlow.Create(DEFAULT_TRANSLATION_PROMPT, True, False);
          Response := '';
        end;
        2: begin
          Flow := TAITranslationFlow.Create(DEFAULT_TRANSLATION_PROMPT, True, True);
          Response := '{"translation":"","data":[]}';
        end;
      end;
      Parsed := Flow.ParseResponse(Response, FSource);
      try
        AssertTrue('Raw notes and metadata must not qualify as an empty destination',
          Parsed.ParseError <> '');
        AssertApplicationRejected(Flow, Parsed, FSource);
        AssertEquals(DestinationTexts[TextIndex], FDestination.Text);
        AssertNoPatch(Parsed, '');
      finally
        Parsed.Free;
      end;

      Parsed := Flow.ParseResponse(Response, nil);
      try
        AssertEquals('An empty envelope can parse without verse context', '', Parsed.ParseError);
        AssertApplicationRejected(Flow, Parsed, FSource);
        AssertEquals(DestinationTexts[TextIndex], FDestination.Text);
      finally
        Parsed.Free;
      end;
    end;
  end;
end;

initialization
  RegisterTest(TAIResponseValidationTests);
end.
