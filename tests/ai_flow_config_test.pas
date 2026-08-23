unit ai_flow_config_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ai_flows, ai_flow_config,
  ai_association_flow, ai_translation_flow, Verse;

type
  TAIFlowConfigTests = class(TTestCase)
  published
    procedure ExamplesRoundTripPreservesPairsXml;
    procedure LegacyExamplesWithoutPairsXmlStillLoad;
    procedure AssociationPromptUsesExpectedResponseNotPairsXml;
    procedure TranslationConfigDefaultsIncludeAssociationsToFalse;
    procedure TranslationPromptWithoutAssociationsStaysPlainText;
    procedure TranslationPromptWithAssociationsUsesCombinedContract;
    procedure TranslationCombinedParserAcceptsDirectAndMarkdownJson;
    procedure TranslationCombinedParserRejectsInvalidEnvelopes;
    procedure TranslationCombinedParserRejectsNonLiteralDestinations;
    procedure TranslationCombinedParserRejectsNonLiteralSources;
    procedure TranslationCombinedParserRejectsEmptyForNonEmptySource;
    procedure TranslationCombinedApplyPublishesTextAndPairs;
    procedure TranslationCombinedApplyRollsBackWhenNoPairMatches;
    procedure TranslationApplyRespectsForceOverwrite;
    procedure TranslationOverwriteClearsPairsOnlyWhenTextChanges;
    procedure TranslationEligibilityRespectsForceOverwrite;
    procedure MoveFlowExampleChangesOrder;
  end;

implementation

procedure TAIFlowConfigTests.ExamplesRoundTripPreservesPairsXml;
var
  Examples, Loaded: TAIFlowExampleArray;
  Serialized: string;
begin
  SetLength(Examples, 1);
  Examples[0].ReferenceId := '45,1,1';
  Examples[0].ReferenceCaption := 'Romans 1:1';
  Examples[0].SourceText := 'Paulos doulos';
  Examples[0].TargetText := 'Paulo servo';
  Examples[0].SourceXml := '<WG>Paulos<Wg> doulos';
  Examples[0].TargetXml := 'Paulo <WG>servo<Wg>';
  Examples[0].ExpectedResponse := '{"data":[["Paulos","Paulo"]]}';
  Examples[0].PairsXml := '<par a="0" b="0">';

  Serialized := SaveFlowExamples(Examples);
  Loaded := LoadFlowExamples(Serialized);

  AssertEquals(1, Length(Loaded));
  AssertEquals(Examples[0].ReferenceId, Loaded[0].ReferenceId);
  AssertEquals(Examples[0].ExpectedResponse, Loaded[0].ExpectedResponse);
  AssertEquals(Examples[0].SourceXml, Loaded[0].SourceXml);
  AssertEquals(Examples[0].TargetXml, Loaded[0].TargetXml);
  AssertEquals(Examples[0].PairsXml, Loaded[0].PairsXml);
end;

procedure TAIFlowConfigTests.LegacyExamplesWithoutPairsXmlStillLoad;
var
  Loaded: TAIFlowExampleArray;
begin
  Loaded := LoadFlowExamples(
    '[{"reference_id":"45,1,1","reference_caption":"Romans 1:1",' +
    '"source_text":"source","target_text":"target","expected_response":"response"}]');

  AssertEquals(1, Length(Loaded));
  AssertEquals('45,1,1', Loaded[0].ReferenceId);
  AssertEquals('response', Loaded[0].ExpectedResponse);
  AssertEquals('', Loaded[0].SourceXml);
  AssertEquals('', Loaded[0].TargetXml);
  AssertEquals('', Loaded[0].PairsXml);
end;

procedure TAIFlowConfigTests.AssociationPromptUsesExpectedResponseNotPairsXml;
var
  Examples: TAIFlowExampleArray;
  Serialized, Prompt: string;
  FlowConfig: IAIFlowConfig;
begin
  SetLength(Examples, 1);
  Examples[0].ReferenceId := '45,1,1';
  Examples[0].ReferenceCaption := 'Romans 1:1';
  Examples[0].SourceText := 'alpha';
  Examples[0].TargetText := 'beta';
  Examples[0].SourceXml := '<WG>source-xml-secret<Wg>';
  Examples[0].TargetXml := '<WG>target-xml-secret<Wg>';
  Examples[0].ExpectedResponse := '{"data":[["alpha","beta"]]}';
  Examples[0].PairsXml := '<par a="0" b="0">visual-only-secret</par>';

  Serialized := SaveFlowExamples(Examples);
  FlowConfig := TAIAssociationFlowConfig.Create;
  Prompt := FlowConfig.BuildPromptFromConfig([
    AI_ASSOCIATION_EXAMPLES_KEY, Serialized
  ]);

  AssertTrue(Pos(Examples[0].ExpectedResponse, Prompt) > 0);
  AssertEquals(0, Pos('pairs_xml', Prompt));
  AssertEquals(0, Pos('source_xml', Prompt));
  AssertEquals(0, Pos('target_xml', Prompt));
  AssertEquals(0, Pos('source-xml-secret', Prompt));
  AssertEquals(0, Pos('target-xml-secret', Prompt));
  AssertEquals(0, Pos('visual-only-secret', Prompt));
end;


procedure TAIFlowConfigTests.TranslationConfigDefaultsIncludeAssociationsToFalse;
var
  FlowConfig: IAIFlowConfig;
  Fields: TAIFlowConfigFieldArray;
  I: Integer;
  Found: Boolean;
begin
  FlowConfig := TAITranslationFlowConfig.Create;
  Fields := FlowConfig.GetConfigFields;
  Found := False;

  for I := 0 to High(Fields) do
    if Fields[I].Key = AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS then
    begin
      Found := True;
      AssertEquals('boolean', Fields[I].FieldType);
      AssertEquals('false', LowerCase(Fields[I].DefaultValue));
      AssertFalse(Fields[I].Required);
    end;

  AssertTrue('Translation include-associations config field was not exposed', Found);
end;

procedure TAIFlowConfigTests.TranslationPromptWithoutAssociationsStaysPlainText;
var
  FlowConfig: IAIFlowConfig;
  Prompt: string;
begin
  FlowConfig := TAITranslationFlowConfig.Create;
  Prompt := FlowConfig.BuildPromptFromConfig([
    AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS, False,
    AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS,
      'association-instruction-must-be-ignored',
    AI_ASSOCIATION_EXAMPLES_KEY, '[]'
  ]);

  AssertEquals(DEFAULT_TRANSLATION_PROMPT, Prompt);
  AssertTrue(Pos('Return ONLY the translated text', Prompt) > 0);
  AssertEquals(0, Pos('"translation"', Prompt));
  AssertEquals(0, Pos('association-instruction-must-be-ignored', Prompt));
end;

procedure TAIFlowConfigTests.TranslationPromptWithAssociationsUsesCombinedContract;
var
  TranslationExamples, AssociationExamples: TAIFlowExampleArray;
  TranslationSerialized, AssociationSerialized, Prompt: string;
  FlowConfig: IAIFlowConfig;
begin
  SetLength(TranslationExamples, 1);
  TranslationExamples[0].ReferenceId := '1,1,1';
  TranslationExamples[0].ReferenceCaption := 'Example translation';
  TranslationExamples[0].SourceText := 'translation-source-secret';
  TranslationExamples[0].ExpectedResponse := 'translation-target-secret';

  SetLength(AssociationExamples, 1);
  AssociationExamples[0].ReferenceId := '1,1,2';
  AssociationExamples[0].ReferenceCaption := 'Example association';
  AssociationExamples[0].SourceText := 'association-source-secret';
  AssociationExamples[0].TargetText := 'association-target-secret';
  AssociationExamples[0].ExpectedResponse :=
    '{"data":[["association-source-secret","association-target-secret"]],' +
    '"comments":"association-comment-secret"}';

  TranslationSerialized := SaveFlowExamples(TranslationExamples);
  AssociationSerialized := SaveFlowExamples(AssociationExamples);
  FlowConfig := TAITranslationFlowConfig.Create;
  Prompt := FlowConfig.BuildPromptFromConfig([
    AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE, 'Brazilian Portuguese',
    AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS,
      'translation-instruction-secret',
    AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS, True,
    AI_TRANSLATION_EXAMPLES_KEY, TranslationSerialized,
    AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS,
      'association-instruction-secret',
    AI_ASSOCIATION_EXAMPLES_KEY, AssociationSerialized,
    AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS, True,
    AI_ASSOCIATION_CONFIG_KEY_PROVIDER, 'association-provider-secret',
    AI_ASSOCIATION_CONFIG_KEY_MODEL, 'association-model-secret',
    AI_ASSOCIATION_CONFIG_KEY_API_KEY, 'association-api-key-secret'
  ]);

  AssertTrue(Pos('"translation"', Prompt) > 0);
  AssertTrue(Pos('"data"', Prompt) > 0);
  AssertTrue(Pos('translation-instruction-secret', Prompt) > 0);
  AssertTrue(Pos('association-instruction-secret', Prompt) > 0);
  AssertTrue(Pos('translation-source-secret', Prompt) > 0);
  AssertTrue(Pos('translation-target-secret', Prompt) > 0);
  AssertTrue(Pos('association-source-secret', Prompt) > 0);
  AssertTrue(Pos('association-target-secret', Prompt) > 0);
  AssertTrue(Pos('Expected "data" value:', Prompt) > 0);
  AssertEquals(0, Pos('Expected response:', Prompt));
  AssertEquals(0, Pos('"comments"', LowerCase(Prompt)));
  AssertEquals(0, Pos('association-provider-secret', Prompt));
  AssertEquals(0, Pos('association-model-secret', Prompt));
  AssertEquals(0, Pos('association-api-key-secret', Prompt));
  AssertEquals(0, Pos('association-comment-secret', Prompt));
end;

procedure TAIFlowConfigTests.TranslationCombinedParserAcceptsDirectAndMarkdownJson;
var
  Parsed: TAITranslationResult;
  Json: string;
begin
  Json :=
    '{"translation":"In the beginning","data":' +
    '[["source one","In the"],["source two","beginning"]]}';

  Parsed := ParseTranslationWithAssociationsJson(Json);
  try
    AssertEquals('', Parsed.ParseError);
    AssertEquals('In the beginning', Parsed.TranslatedText);
    AssertEquals(2, Length(Parsed.Pairs));
    AssertEquals('source one', Parsed.Pairs[0].OriginalPhrase);
    AssertEquals('In the', Parsed.Pairs[0].TranslatedPhrase);
  finally
    Parsed.Free;
  end;

  Parsed := ParseTranslationWithAssociationsJson(
    '```json' + LineEnding + Json + LineEnding + '```');
  try
    AssertEquals('', Parsed.ParseError);
    AssertEquals('In the beginning', Parsed.TranslatedText);
    AssertEquals(2, Length(Parsed.Pairs));
  finally
    Parsed.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationCombinedParserRejectsInvalidEnvelopes;

  procedure AssertInvalid(const Json, FailureMessage: string);
  var
    Parsed: TAITranslationResult;
  begin
    Parsed := ParseTranslationWithAssociationsJson(Json);
    try
      AssertTrue(FailureMessage, Trim(Parsed.ParseError) <> '');
      AssertFalse(Parsed.AcceptedDestinationTextSet);
      AssertFalse(Parsed.AcceptedPairsSet);
    finally
      Parsed.Free;
    end;
  end;

begin
  AssertInvalid('[]', 'Top-level arrays must be rejected');
  AssertInvalid(
    '{"data":[["source","target"]]}',
    'Missing translation must be rejected');
  AssertInvalid(
    '{"translation":123,"data":[["source","target"]]}',
    'Non-string translation must be rejected');
  AssertInvalid(
    '{"translation":"target","data":{}}',
    'Non-array data must be rejected');
  AssertInvalid(
    '{"translation":"target","data":[]}',
    'Non-empty translation without pairs must be rejected');
  AssertInvalid(
    '{"translation":"target","data":[["","target"],["source",1]]}',
    'An envelope without any structurally valid pair must be rejected');
end;

procedure TAIFlowConfigTests.TranslationCombinedParserRejectsNonLiteralDestinations;
var
  Parsed: TAITranslationResult;
begin
  Parsed := ParseTranslationWithAssociationsJson(
    '{"translation":"literal target","data":' +
    '[["ignored source","not present"],["kept source","target"]]}');
  try
    AssertEquals('', Parsed.ParseError);
    AssertEquals(1, Length(Parsed.Pairs));
    AssertEquals('kept source', Parsed.Pairs[0].OriginalPhrase);
    AssertEquals('target', Parsed.Pairs[0].TranslatedPhrase);
  finally
    Parsed.Free;
  end;

  Parsed := ParseTranslationWithAssociationsJson(
    '{"translation":"literal target","data":' +
    '[["source","not present"]]}');
  try
    AssertTrue(Trim(Parsed.ParseError) <> '');
    AssertFalse(Parsed.AcceptedDestinationTextSet);
    AssertFalse(Parsed.AcceptedPairsSet);
  finally
    Parsed.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationCombinedParserRejectsNonLiteralSources;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  TranslationResult: TAITranslationResult;
  SourceVerse: TVerse;
begin
  SourceVerse := TVerse.Create;
  Parsed := nil;
  try
    SourceVerse.Text := 'literal source';
    Flow := TAITranslationFlow.Create(
      DEFAULT_TRANSLATION_PROMPT, False, True);

    Parsed := Flow.ParseResponse(
      '{"translation":"literal target","data":' +
      '[["invented source","literal target"]]}', SourceVerse);
    AssertTrue(Trim(Parsed.ParseError) <> '');
    AssertFalse(Parsed.AcceptedDestinationTextSet);
    AssertFalse(Parsed.AcceptedPairsSet);
    FreeAndNil(Parsed);

    Parsed := Flow.ParseResponse(
      '{"translation":"literal target","data":' +
      '[["invented source","literal"],["source","target"]]}',
      SourceVerse);
    AssertEquals('', Parsed.ParseError);
    AssertTrue(Parsed is TAITranslationResult);
    TranslationResult := TAITranslationResult(Parsed);
    AssertEquals(1, Length(TranslationResult.Pairs));
    AssertEquals('source', TranslationResult.Pairs[0].OriginalPhrase);
    AssertEquals('target', TranslationResult.Pairs[0].TranslatedPhrase);
  finally
    Parsed.Free;
    Flow := nil;
    SourceVerse.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationCombinedParserRejectsEmptyForNonEmptySource;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  SourceVerse: TVerse;
begin
  SourceVerse := TVerse.Create;
  Parsed := nil;
  try
    SourceVerse.Text := 'non-empty source';
    Flow := TAITranslationFlow.Create(
      DEFAULT_TRANSLATION_PROMPT, False, True);
    Parsed := Flow.ParseResponse(
      '{"translation":"","data":[]}', SourceVerse);

    AssertTrue(Trim(Parsed.ParseError) <> '');
    AssertFalse(Parsed.AcceptedDestinationTextSet);
    AssertFalse(Parsed.AcceptedPairsSet);
  finally
    Parsed.Free;
    Flow := nil;
    SourceVerse.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationCombinedApplyPublishesTextAndPairs;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  SourceVerse, DestinationVerse: TVerse;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  Parsed := nil;
  try
    SourceVerse.Text := 'source one source two';
    DestinationVerse.Text := '';
    SourceVerse.PairedVerse := DestinationVerse;
    DestinationVerse.PairedVerse := SourceVerse;

    Flow := TAITranslationFlow.Create(
      DEFAULT_TRANSLATION_PROMPT, False, True);
    Parsed := Flow.ParseResponse(
      '{"translation":"target one target two","data":' +
      '[["source one","target one"],["source two","target two"]]}',
      SourceVerse);
    AssertEquals('', Parsed.ParseError);

    Flow.ApplyResult(Parsed, SourceVerse);

    AssertTrue(Parsed.AcceptedDestinationTextSet);
    AssertEquals('target one target two', Parsed.AcceptedDestinationText);
    AssertTrue(Parsed.AcceptedPairsSet);
    AssertTrue(Trim(Parsed.AcceptedPairs) <> '');
    AssertTrue(Pos('<par ', LowerCase(Parsed.AcceptedPairs)) > 0);
    AssertFalse(Parsed.AcceptedSourceTextSet);
    AssertFalse(Parsed.AcceptedCommentsSet);
    AssertFalse(Parsed.AcceptedStatusSet);
    AssertEquals('source one source two', SourceVerse.PlainText);
    AssertEquals('target one target two', DestinationVerse.PlainText);
  finally
    Parsed.Free;
    Flow := nil;
    SourceVerse.PairedVerse := nil;
    DestinationVerse.PairedVerse := nil;
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationCombinedApplyRollsBackWhenNoPairMatches;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  SourceVerse, DestinationVerse: TVerse;
  Failed: Boolean;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  Parsed := nil;
  try
    SourceVerse.Text := 'source text';
    DestinationVerse.Text := 'old destination';
    SourceVerse.PairedVerse := DestinationVerse;
    DestinationVerse.PairedVerse := SourceVerse;

    Flow := TAITranslationFlow.Create(
      DEFAULT_TRANSLATION_PROMPT, True, True);
    Parsed := Flow.ParseResponse(
      '{"translation":"new destination","data":' +
      '[["our","new"]]}',
      SourceVerse);
    AssertEquals('', Parsed.ParseError);

    Failed := False;
    try
      Flow.ApplyResult(Parsed, SourceVerse);
    except
      on E: Exception do
        Failed := True;
    end;

    AssertTrue('A combined proposal with no materialized pair must fail', Failed);
    AssertEquals('old destination', DestinationVerse.PlainText);
    AssertFalse(Parsed.AcceptedDestinationTextSet);
    AssertFalse(Parsed.AcceptedPairsSet);
  finally
    Parsed.Free;
    Flow := nil;
    SourceVerse.PairedVerse := nil;
    DestinationVerse.PairedVerse := nil;
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationApplyRespectsForceOverwrite;
var
  Flow: IAIVerseFlow;
  Parsed: TAIFlowResult;
  SourceVerse, DestinationVerse: TVerse;
  Failed: Boolean;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  Parsed := nil;
  try
    SourceVerse.Text := 'source text';
    DestinationVerse.Text := 'existing destination';
    SourceVerse.PairedVerse := DestinationVerse;
    DestinationVerse.PairedVerse := SourceVerse;

    Flow := TAITranslationFlow.Create(
      DEFAULT_TRANSLATION_PROMPT, False, False);
    Parsed := Flow.ParseResponse('replacement destination', SourceVerse);

    Failed := False;
    try
      Flow.ApplyResult(Parsed, SourceVerse);
    except
      on E: Exception do
        Failed := True;
    end;

    AssertTrue('Non-forced overwrite must fail defensively', Failed);
    AssertEquals('existing destination', DestinationVerse.PlainText);
    AssertFalse(Parsed.AcceptedDestinationTextSet);
    AssertFalse(Parsed.AcceptedPairsSet);
  finally
    Parsed.Free;
    Flow := nil;
    SourceVerse.PairedVerse := nil;
    DestinationVerse.PairedVerse := nil;
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationOverwriteClearsPairsOnlyWhenTextChanges;
var
  Flow: IAIVerseFlow;
  ChangedResult, UnchangedResult: TAIFlowResult;
  SourceVerse, DestinationVerse: TVerse;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  ChangedResult := nil;
  UnchangedResult := nil;
  try
    SourceVerse.Text := 'source text';
    DestinationVerse.Text := 'old destination';
    SourceVerse.PairedVerse := DestinationVerse;
    DestinationVerse.PairedVerse := SourceVerse;
    Flow := TAITranslationFlow.Create(
      DEFAULT_TRANSLATION_PROMPT, True, False);

    ChangedResult := Flow.ParseResponse('new destination', SourceVerse);
    Flow.ApplyResult(ChangedResult, SourceVerse);
    AssertTrue(ChangedResult.AcceptedDestinationTextSet);
    AssertTrue(ChangedResult.AcceptedPairsSet);
    AssertEquals('', ChangedResult.AcceptedPairs);

    DestinationVerse.Text := 'same destination';
    UnchangedResult := Flow.ParseResponse('same destination', SourceVerse);
    Flow.ApplyResult(UnchangedResult, SourceVerse);
    AssertTrue(UnchangedResult.AcceptedDestinationTextSet);
    AssertFalse(UnchangedResult.AcceptedPairsSet);
  finally
    UnchangedResult.Free;
    ChangedResult.Free;
    Flow := nil;
    SourceVerse.PairedVerse := nil;
    DestinationVerse.PairedVerse := nil;
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TAIFlowConfigTests.TranslationEligibilityRespectsForceOverwrite;
begin
  AssertTrue(IsTranslationDestinationEligible('', False));
  AssertTrue(IsTranslationDestinationEligible('   ', False));
  AssertFalse(IsTranslationDestinationEligible('existing destination', False));
  AssertTrue(IsTranslationDestinationEligible('existing destination', True));
end;

procedure TAIFlowConfigTests.MoveFlowExampleChangesOrder;
var
  Examples, Moved: TAIFlowExampleArray;
begin
  SetLength(Examples, 3);
  Examples[0].ReferenceId := 'a';
  Examples[1].ReferenceId := 'b';
  Examples[2].ReferenceId := 'c';

  Moved := MoveFlowExample(Examples, 2, 0);

  AssertEquals('c', Moved[0].ReferenceId);
  AssertEquals('a', Moved[1].ReferenceId);
  AssertEquals('b', Moved[2].ReferenceId);
end;

initialization
  RegisterTest(TAIFlowConfigTests);

end.
