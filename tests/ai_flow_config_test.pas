unit ai_flow_config_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ai_flows, ai_flow_config,
  ai_association_flow;

type
  TAIFlowConfigTests = class(TTestCase)
  published
    procedure ExamplesRoundTripPreservesPairsXml;
    procedure LegacyExamplesWithoutPairsXmlStillLoad;
    procedure AssociationPromptUsesExpectedResponseNotPairsXml;
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
