unit ai_association_flow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, Verse, Syntagm, ONTParser, ONTTokenizer,
  fpjson, jsonparser, llmclient, ai_debug;

const
  // Prompt components to keep it DRY
  ASSOCIATION_PROMPT_HEADER = 
     'You are a biblical text association expert. Your task is to identify and match corresponding words between an original biblical text and its translation.';
  
  ASSOCIATION_PROMPT_RULES = 
    'Rules:' + LineEnding +
    '1. Prioritize word-for-word correspondences whenever possible' + LineEnding +
    '2. Only use multi-word phrases or expressions when a 1:1 word match is not viable (e.g., for idiomatic translations or specific grammatical constructions)' + LineEnding +
    '3. Consider semantic equivalence, not just literal matches' + LineEnding +
    '4. Respect the grammatical and syntactic structure of both languages' + LineEnding +
    '5. Handle reordering common in translations (e.g., adjective placement)' + LineEnding +
    '6. Consider idiomatic expressions and their translated equivalents';

  ASSOCIATION_PROMPT_FORMAT_HEADER = 
    'Output Format:' + LineEnding +
    'Return a JSON object with this exact structure:';

  ASSOCIATION_PROMPT_FOOTER = 
    'Each pair in "data" must contain exactly 2 strings.' + LineEnding +
    'If you cannot find a clear correspondence for a word, you may omit it from the pairs.' + LineEnding +
    'Focus on accuracy over completeness.';

  DEFAULT_ASSOCIATION_PROMPT =
    ASSOCIATION_PROMPT_HEADER + LineEnding + LineEnding +
    ASSOCIATION_PROMPT_RULES + LineEnding + LineEnding +
    ASSOCIATION_PROMPT_FORMAT_HEADER + LineEnding +
    '{' + LineEnding +
    '  "data": [["word_or_phrase_from_original", "word_or_phrase_from_translation"], ...]' + LineEnding +
    '}' + LineEnding + LineEnding +
    ASSOCIATION_PROMPT_FOOTER;
    
  // Configuration keys for this flow
  AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS = 'ai.flow.association.custom_instructions';
  AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS = 'ai.flow.association.include_comments';
  AI_ASSOCIATION_CONFIG_KEY_MODEL = 'ai.flow.association.model';
  AI_ASSOCIATION_CONFIG_KEY_API_KEY = 'ai.flow.association.api_key';
  AI_ASSOCIATION_CONFIG_KEY_PROVIDER = 'ai.flow.association.provider';
  
  // Default customizable instructions (shown to user)
  DEFAULT_ASSOCIATION_CUSTOM_INSTRUCTIONS =
    'Additional guidelines (optional):' + LineEnding +
    LineEnding +
    '- Add specific rules for your translation pairs' + LineEnding +
    '- Provide examples for edge cases' + LineEnding +
    '- Describe special handling for certain text types';

type
  // Structured association pair
  TAssociationPair = record
    OriginalPhrase: string;
    TranslatedPhrase: string;
  end;

  TAssociationResult = class(TAIFlowResult)
  public
    Pairs: array of TAssociationPair;
    Comments: string;
    PairsXML: string;  // XML representation of pairs after associations are applied (populated by ApplyResult)
  end;
  
  { TAIAssociationFlowConfig - Configuration provider for association flow }
  TAIAssociationFlowConfig = class(TInterfacedObject, IAIFlowConfig)
  public
    function GetFlowId: string;
    function GetDisplayName: string;
    function GetDescription: string;
    function GetOutputFormatInfo: string;
    function GetDefaultCustomInstructions: string;
    function GetConfigFields: TAIFlowConfigFieldArray;
    function BuildPromptFromConfig(const ConfigValues: array of const): string;
    function CreateFlowInstance(Context: IAIFlowContext): IAIVerseFlow;
  end;

  { TAIAssociationFlow }
  TAIAssociationFlow = class(TInterfacedObject, IAIVerseFlow)
  private
    FPrompt: string;
    FIncludeComments: Boolean;
    FProviderOverride: string;
    FModelOverride: string;
    FApiKeyOverride: string;
  protected
    function Name: string;
    procedure PrepareRequest(Verse: TVerse; out Messages: array of TLLMMessage); overload;
    function BuildMessages(Verse: TVerse): TLLMMessageArray;
    function ParseResponse(const RawContent: string; Verse: TVerse): TAIFlowResult;
    procedure ApplyResult(ResultObj: TAIFlowResult; Verse: TVerse);
    function RequiresIndividualClient: Boolean;
    function ModelOverride: string;
    function ApiKeyOverride: string;
    function ProviderOverride: string;
  public
    constructor Create(const APrompt: string = DEFAULT_ASSOCIATION_PROMPT; 
                       AIncludeComments: Boolean = False;
                       const AProvider: string = '';
                       const AModel: string = '';
                       const AApiKey: string = '');
  end;

  // Helper to parse raw json into TAssociationResult (pure)
  function ParseAssociationJson(const RawJson: string): TAssociationResult; // Parses expected JSON structure
  function ExtractJsonFromMarkdown(const RawContent: string): string;
  function BuildAssociationPromptSection(const CustomInstructions,
    SerializedExamples, OutputObject: string;
    ExamplesAsDataValue: Boolean = False): string;
  function ApplyAssociationPairs(Association: TAssociationResult;
    Verse: TVerse): Integer;

resourcestring
  SDisplayNameAssociation = 'Association';
  SDescriptionAssociation = 'Matches corresponding phrases between original and translated texts';
  SOutputFormatInfoAssociation = 'This flow expects JSON output: {"data": [[original, translation], ...], "comments": "..." (optional)}';
  SConfigAdditionalInstructionsDisplayName = 'Additional Instructions';
  SConfigAdditionalInstructionsDescription = 'Add specific guidelines for your project (e.g., handling of poetry, examples for edge cases)';
  SConfigIncludeCommentsDisplayName = 'Include AI Comments';
  SConfigIncludeCommentsDescription = 'If enabled, the AI will provide critical comments about the associations.';
  SConfigModelOverrideDisplayName = 'Model';
  SConfigModelOverrideDescription = 'Specify a different model for this flow. Leave empty to use global setting.';
  SConfigApiKeyOverrideDisplayName = 'API Key';
  SConfigApiKeyOverrideDescription = 'Specify a different API key for this flow. Leave empty to use global setting.';
  SConfigProviderOverrideDisplayName = 'Provider';
  SConfigProviderOverrideDescription = 'Specify a different provider for this flow. Leave empty to use global setting.';

implementation

uses
  ai_flow_config;

function FormatAssociationExamplesPromptSection(const Examples: TAIFlowExampleArray): string;
var
  I: Integer;
begin
  Result := '';
  if Length(Examples) = 0 then
    Exit;

  Result := '=== Association Examples ===' + LineEnding +
            'Use these examples as guidance for the expected matching style and JSON output.';
  for I := 0 to High(Examples) do
  begin
    Result := Result + LineEnding + LineEnding +
              Format('Example %d - %s', [I + 1, Examples[I].ReferenceCaption]) + LineEnding +
              Format('Original: "%s"', [Examples[I].SourceText]) + LineEnding +
              Format('Translation: "%s"', [Examples[I].TargetText]) + LineEnding +
              'Expected response: ' + Examples[I].ExpectedResponse;
  end;
end;

function ExtractAssociationDataValue(const ExpectedResponse: string): string;
var
  Parser: TJSONParser;
  Data, DataNode: TJSONData;
begin
  Result := '[]';
  Data := nil;
  try
    try
      Parser := TJSONParser.Create(
        ExtractJsonFromMarkdown(ExpectedResponse), []);
      try
        Data := Parser.Parse;
      finally
        Parser.Free;
      end;
      if Data is TJSONObject then
      begin
        DataNode := TJSONObject(Data).Find('data');
        if DataNode is TJSONArray then
          Result := DataNode.AsJSON;
      end;
    except
      on E: Exception do
        AIDebugLogFmt('[Association] Invalid configured example ignored: %s',
          [E.Message]);
    end;
  finally
    Data.Free;
  end;
end;

function FormatAssociationDataExamplesPromptSection(
  const Examples: TAIFlowExampleArray): string;
var
  I: Integer;
begin
  Result := '';
  if Length(Examples) = 0 then
    Exit;

  Result := '=== Association Examples ===' + LineEnding +
    'Use these examples as guidance for the value of the "data" field.';
  for I := 0 to High(Examples) do
    Result := Result + LineEnding + LineEnding +
      Format('Example %d - %s', [I + 1, Examples[I].ReferenceCaption]) +
      LineEnding +
      Format('Original: "%s"', [Examples[I].SourceText]) + LineEnding +
      Format('Translation: "%s"', [Examples[I].TargetText]) + LineEnding +
      'Expected "data" value: ' +
      ExtractAssociationDataValue(Examples[I].ExpectedResponse);
end;

function BuildAssociationPromptSection(const CustomInstructions,
  SerializedExamples, OutputObject: string;
  ExamplesAsDataValue: Boolean): string;
var
  ExamplesSection: string;
begin
  Result :=
    ASSOCIATION_PROMPT_HEADER + LineEnding + LineEnding +
    ASSOCIATION_PROMPT_RULES + LineEnding + LineEnding +
    ASSOCIATION_PROMPT_FORMAT_HEADER + LineEnding +
    OutputObject + LineEnding + LineEnding +
    ASSOCIATION_PROMPT_FOOTER;

  if Trim(CustomInstructions) <> '' then
    Result := Result + LineEnding + LineEnding +
      '=== Additional Project-Specific Instructions ===' + LineEnding +
      Trim(CustomInstructions);

  if ExamplesAsDataValue then
    ExamplesSection := FormatAssociationDataExamplesPromptSection(
      LoadFlowExamples(SerializedExamples))
  else
    ExamplesSection := FormatAssociationExamplesPromptSection(
      LoadFlowExamples(SerializedExamples));
  if ExamplesSection <> '' then
    Result := Result + LineEnding + LineEnding + ExamplesSection;
end;

function ExtractJsonFromMarkdown(const RawContent: string): string;
var
  StartPos, EndPos: Integer;
  Content: string;
begin
  Content := Trim(RawContent);
  
  // Check if content starts with ```json or just ```
  if (Pos('```json', Content) = 1) or (Pos('```', Content) = 1) then
  begin
    // Find the first newline after the opening ```
    StartPos := Pos(LineEnding, Content);
    if StartPos = 0 then
      StartPos := Pos(#10, Content); // Try Unix line ending
    if StartPos = 0 then
      StartPos := Pos(#13, Content); // Try Mac line ending
      
    if StartPos > 0 then
    begin
      Inc(StartPos); // Move past the newline
      
      // Find the closing ```
      EndPos := Pos('```', Content, StartPos);
      if EndPos > StartPos then
      begin
        Result := Trim(Copy(Content, StartPos, EndPos - StartPos));
        Exit;
      end;
    end;
  end;
  
  // If no markdown tags found, return original content
  Result := Content;
end;

function ParseAssociationJson(const RawJson: string): TAssociationResult;
var
  Parser: TJSONParser;
  Data: TJSONData;
  Obj: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  PArr: TJSONArray;
  Pair: TAssociationPair;
  CommentsNode: TJSONData;
  CleanJson: string;
begin
  Result := TAssociationResult.Create;
  Data := nil;
  try
    try
    // Extract JSON from markdown code blocks if present
    CleanJson := ExtractJsonFromMarkdown(RawJson);
    
    // Log if extraction occurred
    if CleanJson <> RawJson then
      AIDebugLog('[Association] Extracted JSON from markdown code block');
    
    Parser := TJSONParser.Create(CleanJson, []);
    try
      Data := Parser.Parse;
    finally
      Parser.Free;
    end;
    if (Data <> nil) and (Data.JSONType = jtObject) then
    begin
      Obj := TJSONObject(Data);
      CommentsNode := Obj.Find('comments');
      if Assigned(CommentsNode) and (CommentsNode.JSONType = jtString) then
        Result.Comments := CommentsNode.AsString;
      if (Obj.Find('data') <> nil) and (Obj.Elements['data'] is TJSONArray) then
      begin
        Arr := TJSONArray(Obj.Elements['data']);
        for I := 0 to Arr.Count - 1 do
        begin
          if Arr.Items[I] is TJSONArray then
          begin
            PArr := TJSONArray(Arr.Items[I]);
            if (PArr.Count = 2) and (PArr.Items[0] is TJSONString) and (PArr.Items[1] is TJSONString) then
            begin
              Pair.OriginalPhrase := TJSONString(PArr.Items[0]).Value;
              Pair.TranslatedPhrase := TJSONString(PArr.Items[1]).Value;
              SetLength(Result.Pairs, Length(Result.Pairs)+1);
              Result.Pairs[High(Result.Pairs)] := Pair;
            end;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Result.ParseError := E.Message;
    end;
  end;
  finally
    Data.Free;
  end;
end;

{ TAIAssociationFlowConfig }

function TAIAssociationFlowConfig.GetFlowId: string;
begin
  Result := 'association';
end;

function TAIAssociationFlowConfig.GetDisplayName: string;
begin
  Result := SDisplayNameAssociation;
end;

function TAIAssociationFlowConfig.GetDescription: string;
begin
  Result := SDescriptionAssociation;
end;

function TAIAssociationFlowConfig.GetOutputFormatInfo: string;
begin
  Result := SOutputFormatInfoAssociation;
end;

function TAIAssociationFlowConfig.GetDefaultCustomInstructions: string;
begin
  Result := DEFAULT_ASSOCIATION_CUSTOM_INSTRUCTIONS;
end;

function TAIAssociationFlowConfig.GetConfigFields: TAIFlowConfigFieldArray;
begin
  SetLength(Result, 5);
  
  // Custom instructions field
  Result[0].Key := AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS;
  Result[0].DisplayName := SConfigAdditionalInstructionsDisplayName;
  Result[0].FieldType := 'memo';
  Result[0].DefaultValue := DEFAULT_ASSOCIATION_CUSTOM_INSTRUCTIONS;
  Result[0].Description := SConfigAdditionalInstructionsDescription;
  Result[0].Required := False;

  // Include comments field
  Result[1].Key := AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS;
  Result[1].DisplayName := SConfigIncludeCommentsDisplayName;
  Result[1].FieldType := 'boolean';
  Result[1].DefaultValue := 'false';
  Result[1].Description := SConfigIncludeCommentsDescription;
  Result[1].Required := False;

  // Provider override
  Result[2].Key := AI_ASSOCIATION_CONFIG_KEY_PROVIDER;
  Result[2].DisplayName := SConfigProviderOverrideDisplayName;
  Result[2].FieldType := 'provider';
  Result[2].DefaultValue := '';
  Result[2].Description := SConfigProviderOverrideDescription;
  Result[2].Required := False;

  // Model override
  Result[3].Key := AI_ASSOCIATION_CONFIG_KEY_MODEL;
  Result[3].DisplayName := SConfigModelOverrideDisplayName;
  Result[3].FieldType := 'model';
  Result[3].DefaultValue := '';
  Result[3].Description := SConfigModelOverrideDescription;
  Result[3].Required := False;

  // API Key override
  Result[4].Key := AI_ASSOCIATION_CONFIG_KEY_API_KEY;
  Result[4].DisplayName := SConfigApiKeyOverrideDisplayName;
  Result[4].FieldType := 'password';
  Result[4].DefaultValue := '';
  Result[4].Description := SConfigApiKeyOverrideDescription;
  Result[4].Required := False;
end;

function TAIAssociationFlowConfig.BuildPromptFromConfig(const ConfigValues: array of const): string;
var
  customInstructions: string;
  includeComments: Boolean;
  serializedExamples: string;
  outputObject: string;
  I: Integer;
begin
  includeComments := False;
  customInstructions := '';
  serializedExamples := '';
  
  I := 0;
  while I < Length(ConfigValues) do
  begin
    if (ConfigValues[I].VType = vtAnsiString) then
    begin
      if string(ConfigValues[I].VAnsiString) = AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS then
      begin
        if (I + 1 < Length(ConfigValues)) and (ConfigValues[I+1].VType = vtAnsiString) then
          customInstructions := Trim(string(ConfigValues[I+1].VAnsiString));
      end
      else if string(ConfigValues[I].VAnsiString) = AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS then
      begin
        if (I + 1 < Length(ConfigValues)) then
        begin
          case ConfigValues[I+1].VType of
            vtBoolean: includeComments := ConfigValues[I+1].VBoolean;
            vtAnsiString: includeComments := LowerCase(string(ConfigValues[I+1].VAnsiString)) = 'true';
          end;
        end;
      end
      else if string(ConfigValues[I].VAnsiString) = AI_ASSOCIATION_EXAMPLES_KEY then
      begin
        if (I + 1 < Length(ConfigValues)) and (ConfigValues[I+1].VType = vtAnsiString) then
          serializedExamples := string(ConfigValues[I+1].VAnsiString);
      end;
    end;
    Inc(I, 2);
  end;

  outputObject :=
    '{' + LineEnding +
    '  "data": [["phrase_from_original", "phrase_from_translation"], ...]';
    
  if includeComments then
    outputObject := outputObject + ',' + LineEnding +
      '  "comments": "explanation of difficult associations"';

  outputObject := outputObject + LineEnding + '}';
  Result := BuildAssociationPromptSection(customInstructions,
    serializedExamples, outputObject);
end;

function TAIAssociationFlowConfig.CreateFlowInstance(Context: IAIFlowContext): IAIVerseFlow;
var
  customInstructions: string;
  includeComments: Boolean;
  overrideEnabled: Boolean;
  overrideEnabledValue: string;
  providerOverride, modelOverride, apiKeyOverride: string;
  serializedExamples: string;
  flow: TAIAssociationFlow;
begin
  Result := nil;
  if not Assigned(Context) then
    raise Exception.Create('CreateFlowInstance: missing AI flow context');
  
  // Read configuration from project
  customInstructions := Context.GetProjectInfo(AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS);
  includeComments := LowerCase(Context.GetProjectInfo(AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS)) = 'true';
  providerOverride := Context.GetProjectInfo(AI_ASSOCIATION_CONFIG_KEY_PROVIDER);
  modelOverride := Context.GetProjectInfo(AI_ASSOCIATION_CONFIG_KEY_MODEL);
  apiKeyOverride := Context.GetProjectInfo(AI_ASSOCIATION_CONFIG_KEY_API_KEY);
  overrideEnabledValue := Trim(Context.GetProjectInfo(GetFlowOverrideEnabledConfigKey('association')));
  if overrideEnabledValue = '' then
    overrideEnabled := (Trim(providerOverride) <> '') or (Trim(modelOverride) <> '') or (Trim(apiKeyOverride) <> '')
  else
    overrideEnabled := LowerCase(overrideEnabledValue) = 'true';
  if not overrideEnabled then
  begin
    providerOverride := '';
    modelOverride := '';
    apiKeyOverride := '';
  end;
  serializedExamples := Context.GetProjectInfo(AI_ASSOCIATION_EXAMPLES_KEY);
  
  // Build flow with constructed prompt
  flow := TAIAssociationFlow.Create(
    BuildPromptFromConfig([
      AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, customInstructions,
      AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS, includeComments,
      AI_ASSOCIATION_EXAMPLES_KEY, serializedExamples
    ]),
    includeComments,
    providerOverride,
    modelOverride,
    apiKeyOverride
  );
  
  Result := flow;
end;

{ TAIAssociationFlow }

constructor TAIAssociationFlow.Create(const APrompt: string; AIncludeComments: Boolean; const AProvider, AModel, AApiKey: string);
begin
  inherited Create;
  FPrompt := APrompt;
  FIncludeComments := AIncludeComments;
  FProviderOverride := AProvider;
  FModelOverride := AModel;
  FApiKeyOverride := AApiKey;
end;

function TAIAssociationFlow.Name: string;
begin
  Result := 'association';
end;

function TAIAssociationFlow.ModelOverride: string;
begin
  Result := FModelOverride;
end;

function TAIAssociationFlow.ApiKeyOverride: string;
begin
  Result := FApiKeyOverride;
end;

function TAIAssociationFlow.ProviderOverride: string;
begin
  Result := FProviderOverride;
end;

procedure TAIAssociationFlow.PrepareRequest(Verse: TVerse; out Messages: array of TLLMMessage);
begin
  // Legacy fixed-size support; expect length >=2
  if Length(Messages) >= 2 then
  begin
    Messages[0].Role := 'system';
    Messages[0].Content := FPrompt;
    Messages[1].Role := 'user';
    if Assigned(Verse.PairedVerse) then
      Messages[1].Content := Format('Original: "%s"\nTranslated: "%s"',[Verse.PlainText, Verse.PairedVerse.PlainText])
    else
      Messages[1].Content := Format('Original: "%s"\nTranslated: ""',[Verse.PlainText]);
    
    AIDebugLogFmt('[Association] PrepareRequest for verse: %s', [Verse.Reference]);
    AIDebugLogFmt('[Association] System prompt length: %d chars', [Length(Messages[0].Content)]);
    AIDebugLogFmt('[Association] User message: %s', [Messages[1].Content]);
  end;
end;

function TAIAssociationFlow.BuildMessages(Verse: TVerse): TLLMMessageArray;
begin
  Result := nil; // Initialize
  SetLength(Result, 2);
  PrepareRequest(Verse, Result);
end;

function TAIAssociationFlow.ParseResponse(const RawContent: string; Verse: TVerse): TAIFlowResult;
var
  AssocResult: TAssociationResult;
begin
  // Verse parameter not used in association parsing, but required by interface
  if Verse = nil then ; // Suppress unused parameter warning
  
  AIDebugLogFmt('[Association] ParseResponse - Raw content length: %d', [Length(RawContent)]);
  AIDebugLog('[Association] Raw JSON: ' + RawContent);
  
  Result := ParseAssociationJson(RawContent);
  
  if Result is TAssociationResult then
  begin
    AssocResult := TAssociationResult(Result);
    
    // Explicitly clear comments if not enabled in configuration, 
    // even if the AI included them anyway
    if not FIncludeComments then
      AssocResult.Comments := '';

    AIDebugLogFmt('[Association] Parsed %d association pairs', [Length(AssocResult.Pairs)]);
    if AssocResult.Comments <> '' then
      AIDebugLogFmt('[Association] Comments: %s', [AssocResult.Comments]);
  end;
end;

function ApplyAssociationPairs(Association: TAssociationResult;
  Verse: TVerse): Integer;
var
  I: Integer;
  OriginalTexts, TranslatedTexts: TStringList;
  Parser: TONTParser;
  ParsedSyntagms: TSyntagmList;
  OriginalSelectionCount, TranslatedSelectionCount: Integer;
  
  procedure CollectTexts(const Phrase: string; VerseOwner: TVerse; Texts: TStringList);
  var
    J: Integer;
  begin
    if Trim(Phrase) = '' then Exit;
    Parser := TONTParser.Create;
    try
      ParsedSyntagms := Parser.ParseLine(Phrase, VerseOwner);
      try
        for J := 0 to ParsedSyntagms.Count - 1 do
          if ParsedSyntagms[J].Kind in [ONTTokenizer.skSyntagm] then
            Texts.Add(ParsedSyntagms[J].Text);
      finally
        ParsedSyntagms.Free;
      end;
    finally
      Parser.Free;
    end;
  end;
begin
  Result := 0;
  if not Assigned(Association) then
    raise Exception.Create('ApplyAssociationPairs called with nil association result');
  if not Assigned(Verse) then
    raise Exception.Create('ApplyAssociationPairs called with nil Verse');
  if Verse.PairedVerse = nil then
    raise Exception.CreateFmt(
      'ApplyAssociationPairs: Verse has no paired verse. VerseRef=%s',
      [Verse.Reference]);

  Verse.ClearSelection;
  Verse.PairedVerse.ClearSelection;
  for I := 0 to High(Association.Pairs) do
  begin
    if (Trim(Association.Pairs[I].OriginalPhrase) = '') or
       (Trim(Association.Pairs[I].TranslatedPhrase) = '') then
      Continue;

    AIDebugLogFmt('[Association] Pair %d: "%s" <-> "%s"',
      [I, Association.Pairs[I].OriginalPhrase,
       Association.Pairs[I].TranslatedPhrase]);

    OriginalTexts := TStringList.Create;
    TranslatedTexts := TStringList.Create;
    try
      CollectTexts(Association.Pairs[I].OriginalPhrase, Verse, OriginalTexts);
      CollectTexts(Association.Pairs[I].TranslatedPhrase,
        Verse.PairedVerse, TranslatedTexts);

      AIDebugLogVerboseFmt('[Association]   Original syntagms collected: %d',
        [OriginalTexts.Count]);
      AIDebugLogVerboseFmt('[Association]   Translated syntagms collected: %d',
        [TranslatedTexts.Count]);

      Verse.FindAndSelectSyntagms(OriginalTexts);
      Verse.PairedVerse.FindAndSelectSyntagms(TranslatedTexts);

      OriginalSelectionCount := Verse.Selection.Count;
      TranslatedSelectionCount := Verse.PairedVerse.Selection.Count;

      AIDebugLogFmt(
        '[Association]   Selection counts: original=%d/%d, translated=%d/%d',
        [OriginalSelectionCount, OriginalTexts.Count,
         TranslatedSelectionCount, TranslatedTexts.Count]);

      if (OriginalSelectionCount = 0) or
         (TranslatedSelectionCount = 0) then
      begin
        AIDebugLogFmt(
          '[Association]   Skipping pair %d due to incomplete selection: "%s" <-> "%s"',
          [I, Association.Pairs[I].OriginalPhrase,
           Association.Pairs[I].TranslatedPhrase]);
        Continue;
      end;

      Verse.AssociateSyntagms;
      Inc(Result);
    finally
      OriginalTexts.Free;
      TranslatedTexts.Free;
      Verse.ClearSelection;
      Verse.PairedVerse.ClearSelection;
    end;
  end;

  Association.PairsXML := Verse.Pairs;
end;

procedure TAIAssociationFlow.ApplyResult(ResultObj: TAIFlowResult; Verse: TVerse);
var
  Assoc: TAssociationResult;
begin
  if not (ResultObj is TAssociationResult) then
    Exit;
  if not Assigned(Verse) then
    raise Exception.Create('ApplyResult called with nil Verse');
  Assoc := TAssociationResult(ResultObj);
  if Verse.PairedVerse = nil then
    raise Exception.CreateFmt('ApplyResult: Verse has no paired verse. VerseRef=%s',
      [Verse.Reference]);

  AIDebugLogFmt('[Association] ApplyResult for verse: %s', [Verse.Reference]);
  AIDebugLogFmt('[Association] Processing %d pairs', [Length(Assoc.Pairs)]);

  try
    ApplyAssociationPairs(Assoc, Verse);
    Assoc.SetAcceptedPairs(Assoc.PairsXML);
    if FIncludeComments then
      Assoc.SetAcceptedComments(Assoc.Comments);
    AIDebugLogFmt('[Association] Associations complete. PairsXML length: %d chars',
      [Length(Assoc.PairsXML)]);
    AIDebugLogFmt(
      '[Association] Pairs updated in memory for verse: %s (NOT saved to DB yet)',
      [Verse.Reference]);
  except
    on E: Exception do
    begin
      AIDebugLogFmt('[Association] Exception in ApplyResult for verse %s: %s', [Verse.Reference, E.Message]);
      raise; // re-raise so manager can mark job as failed and preserve job for inspection
    end;
  end;
end;

function TAIAssociationFlow.RequiresIndividualClient: Boolean;
begin
  Result := False; // safe to share client if TBaseLLMClient is thread-safe per call
end;

end.
