unit ai_translation_flow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, ai_association_flow, llmclient, Verse, ai_debug;

const
  TRANSLATION_PROMPT_HEADER =
    'You are a professional biblical translator with expertise in ancient languages and modern translations.' + LineEnding +
    LineEnding;

  TRANSLATION_PROMPT_GUIDELINES =
    'Translation Guidelines:' + LineEnding +
    '1. Maintain the theological and semantic meaning of the original text' + LineEnding +
    '2. Preserve the literary style and tone (narrative, poetic, prophetic, etc.)' + LineEnding +
    '3. Keep proper names untranslated unless there is a well-established equivalent' + LineEnding +
    '4. Maintain punctuation marks and sentence structure when possible' + LineEnding +
    '5. Use natural, fluent language in the target language' + LineEnding +
    '6. Be consistent with established biblical terminology' + LineEnding +
    '7. Consider the broader context of the passage' + LineEnding +
    LineEnding;

  TRANSLATION_PLAIN_OUTPUT_FORMAT =
    'Output Format:' + LineEnding +
    'Return ONLY the translated text, without explanations, notes, or commentary.' + LineEnding +
    'Do not include quotation marks around the translation.' + LineEnding +
    'Maintain the same formatting (line breaks, spacing) as the original if present.';

  DEFAULT_TRANSLATION_PROMPT =
    TRANSLATION_PROMPT_HEADER +
    TRANSLATION_PROMPT_GUIDELINES +
    TRANSLATION_PLAIN_OUTPUT_FORMAT;
    
  // Configuration keys for this flow
  AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS = 'ai.flow.translation.custom_instructions';
  AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE = 'ai.flow.translation.target_language';
  AI_TRANSLATION_CONFIG_KEY_FORCE_OVERWRITE = 'ai.flow.translation.force_overwrite';
  AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS = 'ai.flow.translation.include_associations';
  AI_TRANSLATION_CONFIG_KEY_MODEL = 'ai.flow.translation.model';
  AI_TRANSLATION_CONFIG_KEY_API_KEY = 'ai.flow.translation.api_key';
  AI_TRANSLATION_CONFIG_KEY_PROVIDER = 'ai.flow.translation.provider';
  
  // Default customizable instructions
  DEFAULT_TRANSLATION_CUSTOM_INSTRUCTIONS =
    'Additional translation guidelines (optional):' + LineEnding +
    LineEnding +
    '- Specify translation philosophy (literal vs. dynamic)' + LineEnding +
    '- Define preferred terminology' + LineEnding +
    '- Provide context-specific rules';

 type
   TAITranslationResult = class(TAssociationResult)
   public
     TranslatedText: string;
   end;
   
   { TAITranslationFlowConfig - Configuration provider for translation flow }
   TAITranslationFlowConfig = class(TInterfacedObject, IAIFlowConfig)
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

   { TAITranslationFlow }
   TAITranslationFlow = class(TInterfacedObject, IAIVerseFlow)
   private
     FPrompt: string; // system prompt orientando tradução
     FForceOverwrite: Boolean;
     FIncludeAssociations: Boolean;
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
     constructor Create(const APrompt: string = DEFAULT_TRANSLATION_PROMPT;
                        AForceOverwrite: Boolean = False;
                        const AProvider: string = '';
                        const AModel: string = '';
                        const AApiKey: string = ''); overload;
     constructor Create(const APrompt: string; AForceOverwrite,
                        AIncludeAssociations: Boolean;
                        const AProvider: string = '';
                        const AModel: string = '';
                        const AApiKey: string = ''); overload;
     property ForceOverwrite: Boolean read FForceOverwrite write FForceOverwrite;
     property IncludeAssociations: Boolean read FIncludeAssociations
       write FIncludeAssociations;
   end;

 function ParseTranslationWithAssociationsJson(
   const RawJson: string): TAITranslationResult;
 function IsTranslationDestinationEligible(const DestinationText: string;
   ForceOverwrite: Boolean): Boolean;

resourcestring
  SDisplayNameTranslation = 'Translation';
  SDescriptionTranslation = 'Translates biblical verses maintaining theological and stylistic accuracy';
  SOutputFormatInfoTranslation = 'This flow expects plain text output, or a JSON object with translation and data when translated-text associations are enabled.';
  SConfigTargetLanguageDisplayName = 'Target Language';
  SConfigTargetLanguageDescription = 'Specify the target language for translation (e.g., "Brazilian Portuguese", "Spanish")';
  SConfigAdditionalInstructionsDisplayName = 'Additional Instructions';
  SConfigAdditionalInstructionsDescription = 'Add translation philosophy, terminology preferences, or context-specific rules';
  SConfigForceOverwriteDisplayName = 'Force Overwrite';
  SConfigForceOverwriteDescription = 'If checked, overwrites existing translations. If unchecked, only translates empty verses.';
  SConfigIncludeAssociationsDisplayName = 'Include association of translated text';
  SConfigIncludeAssociationsDescription = 'If checked, returns and applies associations between the original text and the new translation.';
  SConfigModelOverrideDisplayName = 'Model';
  SConfigModelOverrideDescription = 'Specify a different model for this flow. Leave empty to use global setting.';
  SConfigApiKeyOverrideDisplayName = 'API Key';
  SConfigApiKeyOverrideDescription = 'Specify a different API key for this flow. Leave empty to use global setting.';
  SConfigProviderOverrideDisplayName = 'Provider';
  SConfigProviderOverrideDescription = 'Specify a different provider for this flow. Leave empty to use global setting.';

implementation

uses
  ai_flow_config, fpjson, jsonparser;

function FormatTranslationExamplesPromptSection(const Examples: TAIFlowExampleArray;
  IncludeAssociations: Boolean): string;
var
  I: Integer;
begin
  Result := '';
  if Length(Examples) = 0 then
    Exit;

  Result := '=== Translation Examples ===' + LineEnding;
  if IncludeAssociations then
    Result := Result +
      'Use these examples as guidance for the value of the "translation" field.'
  else
    Result := Result +
      'Use these examples as guidance for tone, terminology, and expected output.';
  for I := 0 to High(Examples) do
  begin
    Result := Result + LineEnding + LineEnding +
              Format('Example %d - %s', [I + 1, Examples[I].ReferenceCaption]) + LineEnding +
              Format('Original: "%s"', [Examples[I].SourceText]) + LineEnding;
    if IncludeAssociations then
      Result := Result + Format('Expected "translation" value: "%s"',
        [Examples[I].ExpectedResponse])
    else
      Result := Result + Format('Expected translation: "%s"',
        [Examples[I].ExpectedResponse]);
  end;
end;

function ParseTranslationWithAssociationsJson(
  const RawJson: string): TAITranslationResult;
var
  Parser: TJSONParser;
  Data, TranslationNode, AssociationNode: TJSONData;
  Obj: TJSONObject;
  Association: TAssociationResult;
  CleanJson: string;
  I, ValidPairCount: Integer;
begin
  Result := TAITranslationResult.Create;
  Data := nil;
  Association := nil;
  try
    try
      CleanJson := ExtractJsonFromMarkdown(RawJson);
      Parser := TJSONParser.Create(CleanJson, []);
      try
        Data := Parser.Parse;
      finally
        Parser.Free;
      end;

      if not (Data is TJSONObject) then
        raise Exception.Create('Expected a JSON object');
      Obj := TJSONObject(Data);

      TranslationNode := Obj.Find('translation');
      if not (TranslationNode is TJSONString) then
        raise Exception.Create('Required field "translation" must be a string');
      Result.TranslatedText := TJSONString(TranslationNode).Value;

      AssociationNode := Obj.Find('data');
      if not (AssociationNode is TJSONArray) then
        raise Exception.Create('Required field "data" must be an array');

      Association := ParseAssociationJson(CleanJson);
      if Association.ParseError <> '' then
        raise Exception.Create(Association.ParseError);
      ValidPairCount := 0;
      SetLength(Result.Pairs, 0);
      for I := 0 to High(Association.Pairs) do
        if (Trim(Association.Pairs[I].OriginalPhrase) <> '') and
           (Trim(Association.Pairs[I].TranslatedPhrase) <> '') and
           (Pos(Association.Pairs[I].TranslatedPhrase,
             Result.TranslatedText) > 0) then
        begin
          SetLength(Result.Pairs, ValidPairCount + 1);
          Result.Pairs[ValidPairCount] := Association.Pairs[I];
          Inc(ValidPairCount);
        end;
      if (Trim(Result.TranslatedText) <> '') and (ValidPairCount = 0) then
        raise Exception.Create(
          'A non-empty translation requires at least one valid association pair');
    except
      on E: Exception do
        Result.ParseError := E.Message;
    end;
  finally
    Association.Free;
    Data.Free;
  end;
end;

function IsTranslationDestinationEligible(const DestinationText: string;
  ForceOverwrite: Boolean): Boolean;
begin
  Result := ForceOverwrite or (Trim(DestinationText) = '');
end;

{ TAITranslationFlowConfig }

function TAITranslationFlowConfig.GetFlowId: string;
begin
  Result := 'translation';
end;

function TAITranslationFlowConfig.GetDisplayName: string;
begin
  Result := SDisplayNameTranslation;
end;

function TAITranslationFlowConfig.GetDescription: string;
begin
  Result := SDescriptionTranslation;
end;

function TAITranslationFlowConfig.GetOutputFormatInfo: string;
begin
  Result := SOutputFormatInfoTranslation;
end;

function TAITranslationFlowConfig.GetDefaultCustomInstructions: string;
begin
  Result := DEFAULT_TRANSLATION_CUSTOM_INSTRUCTIONS;
end;

function TAITranslationFlowConfig.GetConfigFields: TAIFlowConfigFieldArray;
begin
  SetLength(Result, 7);
  
  // Target language field
  Result[0].Key := AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE;
  Result[0].DisplayName := SConfigTargetLanguageDisplayName;
  Result[0].FieldType := 'string';
  Result[0].DefaultValue := '';
  Result[0].Description := SConfigTargetLanguageDescription;
  Result[0].Required := False;
  
  // Custom instructions field
  Result[1].Key := AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS;
  Result[1].DisplayName := SConfigAdditionalInstructionsDisplayName;
  Result[1].FieldType := 'memo';
  Result[1].DefaultValue := DEFAULT_TRANSLATION_CUSTOM_INSTRUCTIONS;
  Result[1].Description := SConfigAdditionalInstructionsDescription;
  Result[1].Required := False;
  
  // Force overwrite field
  Result[2].Key := AI_TRANSLATION_CONFIG_KEY_FORCE_OVERWRITE;
  Result[2].DisplayName := SConfigForceOverwriteDisplayName;
  Result[2].FieldType := 'boolean';
  Result[2].DefaultValue := 'false';
  Result[2].Description := SConfigForceOverwriteDescription;
  Result[2].Required := False;

  // Include associations field
  Result[3].Key := AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS;
  Result[3].DisplayName := SConfigIncludeAssociationsDisplayName;
  Result[3].FieldType := 'boolean';
  Result[3].DefaultValue := 'false';
  Result[3].Description := SConfigIncludeAssociationsDescription;
  Result[3].Required := False;

  // Provider override
  Result[4].Key := AI_TRANSLATION_CONFIG_KEY_PROVIDER;
  Result[4].DisplayName := SConfigProviderOverrideDisplayName;
  Result[4].FieldType := 'provider';
  Result[4].DefaultValue := '';
  Result[4].Description := SConfigProviderOverrideDescription;
  Result[4].Required := False;

  // Model override
  Result[5].Key := AI_TRANSLATION_CONFIG_KEY_MODEL;
  Result[5].DisplayName := SConfigModelOverrideDisplayName;
  Result[5].FieldType := 'model';
  Result[5].DefaultValue := '';
  Result[5].Description := SConfigModelOverrideDescription;
  Result[5].Required := False;

  // API Key override
  Result[6].Key := AI_TRANSLATION_CONFIG_KEY_API_KEY;
  Result[6].DisplayName := SConfigApiKeyOverrideDisplayName;
  Result[6].FieldType := 'password';
  Result[6].DefaultValue := '';
  Result[6].Description := SConfigApiKeyOverrideDescription;
  Result[6].Required := False;
end;

function TAITranslationFlowConfig.BuildPromptFromConfig(const ConfigValues: array of const): string;
var
  i: Integer;
  key, value: string;
  targetLanguage, customInstructions, serializedExamples, examplesSection: string;
  associationInstructions, associationExamples, outputObject: string;
  includeAssociations: Boolean;
begin
  targetLanguage := '';
  customInstructions := '';
  serializedExamples := '';
  associationInstructions := '';
  associationExamples := '';
  includeAssociations := False;
  
  // Extract config values (expecting key-value pairs)
  i := 0;
  while i < Length(ConfigValues) - 1 do
  begin
    if ConfigValues[i].VType = vtAnsiString then
    begin
      key := string(ConfigValues[i].VAnsiString);
      if key = AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS then
      begin
        case ConfigValues[i+1].VType of
          vtBoolean: includeAssociations := ConfigValues[i+1].VBoolean;
          vtAnsiString: includeAssociations :=
            LowerCase(string(ConfigValues[i+1].VAnsiString)) = 'true';
        end;
      end
      else if ConfigValues[i+1].VType = vtAnsiString then
      begin
        value := string(ConfigValues[i+1].VAnsiString);
        if key = AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE then
          targetLanguage := Trim(value)
        else if key = AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS then
          customInstructions := Trim(value)
        else if key = AI_TRANSLATION_EXAMPLES_KEY then
          serializedExamples := value
        else if key = AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS then
          associationInstructions := Trim(value)
        else if key = AI_ASSOCIATION_EXAMPLES_KEY then
          associationExamples := value;
      end;
    end;
    Inc(i, 2);
  end;

  if includeAssociations then
    Result := TRANSLATION_PROMPT_HEADER + TRANSLATION_PROMPT_GUIDELINES
  else
    Result := DEFAULT_TRANSLATION_PROMPT;
  
  // Add target language section if specified
  if targetLanguage <> '' then
  begin
    Result := Result + LineEnding + LineEnding +
              '=== Target Language ===' + LineEnding +
              'Translate to: ' + targetLanguage;
  end;
  
  // Add custom instructions if specified
  if customInstructions <> '' then
  begin
    Result := Result + LineEnding + LineEnding +
              '=== Additional Project-Specific Instructions ===' + LineEnding +
              customInstructions;
  end;

  examplesSection := FormatTranslationExamplesPromptSection(
    LoadFlowExamples(serializedExamples), includeAssociations);
  if examplesSection <> '' then
    Result := Result + LineEnding + LineEnding + examplesSection;

  if includeAssociations then
  begin
    outputObject :=
      '{' + LineEnding +
      '  "translation": "complete translated verse",' + LineEnding +
      '  "data": [[' +
      '"phrase copied exactly from the original", ' +
      '"phrase copied exactly from translation"], ...]' + LineEnding +
      '}';
    Result := Result + LineEnding + LineEnding +
      '=== Translate and Associate in One Response ===' + LineEnding +
      'Translate the complete original verse, then associate phrases from the ' +
      'original with phrases from that exact translation.' + LineEnding +
      'Every destination phrase in "data" must be copied verbatim from the ' +
      '"translation" value. Every source phrase must be copied verbatim from ' +
      'the original input.' + LineEnding + LineEnding +
      BuildAssociationPromptSection(associationInstructions,
        associationExamples, outputObject, True) + LineEnding + LineEnding +
      'Return only the JSON object. Do not add commentary or Markdown fences.';
  end;
end;

function TAITranslationFlowConfig.CreateFlowInstance(Context: IAIFlowContext): IAIVerseFlow;
var
  targetLanguage, customInstructions: string;
  forceOverwrite, includeAssociations: Boolean;
  overrideEnabled: Boolean;
  overrideEnabledValue: string;
  providerOverride, modelOverride, apiKeyOverride: string;
  serializedExamples, associationInstructions, associationExamples: string;
  flow: TAITranslationFlow;
begin
  Result := nil;
  if not Assigned(Context) then
    raise Exception.Create('CreateFlowInstance: missing AI flow context');
  
  // Read configuration from project
  targetLanguage := Context.GetProjectInfo(AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE);
  customInstructions := Context.GetProjectInfo(AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS);
  forceOverwrite := LowerCase(Context.GetProjectInfo(AI_TRANSLATION_CONFIG_KEY_FORCE_OVERWRITE)) = 'true';
  includeAssociations := LowerCase(
    Context.GetProjectInfo(AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS)) = 'true';
  providerOverride := Context.GetProjectInfo(AI_TRANSLATION_CONFIG_KEY_PROVIDER);
  modelOverride := Context.GetProjectInfo(AI_TRANSLATION_CONFIG_KEY_MODEL);
  apiKeyOverride := Context.GetProjectInfo(AI_TRANSLATION_CONFIG_KEY_API_KEY);
  overrideEnabledValue := Trim(Context.GetProjectInfo(GetFlowOverrideEnabledConfigKey('translation')));
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
  serializedExamples := Context.GetProjectInfo(AI_TRANSLATION_EXAMPLES_KEY);
  associationInstructions := Context.GetProjectInfo(
    AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS);
  associationExamples := Context.GetProjectInfo(AI_ASSOCIATION_EXAMPLES_KEY);
  
  // Create TAITranslationFlow with built prompt and forceOverwrite flag
  flow := TAITranslationFlow.Create(
    BuildPromptFromConfig([
      AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE, targetLanguage,
      AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, customInstructions,
      AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS, includeAssociations,
      AI_TRANSLATION_EXAMPLES_KEY, serializedExamples,
      AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, associationInstructions,
      AI_ASSOCIATION_EXAMPLES_KEY, associationExamples
    ]),
    forceOverwrite,
    includeAssociations,
    providerOverride,
    modelOverride,
    apiKeyOverride
  );
  
  Result := flow;
end;

{ TAITranslationFlow }

constructor TAITranslationFlow.Create(const APrompt: string;
  AForceOverwrite: Boolean; const AProvider, AModel, AApiKey: string);
begin
  inherited Create;
  FPrompt := APrompt;
  FForceOverwrite := AForceOverwrite;
  FIncludeAssociations := False;
  FProviderOverride := AProvider;
  FModelOverride := AModel;
  FApiKeyOverride := AApiKey;
end;

constructor TAITranslationFlow.Create(const APrompt: string;
  AForceOverwrite, AIncludeAssociations: Boolean;
  const AProvider, AModel, AApiKey: string);
begin
  inherited Create;
  FPrompt := APrompt;
  FForceOverwrite := AForceOverwrite;
  FIncludeAssociations := AIncludeAssociations;
  FProviderOverride := AProvider;
  FModelOverride := AModel;
  FApiKeyOverride := AApiKey;
end;

function TAITranslationFlow.Name: string;
begin
  Result := 'translation';
end;

function TAITranslationFlow.ModelOverride: string;
begin
  Result := FModelOverride;
end;

function TAITranslationFlow.ApiKeyOverride: string;
begin
  Result := FApiKeyOverride;
end;

function TAITranslationFlow.ProviderOverride: string;
begin
  Result := FProviderOverride;
end;

procedure TAITranslationFlow.PrepareRequest(Verse: TVerse; out Messages: array of TLLMMessage);
begin
  if Length(Messages) >= 2 then
  begin
    Messages[0].Role := 'system';
    Messages[0].Content := FPrompt; // Ex: "Traduza o texto fornecido mantendo pontuação." (configurável)
    Messages[1].Role := 'user';
    if Assigned(Verse) then
    begin
      Messages[1].Content := Verse.PlainText;
      AIDebugLogFmt('[Translation] PrepareRequest for verse: %s', [Verse.Reference]);
      AIDebugLogFmt('[Translation] Original text: %s', [Messages[1].Content]);
    end
    else
    begin
      Messages[1].Content := '';
      AIDebugLog('[Translation] PrepareRequest for verse: <nil>');
    end;
    AIDebugLogFmt('[Translation] System prompt length: %d chars', [Length(Messages[0].Content)]);
  end;
end;

function TAITranslationFlow.BuildMessages(Verse: TVerse): TLLMMessageArray;
begin
  Result := nil;
  SetLength(Result, 2);
  PrepareRequest(Verse, Result);
end;

function TAITranslationFlow.ParseResponse(const RawContent: string; Verse: TVerse): TAIFlowResult;
var
  R: TAITranslationResult;
  I, ValidPairCount: Integer;
begin
  if FIncludeAssociations then
  begin
    AIDebugLogFmt('[Translation] ParseResponse combined JSON - Raw content length: %d',
      [Length(RawContent)]);
    Result := ParseTranslationWithAssociationsJson(RawContent);
    R := TAITranslationResult(Result);
    if (R.ParseError = '') and Assigned(Verse) and
       (Trim(Verse.PlainText) <> '') and (Trim(R.TranslatedText) = '') then
      R.ParseError := 'A non-empty original requires a non-empty translation';

    if (R.ParseError = '') and Assigned(Verse) and
       (Trim(R.TranslatedText) <> '') then
    begin
      ValidPairCount := 0;
      for I := 0 to High(R.Pairs) do
        if Pos(R.Pairs[I].OriginalPhrase, Verse.PlainText) > 0 then
        begin
          R.Pairs[ValidPairCount] := R.Pairs[I];
          Inc(ValidPairCount);
        end;
      SetLength(R.Pairs, ValidPairCount);

      if ValidPairCount = 0 then
        R.ParseError :=
          'A non-empty translation requires at least one association pair ' +
          'whose source phrase occurs literally in the original';
    end;
    Exit;
  end;

  Result := nil;
  R := TAITranslationResult.Create;
  try
    AIDebugLogFmt('[Translation] ParseResponse - Raw content length: %d', [Length(RawContent)]);
    AIDebugLog('[Translation] Translated text: ' + RawContent);
    
    // Primeira versão: resposta já é a tradução em texto puro
    R.TranslatedText := Trim(RawContent);
    Result := R;
  except
    R.Free;
    raise;
  end;
end;

procedure TAITranslationFlow.ApplyResult(ResultObj: TAIFlowResult; Verse: TVerse);
var
  R: TAITranslationResult;
  OldDestinationText, OldPairs: string;
  AppliedPairCount: Integer;
begin
  if not (ResultObj is TAITranslationResult) then Exit;
  R := TAITranslationResult(ResultObj);
  
  if not Assigned(Verse) then
  begin
    AIDebugLog('[Translation] ApplyResult for verse: <nil> - Skipped');
    Exit;
  end;
  
  if (Verse.PairedVerse = nil) then
  begin
    AIDebugLogFmt('[Translation] ApplyResult for verse: %s - No paired verse', [Verse.Reference]);
    Exit; // necessita par destino
  end;
  
  AIDebugLogFmt('[Translation] ApplyResult for verse: %s', [Verse.Reference]);
  AIDebugLogFmt('[Translation] Target verse current text: "%s"', [Verse.PairedVerse.PlainText]);
  AIDebugLogFmt('[Translation] ForceOverwrite: %s', [BoolToStr(FForceOverwrite, True)]);
  AIDebugLogFmt('[Translation] IncludeAssociations: %s',
    [BoolToStr(FIncludeAssociations, True)]);
  AIDebugLogFmt('[Translation] Translated text: "%s"', [R.TranslatedText]);
  
  if not IsTranslationDestinationEligible(Verse.PairedVerse.PlainText,
    FForceOverwrite) then
    raise Exception.CreateFmt(
      'Translation refused because destination verse %s is not empty and Force Overwrite is disabled',
      [Verse.PairedVerse.Reference]);

  if not FIncludeAssociations then
  begin
    if FForceOverwrite and
       (Verse.PairedVerse.Text <> R.TranslatedText) then
      R.SetAcceptedPairs('');
    R.SetAcceptedDestinationText(R.TranslatedText);
    Exit;
  end;

  OldDestinationText := Verse.PairedVerse.Text;
  OldPairs := Verse.Pairs;
  try
    Verse.ClearAssociations;
    Verse.PairedVerse.Text := R.TranslatedText;
    AppliedPairCount := ApplyAssociationPairs(R, Verse);
    if AppliedPairCount = 0 then
      raise Exception.Create(
        'No association pair could be applied to the translated verse');

    R.SetAcceptedDestinationText(R.TranslatedText);
    R.SetAcceptedPairs(R.PairsXML);
    AIDebugLogFmt('[Translation] Materialized %d association pairs',
      [AppliedPairCount]);
  except
    try
      Verse.ClearAssociations;
      Verse.PairedVerse.Text := OldDestinationText;
      Verse.Pairs := OldPairs;
    except
      on RestoreError: Exception do
        AIDebugLogFmt('[Translation] Failed to restore temporary verse after error: %s',
          [RestoreError.Message]);
    end;
    raise;
  end;
end;

function TAITranslationFlow.RequiresIndividualClient: Boolean;
begin
  Result := False;
end;

end.
