unit ai_translation_flow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, llmclient, versiculo, ai_debug;

const
  DEFAULT_TRANSLATION_PROMPT =
    'You are a professional biblical translator with expertise in ancient languages and modern translations.' + LineEnding +
    LineEnding +
    'Translation Guidelines:' + LineEnding +
    '1. Maintain the theological and semantic meaning of the original text' + LineEnding +
    '2. Preserve the literary style and tone (narrative, poetic, prophetic, etc.)' + LineEnding +
    '3. Keep proper names untranslated unless there is a well-established equivalent' + LineEnding +
    '4. Maintain punctuation marks and sentence structure when possible' + LineEnding +
    '5. Use natural, fluent language in the target language' + LineEnding +
    '6. Be consistent with established biblical terminology' + LineEnding +
    '7. Consider the broader context of the passage' + LineEnding +
    LineEnding +
    'Output Format:' + LineEnding +
    'Return ONLY the translated text, without explanations, notes, or commentary.' + LineEnding +
    'Do not include quotation marks around the translation.' + LineEnding +
    'Maintain the same formatting (line breaks, spacing) as the original if present.';
    
  // Configuration keys for this flow
  AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS = 'ai.flow.translation.custom_instructions';
  AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE = 'ai.flow.translation.target_language';
  AI_TRANSLATION_CONFIG_KEY_FORCE_OVERWRITE = 'ai.flow.translation.force_overwrite';
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
   TAITranslationResult = class(TAIFlowResult)
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
     function CreateFlowInstance(Project: TObject): IAIVerseFlow;
   end;

   { TAITranslationFlow }
   TAITranslationFlow = class(TInterfacedObject, IAIVerseFlow)
   private
     FPrompt: string; // system prompt orientando tradução
     FForceOverwrite: Boolean;
     FProviderOverride: string;
     FModelOverride: string;
     FApiKeyOverride: string;
   protected
     function Name: string;
     procedure PrepareRequest(Verse: TVersiculo; out Messages: array of TLLMMessage); overload;
     function BuildMessages(Verse: TVersiculo): TLLMMessageArray;
     function ParseResponse(const RawContent: string; Verse: TVersiculo): TAIFlowResult;
     procedure ApplyResult(ResultObj: TAIFlowResult; Verse: TVersiculo);
     function RequiresIndividualClient: Boolean;
     function ModelOverride: string;
     function ApiKeyOverride: string;
     function ProviderOverride: string;
   public
     constructor Create(const APrompt: string = DEFAULT_TRANSLATION_PROMPT; 
                        AForceOverwrite: Boolean = False;
                        const AProvider: string = '';
                        const AModel: string = '';
                        const AApiKey: string = '');
     property ForceOverwrite: Boolean read FForceOverwrite write FForceOverwrite;
   end;

resourcestring
  SDisplayNameTranslation = 'Translation';
  SDescriptionTranslation = 'Translates biblical verses maintaining theological and stylistic accuracy';
  SOutputFormatInfoTranslation = 'This flow expects plain text output without quotes or explanations';
  SConfigTargetLanguageDisplayName = 'Target Language';
  SConfigTargetLanguageDescription = 'Specify the target language for translation (e.g., "Brazilian Portuguese", "Spanish")';
  SConfigAdditionalInstructionsDisplayName = 'Additional Instructions';
  SConfigAdditionalInstructionsDescription = 'Add translation philosophy, terminology preferences, or context-specific rules';
  SConfigForceOverwriteDisplayName = 'Force Overwrite';
  SConfigForceOverwriteDescription = 'If checked, overwrites existing translations. If unchecked, only translates empty verses.';
  SConfigModelOverrideDisplayName = 'Model';
  SConfigModelOverrideDescription = 'Specify a different model for this flow. Leave empty to use global setting.';
  SConfigApiKeyOverrideDisplayName = 'API Key';
  SConfigApiKeyOverrideDescription = 'Specify a different API key for this flow. Leave empty to use global setting.';
  SConfigProviderOverrideDisplayName = 'Provider';
  SConfigProviderOverrideDescription = 'Specify a different provider for this flow. Leave empty to use global setting.';

implementation

uses
  Projeto, ai_flow_config;

function FormatTranslationExamplesPromptSection(const Examples: TAIFlowExampleArray): string;
var
  I: Integer;
begin
  Result := '';
  if Length(Examples) = 0 then
    Exit;

  Result := '=== Translation Examples ===' + LineEnding +
            'Use these examples as guidance for tone, terminology, and expected output.';
  for I := 0 to High(Examples) do
  begin
    Result := Result + LineEnding + LineEnding +
              Format('Example %d - %s', [I + 1, Examples[I].ReferenceCaption]) + LineEnding +
              Format('Original: "%s"', [Examples[I].SourceText]) + LineEnding +
              Format('Expected translation: "%s"', [Examples[I].ExpectedResponse]);
  end;
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
  SetLength(Result, 6);
  
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

  // Provider override
  Result[3].Key := AI_TRANSLATION_CONFIG_KEY_PROVIDER;
  Result[3].DisplayName := SConfigProviderOverrideDisplayName;
  Result[3].FieldType := 'provider';
  Result[3].DefaultValue := '';
  Result[3].Description := SConfigProviderOverrideDescription;
  Result[3].Required := False;

  // Model override
  Result[4].Key := AI_TRANSLATION_CONFIG_KEY_MODEL;
  Result[4].DisplayName := SConfigModelOverrideDisplayName;
  Result[4].FieldType := 'model';
  Result[4].DefaultValue := '';
  Result[4].Description := SConfigModelOverrideDescription;
  Result[4].Required := False;

  // API Key override
  Result[5].Key := AI_TRANSLATION_CONFIG_KEY_API_KEY;
  Result[5].DisplayName := SConfigApiKeyOverrideDisplayName;
  Result[5].FieldType := 'password';
  Result[5].DefaultValue := '';
  Result[5].Description := SConfigApiKeyOverrideDescription;
  Result[5].Required := False;
end;

function TAITranslationFlowConfig.BuildPromptFromConfig(const ConfigValues: array of const): string;
var
  i: Integer;
  key, value: string;
  targetLanguage, customInstructions, serializedExamples, examplesSection: string;
begin
  Result := DEFAULT_TRANSLATION_PROMPT;
  targetLanguage := '';
  customInstructions := '';
  serializedExamples := '';
  i := 0;
  
  // Extract config values (expecting key-value pairs)
  i := 0;
  while i < Length(ConfigValues) - 1 do
  begin
    if (ConfigValues[i].VType = vtAnsiString) and (ConfigValues[i+1].VType = vtAnsiString) then
    begin
      key := string(ConfigValues[i].VAnsiString);
      value := string(ConfigValues[i+1].VAnsiString);
      
      if key = AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE then
        targetLanguage := Trim(value)
      else if key = AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS then
        customInstructions := Trim(value)
      else if key = AI_TRANSLATION_EXAMPLES_KEY then
        serializedExamples := value;
    end;
    Inc(i, 2);
  end;
  
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

  examplesSection := FormatTranslationExamplesPromptSection(LoadFlowExamples(serializedExamples));
  if examplesSection <> '' then
    Result := Result + LineEnding + LineEnding + examplesSection;
end;

function TAITranslationFlowConfig.CreateFlowInstance(Project: TObject): IAIVerseFlow;
var
  proj: TProjeto;
  targetLanguage, customInstructions: string;
  forceOverwrite: Boolean;
  overrideEnabled: Boolean;
  overrideEnabledValue: string;
  providerOverride, modelOverride, apiKeyOverride: string;
  serializedExamples: string;
  configValues: array[0..5] of TVarRec;
  flow: TAITranslationFlow;
begin
  Result := nil;
  
  // Cast project to TProjeto
  if not (Project is TProjeto) then
    raise Exception.Create('CreateFlowInstance: Project must be TProjeto instance');
  
  proj := TProjeto(Project);
  
  // Read configuration from project
  targetLanguage := proj.ObterInfo(AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE);
  customInstructions := proj.ObterInfo(AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS);
  forceOverwrite := LowerCase(proj.ObterInfo(AI_TRANSLATION_CONFIG_KEY_FORCE_OVERWRITE)) = 'true';
  providerOverride := proj.ObterInfo(AI_TRANSLATION_CONFIG_KEY_PROVIDER);
  modelOverride := proj.ObterInfo(AI_TRANSLATION_CONFIG_KEY_MODEL);
  apiKeyOverride := proj.ObterInfo(AI_TRANSLATION_CONFIG_KEY_API_KEY);
  overrideEnabledValue := Trim(proj.ObterInfo(GetFlowOverrideEnabledConfigKey('translation')));
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
  serializedExamples := proj.ObterInfo(AI_TRANSLATION_EXAMPLES_KEY);
  
  // Build configValues array for BuildPromptFromConfig
  // Array format: [key1, value1, key2, value2, ...]
  configValues[0].VType := vtAnsiString;
  configValues[0].VAnsiString := PAnsiChar(AnsiString(AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE));
  configValues[1].VType := vtAnsiString;
  configValues[1].VAnsiString := PAnsiChar(AnsiString(targetLanguage));
  configValues[2].VType := vtAnsiString;
  configValues[2].VAnsiString := PAnsiChar(AnsiString(AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS));
  configValues[3].VType := vtAnsiString;
  configValues[3].VAnsiString := PAnsiChar(AnsiString(customInstructions));
  configValues[4].VType := vtAnsiString;
  configValues[4].VAnsiString := PAnsiChar(AnsiString(AI_TRANSLATION_EXAMPLES_KEY));
  configValues[5].VType := vtAnsiString;
  configValues[5].VAnsiString := PAnsiChar(AnsiString(serializedExamples));
  
  // Create TAITranslationFlow with built prompt and forceOverwrite flag
  flow := TAITranslationFlow.Create(
    BuildPromptFromConfig(configValues),
    forceOverwrite,
    providerOverride,
    modelOverride,
    apiKeyOverride
  );
  
  Result := flow;
end;

{ TAITranslationFlow }

constructor TAITranslationFlow.Create(const APrompt: string; AForceOverwrite: Boolean; const AProvider, AModel, AApiKey: string);
begin
  inherited Create;
  FPrompt := APrompt;
  FForceOverwrite := AForceOverwrite;
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

procedure TAITranslationFlow.PrepareRequest(Verse: TVersiculo; out Messages: array of TLLMMessage);
begin
  if Length(Messages) >= 2 then
  begin
    Messages[0].Role := 'system';
    Messages[0].Content := FPrompt; // Ex: "Traduza o texto fornecido mantendo pontuação." (configurável)
    Messages[1].Role := 'user';
    if Assigned(Verse) then
    begin
      Messages[1].Content := Verse.TextoSimples;
      AIDebugLogFmt('[Translation] PrepareRequest for verse: %s', [Verse.Referencia]);
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

function TAITranslationFlow.BuildMessages(Verse: TVersiculo): TLLMMessageArray;
begin
  Result := nil;
  SetLength(Result, 2);
  PrepareRequest(Verse, Result);
end;

function TAITranslationFlow.ParseResponse(const RawContent: string; Verse: TVersiculo): TAIFlowResult;
var
  R: TAITranslationResult;
begin
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

procedure TAITranslationFlow.ApplyResult(ResultObj: TAIFlowResult; Verse: TVersiculo);
var
  R: TAITranslationResult;
begin
  if not (ResultObj is TAITranslationResult) then Exit;
  R := TAITranslationResult(ResultObj);
  
  if not Assigned(Verse) then
  begin
    AIDebugLog('[Translation] ApplyResult for verse: <nil> - Skipped');
    Exit;
  end;
  
  if (Verse.VersiculoPar = nil) then
  begin
    AIDebugLogFmt('[Translation] ApplyResult for verse: %s - No paired verse', [Verse.Referencia]);
    Exit; // necessita par destino
  end;
  
  AIDebugLogFmt('[Translation] ApplyResult for verse: %s', [Verse.Referencia]);
  AIDebugLogFmt('[Translation] Target verse current text: "%s"', [Verse.VersiculoPar.TextoSimples]);
  AIDebugLogFmt('[Translation] ForceOverwrite: %s', [BoolToStr(FForceOverwrite, True)]);
  AIDebugLogFmt('[Translation] Translated text: "%s"', [R.TranslatedText]);
  
  // CHANGED: Do NOT navigate and do NOT modify the verse!
  // Work directly with temporary verse objects created for batch processing.
  // Store the translation in memory only. The TAIProcessingBatch will store
  // the translated text and apply it to the database ONLY after user review.
  R.SetAcceptedDestinationText(R.TranslatedText);
  
  // The translation is now available in R.TranslatedText for the modal to display
  // and the batch processing to apply when user clicks Apply.
end;

function TAITranslationFlow.RequiresIndividualClient: Boolean;
begin
  Result := False;
end;

end.
