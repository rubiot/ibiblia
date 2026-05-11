unit ai_flow_config;

{$mode objfpc}{$H+}

{ Common configuration management for AI flows }

interface

uses
  Classes, SysUtils, ai_flows;

type
  // Common configuration keys (shared across all flows)
  TAICommonConfigKeys = record
    Model: string;
    ApiKey: string;
    MaxParallel: string;
    MaxRetries: string;
    RetryDelay: string;
    AutoSaveInterval: string;
  end;

  TAIFlowExample = record
    ReferenceId: string;
    ReferenceCaption: string;
    SourceText: string;
    TargetText: string;
    ExpectedResponse: string;
  end;

  TAIFlowExampleArray = array of TAIFlowExample;

const
  // Common configuration keys
  AI_COMMON_CONFIG: TAICommonConfigKeys = (
    Model: 'ai.common.model';
    ApiKey: 'ai.common.api_key';
    MaxParallel: 'ai.common.max_parallel';
    MaxRetries: 'ai.common.max_retries';
    RetryDelay: 'ai.common.retry_delay';
    AutoSaveInterval: 'ai.common.autosave_interval';
  );
  
  // Legacy keys for migration
  LEGACY_MODEL_KEY = 'ai_associator.model';
  LEGACY_API_KEY = 'ai_associator.api_key';
  LEGACY_PROMPT_KEY = 'ai_associator.prompt';
  LEGACY_MAX_PARALLEL_KEY = 'ai_associator.max_parallel';
  LEGACY_MAX_RETRIES_KEY = 'ai_associator.max_retries';
  LEGACY_RETRY_DELAY_KEY = 'ai_associator.retry_delay';
  LEGACY_AUTOSAVE_INTERVAL_KEY = 'ai_associator.autosave_interval';

  AI_ASSOCIATION_EXAMPLES_KEY = 'ai.flow.association.examples';
  AI_TRANSLATION_EXAMPLES_KEY = 'ai.flow.translation.examples';

// Extract only the customizable part from a full prompt (for backward compatibility)
function ExtractCustomInstructions(const FullPrompt, DefaultPrompt: string): string;

function GetFlowExamplesConfigKey(const FlowId: string): string;
function LoadFlowExamples(const SerializedExamples: string): TAIFlowExampleArray;
function SaveFlowExamples(const Examples: TAIFlowExampleArray): string;
function IndexOfFlowExampleByReference(const Examples: TAIFlowExampleArray; const ReferenceId: string): Integer;
function UpsertFlowExample(const Examples: TAIFlowExampleArray; const Example: TAIFlowExample; ReplaceExisting: Boolean): TAIFlowExampleArray;
function RemoveFlowExample(const Examples: TAIFlowExampleArray; const ReferenceId: string): TAIFlowExampleArray;
function FormatFlowExampleSummary(const Example: TAIFlowExample; const MaxPreviewLength: Integer = 60): string;

// Get flow configuration by ID
function GetFlowConfigById(const FlowId: string): IAIFlowConfig;

implementation

uses
  fpjson, jsonparser, ai_association_flow, ai_translation_flow;

function TrimPreview(const Value: string; const MaxPreviewLength: Integer): string;
var
  Normalized: string;
begin
  Normalized := Trim(Value.Replace(#13#10, ' ').Replace(#10, ' ').Replace(#13, ' '));
  if (MaxPreviewLength > 0) and (Length(Normalized) > MaxPreviewLength) then
    Result := Copy(Normalized, 1, MaxPreviewLength - 3) + '...'
  else
    Result := Normalized;
end;

function ExtractCustomInstructions(const FullPrompt, DefaultPrompt: string): string;
const
  SEPARATOR = '=== Additional Project-Specific Instructions ===';
var
  SepPos: Integer;
begin
  Result := '';
  
  // If prompt is empty or same as default, no custom instructions
  if (Trim(FullPrompt) = '') or (Trim(FullPrompt) = Trim(DefaultPrompt)) then
    Exit;
  
  // Look for separator
  SepPos := Pos(SEPARATOR, FullPrompt);
  if SepPos > 0 then
  begin
    // Extract everything after separator
    Result := Trim(Copy(FullPrompt, SepPos + Length(SEPARATOR), MaxInt));
  end
  else
  begin
    // No separator found - might be old format
    // Check if it starts with default prompt
    if Pos(DefaultPrompt, FullPrompt) = 1 then
    begin
      // Remove default part and keep the rest
      Result := Trim(Copy(FullPrompt, Length(DefaultPrompt) + 1, MaxInt));
    end
    else
    begin
      // Completely different - treat entire prompt as custom
      Result := Trim(FullPrompt);
    end;
  end;
end;

function GetFlowExamplesConfigKey(const FlowId: string): string;
begin
  case LowerCase(Trim(FlowId)) of
    'association': Result := AI_ASSOCIATION_EXAMPLES_KEY;
    'translation': Result := AI_TRANSLATION_EXAMPLES_KEY;
  else
    raise Exception.CreateFmt('Unknown flow ID for examples: %s', [FlowId]);
  end;
end;

function LoadFlowExamples(const SerializedExamples: string): TAIFlowExampleArray;
var
  Parser: TJSONParser;
  Data: TJSONData;
  JsonArray: TJSONArray;
  JsonObject: TJSONObject;
  I: Integer;
begin
  Result := nil;
  if Trim(SerializedExamples) = '' then
    Exit;

  Parser := TJSONParser.Create(SerializedExamples, []);
  try
    Data := Parser.Parse;
    try
      if not (Data is TJSONArray) then
        Exit;

      JsonArray := TJSONArray(Data);
      SetLength(Result, JsonArray.Count);
      for I := 0 to JsonArray.Count - 1 do
      begin
        if not (JsonArray.Items[I] is TJSONObject) then
          Continue;

        JsonObject := TJSONObject(JsonArray.Items[I]);
        Result[I].ReferenceId := JsonObject.Get('reference_id', '');
        Result[I].ReferenceCaption := JsonObject.Get('reference_caption', '');
        Result[I].SourceText := JsonObject.Get('source_text', '');
        Result[I].TargetText := JsonObject.Get('target_text', '');
        Result[I].ExpectedResponse := JsonObject.Get('expected_response', '');
      end;
    finally
      Data.Free;
    end;
  finally
    Parser.Free;
  end;
end;

function SaveFlowExamples(const Examples: TAIFlowExampleArray): string;
var
  JsonArray: TJSONArray;
  JsonObject: TJSONObject;
  I: Integer;
begin
  JsonArray := TJSONArray.Create;
  try
    for I := 0 to High(Examples) do
    begin
      JsonObject := TJSONObject.Create;
      JsonObject.Add('reference_id', Examples[I].ReferenceId);
      JsonObject.Add('reference_caption', Examples[I].ReferenceCaption);
      JsonObject.Add('source_text', Examples[I].SourceText);
      JsonObject.Add('target_text', Examples[I].TargetText);
      JsonObject.Add('expected_response', Examples[I].ExpectedResponse);
      JsonArray.Add(JsonObject);
    end;

    Result := JsonArray.AsJSON;
  finally
    JsonArray.Free;
  end;
end;

function IndexOfFlowExampleByReference(const Examples: TAIFlowExampleArray; const ReferenceId: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Examples) do
    if SameText(Examples[I].ReferenceId, ReferenceId) then
      Exit(I);
end;

function UpsertFlowExample(const Examples: TAIFlowExampleArray; const Example: TAIFlowExample; ReplaceExisting: Boolean): TAIFlowExampleArray;
var
  Index: Integer;
begin
  Result := Copy(Examples);
  Index := IndexOfFlowExampleByReference(Result, Example.ReferenceId);
  if Index >= 0 then
  begin
    if ReplaceExisting then
      Result[Index] := Example;
    Exit;
  end;

  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := Example;
end;

function RemoveFlowExample(const Examples: TAIFlowExampleArray; const ReferenceId: string): TAIFlowExampleArray;
var
  I, Index: Integer;
begin
  Result := Copy(Examples);
  Index := IndexOfFlowExampleByReference(Result, ReferenceId);
  if Index < 0 then
    Exit;

  for I := Index to High(Result) - 1 do
    Result[I] := Result[I + 1];
  SetLength(Result, Length(Result) - 1);
end;

function FormatFlowExampleSummary(const Example: TAIFlowExample; const MaxPreviewLength: Integer): string;
var
  Preview: string;
begin
  Preview := TrimPreview(Example.SourceText, MaxPreviewLength);
  if Preview <> '' then
    Result := Format('%s - %s', [Example.ReferenceCaption, Preview])
  else
    Result := Example.ReferenceCaption;
end;

function GetFlowConfigById(const FlowId: string): IAIFlowConfig;
begin
  Result := nil;
  
  case LowerCase(FlowId) of
    'association': Result := TAIAssociationFlowConfig.Create;
    'translation': Result := TAITranslationFlowConfig.Create;
  else
    raise Exception.CreateFmt('Unknown flow ID: %s', [FlowId]);
  end;
end;

end.
