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

// Extract only the customizable part from a full prompt (for backward compatibility)
function ExtractCustomInstructions(const FullPrompt, DefaultPrompt: string): string;

// Get flow configuration by ID
function GetFlowConfigById(const FlowId: string): IAIFlowConfig;

implementation

uses
  ai_association_flow, ai_translation_flow;

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
