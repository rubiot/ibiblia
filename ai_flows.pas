unit ai_flows;

{$mode objfpc}{$H+}

{ Basic abstractions for AI verse processing flows }

interface

uses
  Classes, SysUtils, llmclient, versiculo, project_config;

type
  // Generic job state (future use in manager)
  TAIJobState = (ajsPending, ajsFetching, ajsCompleted, ajsFailed, ajsCancelled);

  // Optional value records — replace boolean flag pairs
  TOptionalString = record
    Value: string;
    HasValue: Boolean;
  end;

  TOptionalInt = record
    Value: Integer;
    HasValue: Boolean;
  end;

  // Base result class for flow-specific parsed data
  TAIFlowResult = class
  private
    FAcceptedSourceText: TOptionalString;
    FAcceptedDestinationText: TOptionalString;
    FAcceptedPairs: TOptionalString;
    FAcceptedComments: TOptionalString;
    FAcceptedStatus: TOptionalInt;
  public
    ParseError: string; // non-empty when ParseResponse could not parse the response
    procedure CaptureAcceptedVerseState(Verse: TVersiculo);
    procedure SetAcceptedSourceText(const Value: string);
    procedure SetAcceptedDestinationText(const Value: string);
    procedure SetAcceptedPairs(const Value: string);
    procedure SetAcceptedComments(const Value: string);
    procedure SetAcceptedStatus(Value: Integer);

    property AcceptedSourceText: string read FAcceptedSourceText.Value;
    property AcceptedDestinationText: string read FAcceptedDestinationText.Value;
    property AcceptedPairs: string read FAcceptedPairs.Value;
    property AcceptedComments: string read FAcceptedComments.Value;
    property AcceptedStatus: Integer read FAcceptedStatus.Value;
    property AcceptedSourceTextSet: Boolean read FAcceptedSourceText.HasValue;
    property AcceptedDestinationTextSet: Boolean read FAcceptedDestinationText.HasValue;
    property AcceptedPairsSet: Boolean read FAcceptedPairs.HasValue;
    property AcceptedCommentsSet: Boolean read FAcceptedComments.HasValue;
    property AcceptedStatusSet: Boolean read FAcceptedStatus.HasValue;
  end;

  // Forward declaration
  IAIVerseFlow = interface;
  IAIFlowConfig = interface;
  IAIFlowContext = interface;

  // Callback/event types (will be used by manager in future phases)
  TOnAIFlowApply = procedure(const Flow: IAIVerseFlow; Verse: TVersiculo; ResultObj: TAIFlowResult) of object;

  IAIFlowContext = interface
    ['{8D2EA80B-78F5-44A4-B6F9-EA4C5E8456A3}']
    function GetProjectInfo(Key: string): string;
  end;

  TAIFlowContext = class(TInterfacedObject, IAIFlowContext)
  private
    FConfig: TProjectConfig;
  public
    constructor Create(AConfig: TProjectConfig);
    function GetProjectInfo(Key: string): string;
  end;
  
  // Configuration field descriptor
  TAIFlowConfigField = record
    Key: string;              // Configuration key (e.g., 'ai.flow.translation.target_language')
    DisplayName: string;      // Human-readable name for UI
    FieldType: string;        // 'string', 'boolean', 'integer', 'memo', 'model', 'password', 'provider'
    DefaultValue: string;     // Default value
    Description: string;      // Help text / description
    Required: Boolean;        // Whether field is required
  end;
  
  TAIFlowConfigFieldArray = array of TAIFlowConfigField;

  // Interface every AI processing flow must implement
  IAIVerseFlow = interface
    ['{3D2F7A7E-7E15-4F5C-9AA9-6F5C4BA1D0B2}']
    function Name: string;
    // Prepare messages for the LLM request (should be pure / no side effects)
    procedure PrepareRequest(Verse: TVersiculo; out Messages: array of TLLMMessage); overload; // discouraged (fixed len)
    function BuildMessages(Verse: TVersiculo): TLLMMessageArray; // preferred dynamic builder
    // Parse raw LLM content (JSON/string) into a structured result
    function ParseResponse(const RawContent: string; Verse: TVersiculo): TAIFlowResult;
    // Apply the structured result to the verse (must run on main thread)
    procedure ApplyResult(ResultObj: TAIFlowResult; Verse: TVersiculo);
    // Optional: Provide model + prompt if owned by flow (else caller supplies client)
    function RequiresIndividualClient: Boolean;
    
    // Provider overrides (return empty for using project defaults)
    function ModelOverride: string;
    function ApiKeyOverride: string;
    function ProviderOverride: string;
  end;
  
  // Interface for flow configuration metadata and prompt building
  // Each flow should implement this to expose its specific configuration needs
  IAIFlowConfig = interface
    ['{8A3F2B5C-9D4E-4A1F-B2C6-7E8F9A0B1C2D}']
    // Return unique identifier for this flow (e.g., 'association', 'translation')
    function GetFlowId: string;
    
    // Return display name for UI (e.g., 'Association', 'Translation')
    function GetDisplayName: string;
    
    // Return description of what this flow does
    function GetDescription: string;
    
    // Return information about expected output format (for UI display)
    function GetOutputFormatInfo: string;
    
    // Return default customizable instructions (user-editable part)
    function GetDefaultCustomInstructions: string;
    
    // Return list of flow-specific configuration fields
    function GetConfigFields: TAIFlowConfigFieldArray;
    
    // Build final prompt from stored configuration
    // configValues: key-value pairs from project settings
    function BuildPromptFromConfig(const ConfigValues: array of const): string;
    
    // Create and configure flow instance from project settings
    function CreateFlowInstance(Context: IAIFlowContext): IAIVerseFlow;
  end;

function CreateAIFlowContext(AConfig: TProjectConfig): IAIFlowContext;

implementation

{ Currently only abstractions, concrete flows in ai_association_flow.pas }

constructor TAIFlowContext.Create(AConfig: TProjectConfig);
begin
  inherited Create;
  FConfig := AConfig;
end;

function TAIFlowContext.GetProjectInfo(Key: string): string;
begin
  if not Assigned(FConfig) then
    Exit('');

  Result := FConfig.ReadString(Key);
end;

function CreateAIFlowContext(AConfig: TProjectConfig): IAIFlowContext;
begin
  Result := TAIFlowContext.Create(AConfig);
end;

procedure TAIFlowResult.CaptureAcceptedVerseState(Verse: TVersiculo);
begin
  if not Assigned(Verse) then
    Exit;

  SetAcceptedSourceText(Verse.Texto);
  SetAcceptedPairs(Verse.Pares);

  if Assigned(Verse.VersiculoPar) then
    SetAcceptedDestinationText(Verse.VersiculoPar.Texto);
end;

procedure TAIFlowResult.SetAcceptedSourceText(const Value: string);
begin
  FAcceptedSourceText.Value := Value;
  FAcceptedSourceText.HasValue := True;
end;

procedure TAIFlowResult.SetAcceptedDestinationText(const Value: string);
begin
  FAcceptedDestinationText.Value := Value;
  FAcceptedDestinationText.HasValue := True;
end;

procedure TAIFlowResult.SetAcceptedPairs(const Value: string);
begin
  FAcceptedPairs.Value := Value;
  FAcceptedPairs.HasValue := True;
end;

procedure TAIFlowResult.SetAcceptedComments(const Value: string);
begin
  FAcceptedComments.Value := Value;
  FAcceptedComments.HasValue := True;
end;

procedure TAIFlowResult.SetAcceptedStatus(Value: Integer);
begin
  FAcceptedStatus.Value := Value;
  FAcceptedStatus.HasValue := True;
end;

end.
