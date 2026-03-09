unit ai_flows;

{$mode objfpc}{$H+}

{ Basic abstractions for AI verse processing flows }

interface

uses
  Classes, SysUtils, llmclient, versiculo;

type
  // Generic job state (future use in manager)
  TAIJobState = (ajsPending, ajsFetching, ajsCompleted, ajsFailed, ajsCancelled);

  // Base result class for flow-specific parsed data
  TAIFlowResult = class
  end;

  // Forward declaration
  IAIVerseFlow = interface;
  IAIFlowConfig = interface;

  // Callback/event types (will be used by manager in future phases)
  TOnAIFlowApply = procedure(const Flow: IAIVerseFlow; Verse: TVersiculo; ResultObj: TAIFlowResult) of object;
  
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
    // Project holds GetProjectInfo() to fetch configuration
    function CreateFlowInstance(Project: TObject): IAIVerseFlow;
  end;

implementation

{ Currently only abstractions, concrete flows in ai_association_flow.pas }

end.
