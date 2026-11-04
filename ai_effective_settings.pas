unit ai_effective_settings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ai_settings_types, ai_global_settings, ai_credential_store,
  project_config, ai_flows;

type
  TAIProjectAISettings = record
    Values: TAISettingValues;
    Inherit: array[TAICommonSetting] of Boolean;
    ApiKey: string;
    InheritApiKey, ApiKeyConfigured: Boolean;
    ApiKeyError, ApiKeyProvider: string;
  end;

  TAICommonSettings = record
    Model, Provider: string;
    MaxParallel, MaxRetries, RetryDelay, AutoSaveInterval: Integer;
    Sources: TAISettingSources;
  end;

  TAIResolvedConnection = record
    Provider, Model, ApiKey: string;
    ModelSource, CredentialSource: TAISettingSource;
  end;

function ReadProjectAISettings(Config: TProjectConfig): TAIProjectAISettings;
procedure StageProjectAISettings(Config: TProjectConfig;
  const Draft: TAIProjectAISettings);
procedure ValidateAISettingValues(const Values: TAISettingValues);
function ResolveCommonAISettings(const Draft: TAIProjectAISettings;
  Global: TAIGlobalSettings): TAICommonSettings; overload;
function ResolveCommonAISettings(Config: TProjectConfig;
  Global: TAIGlobalSettings = nil): TAICommonSettings; overload;
function ResolveAIConnection(const Draft: TAIProjectAISettings;
  Global: TAIGlobalSettings): TAIResolvedConnection; overload;
function ResolveAIConnection(Config: TProjectConfig; Global: TAIGlobalSettings = nil;
  Flow: IAIVerseFlow = nil; Scope: TAICredentialScope = acsCommon): TAIResolvedConnection; overload;
function ProjectCredentialProvider(Config: TProjectConfig;
  Scope: TAICredentialScope; const CommonModel: string): string;
procedure BindProjectAICredentials(Config: TProjectConfig);
procedure MigrateLegacyAISettings(Config: TProjectConfig);

implementation

uses ai_flow_config, ai_association_flow;

resourcestring
  SAIConfigurationMissing = 'Configure the AI model and API key in LLM Options before processing.';
  SAIProjectKeyMissing = 'The project API key is unavailable. Configure it in LLM Options or explicitly choose the global default.';
  SAICredentialProviderMismatch = 'The configured API key belongs to a different provider. Configure a key for %s or choose the global default.';
  SAIInvalidParameter = 'Invalid AI setting: %s.';
  SAIUnsupportedProvider = 'The selected AI provider is not supported.';
  SAIUnboundCredential = 'The provider of this project API key is unknown. Re-enter the key for the selected provider or choose the global default.';

function IsTrue(const Value: string): Boolean;
begin
  Result := SameText(Trim(Value), 'true') or (Trim(Value) = '1') or
    SameText(Trim(Value), 'yes');
end;

function ReadLocalValue(Config: TProjectConfig; Setting: TAICommonSetting): string;
begin
  Result := '';
  if not Assigned(Config) then Exit;
  Result := Trim(Config.ReadString(AISettingConfigKey(Setting)));
  if Result = '' then
    Result := Trim(Config.ReadString(AISettingLegacyKey(Setting)));
end;

function ProjectCredentialProvider(Config: TProjectConfig;
  Scope: TAICredentialScope; const CommonModel: string): string;
var Prefix, Model, ExplicitProvider: string;
begin
  Result := '';
  if not Assigned(Config) then Exit;
  ExplicitProvider := Trim(Config.ReadString(AICredentialProviderKey(Scope)));
  if ExplicitProvider <> '' then
  begin
    Result := CanonicalAIProvider(ExplicitProvider);
    if Result = '' then raise EAICredentialError.Create(SAIUnsupportedProvider);
    Exit;
  end;
  Model := ReadLocalValue(Config, aisModel);
  if Model = '' then Model := CommonModel;
  if Scope <> acsCommon then
  begin
    if Scope = acsAssociation then Prefix := 'ai.flow.association.'
    else Prefix := 'ai.flow.translation.';
    ExplicitProvider := Trim(Config.ReadString(Prefix + 'provider'));
    if ExplicitProvider <> '' then
    begin
      Result := CanonicalAIProvider(ExplicitProvider);
      if Result = '' then raise EAICredentialError.Create(SAIUnsupportedProvider);
      Exit;
    end;
    if Trim(Config.ReadString(Prefix + 'model')) <> '' then
      Model := Config.ReadString(Prefix + 'model');
  end;
  Result := AIProviderForModel(Model);
end;

function ReadProjectAISettings(Config: TProjectConfig): TAIProjectAISettings;
var Setting: TAICommonSetting; Flag: string;
begin
  Result := Default(TAIProjectAISettings);
  for Setting in TAICommonSetting do
  begin
    Result.Values[Setting] := ReadLocalValue(Config, Setting);
    Flag := '';
    if Assigned(Config) then Flag := Config.ReadString(AISettingInheritanceKey(Setting));
    if Flag = '' then Result.Inherit[Setting] := Result.Values[Setting] = ''
    else Result.Inherit[Setting] := IsTrue(Flag);
  end;
  Result.InheritApiKey := True;
  if not Assigned(Config) then Exit;
  Result.ApiKeyConfigured := Config.AICredentials.HasConfiguredCredential(acsCommon);
  Flag := Config.ReadString(AI_KEY_INHERIT_CONFIG);
  if Flag = '' then Result.InheritApiKey := not Result.ApiKeyConfigured
  else Result.InheritApiKey := IsTrue(Flag);
  Config.AICredentials.TryResolve(acsCommon, Result.ApiKey, Result.ApiKeyError);
  try
    Result.ApiKeyProvider := ProjectCredentialProvider(Config, acsCommon,
      Result.Values[aisModel]);
  except
    on E: EAICredentialError do Result.ApiKeyError := E.Message;
  end;
end;

procedure ValidateAISettingValues(const Values: TAISettingValues);
var Setting: TAICommonSetting; Value: Integer;
begin
  for Setting in TAICommonSetting do
  begin
    if (Setting = aisModel) or (Trim(Values[Setting]) = '') then Continue;
    if not TryStrToInt(Trim(Values[Setting]), Value) then
      raise EAICredentialError.CreateFmt(SAIInvalidParameter, [AISettingConfigKey(Setting)]);
    if ((Setting = aisMaxParallel) and (Value <= 0)) or
       ((Setting in [aisMaxRetries, aisRetryDelay]) and (Value < 0)) then
      raise EAICredentialError.CreateFmt(SAIInvalidParameter, [AISettingConfigKey(Setting)]);
  end;
end;

function ResolveCommonAISettings(const Draft: TAIProjectAISettings;
  Global: TAIGlobalSettings): TAICommonSettings;
var Setting: TAICommonSetting; Values: TAISettingValues; OwnGlobal: Boolean;
begin
  OwnGlobal := not Assigned(Global);
  if OwnGlobal then Global := TAIGlobalSettings.Create;
  try
    Result := Default(TAICommonSettings);
    for Setting in TAICommonSetting do
    begin
      Values[Setting] := '';
      if not Draft.Inherit[Setting] then
      begin
        Values[Setting] := Trim(Draft.Values[Setting]);
        Result.Sources[Setting] := asoProject;
      end
      else
      begin
        Values[Setting] := Trim(Global.ReadValue(Setting));
        Result.Sources[Setting] := asoGlobal;
        if Values[Setting] = '' then
        begin
          Values[Setting] := AISettingDefault(Setting);
          Result.Sources[Setting] := asoDefault;
        end;
      end;
      if (Setting <> aisModel) and (Values[Setting] = '') then
        raise EAICredentialError.CreateFmt(SAIInvalidParameter, [AISettingConfigKey(Setting)]);
    end;
    ValidateAISettingValues(Values);
    Result.Model := Values[aisModel];
    Result.Provider := AIProviderForModel(Result.Model);
    Result.MaxParallel := StrToInt(Values[aisMaxParallel]);
    Result.MaxRetries := StrToInt(Values[aisMaxRetries]);
    Result.RetryDelay := StrToInt(Values[aisRetryDelay]);
    Result.AutoSaveInterval := StrToInt(Values[aisAutoSaveInterval]);
  finally
    if OwnGlobal then Global.Free;
  end;
end;

function ResolveCommonAISettings(Config: TProjectConfig;
  Global: TAIGlobalSettings): TAICommonSettings;
begin
  Result := ResolveCommonAISettings(ReadProjectAISettings(Config), Global);
end;

function ResolveConnection(const Draft: TAIProjectAISettings;
  Global: TAIGlobalSettings; Flow: IAIVerseFlow;
  const FlowKeyProvider: string): TAIResolvedConnection;
var Common: TAICommonSettings; FlowKey, FlowProvider: string;
begin
  Common := ResolveCommonAISettings(Draft, Global);
  Result := Default(TAIResolvedConnection);
  Result.Model := Common.Model;
  Result.ModelSource := Common.Sources[aisModel];
  FlowKey := '';
  FlowProvider := '';
  if Assigned(Flow) then
  begin
    if Trim(Flow.ModelOverride) <> '' then
    begin
      Result.Model := Trim(Flow.ModelOverride);
      Result.ModelSource := asoFlow;
    end;
    FlowProvider := Trim(Flow.ProviderOverride);
    FlowKey := Trim(Flow.ApiKeyOverride);
  end;
  Result.Provider := AIProviderForModel(Result.Model);
  if FlowProvider <> '' then
  begin
    Result.Provider := CanonicalAIProvider(FlowProvider);
    if Result.Provider = '' then raise EAICredentialError.Create(SAIUnsupportedProvider);
  end;
  if (Result.Model = '') or (Result.Provider = '') then
    raise EAICredentialError.Create(SAIConfigurationMissing);
  if FlowKey <> '' then
  begin
    if FlowKeyProvider = '' then raise EAICredentialError.Create(SAIUnboundCredential);
    if not SameText(FlowKeyProvider, Result.Provider) then
      raise EAICredentialError.CreateFmt(SAICredentialProviderMismatch, [Result.Provider]);
    Result.ApiKey := FlowKey;
    Result.CredentialSource := asoFlow;
  end
  else if not Draft.InheritApiKey and
    ((Draft.ApiKeyProvider = '') or SameText(Draft.ApiKeyProvider, Result.Provider) or
      SameText(Common.Provider, Result.Provider)) then
  begin
    if Draft.ApiKeyProvider = '' then
      raise EAICredentialError.Create(SAIUnboundCredential);
    if not SameText(Draft.ApiKeyProvider, Result.Provider) then
      raise EAICredentialError.CreateFmt(SAICredentialProviderMismatch, [Result.Provider]);
    if (Draft.ApiKeyError <> '') or (Trim(Draft.ApiKey) = '') then
      raise EAICredentialError.Create(SAIProjectKeyMissing);
    Result.ApiKey := Trim(Draft.ApiKey);
    Result.CredentialSource := asoProject;
  end
  else
  begin
    Result.ApiKey := Global.ResolveCredential(Result.Provider);
    Result.CredentialSource := asoGlobal;
  end;
  if Result.ApiKey = '' then raise EAICredentialError.Create(SAIConfigurationMissing);
end;

function ResolveAIConnection(const Draft: TAIProjectAISettings;
  Global: TAIGlobalSettings): TAIResolvedConnection;
var OwnGlobal: Boolean;
begin
  OwnGlobal := not Assigned(Global);
  if OwnGlobal then Global := TAIGlobalSettings.Create;
  try
    Result := ResolveConnection(Draft, Global, nil, '');
  finally
    if OwnGlobal then Global.Free;
  end;
end;

function ResolveAIConnection(Config: TProjectConfig; Global: TAIGlobalSettings;
  Flow: IAIVerseFlow; Scope: TAICredentialScope): TAIResolvedConnection;
var Draft: TAIProjectAISettings;
  OwnGlobal: Boolean; KeyProvider: string;
begin
  OwnGlobal := not Assigned(Global);
  if OwnGlobal then Global := TAIGlobalSettings.Create;
  try
    Draft := ReadProjectAISettings(Config);
    KeyProvider := '';
    if Assigned(Flow) and (Flow.ApiKeyOverride <> '') then
    begin
      KeyProvider := ProjectCredentialProvider(Config, Scope, Draft.Values[aisModel]);
    end;
    Result := ResolveConnection(Draft, Global, Flow, KeyProvider);
  finally
    if OwnGlobal then Global.Free;
  end;
end;

procedure BindProjectAICredentials(Config: TProjectConfig);
var Scope: TAICredentialScope; Provider, Model: string;
begin
  if not Assigned(Config) then Exit;
  Model := ReadLocalValue(Config, aisModel);
  for Scope in TAICredentialScope do
    if Config.AICredentials.HasConfiguredCredential(Scope) and
       (Config.ReadString(AICredentialProviderKey(Scope)) = '') then
    begin
      Provider := ProjectCredentialProvider(Config, Scope, Model);
      if Provider <> '' then Config.WriteString(AICredentialProviderKey(Scope), Provider);
    end;
end;

procedure StageProjectAISettings(Config: TProjectConfig;
  const Draft: TAIProjectAISettings);
var Setting: TAICommonSetting; Values: TAISettingValues;
begin
  if not Assigned(Config) then Exit;
  Values := Draft.Values;
  for Setting in TAICommonSetting do
    if Draft.Inherit[Setting] then Values[Setting] := '';
  ValidateAISettingValues(Values);
  BindProjectAICredentials(Config);
  for Setting in TAICommonSetting do
  begin
    Config.WriteString(AISettingInheritanceKey(Setting), BoolToStr(Draft.Inherit[Setting], True));
    if not Draft.Inherit[Setting] then
      Config.WriteString(AISettingConfigKey(Setting), Trim(Draft.Values[Setting]));
  end;
  Config.WriteString(AI_KEY_INHERIT_CONFIG, BoolToStr(Draft.InheritApiKey, True));
  if (Draft.ApiKeyError = '') or (Draft.ApiKey <> '') then
  begin
    Config.AICredentials.Stage(acsCommon, Draft.ApiKey);
    if Draft.ApiKeyProvider <> '' then
      Config.WriteString(AICredentialProviderKey(acsCommon), Draft.ApiKeyProvider);
  end;
end;

procedure MigrateLegacyAISettings(Config: TProjectConfig);
var Setting: TAICommonSetting; Value, LegacyPrompt: string;
  FlowConfig: IAIFlowConfig;
begin
  if not Assigned(Config) then Exit;
  for Setting in TAICommonSetting do
    if Config.ReadString(AISettingConfigKey(Setting)) = '' then
    begin
      Value := Config.ReadString(AISettingLegacyKey(Setting));
      if Value <> '' then Config.WriteString(AISettingConfigKey(Setting), Value);
    end;
  LegacyPrompt := Config.ReadString(LEGACY_PROMPT_KEY);
  if (LegacyPrompt <> '') and
     (Config.ReadString(AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS) = '') then
  begin
    FlowConfig := GetFlowConfigById('association');
    Value := ExtractCustomInstructions(LegacyPrompt, FlowConfig.BuildPromptFromConfig([]));
    if Value = '' then Value := FlowConfig.GetDefaultCustomInstructions;
    Config.WriteString(AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, Value);
  end;
  BindProjectAICredentials(Config);
end;

end.
