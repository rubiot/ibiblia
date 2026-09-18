unit ai_settings_types;

{$mode objfpc}{$H+}

interface

uses ai_credential_store;

type
  TAICommonSetting = (aisModel, aisMaxParallel, aisMaxRetries, aisRetryDelay,
    aisAutoSaveInterval);
  TAISettingValues = array[TAICommonSetting] of string;
  TAISettingSource = (asoDefault, asoGlobal, asoProject, asoFlow);
  TAISettingSources = array[TAICommonSetting] of TAISettingSource;

const
  AI_KEY_INHERIT_CONFIG = 'ai.common.inherit.api_key';

function AISettingConfigKey(Setting: TAICommonSetting): string;
function AISettingLegacyKey(Setting: TAICommonSetting): string;
function AISettingInheritanceKey(Setting: TAICommonSetting): string;
function AISettingDefault(Setting: TAICommonSetting): string;
function AICredentialProviderKey(Scope: TAICredentialScope): string;
function CanonicalAIProvider(const Provider: string): string;
function AIProviderForModel(const Model: string): string;

implementation

uses SysUtils, llmclient;

const
  SettingSuffixes: array[TAICommonSetting] of string = ('model',
    'max_parallel', 'max_retries', 'retry_delay', 'autosave_interval');
  SettingDefaults: TAISettingValues = ('', '2', '3', '1000', '10');

function AISettingConfigKey(Setting: TAICommonSetting): string;
begin
  Result := 'ai.common.' + SettingSuffixes[Setting];
end;

function AISettingLegacyKey(Setting: TAICommonSetting): string;
begin
  Result := 'ai_associator.' + SettingSuffixes[Setting];
end;

function AISettingInheritanceKey(Setting: TAICommonSetting): string;
begin
  Result := 'ai.common.inherit.' + SettingSuffixes[Setting];
end;

function AISettingDefault(Setting: TAICommonSetting): string;
begin
  Result := SettingDefaults[Setting];
end;

function AICredentialProviderKey(Scope: TAICredentialScope): string;
begin
  case Scope of
    acsCommon: Result := 'ai.common.credential_provider';
    acsAssociation: Result := 'ai.flow.association.credential_provider';
    acsTranslation: Result := 'ai.flow.translation.credential_provider';
  end;
end;

function CanonicalAIProvider(const Provider: string): string;
begin
  if SameText(Trim(Provider), 'OpenAI') then Result := 'OpenAI'
  else if SameText(Trim(Provider), 'Google') then Result := 'Google'
  else if SameText(Trim(Provider), 'DeepSeek') then Result := 'DeepSeek'
  else Result := '';
end;

function AIProviderForModel(const Model: string): string;
var Info: TLLMModelInfo;
begin
  Result := '';
  if Trim(Model) = '' then Exit;
  Info := GetModelInfo(Trim(Model));
  case Info.Backend of
    lbOpenAI: Result := 'OpenAI';
    lbGoogle: Result := 'Google';
    lbDeepSeek: Result := 'DeepSeek';
  end;
end;

end.
