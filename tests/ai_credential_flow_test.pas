unit ai_credential_flow_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ai_flows, ai_flow_config,
  ai_credential_store, ai_association_flow, ai_translation_flow;

type
  TCredentialFlowContext = class(TInterfacedObject, IAIFlowContext)
  private
    FInfo: TStringList;
    FConfigured: array[TAICredentialScope] of Boolean;
    FUnavailable: array[TAICredentialScope] of Boolean;
    FSecrets: array[TAICredentialScope] of string;
    FResolveCount: array[TAICredentialScope] of Integer;
    FHasCount: array[TAICredentialScope] of Integer;
    FLegacyReadCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetInfo(const Key, Value: string);
    procedure SetCredential(Scope: TAICredentialScope;
      Configured: Boolean; const Secret: string; Unavailable: Boolean = False);
    function GetProjectInfo(Key: string): string;
    function ResolveCredential(Scope: TAICredentialScope): string;
    function HasCredential(Scope: TAICredentialScope): Boolean;
    function ResolutionCount(Scope: TAICredentialScope): Integer;
    function PresenceCheckCount(Scope: TAICredentialScope): Integer;
    property LegacyReadCount: Integer read FLegacyReadCount;
  end;

  TAICredentialFlowTests = class(TTestCase)
  private
    FContextObject: TCredentialFlowContext;
    FContext: IAIFlowContext;
    function FlowId(Scope: TAICredentialScope): string;
    function MakeFlow(Scope: TAICredentialScope): IAIVerseFlow;
    procedure SetOverride(Scope: TAICredentialScope; const Enabled: string);
    procedure SetProviderAndModel(Scope: TAICredentialScope);
    procedure AssertOtherScopesWereNotResolved(Scope: TAICredentialScope);
    procedure CheckExplicitOverride(Scope: TAICredentialScope);
    procedure CheckImplicitOverride(Scope: TAICredentialScope);
    procedure CheckDisabledUnavailableOverride(Scope: TAICredentialScope);
    procedure CheckEnabledUnavailableOverride(Scope: TAICredentialScope);
    procedure CheckOptionalEmptyCredential(Scope: TAICredentialScope);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AssociationExplicitOverrideResolvesLocalCredential;
    procedure TranslationExplicitOverrideResolvesLocalCredential;
    procedure AssociationImplicitOverrideDetectsLocalCredential;
    procedure TranslationImplicitOverrideDetectsLocalCredential;
    procedure AssociationDisabledOverrideIgnoresUnavailableReference;
    procedure TranslationDisabledOverrideIgnoresUnavailableReference;
    procedure AssociationEnabledMissingCredentialBlocksFallback;
    procedure TranslationEnabledMissingCredentialBlocksFallback;
    procedure AssociationModelOverrideCanUseOptionalGlobalCredential;
    procedure TranslationModelOverrideCanUseOptionalGlobalCredential;
  end;

implementation

constructor TCredentialFlowContext.Create;
begin
  inherited Create;
  FInfo := TStringList.Create;
end;

destructor TCredentialFlowContext.Destroy;
begin
  FInfo.Free;
  inherited Destroy;
end;

procedure TCredentialFlowContext.SetInfo(const Key, Value: string);
begin
  FInfo.Values[Key] := Value;
end;

procedure TCredentialFlowContext.SetCredential(Scope: TAICredentialScope;
  Configured: Boolean; const Secret: string; Unavailable: Boolean);
begin
  FConfigured[Scope] := Configured;
  FSecrets[Scope] := Secret;
  FUnavailable[Scope] := Unavailable;
end;

function TCredentialFlowContext.GetProjectInfo(Key: string): string;
begin
  if IsLegacyAICredentialKey(Key) then
  begin
    Inc(FLegacyReadCount);
    raise Exception.Create('Flows must resolve credentials through the credential service');
  end;
  Result := FInfo.Values[Key];
end;

function TCredentialFlowContext.ResolveCredential(
  Scope: TAICredentialScope): string;
begin
  Inc(FResolveCount[Scope]);
  if FUnavailable[Scope] then
    raise EAICredentialError.Create('Configured test credential is unavailable locally');
  Result := FSecrets[Scope];
end;

function TCredentialFlowContext.HasCredential(
  Scope: TAICredentialScope): Boolean;
begin
  Inc(FHasCount[Scope]);
  Result := FConfigured[Scope];
end;

function TCredentialFlowContext.ResolutionCount(
  Scope: TAICredentialScope): Integer;
begin
  Result := FResolveCount[Scope];
end;

function TCredentialFlowContext.PresenceCheckCount(
  Scope: TAICredentialScope): Integer;
begin
  Result := FHasCount[Scope];
end;

procedure TAICredentialFlowTests.SetUp;
begin
  inherited SetUp;
  FContextObject := TCredentialFlowContext.Create;
  FContext := FContextObject;
  FContextObject.SetCredential(acsCommon, True, 'test-only-common');
end;

procedure TAICredentialFlowTests.TearDown;
begin
  FContext := nil;
  FContextObject := nil;
  inherited TearDown;
end;

function TAICredentialFlowTests.FlowId(Scope: TAICredentialScope): string;
begin
  case Scope of
    acsAssociation: Result := 'association';
    acsTranslation: Result := 'translation';
  else
    raise Exception.Create('Test helper requires a flow credential scope');
  end;
end;

function TAICredentialFlowTests.MakeFlow(Scope: TAICredentialScope): IAIVerseFlow;
var
  Config: IAIFlowConfig;
begin
  case Scope of
    acsAssociation: Config := TAIAssociationFlowConfig.Create;
    acsTranslation: Config := TAITranslationFlowConfig.Create;
  else
    raise Exception.Create('Test helper requires a flow credential scope');
  end;
  Result := Config.CreateFlowInstance(FContext);
end;

procedure TAICredentialFlowTests.SetOverride(Scope: TAICredentialScope;
  const Enabled: string);
begin
  FContextObject.SetInfo(GetFlowOverrideEnabledConfigKey(FlowId(Scope)), Enabled);
end;

procedure TAICredentialFlowTests.SetProviderAndModel(Scope: TAICredentialScope);
begin
  FContextObject.SetInfo('ai.flow.' + FlowId(Scope) + '.provider', 'DeepSeek');
  FContextObject.SetInfo('ai.flow.' + FlowId(Scope) + '.model', 'test-override-model');
end;

procedure TAICredentialFlowTests.AssertOtherScopesWereNotResolved(
  Scope: TAICredentialScope);
var
  Other: TAICredentialScope;
begin
  for Other in TAICredentialScope do
    if Other <> Scope then
      AssertEquals('Flow must not resolve a different credential scope',
        0, FContextObject.ResolutionCount(Other));
  AssertEquals('Plaintext project credential reads are forbidden',
    0, FContextObject.LegacyReadCount);
end;

procedure TAICredentialFlowTests.CheckExplicitOverride(Scope: TAICredentialScope);
var
  Flow: IAIVerseFlow;
  ExpectedSecret: string;
begin
  ExpectedSecret := 'test-only-' + FlowId(Scope);
  SetOverride(Scope, 'true');
  SetProviderAndModel(Scope);
  FContextObject.SetCredential(Scope, True, ExpectedSecret);
  Flow := MakeFlow(Scope);
  AssertEquals(ExpectedSecret, Flow.ApiKeyOverride);
  AssertEquals('DeepSeek', Flow.ProviderOverride);
  AssertEquals('test-override-model', Flow.ModelOverride);
  AssertEquals(1, FContextObject.ResolutionCount(Scope));
  AssertOtherScopesWereNotResolved(Scope);
end;

procedure TAICredentialFlowTests.CheckImplicitOverride(Scope: TAICredentialScope);
var
  Flow: IAIVerseFlow;
  ExpectedSecret: string;
begin
  ExpectedSecret := 'test-only-' + FlowId(Scope);
  FContextObject.SetCredential(Scope, True, ExpectedSecret);
  Flow := MakeFlow(Scope);
  AssertEquals('A credential reference alone must activate a legacy implicit override',
    ExpectedSecret, Flow.ApiKeyOverride);
  AssertEquals(1, FContextObject.PresenceCheckCount(Scope));
  AssertEquals(1, FContextObject.ResolutionCount(Scope));
  AssertOtherScopesWereNotResolved(Scope);
end;

procedure TAICredentialFlowTests.CheckDisabledUnavailableOverride(
  Scope: TAICredentialScope);
var
  Flow: IAIVerseFlow;
begin
  SetOverride(Scope, 'false');
  SetProviderAndModel(Scope);
  FContextObject.SetCredential(Scope, True, '', True);
  Flow := MakeFlow(Scope);
  AssertEquals('Disabled override must preserve the global credential contract',
    '', Flow.ApiKeyOverride);
  AssertEquals('', Flow.ProviderOverride);
  AssertEquals('', Flow.ModelOverride);
  AssertEquals('A disabled credential must not be read from local storage',
    0, FContextObject.ResolutionCount(Scope));
  AssertOtherScopesWereNotResolved(Scope);
end;

procedure TAICredentialFlowTests.CheckEnabledUnavailableOverride(
  Scope: TAICredentialScope);
var
  Flow: IAIVerseFlow;
  Failed: Boolean;
begin
  SetOverride(Scope, 'true');
  FContextObject.SetCredential(Scope, True, '', True);
  Flow := nil;
  Failed := False;
  try
    Flow := MakeFlow(Scope);
  except
    on E: EAICredentialError do Failed := True;
  end;
  AssertTrue('An unavailable configured override must block flow creation', Failed);
  AssertFalse('A missing configured override cannot produce a flow using the common key',
    Assigned(Flow));
  AssertEquals(1, FContextObject.ResolutionCount(Scope));
  AssertOtherScopesWereNotResolved(Scope);
end;

procedure TAICredentialFlowTests.CheckOptionalEmptyCredential(
  Scope: TAICredentialScope);
var
  Flow: IAIVerseFlow;
begin
  SetOverride(Scope, 'true');
  SetProviderAndModel(Scope);
  FContextObject.SetCredential(Scope, False, '');
  Flow := MakeFlow(Scope);
  AssertEquals('An unconfigured optional key keeps global fallback available',
    '', Flow.ApiKeyOverride);
  AssertEquals('DeepSeek', Flow.ProviderOverride);
  AssertEquals('test-override-model', Flow.ModelOverride);
  AssertEquals(1, FContextObject.ResolutionCount(Scope));
  AssertOtherScopesWereNotResolved(Scope);
end;

procedure TAICredentialFlowTests.AssociationExplicitOverrideResolvesLocalCredential;
begin
  CheckExplicitOverride(acsAssociation);
end;

procedure TAICredentialFlowTests.TranslationExplicitOverrideResolvesLocalCredential;
begin
  CheckExplicitOverride(acsTranslation);
end;

procedure TAICredentialFlowTests.AssociationImplicitOverrideDetectsLocalCredential;
begin
  CheckImplicitOverride(acsAssociation);
end;

procedure TAICredentialFlowTests.TranslationImplicitOverrideDetectsLocalCredential;
begin
  CheckImplicitOverride(acsTranslation);
end;

procedure TAICredentialFlowTests.AssociationDisabledOverrideIgnoresUnavailableReference;
begin
  CheckDisabledUnavailableOverride(acsAssociation);
end;

procedure TAICredentialFlowTests.TranslationDisabledOverrideIgnoresUnavailableReference;
begin
  CheckDisabledUnavailableOverride(acsTranslation);
end;

procedure TAICredentialFlowTests.AssociationEnabledMissingCredentialBlocksFallback;
begin
  CheckEnabledUnavailableOverride(acsAssociation);
end;

procedure TAICredentialFlowTests.TranslationEnabledMissingCredentialBlocksFallback;
begin
  CheckEnabledUnavailableOverride(acsTranslation);
end;

procedure TAICredentialFlowTests.AssociationModelOverrideCanUseOptionalGlobalCredential;
begin
  CheckOptionalEmptyCredential(acsAssociation);
end;

procedure TAICredentialFlowTests.TranslationModelOverrideCanUseOptionalGlobalCredential;
begin
  CheckOptionalEmptyCredential(acsTranslation);
end;

initialization
  RegisterTest(TAICredentialFlowTests);
end.
