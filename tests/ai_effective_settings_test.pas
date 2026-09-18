unit ai_effective_settings_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, project_persistence, project_config,
  ai_credential_store, ai_settings_types, ai_global_settings,
  ai_effective_settings, ai_flows, ai_flow_config, ai_batch_orchestrator, ai_batch_work_item;

type
  TAIEffectiveSettingsTests = class(TTestCase)
  private
    FDirectory: string;
    FPersistence, FOtherPersistence: TProjectPersistence;
    FConfig, FOtherConfig: TProjectConfig;
    FGlobal: TAIGlobalSettings;
    FStore: IAICredentialStore;
    procedure CreateProjectFile(const FileName: string);
    procedure SeedRawInfo(const Key, Value: string);
    procedure SeedGlobalConnection;
    procedure CheckConnectionBlocked(const Draft: TAIProjectAISettings);
    function AssociationFlow: IAIVerseFlow;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EmptyProjectsUseInternalDefaultsWithoutBeingModified;
    procedure ProjectsShareGlobalsAndOverrideOptionsIndependently;
    procedure ExistingCommonAndLegacyValuesRemainProjectOverrides;
    procedure ReadingLegacyCredentialsDoesNotMigrateOrWrite;
    procedure SavingLegacySettingsBindsProviderAndMigratesMetadata;
    procedure ProjectKeyOverridesGlobalUntilExplicitlyInherited;
    procedure MissingSelectedProjectKeyNeverFallsBackToGlobal;
    procedure UnavailableProjectKeyNeverFallsBackToGlobal;
    procedure ProjectProviderMismatchBlocksConnection;
    procedure UnknownProjectKeyProviderBlocksConnection;
    procedure ChangingInheritedProviderSelectsOnlyThatGlobalKey;
    procedure GlobalKeyDoesNotEnableAFlowOverride;
    procedure EnabledFlowKeyTakesPriorityOverProjectAndGlobal;
    procedure FlowChangingProviderUsesMatchingGlobalKey;
    procedure FlowKeyWithDifferentBoundProviderIsBlocked;
    procedure OperationSnapshotSurvivesLaterGlobalChanges;
    procedure DraftEditsAreTestableWithoutChangingTheProject;
    procedure InvalidExecutionParametersAreRejected;
    procedure NonpositiveAutoSaveIntervalRemainsSupported;
    procedure BatchPreparationFreezesResolvedExecutionSettings;
  end;

implementation

uses sqlite3conn, sqldb;

procedure TAIEffectiveSettingsTests.CreateProjectFile(const FileName: string);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  try
    Connection.DatabaseName := FileName;
    Connection.Transaction := Transaction;
    Connection.Open;
    Connection.ExecuteDirect('create table info (id text primary key, valor text)');
    Connection.ExecuteDirect('create table pares (' +
      'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
      'pare_texto_destino text, pare_texto_consulta1 text, ' +
      'pare_texto_consulta2 text, pare_pares text, pare_situacao number, pare_comentarios text)');
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,1,1'', ''Gen 1:1'', ''source'', ''destination'', '''', '''', '''', 0, '''')');
    Transaction.Commit;
  finally
    Connection.Free;
  end;
end;

procedure TAIEffectiveSettingsTests.SetUp;
begin
  inherited SetUp;
  FDirectory := GetTempFileName(GetTempDir, 'aie');
  DeleteFile(FDirectory);
  AssertTrue(CreateDir(FDirectory));
  FDirectory := IncludeTrailingPathDelimiter(FDirectory);
  CreateProjectFile(FDirectory + 'first.ibiblia');
  CreateProjectFile(FDirectory + 'second.ibiblia');
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(FDirectory + 'first.ibiblia');
  FOtherPersistence := TProjectPersistence.Create;
  FOtherPersistence.Open(FDirectory + 'second.ibiblia');
  FStore := TMemoryAICredentialStore.Create(True);
  FConfig := TProjectConfig.Create(FPersistence, FStore);
  FOtherConfig := TProjectConfig.Create(FOtherPersistence, FStore);
  FGlobal := TAIGlobalSettings.Create(FDirectory + 'llm.ini', FStore);
end;

procedure TAIEffectiveSettingsTests.TearDown;
var Search: TSearchRec;
begin
  FGlobal.Free;
  FConfig.Free;
  FOtherConfig.Free;
  FPersistence.Free;
  FOtherPersistence.Free;
  FStore := nil;
  if FindFirst(FDirectory + '*', faAnyFile, Search) = 0 then
  begin
    repeat
      if (Search.Attr and faDirectory) = 0 then DeleteFile(FDirectory + Search.Name);
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
  RemoveDir(ExcludeTrailingPathDelimiter(FDirectory));
  inherited TearDown;
end;

procedure TAIEffectiveSettingsTests.SeedRawInfo(const Key, Value: string);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  FileName: string;
begin
  FileName := FPersistence.FileName;
  FPersistence.Close;
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  Query := TSQLQuery.Create(Connection);
  try
    Connection.DatabaseName := FileName;
    Connection.Transaction := Transaction;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Query.SQL.Text := 'insert or replace into info (id, valor) values (:id, :value)';
    Query.ParamByName('id').AsString := Key;
    Query.ParamByName('value').AsString := Value;
    Query.ExecSQL;
    Transaction.Commit;
  finally
    Connection.Free;
    FPersistence.Open(FileName);
    FConfig.Attach(FPersistence);
  end;
end;

procedure TAIEffectiveSettingsTests.SeedGlobalConnection;
begin
  FGlobal.SetValue(aisModel, 'gpt-4o-mini');
  FGlobal.StageCredential('OpenAI', 'synthetic-global-openai');
  FGlobal.StageCredential('Google', 'synthetic-global-google');
  FGlobal.StageCredential('DeepSeek', 'synthetic-global-deepseek');
end;

procedure TAIEffectiveSettingsTests.CheckConnectionBlocked(
  const Draft: TAIProjectAISettings);
var Failed: Boolean;
begin
  Failed := False;
  try
    ResolveAIConnection(Draft, FGlobal);
  except
    on E: EAICredentialError do
    begin
      Failed := True;
      AssertTrue('The failure must explain the configuration problem', E.Message <> '');
      AssertEquals('Errors cannot expose credentials', 0, Pos('synthetic-', E.Message));
    end;
  end;
  AssertTrue('An unavailable or inapplicable selected key must block processing', Failed);
end;

function TAIEffectiveSettingsTests.AssociationFlow: IAIVerseFlow;
var FlowConfig: IAIFlowConfig;
begin
  FlowConfig := GetFlowConfigById('association');
  Result := FlowConfig.CreateFlowInstance(CreateAIFlowContext(FConfig));
end;

procedure TAIEffectiveSettingsTests.EmptyProjectsUseInternalDefaultsWithoutBeingModified;
var
  Common: TAICommonSettings;
  Draft: TAIProjectAISettings;
  Setting: TAICommonSetting;
begin
  Draft := ReadProjectAISettings(FConfig);
  Common := ResolveCommonAISettings(Draft, FGlobal);
  AssertEquals(2, Common.MaxParallel);
  AssertEquals(3, Common.MaxRetries);
  AssertEquals(1000, Common.RetryDelay);
  AssertEquals(10, Common.AutoSaveInterval);
  for Setting in TAICommonSetting do
  begin
    AssertTrue(Draft.Inherit[Setting]);
    AssertEquals(Ord(asoDefault), Ord(Common.Sources[Setting]));
  end;
  AssertTrue(Draft.InheritApiKey);
  AssertFalse(FPersistence.UpdatesPending);
  AssertFalse(FConfig.AICredentials.HasPendingChanges);
  AssertFalse(FileExists(FDirectory + 'llm.ini'));
end;

procedure TAIEffectiveSettingsTests.ProjectsShareGlobalsAndOverrideOptionsIndependently;
var
  Draft: TAIProjectAISettings;
  First, Second: TAICommonSettings;
begin
  SeedGlobalConnection;
  FGlobal.SetValue(aisMaxParallel, '5');
  FGlobal.SetValue(aisMaxRetries, '6');
  Draft := ReadProjectAISettings(FConfig);
  Draft.Inherit[aisMaxParallel] := False;
  Draft.Values[aisMaxParallel] := '8';
  StageProjectAISettings(FConfig, Draft);
  First := ResolveCommonAISettings(FConfig, FGlobal);
  Second := ResolveCommonAISettings(FOtherConfig, FGlobal);
  AssertEquals(8, First.MaxParallel);
  AssertEquals(5, Second.MaxParallel);
  AssertEquals(6, First.MaxRetries);
  AssertEquals(6, Second.MaxRetries);
  AssertEquals(Ord(asoProject), Ord(First.Sources[aisMaxParallel]));
  AssertEquals(Ord(asoGlobal), Ord(Second.Sources[aisMaxParallel]));
  FGlobal.SetValue(aisMaxParallel, '7');
  FGlobal.SetValue(aisMaxRetries, '4');
  First := ResolveCommonAISettings(FConfig, FGlobal);
  Second := ResolveCommonAISettings(FOtherConfig, FGlobal);
  AssertEquals(8, First.MaxParallel);
  AssertEquals(7, Second.MaxParallel);
  AssertEquals(4, First.MaxRetries);
  Draft := ReadProjectAISettings(FConfig);
  Draft.Inherit[aisMaxParallel] := True;
  StageProjectAISettings(FConfig, Draft);
  First := ResolveCommonAISettings(FConfig, FGlobal);
  AssertEquals(7, First.MaxParallel);
  AssertEquals(Ord(asoGlobal), Ord(First.Sources[aisMaxParallel]));
  AssertFalse(FOtherPersistence.UpdatesPending);
end;

procedure TAIEffectiveSettingsTests.ExistingCommonAndLegacyValuesRemainProjectOverrides;
var Common: TAICommonSettings; Draft: TAIProjectAISettings;
begin
  SeedRawInfo(AISettingLegacyKey(aisMaxParallel), '9');
  SeedRawInfo(AISettingLegacyKey(aisMaxRetries), '8');
  SeedRawInfo(AISettingConfigKey(aisMaxRetries), '5');
  FGlobal.SetValue(aisMaxParallel, '4');
  FGlobal.SetValue(aisMaxRetries, '2');
  FGlobal.SetValue(aisRetryDelay, '250');
  Draft := ReadProjectAISettings(FConfig);
  Common := ResolveCommonAISettings(Draft, FGlobal);
  AssertEquals(9, Common.MaxParallel);
  AssertEquals('Common keys precede legacy aliases', 5, Common.MaxRetries);
  AssertEquals(250, Common.RetryDelay);
  AssertFalse(Draft.Inherit[aisMaxParallel]);
  AssertFalse(Draft.Inherit[aisMaxRetries]);
  AssertTrue(Draft.Inherit[aisRetryDelay]);
  AssertFalse(FPersistence.UpdatesPending);
end;

procedure TAIEffectiveSettingsTests.ReadingLegacyCredentialsDoesNotMigrateOrWrite;
var Connection: TAIResolvedConnection;
begin
  SeedRawInfo(AISettingLegacyKey(aisModel), 'deepseek-chat');
  SeedRawInfo(AI_LEGACY_COMMON_CREDENTIAL_KEY, 'synthetic-legacy-deepseek');
  SeedGlobalConnection;
  Connection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals('DeepSeek', Connection.Provider);
  AssertEquals('synthetic-legacy-deepseek', Connection.ApiKey);
  AssertEquals(Ord(asoProject), Ord(Connection.CredentialSource));
  AssertFalse(FPersistence.UpdatesPending);
  AssertFalse(FConfig.AICredentials.HasPendingChanges);
  AssertFalse(FConfig.HasKey(AICredentialProviderKey(acsCommon)));
  AssertFalse(FConfig.HasKey(AISettingConfigKey(aisModel)));
  AssertEquals('synthetic-legacy-deepseek', FConfig.ReadString(AI_LEGACY_COMMON_CREDENTIAL_KEY));
end;

procedure TAIEffectiveSettingsTests.SavingLegacySettingsBindsProviderAndMigratesMetadata;
begin
  SeedRawInfo(AISettingLegacyKey(aisModel), 'deepseek-chat');
  SeedRawInfo(AISettingLegacyKey(aisMaxParallel), '3');
  SeedRawInfo(AI_LEGACY_COMMON_CREDENTIAL_KEY, 'synthetic-legacy-deepseek');
  MigrateLegacyAISettings(FConfig);
  AssertEquals('deepseek-chat', FConfig.ReadString(AISettingConfigKey(aisModel)));
  AssertEquals('3', FConfig.ReadString(AISettingConfigKey(aisMaxParallel)));
  AssertEquals('DeepSeek', FConfig.ReadString(AICredentialProviderKey(acsCommon)));
  FPersistence.Commit;
  FPersistence.Close;
  FPersistence.Open(FDirectory + 'first.ibiblia');
  AssertEquals('DeepSeek', FConfig.ReadString(AICredentialProviderKey(acsCommon)));
  AssertEquals('3', FConfig.ReadString(AISettingConfigKey(aisMaxParallel)));
end;

procedure TAIEffectiveSettingsTests.ProjectKeyOverridesGlobalUntilExplicitlyInherited;
var Draft: TAIProjectAISettings; Connection: TAIResolvedConnection;
begin
  SeedGlobalConnection;
  Draft := ReadProjectAISettings(FConfig);
  Draft.InheritApiKey := False;
  Draft.ApiKey := 'synthetic-project-openai';
  Draft.ApiKeyConfigured := True;
  Draft.ApiKeyProvider := 'OpenAI';
  StageProjectAISettings(FConfig, Draft);
  Connection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals('synthetic-project-openai', Connection.ApiKey);
  AssertEquals(Ord(asoProject), Ord(Connection.CredentialSource));
  Connection := ResolveAIConnection(FOtherConfig, FGlobal);
  AssertEquals('synthetic-global-openai', Connection.ApiKey);
  Draft := ReadProjectAISettings(FConfig);
  Draft.InheritApiKey := True;
  StageProjectAISettings(FConfig, Draft);
  Connection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals('synthetic-global-openai', Connection.ApiKey);
  AssertEquals(Ord(asoGlobal), Ord(Connection.CredentialSource));
end;

procedure TAIEffectiveSettingsTests.MissingSelectedProjectKeyNeverFallsBackToGlobal;
var Draft: TAIProjectAISettings;
begin
  SeedGlobalConnection;
  Draft := ReadProjectAISettings(FConfig);
  Draft.InheritApiKey := False;
  Draft.ApiKeyProvider := 'OpenAI';
  CheckConnectionBlocked(Draft);
end;

procedure TAIEffectiveSettingsTests.UnavailableProjectKeyNeverFallsBackToGlobal;
var Draft: TAIProjectAISettings; Connection: TAIResolvedConnection;
begin
  SeedRawInfo(AISettingConfigKey(aisModel), 'gpt-4o-mini');
  SeedRawInfo(AICredentialReferenceKey(acsCommon), NewAICredentialReference);
  SeedGlobalConnection;
  Draft := ReadProjectAISettings(FConfig);
  AssertTrue(Draft.ApiKeyConfigured);
  AssertFalse(Draft.InheritApiKey);
  AssertTrue(Draft.ApiKeyError <> '');
  CheckConnectionBlocked(Draft);
  Draft.InheritApiKey := True;
  Connection := ResolveAIConnection(Draft, FGlobal);
  AssertEquals('Explicit inheritance recovers from an unavailable reference',
    'synthetic-global-openai', Connection.ApiKey);
end;

procedure TAIEffectiveSettingsTests.ProjectProviderMismatchBlocksConnection;
var Draft: TAIProjectAISettings;
begin
  SeedGlobalConnection;
  Draft := ReadProjectAISettings(FConfig);
  Draft.Inherit[aisModel] := False;
  Draft.Values[aisModel] := 'gemini-1.5-flash';
  Draft.InheritApiKey := False;
  Draft.ApiKeyConfigured := True;
  Draft.ApiKey := 'synthetic-project-openai';
  Draft.ApiKeyProvider := 'OpenAI';
  CheckConnectionBlocked(Draft);
end;

procedure TAIEffectiveSettingsTests.UnknownProjectKeyProviderBlocksConnection;
var Draft: TAIProjectAISettings;
begin
  SeedGlobalConnection;
  Draft := ReadProjectAISettings(FConfig);
  Draft.InheritApiKey := False;
  Draft.ApiKeyConfigured := True;
  Draft.ApiKey := 'synthetic-project-unbound';
  CheckConnectionBlocked(Draft);
end;

procedure TAIEffectiveSettingsTests.ChangingInheritedProviderSelectsOnlyThatGlobalKey;
var Connection: TAIResolvedConnection;
begin
  SeedGlobalConnection;
  Connection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals('synthetic-global-openai', Connection.ApiKey);
  FGlobal.SetValue(aisModel, 'gemini-1.5-flash');
  Connection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals('Google', Connection.Provider);
  AssertEquals('synthetic-global-google', Connection.ApiKey);
  FGlobal.SetValue(aisModel, 'deepseek-chat');
  Connection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals('DeepSeek', Connection.Provider);
  AssertEquals('synthetic-global-deepseek', Connection.ApiKey);
end;

procedure TAIEffectiveSettingsTests.GlobalKeyDoesNotEnableAFlowOverride;
var Flow: IAIVerseFlow; Connection: TAIResolvedConnection;
begin
  SeedGlobalConnection;
  Flow := AssociationFlow;
  AssertEquals('', Flow.ModelOverride);
  AssertEquals('', Flow.ProviderOverride);
  AssertEquals('', Flow.ApiKeyOverride);
  Connection := ResolveAIConnection(FConfig, FGlobal, Flow, acsAssociation);
  AssertEquals('synthetic-global-openai', Connection.ApiKey);
  AssertEquals(Ord(asoGlobal), Ord(Connection.CredentialSource));
  AssertFalse(FPersistence.UpdatesPending);
end;

procedure TAIEffectiveSettingsTests.EnabledFlowKeyTakesPriorityOverProjectAndGlobal;
var Connection: TAIResolvedConnection;
begin
  SeedGlobalConnection;
  FConfig.WriteString(AISettingConfigKey(aisModel), 'gpt-4o-mini');
  FConfig.AICredentials.Stage(acsCommon, 'synthetic-project-openai');
  FConfig.WriteBoolean(GetFlowOverrideEnabledConfigKey('association'), True);
  FConfig.AICredentials.Stage(acsAssociation, 'synthetic-flow-openai');
  Connection := ResolveAIConnection(FConfig, FGlobal, AssociationFlow, acsAssociation);
  AssertEquals('synthetic-flow-openai', Connection.ApiKey);
  AssertEquals(Ord(asoFlow), Ord(Connection.CredentialSource));
end;

procedure TAIEffectiveSettingsTests.FlowChangingProviderUsesMatchingGlobalKey;
var Connection: TAIResolvedConnection;
begin
  SeedGlobalConnection;
  FConfig.WriteString(AISettingConfigKey(aisModel), 'gpt-4o-mini');
  FConfig.AICredentials.Stage(acsCommon, 'synthetic-project-openai');
  FConfig.WriteBoolean(GetFlowOverrideEnabledConfigKey('association'), True);
  FConfig.WriteString('ai.flow.association.provider', 'Google');
  FConfig.WriteString('ai.flow.association.model', 'gemini-1.5-flash');
  Connection := ResolveAIConnection(FConfig, FGlobal, AssociationFlow, acsAssociation);
  AssertEquals('Google', Connection.Provider);
  AssertEquals('gemini-1.5-flash', Connection.Model);
  AssertEquals('synthetic-global-google', Connection.ApiKey);
  AssertEquals(Ord(asoFlow), Ord(Connection.ModelSource));
  AssertEquals(Ord(asoGlobal), Ord(Connection.CredentialSource));
end;

procedure TAIEffectiveSettingsTests.FlowKeyWithDifferentBoundProviderIsBlocked;
var Failed: Boolean;
begin
  SeedGlobalConnection;
  FConfig.WriteBoolean(GetFlowOverrideEnabledConfigKey('association'), True);
  FConfig.WriteString('ai.flow.association.provider', 'Google');
  FConfig.WriteString('ai.flow.association.model', 'gemini-1.5-flash');
  FConfig.AICredentials.Stage(acsAssociation, 'synthetic-flow-openai');
  FConfig.WriteString(AICredentialProviderKey(acsAssociation), 'OpenAI');
  Failed := False;
  try
    ResolveAIConnection(FConfig, FGlobal, AssociationFlow, acsAssociation);
  except
    on E: EAICredentialError do Failed := True;
  end;
  AssertTrue('A scoped key must remain bound to its original provider', Failed);
end;

procedure TAIEffectiveSettingsTests.OperationSnapshotSurvivesLaterGlobalChanges;
var
  Started, Later: TAICommonSettings;
  StartedConnection, LaterConnection: TAIResolvedConnection;
begin
  SeedGlobalConnection;
  FGlobal.SetValue(aisMaxParallel, '4');
  FGlobal.SetValue(aisAutoSaveInterval, '12');
  Started := ResolveCommonAISettings(FConfig, FGlobal);
  StartedConnection := ResolveAIConnection(FConfig, FGlobal);
  FGlobal.SetValue(aisModel, 'deepseek-chat');
  FGlobal.SetValue(aisMaxParallel, '9');
  FGlobal.SetValue(aisAutoSaveInterval, '2');
  FGlobal.StageCredential('OpenAI', 'synthetic-updated-openai');
  Later := ResolveCommonAISettings(FConfig, FGlobal);
  LaterConnection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals(4, Started.MaxParallel);
  AssertEquals(12, Started.AutoSaveInterval);
  AssertEquals('gpt-4o-mini', StartedConnection.Model);
  AssertEquals('synthetic-global-openai', StartedConnection.ApiKey);
  AssertEquals(9, Later.MaxParallel);
  AssertEquals(2, Later.AutoSaveInterval);
  AssertEquals('deepseek-chat', LaterConnection.Model);
  AssertEquals('synthetic-global-deepseek', LaterConnection.ApiKey);
end;

procedure TAIEffectiveSettingsTests.DraftEditsAreTestableWithoutChangingTheProject;
var Draft: TAIProjectAISettings; Connection: TAIResolvedConnection;
begin
  SeedGlobalConnection;
  Draft := ReadProjectAISettings(FConfig);
  Draft.Inherit[aisModel] := False;
  Draft.Values[aisModel] := 'deepseek-chat';
  Draft.InheritApiKey := False;
  Draft.ApiKeyConfigured := True;
  Draft.ApiKey := 'synthetic-unsaved-draft-key';
  Draft.ApiKeyProvider := 'DeepSeek';
  Connection := ResolveAIConnection(Draft, FGlobal);
  AssertEquals('deepseek-chat', Connection.Model);
  AssertEquals('synthetic-unsaved-draft-key', Connection.ApiKey);
  AssertFalse(FPersistence.UpdatesPending);
  AssertFalse(FConfig.AICredentials.HasPendingChanges);
  Connection := ResolveAIConnection(FConfig, FGlobal);
  AssertEquals('gpt-4o-mini', Connection.Model);
  AssertEquals('synthetic-global-openai', Connection.ApiKey);
end;

procedure TAIEffectiveSettingsTests.InvalidExecutionParametersAreRejected;
const InvalidValues: array[0..2] of string = ('0', '-1', 'not-a-number');
var Draft: TAIProjectAISettings; I: Integer; Failed: Boolean;
begin
  Draft := ReadProjectAISettings(FConfig);
  Draft.Inherit[aisMaxParallel] := False;
  for I := Low(InvalidValues) to High(InvalidValues) do
  begin
    Draft.Values[aisMaxParallel] := InvalidValues[I];
    Failed := False;
    try
      ResolveCommonAISettings(Draft, FGlobal);
    except
      on E: EAICredentialError do Failed := True;
    end;
    AssertTrue('Invalid parallelism must not reach worker creation', Failed);
  end;
end;

procedure TAIEffectiveSettingsTests.NonpositiveAutoSaveIntervalRemainsSupported;
var Draft: TAIProjectAISettings; Common: TAICommonSettings;
begin
  Draft := ReadProjectAISettings(FConfig);
  Draft.Inherit[aisAutoSaveInterval] := False;
  Draft.Values[aisAutoSaveInterval] := '0';
  Common := ResolveCommonAISettings(Draft, FGlobal);
  AssertEquals(0, Common.AutoSaveInterval);
  Draft.Values[aisAutoSaveInterval] := '-1';
  Common := ResolveCommonAISettings(Draft, FGlobal);
  AssertEquals(-1, Common.AutoSaveInterval);
end;

procedure TAIEffectiveSettingsTests.BatchPreparationFreezesResolvedExecutionSettings;
var
  Orchestrator: TAIBatchOrchestrator;
  WorkItems: TAIBatchWorkItemList;
  Data: TVerseData;
  Preparation: TAIBatchPreparation;
begin
  SeedGlobalConnection;
  FGlobal.SetValue(aisMaxParallel, '4');
  FGlobal.SetValue(aisMaxRetries, '5');
  FGlobal.SetValue(aisRetryDelay, '250');
  FGlobal.SetValue(aisAutoSaveInterval, '12');
  Data := Default(TVerseData);
  Data.ReferenceID := '1,1,1';
  Data.ReferenceName := 'Gen 1:1';
  Data.OriginalText := 'source';
  Data.TranslatedText := 'destination';
  WorkItems := TAIBatchWorkItemList.Create;
  Orchestrator := TAIBatchOrchestrator.Create(FConfig, nil, FGlobal);
  try
    WorkItems.AddItem(TAIBatchWorkItem.Create(1, Data));
    Preparation := Orchestrator.PrepareBatch('association', WorkItems, 0);
    try
      AssertTrue(Assigned(Preparation.Manager));
      AssertEquals(5, Preparation.Manager.MaxRetries);
      AssertEquals(250, Preparation.Manager.RetryBaseDelay);
      AssertEquals(12, Preparation.Manager.AutoSaveInterval);
      FGlobal.SetValue(aisMaxRetries, '1');
      FGlobal.SetValue(aisRetryDelay, '50');
      FGlobal.SetValue(aisAutoSaveInterval, '0');
      AssertEquals(5, Preparation.Manager.MaxRetries);
      AssertEquals(250, Preparation.Manager.RetryBaseDelay);
      AssertEquals(12, Preparation.Manager.AutoSaveInterval);
      AssertTrue('Preparation must not start requests', Preparation.Manager.IsStopped);
    finally
      Preparation.Manager.Free;
    end;
  finally
    Orchestrator.Free;
    WorkItems.Free;
  end;
end;

initialization
  RegisterTest(TAIEffectiveSettingsTests);

end.