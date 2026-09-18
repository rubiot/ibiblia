unit ai_global_settings_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ai_credential_store,
  ai_settings_types, ai_global_settings;

type
  TAIGlobalSettingsTests = class(TTestCase)
  private
    FDirectory, FFileName: string;
    FStore: IAICredentialStore;
    FStoreObject: TObject;
    FSettings: TAIGlobalSettings;
    function FileContents: string;
    procedure SeedSavedSettings;
    procedure CheckSaveFailure;
    procedure CheckStoredOriginal;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OpeningAndCancellingChangesHaveNoPersistentEffects;
    procedure RoundTripStoresOnlyMetadataAndSeparatesProviders;
    procedure ReplacingAKeyPreservesReferencesUsedByExistingCopies;
    procedure StoreWriteFailurePreservesSavedSettingsAndCanRetry;
    procedure StoreVerificationFailurePreservesSavedSettingsAndCanRetry;
    procedure MetadataFailurePreservesPreviousCredentialsAndCanRetry;
    procedure MissingReferencedCredentialDoesNotBecomeAnEmptyKey;
    procedure SessionOnlyStoreCannotPublishPersistentCredentialMetadata;
  end;

implementation

type
  TGlobalFaultStore = class(TMemoryAICredentialStore)
  public
    FailWrite, FailVerify: Boolean;
    WriteCalls: Integer;
    References: TStringList;
    constructor Create;
    destructor Destroy; override;
    function TryRead(const Reference: string; out Secret: string): Boolean; override;
    procedure Write(const Reference, Secret: string); override;
  end;

constructor TGlobalFaultStore.Create;
begin
  inherited Create(True);
  References := TStringList.Create;
end;

destructor TGlobalFaultStore.Destroy;
begin
  References.Free;
  inherited Destroy;
end;

function TGlobalFaultStore.TryRead(const Reference: string;
  out Secret: string): Boolean;
begin
  Result := inherited TryRead(Reference, Secret);
  if Result and FailVerify then Secret := 'synthetic-corrupt-readback';
end;

procedure TGlobalFaultStore.Write(const Reference, Secret: string);
begin
  Inc(WriteCalls);
  if FailWrite then raise Exception.Create('Synthetic failure: ' + Secret);
  inherited Write(Reference, Secret);
  References.Add(Reference);
end;

procedure TAIGlobalSettingsTests.SetUp;
begin
  inherited SetUp;
  FDirectory := GetTempFileName(GetTempDir, 'aig');
  DeleteFile(FDirectory);
  AssertTrue(CreateDir(FDirectory));
  FFileName := IncludeTrailingPathDelimiter(FDirectory) + 'llm.ini';
  FStoreObject := TGlobalFaultStore.Create;
  FStore := TGlobalFaultStore(FStoreObject);
  FSettings := TAIGlobalSettings.Create(FFileName, FStore);
end;

procedure TAIGlobalSettingsTests.TearDown;
var
  Search: TSearchRec;
begin
  FSettings.Free;
  FStore := nil;
  FStoreObject := nil;
  if DirectoryExists(FFileName) then RemoveDir(FFileName);
  if FindFirst(IncludeTrailingPathDelimiter(FDirectory) + '*', faAnyFile,
    Search) = 0 then
  begin
    repeat
      if (Search.Attr and faDirectory) = 0 then
        DeleteFile(IncludeTrailingPathDelimiter(FDirectory) + Search.Name);
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
  RemoveDir(FDirectory);
  inherited TearDown;
end;

function TAIGlobalSettingsTests.FileContents: string;
var
  Contents: TStringList;
begin
  Contents := TStringList.Create;
  try
    Contents.LoadFromFile(FFileName);
    Result := Contents.Text;
  finally
    Contents.Free;
  end;
end;

procedure TAIGlobalSettingsTests.SeedSavedSettings;
begin
  FSettings.SetValue(aisModel, 'gpt-4o-mini');
  FSettings.SetValue(aisMaxParallel, '4');
  FSettings.StageCredential('OpenAI', 'synthetic-original-key');
  FSettings.Save;
end;

procedure TAIGlobalSettingsTests.CheckSaveFailure;
var
  Failed: Boolean;
begin
  Failed := False;
  try
    FSettings.Save;
  except
    on E: EAICredentialError do
    begin
      Failed := True;
      AssertEquals('Errors cannot expose the original key', 0,
        Pos('synthetic-original-key', E.Message));
      AssertEquals('Errors cannot expose a replacement key', 0,
        Pos('synthetic-replacement-key', E.Message));
    end;
  end;
  AssertTrue('Save must fail explicitly', Failed);
  AssertTrue('Failed changes must remain retryable', FSettings.HasChanges);
end;

procedure TAIGlobalSettingsTests.CheckStoredOriginal;
var
  Saved: TAIGlobalSettings;
begin
  Saved := TAIGlobalSettings.Create(FFileName, FStore);
  try
    AssertEquals('gpt-4o-mini', Saved.ReadValue(aisModel));
    AssertEquals('4', Saved.ReadValue(aisMaxParallel));
    AssertEquals('synthetic-original-key', Saved.ResolveCredential('OpenAI'));
  finally
    Saved.Free;
  end;
end;

procedure TAIGlobalSettingsTests.OpeningAndCancellingChangesHaveNoPersistentEffects;
begin
  AssertFalse(FSettings.HasChanges);
  AssertFalse(FileExists(FFileName));
  FSettings.SetValue(aisMaxParallel, '7');
  FSettings.StageCredential('Google', 'synthetic-cancelled-key');
  AssertTrue(FSettings.HasChanges);
  AssertEquals('synthetic-cancelled-key', FSettings.ResolveCredential('Google'));
  AssertEquals(0, TGlobalFaultStore(FStoreObject).WriteCalls);
  FreeAndNil(FSettings);
  FSettings := TAIGlobalSettings.Create(FFileName, FStore);
  AssertFalse(FSettings.HasChanges);
  AssertFalse(FileExists(FFileName));
  AssertFalse(FSettings.HasCredential('Google'));
end;

procedure TAIGlobalSettingsTests.RoundTripStoresOnlyMetadataAndSeparatesProviders;
var
  Contents: string;
begin
  SeedSavedSettings;
  FSettings.StageCredential('Google', 'synthetic-google-key');
  FSettings.StageCredential('DeepSeek', 'synthetic-deepseek-key');
  FSettings.SetValue(aisAutoSaveInterval, '0');
  FSettings.Save;
  AssertFalse(FSettings.HasChanges);
  Contents := FileContents;
  AssertEquals(0, Pos('synthetic-', Contents));
  AssertTrue('The metadata contains a credential reference',
    Pos(TGlobalFaultStore(FStoreObject).References[0], Contents) > 0);
  FreeAndNil(FSettings);
  FSettings := TAIGlobalSettings.Create(FFileName, FStore);
  CheckStoredOriginal;
  AssertEquals('0', FSettings.ReadValue(aisAutoSaveInterval));
  AssertEquals('synthetic-google-key', FSettings.ResolveCredential('Google'));
  AssertEquals('synthetic-deepseek-key', FSettings.ResolveCredential('DeepSeek'));
end;

procedure TAIGlobalSettingsTests.ReplacingAKeyPreservesReferencesUsedByExistingCopies;
var
  OriginalReference, Secret: string;
  Previous: TAIGlobalSettings;
begin
  SeedSavedSettings;
  OriginalReference := TGlobalFaultStore(FStoreObject).References[0];
  Previous := TAIGlobalSettings.Create(FFileName, FStore);
  try
    FSettings.StageCredential('OpenAI', 'synthetic-replacement-key');
    FSettings.Save;
    AssertTrue(FStore.TryRead(OriginalReference, Secret));
    AssertEquals('synthetic-original-key', Secret);
    AssertEquals('synthetic-original-key', Previous.ResolveCredential('OpenAI'));
    AssertEquals('synthetic-replacement-key', FSettings.ResolveCredential('OpenAI'));
    AssertEquals(0, Pos('synthetic-', FileContents));
  finally
    Previous.Free;
  end;
end;

procedure TAIGlobalSettingsTests.StoreWriteFailurePreservesSavedSettingsAndCanRetry;
var
  OriginalFile: string;
begin
  SeedSavedSettings;
  OriginalFile := FileContents;
  FSettings.SetValue(aisMaxParallel, '9');
  FSettings.StageCredential('OpenAI', 'synthetic-replacement-key');
  TGlobalFaultStore(FStoreObject).FailWrite := True;
  CheckSaveFailure;
  AssertEquals(OriginalFile, FileContents);
  TGlobalFaultStore(FStoreObject).FailWrite := False;
  CheckStoredOriginal;
  FSettings.Save;
  AssertEquals('synthetic-replacement-key', FSettings.ResolveCredential('OpenAI'));
  AssertFalse(FSettings.HasChanges);
end;

procedure TAIGlobalSettingsTests.StoreVerificationFailurePreservesSavedSettingsAndCanRetry;
var
  OriginalFile: string;
begin
  SeedSavedSettings;
  OriginalFile := FileContents;
  FSettings.SetValue(aisModel, 'gemini-1.5-flash');
  FSettings.StageCredential('OpenAI', 'synthetic-replacement-key');
  TGlobalFaultStore(FStoreObject).FailVerify := True;
  CheckSaveFailure;
  AssertEquals(OriginalFile, FileContents);
  TGlobalFaultStore(FStoreObject).FailVerify := False;
  CheckStoredOriginal;
  FSettings.Save;
  AssertEquals('synthetic-replacement-key', FSettings.ResolveCredential('OpenAI'));
end;

procedure TAIGlobalSettingsTests.MetadataFailurePreservesPreviousCredentialsAndCanRetry;
var
  OriginalFile, OriginalReference, Secret: string;
begin
  SeedSavedSettings;
  OriginalFile := FileContents;
  OriginalReference := TGlobalFaultStore(FStoreObject).References[0];
  FSettings.StageCredential('OpenAI', 'synthetic-replacement-key');
  AssertTrue(RenameFile(FFileName, FFileName + '.saved'));
  AssertTrue(CreateDir(FFileName));
  try
    CheckSaveFailure;
    AssertTrue(FStore.TryRead(OriginalReference, Secret));
    AssertEquals('synthetic-original-key', Secret);
  finally
    AssertTrue(RemoveDir(FFileName));
    AssertTrue(RenameFile(FFileName + '.saved', FFileName));
  end;
  AssertEquals(OriginalFile, FileContents);
  CheckStoredOriginal;
  FSettings.Save;
  AssertEquals('synthetic-replacement-key', FSettings.ResolveCredential('OpenAI'));
end;

procedure TAIGlobalSettingsTests.MissingReferencedCredentialDoesNotBecomeAnEmptyKey;
var
  Secret, ErrorMessage: string;
begin
  SeedSavedSettings;
  FStore.Delete(TGlobalFaultStore(FStoreObject).References[0]);
  FreeAndNil(FSettings);
  FSettings := TAIGlobalSettings.Create(FFileName, FStore);
  AssertTrue(FSettings.HasCredential('OpenAI'));
  AssertFalse(FSettings.TryResolveCredential('OpenAI', Secret, ErrorMessage));
  AssertEquals('', Secret);
  AssertTrue('An unavailable configured key needs a visible error', ErrorMessage <> '');
end;

procedure TAIGlobalSettingsTests.SessionOnlyStoreCannotPublishPersistentCredentialMetadata;
begin
  FreeAndNil(FSettings);
  FStore := TMemoryAICredentialStore.Create(False);
  FStoreObject := nil;
  FSettings := TAIGlobalSettings.Create(FFileName, FStore);
  FSettings.StageCredential('OpenAI', 'synthetic-replacement-key');
  CheckSaveFailure;
  AssertFalse(FileExists(FFileName));
end;

initialization
  RegisterTest(TAIGlobalSettingsTests);

end.
