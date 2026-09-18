unit ai_global_settings;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ai_settings_types, ai_credential_store;

type
  TAIGlobalSettings = class
  private
    FFileName: string;
    FStore: IAICredentialStore;
    FValues, FOriginalValues: TAISettingValues;
    FReferences: array[0..2] of string;
    FPending: array[0..2] of Boolean;
    FSecrets, FPreparedReferences: array[0..2] of string;
    procedure Load;
  public
    constructor Create(const AFileName: string = '';
      const AStore: IAICredentialStore = nil);
    function ReadValue(Setting: TAICommonSetting): string;
    procedure SetValue(Setting: TAICommonSetting; const Value: string);
    function HasCredential(const Provider: string): Boolean;
    function ResolveCredential(const Provider: string): string;
    function TryResolveCredential(const Provider: string; out Secret,
      ErrorMessage: string): Boolean;
    procedure StageCredential(const Provider, Secret: string);
    procedure Save;
    function HasChanges: Boolean;
    property FileName: string read FFileName;
  end;

implementation

uses IniFiles
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  Providers: array[0..2] of string = ('OpenAI', 'Google', 'DeepSeek');
  OptionsSection = 'options';
  CredentialsSection = 'credentials';

resourcestring
  SGlobalProviderInvalid = 'The AI credential provider is not supported.';
  SGlobalCredentialUnavailable = 'The global AI key is unavailable for this user on this computer. Configure the key in LLM Options.';
  SGlobalLoadFailed = 'The global LLM settings could not be read. Check access to the local configuration file.';
  SGlobalSaveFailed = 'The global LLM settings could not be saved. The previous settings and credentials have been preserved.';
  SGlobalStoreUnavailable = 'Persistent local credential storage is unavailable. The previous global LLM settings and credentials have been preserved.';
  SGlobalCredentialVerificationFailed = 'The new global AI key could not be verified in local credential storage. The previous global LLM settings and credentials have been preserved.';

function ProviderIndex(const Provider: string): Integer;
var Canonical: string; I: Integer;
begin
  Canonical := CanonicalAIProvider(Provider);
  for I := Low(Providers) to High(Providers) do
    if Canonical = Providers[I] then Exit(I);
  raise EAICredentialError.Create(SGlobalProviderInvalid);
end;

function DefaultSettingsFileName: string;
var DirectoryName: string;
begin
  {$IFDEF MSWINDOWS}
  DirectoryName := SysUtils.GetEnvironmentVariable('LOCALAPPDATA');
  if DirectoryName = '' then
    DirectoryName := SysUtils.GetEnvironmentVariable('APPDATA');
  {$ELSE}
  DirectoryName := SysUtils.GetEnvironmentVariable('XDG_CONFIG_HOME');
  if DirectoryName = '' then
    DirectoryName := IncludeTrailingPathDelimiter(GetUserDir) + '.config';
  {$ENDIF}
  if DirectoryName = '' then DirectoryName := GetAppConfigDir(False);
  Result := IncludeTrailingPathDelimiter(DirectoryName) + 'iBiblia' +
    PathDelim + 'llm.ini';
end;

constructor TAIGlobalSettings.Create(const AFileName: string;
  const AStore: IAICredentialStore);
begin
  inherited Create;
  if AFileName = '' then FFileName := DefaultSettingsFileName
  else FFileName := ExpandFileName(AFileName);
  FStore := AStore;
  if not Assigned(FStore) then FStore := CreateAICredentialStore;
  Load;
end;

procedure TAIGlobalSettings.Load;
var Ini: TMemIniFile; Setting: TAICommonSetting; I: Integer;
begin
  if not FileExists(FFileName) then Exit;
  try
    Ini := TMemIniFile.Create(FFileName);
    try
      for Setting in TAICommonSetting do
        FValues[Setting] := Ini.ReadString(OptionsSection,
          AISettingConfigKey(Setting), '');
      FOriginalValues := FValues;
      for I := Low(Providers) to High(Providers) do
        FReferences[I] := Ini.ReadString(CredentialsSection, Providers[I], '');
    finally
      Ini.Free;
    end;
  except
    raise EAICredentialError.Create(SGlobalLoadFailed);
  end;
end;

function TAIGlobalSettings.ReadValue(Setting: TAICommonSetting): string;
begin
  Result := FValues[Setting];
end;

procedure TAIGlobalSettings.SetValue(Setting: TAICommonSetting;
  const Value: string);
begin
  FValues[Setting] := Trim(Value);
end;

function TAIGlobalSettings.HasCredential(const Provider: string): Boolean;
var I: Integer;
begin
  I := ProviderIndex(Provider);
  if FPending[I] then Result := FSecrets[I] <> ''
  else Result := FReferences[I] <> '';
end;

function TAIGlobalSettings.ResolveCredential(const Provider: string): string;
var I: Integer;
begin
  I := ProviderIndex(Provider);
  if FPending[I] then Exit(FSecrets[I]);
  Result := '';
  if FReferences[I] = '' then Exit;
  try
    if not FStore.TryRead(FReferences[I], Result) or (Result = '') then
      raise EAICredentialError.Create(SGlobalCredentialUnavailable);
  except
    raise EAICredentialError.Create(SGlobalCredentialUnavailable);
  end;
end;

function TAIGlobalSettings.TryResolveCredential(const Provider: string;
  out Secret, ErrorMessage: string): Boolean;
begin
  Secret := '';
  ErrorMessage := '';
  try
    Secret := ResolveCredential(Provider);
    Result := True;
  except
    on E: EAICredentialError do
    begin
      Secret := '';
      ErrorMessage := E.Message;
      Result := False;
    end;
    on E: Exception do
    begin
      Secret := '';
      ErrorMessage := SGlobalCredentialUnavailable;
      Result := False;
    end;
  end;
end;

procedure TAIGlobalSettings.StageCredential(const Provider, Secret: string);
var I: Integer; Previous, ErrorMessage, NewSecret: string;
begin
  I := ProviderIndex(Provider);
  NewSecret := Trim(Secret);
  if TryResolveCredential(Provider, Previous, ErrorMessage) and
    (Previous = NewSecret) then Exit;
  FSecrets[I] := NewSecret;
  FPending[I] := True;
  FPreparedReferences[I] := '';
end;

function TAIGlobalSettings.HasChanges: Boolean;
var Setting: TAICommonSetting; I: Integer;
begin
  Result := True;
  for Setting in TAICommonSetting do
    if FValues[Setting] <> FOriginalValues[Setting] then Exit;
  for I := Low(Providers) to High(Providers) do
    if FPending[I] then Exit;
  Result := False;
end;

procedure PublishSettingsFile(const TemporaryFile, FinalFile: string);
{$IFDEF MSWINDOWS}
var TemporaryWide, FinalWide: UnicodeString; FileHandle: THandle;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TemporaryWide := UTF8Decode(TemporaryFile);
  FinalWide := UTF8Decode(FinalFile);
  FileHandle := CreateFileW(PWideChar(TemporaryWide), GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    if not FlushFileBuffers(FileHandle) then RaiseLastOSError;
  finally
    CloseHandle(FileHandle);
  end;
  if not MoveFileExW(PWideChar(TemporaryWide), PWideChar(FinalWide),
    MOVEFILE_REPLACE_EXISTING or $00000008 { MOVEFILE_WRITE_THROUGH }) then
    RaiseLastOSError;
  {$ELSE}
  if not RenameFile(TemporaryFile, FinalFile) then RaiseLastOSError;
  {$ENDIF}
end;

procedure TAIGlobalSettings.Save;
var
  Ini: TMemIniFile;
  Lines: TStringList;
  Setting: TAICommonSetting;
  I: Integer;
  Reference, Verified, TemporaryFile, DirectoryName: string;
  References: array[0..2] of string;
  SavedValues: TAISettingValues;
begin
  if not HasChanges then Exit;
  TemporaryFile := '';
  try
    try
      { Prepare every credential before publishing any configuration. Existing
        references are never overwritten or deleted: copied projects may use them. }
      References := FReferences;
      for I := Low(Providers) to High(Providers) do
      begin
        if not FPending[I] then Continue;
        if FSecrets[I] = '' then
        begin
          References[I] := '';
          Continue;
        end;
        if not FStore.IsPersistent then
          raise EAICredentialError.Create(SGlobalStoreUnavailable);
        if FPreparedReferences[I] = '' then
          FPreparedReferences[I] := NewAICredentialReference;
        Reference := FPreparedReferences[I];
        FStore.Write(Reference, FSecrets[I]);
        Verified := '';
        if not FStore.TryRead(Reference, Verified) or
          (Verified <> FSecrets[I]) then
          raise EAICredentialError.Create(SGlobalCredentialVerificationFailed);
        References[I] := Reference;
      end;

      DirectoryName := ExtractFileDir(FFileName);
      if not ForceDirectories(DirectoryName) then
        raise EAICredentialError.Create(SGlobalSaveFailed);
      TemporaryFile := FFileName + '.saving-' + NewAICredentialReference;
      Ini := TMemIniFile.Create(FFileName);
      try
        { Merge untouched fields with the current file so another dialog's
          independent changes are not reverted by this draft. }
        for Setting in TAICommonSetting do
        begin
          if FValues[Setting] <> FOriginalValues[Setting] then
            Ini.WriteString(OptionsSection, AISettingConfigKey(Setting),
              FValues[Setting]);
          SavedValues[Setting] := Ini.ReadString(OptionsSection,
            AISettingConfigKey(Setting), '');
        end;
        for I := Low(Providers) to High(Providers) do
        begin
          if FPending[I] then
            Ini.WriteString(CredentialsSection, Providers[I], References[I]);
          References[I] := Ini.ReadString(CredentialsSection, Providers[I], '');
        end;
        Lines := TStringList.Create;
        try
          Ini.GetStrings(Lines);
          Lines.SaveToFile(TemporaryFile);
        finally
          Lines.Free;
        end;
      finally
        Ini.Free;
      end;
      PublishSettingsFile(TemporaryFile, FFileName);
      TemporaryFile := '';
      FReferences := References;
      FValues := SavedValues;
      FOriginalValues := FValues;
      for I := Low(Providers) to High(Providers) do
      begin
        FPending[I] := False;
        FSecrets[I] := '';
        FPreparedReferences[I] := '';
      end;
    except
      { Store and filesystem exceptions may contain sensitive data. }
      raise EAICredentialError.Create(SGlobalSaveFailed);
    end;
  finally
    if TemporaryFile <> '' then SysUtils.DeleteFile(TemporaryFile);
  end;
end;

end.
