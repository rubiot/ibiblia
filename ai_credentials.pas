unit ai_credentials;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ai_credential_store, project_persistence;

type
  TAIProjectCredentials = class
  private
    FPersistence: TProjectPersistence;
    FStore: IAICredentialStore;
    FPending: array[TAICredentialScope] of Boolean;
    FValues: array[TAICredentialScope] of string;
    FPreparedReferences: array[TAICredentialScope] of string;
    function ReadLegacy(Scope: TAICredentialScope): string;
    function HasLegacyValues: Boolean;
  public
    constructor Create(APersistence: TProjectPersistence;
      const AStore: IAICredentialStore = nil);
    procedure Attach(APersistence: TProjectPersistence);
    function Resolve(Scope: TAICredentialScope): string;
    function TryResolve(Scope: TAICredentialScope; out Secret,
      ErrorMessage: string): Boolean;
    function HasConfiguredCredential(Scope: TAICredentialScope): Boolean;
    procedure Stage(Scope: TAICredentialScope; const Secret: string);
    function HasPendingChanges: Boolean;
    function MigrationPending: Boolean;
    function PersistAfterSave(out ErrorMessage: string): Boolean;
    property Store: IAICredentialStore read FStore;
  end;

implementation

resourcestring
  SCredentialNotAvailable = 'This project uses an AI key that is not available for this user on this computer. Configure the key in LLM Options.';
  SCredentialSessionOnly = 'AI keys are available for this session only. Local credential storage is unavailable; existing project credentials have not been removed.';
  SCredentialVerificationFailed = 'The local AI credential could not be verified. Existing project credentials have not been removed.';
  SCredentialMigrationFailed = 'Project content was saved, but AI credential migration is pending. Retry saving after local storage becomes available.';

constructor TAIProjectCredentials.Create(APersistence: TProjectPersistence;
  const AStore: IAICredentialStore);
begin
  inherited Create;
  FStore := AStore;
  if not Assigned(FStore) then FStore := CreateAICredentialStore;
  Attach(APersistence);
end;

procedure TAIProjectCredentials.Attach(APersistence: TProjectPersistence);
var Scope: TAICredentialScope;
begin
  FPersistence := APersistence;
  for Scope in TAICredentialScope do
  begin
    FPending[Scope] := False;
    FValues[Scope] := '';
    FPreparedReferences[Scope] := '';
  end;
end;

function TAIProjectCredentials.ReadLegacy(Scope: TAICredentialScope): string;
begin
  Result := '';
  if not Assigned(FPersistence) then Exit;
  Result := FPersistence.ReadInfo(AICredentialKey(Scope));
  if (Scope = acsCommon) and (Result = '') then
    Result := FPersistence.ReadInfo(AI_LEGACY_COMMON_CREDENTIAL_KEY);
end;

function TAIProjectCredentials.HasLegacyValues: Boolean;
var Scope: TAICredentialScope;
begin
  Result := Assigned(FPersistence) and
    (FPersistence.ReadInfo(AI_LEGACY_COMMON_CREDENTIAL_KEY) <> '');
  for Scope in TAICredentialScope do
    Result := Result or (ReadLegacy(Scope) <> '');
end;

function TAIProjectCredentials.Resolve(Scope: TAICredentialScope): string;
var Reference: string;
begin
  if FPending[Scope] then Exit(FValues[Scope]);
  Result := '';
  if not Assigned(FPersistence) then Exit;
  Reference := FPersistence.ReadInfo(AICredentialReferenceKey(Scope));
  if Reference <> '' then
  begin
    if not FStore.TryRead(Reference, Result) or (Result = '') then
      raise EAICredentialError.Create(SCredentialNotAvailable);
  end
  else
    Result := ReadLegacy(Scope);
end;

function TAIProjectCredentials.TryResolve(Scope: TAICredentialScope;
  out Secret, ErrorMessage: string): Boolean;
begin
  ErrorMessage := '';
  Secret := '';
  try
    Secret := Resolve(Scope);
    Result := True;
  except
    on E: EAICredentialError do
    begin
      ErrorMessage := E.Message;
      Result := False;
    end;
    on E: Exception do
    begin
      ErrorMessage := SCredentialNotAvailable;
      Result := False;
    end;
  end;
end;

function TAIProjectCredentials.HasConfiguredCredential(
  Scope: TAICredentialScope): Boolean;
begin
  if FPending[Scope] then Exit(FValues[Scope] <> '');
  Result := Assigned(FPersistence) and
    ((FPersistence.ReadInfo(AICredentialReferenceKey(Scope)) <> '') or
      (ReadLegacy(Scope) <> ''));
end;

procedure TAIProjectCredentials.Stage(Scope: TAICredentialScope;
  const Secret: string);
var Previous, ErrorMessage: string;
begin
  if TryResolve(Scope, Previous, ErrorMessage) and (Previous = Trim(Secret)) then
    Exit;
  FValues[Scope] := Trim(Secret);
  FPending[Scope] := True;
  FPreparedReferences[Scope] := '';
end;

function TAIProjectCredentials.HasPendingChanges: Boolean;
var Scope: TAICredentialScope;
begin
  Result := False;
  for Scope in TAICredentialScope do
    Result := Result or FPending[Scope];
end;

function TAIProjectCredentials.MigrationPending: Boolean;
begin
  Result := HasPendingChanges or HasLegacyValues or
    (Assigned(FPersistence) and
      (FPersistence.ReadInfo(AI_CREDENTIAL_CLEANUP_PENDING_KEY) = '1'));
end;

function TAIProjectCredentials.PersistAfterSave(
  out ErrorMessage: string): Boolean;
var
  Scope: TAICredentialScope;
  References: TStringList;
  Secret, Verified, Reference: string;
  RemoveLegacy, NeedsWrite: Boolean;
begin
  ErrorMessage := '';
  Result := True;
  if not Assigned(FPersistence) or not MigrationPending then Exit;
  if not FStore.IsPersistent then
  begin
    ErrorMessage := SCredentialSessionOnly;
    Exit(False);
  end;
  References := TStringList.Create;
  try
    try
      RemoveLegacy := HasLegacyValues;
      for Scope in TAICredentialScope do
      begin
        Reference := FPersistence.ReadInfo(AICredentialReferenceKey(Scope));
        NeedsWrite := FPending[Scope] or (ReadLegacy(Scope) <> '');
        if not NeedsWrite then Continue;
        Secret := Resolve(Scope);
        if Secret = '' then
          Reference := ''
        else if FPending[Scope] or (Reference = '') then
        begin
          if FPreparedReferences[Scope] = '' then
            FPreparedReferences[Scope] := NewAICredentialReference;
          Reference := FPreparedReferences[Scope];
          FStore.Write(Reference, Secret);
        end;
        if (Secret <> '') and
           (not FStore.TryRead(Reference, Verified) or (Verified <> Secret)) then
          raise EAICredentialError.Create(SCredentialVerificationFailed);
        References.Values[AICredentialReferenceKey(Scope)] := Reference;
        { TStringList.Values removes empty values; a clear still needs a row. }
        if References.IndexOfName(AICredentialReferenceKey(Scope)) < 0 then
          References.Add(AICredentialReferenceKey(Scope) + '=');
      end;
      if (References.Count > 0) or RemoveLegacy then
        FPersistence.CommitCredentialReferences(References, RemoveLegacy);
      for Scope in TAICredentialScope do
      begin
        FPending[Scope] := False;
        FValues[Scope] := '';
        FPreparedReferences[Scope] := '';
      end;
      FPersistence.FinishCredentialCleanup;
    except
      on E: EAICredentialError do
      begin
        ErrorMessage := SCredentialMigrationFailed + LineEnding + E.Message;
        Result := False;
      end;
      on E: Exception do
      begin
        { Database/OS exceptions may include SQL or other sensitive values. }
        ErrorMessage := SCredentialMigrationFailed;
        Result := False;
      end;
    end;
  finally
    References.Free;
  end;
end;

end.
