unit ai_credential_store;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
  TAICredentialScope = (acsCommon, acsAssociation, acsTranslation);
  EAICredentialError = class(Exception);

  IAICredentialStore = interface
    ['{B0793F3E-2B71-4D20-AF88-B97B2D305607}']
    function IsPersistent: Boolean;
    function TryRead(const Reference: string; out Secret: string): Boolean;
    procedure Write(const Reference, Secret: string);
    procedure Delete(const Reference: string);
  end;

  TMemoryAICredentialStore = class(TInterfacedObject, IAICredentialStore)
  private
    FValues: TStringList;
    FPersistent: Boolean;
  public
    constructor Create(APersistent: Boolean = False);
    destructor Destroy; override;
    function IsPersistent: Boolean;
    function TryRead(const Reference: string; out Secret: string): Boolean; virtual;
    procedure Write(const Reference, Secret: string); virtual;
    procedure Delete(const Reference: string); virtual;
  end;

const
  AI_LEGACY_COMMON_CREDENTIAL_KEY = 'ai_associator.api_key';
  AI_CREDENTIAL_CLEANUP_PENDING_KEY = 'ai.credentials.cleanup_pending';

function AICredentialKey(Scope: TAICredentialScope): string;
function AICredentialReferenceKey(Scope: TAICredentialScope): string;
function IsLegacyAICredentialKey(const Key: string): Boolean;
function NewAICredentialReference: string;
function CreateAICredentialStore: IAICredentialStore;

implementation

{$IFDEF MSWINDOWS}
uses Windows;
{$ELSE}
uses IniFiles, BaseUnix;
{$ENDIF}

{$IFDEF MSWINDOWS}

{$packrecords c}
type
  PCredentialW = ^TCredentialW;
  TCredentialW = record
    Flags, CredentialType: DWORD;
    TargetName, Comment: PWideChar;
    LastWritten: TFileTime;
    CredentialBlobSize: DWORD;
    CredentialBlob: PByte;
    Persist, AttributeCount: DWORD;
    Attributes: Pointer;
    TargetAlias, UserName: PWideChar;
  end;
{$packrecords default}

  TWindowsAICredentialStore = class(TInterfacedObject, IAICredentialStore)
  public
    function IsPersistent: Boolean;
    function TryRead(const Reference: string; out Secret: string): Boolean;
    procedure Write(const Reference, Secret: string);
    procedure Delete(const Reference: string);
  end;

function CredReadW(TargetName: PWideChar; CredentialType, Flags: DWORD;
  out Credential: PCredentialW): BOOL; stdcall; external 'advapi32.dll';
function CredWriteW(Credential: PCredentialW; Flags: DWORD): BOOL;
  stdcall; external 'advapi32.dll';
function CredDeleteW(TargetName: PWideChar; CredentialType, Flags: DWORD): BOOL;
  stdcall; external 'advapi32.dll';
procedure CredFree(Buffer: Pointer); stdcall; external 'advapi32.dll';
{$ENDIF}

resourcestring
  SCredentialReferenceInvalid = 'The local AI credential reference is invalid.';
  SCredentialStoreError = 'The Windows credential store could not complete the operation (error %d).';
  SCredentialTooLong = 'The API key exceeds the credential store size limit.';

function AICredentialKey(Scope: TAICredentialScope): string;
begin
  case Scope of
    acsCommon: Result := 'ai.common.api_key';
    acsAssociation: Result := 'ai.flow.association.api_key';
    acsTranslation: Result := 'ai.flow.translation.api_key';
  end;
end;

function AICredentialReferenceKey(Scope: TAICredentialScope): string;
begin
  case Scope of
    acsCommon: Result := 'ai.common.credential_ref';
    acsAssociation: Result := 'ai.flow.association.credential_ref';
    acsTranslation: Result := 'ai.flow.translation.credential_ref';
  end;
end;

function IsLegacyAICredentialKey(const Key: string): Boolean;
var Scope: TAICredentialScope;
begin
  Result := Key = AI_LEGACY_COMMON_CREDENTIAL_KEY;
  for Scope in TAICredentialScope do
    Result := Result or (Key = AICredentialKey(Scope));
end;

function NewAICredentialReference: string;
var Id: TGUID;
begin
  if CreateGUID(Id) <> 0 then
    raise EAICredentialError.Create(SCredentialReferenceInvalid);
  Result := LowerCase(Copy(GUIDToString(Id), 2, 36));
end;

procedure ValidateReference(const Reference: string);
var Id: TGUID;
begin
  try
    if Length(Reference) <> 36 then
      raise EAICredentialError.Create(SCredentialReferenceInvalid);
    Id := StringToGUID('{' + Reference + '}');
    if not SameText(Copy(GUIDToString(Id), 2, 36), Reference) then
      raise EAICredentialError.Create(SCredentialReferenceInvalid);
  except
    raise EAICredentialError.Create(SCredentialReferenceInvalid);
  end;
end;

constructor TMemoryAICredentialStore.Create(APersistent: Boolean);
begin
  inherited Create;
  FValues := TStringList.Create;
  FPersistent := APersistent;
end;

destructor TMemoryAICredentialStore.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TMemoryAICredentialStore.IsPersistent: Boolean;
begin
  Result := FPersistent;
end;

function TMemoryAICredentialStore.TryRead(const Reference: string;
  out Secret: string): Boolean;
var Index: Integer;
begin
  ValidateReference(Reference);
  Index := FValues.IndexOfName(Reference);
  Result := Index >= 0;
  Secret := '';
  if Result then Secret := FValues.ValueFromIndex[Index];
end;

procedure TMemoryAICredentialStore.Write(const Reference, Secret: string);
begin
  ValidateReference(Reference);
  FValues.Values[Reference] := Secret;
end;

procedure TMemoryAICredentialStore.Delete(const Reference: string);
var Index: Integer;
begin
  ValidateReference(Reference);
  Index := FValues.IndexOfName(Reference);
  if Index >= 0 then FValues.Delete(Index);
end;

{$IFDEF MSWINDOWS}
function TWindowsAICredentialStore.IsPersistent: Boolean;
begin
  Result := True;
end;

function TWindowsAICredentialStore.TryRead(const Reference: string;
  out Secret: string): Boolean;
var
  Target: UnicodeString;
  Credential: PCredentialW;
  ErrorCode: DWORD;
begin
  ValidateReference(Reference);
  Secret := '';
  Target := UTF8Decode('iBiblia/ai/' + Reference);
  Credential := nil;
  Result := CredReadW(PWideChar(Target), 1, 0, Credential);
  if not Result then
  begin
    ErrorCode := GetLastError;
    if ErrorCode = 1168 { ERROR_NOT_FOUND } then Exit;
    raise EAICredentialError.CreateFmt(SCredentialStoreError, [ErrorCode]);
  end;
  try
    if Credential^.CredentialBlobSize > 0 then
      SetString(Secret, PChar(Credential^.CredentialBlob),
        Credential^.CredentialBlobSize);
  finally
    CredFree(Credential);
  end;
end;

procedure TWindowsAICredentialStore.Write(const Reference, Secret: string);
var
  Target: UnicodeString;
  Credential: TCredentialW;
begin
  ValidateReference(Reference);
  if Length(Secret) > 2560 then
    raise EAICredentialError.Create(SCredentialTooLong);
  Target := UTF8Decode('iBiblia/ai/' + Reference);
  FillChar(Credential, SizeOf(Credential), 0);
  Credential.CredentialType := 1; // CRED_TYPE_GENERIC
  Credential.TargetName := PWideChar(Target);
  Credential.CredentialBlobSize := Length(Secret);
  Credential.CredentialBlob := PByte(PChar(Secret));
  Credential.Persist := 2; // CRED_PERSIST_LOCAL_MACHINE (same user)
  if not CredWriteW(@Credential, 0) then
    raise EAICredentialError.CreateFmt(SCredentialStoreError, [GetLastError]);
end;

procedure TWindowsAICredentialStore.Delete(const Reference: string);
var
  Target: UnicodeString;
  ErrorCode: DWORD;
begin
  ValidateReference(Reference);
  Target := UTF8Decode('iBiblia/ai/' + Reference);
  if not CredDeleteW(PWideChar(Target), 1, 0) then
  begin
    ErrorCode := GetLastError;
    if ErrorCode <> 1168 { ERROR_NOT_FOUND } then
      raise EAICredentialError.CreateFmt(SCredentialStoreError, [ErrorCode]);
  end;
end;
{$ELSE}
type
  TUnixFileAICredentialStore = class(TInterfacedObject, IAICredentialStore)
  private
    function StoreFileName: string;
  public
    function IsPersistent: Boolean;
    function TryRead(const Reference: string; out Secret: string): Boolean;
    procedure Write(const Reference, Secret: string);
    procedure Delete(const Reference: string);
  end;

function TUnixFileAICredentialStore.StoreFileName: string;
var
  Dir: string;
begin
  Dir := SysUtils.GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Dir = '' then
    Dir := IncludeTrailingPathDelimiter(GetUserDir) + '.config';
  if Dir = '' then
    Dir := GetAppConfigDir(False);
  Result := IncludeTrailingPathDelimiter(Dir) + 'iBiblia' + PathDelim + 'credentials.ini';
end;

function TUnixFileAICredentialStore.IsPersistent: Boolean;
begin
  Result := True;
end;

function TUnixFileAICredentialStore.TryRead(const Reference: string;
  out Secret: string): Boolean;
var
  Ini: TMemIniFile;
  FileName: string;
begin
  ValidateReference(Reference);
  Secret := '';
  FileName := StoreFileName;
  if not FileExists(FileName) then
    Exit(False);
  Ini := TMemIniFile.Create(FileName);
  try
    Result := Ini.ValueExists('secrets', Reference);
    if Result then
      Secret := Ini.ReadString('secrets', Reference, '');
  finally
    Ini.Free;
  end;
end;

procedure TUnixFileAICredentialStore.Write(const Reference, Secret: string);
var
  Ini: TMemIniFile;
  Lines: TStringList;
  FileName, DirName, TmpFile: string;
begin
  ValidateReference(Reference);
  if Length(Secret) > 2560 then
    raise EAICredentialError.Create(SCredentialTooLong);
  FileName := StoreFileName;
  DirName := ExtractFileDir(FileName);
  if not ForceDirectories(DirName) then
    raise EAICredentialError.CreateFmt(SCredentialStoreError, [fpGetErrno]);
  FpChmod(PChar(DirName), &700);
  TmpFile := FileName + '.tmp-' + NewAICredentialReference;
  Ini := TMemIniFile.Create(FileName);
  try
    Ini.WriteString('secrets', Reference, Secret);
    Lines := TStringList.Create;
    try
      Ini.GetStrings(Lines);
      Lines.SaveToFile(TmpFile);
    finally
      Lines.Free;
    end;
  finally
    Ini.Free;
  end;
  FpChmod(PChar(TmpFile), &600);
  if not RenameFile(TmpFile, FileName) then
  begin
    SysUtils.DeleteFile(TmpFile);
    raise EAICredentialError.CreateFmt(SCredentialStoreError, [fpGetErrno]);
  end;
  FpChmod(PChar(FileName), &600);
end;

procedure TUnixFileAICredentialStore.Delete(const Reference: string);
var
  Ini: TMemIniFile;
  Lines: TStringList;
  FileName: string;
begin
  ValidateReference(Reference);
  FileName := StoreFileName;
  if not FileExists(FileName) then
    Exit;
  Ini := TMemIniFile.Create(FileName);
  try
    if not Ini.ValueExists('secrets', Reference) then
      Exit;
    Ini.DeleteKey('secrets', Reference);
    Lines := TStringList.Create;
    try
      Ini.GetStrings(Lines);
      Lines.SaveToFile(FileName);
    finally
      Lines.Free;
    end;
    FpChmod(PChar(FileName), &600);
  finally
    Ini.Free;
  end;
end;
{$ENDIF}

function CreateAICredentialStore: IAICredentialStore;
begin
  {$IFDEF MSWINDOWS}
  Result := TWindowsAICredentialStore.Create;
  {$ELSE}
  Result := TUnixFileAICredentialStore.Create;
  {$ENDIF}
end;

end.
