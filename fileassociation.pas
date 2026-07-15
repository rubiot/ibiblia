unit fileassociation;

{$mode objfpc}
{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows, Registry, ShellApi,
  {$ENDIF}
  SysUtils, Classes;

function IsFileAssociated(const Extension: string): Boolean;
function AssociateFileExtension(const Extension, FileType, Description, 
  ExecutablePath, IconPath: string; IconIndex: Integer = 0): Boolean;
function RemoveFileAssociation(const Extension, FileType: string): Boolean;
function GetAssociatedApp(const Ext: string): string;

implementation

{$IFDEF WINDOWS}
const
  USER_CLASSES_KEY = 'Software\Classes\';
  MACHINE_CLASSES_KEY = 'Software\Classes\';

procedure NotifyShellAssociationChanged;
begin
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST or SHCNF_FLUSH, nil, nil);
end;

function ExtractExecutableFromCommand(Command: string): string;
begin
  Result := '';
  Command := TrimLeft(Command);
  if Command = '' then
    Exit;

  if Command[1] = '"' then
  begin
    Delete(Command, 1, 1);
    if Pos('"', Command) > 0 then
      Result := Copy(Command, 1, Pos('"', Command) - 1)
    else
      Result := Command;
  end
  else if Pos(' ', Command) > 0 then
    Result := Copy(Command, 1, Pos(' ', Command) - 1)
  else
    Result := Command;
end;

function SameExecutablePath(const LeftPath, RightPath: string): Boolean;
begin
  Result := (LeftPath <> '') and (RightPath <> '') and
    SameText(ExpandFileName(LeftPath), ExpandFileName(RightPath));
end;

function WriteRegistryString(ARootKey: HKEY; const KeyName, ValueName,
  Value: string): Boolean;
var
  Key: HKEY;
  Disposition: DWORD;
  KeyNameW, ValueNameW, ValueW: UnicodeString;
begin
  Result := False;
  KeyNameW := UnicodeString(KeyName);
  ValueNameW := UnicodeString(ValueName);
  ValueW := UnicodeString(Value);

  if RegCreateKeyExW(ARootKey, PWideChar(KeyNameW), 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_WRITE, nil, @Key,
    @Disposition) <> ERROR_SUCCESS then
    Exit;
  try
    Result := RegSetValueExW(Key, PWideChar(ValueNameW), 0, REG_SZ,
      LPBYTE(PWideChar(ValueW)), (Length(ValueW) + 1) * SizeOf(WideChar)) =
      ERROR_SUCCESS;
  finally
    RegCloseKey(Key);
  end;
end;

function WriteRegistryNone(ARootKey: HKEY; const KeyName, ValueName: string): Boolean;
var
  Key: HKEY;
  Disposition: DWORD;
  KeyNameW, ValueNameW: UnicodeString;
begin
  Result := False;
  KeyNameW := UnicodeString(KeyName);
  ValueNameW := UnicodeString(ValueName);

  if RegCreateKeyExW(ARootKey, PWideChar(KeyNameW), 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_WRITE, nil, @Key,
    @Disposition) <> ERROR_SUCCESS then
    Exit;
  try
    Result := RegSetValueExW(Key, PWideChar(ValueNameW), 0, REG_NONE,
      nil, 0) = ERROR_SUCCESS;
  finally
    RegCloseKey(Key);
  end;
end;

function DeleteRegistryValue(ARootKey: HKEY; const KeyName,
  ValueName: string): Boolean;
var
  Key: HKEY;
  KeyNameW, ValueNameW: UnicodeString;
  ErrorCode: LongInt;
begin
  Result := True;
  KeyNameW := UnicodeString(KeyName);
  ValueNameW := UnicodeString(ValueName);

  ErrorCode := RegOpenKeyExW(ARootKey, PWideChar(KeyNameW), 0,
    KEY_SET_VALUE, @Key);
  if ErrorCode = ERROR_FILE_NOT_FOUND then
    Exit;
  if ErrorCode <> ERROR_SUCCESS then
  begin
    Result := False;
    Exit;
  end;
  try
    ErrorCode := RegDeleteValueW(Key, PWideChar(ValueNameW));
    Result := (ErrorCode = ERROR_SUCCESS) or (ErrorCode = ERROR_FILE_NOT_FOUND);
  finally
    RegCloseKey(Key);
  end;
end;

function ReadRegistryString(ARootKey: HKEY; const KeyName, ValueName: string;
  out Value: string): Boolean;
var
  Key: HKEY;
  DataType: DWORD;
  DataSize: DWORD;
  ValueNameW, KeyNameW, DataW: UnicodeString;
begin
  Result := False;
  Value := '';
  KeyNameW := UnicodeString(KeyName);
  ValueNameW := UnicodeString(ValueName);

  if RegOpenKeyExW(ARootKey, PWideChar(KeyNameW), 0, KEY_READ, @Key) <>
    ERROR_SUCCESS then
    Exit;
  try
    DataSize := 0;
    DataType := REG_NONE;
    if RegQueryValueExW(Key, PWideChar(ValueNameW), nil, @DataType, nil,
      @DataSize) <> ERROR_SUCCESS then
      Exit;
    if not (DataType in [REG_SZ, REG_EXPAND_SZ]) then
      Exit;

    SetLength(DataW, DataSize div SizeOf(WideChar));
    if DataSize > 0 then
    begin
      if RegQueryValueExW(Key, PWideChar(ValueNameW), nil, @DataType,
        LPBYTE(PWideChar(DataW)), @DataSize) <> ERROR_SUCCESS then
        Exit;
      while (Length(DataW) > 0) and (DataW[Length(DataW)] = #0) do
        Delete(DataW, Length(DataW), 1);
    end;

    Value := string(DataW);
    Result := True;
  finally
    RegCloseKey(Key);
  end;
end;
{$ENDIF}

function IsFileAssociated(const Extension: string): Boolean;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  ExtVal: string;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKeyReadOnly(Extension) then
    begin
      ExtVal := Reg.ReadString('');
      Reg.CloseKey;
      Result := (ExtVal <> '');
    end;
  finally
    Reg.Free;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

function AssociateFileExtension(const Extension, FileType, Description, 
  ExecutablePath, IconPath: string; IconIndex: Integer = 0): Boolean;
{$IFDEF WINDOWS}
  function ClearCurrentUserOpenWithHistory: Boolean;
  var
    Reg: TRegistry;
    FileExtsKey, OpenWithListKey: string;
  begin
    Result := True;

    Result := DeleteRegistryValue(HKEY_CURRENT_USER,
      USER_CLASSES_KEY + Extension + '\OpenWithProgids', 'VSCode.bib') and
      Result;

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      FileExtsKey := 'Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\' +
        Extension;
      OpenWithListKey := FileExtsKey + '\OpenWithList';

      if Reg.KeyExists(OpenWithListKey) then
        Result := Reg.DeleteKey(OpenWithListKey) and Result;

      if Reg.OpenKey(FileExtsKey + '\OpenWithProgids', True) then
      begin
        Reg.DeleteValue(FileType);
        Reg.CloseKey;
      end;
      Result := WriteRegistryNone(HKEY_CURRENT_USER,
        FileExtsKey + '\OpenWithProgids', FileType) and Result;
    finally
      Reg.Free;
    end;
  end;

  function WriteAssociation(ARootKey: HKEY; const ClassesKey: string): Boolean;
  var
    ExtValue, CommandValue, CommandText, IconText: string;
  begin
    Result := False;
    CommandText := Format('"%s" "%%1"', [ExecutablePath]);
    IconText := Format('"%s",%d', [IconPath, IconIndex]);

    if not WriteRegistryString(ARootKey, ClassesKey + Extension, '', FileType) then
      Exit;

    if not WriteRegistryNone(ARootKey, ClassesKey + Extension + '\OpenWithProgids',
      FileType) then
      Exit;

    if not WriteRegistryString(ARootKey, ClassesKey + FileType, '',
      Description) then
      Exit;

    if not WriteRegistryString(ARootKey, ClassesKey + FileType + '\DefaultIcon',
      '', IconText) then
      Exit;

    if not WriteRegistryString(ARootKey, ClassesKey + FileType + '\shell', '',
      'open') then
      Exit;

    if not WriteRegistryString(ARootKey, ClassesKey + FileType + '\shell\open',
      '', '&Open') then
      Exit;

    if not WriteRegistryString(ARootKey,
      ClassesKey + FileType + '\shell\open\command', '', CommandText) then
      Exit;

    if not WriteRegistryString(ARootKey,
      ClassesKey + 'Applications\iBiblia.exe\shell\open\command', '',
      CommandText) then
      Exit;

    if not ReadRegistryString(ARootKey, ClassesKey + Extension, '', ExtValue) then
      Exit;
    if not SameText(ExtValue, FileType) then
      Exit;

    if not ReadRegistryString(ARootKey,
      ClassesKey + FileType + '\shell\open\command', '', CommandValue) then
      Exit;

    Result := SameExecutablePath(ExtractExecutableFromCommand(CommandValue), ExecutablePath);
  end;
begin
  try
    Result := WriteAssociation(HKEY_LOCAL_MACHINE, MACHINE_CLASSES_KEY);
    Result := WriteAssociation(HKEY_CURRENT_USER, USER_CLASSES_KEY) and Result;
    Result := ClearCurrentUserOpenWithHistory and Result;
    Result := Result and SameExecutablePath(GetAssociatedApp(Extension), ExecutablePath);
    if Result then
      NotifyShellAssociationChanged;
  except
    Result := False;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

function RemoveFileAssociation(const Extension, FileType: string): Boolean;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  ExtVal: string;

  function ClearExtensionDefaultValue(ARootKey: HKEY; const ClassesKey: string): Boolean;
  begin
    Result := True;
    Reg.RootKey := ARootKey;

    if Reg.OpenKeyReadOnly(ClassesKey + Extension) then
    begin
      ExtVal := Reg.ReadString('');
      Reg.CloseKey;
    end
    else
      ExtVal := '';

    if not SameText(ExtVal, FileType) then
      Exit;

    if Reg.OpenKey(ClassesKey + Extension, False) then
    begin
      Result := Reg.DeleteValue('');
      if not Result then
      begin
        Reg.WriteString('', '');
        Result := True;
      end;
      Reg.CloseKey;
    end
    else
      Result := False;
  end;

  function DeleteValueIfExists(const Key, ValueName: string): Boolean;
  begin
    Result := True;
    if not Reg.OpenKey(Key, False) then
      Exit;
    try
      if Reg.ValueExists(ValueName) then
        Result := Reg.DeleteValue(ValueName);
    finally
      Reg.CloseKey;
    end;
  end;

  function DeleteKeyIfExists(const Key: string): Boolean;
  begin
    if Reg.KeyExists(Key) then
      Result := Reg.DeleteKey(Key)
    else
      Result := True;
  end;

  function RemoveUserAssociation: Boolean;
  begin
    Reg.RootKey := HKEY_CURRENT_USER;

    Result := ClearExtensionDefaultValue(HKEY_CURRENT_USER, USER_CLASSES_KEY);
    Result := DeleteValueIfExists(USER_CLASSES_KEY + Extension + '\OpenWithProgids',
      FileType) and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + FileType + '\DefaultIcon') and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + FileType + '\shell\open\command') and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + FileType + '\shell\open') and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + FileType + '\shell') and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + FileType) and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + 'Applications\iBiblia.exe\shell\open\command') and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + 'Applications\iBiblia.exe\shell\open') and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + 'Applications\iBiblia.exe\shell') and Result;
    Result := DeleteKeyIfExists(USER_CLASSES_KEY + 'Applications\iBiblia.exe') and Result;
  end;

  function RemoveMachineAssociation: Boolean;
  begin
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    Result := ClearExtensionDefaultValue(HKEY_LOCAL_MACHINE, MACHINE_CLASSES_KEY);
    Result := DeleteValueIfExists(MACHINE_CLASSES_KEY + Extension + '\OpenWithProgids',
      FileType) and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + FileType + '\DefaultIcon') and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + FileType + '\shell\open\command') and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + FileType + '\shell\open') and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + FileType + '\shell') and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + FileType) and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + 'Applications\iBiblia.exe\shell\open\command') and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + 'Applications\iBiblia.exe\shell\open') and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + 'Applications\iBiblia.exe\shell') and Result;
    Result := DeleteKeyIfExists(MACHINE_CLASSES_KEY + 'Applications\iBiblia.exe') and Result;
  end;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Result := RemoveUserAssociation;
    Result := RemoveMachineAssociation and Result;

    if Result then
      NotifyShellAssociationChanged;
  finally
    Reg.Free;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}


function GetAssociatedApp(const Ext: string): string;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  FileType, Command, UserChoiceProgId: string;

  function ReadStringFromRoot(ARootKey: HKEY; const KeyName, ValueName: string): string;
  begin
    Result := '';
    Reg.RootKey := ARootKey;
    if Reg.OpenKeyReadOnly(KeyName) then
    begin
      Result := Reg.ReadString(ValueName);
      Reg.CloseKey;
    end;
  end;

begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    // 1. Check UserChoice (per-user association)
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\' + Ext + '\UserChoice') then
    begin
      if Reg.ValueExists('ProgId') then
        UserChoiceProgId := Reg.ReadString('ProgId')
      else
        UserChoiceProgId := '';
      Reg.CloseKey;
    end
    else
      UserChoiceProgId := '';

    // 2. Use UserChoice ProgId if present, else fallback to HKCR
    if (UserChoiceProgId <> '') then
      FileType := UserChoiceProgId
    else
    begin
      FileType := ReadStringFromRoot(HKEY_CURRENT_USER, USER_CLASSES_KEY + Ext, '');
      if FileType = '' then
        FileType := ReadStringFromRoot(HKEY_LOCAL_MACHINE, MACHINE_CLASSES_KEY + Ext, '');
      if FileType = '' then
        FileType := ReadStringFromRoot(HKEY_CLASSES_ROOT, Ext, '');
    end;

    if FileType <> '' then
    begin
      Command := ReadStringFromRoot(HKEY_CURRENT_USER, USER_CLASSES_KEY + FileType + '\shell\open\command', '');
      if Command = '' then
        Command := ReadStringFromRoot(HKEY_LOCAL_MACHINE, MACHINE_CLASSES_KEY + FileType + '\shell\open\command', '');
      if Command = '' then
        Command := ReadStringFromRoot(HKEY_CLASSES_ROOT, FileType + '\shell\open\command', '');

      Result := ExtractExecutableFromCommand(Command);
    end;
  finally
    Reg.Free;
  end;
end;
{$ELSE}
begin
  Result := '';
end;
{$ENDIF}

end.
