unit fileassociation;

{$mode objfpc}
{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows, Registry,
  {$ENDIF}
  SysUtils, Classes;

function IsFileAssociated(const Extension: string): Boolean;
function AssociateFileExtension(const Extension, FileType, Description, 
  ExecutablePath, IconPath: string; IconIndex: Integer = 0): Boolean;
function RemoveFileAssociation(const Extension, FileType: string): Boolean;
function GetAssociatedApp(const Ext: string): string;

implementation

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
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    
    // Set extension mapping to file type
    if Reg.OpenKey(Extension, True) then
    begin
      Reg.WriteString('', FileType);
      Reg.CloseKey;
    end else
      Exit;
      
    // Set file type information
    if Reg.OpenKey(FileType, True) then
    begin
      Reg.WriteString('', Description);
      Reg.CloseKey;
    end else
      Exit;
      
    // Set default icon
    if Reg.OpenKey(FileType + '\DefaultIcon', True) then
    begin
      Reg.WriteString('', Format('%s,%d', [IconPath, IconIndex]));
      Reg.CloseKey;
    end else
      Exit;
      
    // Set open command
    if Reg.OpenKey(FileType + '\shell\open\command', True) then
    begin
      Reg.WriteString('', Format('"%s" "%%1"', [ExecutablePath]));
      Reg.CloseKey;
    end else
      Exit;
      
    Result := True;
  finally
    Reg.Free;
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
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    
    // Remove extension mapping
    Result := Reg.DeleteKey(Extension);
    
    // Remove file type information
    Result := Result and Reg.DeleteKey(FileType + '\DefaultIcon');
    Result := Result and Reg.DeleteKey(FileType + '\shell\open\command');
    Result := Result and Reg.DeleteKey(FileType + '\shell\open');
    Result := Result and Reg.DeleteKey(FileType + '\shell');
    Result := Result and Reg.DeleteKey(FileType);
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
begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    // 1. Check UserChoice (per-user association)
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\FileExts\\' + Ext + '\\UserChoice') then
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
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if (UserChoiceProgId <> '') then
      FileType := UserChoiceProgId
    else if Reg.OpenKeyReadOnly(Ext) then
    begin
      FileType := Reg.ReadString('');
      Reg.CloseKey;
    end
    else
      FileType := '';

    if FileType <> '' then
    begin
      if Reg.OpenKeyReadOnly(FileType + '\\shell\\open\\command') then
      begin
        Command := Reg.ReadString('');
        Reg.CloseKey;
        // Extract executable from command string (may have parameters)
        if Command <> '' then
        begin
          Command := TrimLeft(Command);
          if (Length(Command) > 0) and (Command[1] = '"') then
          begin
            // Quoted path
            Delete(Command, 1, 1);
            if Pos('"', Command) > 0 then
              Result := Copy(Command, 1, Pos('"', Command) - 1)
            else
              Result := Command;
          end
          else
          begin
            // Unquoted path, take up to first space
            if Pos(' ', Command) > 0 then
              Result := Copy(Command, 1, Pos(' ', Command) - 1)
            else
              Result := Command;
          end;
        end;
      end;
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
