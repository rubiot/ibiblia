unit export_destination_history;

{$mode objfpc}{$H+}

interface

uses IniFiles;

type
  TExportDestinationKind = (edTaggedTheWord, edInterlinearTheWord,
    edAnalyticalConcordance, edSyntheticConcordance);

function ReadExportDestination(Settings: TCustomIniFile;
  const ProjectFileName: string; Kind: TExportDestinationKind;
  const DefaultFileName: string): string;
function TryRememberExportDestination(Settings: TCustomIniFile;
  const ProjectFileName: string; Kind: TExportDestinationKind;
  const FileName: string; out ErrorMessage: string): Boolean;

implementation

uses Classes, SysUtils, LazFileUtils, LazUTF8;

const
  ExportHistorySectionPrefix = 'export.project.v1.';
  ExportHistoryKeys: array[TExportDestinationKind] of string = (
    'last.tagged.destination.file',
    'last.interlinear.source.file',
    'last.strongs.analytical.concordance.file',
    'last.strongs.synthetic.concordance.file');

resourcestring
  SExportHistorySettingsUnavailable = 'Local export history settings are unavailable.';
  SExportHistoryInvalidProject = 'The project file name is invalid for export history.';
  SExportHistoryInvalidDestination = 'The export destination file name is invalid.';
  SExportHistoryInvalidKind = 'The export history type is invalid.';
  SExportHistoryWriteFailed = 'Could not save the local export destination: %s';

function IsValidKind(Kind: TExportDestinationKind): Boolean;
begin
  Result := (Ord(Kind) >= Ord(Low(TExportDestinationKind))) and
    (Ord(Kind) <= Ord(High(TExportDestinationKind)));
end;

function IsExcludedDestination(Kind: TExportDestinationKind;
  const FileName: string): Boolean;
begin
  Result := (Kind in [edTaggedTheWord, edInterlinearTheWord]) and
    SameText(ExtractFileExt(Trim(FileName)), '.mybible');
end;

function TryNormalizeFileName(const FileName: string;
  out Normalized: string): Boolean;
var
  Value: string;
  I: Integer;
begin
  Result := False;
  Normalized := '';
  Value := Trim(FileName);
  if Value = '' then Exit;
  for I := 1 to Length(FileName) do
    if Ord(FileName[I]) < 32 then Exit;
  {$IFDEF WINDOWS}
  Value := SwitchPathDelims(Value, pdsWindows);
  {$ENDIF}
  if Value[Length(Value)] = PathDelim then Exit;
  Normalized := CleanAndExpandFilename(Value);
  if (Normalized = '') or (ExtractFileName(Normalized) = '') or
     DirectoryExists(Normalized) then
  begin
    Normalized := '';
    Exit;
  end;
  Result := True;
end;

{$IFDEF WINDOWS}
function CompareStringOrdinalForExport(First: PWideChar; FirstLength: LongInt;
  Second: PWideChar; SecondLength, IgnoreCase: LongInt): LongInt; stdcall;
  external 'kernel32.dll' name 'CompareStringOrdinal';
{$ENDIF}

function SameProjectFileName(const First, Second: string): Boolean;
{$IFDEF WINDOWS}
const
  OrdinalStringsEqual = 2;
var
  FirstWide, SecondWide: UnicodeString;
  Comparison: LongInt;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  FirstWide := UTF8Decode(First);
  SecondWide := UTF8Decode(Second);
  Comparison := CompareStringOrdinalForExport(PWideChar(FirstWide),
    Length(FirstWide), PWideChar(SecondWide), Length(SecondWide), 1);
  if Comparison = 0 then RaiseLastOSError;
  Result := Comparison = OrdinalStringsEqual;
  {$ELSE}
  Result := First = Second;
  {$ENDIF}
end;

function HexDigitValue(Value: Char): Integer;
begin
  case Value of
    '0'..'9': Result := Ord(Value) - Ord('0');
    'a'..'f': Result := Ord(Value) - Ord('a') + 10;
    'A'..'F': Result := Ord(Value) - Ord('A') + 10;
    else Result := -1;
  end;
end;

function TryDecodeProjectSection(const Section: string;
  out ProjectFileName: string): Boolean;
var
  Encoded: string;
  I, HighDigit, LowDigit, ByteValue: Integer;
begin
  Result := False;
  ProjectFileName := '';
  if Copy(Section, 1, Length(ExportHistorySectionPrefix)) <>
     ExportHistorySectionPrefix then Exit;
  Encoded := Copy(Section, Length(ExportHistorySectionPrefix) + 1, MaxInt);
  if (Encoded = '') or Odd(Length(Encoded)) then Exit;
  SetLength(ProjectFileName, Length(Encoded) div 2);
  for I := 1 to Length(ProjectFileName) do
  begin
    HighDigit := HexDigitValue(Encoded[I * 2 - 1]);
    LowDigit := HexDigitValue(Encoded[I * 2]);
    if (HighDigit < 0) or (LowDigit < 0) then Exit;
    ByteValue := (HighDigit shl 4) or LowDigit;
    if ByteValue < 32 then Exit;
    ProjectFileName[I] := Char(ByteValue);
  end;
  Result := FindInvalidUTF8Codepoint(PChar(ProjectFileName),
    Length(ProjectFileName)) < 0;
end;

function TryProjectSection(Settings: TCustomIniFile;
  const ProjectFileName: string; out Section: string): Boolean;
const
  HexDigits: array[0..15] of Char = '0123456789abcdef';
var
  Normalized, Encoded, ExistingFileName: string;
  I, ByteValue: Integer;
  Sections: TStringList;
begin
  Section := '';
  Result := TryNormalizeFileName(ProjectFileName, Normalized);
  if not Result then Exit;
  Sections := TStringList.Create;
  try
    Settings.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
      if TryDecodeProjectSection(Sections[I], ExistingFileName) then
        if SameProjectFileName(Normalized, ExistingFileName) then
        begin
          Section := Sections[I];
          Exit(True);
        end;
  finally
    Sections.Free;
  end;
  // Preserve spelling: linguistic case folding can merge distinct Windows names.
  SetLength(Encoded, Length(Normalized) * 2);
  for I := 1 to Length(Normalized) do
  begin
    ByteValue := Ord(Normalized[I]);
    Encoded[I * 2 - 1] := HexDigits[ByteValue shr 4];
    Encoded[I * 2] := HexDigits[ByteValue and $F];
  end;
  Section := ExportHistorySectionPrefix + Encoded;
end;

function ReadExportDestination(Settings: TCustomIniFile;
  const ProjectFileName: string; Kind: TExportDestinationKind;
  const DefaultFileName: string): string;
var
  Section, StoredFileName, Normalized: string;
begin
  Result := DefaultFileName;
  if (Settings = nil) or not IsValidKind(Kind) then Exit;
  try
    if not TryProjectSection(Settings, ProjectFileName, Section) then Exit;
    StoredFileName := Settings.ReadString(Section, ExportHistoryKeys[Kind], '');
    if IsExcludedDestination(Kind, StoredFileName) or
       not TryNormalizeFileName(StoredFileName, Normalized) then Exit;
    if DirectoryExists(ExtractFilePath(Normalized)) then
      Result := Normalized;
  except
    // Unavailable preferences must not prevent the user choosing an export file.
    Result := DefaultFileName;
  end;
end;

function TryRememberExportDestination(Settings: TCustomIniFile;
  const ProjectFileName: string; Kind: TExportDestinationKind;
  const FileName: string; out ErrorMessage: string): Boolean;
var
  Section, Normalized: string;
begin
  Result := False;
  ErrorMessage := '';
  try
    if not IsValidKind(Kind) then
    begin
      ErrorMessage := SExportHistoryInvalidKind;
      Exit;
    end;
    if IsExcludedDestination(Kind, FileName) then Exit(True);
    if Settings = nil then
    begin
      ErrorMessage := SExportHistorySettingsUnavailable;
      Exit;
    end;
    if not TryProjectSection(Settings, ProjectFileName, Section) then
    begin
      ErrorMessage := SExportHistoryInvalidProject;
      Exit;
    end;
    if not TryNormalizeFileName(FileName, Normalized) then
    begin
      ErrorMessage := SExportHistoryInvalidDestination;
      Exit;
    end;
    Settings.WriteString(Section, ExportHistoryKeys[Kind], Normalized);
    if Settings is TIniFile then
    begin
      if TIniFile(Settings).CacheUpdates then Settings.UpdateFile;
    end
    else
      Settings.UpdateFile;
    Result := True;
  except
    on E: Exception do
      ErrorMessage := Format(SExportHistoryWriteFailed, [E.Message]);
  end;
end;

end.
