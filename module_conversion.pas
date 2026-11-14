unit module_conversion;

{ Lets iBiblia 1.1.0 import e-Sword (.bblx, .bbli, .bbl), USFM (.usfm, .sfm)
  and USFX (.usfx, .xml) Bibles: the file is converted once to a theWord
  module (.ont/.ot/.nt) in a temporary folder, and that module goes through
  1.1.0's normal import. Apocrypha / deuterocanonical verses (books 67+)
  found in the file are kept with the converted module for the project to
  add after the import. Conversion code: esword_usfm.pas (from the mod). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, esword_usfm;

type
  TApocryphaPayload = record
    Verses: TESwordExtraVerseArray;
    BooksInfo: string; // e.g. "67=Tobit,68=Judith" as produced by esword_usfm
  end;

{ True for the extensions handled here. }
function IsConvertibleBibleFile(const FileName: string): Boolean;
{ For a theWord module returns FileName unchanged. For e-Sword/USFM/USFX
  returns the path of the converted theWord module. False + ErrorMessage on
  failure. }
function PrepareBibleModuleFile(const FileName: string; out ModuleFileName,
  ErrorMessage: string): Boolean;
{ Apocrypha verses that came with a converted module, if any. }
function TryGetApocryphaPayload(const ModuleFileName: string;
  out Payload: TApocryphaPayload): Boolean;
{ The e-Sword/USFM/USFX file a converted module came from. }
function TryGetConversionSource(const ModuleFileName: string;
  out SourceFileName: string): Boolean;

resourcestring
  SBibleModuleFilterSuffix = '|e-Sword Bibles (*.bblx, *.bbli, *.bbl)|*.bblx;*.bbli;*.bbl' +
    '|USFM / USFX Bibles (*.usfm, *.sfm, *.usfx, *.xml)|*.usfm;*.sfm;*.usfx;*.xml';
  SConversionFailed = 'Could not read this Bible file: %s';
  SNoBibleBooksFound = 'No Old or New Testament books were found in this file.';

implementation

type
  TPayloadEntry = class
    ModuleFileName: string;
    SourceFileName: string;
    Payload: TApocryphaPayload;
  end;

var
  Payloads: TList = nil;

function IsConvertibleBibleFile(const FileName: string): Boolean;
begin
  Result := TESwordModule.IsESwordFileName(FileName) or
    TESwordModule.IsUSFMOrUSFXFileName(FileName);
end;

function ConversionFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'iBiblia' +
    PathDelim + 'converted' + PathDelim;
  ForceDirectories(Result);
end;

procedure RememberPayload(const ModuleFileName, SourceFileName: string;
  const Payload: TApocryphaPayload);
var
  Index: Integer;
  Entry: TPayloadEntry;
begin
  if Payloads = nil then
    Payloads := TList.Create;
  for Index := Payloads.Count - 1 downto 0 do
    if SameFileName(TPayloadEntry(Payloads[Index]).ModuleFileName, ModuleFileName) then
    begin
      TPayloadEntry(Payloads[Index]).Free;
      Payloads.Delete(Index);
    end;
  Entry := TPayloadEntry.Create;
  Entry.ModuleFileName := ModuleFileName;
  Entry.SourceFileName := SourceFileName;
  Entry.Payload := Payload;
  Payloads.Add(Entry);
end;

function TryGetApocryphaPayload(const ModuleFileName: string;
  out Payload: TApocryphaPayload): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Payload.Verses := nil;
  Payload.BooksInfo := '';
  if Payloads = nil then
    Exit;
  for Index := 0 to Payloads.Count - 1 do
    if SameFileName(TPayloadEntry(Payloads[Index]).ModuleFileName, ModuleFileName) then
    begin
      Payload := TPayloadEntry(Payloads[Index]).Payload;
      Exit(Length(Payload.Verses) > 0);
    end;
end;

function TryGetConversionSource(const ModuleFileName: string;
  out SourceFileName: string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  SourceFileName := '';
  if Payloads = nil then
    Exit;
  for Index := 0 to Payloads.Count - 1 do
    if SameFileName(TPayloadEntry(Payloads[Index]).ModuleFileName, ModuleFileName) then
    begin
      SourceFileName := TPayloadEntry(Payloads[Index]).SourceFileName;
      Exit(SourceFileName <> '');
    end;
end;

function PrepareBibleModuleFile(const FileName: string; out ModuleFileName,
  ErrorMessage: string): Boolean;
var
  Lines: TStringList;
  Payload: TApocryphaPayload;
  HasOT, HasNT, Loaded: Boolean;
  Extension: string;
  Stream: TFileStream;
  Text: string;
const
  UTF8_BOM: string = #$EF#$BB#$BF;
begin
  ModuleFileName := FileName;
  ErrorMessage := '';
  Result := True;
  if not IsConvertibleBibleFile(FileName) then
    Exit;

  Result := False;
  Lines := TStringList.Create;
  try
    Payload.Verses := nil;
    Payload.BooksInfo := '';
    try
      if TESwordModule.IsESwordFileName(FileName) then
        Loaded := TESwordModule.LoadESwordBibleWithApocrypha(FileName, Lines,
          Payload.Verses, Payload.BooksInfo, HasOT, HasNT, ErrorMessage)
      else if TESwordModule.IsUSFXFileName(FileName) then
        Loaded := TESwordModule.LoadUSFXBibleWithApocrypha(FileName, Lines,
          Payload.Verses, Payload.BooksInfo, HasOT, HasNT, ErrorMessage)
      else
        Loaded := TESwordModule.LoadUSFMBibleWithApocrypha(FileName, Lines,
          Payload.Verses, Payload.BooksInfo, HasOT, HasNT, ErrorMessage);
    except
      on E: Exception do
      begin
        Loaded := False;
        ErrorMessage := Format(SConversionFailed, [E.Message]);
      end;
    end;
    if not Loaded then
    begin
      if ErrorMessage = '' then
        ErrorMessage := Format(SConversionFailed, [ExtractFileName(FileName)]);
      Exit;
    end;
    if not (HasOT or HasNT) then
    begin
      ErrorMessage := SNoBibleBooksFound;
      Exit;
    end;

    if HasOT and HasNT then
      Extension := '.ont'
    else if HasOT then
      Extension := '.ot'
    else
      Extension := '.nt';
    ModuleFileName := ConversionFolder + ChangeFileExt(ExtractFileName(FileName), '') +
      '_' + StringReplace(ExtractFileExt(FileName), '.', '', []) + Extension;

    Lines.LineBreak := #13#10;
    Text := Lines.Text;
    if Copy(Text, 1, 3) <> UTF8_BOM then
      Text := UTF8_BOM + Text;
    Stream := TFileStream.Create(ModuleFileName, fmCreate);
    try
      if Text <> '' then
        Stream.WriteBuffer(Text[1], Length(Text));
    finally
      Stream.Free;
    end;
    RememberPayload(ModuleFileName, FileName, Payload);
    Result := True;
  finally
    Lines.Free;
  end;
end;

procedure FreePayloads;
var
  Index: Integer;
begin
  if Payloads = nil then
    Exit;
  for Index := 0 to Payloads.Count - 1 do
    TPayloadEntry(Payloads[Index]).Free;
  FreeAndNil(Payloads);
end;

finalization
  FreePayloads;

end.
