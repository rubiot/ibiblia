unit dictionary_manager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils, sqldb, sqlite3conn, syntagm, project_persistence;

type
  TDictionaryModuleType = (dmtStrong, dmtMorphology);
  TDictionaryValidationResult = (
    dvrValid,
    dvrMissingStrongSupport,
    dvrMissingMorphologySupport,
    dvrEncrypted,
    dvrCompressed,
    dvrNonRTF
  );

  TDictionaryEntry = record
    Key: string;
    Definition: string;
  end;

  TDictionaryEntryArray = array of TDictionaryEntry;
  TDictionaryQueryArray = array[0..3] of TSQLQuery;

  TDictionaryManager = class
  private
    FStrongDictionaries: TDictionaryQueryArray;
    FMorphologyDictionaries: TDictionaryQueryArray;
    class procedure AppendEntry(var Entries: TDictionaryEntryArray;
      const Key, Definition: string); static;
    procedure AppendStrongEntries(var Entries: TDictionaryEntryArray;
      Syntagm: TSyntagm; TextIndex: Integer);
    procedure AppendMorphologyEntries(var Entries: TDictionaryEntryArray;
      Syntagm: TSyntagm; TextIndex: Integer);
    class procedure ReleaseQuery(var Query: TSQLQuery); static;
    function GetStrongDictionary(TextIndex: Integer): TSQLQuery;
    function GetMorphologyDictionary(TextIndex: Integer): TSQLQuery;
  public
    constructor Create;
    destructor Destroy; override;
    class function FormatRTF(const RTF: string): string; static;
    class function ValidateDictionaryModule(const DictionaryPath: string;
      ModuleType: TDictionaryModuleType): TDictionaryValidationResult; static;
    procedure Clear;
    function HasStrongDictionary(TextIndex: Integer): Boolean;
    function HasMorphologyDictionary(TextIndex: Integer): Boolean;
    function AssignStrongDictionary(const DictionaryPath: string; TextIndex: Integer): Boolean;
    function AssignMorphologyDictionary(const DictionaryPath: string; TextIndex: Integer): Boolean;
    function LookupStrong(const Strong: string; TextIndex: Integer): string;
    function LookupMorphology(const Morphology: string; TextIndex: Integer): string;
    procedure CollectPopupEntries(Syntagm: TSyntagm; TextIndex: Integer;
      out StrongEntries, MorphologyEntries: TDictionaryEntryArray);
  end;

implementation

class procedure TDictionaryManager.AppendEntry(var Entries: TDictionaryEntryArray;
  const Key, Definition: string);
var
  NextIndex: Integer;
begin
  NextIndex := Length(Entries);
  SetLength(Entries, NextIndex + 1);
  Entries[NextIndex].Key := Key;
  Entries[NextIndex].Definition := Definition;
end;

procedure TDictionaryManager.AppendStrongEntries(var Entries: TDictionaryEntryArray;
  Syntagm: TSyntagm; TextIndex: Integer);
var
  EntryKey: string;
  PairSyntagm: TSyntagm;
begin
  if not HasStrongDictionary(TextIndex) then
    Exit;

  if Syntagm.Strong.Count > 0 then
  begin
    for EntryKey in Syntagm.Strong do
      AppendEntry(Entries, EntryKey, LookupStrong(EntryKey, TextIndex));
    Exit;
  end;

  for PairSyntagm in Syntagm.Pairs do
    for EntryKey in PairSyntagm.Strong do
      AppendEntry(Entries, EntryKey, LookupStrong(EntryKey, TextIndex));
end;

procedure TDictionaryManager.AppendMorphologyEntries(
  var Entries: TDictionaryEntryArray; Syntagm: TSyntagm; TextIndex: Integer);
var
  EntryKey: string;
  PairSyntagm: TSyntagm;
begin
  if not HasMorphologyDictionary(TextIndex) then
    Exit;

  if Syntagm.Morph.Count > 0 then
  begin
    for EntryKey in Syntagm.Morph do
      AppendEntry(Entries, EntryKey, LookupMorphology(EntryKey, TextIndex));
    Exit;
  end;

  for PairSyntagm in Syntagm.Pairs do
    for EntryKey in PairSyntagm.Morph do
      AppendEntry(Entries, EntryKey, LookupMorphology(EntryKey, TextIndex));
end;

constructor TDictionaryManager.Create;
begin
  inherited Create;
  FillChar(FStrongDictionaries, SizeOf(FStrongDictionaries), 0);
  FillChar(FMorphologyDictionaries, SizeOf(FMorphologyDictionaries), 0);
end;

destructor TDictionaryManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class procedure TDictionaryManager.ReleaseQuery(var Query: TSQLQuery);
begin
  if not Assigned(Query) then
    Exit;

  Query.Close;
  Query.DataBase.Close;
  TSQLite3Connection(Query.DataBase).Transaction.Free;
  Query.DataBase.Free;
  Query.Free;
  Query := nil;
end;

function TDictionaryManager.GetStrongDictionary(TextIndex: Integer): TSQLQuery;
begin
  if (TextIndex < Low(FStrongDictionaries)) or (TextIndex > High(FStrongDictionaries)) then
    Exit(nil);

  Result := FStrongDictionaries[TextIndex];
end;

function TDictionaryManager.GetMorphologyDictionary(TextIndex: Integer): TSQLQuery;
begin
  if (TextIndex < Low(FMorphologyDictionaries)) or (TextIndex > High(FMorphologyDictionaries)) then
    Exit(nil);

  Result := FMorphologyDictionaries[TextIndex];
end;

class function TDictionaryManager.FormatRTF(const RTF: string): string;
begin
  Result := '';
  if RTF.IsEmpty then
    Exit;

  if AnsiStartsStr('{\rtf1', RTF) then
    Result := RTF
  else
    Result := '{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Tahoma;}' +
      '{\f1\fswiss\fprq2\fcharset0 Tahoma;}{\f2\fswiss\fcharset0 Arial;}}' +
      '{\colortbl ;' +
        '\red0\green0\blue0;' +
        '\red0\green0\blue255;' +
        '\red150\green60\blue100;' +
        '\red0\green255\blue0;' +
        '\red255\green0\blue255;' +
        '\red255\green0\blue0;' +
        '\red255\green255\blue0;' +
        '\red255\green255\blue255;' +
        '\red0\green0\blue128;' +
        '\red0\green128\blue128;' +
        '\red255\green0\blue0;' +
        '\red128\green0\blue128;' +
        '\red128\green0\blue0;' +
        '\red128\green128\blue0;' +
        '\red128\green128\blue128;' +
        '\red192\green192\blue192;}' +
      '\viewkind4\uc1\pard\lang1046\f0\fs20 ' + RTF + '}';
end;

class function TDictionaryManager.ValidateDictionaryModule(
  const DictionaryPath: string; ModuleType: TDictionaryModuleType):
  TDictionaryValidationResult;
var
  ConfigQuery: TSQLQuery;
  HasStrongSupport: Boolean;
  HasMorphologySupport: Boolean;
  IsEncrypted: Boolean;
  IsCompressed: Boolean;
  IsNonRTF: Boolean;
begin
  HasStrongSupport := False;
  HasMorphologySupport := False;
  IsEncrypted := False;
  IsCompressed := False;
  IsNonRTF := False;

  ConfigQuery := TProjectPersistence.CreateQueryObject(DictionaryPath);
  try
    ConfigQuery.SQL.Text :=
      'select distinct * from config where name in (''strong'',''morph'',''compressed'',''secure'',''do.encrypt'',''content.type'')';
    ConfigQuery.Prepare;
    ConfigQuery.Open;
    while not ConfigQuery.EOF do
    begin
      case ConfigQuery.FieldByName('name').AsString of
        'strong':
          HasStrongSupport := ConfigQuery.FieldByName('value').AsString = '1';
        'morph':
          HasMorphologySupport := ConfigQuery.FieldByName('value').AsString = '1';
        'compressed':
          IsCompressed := ConfigQuery.FieldByName('value').AsString = '1';
        'secure', 'do.encrypt':
          IsEncrypted := ConfigQuery.FieldByName('value').AsString = '1';
        'content.type':
          IsNonRTF := ConfigQuery.FieldByName('value').AsString.ToLower <> 'rtf';
      end;
      ConfigQuery.Next;
    end;
  finally
    ReleaseQuery(ConfigQuery);
  end;

  if (ModuleType = dmtMorphology) and not HasMorphologySupport then
    Exit(dvrMissingMorphologySupport);

  if (ModuleType = dmtStrong) and not HasStrongSupport then
    Exit(dvrMissingStrongSupport);

  if IsEncrypted then
    Exit(dvrEncrypted);

  if IsCompressed then
    Exit(dvrCompressed);

  if IsNonRTF then
    Exit(dvrNonRTF);

  Result := dvrValid;
end;

procedure TDictionaryManager.Clear;
var
  Index: Integer;
begin
  for Index := Low(FStrongDictionaries) to High(FStrongDictionaries) do
    ReleaseQuery(FStrongDictionaries[Index]);

  for Index := Low(FMorphologyDictionaries) to High(FMorphologyDictionaries) do
    ReleaseQuery(FMorphologyDictionaries[Index]);
end;

function TDictionaryManager.HasStrongDictionary(TextIndex: Integer): Boolean;
begin
  Result := Assigned(GetStrongDictionary(TextIndex));
end;

function TDictionaryManager.HasMorphologyDictionary(TextIndex: Integer): Boolean;
begin
  Result := Assigned(GetMorphologyDictionary(TextIndex));
end;

function TDictionaryManager.AssignStrongDictionary(const DictionaryPath: string;
  TextIndex: Integer): Boolean;
begin
  Result := False;
  if (DictionaryPath = '') or (TextIndex < Low(FStrongDictionaries)) or (TextIndex > High(FStrongDictionaries)) then
    Exit;

  ReleaseQuery(FStrongDictionaries[TextIndex]);
  FStrongDictionaries[TextIndex] := TProjectPersistence.CreateQueryObject(DictionaryPath);
  FStrongDictionaries[TextIndex].SQL.Text :=
    'select data from topics, content where subject = :strong and id = topic_id';
  FStrongDictionaries[TextIndex].Prepare;
  Result := True;
end;

function TDictionaryManager.AssignMorphologyDictionary(const DictionaryPath: string;
  TextIndex: Integer): Boolean;
begin
  Result := False;
  if (DictionaryPath = '') or (TextIndex < Low(FMorphologyDictionaries)) or (TextIndex > High(FMorphologyDictionaries)) then
    Exit;

  ReleaseQuery(FMorphologyDictionaries[TextIndex]);
  FMorphologyDictionaries[TextIndex] := TProjectPersistence.CreateQueryObject(DictionaryPath);
  FMorphologyDictionaries[TextIndex].SQL.Text :=
    'select data from topics, content where subject = :morf and id = topic_id';
  FMorphologyDictionaries[TextIndex].Prepare;
  Result := True;
end;

function TDictionaryManager.LookupStrong(const Strong: string; TextIndex: Integer): string;
var
  Query: TSQLQuery;
begin
  Result := '';
  Query := GetStrongDictionary(TextIndex);
  if not Assigned(Query) or (Strong = '') then
    Exit;

  Query.Close;
  Query.Params.ParamByName('strong').AsString := Strong;
  Query.Open;
  Result := FormatRTF(Query.Fields[0].AsString);
end;

function TDictionaryManager.LookupMorphology(const Morphology: string;
  TextIndex: Integer): string;
var
  Query: TSQLQuery;
  MorphologyKey: string;
begin
  Result := '';
  Query := GetMorphologyDictionary(TextIndex);
  if not Assigned(Query) or (Morphology = '') then
    Exit;

  MorphologyKey := Morphology;
  if Pos(' ', MorphologyKey) > 1 then
    MorphologyKey := Copy(MorphologyKey, 1, Pos(' ', MorphologyKey) - 1);

  Query.Close;
  Query.Params.ParamByName('morf').AsString := MorphologyKey;
  Query.Open;
  if Query.Fields[0].AsString = '' then
    Exit;

  Result := FormatRTF(Query.Fields[0].AsString);
end;

procedure TDictionaryManager.CollectPopupEntries(Syntagm: TSyntagm;
  TextIndex: Integer; out StrongEntries, MorphologyEntries: TDictionaryEntryArray);
begin
  SetLength(StrongEntries, 0);
  SetLength(MorphologyEntries, 0);

  if not Assigned(Syntagm) then
    Exit;

  AppendMorphologyEntries(MorphologyEntries, Syntagm, TextIndex);
  AppendStrongEntries(StrongEntries, Syntagm, TextIndex);
end;

end.