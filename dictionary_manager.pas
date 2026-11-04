unit dictionary_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sqldb, sqlite3conn, zstream, ONTTokenizer, syntagm,
  project_persistence;

type
  TDictionaryModuleType = (dmtStrong, dmtMorphology);
  TDictionaryValidationResult = (
    dvrValid,
    dvrMissingStrongSupport,
    dvrMissingMorphologySupport,
    dvrEncrypted,
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
    function GetStrongDictionary(TextIndex: Integer): TSQLQuery;
    function GetMorphologyDictionary(TextIndex: Integer): TSQLQuery;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure ReleaseQuery(var Query: TSQLQuery); static;
    class function FormatRTF(const RTF: string): string; static;
    class function DecodeContentData(const Data: RawByteString): string; static;
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
    { Definition of an untagged word from the default word dictionary. }
    function LookupWord(const Word: string; out MatchedKey: string): string;
  end;

{ Default dictionaries, shared by all projects (Tools > Default dictionaries).
  They are used when the project has no dictionary for a text, or when the
  project's dictionary has no entry: Strong's G-numbers go to the Greek one,
  H-numbers to the Hebrew one. The word dictionary (e.g. an English Bible
  dictionary) is looked up for words that have no Strong's or morphology.
  Empty paths turn a slot off. }
procedure SetDefaultDictionaries(const GreekStrongPath, HebrewStrongPath,
  MorphologyPath, WordPath: string);
procedure SetUseDefaultDictionaries(AEnabled: Boolean);
function GetUseDefaultDictionaries: Boolean;
procedure GetDefaultDictionaryPaths(out GreekStrongPath, HebrewStrongPath,
  MorphologyPath, WordPath: string);

implementation

var
  GUseDefaultDictionaries: Boolean = True;
  GDefaultStrongGreek: TSQLQuery = nil;
  GDefaultStrongHebrew: TSQLQuery = nil;
  GDefaultMorphology: TSQLQuery = nil;
  GDefaultWord: TSQLQuery = nil;
  GDefaultPaths: array[0..3] of string;

function IsNonEmptySQLiteFile(const APath: string): Boolean;
var
  sr: TSearchRec;
begin
  Result := False;
  if (APath = '') or (not FileExists(APath)) then
    Exit;
  if FindFirst(APath, faAnyFile, sr) = 0 then
  begin
    try
      if sr.Size = 0 then
      begin
        // Clean up 0-byte stub file created by an earlier failed SQLite open
        SysUtils.DeleteFile(APath);
        Exit(False);
      end;
      Result := sr.Size >= 100;
    finally
      SysUtils.FindClose(sr);
    end;
  end;
end;

function SQLiteTableExists(Query: TSQLQuery; const TableName: string): Boolean;
begin
  Result := False;
  if not Assigned(Query) then
    Exit;
  try
    Query.Close;
    Query.SQL.Text :=
      'select count(*) as cnt from sqlite_master where type=''table'' and lower(name)=lower(:tbl)';
    Query.Params.ParamByName('tbl').AsString := TableName;
    Query.Open;
    if not Query.EOF then
      Result := Query.FieldByName('cnt').AsInteger > 0;
    Query.Close;
  except
    Result := False;
  end;
end;

function OpenDefaultDictionary(const Path, Sql: string): TSQLQuery;
begin
  Result := nil;
  if not IsNonEmptySQLiteFile(Path) then
    Exit;
  try
    Result := TProjectPersistence.CreateQueryObject(Path);
    if (not SQLiteTableExists(Result, 'topics')) or
       (not SQLiteTableExists(Result, 'content')) then
    begin
      TDictionaryManager.ReleaseQuery(Result);
      Result := nil;
      Exit;
    end;
    Result.SQL.Text := Sql;
    Result.Prepare;
  except
    TDictionaryManager.ReleaseQuery(Result);
    Result := nil;
  end;
end;

procedure SetDefaultDictionaries(const GreekStrongPath, HebrewStrongPath,
  MorphologyPath, WordPath: string);

  procedure Update(Slot: Integer; var Query: TSQLQuery; const Path: string;
    ModuleType: TDictionaryModuleType; CheckModule: Boolean; const Sql: string);
  var
    FullPath: string;
  begin
    FullPath := Trim(Path);
    if (FullPath <> '') and not FileExists(FullPath) and
       FileExists(ExtractFilePath(ParamStr(0)) + FullPath) then
      FullPath := ExtractFilePath(ParamStr(0)) + FullPath;
    if (FullPath = GDefaultPaths[Slot]) and (Assigned(Query) or (FullPath = '')) then
      Exit;
    TDictionaryManager.ReleaseQuery(Query);
    GDefaultPaths[Slot] := FullPath;
    if not IsNonEmptySQLiteFile(FullPath) then
      Exit;
    try
      if CheckModule and (TDictionaryManager.ValidateDictionaryModule(FullPath,
         ModuleType) <> dvrValid) then
        Exit;
    except
      Exit;
    end;
    Query := OpenDefaultDictionary(FullPath, Sql);
  end;

begin
  Update(0, GDefaultStrongGreek, GreekStrongPath, dmtStrong, True,
    'select data from topics, content where subject = :strong and id = topic_id');
  Update(1, GDefaultStrongHebrew, HebrewStrongPath, dmtStrong, True,
    'select data from topics, content where subject = :strong and id = topic_id');
  Update(2, GDefaultMorphology, MorphologyPath, dmtMorphology, True,
    'select data from topics, content where subject = :morf and id = topic_id');
  Update(3, GDefaultWord, WordPath, dmtStrong, False,
    'select subject, data from topics, content where subject = :word collate nocase ' +
    'and id = topic_id limit 1');
end;

procedure SetUseDefaultDictionaries(AEnabled: Boolean);
begin
  GUseDefaultDictionaries := AEnabled;
end;

function GetUseDefaultDictionaries: Boolean;
begin
  Result := GUseDefaultDictionaries;
end;

procedure GetDefaultDictionaryPaths(out GreekStrongPath, HebrewStrongPath,
  MorphologyPath, WordPath: string);
begin
  GreekStrongPath  := GDefaultPaths[0];
  HebrewStrongPath := GDefaultPaths[1];
  MorphologyPath   := GDefaultPaths[2];
  WordPath         := GDefaultPaths[3];
end;

{ Strong's keys as written in modules vary: G26, G0026, H1234a. }
function StrongKeyCandidates(const Strong: string): TStringArray;
var
  Prefix, Digits, Suffix: string;
  Index: Integer;

  procedure Add(const Key: string);
  var
    K: Integer;
  begin
    if Key = '' then
      Exit;
    for K := 0 to High(Result) do
      if Result[K] = Key then
        Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Key;
  end;

begin
  Result := nil;
  Add(Strong);
  if Length(Strong) < 2 then
    Exit;
  Prefix := UpperCase(Strong[1]);
  if not ((Prefix = 'G') or (Prefix = 'H')) then
    Exit;
  Index := 2;
  Digits := '';
  while (Index <= Length(Strong)) and (Strong[Index] in ['0'..'9']) do
  begin
    Digits := Digits + Strong[Index];
    Inc(Index);
  end;
  if Digits = '' then
    Exit;
  Suffix := Copy(Strong, Index, MaxInt);
  while (Length(Digits) > 1) and (Digits[1] = '0') do
    Delete(Digits, 1, 1);
  Add(Prefix + Digits + Suffix);
  Add(Prefix + StringOfChar('0', 4 - Length(Digits)) + Digits + Suffix);
  if Suffix <> '' then
  begin
    Add(Prefix + Digits);
    Add(Prefix + StringOfChar('0', 4 - Length(Digits)) + Digits);
  end;
end;

{ First non-empty data for any of the keys. }
function QueryDefinition(Query: TSQLQuery; const ParamName: string;
  const Keys: array of string): string;
var
  Key: string;
begin
  Result := '';
  if not Assigned(Query) then
    Exit;
  for Key in Keys do
  begin
    Query.Close;
    Query.Params.ParamByName(ParamName).AsString := Key;
    Query.Open;
    if not Query.EOF then
      Result := Query.FieldByName('data').AsString;
    if Result <> '' then
      Exit;
  end;
end;

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
  EntryKey, Def: string;
  PairSyntagm: TSyntagm;
begin
  if Syntagm.Strong.Count > 0 then
  begin
    if not HasStrongDictionary(TextIndex) then
      Exit;
    for EntryKey in Syntagm.Strong do
      AppendEntry(Entries, EntryKey, LookupStrong(EntryKey, TextIndex));
    Exit;
  end;

  if not HasStrongDictionary(TextIndex) and not HasStrongDictionary(0) then
    Exit;

  for PairSyntagm in Syntagm.Pairs do
    for EntryKey in PairSyntagm.Strong do
    begin
      Def := LookupStrong(EntryKey, TextIndex);
      if (Def = '') and (TextIndex <> 0) then
        Def := LookupStrong(EntryKey, 0);
      AppendEntry(Entries, EntryKey, Def);
    end;
end;

procedure TDictionaryManager.AppendMorphologyEntries(
  var Entries: TDictionaryEntryArray; Syntagm: TSyntagm; TextIndex: Integer);
var
  EntryKey, Def: string;
  PairSyntagm: TSyntagm;
begin
  if Syntagm.Morph.Count > 0 then
  begin
    if not HasMorphologyDictionary(TextIndex) then
      Exit;
    for EntryKey in Syntagm.Morph do
      AppendEntry(Entries, EntryKey, LookupMorphology(EntryKey, TextIndex));
    Exit;
  end;

  if not HasMorphologyDictionary(TextIndex) and not HasMorphologyDictionary(0) then
    Exit;

  for PairSyntagm in Syntagm.Pairs do
    for EntryKey in PairSyntagm.Morph do
    begin
      Def := LookupMorphology(EntryKey, TextIndex);
      if (Def = '') and (TextIndex <> 0) then
        Def := LookupMorphology(EntryKey, 0);
      AppendEntry(Entries, EntryKey, Def);
    end;
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
var
  conn: TSQLite3Connection;
  trans: TSQLTransaction;
begin
  if not Assigned(Query) then
    Exit;

  try
    if Query.Active then
      Query.Close;
  except
  end;
  try
    if Query.Prepared then
      Query.UnPrepare;
  except
  end;
  if Assigned(Query.DataBase) then
  begin
    conn := TSQLite3Connection(Query.DataBase);
    try
      if Assigned(conn.Transaction) and conn.Transaction.Active then
        conn.Transaction.Rollback;
    except
    end;
    try
      if conn.Connected then
        conn.Close;
    except
    end;
    if Assigned(conn.Transaction) then
    begin
      trans := TSQLTransaction(conn.Transaction);
      try
        Query.Transaction := nil;
        conn.Transaction := nil;
        trans.Free;
      except
      end;
    end;
    try
      Query.DataBase := nil;
    except
    end;
    try
      Query.Free;
    except
    end;
    Query := nil;
    try
      conn.Free;
    except
    end;
  end
  else
  begin
    try
      Query.Transaction := nil;
      Query.Free;
    except
    end;
    Query := nil;
  end;
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
  IsNonRTF: Boolean;
  HasTopicsAndContent: Boolean;
begin
  HasStrongSupport := False;
  HasMorphologySupport := False;
  IsEncrypted := False;
  IsNonRTF := False;

  if not IsNonEmptySQLiteFile(DictionaryPath) then
  begin
    if ModuleType = dmtMorphology then
      Exit(dvrMissingMorphologySupport)
    else
      Exit(dvrMissingStrongSupport);
  end;

  ConfigQuery := nil;
  try
    ConfigQuery := TProjectPersistence.CreateQueryObject(DictionaryPath);
    try
      HasTopicsAndContent := SQLiteTableExists(ConfigQuery, 'topics') and
                             SQLiteTableExists(ConfigQuery, 'content');
      if not SQLiteTableExists(ConfigQuery, 'config') then
      begin
        if HasTopicsAndContent then
          Exit(dvrValid)
        else if ModuleType = dmtMorphology then
          Exit(dvrMissingMorphologySupport)
        else
          Exit(dvrMissingStrongSupport);
      end;

      ConfigQuery.SQL.Text :=
        'select distinct * from config where name in (''strong'',''morph'',''secure'',''do.encrypt'',''content.type'')';
      ConfigQuery.Prepare;
      ConfigQuery.Open;
      while not ConfigQuery.EOF do
      begin
        case ConfigQuery.FieldByName('name').AsString of
          'strong':
            HasStrongSupport := ConfigQuery.FieldByName('value').AsString = '1';
          'morph':
            HasMorphologySupport := ConfigQuery.FieldByName('value').AsString = '1';
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
  except
    if ModuleType = dmtMorphology then
      Exit(dvrMissingMorphologySupport)
    else
      Exit(dvrMissingStrongSupport);
  end;

  if (ModuleType = dmtMorphology) and not HasMorphologySupport then
    Exit(dvrMissingMorphologySupport);

  if (ModuleType = dmtStrong) and not HasStrongSupport then
    Exit(dvrMissingStrongSupport);

  if IsEncrypted then
    Exit(dvrEncrypted);

  if IsNonRTF then
    Exit(dvrNonRTF);

  Result := dvrValid;
end;

function InflateStream(Source: TStream; SkipHeader: Boolean;
  out Output: RawByteString): Boolean;
var
  Decompressor: TDecompressionStream;
  Buffer: array[0..8191] of Byte;
  Count: Integer;
  Target: TStringStream;
begin
  Output := '';
  Result := False;
  Target := TStringStream.Create('');
  try
    try
      Decompressor := TDecompressionStream.Create(Source, SkipHeader);
      try
        repeat
          Count := Decompressor.Read(Buffer, SizeOf(Buffer));
          if Count > 0 then
            Target.WriteBuffer(Buffer, Count);
        until Count = 0;
      finally
        Decompressor.Free;
      end;
      Output := Target.DataString;
      Result := Output <> '';
    except
      Output := '';
    end;
  finally
    Target.Free;
  end;
end;

{ theWord modules with config "compressed=1" store content.data as a zlib
  stream. Uncompressed RTF is returned unchanged; for anything else try a
  zlib stream, then a raw deflate stream, then a zlib stream behind a short
  header. If nothing works the data is returned as it is. }
class function TDictionaryManager.DecodeContentData(const Data: RawByteString): string;
var
  Source: TStringStream;
  Decoded: RawByteString;
  Offset, Limit: Integer;
begin
  Result := Data;
  if (Data = '') or (Data[1] = '{') or (Data[1] = '\') then
    Exit;

  Source := TStringStream.Create(Data);
  try
    Source.Position := 0;
    if InflateStream(Source, False, Decoded) then
      Exit(Decoded);

    Source.Position := 0;
    if InflateStream(Source, True, Decoded) then
      Exit(Decoded);

    Limit := Length(Data) - 2;
    if Limit > 32 then
      Limit := 32;
    for Offset := 1 to Limit do
      if (Data[Offset + 1] = #$78) and
        (Data[Offset + 2] in [#$01, #$5E, #$9C, #$DA]) then
      begin
        Source.Position := Offset;
        if InflateStream(Source, False, Decoded) then
          Exit(Decoded);
      end;
  finally
    Source.Free;
  end;
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
  if GUseDefaultDictionaries then
    Result := Assigned(GDefaultStrongGreek) or Assigned(GDefaultStrongHebrew) or
      Assigned(GetStrongDictionary(TextIndex))
  else
    Result := Assigned(GetStrongDictionary(TextIndex));
end;

function TDictionaryManager.HasMorphologyDictionary(TextIndex: Integer): Boolean;
begin
  if GUseDefaultDictionaries then
    Result := Assigned(GDefaultMorphology) or Assigned(GetMorphologyDictionary(TextIndex))
  else
    Result := Assigned(GetMorphologyDictionary(TextIndex));
end;

function TDictionaryManager.AssignStrongDictionary(const DictionaryPath: string;
  TextIndex: Integer): Boolean;
begin
  Result := False;
  if (TextIndex < Low(FStrongDictionaries)) or (TextIndex > High(FStrongDictionaries)) then
    Exit;

  ReleaseQuery(FStrongDictionaries[TextIndex]);
  if DictionaryPath = '' then
    Exit(True);
  if not IsNonEmptySQLiteFile(DictionaryPath) then
    Exit(False);

  try
    FStrongDictionaries[TextIndex] := TProjectPersistence.CreateQueryObject(DictionaryPath);
    if (not SQLiteTableExists(FStrongDictionaries[TextIndex], 'topics')) or
       (not SQLiteTableExists(FStrongDictionaries[TextIndex], 'content')) then
    begin
      ReleaseQuery(FStrongDictionaries[TextIndex]);
      Exit(False);
    end;
    FStrongDictionaries[TextIndex].SQL.Text :=
      'select data from topics, content where subject = :strong and id = topic_id';
    FStrongDictionaries[TextIndex].Prepare;
    Result := True;
  except
    ReleaseQuery(FStrongDictionaries[TextIndex]);
    Result := False;
  end;
end;

function TDictionaryManager.AssignMorphologyDictionary(const DictionaryPath: string;
  TextIndex: Integer): Boolean;
begin
  Result := False;
  if (TextIndex < Low(FMorphologyDictionaries)) or (TextIndex > High(FMorphologyDictionaries)) then
    Exit;

  ReleaseQuery(FMorphologyDictionaries[TextIndex]);
  if DictionaryPath = '' then
    Exit(True);
  if not IsNonEmptySQLiteFile(DictionaryPath) then
    Exit(False);

  try
    FMorphologyDictionaries[TextIndex] := TProjectPersistence.CreateQueryObject(DictionaryPath);
    if (not SQLiteTableExists(FMorphologyDictionaries[TextIndex], 'topics')) or
       (not SQLiteTableExists(FMorphologyDictionaries[TextIndex], 'content')) then
    begin
      ReleaseQuery(FMorphologyDictionaries[TextIndex]);
      Exit(False);
    end;
    FMorphologyDictionaries[TextIndex].SQL.Text :=
      'select data from topics, content where subject = :morf and id = topic_id';
    FMorphologyDictionaries[TextIndex].Prepare;
    Result := True;
  except
    ReleaseQuery(FMorphologyDictionaries[TextIndex]);
    Result := False;
  end;
end;

function TDictionaryManager.LookupStrong(const Strong: string; TextIndex: Integer): string;
var
  Keys: TStringArray;
  Data: string;
begin
  Result := '';
  if Strong = '' then
    Exit;
  Keys := StrongKeyCandidates(Strong);
  Data := '';
  if GUseDefaultDictionaries then
  begin
    if UpCase(Strong[1]) = 'H' then
      Data := QueryDefinition(GDefaultStrongHebrew, 'strong', Keys)
    else
      Data := QueryDefinition(GDefaultStrongGreek, 'strong', Keys);
    if Data = '' then
      Data := QueryDefinition(GetStrongDictionary(TextIndex), 'strong', Keys);
  end
  else
    Data := QueryDefinition(GetStrongDictionary(TextIndex), 'strong', Keys);
  if Data <> '' then
    Result := FormatRTF(DecodeContentData(Data));
end;

function TDictionaryManager.LookupMorphology(const Morphology: string;
  TextIndex: Integer): string;
var
  MorphologyKey, Data: string;
begin
  Result := '';
  if Morphology = '' then
    Exit;

  MorphologyKey := Morphology;
  if Pos(' ', MorphologyKey) > 1 then
    MorphologyKey := Copy(MorphologyKey, 1, Pos(' ', MorphologyKey) - 1);

  Data := '';
  if GUseDefaultDictionaries then
  begin
    Data := QueryDefinition(GDefaultMorphology, 'morf', [MorphologyKey]);
    if Data = '' then
      Data := QueryDefinition(GetMorphologyDictionary(TextIndex), 'morf', [MorphologyKey]);
  end
  else
    Data := QueryDefinition(GetMorphologyDictionary(TextIndex), 'morf', [MorphologyKey]);
  if Data <> '' then
    Result := FormatRTF(DecodeContentData(Data));
end;

function TDictionaryManager.LookupWord(const Word: string;
  out MatchedKey: string): string;
var
  Clean, Lower, Candidate, Data: string;
  Candidates: array of string;

  procedure Add(const Value: string);
  begin
    if Value <> '' then
    begin
      SetLength(Candidates, Length(Candidates) + 1);
      Candidates[High(Candidates)] := Value;
    end;
  end;

begin
  Result := '';
  MatchedKey := '';
  if not Assigned(GDefaultWord) then
    Exit;
  Clean := Trim(Word);
  while (Clean <> '') and not (Clean[1] in ['A'..'Z', 'a'..'z', #128..#255]) do
    Delete(Clean, 1, 1);
  while (Clean <> '') and not (Clean[Length(Clean)] in ['A'..'Z', 'a'..'z', #128..#255]) do
    Delete(Clean, Length(Clean), 1);
  if Pos('''s', LowerCase(Clean)) = Length(Clean) - 1 then
    Clean := Copy(Clean, 1, Length(Clean) - 2);
  if Clean = '' then
    Exit;

  Lower := LowerCase(Clean);
  Candidates := nil;
  Add(Clean);
  // simple English stems: plurals and verb endings
  if AnsiEndsStr('ies', Lower) then Add(Copy(Clean, 1, Length(Clean) - 3) + 'y');
  if AnsiEndsStr('es', Lower) then Add(Copy(Clean, 1, Length(Clean) - 2));
  if AnsiEndsStr('s', Lower) and not AnsiEndsStr('ss', Lower) then
    Add(Copy(Clean, 1, Length(Clean) - 1));
  if AnsiEndsStr('ed', Lower) then
  begin
    Add(Copy(Clean, 1, Length(Clean) - 2));
    Add(Copy(Clean, 1, Length(Clean) - 1));
  end;
  if AnsiEndsStr('ing', Lower) then
  begin
    Add(Copy(Clean, 1, Length(Clean) - 3));
    Add(Copy(Clean, 1, Length(Clean) - 3) + 'e');
  end;

  for Candidate in Candidates do
  begin
    GDefaultWord.Close;
    GDefaultWord.Params.ParamByName('word').AsString := Candidate;
    GDefaultWord.Open;
    if not GDefaultWord.EOF then
    begin
      Data := GDefaultWord.FieldByName('data').AsString;
      if Data <> '' then
      begin
        MatchedKey := GDefaultWord.FieldByName('subject').AsString;
        Result := FormatRTF(DecodeContentData(Data));
        Exit;
      end;
    end;
  end;
end;

procedure TDictionaryManager.CollectPopupEntries(Syntagm: TSyntagm;
  TextIndex: Integer; out StrongEntries, MorphologyEntries: TDictionaryEntryArray);
var
  Definition, MatchedKey: string;
begin
  SetLength(StrongEntries, 0);
  SetLength(MorphologyEntries, 0);

  if not Assigned(Syntagm) then
    Exit;

  AppendMorphologyEntries(MorphologyEntries, Syntagm, TextIndex);
  AppendStrongEntries(StrongEntries, Syntagm, TextIndex);

  // untagged word: try the default word dictionary when default dictionaries are enabled
  if GUseDefaultDictionaries and
     (Length(StrongEntries) = 0) and (Length(MorphologyEntries) = 0) and
     (Syntagm.Kind = skSyntagm) and Assigned(GDefaultWord) then
  begin
    Definition := LookupWord(Syntagm.Text, MatchedKey);
    if Definition <> '' then
      AppendEntry(StrongEntries, MatchedKey, Definition);
  end;
end;

finalization
  TDictionaryManager.ReleaseQuery(GDefaultStrongGreek);
  TDictionaryManager.ReleaseQuery(GDefaultStrongHebrew);
  TDictionaryManager.ReleaseQuery(GDefaultMorphology);
  TDictionaryManager.ReleaseQuery(GDefaultWord);

end.
