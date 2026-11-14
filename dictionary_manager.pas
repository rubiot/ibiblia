unit dictionary_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, db, sqldb, sqlite3conn, zstream, ONTTokenizer, syntagm,
  project_persistence;

type
  TDictionaryModuleType = (dmtStrong, dmtMorphology);
  TDictionaryValidationResult = (
    dvrValid,
    dvrMissingStrongSupport,
    dvrMissingMorphologySupport,
    dvrEncrypted, dvrCompressed,
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

uses
  Forms, LazUTF8, ui_style;

var
  GUseDefaultDictionaries: Boolean = True;
  GDefaultStrongGreek: TSQLQuery = nil;
  GDefaultStrongHebrew: TSQLQuery = nil;
  GDefaultMorphology: TSQLQuery = nil;
  GDefaultWord: TSQLQuery = nil;
  GDefaultPaths: array[0..3] of string;

function ResolveDictionaryFilePath(const APath: string): string;
var
  Clean, Normalized, BaseName, ExeDir: string;
  I: Integer;
begin
  Clean := Trim(APath);
  if Clean = '' then
    Exit('');
  if FileExists(Clean) then
    Exit(Clean);
  Normalized := StringReplace(StringReplace(Clean, '\', PathDelim, [rfReplaceAll]),
    '/', PathDelim, [rfReplaceAll]);
  if FileExists(Normalized) then
    Exit(Normalized);
  BaseName := Clean;
  for I := Length(Clean) downto 1 do
    if Clean[I] in ['/', '\'] then
    begin
      BaseName := Copy(Clean, I + 1, MaxInt);
      Break;
    end;
  ExeDir := ExtractFilePath(ParamStr(0));
  if FileExists(ExeDir + Normalized) then
    Exit(ExeDir + Normalized);
  if (BaseName <> '') and FileExists(ExeDir + BaseName) then
    Exit(ExeDir + BaseName);
  if (BaseName <> '') and FileExists(ExeDir + 'dicts' + PathDelim + BaseName) then
    Exit(ExeDir + 'dicts' + PathDelim + BaseName);
  Result := Normalized;
end;

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
    FullPath := ResolveDictionaryFilePath(Path);
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
    'select data, data2 from topics, content where subject = :strong and id = topic_id');
  Update(1, GDefaultStrongHebrew, HebrewStrongPath, dmtStrong, True,
    'select data, data2 from topics, content where subject = :strong and id = topic_id');
  Update(2, GDefaultMorphology, MorphologyPath, dmtMorphology, True,
    'select data, data2 from topics, content where subject = :morf and id = topic_id');
  Update(3, GDefaultWord, WordPath, dmtStrong, False,
    'select subject, data, data2 from topics, content where subject = :word collate nocase ' +
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

{ Read a blob or text field from a dictionary query without string truncation on binary zlib streams. }
function ReadDictionaryFieldBytes(AField: TField): RawByteString;
var
  MemStream: TMemoryStream;
  RawStr: RawByteString;
begin
  Result := '';
  if (AField = nil) or AField.IsNull then
    Exit;
  MemStream := TMemoryStream.Create;
  try
    if AField is TBlobField then
      TBlobField(AField).SaveToStream(MemStream)
    else
    begin
      RawStr := AField.AsString;
      if RawStr <> '' then
        MemStream.WriteBuffer(RawStr[1], Length(RawStr));
    end;
    if MemStream.Size > 0 then
    begin
      SetLength(Result, MemStream.Size);
      MemStream.Position := 0;
      MemStream.ReadBuffer(Result[1], MemStream.Size);
    end;
  finally
    MemStream.Free;
  end;
end;

function ExtractDecodedRowContent(Query: TSQLQuery): string;
var
  Raw: RawByteString;
  PId, EndIdx, TargetId: Integer;
  IdStr, LinkSubj: string;
  LinkQry: TSQLQuery;
begin
  Result := '';
  if (Query = nil) or Query.EOF then
    Exit;

  if (Query.FieldCount > 0) and (not Query.Fields[0].IsNull) then
  begin
    Raw := ReadDictionaryFieldBytes(Query.Fields[0]);
    if Raw <> '' then
      Result := TDictionaryManager.DecodeContentData(Raw);
  end;
  if (Result = '') and (Query.FieldCount > 1) and (not Query.Fields[1].IsNull) then
  begin
    Raw := ReadDictionaryFieldBytes(Query.Fields[1]);
    if Raw <> '' then
      Result := TDictionaryManager.DecodeContentData(Raw);
  end;

  { Follow theWord meta=_twmgc_ / type=link redirect topics (e.g. Strong number redirecting to lemma or BDB entry) }
  if (Result <> '') and ((Pos('meta=_twmgc_', Result) > 0) or (Pos('type=link', Result) > 0)) and
     Assigned(Query.DataBase) then
  begin
    PId := Pos('id=', Result);
    if PId > 0 then
    begin
      Inc(PId, 3);
      EndIdx := PId;
      while (EndIdx <= Length(Result)) and (Result[EndIdx] in ['0'..'9']) do
        Inc(EndIdx);
      IdStr := Copy(Result, PId, EndIdx - PId);
      TargetId := StrToIntDef(IdStr, -1);
      if TargetId >= 0 then
      begin
        Result := '';
        LinkQry := TSQLQuery.Create(nil);
        try
          LinkQry.DataBase := Query.DataBase;
          LinkQry.Transaction := Query.Transaction;
          LinkQry.SQL.Text := 'SELECT data, data2 FROM content WHERE topic_id = :tid';
          LinkQry.Params.ParamByName('tid').AsInteger := TargetId;
          LinkQry.Open;
          try
            if not LinkQry.EOF then
            begin
              Raw := ReadDictionaryFieldBytes(LinkQry.Fields[0]);
              if Raw <> '' then
                Result := TDictionaryManager.DecodeContentData(Raw);
              if (Result = '') and (LinkQry.FieldCount > 1) and (not LinkQry.Fields[1].IsNull) then
              begin
                Raw := ReadDictionaryFieldBytes(LinkQry.Fields[1]);
                if Raw <> '' then
                  Result := TDictionaryManager.DecodeContentData(Raw);
              end;
            end;
          finally
            LinkQry.Close;
          end;
        except
          Result := '';
        end;
        LinkQry.Free;
      end;
    end
    else
    begin
      PId := Pos('subject=', Result);
      if PId > 0 then
      begin
        Inc(PId, 8);
        EndIdx := PId;
        while (EndIdx <= Length(Result)) and not (Result[EndIdx] in [#13, #10]) do
          Inc(EndIdx);
        LinkSubj := Trim(Copy(Result, PId, EndIdx - PId));
        if LinkSubj <> '' then
        begin
          Result := '';
          LinkQry := TSQLQuery.Create(nil);
          try
            LinkQry.DataBase := Query.DataBase;
            LinkQry.Transaction := Query.Transaction;
            LinkQry.SQL.Text := 'SELECT c.data, c.data2 FROM topics t JOIN content c ON t.id = c.topic_id WHERE LOWER(t.subject) = LOWER(:subj) LIMIT 1';
            LinkQry.Params.ParamByName('subj').AsString := LinkSubj;
            LinkQry.Open;
            try
              if not LinkQry.EOF then
              begin
                Raw := ReadDictionaryFieldBytes(LinkQry.Fields[0]);
                if Raw <> '' then
                  Result := TDictionaryManager.DecodeContentData(Raw);
                if (Result = '') and (LinkQry.FieldCount > 1) and (not LinkQry.Fields[1].IsNull) then
                begin
                  Raw := ReadDictionaryFieldBytes(LinkQry.Fields[1]);
                  if Raw <> '' then
                    Result := TDictionaryManager.DecodeContentData(Raw);
                end;
              end;
            finally
              LinkQry.Close;
            end;
          except
            Result := '';
          end;
          LinkQry.Free;
        end;
      end;
    end;
  end;
end;

{ Strong's keys as written in modules vary: G26, G0026, H1234a, H07225, Hc/H7225, <WH7225x>. }
function NormalizeStrongToken(const Strong: string; out IsHebrew: Boolean;
  out NumVal: Integer; out DigitsOnly, Suffix: string): string;
var
  S, CleanPart: string;
  SpacePos, SlashPos, I: Integer;
begin
  Result := '';
  IsHebrew := False;
  NumVal := -1;
  DigitsOnly := '';
  Suffix := '';

  S := Trim(StringReplace(Strong, #$E2#$80#$AF, ' ', [rfReplaceAll]));
  if S = '' then
    Exit;

  while (S <> '') and (S[1] in ['<', ' ', '"', '''']) do
    Delete(S, 1, 1);
  while (S <> '') and (S[Length(S)] in ['>', ' ', '"', '''']) do
    Delete(S, Length(S), 1);

  SpacePos := Pos(' ', S);
  if SpacePos > 1 then
    S := Copy(S, 1, SpacePos - 1);

  if (Length(S) >= 2) and ((UpperCase(Copy(S, 1, 2)) = 'WH') or (UpperCase(Copy(S, 1, 2)) = 'WG')) then
    Delete(S, 1, 1);

  CleanPart := S;
  SlashPos := LastDelimiter('/', CleanPart);
  if SlashPos > 0 then
    CleanPart := Copy(CleanPart, SlashPos + 1, MaxInt);
  if (Length(CleanPart) >= 2) and ((UpperCase(Copy(CleanPart, 1, 2)) = 'WH') or (UpperCase(Copy(CleanPart, 1, 2)) = 'WG')) then
    Delete(CleanPart, 1, 1);

  if CleanPart = '' then
    CleanPart := S;
  if CleanPart = '' then
    Exit;

  if (CleanPart[1] in ['H', 'h']) or (S[1] in ['H', 'h']) or (Pos('/H', UpperCase(S)) > 0) then
    IsHebrew := True
  else
    IsHebrew := False;

  for I := 1 to Length(CleanPart) do
  begin
    if CleanPart[I] in ['0'..'9'] then
      DigitsOnly := DigitsOnly + CleanPart[I]
    else if (DigitsOnly <> '') and (CleanPart[I] in ['a'..'z', 'A'..'Z']) then
      Suffix := Suffix + CleanPart[I];
  end;

  { Strip trailing theWord reuse/untranslated flags ('s' or 'x') if not a real sub-entry like 'a'..'d' }
  if (Length(Suffix) = 1) and (Suffix[1] in ['x', 'X', 's', 'S']) then
    Suffix := '';

  NumVal := StrToIntDef(DigitsOnly, -1);
  Result := CleanPart;
end;

function StrongKeyCandidates(const Strong: string): TStringArray;
var
  CleanPart, DigitsOnly, Suffix, LangPrefix: string;
  IsHebrew: Boolean;
  NumVal: Integer;

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
  CleanPart := NormalizeStrongToken(Strong, IsHebrew, NumVal, DigitsOnly, Suffix);
  if CleanPart = '' then
    Exit;

  if IsHebrew then
    LangPrefix := 'H'
  else
    LangPrefix := 'G';

  if NumVal >= 0 then
  begin
    if Suffix <> '' then
    begin
      Add(LangPrefix + IntToStr(NumVal) + Suffix);
      Add(LowerCase(LangPrefix) + IntToStr(NumVal) + Suffix);
      Add(LangPrefix + Format('%.4d', [NumVal]) + Suffix);
      Add(LowerCase(LangPrefix) + Format('%.4d', [NumVal]) + Suffix);
      Add(LangPrefix + Format('%.5d', [NumVal]) + Suffix);
      Add(LowerCase(LangPrefix) + Format('%.5d', [NumVal]) + Suffix);
    end;

    Add(LangPrefix + IntToStr(NumVal));
    Add(LowerCase(LangPrefix) + IntToStr(NumVal));
    Add(LangPrefix + DigitsOnly);
    Add(LowerCase(LangPrefix) + DigitsOnly);
    Add(CleanPart);
    Add(Strong);

    Add(LangPrefix + Format('%.4d', [NumVal]));
    Add(LowerCase(LangPrefix) + Format('%.4d', [NumVal]));
    Add(LangPrefix + Format('%.5d', [NumVal]));
    Add(LowerCase(LangPrefix) + Format('%.5d', [NumVal]));

    Add(IntToStr(NumVal));
    Add(DigitsOnly);
    Add(Format('%.4d', [NumVal]));
    Add(Format('%.5d', [NumVal]));
  end
  else
  begin
    Add(CleanPart);
    Add(Strong);
  end;
end;

{ First non-empty decoded definition for any of the keys, including topics_wordindex and headword fallbacks. }
function QueryDefinition(Query: TSQLQuery; const ParamName: string;
  const Keys: array of string; IsStrongQuery: Boolean = False;
  IsHebrew: Boolean = False; NumVal: Integer = -1): string;
var
  Key, U1, U2, U3, LangPrefix: string;
  AuxQry: TSQLQuery;
begin
  Result := '';
  if not Assigned(Query) then
    Exit;
  for Key in Keys do
  begin
    if Key = '' then
      Continue;
    try
      Query.Close;
      Query.Params.ParamByName(ParamName).AsString := Key;
      Query.Open;
      try
        if not Query.EOF then
          Result := ExtractDecodedRowContent(Query);
      finally
        Query.Close;
      end;
    except
      Result := '';
    end;
    if Result <> '' then
      Exit;
  end;

  { Fallback for theWord Lexicon dictionaries (e.g. uBDB, BDB, TWOT, Mickelson, Strong)
    that index Strong numbers in topics_wordindex or prefix them in topics.subject }
  if IsStrongQuery and (NumVal >= 0) and Assigned(Query.DataBase) then
  begin
    if IsHebrew then
      LangPrefix := 'H'
    else
      LangPrefix := 'G';
    U1 := LangPrefix + IntToStr(NumVal);
    U2 := LangPrefix + Format('%.4d', [NumVal]);
    U3 := LangPrefix + Format('%.5d', [NumVal]);

    AuxQry := TSQLQuery.Create(nil);
    try
      AuxQry.DataBase := Query.DataBase;
      AuxQry.Transaction := Query.Transaction;

      if SQLiteTableExists(AuxQry, 'topics_wordindex') then
      begin
        try
          AuxQry.SQL.Text :=
            'SELECT c.data, c.data2 FROM topics_wordindex wi ' +
            'JOIN content c ON c.topic_id = wi.id ' +
            'WHERE LOWER(wi.word) IN (:w1, :w2, :w3) ' +
            'ORDER BY wi.priority ASC, wi.id ASC LIMIT 1';
          AuxQry.Params.ParamByName('w1').AsString := LowerCase(U1);
          AuxQry.Params.ParamByName('w2').AsString := LowerCase(U2);
          AuxQry.Params.ParamByName('w3').AsString := LowerCase(U3);
          AuxQry.Open;
          try
            if not AuxQry.EOF then
              Result := ExtractDecodedRowContent(AuxQry);
          finally
            AuxQry.Close;
          end;
        except
          Result := '';
        end;
        if Result <> '' then
          Exit;
      end;

      try
        AuxQry.SQL.Text :=
          'SELECT c.data, c.data2 FROM topics t ' +
          'JOIN content c ON c.topic_id = t.id ' +
          'WHERE UPPER(t.subject) = :u1 OR UPPER(t.subject) = :u2 OR UPPER(t.subject) = :u3 ' +
          '   OR UPPER(t.subject) LIKE :p1 OR UPPER(t.subject) LIKE :p2 ' +
          '   OR UPPER(t.subject) LIKE :p3 OR UPPER(t.subject) LIKE :p4 ' +
          'ORDER BY t.id ASC LIMIT 1';
        AuxQry.Params.ParamByName('u1').AsString := U1;
        AuxQry.Params.ParamByName('u2').AsString := U2;
        AuxQry.Params.ParamByName('u3').AsString := U3;
        AuxQry.Params.ParamByName('p1').AsString := U1 + ' %';
        AuxQry.Params.ParamByName('p2').AsString := U1 + '-%';
        AuxQry.Params.ParamByName('p3').AsString := U1 + ':%';
        AuxQry.Params.ParamByName('p4').AsString := '%(' + U1 + ')%';
        AuxQry.Open;
        try
          if not AuxQry.EOF then
            Result := ExtractDecodedRowContent(AuxQry);
        finally
          AuxQry.Close;
        end;
      except
        Result := '';
      end;
    finally
      AuxQry.Free;
    end;
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
    if not HasStrongDictionary(TextIndex) and not HasStrongDictionary(0) then
      Exit;
    for EntryKey in Syntagm.Strong do
    begin
      Def := LookupStrong(EntryKey, TextIndex);
      if (Def = '') and (TextIndex <> 0) then
        Def := LookupStrong(EntryKey, 0);
      if Def <> '' then
        AppendEntry(Entries, EntryKey, Def);
    end;
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
      if Def <> '' then
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

function SanitizeRtfFontsAndBidi(const ARtf: string): string;
const
  CustomGreekFonts: array[0..10] of string = (
    'Gentium', 'GentiumAlt', 'Gentium Basic', 'Gentium Plus',
    'Galatia SIL', 'TITUS Cyberbit Basic', 'SBL Greek',
    'SPIonic', 'LaserGreek', 'Graeca', 'Bwgrkl'
  );
  CustomHebrewFonts: array[0..10] of string = (
    'Cardo', 'Ezra SIL', 'Ezra SIL SR', 'SBL Hebrew',
    'SPTiberian', 'TITUS Cyberbit Basic', 'Bwhebb',
    'Bwhebl', 'BSTHebrew', 'David', 'FrankRuehl'
  );
var
  PStart, PEnd, I, Depth, Len: Integer;
  FontTbl, Prefix, Suffix, FName, GreekFallback, HebrewFallback: string;
begin
  Result := ARtf;
  if Result = '' then
    Exit;

  {$IFDEF WINDOWS}
  GreekFallback := 'Segoe UI';
  HebrewFallback := 'Times New Roman';
  {$ELSE}
  GreekFallback := UIDefaultFontName;
  HebrewFallback := UIDefaultFontName;
  {$ENDIF}

  PStart := Pos('{\fonttbl', Result);
  if PStart > 0 then
  begin
    Len := Length(Result);
    Depth := 0;
    PEnd := PStart;
    while PEnd <= Len do
    begin
      if Result[PEnd] = '{' then
        Inc(Depth)
      else if Result[PEnd] = '}' then
      begin
        Dec(Depth);
        if Depth = 0 then
          Break;
      end;
      Inc(PEnd);
    end;

    if (PEnd <= Len) and (Depth = 0) then
    begin
      Prefix := Copy(Result, 1, PStart - 1);
      FontTbl := Copy(Result, PStart, PEnd - PStart + 1);
      Suffix := Copy(Result, PEnd + 1, Len - PEnd);

      for I := Low(CustomGreekFonts) to High(CustomGreekFonts) do
      begin
        FName := CustomGreekFonts[I];
        if ((not Assigned(Screen)) or (Screen.Fonts.IndexOf(FName) = -1)) and
           (Pos(LowerCase(FName), LowerCase(FontTbl)) > 0) then
        begin
          FontTbl := StringReplace(FontTbl, FName + ';', GreekFallback + ';', [rfReplaceAll, rfIgnoreCase]);
          FontTbl := StringReplace(FontTbl, FName + ' ;', GreekFallback + ';', [rfReplaceAll, rfIgnoreCase]);
        end;
      end;

      // Substitute specialized Hebrew fonts inside fonttbl with Times New Roman (preserving \fcharset177)
      // so Windows RichEdit invokes Uniscribe Hebrew Right-to-Left shaping with proper niqqud placement.
      for I := Low(CustomHebrewFonts) to High(CustomHebrewFonts) do
      begin
        FName := CustomHebrewFonts[I];
        if Pos(LowerCase(FName), LowerCase(FontTbl)) > 0 then
        begin
          FontTbl := StringReplace(FontTbl, FName + ';', HebrewFallback + ';', [rfReplaceAll, rfIgnoreCase]);
          FontTbl := StringReplace(FontTbl, FName + ' ;', HebrewFallback + ';', [rfReplaceAll, rfIgnoreCase]);
        end;
      end;

      Result := Prefix + FontTbl + Suffix;
    end;
  end;

  // Ensure {\rtf1\fbidis is present in the RTF header so Windows RichEdit activates its BiDi layout engine
  if AnsiStartsStr('{\rtf1', Result) and (Pos('\fbidis', Copy(Result, 1, 40)) = 0) then
    Result := '{\rtf1\fbidis' + Copy(Result, 7, MaxInt);
end;

function EscapeRawUTF8InRtf(const ARtf: string): string;
var
  I, Len, SeqLen: Integer;
  B1, B2, B3, B4: Byte;
  CodePoint, HighSurrogate, LowSurrogate: Cardinal;
  HasHighByte, IsUc0: Boolean;
  SB: TStringBuilder;
  SuffixChar: string;
begin
  Result := ARtf;
  if Result = '' then
    Exit;

  Len := Length(ARtf);
  HasHighByte := False;
  for I := 1 to Len do
    if Byte(ARtf[I]) >= $80 then
    begin
      HasHighByte := True;
      Break;
    end;
  if not HasHighByte then
    Exit;

  IsUc0 := Pos('\uc0', LowerCase(Copy(ARtf, 1, 300))) > 0;
  if IsUc0 then
    SuffixChar := ' '
  else
    SuffixChar := '?';

  SB := TStringBuilder.Create(Len + 256);
  try
    I := 1;
    while I <= Len do
    begin
      B1 := Byte(ARtf[I]);
      if B1 < $80 then
      begin
        SB.Append(ARtf[I]);
        Inc(I);
        Continue;
      end;

      CodePoint := 0;
      SeqLen := 0;
      if (B1 in [$C2..$DF]) and (I + 1 <= Len) then
      begin
        B2 := Byte(ARtf[I + 1]);
        if (B2 and $C0) = $80 then
        begin
          CodePoint := ((Cardinal(B1 and $1F) shl 6) or Cardinal(B2 and $3F));
          SeqLen := 2;
        end;
      end
      else if (B1 in [$E0..$EF]) and (I + 2 <= Len) then
      begin
        B2 := Byte(ARtf[I + 1]);
        B3 := Byte(ARtf[I + 2]);
        if ((B2 and $C0) = $80) and ((B3 and $C0) = $80) then
        begin
          CodePoint := ((Cardinal(B1 and $0F) shl 12) or
                        (Cardinal(B2 and $3F) shl 6) or
                        Cardinal(B3 and $3F));
          if CodePoint >= $0800 then
            SeqLen := 3;
        end;
      end
      else if (B1 in [$F0..$F4]) and (I + 3 <= Len) then
      begin
        B2 := Byte(ARtf[I + 1]);
        B3 := Byte(ARtf[I + 2]);
        B4 := Byte(ARtf[I + 3]);
        if ((B2 and $C0) = $80) and ((B3 and $C0) = $80) and ((B4 and $C0) = $80) then
        begin
          CodePoint := ((Cardinal(B1 and $07) shl 18) or
                        (Cardinal(B2 and $3F) shl 12) or
                        (Cardinal(B3 and $3F) shl 6) or
                        Cardinal(B4 and $3F));
          if (CodePoint >= $10000) and (CodePoint <= $10FFFF) then
            SeqLen := 4;
        end;
      end;

      if SeqLen > 0 then
      begin
        if CodePoint <= $FFFF then
          SB.Append('\u').Append(IntToStr(SmallInt(Word(CodePoint)))).Append(SuffixChar)
        else
        begin
          Dec(CodePoint, $10000);
          HighSurrogate := $D800 + ((CodePoint shr 10) and $3FF);
          LowSurrogate := $DC00 + (CodePoint and $3FF);
          SB.Append('\u').Append(IntToStr(SmallInt(Word(HighSurrogate)))).Append(SuffixChar);
          SB.Append('\u').Append(IntToStr(SmallInt(Word(LowSurrogate)))).Append(SuffixChar);
        end;
        Inc(I, SeqLen);
      end
      else
      begin
        SB.Append('\''').Append(LowerCase(IntToHex(B1, 2)));
        Inc(I);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ Unwraps TRichView / uBDB RTF tables (\trowd...\cell...\row) into clean paragraphs so
  Windows RichEdit never crushes autofit cells (\cellx0 / \cellx140) into a 1-char-wide
  vertical box, strips character background shading (\chcbpat/\cb/\highlight), and ensures
  every Hebrew Unicode sequence (\u1424..\u1524) is wrapped with \rtlch\lang1037 so
  Hebrew letters flow Right-to-Left with niqqud vowels centered under their consonant. }
function UnwrapRtfTablesAndFixHebrewRuns(const ARtf: string): string;
var
  SB: TStringBuilder;
  I, Len, CmdStart, NumStart, NumVal, SignVal, Depth: Integer;
  Cmd, LowerCmd, RawToken, LookAhead: string;
  HasNum, IsUc0, InHebrewRun, JustClosedCell: Boolean;

  procedure CloseHebrewRunIfActive;
  begin
    if InHebrewRun then
    begin
      SB.Append('\ltrch\lang1033 ');
      InHebrewRun := False;
    end;
  end;

  function IsTableOrShadingCmd(const ACmd: string): Boolean;
  begin
    Result :=
      (ACmd = 'intbl') or (ACmd = 'itap') or (ACmd = 'irow') or (ACmd = 'irowband') or
      (ACmd = 'lastrow') or (ACmd = 'taprtl') or (ACmd = 'nonesttables') or
      (ACmd = 'cellx') or (ACmd = 'box') or
      (ACmd = 'cb') or (ACmd = 'chcbpat') or (ACmd = 'highlight') or
      (ACmd = 'chshdng') or (ACmd = 'shading') or (ACmd = 'cbpat') or (ACmd = 'cfpat') or
      AnsiStartsStr('tr', ACmd) or
      AnsiStartsStr('cl', ACmd) or
      AnsiStartsStr('brdr', ACmd) or
      AnsiStartsStr('tbllk', ACmd);
  end;

begin
  Result := ARtf;
  if Result = '' then
    Exit;

  Len := Length(ARtf);
  IsUc0 := Pos('\uc0', LowerCase(Copy(ARtf, 1, 300))) > 0;
  InHebrewRun := False;
  JustClosedCell := False;

  SB := TStringBuilder.Create(Len + 256);
  try
    I := 1;
    while I <= Len do
    begin
      if ARtf[I] = '{' then
      begin
        LookAhead := LowerCase(Copy(ARtf, I + 1, 20));
        if (Pos('\fonttbl', LookAhead) = 1) or
           (Pos('\colortbl', LookAhead) = 1) or
           (Pos('\stylesheet', LookAhead) = 1) or
           (Pos('\info', LookAhead) = 1) then
        begin
          CloseHebrewRunIfActive;
          Depth := 1;
          SB.Append(ARtf[I]);
          Inc(I);
          while (I <= Len) and (Depth > 0) do
          begin
            if ARtf[I] = '\' then
            begin
              SB.Append(ARtf[I]);
              Inc(I);
              if I <= Len then
              begin
                SB.Append(ARtf[I]);
                Inc(I);
              end;
              Continue;
            end;
            if ARtf[I] = '{' then
              Inc(Depth)
            else if ARtf[I] = '}' then
              Dec(Depth);
            SB.Append(ARtf[I]);
            Inc(I);
          end;
          Continue;
        end
        else if Pos('\*\nesttableprops', LookAhead) = 1 then
        begin
          CloseHebrewRunIfActive;
          Depth := 1;
          Inc(I);
          while (I <= Len) and (Depth > 0) do
          begin
            if ARtf[I] = '\' then
            begin
              Inc(I, 2);
              Continue;
            end;
            if ARtf[I] = '{' then
              Inc(Depth)
            else if ARtf[I] = '}' then
              Dec(Depth);
            Inc(I);
          end;
          Continue;
        end;

        CloseHebrewRunIfActive;
        SB.Append('{');
        Inc(I);
        Continue;
      end;

      if ARtf[I] = '}' then
      begin
        CloseHebrewRunIfActive;
        SB.Append('}');
        Inc(I);
        Continue;
      end;

      if ARtf[I] = '\' then
      begin
        if (I + 1 <= Len) and not (ARtf[I + 1] in ['a'..'z', 'A'..'Z']) then
        begin
          CloseHebrewRunIfActive;
          SB.Append(ARtf[I]);
          SB.Append(ARtf[I + 1]);
          Inc(I, 2);
          Continue;
        end;

        CmdStart := I;
        Inc(I);
        while (I <= Len) and (ARtf[I] in ['a'..'z', 'A'..'Z']) do
          Inc(I);
        Cmd := Copy(ARtf, CmdStart + 1, I - (CmdStart + 1));
        LowerCmd := LowerCase(Cmd);

        SignVal := 1;
        HasNum := False;
        NumVal := 0;
        NumStart := I;
        if (I <= Len) and (ARtf[I] = '-') then
        begin
          SignVal := -1;
          Inc(I);
        end;
        while (I <= Len) and (ARtf[I] in ['0'..'9']) do
        begin
          HasNum := True;
          NumVal := NumVal * 10 + (Ord(ARtf[I]) - Ord('0'));
          Inc(I);
        end;
        if not HasNum then
          I := NumStart
        else
          NumVal := NumVal * SignVal;

        if (I <= Len) and (ARtf[I] = ' ') then
          Inc(I);

        if LowerCmd = 'u' then
        begin
          if (not IsUc0) and (I <= Len) and (ARtf[I] = '?') then
            Inc(I);
          RawToken := Copy(ARtf, CmdStart, I - CmdStart);

          // Strip directional override characters (U+202C..U+202E = 8236..8238) that force LTR on Hebrew
          if HasNum and ((NumVal = 8236) or (NumVal = 8237) or (NumVal = 8238)) then
            Continue;

          // Hebrew Unicode block: U+0590..U+05F4 (1424..1524) and Alphabetic Presentation Forms U+FB1D..U+FB4F
          if HasNum and (((NumVal >= 1424) and (NumVal <= 1524)) or
                         ((Word(SmallInt(NumVal)) >= $FB1D) and (Word(SmallInt(NumVal)) <= $FB4F))) then
          begin
            if not InHebrewRun then
            begin
              SB.Append('\rtlch\lang1037 ');
              InHebrewRun := True;
            end;
            SB.Append(RawToken);
            JustClosedCell := False;
            Continue;
          end
          else
          begin
            CloseHebrewRunIfActive;
            SB.Append(RawToken);
            JustClosedCell := False;
            Continue;
          end;
        end;

        CloseHebrewRunIfActive;

        if LowerCmd = 'trowd' then
        begin
          SB.Append('\pard\li240\sb40\sa40 ');
          JustClosedCell := False;
        end
        else if (LowerCmd = 'cell') or (LowerCmd = 'nestcell') then
        begin
          SB.Append(' ');
          JustClosedCell := True;
        end
        else if (LowerCmd = 'row') or (LowerCmd = 'nestrow') then
        begin
          SB.Append('\par\pard ');
          JustClosedCell := False;
        end
        else if (LowerCmd = 'pard') and JustClosedCell then
        begin
          // Skip \pard immediately after \cell so multi-column rows stay on the same line
          JustClosedCell := False;
        end
        else if IsTableOrShadingCmd(LowerCmd) then
        begin
          // Strip table border/width/cellx and background shading control words
        end
        else if (LowerCmd = 'ri') and HasNum and (Abs(NumVal) > 360) then
        begin
          // Strip large right-indent values that crush text into a narrow column
        end
        else if (LowerCmd = 'li') and HasNum and (NumVal > 720) then
        begin
          SB.Append('\li360 ');
        end
        else if (LowerCmd = 'fi') and HasNum and (Abs(NumVal) > 720) then
        begin
          SB.Append('\fi-240 ');
        end
        else
        begin
          SB.Append(Copy(ARtf, CmdStart, I - CmdStart));
          if (LowerCmd <> 'plain') and (LowerCmd <> 'lang') and
             (LowerCmd <> 'f') and (LowerCmd <> 'fs') and (LowerCmd <> 'cf') then
            JustClosedCell := False;
        end;
        Continue;
      end;

      if InHebrewRun and (ARtf[I] = ' ') then
      begin
        // Keep single spaces inside a Hebrew phrase in the same RTL run only if followed by another Hebrew \u
        LookAhead := Copy(ARtf, I + 1, 8);
        if Pos('\u14', LookAhead) = 1 then
        begin
          SB.Append(' ');
          Inc(I);
          Continue;
        end
        else if Pos('\u15', LookAhead) = 1 then
        begin
          SB.Append(' ');
          Inc(I);
          Continue;
        end;
      end;

      if not (ARtf[I] in [#10, #13]) then
      begin
        CloseHebrewRunIfActive;
        JustClosedCell := False;
      end;
      SB.Append(ARtf[I]);
      Inc(I);
    end;
    CloseHebrewRunIfActive;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function TDictionaryManager.FormatRTF(const RTF: string): string;
var
  DefFont, HebFont: string;
begin
  Result := '';
  if RTF.IsEmpty then
    Exit;

  {$IFDEF WINDOWS}
  DefFont := 'Segoe UI';
  HebFont := 'Times New Roman';
  {$ELSE}
  DefFont := UIDefaultFontName;
  HebFont := UIDefaultFontName;
  {$ENDIF}

  if AnsiStartsStr('{\rtf1', RTF) then
    Result := UnwrapRtfTablesAndFixHebrewRuns(EscapeRawUTF8InRtf(SanitizeRtfFontsAndBidi(RTF)))
  else
    Result := UnwrapRtfTablesAndFixHebrewRuns(EscapeRawUTF8InRtf(SanitizeRtfFontsAndBidi(
      '{\rtf1\fbidis\ansi\ansicpg1252\deff0\deflang1033' +
      '{\fonttbl{\f0\fnil\fcharset0 ' + DefFont + ';}' +
      '{\f1\fnil\fcharset161 ' + DefFont + ';}' +
      '{\f2\fnil\fcharset177 ' + HebFont + ';}}' +
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
      '\viewkind4\uc1\pard\ltrpar\lang1033\f0\fs20 ' + RTF + '}')));
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
        'select distinct * from config where name in (''strong'',''strong.h'',''strong.g'',''lemma'',''lemma.h'',''lemma.g'',''morph'',''secure'',''do.encrypt'',''content.type'')';
      ConfigQuery.Prepare;
      ConfigQuery.Open;
      while not ConfigQuery.EOF do
      begin
        case ConfigQuery.FieldByName('name').AsString of
          'strong', 'strong.h', 'strong.g', 'lemma', 'lemma.h', 'lemma.g':
            if ConfigQuery.FieldByName('value').AsString = '1' then
              HasStrongSupport := True;
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

  if (ModuleType = dmtStrong) and (not HasStrongSupport) and (not HasTopicsAndContent) then
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
      'select data, data2 from topics, content where subject = :strong and id = topic_id';
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
      'select data, data2 from topics, content where subject = :morf and id = topic_id';
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
  CleanPart, DigitsOnly, Suffix, Data: string;
  IsHebrew: Boolean;
  NumVal: Integer;
begin
  Result := '';
  if Strong = '' then
    Exit;
  CleanPart := NormalizeStrongToken(Strong, IsHebrew, NumVal, DigitsOnly, Suffix);
  if CleanPart = '' then
    Exit;
  Keys := StrongKeyCandidates(Strong);
  Data := '';
  if GUseDefaultDictionaries then
  begin
    if IsHebrew then
    begin
      Data := QueryDefinition(GDefaultStrongHebrew, 'strong', Keys, True, True, NumVal);
      if (Data = '') and (not Assigned(GDefaultStrongHebrew)) and Assigned(GDefaultStrongGreek) then
        Data := QueryDefinition(GDefaultStrongGreek, 'strong', Keys, True, True, NumVal);
    end
    else
    begin
      Data := QueryDefinition(GDefaultStrongGreek, 'strong', Keys, True, False, NumVal);
      if (Data = '') and (not Assigned(GDefaultStrongGreek)) and Assigned(GDefaultStrongHebrew) then
        Data := QueryDefinition(GDefaultStrongHebrew, 'strong', Keys, True, False, NumVal);
    end;
    if Data = '' then
      Data := QueryDefinition(GetStrongDictionary(TextIndex), 'strong', Keys, True, IsHebrew, NumVal);
    if (Data = '') and (TextIndex <> 0) then
      Data := QueryDefinition(GetStrongDictionary(0), 'strong', Keys, True, IsHebrew, NumVal);
  end
  else
  begin
    Data := QueryDefinition(GetStrongDictionary(TextIndex), 'strong', Keys, True, IsHebrew, NumVal);
    if (Data = '') and (TextIndex <> 0) then
      Data := QueryDefinition(GetStrongDictionary(0), 'strong', Keys, True, IsHebrew, NumVal);
  end;
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
  if (TextIndex = 1) and (Length(StrongEntries) = 0) and (Length(MorphologyEntries) = 0) and
     Assigned(Syntagm) and Assigned(Syntagm.Pairs) and (Syntagm.Pairs.Count > 0) then
  begin
    CollectPopupEntries(Syntagm, 0, StrongEntries, MorphologyEntries);
    if (Length(StrongEntries) = 0) and (Length(MorphologyEntries) = 0) then
      CollectPopupEntries(TSyntagm(Syntagm.Pairs[0]), 0, StrongEntries, MorphologyEntries);
  end;
end;

finalization
  TDictionaryManager.ReleaseQuery(GDefaultStrongGreek);
  TDictionaryManager.ReleaseQuery(GDefaultStrongHebrew);
  TDictionaryManager.ReleaseQuery(GDefaultMorphology);
  TDictionaryManager.ReleaseQuery(GDefaultWord);

end.
