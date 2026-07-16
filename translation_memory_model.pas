unit translation_memory_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Verse, SuggestionManager;

type
  TTranslationMemoryOrder = (
    tmoVerseOccurrence,
    tmoMemoryFrequencyAscending
  );

  TTranslationMemoryStringArray = array of string;
  TTranslationMemoryIntegerArray = array of Integer;

  TTranslationMemoryAlternative = record
    Translation: string;
    Occurrences: Integer;
    IsCurrent: Boolean;
    Matches: TTranslationPairMatchArray;
  end;

  TTranslationMemoryAlternativeArray = array of TTranslationMemoryAlternative;

  TTranslationMemoryEntry = record
    DisplayTerm: string;
    Lemmas: TTranslationMemoryStringArray;
    Forms: TTranslationMemoryStringArray;
    Strongs: TTranslationMemoryStringArray;
    SyntagmIndexes: TTranslationMemoryIntegerArray;
    Alternatives: TTranslationMemoryAlternativeArray;
    HistoricalOccurrences: Integer;
  end;

  TTranslationMemoryEntryArray = array of TTranslationMemoryEntry;

  TTranslationMemoryModel = class
  public
    class function Build(SourceVerse: TVerse;
      SuggestionManager: TSuggestionManager;
      Order: TTranslationMemoryOrder = tmoVerseOccurrence):
      TTranslationMemoryEntryArray; static;
  end;

function TranslationMemoryOrderFromInteger(
  Value: Integer): TTranslationMemoryOrder;

implementation

uses
  LazUTF8, ONTTokenizer, Syntagm;

type
  TTranslationAlternativeCacheItem = class
  public
    Data: TTranslationAlternativeDataArray;
  end;

function TranslationMemoryOrderFromInteger(
  Value: Integer): TTranslationMemoryOrder;
begin
  if (Value < Ord(Low(TTranslationMemoryOrder))) or
     (Value > Ord(High(TTranslationMemoryOrder))) then
    Exit(tmoVerseOccurrence);
  Result := TTranslationMemoryOrder(Value);
end;

function NormalizedKey(const Value: string): string;
begin
  Result := UTF8LowerCase(Trim(Value));
end;

function NormalizeTranslationDisplay(const Value: string): string;
begin
  Result := Trim(StringReplace(Value, ';', ' ', [rfReplaceAll]));
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

function JoinValues(const Values: TStrings; const Separator: string): string;
var
  ValueIndex: Integer;
begin
  Result := '';
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + Separator;
    Result := Result + Values[ValueIndex];
  end;
end;

function JoinArray(const Values: TTranslationMemoryStringArray;
  const Separator: string): string;
var
  Value: string;
begin
  Result := '';
  for Value in Values do
  begin
    if Result <> '' then
      Result := Result + Separator;
    Result := Result + Value;
  end;
end;

procedure AddUniqueString(var Values: TTranslationMemoryStringArray;
  const Value: string);
var
  ExistingValue: string;
  ValueIndex: Integer;
begin
  if Value = '' then
    Exit;
  for ExistingValue in Values do
    if NormalizedKey(ExistingValue) = NormalizedKey(Value) then
      Exit;
  ValueIndex := Length(Values);
  SetLength(Values, ValueIndex + 1);
  Values[ValueIndex] := Value;
end;

procedure AddInteger(var Values: TTranslationMemoryIntegerArray;
  const Value: Integer);
var
  ValueIndex: Integer;
begin
  ValueIndex := Length(Values);
  SetLength(Values, ValueIndex + 1);
  Values[ValueIndex] := Value;
end;

procedure AddDelimitedValues(var Values: TTranslationMemoryStringArray;
  const DelimitedValue: string);
var
  Items: TStringList;
  Item: string;
begin
  if DelimitedValue = '' then
    Exit;
  Items := TStringList.Create;
  try
    Items.Delimiter := ';';
    Items.StrictDelimiter := True;
    Items.DelimitedText := DelimitedValue;
    for Item in Items do
      AddUniqueString(Values, Trim(Item));
  finally
    Items.Free;
  end;
end;

function StrongCombination(Syntagm: TSyntagm): string;
begin
  if not Assigned(Syntagm.Strong) then
    Exit('');
  Result := JoinValues(Syntagm.Strong, ';');
end;

function GroupKey(Syntagm: TSyntagm): string;
var
  LemmaValue: string;
  StrongValue: string;
begin
  LemmaValue := Syntagm.Lemma;
  if LemmaValue <> '' then
    Exit('L:' + NormalizedKey(LemmaValue));

  StrongValue := StrongCombination(Syntagm);
  if StrongValue <> '' then
    Exit('S:' + NormalizedKey(StrongValue));

  Result := 'T:' + NormalizedKey(Syntagm.Text);
end;

function FindEntry(const Keys: TStrings; const Key: string): Integer;
begin
  Result := Keys.IndexOf(Key);
end;

procedure AddHistoricalAlternative(var Entry: TTranslationMemoryEntry;
  const Alternative: TTranslationAlternativeData);
var
  AlternativeIndex: Integer;
  DisplayTranslation: string;
  ExistingMatchIndex: Integer;
  Match: TTranslationPairMatch;
  MatchIndex: Integer;
begin
  DisplayTranslation := NormalizeTranslationDisplay(Alternative.Translation);
  if DisplayTranslation = '' then
    Exit;
  AlternativeIndex := -1;
  for MatchIndex := 0 to Length(Entry.Alternatives) - 1 do
    if NormalizedKey(Entry.Alternatives[MatchIndex].Translation) =
       NormalizedKey(DisplayTranslation) then
    begin
      AlternativeIndex := MatchIndex;
      Break;
    end;

  if AlternativeIndex < 0 then
  begin
    AlternativeIndex := Length(Entry.Alternatives);
    SetLength(Entry.Alternatives, AlternativeIndex + 1);
    Entry.Alternatives[AlternativeIndex].Translation := DisplayTranslation;
    Entry.Alternatives[AlternativeIndex].Occurrences := 0;
    Entry.Alternatives[AlternativeIndex].IsCurrent := False;
  end;

  if Length(Alternative.Matches) = 0 then
  begin
    Inc(Entry.Alternatives[AlternativeIndex].Occurrences,
      Alternative.Occurrences);
    Exit;
  end;

  for Match in Alternative.Matches do
  begin
    ExistingMatchIndex := -1;
    for MatchIndex := 0 to
      Length(Entry.Alternatives[AlternativeIndex].Matches) - 1 do
      if (Entry.Alternatives[AlternativeIndex].Matches[MatchIndex].SourcePhrase =
          Match.SourcePhrase) and
         (Entry.Alternatives[AlternativeIndex].Matches[MatchIndex].TargetPhrase =
          Match.TargetPhrase) then
      begin
        ExistingMatchIndex := MatchIndex;
        Break;
      end;
    if ExistingMatchIndex >= 0 then
      Continue;
    MatchIndex := Length(Entry.Alternatives[AlternativeIndex].Matches);
    SetLength(Entry.Alternatives[AlternativeIndex].Matches, MatchIndex + 1);
    Entry.Alternatives[AlternativeIndex].Matches[MatchIndex] := Match;
    Inc(Entry.Alternatives[AlternativeIndex].Occurrences,
      Match.Occurrences);
  end;
end;

function CurrentTranslation(Syntagm: TSyntagm): string;
var
  PairedSyntagm: TSyntagm;
begin
  Result := '';
  if not Assigned(Syntagm.Pairs) then
    Exit;
  for PairedSyntagm in Syntagm.Pairs do
  begin
    if (PairedSyntagm.Kind <> tsSintagma) or PairedSyntagm.IsItalic then
      Continue;
    if Result <> '' then
      Result := Result + ';';
    Result := Result + PairedSyntagm.GetSuggestionKey(plText);
  end;
  Result := NormalizeTranslationDisplay(Result);
end;

procedure MarkCurrentAlternative(var Entry: TTranslationMemoryEntry;
  const Translation: string);
var
  AlternativeIndex: Integer;
begin
  if Translation = '' then
    Exit;
  for AlternativeIndex := 0 to Length(Entry.Alternatives) - 1 do
    if NormalizedKey(Entry.Alternatives[AlternativeIndex].Translation) =
       NormalizedKey(Translation) then
    begin
      Entry.Alternatives[AlternativeIndex].IsCurrent := True;
      Exit;
    end;

  AlternativeIndex := Length(Entry.Alternatives);
  SetLength(Entry.Alternatives, AlternativeIndex + 1);
  Entry.Alternatives[AlternativeIndex].Translation := Translation;
  Entry.Alternatives[AlternativeIndex].Occurrences := 0;
  Entry.Alternatives[AlternativeIndex].IsCurrent := True;
end;

function AlternativeComesBefore(const LeftValue,
  RightValue: TTranslationMemoryAlternative): Boolean;
var
  LeftText: string;
  RightText: string;
begin
  if LeftValue.Occurrences <> RightValue.Occurrences then
    Exit(LeftValue.Occurrences > RightValue.Occurrences);
  LeftText := NormalizedKey(LeftValue.Translation);
  RightText := NormalizedKey(RightValue.Translation);
  Result := CompareStr(LeftText, RightText) < 0;
end;

procedure SortAlternatives(var Alternatives: TTranslationMemoryAlternativeArray);
var
  LeftIndex: Integer;
  RightIndex: Integer;
  Temporary: TTranslationMemoryAlternative;
begin
  for LeftIndex := 0 to Length(Alternatives) - 2 do
    for RightIndex := LeftIndex + 1 to Length(Alternatives) - 1 do
      if AlternativeComesBefore(Alternatives[RightIndex],
        Alternatives[LeftIndex]) then
      begin
        Temporary := Alternatives[LeftIndex];
        Alternatives[LeftIndex] := Alternatives[RightIndex];
        Alternatives[RightIndex] := Temporary;
      end;
end;

function FirstSyntagmIndex(const Entry: TTranslationMemoryEntry): Integer;
begin
  if Length(Entry.SyntagmIndexes) = 0 then
    Exit(MaxInt);
  Result := Entry.SyntagmIndexes[0];
end;

function MemoryEntryComesBefore(const LeftValue,
  RightValue: TTranslationMemoryEntry): Boolean;
begin
  if LeftValue.HistoricalOccurrences <> RightValue.HistoricalOccurrences then
    Exit(LeftValue.HistoricalOccurrences < RightValue.HistoricalOccurrences);
  Result := FirstSyntagmIndex(LeftValue) < FirstSyntagmIndex(RightValue);
end;

procedure SortEntriesByHistoricalFrequency(
  var Entries: TTranslationMemoryEntryArray);
var
  LeftIndex: Integer;
  RightIndex: Integer;
  Temporary: TTranslationMemoryEntry;
begin
  for LeftIndex := 0 to Length(Entries) - 2 do
    for RightIndex := LeftIndex + 1 to Length(Entries) - 1 do
      if MemoryEntryComesBefore(Entries[RightIndex], Entries[LeftIndex]) then
      begin
        Temporary := Entries[LeftIndex];
        Entries[LeftIndex] := Entries[RightIndex];
        Entries[RightIndex] := Temporary;
      end;
end;

function GetCachedAlternatives(const Key: string;
  SuggestionManager: TSuggestionManager; Cache: TStringList):
  TTranslationAlternativeDataArray;
var
  CacheIndex: Integer;
  CacheItem: TTranslationAlternativeCacheItem;
begin
  CacheIndex := Cache.IndexOf(Key);
  if CacheIndex >= 0 then
    Exit(TTranslationAlternativeCacheItem(Cache.Objects[CacheIndex]).Data);

  CacheItem := TTranslationAlternativeCacheItem.Create;
  try
    CacheItem.Data := SuggestionManager.GetTranslationAlternativeData(Key);
    Cache.AddObject(Key, CacheItem);
    Result := CacheItem.Data;
  except
    CacheItem.Free;
    raise;
  end;
end;

procedure FreeCache(Cache: TStringList);
var
  CacheIndex: Integer;
begin
  for CacheIndex := 0 to Cache.Count - 1 do
    Cache.Objects[CacheIndex].Free;
  Cache.Free;
end;

class function TTranslationMemoryModel.Build(SourceVerse: TVerse;
  SuggestionManager: TSuggestionManager; Order: TTranslationMemoryOrder):
  TTranslationMemoryEntryArray;
var
  EntryKeys: TStringList;
  AlternativeCache: TStringList;
  Syntagm: TSyntagm;
  SyntagmIndex: Integer;
  EntryIndex: Integer;
  Key: string;
  LookupKey: string;
  LemmaValue: string;
  Alternative: TTranslationAlternativeData;
  CachedAlternatives: TTranslationAlternativeDataArray;
  CurrentValue: string;
  EntryLookupKeys: TStringList;
  AllLookupKeys: TStringList;
  AlternativeIndex: Integer;
begin
  SetLength(Result, 0);
  if (SourceVerse = nil) or (SuggestionManager = nil) or
     not Assigned(SourceVerse.Syntagms) then
    Exit;

  EntryKeys := TStringList.Create;
  AlternativeCache := TStringList.Create;
  AllLookupKeys := TStringList.Create;
  try
    EntryKeys.CaseSensitive := True;
    AlternativeCache.CaseSensitive := True;
    AllLookupKeys.Sorted := True;
    AllLookupKeys.CaseSensitive := True;
    AllLookupKeys.UseLocale := False;
    AllLookupKeys.Duplicates := dupIgnore;
    for SyntagmIndex := 0 to SourceVerse.Syntagms.Count - 1 do
    begin
      Syntagm := SourceVerse.Syntagms[SyntagmIndex];
      if (Syntagm.Kind <> tsSintagma) or (Trim(Syntagm.Text) = '') then
        Continue;

      Key := GroupKey(Syntagm);
      EntryIndex := FindEntry(EntryKeys, Key);
      if EntryIndex < 0 then
      begin
        EntryIndex := Length(Result);
        SetLength(Result, EntryIndex + 1);
        EntryKeys.Add(Key);
      end;

      LemmaValue := Syntagm.Lemma;
      AddDelimitedValues(Result[EntryIndex].Lemmas, LemmaValue);
      AddUniqueString(Result[EntryIndex].Forms, Syntagm.Text);
      if Assigned(Syntagm.Strong) then
        for LookupKey in Syntagm.Strong do
          AddUniqueString(Result[EntryIndex].Strongs, LookupKey);
      AddInteger(Result[EntryIndex].SyntagmIndexes, SyntagmIndex);

      LookupKey := Syntagm.GetSuggestionKey(plStrong);
      if LookupKey = '' then
        LookupKey := Syntagm.GetSuggestionKey(plText);
      LookupKey := NormalizedKey(LookupKey);
      if LookupKey <> '' then
        AllLookupKeys.Add(LookupKey);
    end;

    SuggestionManager.PreloadTranslationAlternativeData(AllLookupKeys);

    for EntryIndex := 0 to Length(Result) - 1 do
    begin
      if Length(Result[EntryIndex].Lemmas) > 0 then
        Result[EntryIndex].DisplayTerm :=
          JoinArray(Result[EntryIndex].Lemmas, ' + ')
      else if Length(Result[EntryIndex].Forms) > 0 then
        Result[EntryIndex].DisplayTerm := Result[EntryIndex].Forms[0];

      EntryLookupKeys := TStringList.Create;
      try
        EntryLookupKeys.CaseSensitive := True;
        for SyntagmIndex in Result[EntryIndex].SyntagmIndexes do
        begin
          Syntagm := SourceVerse.Syntagms[SyntagmIndex];
          LookupKey := Syntagm.GetSuggestionKey(plStrong);
          if LookupKey = '' then
            LookupKey := Syntagm.GetSuggestionKey(plText);
          LookupKey := NormalizedKey(LookupKey);
          if EntryLookupKeys.IndexOf(LookupKey) < 0 then
          begin
            EntryLookupKeys.Add(LookupKey);
            CachedAlternatives := GetCachedAlternatives(LookupKey,
              SuggestionManager, AlternativeCache);
            for Alternative in CachedAlternatives do
              AddHistoricalAlternative(Result[EntryIndex], Alternative);
          end;

          CurrentValue := CurrentTranslation(Syntagm);
          MarkCurrentAlternative(Result[EntryIndex], CurrentValue);
        end;
      finally
        EntryLookupKeys.Free;
      end;
      SortAlternatives(Result[EntryIndex].Alternatives);
      Result[EntryIndex].HistoricalOccurrences := 0;
      for AlternativeIndex := 0 to
        Length(Result[EntryIndex].Alternatives) - 1 do
        Inc(Result[EntryIndex].HistoricalOccurrences,
          Result[EntryIndex].Alternatives[AlternativeIndex].Occurrences);
    end;
    if Order = tmoMemoryFrequencyAscending then
      SortEntriesByHistoricalFrequency(Result);
  finally
    AllLookupKeys.Free;
    EntryKeys.Free;
    FreeCache(AlternativeCache);
  end;
end;

end.
