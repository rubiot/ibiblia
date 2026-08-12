unit SuggestionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sqldb, sqlite3conn, Verse,
  Dialogs, LazLogger, PCRE, Syntagm, ONTTokenizer, ConcordanceExport;

type

  TTranslationPairMatch = record
    SourcePhrase: string;
    TargetPhrase: string;
    Occurrences: Integer;
  end;

  TTranslationPairMatchArray = array of TTranslationPairMatch;

  TTranslationAlternativeData = record
    Translation: string;
    Occurrences: Integer;
    Matches: TTranslationPairMatchArray;
  end;

  TTranslationAlternativeDataArray = array of TTranslationAlternativeData;

  TTranslationOccurrenceData = record
    ReferenceID: string;
    DisplayReference: string;
    DestinationText: string;
    Occurrences: Integer;
  end;

  TTranslationOccurrenceDataArray = array of TTranslationOccurrenceData;

  TTranslationOccurrencePage = record
    TotalAssociations: Integer;
    TotalVerses: Integer;
    HasMore: Boolean;
    Items: TTranslationOccurrenceDataArray;
  end;

  TConcordanceOccurrenceData = record
    ReferenceID: string;
    SourcePhrase: string;
    TargetPhrase: string;
    DestinationText: string;
    Occurrences: Integer;
    ReferenceOrder: Integer;
  end;

  TConcordanceOccurrenceEvent = procedure(
    const Data: TConcordanceOccurrenceData) of object;

  TAssociationMemoryRebuildPhase = (
    amrpWriting,
    amrpValidating,
    amrpFinalizing
  );

  TAssociationMemoryRebuildProgressEvent = procedure(
    Phase: TAssociationMemoryRebuildPhase; Current, Total: Integer) of object;

  TSuggestionManager = class
  private
    FConnection: TSQLConnection;
    FTemporaryConnection: TSQLite3Connection;
    FVerbConnection: TSQLite3Connection;
    FVerbFormQuery: TSQLQuery;
    FQuery: TSQLQuery;
    FRemovePairQuery: TSQLQuery;
    FGetSourcePhraseQuery: TSQLQuery;
    FGetTargetPhraseQuery: TSQLQuery;
    FGetTranslationCountQuery: TSQLQuery;
    FGetNextSourcePhraseIdQuery: TSQLQuery;
    FGetNextTargetPhraseIdQuery: TSQLQuery;
    FInsertSourcePhraseQuery: TSQLQuery;
    FInsertTargetPhraseQuery: TSQLQuery;
    FInsertSourcePhraseSyntagmQuery: TSQLQuery;
    FInsertTargetPhraseSyntagmQuery: TSQLQuery;
    FInsertTranslationQuery: TSQLQuery;
    FInsertTranslationCountQuery: TSQLQuery;
    FUpdateTranslationQuery: TSQLQuery;
    FSuggestionsQuery: TSQLQuery;
    FSuggestionsWithoutMorphologyQuery: TSQLQuery;
    FTranslationMemoryQuery: TSQLQuery;
    FDelimitedValues: TStringList;
    FVerbList: TStringList;
    FRecognizeVerbForms: Boolean;
    FRemoveMorphologyRegex: IRegex;
    FRebuildSavepointActive: Boolean;
    FBulkRebuildActive: Boolean;
    FBulkTranslationsFlushed: Boolean;
    FBulkSourcePhraseIds: TStringList;
    FBulkTargetPhraseIds: TStringList;
    FBulkTranslationCounts: TStringList;
    FBulkOccurrenceCounts: TStringList;
    FTranslationAlternativeCache: TStringList;
    FNextBulkSourcePhraseId: Integer;
    FNextBulkTargetPhraseId: Integer;
    FOnRebuildProgress: TAssociationMemoryRebuildProgressEvent;
    function GetOrCreateSourcePhraseId(const Phrase: string): Integer;
    function FindSourcePhraseId(const Phrase: string): Integer;
    function InsertSourcePhraseId(const Phrase: string): Integer;
    function GetOrCreateTargetPhraseId(const Phrase: string): Integer;
    function FindTargetPhraseId(const Phrase: string): Integer;
    function InsertTargetPhraseId(const Phrase: string): Integer;
    function UpdateTranslation(SourcePhraseId, TargetPhraseId: Integer): Boolean;
    function InsertTranslation(SourcePhraseId, TargetPhraseId: Integer): Boolean;
    function IdentifyVerb(const Form: string): Boolean;
    function NormalizeVerb(const Phrase: string): string;
    function GetSuggestions(const Key: string): TSQLQuery;
    function GetTranslationMemoryQuery(const Key: string): TSQLQuery;
    procedure CleanupOrphanPhrases(SourcePhraseId, TargetPhraseId: Integer);
    procedure InsertSourcePhraseWithId(const Phrase: string; PhraseId: Integer);
    procedure InsertTargetPhraseWithId(const Phrase: string; PhraseId: Integer);
    function GetOrCreateBulkSourcePhraseId(const Phrase: string): Integer;
    function GetOrCreateBulkTargetPhraseId(const Phrase: string): Integer;
    procedure AddBulkPair(const Source, Target: string);
    procedure AddBulkOccurrence(const ReferenceID, Source, Target: string);
    procedure FlushBulkTranslations;
    procedure StartBulkRebuild;
    procedure ResetBulkRebuild;
    procedure ClearTranslationAlternativeCache;
    procedure EnsureOccurrenceSchema;
    procedure AddOccurrence(const ReferenceID, Source, Target: string);
    function RemoveOccurrence(const ReferenceID, Source,
      Target: string): Boolean;
    procedure ReportRebuildProgress(Phase: TAssociationMemoryRebuildPhase;
      Current, Total: Integer);
  public
    constructor Create(Owner: TComponent; Connection: TSQLConnection); overload;
    constructor Create(const DatabasePath: string); overload;
    destructor Destroy; override;
    property RecognizeVerbForms: Boolean read FRecognizeVerbForms write FRecognizeVerbForms;
    procedure AddPair(const Source, Target: string);
    procedure AddPairForReference(const ReferenceID, Source, Target: string);
    procedure RemovePair(const Source, Target: string);
    procedure RemovePairForReference(const ReferenceID, Source, Target: string);
    procedure SuggestAssociations(SourceVerse: TVerse);
    procedure ClearAssociationDatabase;
    procedure BeginRebuild;
    procedure PrepareRebuildValidation;
    procedure CommitRebuild;
    procedure RollbackRebuild;
    procedure Commit;
    procedure Rollback;
    function Validate(out ErrorMessage: string): Boolean;
    function ValidatePreparedRebuild(out ErrorMessage: string): Boolean;
    function ValidatePairs(ExpectedPairs: TStrings;
      out ErrorMessage: string): Boolean;
    function GetPairOccurrenceCount(const Source, Target: string): Integer;
    function ValidatePairConsistency(const Source, Target: string;
      ExpectedCount: Integer; out ErrorMessage: string): Boolean;
    function GetTranslationAlternativeData(
      const Key: string): TTranslationAlternativeDataArray;
    procedure PreloadTranslationAlternativeData(Keys: TStrings);
    function GetTranslationOccurrences(
      const Matches: TTranslationPairMatchArray;
      Limit: Integer): TTranslationOccurrencePage;
    function IndexedConcordanceOccurrenceCount: Integer;
    function IndexedConcordanceHasTaggedTargets: Boolean;
    procedure EnumerateIndexedConcordanceOccurrences(
      Handler: TConcordanceOccurrenceEvent);
    function GetUnindexedAssociatedReferences: TStringList;
    function GetTranslationAlternatives(Syntagm: TSyntagm): string;
    property OnRebuildProgress: TAssociationMemoryRebuildProgressEvent
      read FOnRebuildProgress write FOnRebuildProgress;
  end;

implementation

const
  TRANSLATION_ALTERNATIVE_CACHE_LIMIT = 512;

type
  TTranslationAlternativeCacheItem = class
  public
    Data: TTranslationAlternativeDataArray;
  end;

procedure AddTranslationPairMatch(var Matches: TTranslationPairMatchArray;
  const SourcePhrase, TargetPhrase: string; Occurrences: Integer);
var
  MatchIndex: Integer;
begin
  for MatchIndex := 0 to Length(Matches) - 1 do
    if (Matches[MatchIndex].SourcePhrase = SourcePhrase) and
       (Matches[MatchIndex].TargetPhrase = TargetPhrase) then
    begin
      Matches[MatchIndex].Occurrences := Occurrences;
      Exit;
    end;
  MatchIndex := Length(Matches);
  SetLength(Matches, MatchIndex + 1);
  Matches[MatchIndex].SourcePhrase := SourcePhrase;
  Matches[MatchIndex].TargetPhrase := TargetPhrase;
  Matches[MatchIndex].Occurrences := Occurrences;
end;

procedure AddTranslationAlternative(
  var Alternatives: TTranslationAlternativeDataArray;
  const SourcePhrase, Translation: string; Occurrences: Integer);
var
  AlternativeIndex: Integer;
begin
  for AlternativeIndex := 0 to Length(Alternatives) - 1 do
    if Alternatives[AlternativeIndex].Translation = Translation then
    begin
      Inc(Alternatives[AlternativeIndex].Occurrences, Occurrences);
      AddTranslationPairMatch(Alternatives[AlternativeIndex].Matches,
        SourcePhrase, Translation, Occurrences);
      Exit;
    end;
  AlternativeIndex := Length(Alternatives);
  SetLength(Alternatives, AlternativeIndex + 1);
  Alternatives[AlternativeIndex].Translation := Translation;
  Alternatives[AlternativeIndex].Occurrences := Occurrences;
  AddTranslationPairMatch(Alternatives[AlternativeIndex].Matches,
    SourcePhrase, Translation, Occurrences);
end;

procedure SortTranslationAlternatives(
  var Alternatives: TTranslationAlternativeDataArray);
var
  LeftIndex: Integer;
  RightIndex: Integer;
  Temporary: TTranslationAlternativeData;
begin
  for LeftIndex := 0 to Length(Alternatives) - 2 do
    for RightIndex := LeftIndex + 1 to Length(Alternatives) - 1 do
      if (Alternatives[RightIndex].Occurrences >
          Alternatives[LeftIndex].Occurrences) or
         ((Alternatives[RightIndex].Occurrences =
           Alternatives[LeftIndex].Occurrences) and
          (CompareStr(Alternatives[RightIndex].Translation,
           Alternatives[LeftIndex].Translation) < 0)) then
      begin
        Temporary := Alternatives[LeftIndex];
        Alternatives[LeftIndex] := Alternatives[RightIndex];
        Alternatives[RightIndex] := Temporary;
      end;
end;

function SuggestionKeyMatches(const LookupKey, StoredKey: string): Boolean;
begin
  Result := StoredKey = LookupKey;
  if Result or (LookupKey = '') or
     (Length(StoredKey) <= Length(LookupKey)) then
    Exit;

  Result :=
    (StoredKey[Length(LookupKey) + 1] = '<') and
    (Copy(StoredKey, 1, Length(LookupKey)) = LookupKey);
end;

resourcestring
  SCannotAddPairsAfterRebuildValidation =
    'Cannot add pairs after rebuild validation';
  SInvalidBufferedAssociationMemoryPair =
    'Invalid buffered association memory pair';
  SAssociationMemoryRebuildAlreadyActive =
    'Association memory rebuild is already active';
  SAssociationMemoryNonPositiveCounts =
    'Association memory contains non-positive occurrence counts';
  SAssociationMemoryMissingPhrases =
    'Association memory contains translations with missing phrases';
  SAssociationMemoryOrphanPhrases =
    'Association memory contains orphan phrases';
  SAssociationMemoryOrphanSyntagmMappings =
    'Association memory contains orphan syntagm mappings';
  SAssociationMemoryNonPositiveOccurrenceReferences =
    'Association memory contains non-positive reference occurrence counts';
  SAssociationMemoryOrphanOccurrenceReferences =
    'Association memory contains occurrence references not found in the project';
  SAssociationMemoryOccurrenceMismatch =
    'Association memory reference occurrences do not match translation counts';
  SAssociationMemoryUnexpectedOccurrenceFmt =
    'Association memory contains an unexpected occurrence at %s: "%s" -> "%s"';
  SAssociationMemoryOccurrenceCountMismatchFmt =
    'Association memory occurrence mismatch at %s for "%s" -> "%s" ' +
    '(expected %d, stored %d)';
  SAssociationMemoryMissingOccurrencesFmt =
    'Association memory is missing reference occurrences ' +
    '(expected %d, stored %d)';
  SAssociationMemoryRebuildNotPrepared =
    'Association memory rebuild has not been prepared';
  SAssociationMemoryUnexpectedPhraseIdsFmt =
    'Association memory contains unexpected phrase IDs %d -> %d';
  SAssociationMemoryPhraseIdMismatchFmt =
    'Association memory mismatch for phrase IDs %d -> %d ' +
    '(expected %d, stored %d)';
  SAssociationMemoryMissingTranslationPairsFmt =
    'Association memory is missing translation pairs ' +
    '(expected %d, stored %d)';
  SExpectedAssociationPairListInvalid =
    'Expected association pair list is invalid';
  SAssociationMemoryUnexpectedPairFmt =
    'Association memory contains unexpected pair "%s" -> "%s"';
  SAssociationMemoryPairMismatchFmt =
    'Association memory mismatch for "%s" -> "%s" ' +
    '(expected %d, stored %d)';
  SAssociationMemoryMissingExpectedPairs =
    'Association memory is missing expected translation pairs';

procedure TSuggestionManager.EnsureOccurrenceSchema;
begin
  FConnection.ExecuteDirect(
    'create table if not exists association_memory_occurrences (' +
    'amo_reference text not null, amo_source text not null, ' +
    'amo_target text not null, amo_occurrences integer not null, ' +
    'primary key (amo_reference, amo_source, amo_target))');
  FConnection.ExecuteDirect(
    'create index if not exists association_memory_occurrence_pair_idx on ' +
    'association_memory_occurrences ' +
    '(amo_source, amo_target, amo_reference)');
  FConnection.ExecuteDirect(
    'create temp table if not exists association_memory_match_filter (' +
    'amo_source text not null, amo_target text not null, ' +
    'primary key (amo_source, amo_target))');
end;

constructor TSuggestionManager.Create(Owner: TComponent; Connection: TSQLConnection);
begin
  FConnection := Connection;
  FQuery := TSQLQuery.Create(Owner);
  FQuery.DataBase := FConnection;
  EnsureOccurrenceSchema;
  // Schema migration happens on a freshly opened suggestion connection.
  // Publish it immediately so this connection does not retain a write lock
  // while the canonical project connection records its source revision.
  if Assigned(FConnection.Transaction) and
    FConnection.Transaction.Active then
  begin
    FConnection.Transaction.Commit;
    FConnection.StartTransaction;
  end;

  FRemovePairQuery := TSQLQuery.Create(Owner);
  FRemovePairQuery.DataBase := FConnection;
  FRemovePairQuery.SQL.Text := 'update traducoes set trad_ocorrencias = trad_ocorrencias - 1 ' +
    'where trad_lori_id = :lori and trad_ldes_id = :ldes and trad_ocorrencias > 1';
  FRemovePairQuery.Prepare;

  FGetSourcePhraseQuery := TSQLQuery.Create(Owner);
  FGetSourcePhraseQuery.DataBase := FConnection;
  FGetSourcePhraseQuery.SQL.Text := 'select lori_id from locucoes_origem where lori_locucao = :locucao';
  FGetSourcePhraseQuery.Prepare;

  FGetTargetPhraseQuery := TSQLQuery.Create(Owner);
  FGetTargetPhraseQuery.DataBase := FConnection;
  FGetTargetPhraseQuery.SQL.Text := 'select ldes_id from locucoes_destino where ldes_locucao = :locucao';
  FGetTargetPhraseQuery.Prepare;

  FGetTranslationCountQuery := TSQLQuery.Create(Owner);
  FGetTranslationCountQuery.DataBase := FConnection;
  FGetTranslationCountQuery.SQL.Text := 'select trad_ocorrencias from traducoes ' +
    'where trad_lori_id = :lori and trad_ldes_id = :ldes';
  FGetTranslationCountQuery.Prepare;

  FGetNextSourcePhraseIdQuery := TSQLQuery.Create(Owner);
  FGetNextSourcePhraseIdQuery.DataBase := FConnection;
  FGetNextSourcePhraseIdQuery.SQL.Text := 'select max(lori_id) + 1 from locucoes_origem';
  FGetNextSourcePhraseIdQuery.Prepare;

  FGetNextTargetPhraseIdQuery := TSQLQuery.Create(Owner);
  FGetNextTargetPhraseIdQuery.DataBase := FConnection;
  FGetNextTargetPhraseIdQuery.SQL.Text := 'select max(ldes_id) + 1 from locucoes_destino';
  FGetNextTargetPhraseIdQuery.Prepare;

  FInsertSourcePhraseQuery := TSQLQuery.Create(Owner);
  FInsertSourcePhraseQuery.DataBase := FConnection;
  FInsertSourcePhraseQuery.SQL.Text := 'insert into locucoes_origem (lori_id, lori_qtde_sintagmas, lori_locucao) values (:id, :qt, :lc)';
  FInsertSourcePhraseQuery.Prepare;

  FInsertTargetPhraseQuery := TSQLQuery.Create(Owner);
  FInsertTargetPhraseQuery.DataBase := FConnection;
  FInsertTargetPhraseQuery.SQL.Text := 'insert into locucoes_destino (ldes_id, ldes_qtde_sintagmas, ldes_locucao) values (:id, :qt, :lc)';
  FInsertTargetPhraseQuery.Prepare;

  FInsertSourcePhraseSyntagmQuery := TSQLQuery.Create(Owner);
  FInsertSourcePhraseSyntagmQuery.DataBase := FConnection;
  FInsertSourcePhraseSyntagmQuery.SQL.Text := 'insert into sintagmas_locucoes_origem (slor_lori_id, slor_sori_id, slor_ordem) values (:id, :st, :od)';
  FInsertSourcePhraseSyntagmQuery.Prepare;

  FInsertTargetPhraseSyntagmQuery := TSQLQuery.Create(Owner);
  FInsertTargetPhraseSyntagmQuery.DataBase := FConnection;
  FInsertTargetPhraseSyntagmQuery.SQL.Text := 'insert into sintagmas_locucoes_destino (slde_ldes_id, slde_sdes_id, slde_ordem) values (:id, :st, :od)';
  FInsertTargetPhraseSyntagmQuery.Prepare;

  FInsertTranslationQuery := TSQLQuery.Create(Owner);
  FInsertTranslationQuery.DataBase := FConnection;
  FInsertTranslationQuery.SQL.Text := 'insert into traducoes (trad_lori_id, trad_ldes_id, trad_ocorrencias) values (:lori, :ldes, 1)';
  FInsertTranslationQuery.Prepare;

  FInsertTranslationCountQuery := TSQLQuery.Create(Owner);
  FInsertTranslationCountQuery.DataBase := FConnection;
  FInsertTranslationCountQuery.SQL.Text := 'insert into traducoes ' +
    '(trad_lori_id, trad_ldes_id, trad_ocorrencias) ' +
    'values (:lori, :ldes, :ocorrencias)';
  FInsertTranslationCountQuery.Prepare;

  FUpdateTranslationQuery := TSQLQuery.Create(Owner);
  FUpdateTranslationQuery.DataBase := FConnection;
  FUpdateTranslationQuery.SQL.Text := 'update traducoes set trad_ocorrencias = trad_ocorrencias + 1 where trad_lori_id = :lori and trad_ldes_id = :ldes';
  FUpdateTranslationQuery.Prepare;

  FSuggestionsQuery := TSQLQuery.Create(Owner);
  FSuggestionsQuery.DataBase := FConnection;
  FSuggestionsQuery.SQL.Text := 'select lori_locucao, ldes_locucao ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where slor_sori_id = :sori and slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id and trad_ocorrencias > 0 ' +
      'order by lori_qtde_sintagmas desc, trad_ocorrencias desc';
  FSuggestionsQuery.Prepare;

  FSuggestionsWithoutMorphologyQuery := TSQLQuery.Create(Owner);
  FSuggestionsWithoutMorphologyQuery.DataBase := FConnection;
  FSuggestionsWithoutMorphologyQuery.SQL.Text := 'select lori_locucao, ldes_locucao ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where (slor_sori_id = :sori or (' +
      'substr(slor_sori_id, 1, length(:sori_prefix)) = :sori_prefix and ' +
      'substr(slor_sori_id, length(:sori_prefix) + 1, 1) = ''<'')) and ' +
      'slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id and trad_ocorrencias > 0 ' +
      'order by lori_qtde_sintagmas desc, trad_ocorrencias desc';
  FSuggestionsWithoutMorphologyQuery.Prepare;

  FTranslationMemoryQuery := TSQLQuery.Create(Owner);
  FTranslationMemoryQuery.DataBase := FConnection;
  FTranslationMemoryQuery.SQL.Text := 'select lori_locucao, ldes_locucao, ' +
      'trad_ocorrencias ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where (slor_sori_id = :sori or (' +
      'substr(slor_sori_id, 1, length(:sori_prefix)) = :sori_prefix and ' +
      'substr(slor_sori_id, length(:sori_prefix) + 1, 1) = ''<'')) and ' +
      'slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id and trad_ocorrencias > 0 ' +
      'order by trad_ocorrencias desc, ldes_locucao, lori_locucao';
  FTranslationMemoryQuery.Prepare;

  if FileExists('formas_verbos.db3') then
  begin
    FVerbConnection := TSQLite3Connection.Create(nil);
    FVerbConnection.DatabaseName := 'formas_verbos.db3';
    FVerbConnection.Transaction := TSQLTransaction.Create(nil);
    FVerbConnection.StartTransaction;
    FVerbConnection.Open;

    FVerbFormQuery := TSQLQuery.Create(Owner);
    FVerbFormQuery.DataBase := FVerbConnection;
    FVerbFormQuery.SQL.Text := 'select ver_verbo from verbo_forma, verbos where vfo_for_id = ' +
                               '(select for_id from formas where for_forma = :forma) and vfo_ver_id = ver_id';
    FVerbFormQuery.Prepare;
  end
  else
    FVerbFormQuery := nil;

  FDelimitedValues := TStringList.Create;
  FDelimitedValues.Delimiter := ';';
  FDelimitedValues.StrictDelimiter := True;

  FVerbList := TStringList.Create;
  FRecognizeVerbForms := True;
  FRemoveMorphologyRegex := RegexCreate('<wt.*?>', [rcoUTF8]);
  FRebuildSavepointActive := False;
  FBulkRebuildActive := False;
  FBulkTranslationsFlushed := False;
  FBulkSourcePhraseIds := TStringList.Create;
  FBulkSourcePhraseIds.Sorted := True;
  FBulkSourcePhraseIds.CaseSensitive := True;
  FBulkSourcePhraseIds.UseLocale := False;
  FBulkTargetPhraseIds := TStringList.Create;
  FBulkTargetPhraseIds.Sorted := True;
  FBulkTargetPhraseIds.CaseSensitive := True;
  FBulkTargetPhraseIds.UseLocale := False;
  FBulkTranslationCounts := TStringList.Create;
  FBulkTranslationCounts.Sorted := True;
  FBulkTranslationCounts.CaseSensitive := True;
  FBulkTranslationCounts.UseLocale := False;
  FBulkOccurrenceCounts := TStringList.Create;
  FBulkOccurrenceCounts.Sorted := True;
  FBulkOccurrenceCounts.CaseSensitive := True;
  FBulkOccurrenceCounts.UseLocale := False;
  FTranslationAlternativeCache := TStringList.Create;
  FTranslationAlternativeCache.Sorted := True;
  FTranslationAlternativeCache.CaseSensitive := True;
  FTranslationAlternativeCache.UseLocale := False;
end;

constructor TSuggestionManager.Create(const DatabasePath: string);
begin
  FTemporaryConnection := TSQLite3Connection.Create(nil);
  FTemporaryConnection.DatabaseName := DatabasePath;
  FTemporaryConnection.Transaction := TSQLTransaction.Create(nil);
  FTemporaryConnection.StartTransaction;
  FTemporaryConnection.Open;
  Create(nil, FTemporaryConnection);
end;

destructor TSuggestionManager.Destroy;
begin
  if Assigned(FTemporaryConnection) then
  begin
    FTemporaryConnection.Transaction.Free;
    FTemporaryConnection.Close;
    FTemporaryConnection.Free;
  end;

  FQuery.Free;
  FRemovePairQuery.Free;
  FGetSourcePhraseQuery.Free;
  FGetTargetPhraseQuery.Free;
  FGetTranslationCountQuery.Free;
  FGetNextSourcePhraseIdQuery.Free;
  FGetNextTargetPhraseIdQuery.Free;
  FInsertSourcePhraseQuery.Free;
  FInsertTargetPhraseQuery.Free;
  FInsertSourcePhraseSyntagmQuery.Free;
  FInsertTargetPhraseSyntagmQuery.Free;
  FInsertTranslationQuery.Free;
  FInsertTranslationCountQuery.Free;
  FUpdateTranslationQuery.Free;
  FSuggestionsQuery.Free;
  FSuggestionsWithoutMorphologyQuery.Free;
  FTranslationMemoryQuery.Free;

  if FVerbFormQuery <> nil then
  begin
    FVerbFormQuery.Close;
    FVerbFormQuery.Free;
    FVerbConnection.Transaction.Free;
    FVerbConnection.Close;
    FVerbConnection.Free;
  end;

  FDelimitedValues.Free;
  FVerbList.Free;
  ClearTranslationAlternativeCache;
  FTranslationAlternativeCache.Free;
  FBulkOccurrenceCounts.Free;
  FBulkTranslationCounts.Free;
  FBulkTargetPhraseIds.Free;
  FBulkSourcePhraseIds.Free;
  inherited Destroy;
end;

procedure TSuggestionManager.AddPair(const Source, Target: string);
begin
  AddPairForReference('', Source, Target);
end;

procedure TSuggestionManager.AddPairForReference(const ReferenceID, Source,
  Target: string);
var
  SourcePhraseId: Integer;
  TargetPhraseId: Integer;
begin
  if FBulkRebuildActive then
  begin
    AddBulkPair(Source, Target);
    AddBulkOccurrence(ReferenceID, Source, Target);
    Exit;
  end;

  ClearTranslationAlternativeCache;
  SourcePhraseId := GetOrCreateSourcePhraseId(Source.ToLower);
  TargetPhraseId := GetOrCreateTargetPhraseId(Target.ToLower);

  if not UpdateTranslation(SourcePhraseId, TargetPhraseId) then
    InsertTranslation(SourcePhraseId, TargetPhraseId);
  AddOccurrence(ReferenceID, Source, Target);
end;

procedure TSuggestionManager.AddOccurrence(const ReferenceID, Source,
  Target: string);
var
  NormalizedSource: string;
  NormalizedTarget: string;
begin
  NormalizedSource := Source.ToLower;
  NormalizedTarget := Target.ToLower;
  FQuery.Close;
  FQuery.SQL.Text := 'update association_memory_occurrences set ' +
    'amo_occurrences = amo_occurrences + 1 where ' +
    'amo_reference = :reference and amo_source = :source and ' +
    'amo_target = :target';
  FQuery.Params.ParamByName('reference').AsString := ReferenceID;
  FQuery.Params.ParamByName('source').AsString := NormalizedSource;
  FQuery.Params.ParamByName('target').AsString := NormalizedTarget;
  FQuery.ExecSQL;
  if FQuery.RowsAffected > 0 then
    Exit;

  FQuery.Close;
  FQuery.SQL.Text := 'insert into association_memory_occurrences ' +
    '(amo_reference, amo_source, amo_target, amo_occurrences) ' +
    'values (:reference, :source, :target, 1)';
  FQuery.Params.ParamByName('reference').AsString := ReferenceID;
  FQuery.Params.ParamByName('source').AsString := NormalizedSource;
  FQuery.Params.ParamByName('target').AsString := NormalizedTarget;
  FQuery.ExecSQL;
end;

procedure TSuggestionManager.InsertSourcePhraseWithId(const Phrase: string;
  PhraseId: Integer);
var
  PhrasePartIndex: Integer;
begin
  FDelimitedValues.DelimitedText := Phrase;
  FInsertSourcePhraseQuery.Close;
  FInsertSourcePhraseQuery.Params[0].AsInteger := PhraseId;
  FInsertSourcePhraseQuery.Params[1].AsInteger := FDelimitedValues.Count;
  FInsertSourcePhraseQuery.Params[2].AsString := Phrase;
  FInsertSourcePhraseQuery.ExecSQL;

  for PhrasePartIndex := 0 to FDelimitedValues.Count - 1 do
  begin
    FInsertSourcePhraseSyntagmQuery.Close;
    FInsertSourcePhraseSyntagmQuery.Params[0].AsInteger := PhraseId;
    FInsertSourcePhraseSyntagmQuery.Params[1].AsString :=
      FDelimitedValues[PhrasePartIndex];
    FInsertSourcePhraseSyntagmQuery.Params[2].AsInteger := PhrasePartIndex;
    FInsertSourcePhraseSyntagmQuery.ExecSQL;
  end;
end;

procedure TSuggestionManager.InsertTargetPhraseWithId(const Phrase: string;
  PhraseId: Integer);
var
  PhrasePartIndex: Integer;
begin
  FDelimitedValues.DelimitedText := Phrase;
  FInsertTargetPhraseQuery.Close;
  FInsertTargetPhraseQuery.Params[0].AsInteger := PhraseId;
  FInsertTargetPhraseQuery.Params[1].AsInteger := FDelimitedValues.Count;
  FInsertTargetPhraseQuery.Params[2].AsString := Phrase;
  FInsertTargetPhraseQuery.ExecSQL;

  for PhrasePartIndex := 0 to FDelimitedValues.Count - 1 do
  begin
    FInsertTargetPhraseSyntagmQuery.Close;
    FInsertTargetPhraseSyntagmQuery.Params[0].AsInteger := PhraseId;
    FInsertTargetPhraseSyntagmQuery.Params[1].AsString :=
      FDelimitedValues[PhrasePartIndex];
    FInsertTargetPhraseSyntagmQuery.Params[2].AsInteger := PhrasePartIndex;
    FInsertTargetPhraseSyntagmQuery.ExecSQL;
  end;
end;

function TSuggestionManager.GetOrCreateBulkSourcePhraseId(
  const Phrase: string): Integer;
var
  PhraseIndex: Integer;
begin
  PhraseIndex := FBulkSourcePhraseIds.IndexOf(Phrase);
  if PhraseIndex >= 0 then
    Exit(PtrInt(FBulkSourcePhraseIds.Objects[PhraseIndex]));

  Result := FNextBulkSourcePhraseId;
  Inc(FNextBulkSourcePhraseId);
  InsertSourcePhraseWithId(Phrase, Result);
  FBulkSourcePhraseIds.AddObject(Phrase, TObject(PtrInt(Result)));
end;

function TSuggestionManager.GetOrCreateBulkTargetPhraseId(
  const Phrase: string): Integer;
var
  PhraseIndex: Integer;
begin
  PhraseIndex := FBulkTargetPhraseIds.IndexOf(Phrase);
  if PhraseIndex >= 0 then
    Exit(PtrInt(FBulkTargetPhraseIds.Objects[PhraseIndex]));

  Result := FNextBulkTargetPhraseId;
  Inc(FNextBulkTargetPhraseId);
  InsertTargetPhraseWithId(Phrase, Result);
  FBulkTargetPhraseIds.AddObject(Phrase, TObject(PtrInt(Result)));
end;

procedure TSuggestionManager.AddBulkPair(const Source, Target: string);
var
  CountIndex: Integer;
  PairKey: string;
  SourcePhraseId: Integer;
  TargetPhraseId: Integer;
begin
  if FBulkTranslationsFlushed then
    raise Exception.Create(SCannotAddPairsAfterRebuildValidation);
  SourcePhraseId := GetOrCreateBulkSourcePhraseId(Source.ToLower);
  TargetPhraseId := GetOrCreateBulkTargetPhraseId(Target.ToLower);
  PairKey := IntToStr(SourcePhraseId) + #1 + IntToStr(TargetPhraseId);
  CountIndex := FBulkTranslationCounts.IndexOf(PairKey);
  if CountIndex < 0 then
    FBulkTranslationCounts.AddObject(PairKey, TObject(PtrInt(1)))
  else
    FBulkTranslationCounts.Objects[CountIndex] := TObject(
      PtrInt(FBulkTranslationCounts.Objects[CountIndex]) + 1);
end;

procedure TSuggestionManager.AddBulkOccurrence(const ReferenceID, Source,
  Target: string);
var
  CountIndex: Integer;
  OccurrenceKey: string;
begin
  if FBulkTranslationsFlushed then
    raise Exception.Create(SCannotAddPairsAfterRebuildValidation);
  OccurrenceKey := ReferenceID + #1 + Source.ToLower + #1 + Target.ToLower;
  CountIndex := FBulkOccurrenceCounts.IndexOf(OccurrenceKey);
  if CountIndex < 0 then
    FBulkOccurrenceCounts.AddObject(OccurrenceKey, TObject(PtrInt(1)))
  else
    FBulkOccurrenceCounts.Objects[CountIndex] := TObject(
      PtrInt(FBulkOccurrenceCounts.Objects[CountIndex]) + 1);
end;

procedure TSuggestionManager.FlushBulkTranslations;
var
  CountIndex: Integer;
  FirstSeparatorIndex: Integer;
  SeparatorIndex: Integer;
  PairKey: string;
  ReferenceID: string;
  SourcePhrase: string;
  TargetPhrase: string;
  TotalWrites: Integer;
begin
  if FBulkTranslationsFlushed then
    Exit;
  TotalWrites := FBulkTranslationCounts.Count + FBulkOccurrenceCounts.Count;
  ReportRebuildProgress(amrpWriting, 0, TotalWrites);
  for CountIndex := 0 to FBulkTranslationCounts.Count - 1 do
  begin
    PairKey := FBulkTranslationCounts[CountIndex];
    SeparatorIndex := Pos(#1, PairKey);
    if SeparatorIndex <= 1 then
      raise Exception.Create(SInvalidBufferedAssociationMemoryPair);

    FInsertTranslationCountQuery.Close;
    FInsertTranslationCountQuery.Params[0].AsInteger := StrToInt(
      Copy(PairKey, 1, SeparatorIndex - 1));
    FInsertTranslationCountQuery.Params[1].AsInteger := StrToInt(
      Copy(PairKey, SeparatorIndex + 1, MaxInt));
    FInsertTranslationCountQuery.Params[2].AsInteger := PtrInt(
      FBulkTranslationCounts.Objects[CountIndex]);
    FInsertTranslationCountQuery.ExecSQL;
    if ((CountIndex + 1) mod 500) = 0 then
      ReportRebuildProgress(amrpWriting, CountIndex + 1,
        TotalWrites);
  end;

  for CountIndex := 0 to FBulkOccurrenceCounts.Count - 1 do
  begin
    PairKey := FBulkOccurrenceCounts[CountIndex];
    FirstSeparatorIndex := Pos(#1, PairKey);
    SeparatorIndex := PosEx(#1, PairKey, FirstSeparatorIndex + 1);
    if (FirstSeparatorIndex <= 0) or
       (SeparatorIndex <= FirstSeparatorIndex + 1) then
      raise Exception.Create(SInvalidBufferedAssociationMemoryPair);
    ReferenceID := Copy(PairKey, 1, FirstSeparatorIndex - 1);
    SourcePhrase := Copy(PairKey, FirstSeparatorIndex + 1,
      SeparatorIndex - FirstSeparatorIndex - 1);
    TargetPhrase := Copy(PairKey, SeparatorIndex + 1, MaxInt);

    FQuery.Close;
    FQuery.SQL.Text := 'insert into association_memory_occurrences ' +
      '(amo_reference, amo_source, amo_target, amo_occurrences) ' +
      'values (:reference, :source, :target, :occurrences)';
    FQuery.Params.ParamByName('reference').AsString := ReferenceID;
    FQuery.Params.ParamByName('source').AsString := SourcePhrase;
    FQuery.Params.ParamByName('target').AsString := TargetPhrase;
    FQuery.Params.ParamByName('occurrences').AsInteger := PtrInt(
      FBulkOccurrenceCounts.Objects[CountIndex]);
    FQuery.ExecSQL;
    if ((CountIndex + 1) mod 500) = 0 then
      ReportRebuildProgress(amrpWriting,
        FBulkTranslationCounts.Count + CountIndex + 1, TotalWrites);
  end;
  FBulkTranslationsFlushed := True;
  ReportRebuildProgress(amrpWriting, TotalWrites, TotalWrites);
end;

procedure TSuggestionManager.StartBulkRebuild;
begin
  FBulkSourcePhraseIds.Clear;
  FBulkTargetPhraseIds.Clear;
  FBulkTranslationCounts.Clear;
  FBulkOccurrenceCounts.Clear;
  FNextBulkSourcePhraseId := 1;
  FNextBulkTargetPhraseId := 1;
  FBulkRebuildActive := True;
  FBulkTranslationsFlushed := False;
end;

procedure TSuggestionManager.ResetBulkRebuild;
begin
  FBulkRebuildActive := False;
  FBulkTranslationsFlushed := False;
  FBulkSourcePhraseIds.Clear;
  FBulkTargetPhraseIds.Clear;
  FBulkTranslationCounts.Clear;
  FBulkOccurrenceCounts.Clear;
end;

procedure TSuggestionManager.ClearTranslationAlternativeCache;
var
  CacheIndex: Integer;
begin
  if not Assigned(FTranslationAlternativeCache) then
    Exit;
  for CacheIndex := 0 to FTranslationAlternativeCache.Count - 1 do
    FTranslationAlternativeCache.Objects[CacheIndex].Free;
  FTranslationAlternativeCache.Clear;
end;

procedure TSuggestionManager.ReportRebuildProgress(
  Phase: TAssociationMemoryRebuildPhase; Current, Total: Integer);
begin
  if Assigned(FOnRebuildProgress) then
    FOnRebuildProgress(Phase, Current, Total);
end;

procedure TSuggestionManager.RemovePair(const Source, Target: string);
begin
  RemovePairForReference('', Source, Target);
end;

procedure TSuggestionManager.RemovePairForReference(const ReferenceID, Source,
  Target: string);
var
  SourcePhraseId: Integer;
  TargetPhraseId: Integer;
begin
  ClearTranslationAlternativeCache;
  if not RemoveOccurrence(ReferenceID, Source, Target) then
    Exit;
  SourcePhraseId := FindSourcePhraseId(Source.ToLower);
  TargetPhraseId := FindTargetPhraseId(Target.ToLower);

  if (SourcePhraseId > 0) and (TargetPhraseId > 0) then
  begin
    FRemovePairQuery.Close;
    FRemovePairQuery.Params[0].AsInteger := SourcePhraseId;
    FRemovePairQuery.Params[1].AsInteger := TargetPhraseId;
    FRemovePairQuery.ExecSQL;

    if FRemovePairQuery.RowsAffected = 0 then
    begin
      FQuery.Close;
      FQuery.SQL.Text := 'delete from traducoes where trad_lori_id = :lori ' +
        'and trad_ldes_id = :ldes and ' +
        '(trad_ocorrencias is null or trad_ocorrencias <= 1)';
      FQuery.Params.ParamByName('lori').AsInteger := SourcePhraseId;
      FQuery.Params.ParamByName('ldes').AsInteger := TargetPhraseId;
      FQuery.ExecSQL;
    end;
  end;
  CleanupOrphanPhrases(SourcePhraseId, TargetPhraseId);
end;

function TSuggestionManager.RemoveOccurrence(const ReferenceID, Source,
  Target: string): Boolean;
begin
  Result := False;
  FQuery.Close;
  FQuery.SQL.Text := 'update association_memory_occurrences set ' +
    'amo_occurrences = amo_occurrences - 1 where ' +
    'amo_reference = :reference and amo_source = :source and ' +
    'amo_target = :target and amo_occurrences > 1';
  FQuery.Params.ParamByName('reference').AsString := ReferenceID;
  FQuery.Params.ParamByName('source').AsString := Source.ToLower;
  FQuery.Params.ParamByName('target').AsString := Target.ToLower;
  FQuery.ExecSQL;
  if FQuery.RowsAffected > 0 then
    Exit(True);

  FQuery.Close;
  FQuery.SQL.Text := 'delete from association_memory_occurrences where ' +
    'amo_reference = :reference and amo_source = :source and ' +
    'amo_target = :target and amo_occurrences <= 1';
  FQuery.Params.ParamByName('reference').AsString := ReferenceID;
  FQuery.Params.ParamByName('source').AsString := Source.ToLower;
  FQuery.Params.ParamByName('target').AsString := Target.ToLower;
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected > 0;
end;

procedure TSuggestionManager.CleanupOrphanPhrases(SourcePhraseId,
  TargetPhraseId: Integer);
begin
  if SourcePhraseId > 0 then
  begin
    FQuery.Close;
    FQuery.SQL.Text := 'delete from sintagmas_locucoes_origem where slor_lori_id = :id ' +
      'and not exists (select 1 from traducoes where trad_lori_id = :id)';
    FQuery.Params.ParamByName('id').AsInteger := SourcePhraseId;
    FQuery.ExecSQL;

    FQuery.Close;
    FQuery.SQL.Text := 'delete from locucoes_origem where lori_id = :id ' +
      'and not exists (select 1 from traducoes where trad_lori_id = :id)';
    FQuery.Params.ParamByName('id').AsInteger := SourcePhraseId;
    FQuery.ExecSQL;
  end;

  if TargetPhraseId > 0 then
  begin
    FQuery.Close;
    FQuery.SQL.Text := 'delete from sintagmas_locucoes_destino where slde_ldes_id = :id ' +
      'and not exists (select 1 from traducoes where trad_ldes_id = :id)';
    FQuery.Params.ParamByName('id').AsInteger := TargetPhraseId;
    FQuery.ExecSQL;

    FQuery.Close;
    FQuery.SQL.Text := 'delete from locucoes_destino where ldes_id = :id ' +
      'and not exists (select 1 from traducoes where trad_ldes_id = :id)';
    FQuery.Params.ParamByName('id').AsInteger := TargetPhraseId;
    FQuery.ExecSQL;
  end;
end;

function TSuggestionManager.GetOrCreateSourcePhraseId(const Phrase: string): Integer;
begin
  Result := FindSourcePhraseId(Phrase);
  if Result <= 0 then
    Result := InsertSourcePhraseId(Phrase);
end;

function TSuggestionManager.FindSourcePhraseId(const Phrase: string): Integer;
begin
  FGetSourcePhraseQuery.Close;
  FGetSourcePhraseQuery.Params[0].AsString := Phrase;
  FGetSourcePhraseQuery.Open;

  if FGetSourcePhraseQuery.EOF then
    Result := 0
  else
    Result := FGetSourcePhraseQuery.Fields[0].AsInteger;
end;

function TSuggestionManager.InsertSourcePhraseId(const Phrase: string): Integer;
var
  PhrasePartIndex: SmallInt;
begin
  FDelimitedValues.DelimitedText := Phrase;

  FGetNextSourcePhraseIdQuery.Close;
  FGetNextSourcePhraseIdQuery.Open;

  try
    Result := FGetNextSourcePhraseIdQuery.Fields[0].AsInteger;
  except
    Result := 1;
  end;

  FInsertSourcePhraseQuery.Close;
  FInsertSourcePhraseQuery.Params[0].AsInteger := Result;
  FInsertSourcePhraseQuery.Params[1].AsInteger := FDelimitedValues.Count;
  FInsertSourcePhraseQuery.Params[2].AsString := Phrase;
  FInsertSourcePhraseQuery.ExecSQL;

  for PhrasePartIndex := 0 to FDelimitedValues.Count - 1 do
  begin
    FInsertSourcePhraseSyntagmQuery.Close;
    FInsertSourcePhraseSyntagmQuery.Params[0].AsInteger := Result;
    FInsertSourcePhraseSyntagmQuery.Params[1].AsString := FDelimitedValues.Strings[PhrasePartIndex];
    FInsertSourcePhraseSyntagmQuery.Params[2].AsInteger := PhrasePartIndex;
    FInsertSourcePhraseSyntagmQuery.ExecSQL;
  end;
end;

function TSuggestionManager.GetOrCreateTargetPhraseId(const Phrase: string): Integer;
begin
  Result := FindTargetPhraseId(Phrase);

  if Result <= 0 then
    Result := InsertTargetPhraseId(Phrase);
end;

function TSuggestionManager.FindTargetPhraseId(const Phrase: string): Integer;
begin
  FGetTargetPhraseQuery.Close;
  FGetTargetPhraseQuery.Params[0].AsString := Phrase;
  FGetTargetPhraseQuery.Open;

  if FGetTargetPhraseQuery.EOF then
    Result := 0
  else
    Result := FGetTargetPhraseQuery.Fields[0].AsInteger;
end;

function TSuggestionManager.InsertTargetPhraseId(const Phrase: string): Integer;
var
  PhrasePartIndex: SmallInt;
begin
  FDelimitedValues.DelimitedText := Phrase;

  FGetNextTargetPhraseIdQuery.Close;
  FGetNextTargetPhraseIdQuery.Open;

  try
    Result := FGetNextTargetPhraseIdQuery.Fields[0].AsInteger;
  except
    Result := 1;
  end;

  FInsertTargetPhraseQuery.Close;
  FInsertTargetPhraseQuery.Params[0].AsInteger := Result;
  FInsertTargetPhraseQuery.Params[1].AsInteger := FDelimitedValues.Count;
  FInsertTargetPhraseQuery.Params[2].AsString := Phrase;
  FInsertTargetPhraseQuery.ExecSQL;

  for PhrasePartIndex := 0 to FDelimitedValues.Count - 1 do
  begin
    FInsertTargetPhraseSyntagmQuery.Close;
    FInsertTargetPhraseSyntagmQuery.Params[0].AsInteger := Result;
    FInsertTargetPhraseSyntagmQuery.Params[1].AsString := FDelimitedValues.Strings[PhrasePartIndex];
    FInsertTargetPhraseSyntagmQuery.Params[2].AsInteger := PhrasePartIndex;
    FInsertTargetPhraseSyntagmQuery.ExecSQL;
  end;
end;

function TSuggestionManager.UpdateTranslation(SourcePhraseId,
  TargetPhraseId: Integer): Boolean;
begin
  FUpdateTranslationQuery.Close;
  FUpdateTranslationQuery.Params[0].AsInteger := SourcePhraseId;
  FUpdateTranslationQuery.Params[1].AsInteger := TargetPhraseId;
  FUpdateTranslationQuery.ExecSQL;

  Result := FUpdateTranslationQuery.RowsAffected = 1;
end;

function TSuggestionManager.InsertTranslation(SourcePhraseId,
  TargetPhraseId: Integer): Boolean;
begin
  FInsertTranslationQuery.Close;
  FInsertTranslationQuery.Params[0].AsInteger := SourcePhraseId;
  FInsertTranslationQuery.Params[1].AsInteger := TargetPhraseId;
  FInsertTranslationQuery.ExecSQL;

  Result := FInsertTranslationQuery.RowsAffected = 1;
end;

function TSuggestionManager.IdentifyVerb(const Form: string): Boolean;
begin
  FVerbList.Clear;
  FVerbFormQuery.Close;
  FVerbFormQuery.Params[0].AsString := Form;
  FVerbFormQuery.Open;
  while not FVerbFormQuery.EOF do
  begin
    FVerbList.Add(FVerbFormQuery.Fields[0].AsString);
    FVerbFormQuery.Next;
  end;
  Result := FVerbList.Count > 0;
end;

function TSuggestionManager.NormalizeVerb(const Phrase: string): string;
begin
  Result := Phrase;
  FDelimitedValues.DelimitedText := Phrase;
  if IdentifyVerb(FDelimitedValues.Strings[0]) then
  begin
    FDelimitedValues.Strings[0] := FVerbList.Strings[0];
    Result := FDelimitedValues.DelimitedText;
  end;
end;

function TSuggestionManager.GetSuggestions(const Key: string): TSQLQuery;
var
  KeyWithoutMorphology: string;
begin
  FSuggestionsQuery.Close;
  FSuggestionsQuery.Params[0].AsString := Key;
  FSuggestionsQuery.Open;
  DebugLn('%d translation suggestions found with morphology', [FSuggestionsQuery.RecordCount]);
  Result := FSuggestionsQuery;
  if FSuggestionsQuery.RecordCount = 0 then
  begin
    Result := FSuggestionsWithoutMorphologyQuery;

    KeyWithoutMorphology := FRemoveMorphologyRegex.Replace(Key, '');
    FSuggestionsWithoutMorphologyQuery.Close;
    FSuggestionsWithoutMorphologyQuery.Params.ParamByName(
      'sori').AsString := KeyWithoutMorphology;
    FSuggestionsWithoutMorphologyQuery.Params.ParamByName(
      'sori_prefix').AsString := KeyWithoutMorphology;
    FSuggestionsWithoutMorphologyQuery.Open;
    DebugLn('%d translation suggestions found without morphology', [FSuggestionsWithoutMorphologyQuery.RecordCount]);
  end;
end;

function TSuggestionManager.GetTranslationMemoryQuery(const Key: string): TSQLQuery;
begin
  Result := FTranslationMemoryQuery;
  FTranslationMemoryQuery.Close;
  FTranslationMemoryQuery.Params.ParamByName('sori').AsString := Key;
  FTranslationMemoryQuery.Params.ParamByName(
    'sori_prefix').AsString := Key;
  FTranslationMemoryQuery.Open;
end;

procedure TSuggestionManager.SuggestAssociations(SourceVerse: TVerse);
  function CompareTarget(const LeftValue, RightValue: string): Boolean;
  var
    NormalizedLeft: string;
    NormalizedRight: string;
  begin
    DebugLn('  comparing target [%s] x [%s]', [LeftValue, RightValue]);
    if LeftValue = RightValue then
      Exit(True);

    if FVerbFormQuery <> nil then
    begin
      NormalizedLeft := NormalizeVerb(LeftValue);
      NormalizedRight := NormalizeVerb(RightValue);
      DebugLn('  comparing target [%s] x [%s]', [NormalizedLeft, NormalizedRight]);
      if NormalizedLeft = NormalizedRight then
        Exit(True);
    end;

    NormalizedLeft := LeftValue;
    NormalizedRight := RightValue;
    if AnsiEndsStr('s', LeftValue) then
      NormalizedLeft := Copy(LeftValue, 0, Length(LeftValue) - 1);
    if AnsiEndsStr('s', RightValue) then
      NormalizedRight := Copy(RightValue, 0, Length(RightValue) - 1);
    DebugLn('  comparing target [%s] x [%s]', [NormalizedLeft, NormalizedRight]);
    if NormalizedLeft = NormalizedRight then
      Exit(True);

    Result := False;
  end;

  function CompareSource(LeftValue, RightValue: string): Boolean;
  begin
    DebugLn('  comparing source with morphology [%s] x [%s]', [LeftValue, RightValue]);
    Result := False;
    if LeftValue = RightValue then
      Result := True
    else
    begin
      LeftValue := FRemoveMorphologyRegex.Replace(LeftValue, '');
      RightValue := FRemoveMorphologyRegex.Replace(RightValue, '');
      DebugLn('  comparing source without morphology [%s] x [%s]', [LeftValue, RightValue]);
      Result := LeftValue = RightValue;
    end;
  end;

var
  SourceStartIndex: SmallInt;
  SourceScanIndex: SmallInt;
  SourcePhraseIndex: SmallInt;
  TargetPhraseIndex: SmallInt;
  TargetStartIndex: SmallInt;
  TargetScanIndex: SmallInt;
  SelectedSyntagm: TSyntagm;
  TargetVerse: TVerse;
  Associated: Boolean;
  Suggestions: TSQLQuery;
  SourcePhrase: TStringList;
  TargetPhrase: TStringList;
begin
  DebugLn(#13#10'starting association suggestion');

  SourcePhrase := TStringList.Create;
  TargetPhrase := TStringList.Create;
  SourcePhrase.Delimiter := ';';
  SourcePhrase.StrictDelimiter := True;
  TargetPhrase.Delimiter := ';';
  TargetPhrase.StrictDelimiter := True;

  TargetVerse := SourceVerse.PairedVerse;
  SourceStartIndex := -1;
  Suggestions := nil;
  try
    while SourceStartIndex < SourceVerse.Syntagms.Count - 1 do
    begin
      Associated := False;
      Inc(SourceStartIndex);
      SourceVerse.ClearSelection;
      SelectedSyntagm := SourceVerse.Syntagms[SourceStartIndex];

      if (SelectedSyntagm.Kind <> tsSintagma) or (SelectedSyntagm.Pairs.Count > 0) then
        Continue;

      SelectedSyntagm.AddToSelection;
      DebugLn('new unassociated source syntagm: %s', [SelectedSyntagm.Text]);

      Suggestions := GetSuggestions(SelectedSyntagm.GetSuggestionKey(plMetaData));

      while not Suggestions.EOF do
      begin
        DebugLn('testing suggestion: %s => %s', [Suggestions.Fields[0].AsString, Suggestions.Fields[1].AsString]);

        SourcePhrase.DelimitedText := Suggestions.Fields[0].AsString;

        SourceScanIndex := SourceStartIndex + 1;
        SourcePhraseIndex := 1;
        while (SourceScanIndex < SourceVerse.Syntagms.Count) and
              (SourcePhraseIndex < SourcePhrase.Count) do
        begin
          if (SourceVerse.Syntagms[SourceScanIndex].Kind = tsSintagma) and
             (SourceVerse.Syntagms[SourceScanIndex].IsItalic = False) then
          begin
            if not CompareSource(
                 SourceVerse.Syntagms[SourceScanIndex].GetSuggestionKey(plMetaData),
                 SourcePhrase.Strings[SourcePhraseIndex]) then
              Break;

            SourceVerse.Syntagms[SourceScanIndex].AddToSelection;
            Inc(SourcePhraseIndex);
          end;
          Inc(SourceScanIndex);
        end;

        if SourcePhraseIndex = SourcePhrase.Count then
        begin
          DebugLn('source phrase %s found in source text', [Suggestions.Fields[0].AsString]);
          TargetPhrase.DelimitedText := Suggestions.Fields[1].AsString;
          for TargetStartIndex := 0 to TargetVerse.Syntagms.Count - 1 do
          begin
            TargetVerse.ClearSelection;
            SelectedSyntagm := TargetVerse.Syntagms[TargetStartIndex];
            if (SelectedSyntagm.Kind <> tsSintagma) or SelectedSyntagm.IsItalic or
               (SelectedSyntagm.Pairs.Count > 0) then
              Continue;

            if CompareTarget(SelectedSyntagm.GetSuggestionKey(plMetaData), TargetPhrase.Strings[0]) then
            begin
              DebugLn('target phrase start %s found in target text', [TargetPhrase.Strings[0]]);
              SelectedSyntagm.AddToSelection;
              TargetScanIndex := TargetStartIndex + 1;
              TargetPhraseIndex := 1;
              DebugLn('    searching remaining terms of phrase %s', [TargetPhrase.DelimitedText]);
              while (TargetScanIndex < TargetVerse.Syntagms.Count) and
                    (TargetPhraseIndex < TargetPhrase.Count) do
              begin
                if TargetVerse.Syntagms[TargetScanIndex].Kind = tsSintagma then
                begin
                  DebugLn('    searching phrase term: %s', [TargetPhrase.Strings[TargetPhraseIndex]]);
                  if CompareTarget(
                       TargetVerse.Syntagms[TargetScanIndex].GetSuggestionKey(plMetaData),
                       TargetPhrase.Strings[TargetPhraseIndex]) then
                    TargetVerse.Syntagms[TargetScanIndex].AddToSelection
                  else
                    Break;
                  Inc(TargetPhraseIndex);
                end;
                Inc(TargetScanIndex);
              end;

              if TargetPhraseIndex = TargetPhrase.Count then
              begin
                DebugLn('target phrase %s found in target text', [Suggestions.Fields[1].AsString]);
                SourceVerse.AssociateSyntagms;
                Associated := True;
                Break;
              end;
            end;
          end;
        end;

        if Associated then
          Break;

        Suggestions.Next;
      end;
    end;
  finally
    if Assigned(Suggestions) then
      Suggestions.Close;
    SourceVerse.ClearSelection;
    if Assigned(TargetVerse) then
      TargetVerse.ClearSelection;
    SourcePhrase.Free;
    TargetPhrase.Free;
  end;
end;

procedure TSuggestionManager.ClearAssociationDatabase;
begin
  ClearTranslationAlternativeCache;
  with FQuery do
  begin
    Close;
    SQL.Text := 'delete from association_memory_occurrences';
    ExecSQL;

    Close;
    SQL.Text := 'delete from traducoes';
    ExecSQL;

    Close;
    SQL.Text := 'delete from sintagmas_locucoes_origem';
    ExecSQL;

    Close;
    SQL.Text := 'delete from sintagmas_locucoes_destino';
    ExecSQL;

    Close;
    SQL.Text := 'delete from locucoes_origem';
    ExecSQL;

    Close;
    SQL.Text := 'delete from locucoes_destino';
    ExecSQL;
  end;
  StartBulkRebuild;
end;

procedure TSuggestionManager.BeginRebuild;
begin
  if FRebuildSavepointActive then
    raise Exception.Create(SAssociationMemoryRebuildAlreadyActive);
  ClearTranslationAlternativeCache;
  ResetBulkRebuild;
  FConnection.ExecuteDirect('savepoint association_memory_rebuild');
  FRebuildSavepointActive := True;
end;

procedure TSuggestionManager.CommitRebuild;
begin
  if not FRebuildSavepointActive then
    Exit;
  PrepareRebuildValidation;
  ReportRebuildProgress(amrpFinalizing, 0, 0);
  FConnection.ExecuteDirect('release savepoint association_memory_rebuild');
  FRebuildSavepointActive := False;
  ResetBulkRebuild;
end;

procedure TSuggestionManager.PrepareRebuildValidation;
begin
  if FBulkRebuildActive then
    FlushBulkTranslations;
end;

procedure TSuggestionManager.RollbackRebuild;
begin
  if not FRebuildSavepointActive then
    Exit;
  try
    try
      FConnection.ExecuteDirect(
        'rollback to savepoint association_memory_rebuild');
      FConnection.ExecuteDirect(
        'release savepoint association_memory_rebuild');
    except
      // Some SQLDB/SQLite errors invalidate the prepared statement state
      // before a savepoint command can be issued. Falling back to the clean
      // transaction boundary still restores the previously committed cache.
      if Assigned(FConnection.Transaction) and
        FConnection.Transaction.Active then
        FConnection.Transaction.Rollback;
      if Assigned(FConnection.Transaction) and
        not FConnection.Transaction.Active then
        FConnection.StartTransaction;
    end;
  finally
    FRebuildSavepointActive := False;
    ClearTranslationAlternativeCache;
    ResetBulkRebuild;
  end;
end;

procedure TSuggestionManager.Commit;
begin
  FConnection.Transaction.Commit;
  FConnection.StartTransaction;
end;

procedure TSuggestionManager.Rollback;
begin
  FRebuildSavepointActive := False;
  ClearTranslationAlternativeCache;
  ResetBulkRebuild;
  FConnection.Transaction.Rollback;
  FConnection.StartTransaction;
end;

function TSuggestionManager.Validate(out ErrorMessage: string): Boolean;
begin
  ErrorMessage := '';
  try
    FQuery.Close;
    FQuery.SQL.Text := 'select count(*) from traducoes where ' +
      'trad_ocorrencias is null or trad_ocorrencias <= 0';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage :=
        SAssociationMemoryNonPositiveCounts;
      Exit(False);
    end;

    FQuery.Close;
    FQuery.SQL.Text := 'select count(*) from traducoes t ' +
      'left join locucoes_origem o on o.lori_id = t.trad_lori_id ' +
      'left join locucoes_destino d on d.ldes_id = t.trad_ldes_id ' +
      'where o.lori_id is null or d.ldes_id is null';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage :=
        SAssociationMemoryMissingPhrases;
      Exit(False);
    end;

    FQuery.Close;
    FQuery.SQL.Text := 'select (' +
      'select count(*) from locucoes_origem o where not exists ' +
        '(select 1 from traducoes t where t.trad_lori_id = o.lori_id)' +
      ') + (' +
      'select count(*) from locucoes_destino d where not exists ' +
        '(select 1 from traducoes t where t.trad_ldes_id = d.ldes_id)' +
      ')';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage := SAssociationMemoryOrphanPhrases;
      Exit(False);
    end;

    FQuery.Close;
    FQuery.SQL.Text := 'select (' +
      'select count(*) from sintagmas_locucoes_origem s left join ' +
        'locucoes_origem o on o.lori_id = s.slor_lori_id ' +
        'where o.lori_id is null' +
      ') + (' +
      'select count(*) from sintagmas_locucoes_destino s left join ' +
        'locucoes_destino d on d.ldes_id = s.slde_ldes_id ' +
        'where d.ldes_id is null' +
      ')';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage :=
        SAssociationMemoryOrphanSyntagmMappings;
      Exit(False);
    end;

    FQuery.Close;
    FQuery.SQL.Text := 'select count(*) from ' +
      'association_memory_occurrences where amo_occurrences <= 0';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage := SAssociationMemoryNonPositiveOccurrenceReferences;
      Exit(False);
    end;

    FQuery.Close;
    FQuery.SQL.Text := 'select count(*) from ' +
      'association_memory_occurrences o where o.amo_reference <> '''' ' +
      'and not exists (select 1 from pares p where ' +
      'p.pare_id = o.amo_reference)';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage := SAssociationMemoryOrphanOccurrenceReferences;
      Exit(False);
    end;

    FQuery.Close;
    FQuery.SQL.Text := 'select count(*) from (' +
      'select t.trad_lori_id, t.trad_ldes_id, t.trad_ocorrencias, ' +
      'coalesce(sum(r.amo_occurrences), 0) as reference_occurrences ' +
      'from traducoes t ' +
      'join locucoes_origem o on o.lori_id = t.trad_lori_id ' +
      'join locucoes_destino d on d.ldes_id = t.trad_ldes_id ' +
      'left join association_memory_occurrences r on ' +
      'r.amo_source = o.lori_locucao and r.amo_target = d.ldes_locucao ' +
      'group by t.trad_lori_id, t.trad_ldes_id, t.trad_ocorrencias ' +
      'having t.trad_ocorrencias <> reference_occurrences)';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage := SAssociationMemoryOccurrenceMismatch;
      Exit(False);
    end;

    FQuery.Close;
    FQuery.SQL.Text := 'select count(*) from ' +
      'association_memory_occurrences r where not exists (' +
      'select 1 from traducoes t ' +
      'join locucoes_origem o on o.lori_id = t.trad_lori_id ' +
      'join locucoes_destino d on d.ldes_id = t.trad_ldes_id ' +
      'where o.lori_locucao = r.amo_source and ' +
      'd.ldes_locucao = r.amo_target)';
    FQuery.Open;
    if FQuery.Fields[0].AsInteger > 0 then
    begin
      ErrorMessage := SAssociationMemoryOccurrenceMismatch;
      Exit(False);
    end;

    Result := True;
  finally
    FQuery.Close;
  end;
end;

function TSuggestionManager.ValidatePreparedRebuild(
  out ErrorMessage: string): Boolean;
var
  CountIndex: Integer;
  ExpectedCount: Integer;
  OccurrenceKey: string;
  PairKey: string;
  StoredCount: Integer;
  StoredOccurrenceCount: Integer;
  StoredPairCount: Integer;
  TotalExpectedCount: Integer;
begin
  ErrorMessage := '';
  if not FBulkRebuildActive or not FBulkTranslationsFlushed then
  begin
    ErrorMessage := SAssociationMemoryRebuildNotPrepared;
    Exit(False);
  end;

  StoredPairCount := 0;
  TotalExpectedCount := FBulkTranslationCounts.Count +
    FBulkOccurrenceCounts.Count;
  ReportRebuildProgress(amrpValidating, 0, TotalExpectedCount);
  FQuery.Close;
  FQuery.SQL.Text := 'select trad_lori_id, trad_ldes_id, ' +
    'trad_ocorrencias from traducoes';
  FQuery.Open;
  try
    while not FQuery.EOF do
    begin
      PairKey := IntToStr(FQuery.Fields[0].AsInteger) + #1 +
        IntToStr(FQuery.Fields[1].AsInteger);
      CountIndex := FBulkTranslationCounts.IndexOf(PairKey);
      if CountIndex < 0 then
      begin
        ErrorMessage := Format(
          SAssociationMemoryUnexpectedPhraseIdsFmt,
          [FQuery.Fields[0].AsInteger, FQuery.Fields[1].AsInteger]);
        Exit(False);
      end;

      ExpectedCount := PtrInt(FBulkTranslationCounts.Objects[CountIndex]);
      StoredCount := FQuery.Fields[2].AsInteger;
      if StoredCount <> ExpectedCount then
      begin
        ErrorMessage := Format(
          SAssociationMemoryPhraseIdMismatchFmt,
          [FQuery.Fields[0].AsInteger, FQuery.Fields[1].AsInteger,
           ExpectedCount, StoredCount]);
        Exit(False);
      end;

      Inc(StoredPairCount);
      if (StoredPairCount mod 500) = 0 then
        ReportRebuildProgress(amrpValidating, StoredPairCount,
          TotalExpectedCount);
      FQuery.Next;
    end;

    if StoredPairCount <> FBulkTranslationCounts.Count then
    begin
      ErrorMessage := Format(
        SAssociationMemoryMissingTranslationPairsFmt,
        [FBulkTranslationCounts.Count, StoredPairCount]);
      Exit(False);
    end;
  finally
    FQuery.Close;
  end;

  StoredOccurrenceCount := 0;
  FQuery.Close;
  FQuery.SQL.Text := 'select amo_reference, amo_source, amo_target, ' +
    'amo_occurrences from association_memory_occurrences';
  FQuery.Open;
  try
    while not FQuery.EOF do
    begin
      OccurrenceKey := FQuery.Fields[0].AsString + #1 +
        FQuery.Fields[1].AsString + #1 + FQuery.Fields[2].AsString;
      CountIndex := FBulkOccurrenceCounts.IndexOf(OccurrenceKey);
      if CountIndex < 0 then
      begin
        ErrorMessage := Format(SAssociationMemoryUnexpectedOccurrenceFmt,
          [FQuery.Fields[0].AsString, FQuery.Fields[1].AsString,
           FQuery.Fields[2].AsString]);
        Exit(False);
      end;
      ExpectedCount := PtrInt(FBulkOccurrenceCounts.Objects[CountIndex]);
      StoredCount := FQuery.Fields[3].AsInteger;
      if StoredCount <> ExpectedCount then
      begin
        ErrorMessage := Format(SAssociationMemoryOccurrenceCountMismatchFmt,
          [FQuery.Fields[0].AsString, FQuery.Fields[1].AsString,
           FQuery.Fields[2].AsString, ExpectedCount, StoredCount]);
        Exit(False);
      end;
      Inc(StoredOccurrenceCount);
      if (StoredOccurrenceCount mod 500) = 0 then
        ReportRebuildProgress(amrpValidating,
          FBulkTranslationCounts.Count + StoredOccurrenceCount,
          TotalExpectedCount);
      FQuery.Next;
    end;
    if StoredOccurrenceCount <> FBulkOccurrenceCounts.Count then
    begin
      ErrorMessage := Format(SAssociationMemoryMissingOccurrencesFmt,
        [FBulkOccurrenceCounts.Count, StoredOccurrenceCount]);
      Exit(False);
    end;
  finally
    FQuery.Close;
  end;

  ReportRebuildProgress(amrpValidating, TotalExpectedCount,
    TotalExpectedCount);
  // Every stored translation and occurrence was compared above with the
  // complete buffers used to build it. Running Validate here repeated several
  // whole-database scans, including an aggregate join, after progress had
  // already reached 100%, blocking the UI on large projects.
  Result := True;
end;

function TSuggestionManager.ValidatePairs(ExpectedPairs: TStrings;
  out ErrorMessage: string): Boolean;
var
  ExpectedCounts: TStringList;
  PairIndex: Integer;
  CountIndex: Integer;
  ExpectedCount: Integer;
  PairKey: string;
  SourcePhrase: string;
  TargetPhrase: string;
begin
  Result := Validate(ErrorMessage);
  if not Result then
    Exit;

  if not Assigned(ExpectedPairs) or ((ExpectedPairs.Count mod 2) <> 0) then
  begin
    ErrorMessage := SExpectedAssociationPairListInvalid;
    Exit(False);
  end;

    ExpectedCounts := TStringList.Create;
  try
    ExpectedCounts.Sorted := True;
    ExpectedCounts.CaseSensitive := True;
    ExpectedCounts.UseLocale := False;
    for PairIndex := 0 to ExpectedPairs.Count - 1 do
      if (PairIndex mod 2) = 0 then
      begin
        SourcePhrase := ExpectedPairs[PairIndex].ToLower;
        TargetPhrase := ExpectedPairs[PairIndex + 1].ToLower;
        PairKey := SourcePhrase + #1 + TargetPhrase;
        CountIndex := ExpectedCounts.IndexOf(PairKey);
        if CountIndex < 0 then
          ExpectedCounts.AddObject(PairKey, TObject(PtrInt(1)))
        else
          ExpectedCounts.Objects[CountIndex] := TObject(
            PtrInt(ExpectedCounts.Objects[CountIndex]) + 1);
      end;

    FQuery.Close;
    FQuery.SQL.Text := 'select o.lori_locucao, d.ldes_locucao, ' +
      't.trad_ocorrencias from traducoes t ' +
      'join locucoes_origem o on o.lori_id = t.trad_lori_id ' +
      'join locucoes_destino d on d.ldes_id = t.trad_ldes_id';
    FQuery.Open;
    while not FQuery.EOF do
    begin
      SourcePhrase := FQuery.Fields[0].AsString;
      TargetPhrase := FQuery.Fields[1].AsString;
      PairKey := SourcePhrase + #1 + TargetPhrase;
      CountIndex := ExpectedCounts.IndexOf(PairKey);
      if CountIndex < 0 then
      begin
        ErrorMessage := Format(
          SAssociationMemoryUnexpectedPairFmt,
          [SourcePhrase, TargetPhrase]);
        Exit(False);
      end;
      ExpectedCount := PtrInt(ExpectedCounts.Objects[CountIndex]);
      if FQuery.Fields[2].AsInteger <> ExpectedCount then
      begin
        ErrorMessage := Format(
          SAssociationMemoryPairMismatchFmt,
          [SourcePhrase, TargetPhrase, ExpectedCount,
           FQuery.Fields[2].AsInteger]);
        Exit(False);
      end;
      ExpectedCounts.Delete(CountIndex);
      FQuery.Next;
    end;
    if ExpectedCounts.Count > 0 then
    begin
      ErrorMessage := SAssociationMemoryMissingExpectedPairs;
      Exit(False);
    end;
  finally
    FQuery.Close;
    ExpectedCounts.Free;
  end;
  Result := True;
end;

function TSuggestionManager.GetPairOccurrenceCount(const Source,
  Target: string): Integer;
var
  SourcePhraseId: Integer;
  TargetPhraseId: Integer;
begin
  Result := 0;
  SourcePhraseId := FindSourcePhraseId(Source.ToLower);
  TargetPhraseId := FindTargetPhraseId(Target.ToLower);
  if (SourcePhraseId <= 0) or (TargetPhraseId <= 0) then
    Exit;

  FGetTranslationCountQuery.Close;
  FGetTranslationCountQuery.Params.ParamByName('lori').AsInteger :=
    SourcePhraseId;
  FGetTranslationCountQuery.Params.ParamByName('ldes').AsInteger :=
    TargetPhraseId;
  FGetTranslationCountQuery.Open;
  try
    if not FGetTranslationCountQuery.EOF then
      Result := FGetTranslationCountQuery.Fields[0].AsInteger;
  finally
    FGetTranslationCountQuery.Close;
  end;
end;

function TSuggestionManager.ValidatePairConsistency(const Source,
  Target: string; ExpectedCount: Integer; out ErrorMessage: string): Boolean;
var
  ActualCount: Integer;
  InvalidOccurrenceRows: Integer;
  NormalizedSource: string;
  NormalizedTarget: string;
  OrphanReferences: Integer;
  ReferenceOccurrences: Integer;
begin
  Result := False;
  ErrorMessage := '';
  NormalizedSource := Source.ToLower;
  NormalizedTarget := Target.ToLower;

  ActualCount := GetPairOccurrenceCount(NormalizedSource, NormalizedTarget);
  if ActualCount <> ExpectedCount then
  begin
    ErrorMessage := Format(SAssociationMemoryPairMismatchFmt,
      [NormalizedSource, NormalizedTarget, ExpectedCount, ActualCount]);
    Exit;
  end;

  FQuery.Close;
  FQuery.SQL.Text :=
    'select coalesce(sum(o.amo_occurrences), 0), ' +
    'coalesce(sum(case when o.amo_occurrences <= 0 then 1 else 0 end), 0), ' +
    'coalesce(sum(case when o.amo_reference <> '''' and not exists (' +
      'select 1 from pares p where p.pare_id = o.amo_reference' +
    ') then 1 else 0 end), 0) ' +
    'from association_memory_occurrences o where ' +
    'o.amo_source = :source and o.amo_target = :target';
  FQuery.Params.ParamByName('source').AsString := NormalizedSource;
  FQuery.Params.ParamByName('target').AsString := NormalizedTarget;
  FQuery.Open;
  try
    ReferenceOccurrences := FQuery.Fields[0].AsInteger;
    InvalidOccurrenceRows := FQuery.Fields[1].AsInteger;
    OrphanReferences := FQuery.Fields[2].AsInteger;
  finally
    FQuery.Close;
  end;

  if InvalidOccurrenceRows > 0 then
  begin
    ErrorMessage := SAssociationMemoryNonPositiveOccurrenceReferences;
    Exit;
  end;
  if OrphanReferences > 0 then
  begin
    ErrorMessage := SAssociationMemoryOrphanOccurrenceReferences;
    Exit;
  end;
  if ReferenceOccurrences <> ExpectedCount then
  begin
    ErrorMessage := Format(SAssociationMemoryPairMismatchFmt,
      [NormalizedSource, NormalizedTarget, ExpectedCount,
       ReferenceOccurrences]);
    Exit;
  end;
  Result := True;
end;

function TSuggestionManager.GetTranslationAlternatives(Syntagm: TSyntagm): string;
var
  Alternatives: TTranslationAlternativeDataArray;
  Alternative: TTranslationAlternativeData;
  Phrase: string;
  Key: string;
begin
  Result := '';
  Key := Syntagm.GetSuggestionKey(plMetaData);
  if Key.IsEmpty then
    Exit;

  Alternatives := GetTranslationAlternativeData(
    Syntagm.GetSuggestionKey(plStrong));
  for Alternative in Alternatives do
  begin
    Phrase := AnsiReplaceStr(Alternative.Translation, ';', ' ') +
              Format(' (%d)', [Alternative.Occurrences]);
    Result := Format('%s\par\li0 {\b\''95  } %s', [Result, UnicodeToRtf(Phrase)]);
  end;
end;

function TSuggestionManager.GetTranslationAlternativeData(
  const Key: string): TTranslationAlternativeDataArray;
var
  Suggestions: TSQLQuery;
  CacheIndex: Integer;
  CacheItem: TTranslationAlternativeCacheItem;
begin
  Result := nil;
  if Key.IsEmpty then
    Exit;

  CacheIndex := FTranslationAlternativeCache.IndexOf(Key);
  if CacheIndex >= 0 then
  begin
    Result := TTranslationAlternativeCacheItem(
      FTranslationAlternativeCache.Objects[CacheIndex]).Data;
    Exit;
  end;

  if FTranslationAlternativeCache.Count >=
     TRANSLATION_ALTERNATIVE_CACHE_LIMIT then
    ClearTranslationAlternativeCache;

  CacheItem := TTranslationAlternativeCacheItem.Create;
  Suggestions := nil;
  try
    Suggestions := GetTranslationMemoryQuery(Key);
    while not Suggestions.EOF do
    begin
      AddTranslationAlternative(Result,
        Suggestions.Fields[0].AsString,
        Suggestions.Fields[1].AsString,
        Suggestions.Fields[2].AsInteger);
      Suggestions.Next;
    end;
    SortTranslationAlternatives(Result);
    CacheItem.Data := Result;
    FTranslationAlternativeCache.AddObject(Key, CacheItem);
    CacheItem := nil;
  finally
    if Assigned(Suggestions) then
      Suggestions.Close;
    CacheItem.Free;
  end;
end;

function TSuggestionManager.GetTranslationOccurrences(
  const Matches: TTranslationPairMatchArray;
  Limit: Integer): TTranslationOccurrencePage;
var
  InsertQuery: TSQLQuery;
  Match: TTranslationPairMatch;
  Query: TSQLQuery;
  ItemIndex: Integer;
begin
  Result := Default(TTranslationOccurrencePage);
  if (Length(Matches) = 0) or (Limit <= 0) then
    Exit;

  FConnection.ExecuteDirect('delete from association_memory_match_filter');
  InsertQuery := TSQLQuery.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    InsertQuery.DataBase := FConnection;
    InsertQuery.SQL.Text := 'insert or ignore into ' +
      'association_memory_match_filter (amo_source, amo_target) ' +
      'values (:source, :target)';
    InsertQuery.Prepare;
    for Match in Matches do
    begin
      InsertQuery.Params.ParamByName('source').AsString :=
        Match.SourcePhrase;
      InsertQuery.Params.ParamByName('target').AsString :=
        Match.TargetPhrase;
      InsertQuery.ExecSQL;
    end;

    Query.DataBase := FConnection;
    Query.SQL.Text := 'select coalesce(sum(o.amo_occurrences), 0), ' +
      'count(distinct o.amo_reference) ' +
      'from association_memory_occurrences o ' +
      'join association_memory_match_filter f on ' +
      'f.amo_source = o.amo_source and f.amo_target = o.amo_target ' +
      'join pares p on p.pare_id = o.amo_reference';
    Query.Open;
    Result.TotalAssociations := Query.Fields[0].AsInteger;
    Result.TotalVerses := Query.Fields[1].AsInteger;
    Query.Close;

    Query.SQL.Text := 'select o.amo_reference, ' +
      'sum(o.amo_occurrences), p.pare_texto_destino ' +
      'from association_memory_occurrences o ' +
      'join association_memory_match_filter f on ' +
      'f.amo_source = o.amo_source and f.amo_target = o.amo_target ' +
      'join pares p on p.pare_id = o.amo_reference ' +
      'group by p.rowid, o.amo_reference, p.pare_texto_destino ' +
      'order by p.rowid limit :limit';
    Query.Params.ParamByName('limit').AsInteger := Limit;
    Query.Open;
    while not Query.EOF do
    begin
      ItemIndex := Length(Result.Items);
      SetLength(Result.Items, ItemIndex + 1);
      Result.Items[ItemIndex].ReferenceID := Query.Fields[0].AsString;
      Result.Items[ItemIndex].Occurrences := Query.Fields[1].AsInteger;
      Result.Items[ItemIndex].DestinationText := Query.Fields[2].AsString;
      Query.Next;
    end;
    Result.HasMore := Length(Result.Items) < Result.TotalVerses;
  finally
    Query.Free;
    InsertQuery.Free;
    FConnection.ExecuteDirect('delete from association_memory_match_filter');
  end;
end;

function TSuggestionManager.IndexedConcordanceOccurrenceCount: Integer;
begin
  FQuery.Close;
  FQuery.SQL.Text := 'select count(*) from association_memory_occurrences o ' +
    'join pares p on p.pare_id = o.amo_reference';
  FQuery.Open;
  try
    Result := FQuery.Fields[0].AsInteger;
  finally
    FQuery.Close;
  end;
end;

function TSuggestionManager.IndexedConcordanceHasTaggedTargets: Boolean;
begin
  FQuery.Close;
  FQuery.SQL.Text := 'select 1 from association_memory_occurrences ' +
    'where instr(lower(amo_target), ''<wg'') > 0 or ' +
    'instr(lower(amo_target), ''<wh'') > 0 or ' +
    'instr(lower(amo_target), ''<wt'') > 0 limit 1';
  FQuery.Open;
  try
    Result := not FQuery.EOF;
  finally
    FQuery.Close;
  end;
end;

procedure TSuggestionManager.EnumerateIndexedConcordanceOccurrences(
  Handler: TConcordanceOccurrenceEvent);
var
  Data: TConcordanceOccurrenceData;
  Query: TSQLQuery;
begin
  if not Assigned(Handler) then
    Exit;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    Query.SQL.Text := 'select o.amo_reference, o.amo_source, ' +
      'o.amo_target, p.pare_texto_destino, o.amo_occurrences, p.rowid ' +
      'from association_memory_occurrences o ' +
      'join pares p on p.pare_id = o.amo_reference ' +
      'order by p.rowid, o.amo_source, o.amo_target';
    Query.Open;
    while not Query.EOF do
    begin
      Data.ReferenceID := Query.Fields[0].AsString;
      Data.SourcePhrase := Query.Fields[1].AsString;
      Data.TargetPhrase := Query.Fields[2].AsString;
      Data.DestinationText := Query.Fields[3].AsString;
      Data.Occurrences := Query.Fields[4].AsInteger;
      Data.ReferenceOrder := Query.Fields[5].AsInteger;
      Handler(Data);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TSuggestionManager.GetUnindexedAssociatedReferences: TStringList;
var
  Query: TSQLQuery;
begin
  Result := TStringList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    Query.SQL.Text := 'select p.pare_id from pares p ' +
      'where trim(coalesce(p.pare_pares, '''')) <> '''' and not exists (' +
      'select 1 from association_memory_occurrences o ' +
      'where o.amo_reference = p.pare_id) order by p.rowid';
    Query.Open;
    while not Query.EOF do
    begin
      Result.Add(Query.Fields[0].AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TSuggestionManager.PreloadTranslationAlternativeData(Keys: TStrings);
var
  CacheIndex: Integer;
  CacheItem: TTranslationAlternativeCacheItem;
  Key: string;
  KeyIndex: Integer;
  MissingKeys: TStringList;
  Query: TSQLQuery;
  StoredSuggestionKey: string;
  SQLText: string;
begin
  if not Assigned(Keys) or (Keys.Count = 0) then
    Exit;

  if FTranslationAlternativeCache.Count + Keys.Count >
     TRANSLATION_ALTERNATIVE_CACHE_LIMIT then
    ClearTranslationAlternativeCache;

  MissingKeys := TStringList.Create;
  try
    MissingKeys.Sorted := True;
    MissingKeys.CaseSensitive := True;
    MissingKeys.UseLocale := False;
    MissingKeys.Duplicates := dupIgnore;
    for Key in Keys do
      if (Key <> '') and
         (FTranslationAlternativeCache.IndexOf(Key) < 0) then
        MissingKeys.Add(Key);
    if MissingKeys.Count = 0 then
      Exit;

    try
      for KeyIndex := 0 to MissingKeys.Count - 1 do
        FTranslationAlternativeCache.AddObject(MissingKeys[KeyIndex],
          TTranslationAlternativeCacheItem.Create);

      Query := TSQLQuery.Create(nil);
      try
        Query.DataBase := FConnection;
        SQLText := 'select o.lori_locucao, d.ldes_locucao, ' +
          't.trad_ocorrencias, cast(s.slor_sori_id as text) ' +
          'from sintagmas_locucoes_origem s ' +
          'join traducoes t on t.trad_lori_id = s.slor_lori_id ' +
          'join locucoes_origem o on o.lori_id = s.slor_lori_id ' +
          'join locucoes_destino d on d.ldes_id = t.trad_ldes_id ' +
          'where s.slor_ordem = 0 and t.trad_ocorrencias > 0 and (';
        for KeyIndex := 0 to MissingKeys.Count - 1 do
        begin
          if KeyIndex > 0 then
            SQLText := SQLText + ' or ';
          SQLText := SQLText + '(s.slor_sori_id = :key' +
            IntToStr(KeyIndex) + ' or (' +
            'substr(s.slor_sori_id, 1, length(:key_prefix' +
            IntToStr(KeyIndex) + ')) = :key_prefix' +
            IntToStr(KeyIndex) + ' and ' +
            'substr(s.slor_sori_id, length(:key_prefix' +
            IntToStr(KeyIndex) + ') + 1, 1) = ''<''))';
        end;
        Query.SQL.Text := SQLText + ') order by t.trad_ocorrencias desc, ' +
          'd.ldes_locucao, o.lori_locucao';
        for KeyIndex := 0 to MissingKeys.Count - 1 do
        begin
          Query.Params.ParamByName('key' + IntToStr(KeyIndex)).AsString :=
            MissingKeys[KeyIndex];
          Query.Params.ParamByName(
            'key_prefix' + IntToStr(KeyIndex)).AsString :=
            MissingKeys[KeyIndex];
        end;
        Query.Open;
        while not Query.EOF do
        begin
          StoredSuggestionKey := Query.Fields[3].AsString;
          for KeyIndex := 0 to MissingKeys.Count - 1 do
            if SuggestionKeyMatches(MissingKeys[KeyIndex],
              StoredSuggestionKey) then
            begin
              CacheIndex := FTranslationAlternativeCache.IndexOf(
                MissingKeys[KeyIndex]);
              CacheItem := TTranslationAlternativeCacheItem(
                FTranslationAlternativeCache.Objects[CacheIndex]);
              AddTranslationAlternative(CacheItem.Data,
                Query.Fields[0].AsString, Query.Fields[1].AsString,
                Query.Fields[2].AsInteger);
            end;
          Query.Next;
        end;

        for KeyIndex := 0 to MissingKeys.Count - 1 do
        begin
          CacheIndex := FTranslationAlternativeCache.IndexOf(
            MissingKeys[KeyIndex]);
          CacheItem := TTranslationAlternativeCacheItem(
            FTranslationAlternativeCache.Objects[CacheIndex]);
          SortTranslationAlternatives(CacheItem.Data);
        end;
      finally
        Query.Free;
      end;
    except
      ClearTranslationAlternativeCache;
      raise;
    end;
  finally
    MissingKeys.Free;
  end;
end;

end.
