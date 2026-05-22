unit SuggestionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sqldb, sqlite3conn, Verse,
  Dialogs, LazLogger, PCRE, Syntagm, ONTTokenizer, ExportarProjeto;

type

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
    FGetNextSourcePhraseIdQuery: TSQLQuery;
    FGetNextTargetPhraseIdQuery: TSQLQuery;
    FInsertSourcePhraseQuery: TSQLQuery;
    FInsertTargetPhraseQuery: TSQLQuery;
    FInsertSourcePhraseSyntagmQuery: TSQLQuery;
    FInsertTargetPhraseSyntagmQuery: TSQLQuery;
    FInsertTranslationQuery: TSQLQuery;
    FUpdateTranslationQuery: TSQLQuery;
    FSuggestionsQuery: TSQLQuery;
    FSuggestionsWithoutMorphologyQuery: TSQLQuery;
    FTranslationMemoryQuery: TSQLQuery;
    FDelimitedValues: TStringList;
    FVerbList: TStringList;
    FRecognizeVerbForms: Boolean;
    FRemoveMorphologyRegex: IRegex;
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
  public
    constructor Create(Owner: TComponent; Connection: TSQLConnection); overload;
    constructor Create(const DatabasePath: string); overload;
    destructor Destroy; override;
    property RecognizeVerbForms: Boolean read FRecognizeVerbForms write FRecognizeVerbForms;
    procedure AddPair(const Source, Target: string);
    procedure RemovePair(const Source, Target: string);
    procedure SuggestAssociations(SourceVerse: TVerse);
    procedure ClearAssociationDatabase;
    procedure Commit;
    procedure Rollback;
    function GetTranslationAlternatives(Syntagm: TSyntagm): string;
  end;

implementation

constructor TSuggestionManager.Create(Owner: TComponent; Connection: TSQLConnection);
begin
  FConnection := Connection;
  FQuery := TSQLQuery.Create(Owner);
  FQuery.DataBase := FConnection;

  FRemovePairQuery := TSQLQuery.Create(Owner);
  FRemovePairQuery.DataBase := FConnection;
  FRemovePairQuery.SQL.Text := 'update traducoes set trad_ocorrencias = trad_ocorrencias - 1 where trad_lori_id = :lori and trad_ldes_id = :ldes';
  FRemovePairQuery.Prepare;

  FGetSourcePhraseQuery := TSQLQuery.Create(Owner);
  FGetSourcePhraseQuery.DataBase := FConnection;
  FGetSourcePhraseQuery.SQL.Text := 'select lori_id from locucoes_origem where lori_locucao = :locucao';
  FGetSourcePhraseQuery.Prepare;

  FGetTargetPhraseQuery := TSQLQuery.Create(Owner);
  FGetTargetPhraseQuery.DataBase := FConnection;
  FGetTargetPhraseQuery.SQL.Text := 'select ldes_id from locucoes_destino where ldes_locucao = :locucao';
  FGetTargetPhraseQuery.Prepare;

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

  FUpdateTranslationQuery := TSQLQuery.Create(Owner);
  FUpdateTranslationQuery.DataBase := FConnection;
  FUpdateTranslationQuery.SQL.Text := 'update traducoes set trad_ocorrencias = trad_ocorrencias + 1 where trad_lori_id = :lori and trad_ldes_id = :ldes';
  FUpdateTranslationQuery.Prepare;

  FSuggestionsQuery := TSQLQuery.Create(Owner);
  FSuggestionsQuery.DataBase := FConnection;
  FSuggestionsQuery.SQL.Text := 'select lori_locucao, ldes_locucao ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where slor_sori_id = :sori and slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id ' +
      'order by lori_qtde_sintagmas desc, trad_ocorrencias desc';
  FSuggestionsQuery.Prepare;

  FSuggestionsWithoutMorphologyQuery := TSQLQuery.Create(Owner);
  FSuggestionsWithoutMorphologyQuery.DataBase := FConnection;
  FSuggestionsWithoutMorphologyQuery.SQL.Text := 'select lori_locucao, ldes_locucao ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where slor_sori_id like :sori and slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id ' +
      'order by lori_qtde_sintagmas desc, trad_ocorrencias desc';
  FSuggestionsWithoutMorphologyQuery.Prepare;

  FTranslationMemoryQuery := TSQLQuery.Create(Owner);
  FTranslationMemoryQuery.DataBase := FConnection;
  FTranslationMemoryQuery.SQL.Text := 'select ldes_locucao, count(*) ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where slor_sori_id like :sori and slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id ' +
      'group by ldes_locucao order by 2 desc';
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
  FGetNextSourcePhraseIdQuery.Free;
  FGetNextTargetPhraseIdQuery.Free;
  FInsertSourcePhraseQuery.Free;
  FInsertTargetPhraseQuery.Free;
  FInsertSourcePhraseSyntagmQuery.Free;
  FInsertTargetPhraseSyntagmQuery.Free;
  FInsertTranslationQuery.Free;
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
  inherited Destroy;
end;

procedure TSuggestionManager.AddPair(const Source, Target: string);
var
  SourcePhraseId: Integer;
  TargetPhraseId: Integer;
begin
  SourcePhraseId := GetOrCreateSourcePhraseId(Source.ToLower);
  TargetPhraseId := GetOrCreateTargetPhraseId(Target.ToLower);

  if not UpdateTranslation(SourcePhraseId, TargetPhraseId) then
    InsertTranslation(SourcePhraseId, TargetPhraseId);
end;

procedure TSuggestionManager.RemovePair(const Source, Target: string);
var
  SourcePhraseId: Integer;
  TargetPhraseId: Integer;
begin
  SourcePhraseId := GetOrCreateSourcePhraseId(Source.ToLower);
  TargetPhraseId := GetOrCreateTargetPhraseId(Target.ToLower);

  if (SourcePhraseId > 0) and (TargetPhraseId > 0) then
  begin
    FRemovePairQuery.Close;
    FRemovePairQuery.Params[0].AsInteger := SourcePhraseId;
    FRemovePairQuery.Params[1].AsInteger := TargetPhraseId;
    FRemovePairQuery.ExecSQL;
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
    FSuggestionsWithoutMorphologyQuery.Params[0].AsString := KeyWithoutMorphology + '%';
    FSuggestionsWithoutMorphologyQuery.Open;
    DebugLn('%d translation suggestions found without morphology', [FSuggestionsWithoutMorphologyQuery.RecordCount]);
  end;
end;

function TSuggestionManager.GetTranslationMemoryQuery(const Key: string): TSQLQuery;
begin
  Result := FTranslationMemoryQuery;
  FTranslationMemoryQuery.Close;
  FTranslationMemoryQuery.Params[0].AsString := Key + '%';
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
  with FQuery do
  begin
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
end;

procedure TSuggestionManager.Commit;
begin
  FConnection.Transaction.Commit;
  FConnection.StartTransaction;
end;

procedure TSuggestionManager.Rollback;
begin
  FConnection.Transaction.Rollback;
  FConnection.StartTransaction;
end;

function TSuggestionManager.GetTranslationAlternatives(Syntagm: TSyntagm): string;
var
  Suggestions: TSQLQuery;
  Phrase: string;
  Key: string;
begin
  Result := '';
  Key := Syntagm.GetSuggestionKey(plMetaData);
  if Key.IsEmpty then
    Exit;

  Suggestions := GetTranslationMemoryQuery(Syntagm.GetSuggestionKey(plStrong));
  while not Suggestions.EOF do
  begin
    Phrase := AnsiReplaceStr(Suggestions.Fields[0].AsString, ';', ' ') +
              Format(' (%d)', [Suggestions.Fields[1].AsInteger]);
    Result := Format('%s\par\li0 {\b\''95  } %s', [Result, UnicodeToRTF(Phrase)]);
    Suggestions.Next;
  end;
  Suggestions.Close;
end;

end.