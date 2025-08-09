unit AIAssociator;

interface

uses
  Classes, SysUtils, llmclient, versiculo, fpjson, jsonparser, Dialogs, ONTParser, ONTTokenizer, Syntagm;

type
  // Forward declaration if TPair is not globally available or defined later
  generic TPair<K, V> = class
  public
    Key: K;
    Value: V;
    constructor Create(AKey: K; AValue: V);
  end;

  TAiAssociator = class
  protected
    function ParsePhraseToTexts(Phrase: string; VerseOwner: TVersiculo): TStringList;
    procedure ProcessSingleSuggestion(OriginalVerse, TranslatedVerse: TVersiculo; const OriginalPhrase, TranslatedPhrase: string);
    procedure AssociateVerseFromSuggestions(OriginalVerse: TVersiculo; LLMResponseJson: string);
  public
    constructor Create(AClient: TBaseLLMClient; APrompt: string);
    constructor Create(const Model, ApiKey: string; APrompt: string);
    destructor Destroy; override;
    procedure AssociateVerse(OriginalVerse: TVersiculo);
  private
    FClient: TBaseLLMClient;
    FPrompt: string;
    FPairsToAssociate: TList; // Will store TPair<TSyntagmList, TSyntagmList>
  end;

implementation

{ TPair<K, V> }
constructor TPair.Create(AKey: K; AValue: V); // Removed <K, V> from here
begin
  Key := AKey;
  Value := AValue;
end;

{ TAiAssociator }

constructor TAiAssociator.Create(AClient: TBaseLLMClient; APrompt: string);
begin
  inherited Create;
  if not Assigned(AClient) then
    raise Exception.Create('AClient must not be nil.');
  if Trim(APrompt) = '' then
    raise Exception.Create('APrompt must not be empty.');
  FClient := AClient;
  FPrompt := APrompt;
  FPairsToAssociate := TList.Create; // Owns objects by default, need to manage TSyntagmList pairs carefully
end;

constructor TAiAssociator.Create(const Model, ApiKey: string; APrompt: string);
begin
  Create(CreateBackend(Model, ApiKey), APrompt);
end;

destructor TAiAssociator.Destroy;
var
  I: Integer;
  Pair: specialize TPair<TSyntagmList, TSyntagmList>;
begin
  for I := 0 to FPairsToAssociate.Count - 1 do
  begin
    Pair := specialize TPair<TSyntagmList, TSyntagmList>(FPairsToAssociate[I]); // Use specialize only in assignment/cast
    Pair.Key.Free;   // Free the TSyntagmList itself
    Pair.Value.Free; // Free the TSyntagmList itself
    Pair.Free;       // Free the TPair object
  end;
  FPairsToAssociate.Free;
  inherited Destroy;
end;

function TAiAssociator.ParsePhraseToTexts(Phrase: string; VerseOwner: TVersiculo): TStringList;
var
  Parser: TONTParser;
  ParsedSyntagms: TSyntagmList;
  J: Integer;
begin
  Result := TStringList.Create;
  if Trim(Phrase) = '' then Exit;

  Parser := TONTParser.Create;
  try
    ParsedSyntagms := Parser.ParseLine(Phrase, VerseOwner);
    try
      for J := 0 to ParsedSyntagms.Count - 1 do
      begin
        if ParsedSyntagms[J].Kind in [ONTTokenizer.tsSintagma] then // Qualified tsSintagma
          Result.Add(ParsedSyntagms[J].Text);
      end;
    finally
      ParsedSyntagms.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TAiAssociator.ProcessSingleSuggestion(OriginalVerse, TranslatedVerse: TVersiculo; const OriginalPhrase, TranslatedPhrase: string);
var
  OriginalTexts, TranslatedTexts: TStringList;
begin
  if (Trim(OriginalPhrase) = '') or (Trim(TranslatedPhrase) = '') then
    Exit;

  OriginalTexts := ParsePhraseToTexts(OriginalPhrase, OriginalVerse);
  TranslatedTexts := ParsePhraseToTexts(TranslatedPhrase, TranslatedVerse);

  try
    OriginalVerse.FindAndSelectSyntagms(OriginalTexts);
    TranslatedVerse.FindAndSelectSyntagms(TranslatedTexts);
    OriginalVerse.AssociarSintagmas;
  finally
    OriginalTexts.Free;
    TranslatedTexts.Free;
  end;
end;

procedure TAiAssociator.AssociateVerse(OriginalVerse: TVersiculo);
var
  TranslatedVerse: TVersiculo;
  LLMResponseJson: string;
  Messages: array of TLLMMessage = nil; // Initialize to nil to silence hint
begin
  TranslatedVerse := OriginalVerse.VersiculoPar;
  if not Assigned(TranslatedVerse) then
  begin
    ShowMessage('Error: Translated verse (VersiculoPar) is not assigned.');
    Exit;
  end;
  if not Assigned(FClient) then
  begin
    ShowMessage('Error: LLM Client (FClient) is not assigned.');
    Exit;
  end;
  SetLength(Messages, 2);
  Messages[0].Role := 'system';
  Messages[0].Content := FPrompt;
  Messages[1].Role := 'user';
  Messages[1].Content := format('Original: "%s"'#13#10'Translated: "%s"', [OriginalVerse.TextoSimples, TranslatedVerse.TextoSimples]);
  // Use model name directly (replace with your actual model if needed)
  LLMResponseJson := FClient.AskChatCompletions(Messages).Content;
  AssociateVerseFromSuggestions(OriginalVerse, LLMResponseJson);
end;

procedure TAiAssociator.AssociateVerseFromSuggestions(OriginalVerse: TVersiculo; LLMResponseJson: string);
var
  TranslatedVerse: TVersiculo;
  JsonData: TJSONData;
  JsonParserObj: TJSONParser;
  SuggestionsArray: TJSONArray;
  LLMComments: string;
  I: Integer;
  PairArray: TJSONArray;
  OriginalPhrase, TranslatedPhrase: string;
begin
  TranslatedVerse := OriginalVerse.VersiculoPar;

  JsonParserObj := TJSONParser.Create(LLMResponseJson, []); // Use options form to silence deprecation warning
  try
    JsonData := JsonParserObj.Parse;
    if not Assigned(JsonData) or not (JsonData is TJSONObject) then
    begin
      ShowMessage('Error: LLM response is not a valid JSON object.');
      Exit;
    end;

    SuggestionsArray := (JsonData as TJSONObject).Get('data', TJSONArray(nil)) as TJSONArray;
    if (JsonData as TJSONObject).Find('comments') <> nil then
      LLMComments := (JsonData as TJSONObject).Get('comments', '')
    else
      LLMComments := '';

    if not Assigned(SuggestionsArray) then
    begin
      ShowMessage('Error: "data" field missing or not an array in LLM response.');
      Exit;
    end;

    for I := 0 to SuggestionsArray.Count - 1 do
    begin
      OriginalVerse.LimparSelecao;
      TranslatedVerse.LimparSelecao;
      if not (SuggestionsArray.Items[I] is TJSONArray) then continue;
      PairArray := SuggestionsArray.Items[I] as TJSONArray;
      if PairArray.Count <> 2 then continue;
      if not (PairArray.Items[0] is TJSONString) or not (PairArray.Items[1] is TJSONString) then continue;
      OriginalPhrase := (PairArray.Items[0] as TJSONString).Value;
      TranslatedPhrase := (PairArray.Items[1] as TJSONString).Value;
      ProcessSingleSuggestion(OriginalVerse, TranslatedVerse, OriginalPhrase, TranslatedPhrase);
    end;

    // Further processing, e.g., XML generation, can be added here
    if LLMComments <> '' then ShowMessageFmt('LLM Comments: %s', [LLMComments]);
  finally
    JsonParserObj.Free;
    OriginalVerse.LimparSelecao;
    TranslatedVerse.LimparSelecao;
  end;
end;

end.
