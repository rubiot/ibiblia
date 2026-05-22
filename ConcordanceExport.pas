unit ConcordanceExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, contnrs, strutils, PCRE, LazUTF8;

function UnicodeToRtf(s: string): string;
function MakeStrongLink(strong: string; anchor: string): string;
function MakeMorphologyLink(morphology: string; anchor: string): string;

type
  TConcordance = class;

  TConcordancePhrase = class(TObject)
  private
    FParent: TConcordance;
    FPhrase: string;
    FTranslationHash: TFPHashList;
    function GetKey(Index: Integer): string;
    function GetFormattedKey(Index: Integer): string;
    function GetItem(Index: Integer): TStringList;
    function GetCount: Integer;
    function GetReferences(Key: string): TStringList;
    procedure SetReferences(Key: string; const AValue: TStringList);
  public
    constructor Create(AParent: TConcordance; const APhrase: string);
    destructor Destroy; override;
    function GetPhraseRtf: string;
    property References[Key: string]: TStringList read GetReferences write SetReferences; default;
    property Keys[Index: Integer]: string read GetKey;
    property FormattedKeys[Index: Integer]: string read GetFormattedKey;
    property Items[Index: Integer]: TStringList read GetItem;
    property Count: Integer read GetCount;
    property Phrase: string read FPhrase;
  end;

  TConcordance = class(TObject)
  private
    FPhraseHash: TFPHashList;
    FDetailed: Boolean;
    FRemoveMorphologyRegex: IRegex;
    FUnicodeCharsRegex: IRegex;
    FSyntagmRegex: IRegex;
    FTagRegex: IRegex;
    function GetKey(Index: Integer): string;
    function GetItem(Index: Integer): TConcordancePhrase;
    function GetPhrase(const Key: string): TConcordancePhrase;
    function GetCount: Integer;
    procedure SetPhrase(const Key: string; const AValue: TConcordancePhrase);
  public
    constructor Create;
    destructor Destroy; override;
    function GetStrongRtf(const Lemma: string; const Strong: string): string;
    property Keys[Index: Integer]: string read GetKey;
    property Items[Index: Integer]: TConcordancePhrase read GetItem;
    property Count: Integer read GetCount;
    property Detailed: Boolean read FDetailed write FDetailed;
    procedure AddPhrase(Pairs: TStringList; const Ref: string);
    procedure Clear;
    function AsString: string;
  end;

implementation

function UnicodeToRtf(s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: Integer;
begin
  Result := '';
  p := PChar(s);
  repeat
    unicode := UTF8CodepointToUnicode(p, CharLen);
    if unicode > 127 then
      Result := Result + Format('\u%d?', [unicode])
    else if unicode > 0 then
      Result := Result + Chr(unicode);
    Inc(p, CharLen);
  until (CharLen = 0) or (unicode = 0);
end;

function MakeStrongLink(strong: string; anchor: string): string;
var
  font: string;
begin
  if RegexCreate('\p{Greek}', [rcoUTF8]).Matches(anchor).Count > 0 then
    font := '\f1'
  else if RegexCreate('\p{Hebrew}', [rcoUTF8]).Matches(anchor).Count > 0 then
    font := '\f2'
  else
    font := '\f0';

  Result := Format('{\field{\*\fldinst HYPERLINK "tw://[strong]?%s"}{\fldrslt%s\b %s}}',
    [UpperCase(strong), font, UnicodeToRtf(anchor)]);
end;

function MakeMorphologyLink(morphology: string; anchor: string): string;
begin
  Result := Format('{\field{\*\fldinst HYPERLINK "tw://[morph]?%s"}{\fldrslt\cf6 %s}}',
    [UpperCase(morphology), UnicodeToRtf(anchor)]);
end;

function ExtractText(s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: Integer;
  tag: Boolean;
begin
  Result := '';
  tag := False;
  p := PChar(s);
  repeat
    unicode := UTF8CodepointToUnicode(p, CharLen);
    if not tag and (CharLen = 1) and (Chr(unicode) = '<') then
      tag := True;

    if (unicode > 0) and not tag then
    begin
      if CharLen >= 1 then Result := Result + Chr(Byte(p[0]));
      if CharLen >= 2 then Result := Result + Chr(Byte(p[1]));
      if CharLen >= 3 then Result := Result + Chr(Byte(p[2]));
    end;
    if tag and (CharLen = 1) and (Chr(unicode) = '>') then
      tag := False;

    Inc(p, CharLen);

    if not tag and (unicode = Ord('>')) and (p[0] <> '<') then
      Result := Result + ' ';
  until (CharLen = 0) or (unicode = 0);
end;

function ExtractTags(s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: Integer;
  tag: Boolean;
begin
  Result := '';
  tag := False;
  p := PChar(s);
  repeat
    unicode := UTF8CodepointToUnicode(p, CharLen);
    if not tag and (CharLen = 1) and (Chr(unicode) = '<') then
      tag := True;

    if (unicode > 0) and tag then
    begin
      if CharLen >= 1 then Result := Result + Chr(Byte(p[0]));
      if CharLen >= 2 then Result := Result + Chr(Byte(p[1]));
      if CharLen >= 3 then Result := Result + Chr(Byte(p[2]));
    end;
    if tag and (CharLen = 1) and (Chr(unicode) = '>') then
      tag := False;
    Inc(p, CharLen);
  until (CharLen = 0) or (unicode = 0);
end;

function TConcordance.GetPhrase(const Key: string): TConcordancePhrase;
var
  tags: string;
begin
  tags := ExtractTags(Key);

  Result := TConcordancePhrase(FPhraseHash.Find(tags));
  if Result = nil then
  begin
    Result := TConcordancePhrase.Create(Self, IfThen(FDetailed, Key, tags));
    FPhraseHash.Add(tags, Result);
  end;
end;

function TConcordance.GetCount: Integer;
begin
  Result := FPhraseHash.Count;
end;

function TConcordance.GetItem(Index: Integer): TConcordancePhrase;
begin
  Result := nil;
  if (Index >= 0) and (Index < Self.Count) then
    Result := TConcordancePhrase(FPhraseHash[Index]);
end;

function TConcordance.GetKey(Index: Integer): string;
begin
  Result := '';
  if (Index >= 0) and (Index < Self.Count) then
    Result := FPhraseHash.NameOfIndex(Index);
end;

function TConcordance.GetStrongRtf(const Lemma: string; const Strong: string): string;
var
  phraseIndex: Integer;
  keyIndex: Integer;
  referenceIndex: Integer;
  occurrencesInVerse: Integer;
  totalOccurrences: Integer;
  phrase: TConcordancePhrase;
  currentStrong: string;
  lastReference: string;
begin
  totalOccurrences := 0;
  currentStrong := Format('<W%s>', [Strong]).ToLower;
  Result := MakeStrongLink(Strong, Lemma) + ' (__OCORRENCIAS__)\par';

  for phraseIndex := 0 to FPhraseHash.Count - 1 do
  begin
    if Keys[phraseIndex].Contains(currentStrong) then
    begin
      phrase := Self.Items[phraseIndex];
      Result := Result + phrase.GetPhraseRtf;

      for keyIndex := 0 to phrase.Count - 1 do
      begin
        Result := Result +
                  Format('{\i %s (%d)} \par\li500 ',
                    [UnicodeToRtf(phrase.FormattedKeys[keyIndex].Replace(';', ' ')),
                     phrase.Items[keyIndex].Count]);
        lastReference := '';
        occurrencesInVerse := 0;

        for referenceIndex := 0 to phrase.Items[keyIndex].Count - 1 do
        begin
          Inc(totalOccurrences);
          Inc(occurrencesInVerse);
          if lastReference <> phrase.Items[keyIndex].Strings[referenceIndex] then
          begin
            if occurrencesInVerse > 1 then
              Result := Result + Format(' (%d)', [occurrencesInVerse]);
            occurrencesInVerse := 0;
            lastReference := phrase.Items[keyIndex].Strings[referenceIndex];
            if referenceIndex <> 0 then
              Result := Result + '; ';
            Result := Result +
              Format('{\field{\*\fldinst HYPERLINK "tw://bible.*?id=%s"}{\fldrslt\cf2 [vref]}}',
                [AnsiReplaceText(lastReference, ',', '.')]);
          end;
        end;
        Result := Result + '\par\li250 ';
      end;
    end;
  end;

  if totalOccurrences > 0 then
    Result := Result.Replace('__OCORRENCIAS__', IntToStr(totalOccurrences))
  else
    Result := '';
end;

procedure TConcordance.SetPhrase(const Key: string; const AValue: TConcordancePhrase);
begin
end;

constructor TConcordance.Create;
begin
  FPhraseHash := TFPHashList.Create;
  FRemoveMorphologyRegex := RegexCreate('<WT[^>]+>', [rcoUTF8, rcoIgnoreCase]);
  FUnicodeCharsRegex := RegexCreate('[^[:ascii:]]', [rcoUTF8]);
  FSyntagmRegex := RegexCreate('^([^<]+)?((?:<[^>]+>)+)$', [rcoUTF8, rcoIgnoreCase]);
  FTagRegex := RegexCreate('<W([GH][^<]+)>|<WT([^ <]+)(?: l="([^"]+)")?>', [rcoUTF8, rcoIgnoreCase]);
  FDetailed := True;
end;

destructor TConcordance.Destroy;
var
  i: Integer;
begin
  for i := 0 to Self.Count do
    Self.Items[i].Free;

  FPhraseHash.Free;
  inherited Destroy;
end;

procedure TConcordance.AddPhrase(Pairs: TStringList; const Ref: string);
var
  pairIndex: SmallInt;
  sourcePhrase: string;
  targetPhrase: string;
begin
  for pairIndex := 0 to Pairs.Count - 1 do
    if (pairIndex mod 2) = 0 then
    begin
      sourcePhrase := Pairs.Strings[pairIndex];
      targetPhrase := Pairs.Strings[pairIndex + 1];

      if not FDetailed then
        sourcePhrase := FRemoveMorphologyRegex.Replace(sourcePhrase, '');

      GetPhrase(sourcePhrase)[targetPhrase].Add(Ref);
    end;
end;

procedure TConcordance.Clear;
begin
end;

function TConcordance.AsString: string;
begin
  Result := FPhraseHash.Strs;
end;

function TConcordancePhrase.GetReferences(Key: string): TStringList;
begin
  Result := TStringList(FTranslationHash.Find(Key));
  if Result = nil then
  begin
    Result := TStringList.Create;
    FTranslationHash.Add(Key, Result);
  end;
end;

function TConcordancePhrase.GetCount: Integer;
begin
  Result := FTranslationHash.Count;
end;

function TConcordancePhrase.GetItem(Index: Integer): TStringList;
begin
  Result := nil;
  if (Index >= 0) and (Index < Self.Count) then
    Result := TStringList(FTranslationHash[Index]);
end;

function TConcordancePhrase.GetKey(Index: Integer): string;
begin
  Result := '';
  if (Index >= 0) and (Index < Self.Count) then
    Result := FTranslationHash.NameOfIndex(Index);
end;

function TConcordancePhrase.GetFormattedKey(Index: Integer): string;
var
  key: string;
begin
  key := Self.Keys[Index];
  Result := key
              .Replace('-;-', '-')
              .Replace('r-ei', 'rei')
              .Replace('r-ás', 'rás')
              .Replace('r-á', 'rá')
              .Replace('r-emos', 'remos')
              .Replace('r-eis', 'reis')
              .Replace('r-ão', 'rão')
              .Replace('r-ia', 'ria')
              .Replace('r-ias', 'rias')
              .Replace('r-íamos', 'ríamos')
              .Replace('r-íeis', 'ríeis')
              .Replace('r-iam', 'riam')
              .Replace('á-ei', 'arei')
              .Replace('á-ás', 'arás')
              .Replace('á-á', 'ará')
              .Replace('á-emos', 'aremos')
              .Replace('á-eis', 'areis')
              .Replace('á-ão', 'arão')
              .Replace('á-ia', 'aria')
              .Replace('á-ias', 'arias')
              .Replace('á-íamos', 'aríamos')
              .Replace('á-íeis', 'aríeis')
              .Replace('á-iam', 'ariam')
              .Replace(';', ' ')
              .Replace('- ', ' ');

  if Result.EndsWith('á-') then
    Result := Result.Replace('á-', 'ar');
  if Result.EndsWith('ê-') then
    Result := Result.Replace('ê-', 'er');
  if Result.EndsWith('-') then
    Result := Copy(Result, 1, Length(Result) - 1);

  if key.Contains('-') then
    DebugLn(Format('"%s" => "%s"', [key, Result]));
end;

procedure TConcordancePhrase.SetReferences(Key: string; const AValue: TStringList);
begin
end;

constructor TConcordancePhrase.Create(AParent: TConcordance; const APhrase: string);
begin
  FParent := AParent;
  FPhrase := APhrase;
  FTranslationHash := TFPHashList.Create;
end;

destructor TConcordancePhrase.Destroy;
var
  i: Integer;
begin
  for i := 0 to Self.Count do
    Self.Items[i].Free;

  FTranslationHash.Free;
  inherited Destroy;
end;

function TConcordancePhrase.GetPhraseRtf: string;
var
  matches: IMatchCollection;
  tagMatches: IMatchCollection;
  i: SmallInt;
  words: TStringArray;
  wordText: string;
  displayText: string;
  tagText: string;
begin
  Result := '\par\li0 {\u8226?}';
  words := FPhrase.Split(';');
  for wordText in words do
  begin
    matches := FParent.FSyntagmRegex.Matches(wordText);
    if matches.Count = 0 then
      Break;
    displayText := matches[0].Groups[1].GetValue;
    tagText := matches[0].Groups[2].GetValue;

    tagMatches := FParent.FTagRegex.Matches(tagText);
    for i := 0 to tagMatches.Count - 1 do
    begin
      if tagMatches[i].Groups[1].GetMatched then
        Result := Result + IfThen(not Result.EndsWith(' '), ' ') +
          MakeStrongLink(tagMatches[i].Groups[1].GetValue,
            IfThen(FParent.Detailed, displayText,
              UpperCase(tagMatches[i].Groups[1].GetValue)))
      else if tagMatches[i].Groups[2].GetMatched then
        Result := Result + Format(' {\super %s }',
          [MakeMorphologyLink(tagMatches[i].Groups[2].GetValue,
            UpperCase(tagMatches[i].Groups[2].GetValue))]);
    end;
  end;
  Result := Result + '\par\li250 ';
end;

end.