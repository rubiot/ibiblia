unit verse_render_buffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Verse, syntagm, onttokenizer;

type
  TRenderInterlinearMode = (rimMySword, rimTheWord);

  TVerseRenderBuffer = class
  private
    FSourceVerse: TVerse;
    FDestinationVerse: TVerse;
    procedure EnsureReady;
    function CanonicalizeIndexList(const Value: string;
      out Canonical: string): Boolean;
    function CanonicalizeAssociations(const Associations: string;
      Canonical: TStringList): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetTokenizerVersion(Value: Integer);
    procedure LoadTexts(const OriginalText, TranslatedText: string);
    procedure LoadPair(const OriginalText, TranslatedText, Associations: string);
    function AssociationsEquivalent(const Left, Right: string): Boolean;
    function FormatCorruptionDetails(const ErrorMessage, Associations: string): string;
    function ReplaceTextKeepingAssociations(const OriginalText, TranslatedText,
      Associations: string; ReplaceSourceText: Boolean; const NewText: string): string;
    function TryReplaceTextKeepingAssociations(const OriginalText, TranslatedText,
      Associations: string; ReplaceSourceText: Boolean; const NewText: string;
      out UpdatedPairs: string; out CorruptionDetails: string): Boolean;
    function RetokenizeAssociations(const OriginalText, TranslatedText,
      Associations: string; OldVersion, NewVersion: Integer): string;
    function BuildInterlinearLine(const OriginalText, TranslatedText,
      Associations: string; Mode: TRenderInterlinearMode): string;
    function TryBuildInterlinearLine(const OriginalText, TranslatedText,
      Associations: string; Mode: TRenderInterlinearMode; out Line: string;
      out CorruptionDetails: string): Boolean;
    function BuildDestinationLineONT(const OriginalText, TranslatedText,
      Associations: string; ExportMorphology, ExportNAAsItalics,
      MarkReusedStrongs, MarkUntranslatedStrongs: Boolean): string;
    function TryBuildDestinationLineONT(const OriginalText, TranslatedText,
      Associations: string; ExportMorphology, ExportNAAsItalics,
      MarkReusedStrongs, MarkUntranslatedStrongs: Boolean; out Line: string;
      out CorruptionDetails: string): Boolean;
    function ExtractPairsList(const OriginalText, TranslatedText,
      Associations: string; ListType: TPairsListType): TStringList;
    function TryExtractPairsList(const OriginalText, TranslatedText,
      Associations: string; ListType: TPairsListType; out Pairs: TStringList;
      out CorruptionDetails: string): Boolean;
    function ExtractAssociationPairData(const OriginalText, TranslatedText,
      Associations: string;
      ListType: TPairsListType): TAssociationPairDataArray;
    function TryExtractAssociationPairData(const OriginalText, TranslatedText,
      Associations: string; ListType: TPairsListType;
      out PairData: TAssociationPairDataArray;
      out CorruptionDetails: string): Boolean;
    property SourceVerse: TVerse read FSourceVerse;
    property DestinationVerse: TVerse read FDestinationVerse;
  end;

implementation

resourcestring
  SAssociationDataNotCompletelyApplied =
    'Association data could not be applied completely to the verse pair';

function CompareCanonicalIndexes(List: TStringList;
  Index1, Index2: Integer): Integer;
var
  LeftValue: Integer;
  RightValue: Integer;
begin
  LeftValue := StrToInt(List[Index1]);
  RightValue := StrToInt(List[Index2]);
  if LeftValue < RightValue then
    Result := -1
  else if LeftValue > RightValue then
    Result := 1
  else
    Result := 0;
end;

procedure TVerseRenderBuffer.EnsureReady;
begin
  if not Assigned(FSourceVerse) or not Assigned(FDestinationVerse) then
    raise Exception.Create('Verse render buffer is not initialized');
end;

function TVerseRenderBuffer.CanonicalizeIndexList(const Value: string;
  out Canonical: string): Boolean;
var
  Index: Integer;
  ParsedIndex: Integer;
  Items: TStringList;
begin
  Result := False;
  Canonical := '';
  if Value.IsEmpty then
    Exit;

  Items := TStringList.Create;
  try
    Items.StrictDelimiter := True;
    Items.Delimiter := ',';
    Items.DelimitedText := Value;
    if Items.Count = 0 then
      Exit;

    for Index := 0 to Items.Count - 1 do
    begin
      if not TryStrToInt(Trim(Items[Index]), ParsedIndex) or
        (ParsedIndex < 0) then
        Exit;
      Items[Index] := IntToStr(ParsedIndex);
    end;
    Items.CustomSort(@CompareCanonicalIndexes);

    for Index := 0 to Items.Count - 1 do
    begin
      if Index > 0 then
        Canonical := Canonical + ',';
      Canonical := Canonical + Items[Index];
    end;
    Result := True;
  finally
    Items.Free;
  end;
end;

function TVerseRenderBuffer.CanonicalizeAssociations(
  const Associations: string; Canonical: TStringList): Boolean;
var
  Parsed: TSyntagmToken;
  SourceIndexes: string;
  TargetIndexes: string;
  Tokenizer: TONTTokenizer;
begin
  Result := False;
  Canonical.Clear;
  if Associations.IsEmpty then
    Exit(True);

  Tokenizer := TTokenizerFactory.CreateTokenizer(
    FSourceVerse.TokenizerVersion, Associations);
  try
    while Tokenizer.ReadSyntagm(Parsed) <> skNull do
    begin
      if Parsed.Kind = skWhitespace then
        Continue;
      if not AnsiStartsStr('<par ', Parsed.Value) then
        Exit;
      if not CanonicalizeIndexList(
        Tokenizer.ReadTagProperty('a', Parsed), SourceIndexes) or
        not CanonicalizeIndexList(
        Tokenizer.ReadTagProperty('b', Parsed), TargetIndexes) then
        Exit;
      Canonical.Add(SourceIndexes + '=' + TargetIndexes);
    end;
    Canonical.Sort;
    Result := True;
  finally
    Tokenizer.Free;
  end;
end;

function TVerseRenderBuffer.AssociationsEquivalent(const Left,
  Right: string): Boolean;
var
  Index: Integer;
  LeftCanonical: TStringList;
  RightCanonical: TStringList;
begin
  Result := False;
  LeftCanonical := TStringList.Create;
  RightCanonical := TStringList.Create;
  try
    if not CanonicalizeAssociations(Left, LeftCanonical) or
      not CanonicalizeAssociations(Right, RightCanonical) or
      (LeftCanonical.Count <> RightCanonical.Count) then
      Exit;
    for Index := 0 to LeftCanonical.Count - 1 do
      if LeftCanonical[Index] <> RightCanonical[Index] then
        Exit;
    Result := True;
  finally
    RightCanonical.Free;
    LeftCanonical.Free;
  end;
end;

constructor TVerseRenderBuffer.Create;
begin
  inherited Create;
  FSourceVerse := TVerse.Create();
  FDestinationVerse := TVerse.Create();
  FSourceVerse.PairedVerse := FDestinationVerse;
end;

destructor TVerseRenderBuffer.Destroy;
begin
  if Assigned(FSourceVerse) then
    FSourceVerse.Destroy;
  if Assigned(FDestinationVerse) then
    FDestinationVerse.Destroy;
  inherited Destroy;
end;

procedure TVerseRenderBuffer.SetTokenizerVersion(Value: Integer);
begin
  EnsureReady;
  FSourceVerse.TokenizerVersion := Value;
  FDestinationVerse.TokenizerVersion := Value;
end;

procedure TVerseRenderBuffer.LoadTexts(const OriginalText, TranslatedText: string);
begin
  EnsureReady;
  FSourceVerse.Text := OriginalText;
  FDestinationVerse.Text := TranslatedText;
end;

procedure TVerseRenderBuffer.LoadPair(const OriginalText, TranslatedText,
  Associations: string);
begin
  LoadTexts(OriginalText, TranslatedText);
  FSourceVerse.Pairs := Associations;
  if not AssociationsEquivalent(FSourceVerse.Pairs, Associations) then
    raise Exception.Create(SAssociationDataNotCompletelyApplied);
end;

function TVerseRenderBuffer.FormatCorruptionDetails(const ErrorMessage,
  Associations: string): string;
begin
  Result := Format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
    [ErrorMessage, Associations, FSourceVerse.DebugTokens, FDestinationVerse.DebugTokens]);
end;

function TVerseRenderBuffer.ReplaceTextKeepingAssociations(const OriginalText,
  TranslatedText, Associations: string; ReplaceSourceText: Boolean;
  const NewText: string): string;
begin
  LoadPair(OriginalText, TranslatedText, Associations);
  if ReplaceSourceText then
    FSourceVerse.ChangeText(NewText)
  else
    FDestinationVerse.ChangeText(NewText);
  Result := FSourceVerse.Pairs;
end;

function TVerseRenderBuffer.TryReplaceTextKeepingAssociations(
  const OriginalText, TranslatedText, Associations: string;
  ReplaceSourceText: Boolean; const NewText: string; out UpdatedPairs: string;
  out CorruptionDetails: string): Boolean;
begin
  CorruptionDetails := '';
  try
    UpdatedPairs := ReplaceTextKeepingAssociations(OriginalText, TranslatedText,
      Associations, ReplaceSourceText, NewText);
    Result := True;
  except
    on E: Exception do
    begin
      UpdatedPairs := '';
      CorruptionDetails := FormatCorruptionDetails(E.Message, Associations);
      Result := False;
    end;
  end;
end;

function TVerseRenderBuffer.RetokenizeAssociations(const OriginalText,
  TranslatedText, Associations: string; OldVersion, NewVersion: Integer): string;
begin
  EnsureReady;
  FSourceVerse.TokenizerVersion := OldVersion;
  FDestinationVerse.TokenizerVersion := OldVersion;
  LoadPair(OriginalText, TranslatedText, Associations);

  FSourceVerse.TokenizerVersion := NewVersion;
  FDestinationVerse.TokenizerVersion := NewVersion;
  FDestinationVerse.RetokenizePreservingAssociations;
  FSourceVerse.RetokenizePreservingAssociations;
  Result := FSourceVerse.Pairs;
end;

function TVerseRenderBuffer.BuildInterlinearLine(const OriginalText,
  TranslatedText, Associations: string; Mode: TRenderInterlinearMode): string;
begin
  LoadPair(OriginalText, TranslatedText, Associations);
  case Mode of
    rimMySword:
      Result := FSourceVerse.GetMySwordInterlinearLine;
    rimTheWord:
      Result := FSourceVerse.GetTheWordInterlinearLine;
  else
    raise Exception.CreateFmt('Invalid render interlinear mode [%d]', [Ord(Mode)]);
  end;
end;

function TVerseRenderBuffer.TryBuildInterlinearLine(const OriginalText,
  TranslatedText, Associations: string; Mode: TRenderInterlinearMode;
  out Line: string; out CorruptionDetails: string): Boolean;
begin
  CorruptionDetails := '';
  try
    Line := BuildInterlinearLine(OriginalText, TranslatedText, Associations, Mode);
    Result := True;
  except
    on E: Exception do
    begin
      Line := '';
      CorruptionDetails := FormatCorruptionDetails(E.Message, Associations);
      Result := False;
    end;
  end;
end;

function TVerseRenderBuffer.BuildDestinationLineONT(const OriginalText,
  TranslatedText, Associations: string; ExportMorphology, ExportNAAsItalics,
  MarkReusedStrongs, MarkUntranslatedStrongs: Boolean): string;
begin
  LoadPair(OriginalText, TranslatedText, Associations);
  Result := FDestinationVerse.GetONTLine(ExportMorphology, ExportNAAsItalics,
    MarkReusedStrongs, MarkUntranslatedStrongs);
end;

function TVerseRenderBuffer.TryBuildDestinationLineONT(const OriginalText,
  TranslatedText, Associations: string; ExportMorphology, ExportNAAsItalics,
  MarkReusedStrongs, MarkUntranslatedStrongs: Boolean; out Line: string;
  out CorruptionDetails: string): Boolean;
begin
  CorruptionDetails := '';
  try
    Line := BuildDestinationLineONT(OriginalText, TranslatedText, Associations,
      ExportMorphology, ExportNAAsItalics, MarkReusedStrongs,
      MarkUntranslatedStrongs);
    Result := True;
  except
    on E: Exception do
    begin
      Line := '';
      CorruptionDetails := FormatCorruptionDetails(E.Message, Associations);
      Result := False;
    end;
  end;
end;

function TVerseRenderBuffer.ExtractPairsList(const OriginalText,
  TranslatedText, Associations: string; ListType: TPairsListType): TStringList;
begin
  LoadPair(OriginalText, TranslatedText, Associations);
  Result := FSourceVerse.GetPairsList(ListType);
end;

function TVerseRenderBuffer.TryExtractPairsList(const OriginalText,
  TranslatedText, Associations: string; ListType: TPairsListType;
  out Pairs: TStringList; out CorruptionDetails: string): Boolean;
begin
  CorruptionDetails := '';
  try
    Pairs := ExtractPairsList(OriginalText, TranslatedText, Associations, ListType);
    Result := True;
  except
    on E: Exception do
    begin
      Pairs := nil;
      CorruptionDetails := FormatCorruptionDetails(E.Message, Associations);
      Result := False;
    end;
  end;
end;

function TVerseRenderBuffer.ExtractAssociationPairData(const OriginalText,
  TranslatedText, Associations: string;
  ListType: TPairsListType): TAssociationPairDataArray;
begin
  LoadPair(OriginalText, TranslatedText, Associations);
  Result := FSourceVerse.GetAssociationPairData(ListType);
end;

function TVerseRenderBuffer.TryExtractAssociationPairData(
  const OriginalText, TranslatedText, Associations: string;
  ListType: TPairsListType; out PairData: TAssociationPairDataArray;
  out CorruptionDetails: string): Boolean;
begin
  CorruptionDetails := '';
  PairData := nil;
  try
    PairData := ExtractAssociationPairData(OriginalText, TranslatedText,
      Associations, ListType);
    Result := True;
  except
    on E: Exception do
    begin
      PairData := nil;
      CorruptionDetails := FormatCorruptionDetails(E.Message, Associations);
      Result := False;
    end;
  end;
end;

end.
