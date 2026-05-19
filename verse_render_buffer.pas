unit verse_render_buffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, versiculo, syntagm;

type
  TRenderInterlinearMode = (rimMySword, rimTheWord);

  TVerseRenderBuffer = class
  private
    FSourceVerse: TVersiculo;
    FDestinationVerse: TVersiculo;
    procedure EnsureReady;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetTokenizerVersion(Value: Integer);
    procedure LoadTexts(const OriginalText, TranslatedText: string);
    procedure LoadPair(const OriginalText, TranslatedText, Associations: string);
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
    property SourceVerse: TVersiculo read FSourceVerse;
    property DestinationVerse: TVersiculo read FDestinationVerse;
  end;

implementation

procedure TVerseRenderBuffer.EnsureReady;
begin
  if not Assigned(FSourceVerse) or not Assigned(FDestinationVerse) then
    raise Exception.Create('Verse render buffer is not initialized');
end;

constructor TVerseRenderBuffer.Create;
begin
  inherited Create;
  FSourceVerse := TVersiculo.Create();
  FDestinationVerse := TVersiculo.Create();
  FSourceVerse.VersiculoPar := FDestinationVerse;
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
  FSourceVerse.Texto := OriginalText;
  FDestinationVerse.Texto := TranslatedText;
end;

procedure TVerseRenderBuffer.LoadPair(const OriginalText, TranslatedText,
  Associations: string);
begin
  LoadTexts(OriginalText, TranslatedText);
  FSourceVerse.Pares := Associations;
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
    FSourceVerse.AlterarTexto(NewText)
  else
    FDestinationVerse.AlterarTexto(NewText);
  Result := FSourceVerse.Pares;
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
  Result := FSourceVerse.Pares;
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
  Result := FDestinationVerse.GetLinhaONT(ExportMorphology, ExportNAAsItalics,
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
  Result := FSourceVerse.GetListaPares(ListType);
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

end.