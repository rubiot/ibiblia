unit ONTParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, ONTTokenizer, Graphics, HTMLColors;

type

  TONTTokenKind = (
    otNull,
    otEOF,
    otText,       // plain text
    otAddedWords, // FI
    otOTQuote,    // FO
    otJesusWords, // FR
    otNote,       // RF
    otParagraph,  // CM
    otLineBreak,  // CL
    otTitle,      // TS-TS3
    otMetaData,   // unhandled tag

    otInterlinearBlock, // interlinear block
    otTranslationBlock, // translation part of interlinear block
    otTransliterationBlock, // transliteration part of interlinear block
    otHebrewBlock,
    otGreekBlock
  );

  TONTToken = record
    Text: string;
    case Kind: TONTTokenKind of
      otTitle: (Level: integer);
      otNote: (q: string[32]);
  end;

  { TONTParser }

  TONTParser = class
  private
    FTokenizer: TONTTokenizer;
    FChunk: TONTToken;

    function Translate(from: TToken): TONTTokenKind;
    function GetEndingTag(tag: string): string;
  public
    constructor Create(text: string);
    destructor Destroy; override;
    function ReadNext: TONTToken;
  end;

implementation

{ TONTParser }

function TONTParser.ReadNext: TONTToken;
var
  kind: TONTTokenKind;
  return: boolean;
begin
  if FTokenizer.Token.Kind = ttEOF then
  begin
    Result.Kind := otNull;
    Exit;
  end;

  while FTokenizer.ReadToken <> ttEOF do
  begin
    kind := Translate(FTokenizer.Token);
    if kind = FChunk.Kind then
    begin
      FChunk.Text := FChunk.Text + FTokenizer.Token.Text;
    end else
    begin
      if FChunk.Kind <> otNull then
      begin
        Result := FChunk;
        return := true;
      end else
        return := false;

      FChunk.Kind := kind;

      case kind of
        otText:
          ;
        otAddedWords, otOTQuote, otJesusWords, otInterlinearBlock, otTranslationBlock, otTransliterationBlock:
          FTokenizer.ReadUntilTag(GetEndingTag(FTokenizer.Token.Text));
        otNote:
        begin
          FChunk.q := FTokenizer.ReadProperty('q');
          FTokenizer.ReadUntilTag('<Rf>');
        end;
        otTitle:
        begin
          if FTokenizer.Token.Text = '<TS>' then
            FChunk.Level := 0
          else
            FChunk.Level := Copy(FTokenizer.Token.Text, 4, 1).ToInteger;
          FTokenizer.ReadUntilTag('<Ts>');
        end;
        otNull, otEOF, otParagraph, otLineBreak, otMetaData:
          ;
      end;

      FChunk.Text := FTokenizer.Token.Text;
      if return then
        Exit;
    end;
  end;

  Result := FChunk;
end;

function TONTParser.Translate(from: TToken): TONTTokenKind;
begin
  Result := otNull;
  case from.Kind of
    ttNull:
      Result:= otNull;
    ttEOF:
      Result:= otEOF;
    ttPunctuation, ttSpace, ttSyntagm:
      Result := otText;
    ttTag:
      if from.Text = '<FI>' then
        Result := otAddedWords
      else if from.Text = '<FR>' then
        Result := otJesusWords
      else if from.Text = '<FO>' then
        Result := otOTQuote
      else if from.Text.StartsWith('<RF') then
        Result := otNote
      else if from.Text = '<CM>' then
        Result := otParagraph
      else if from.Text = '<CL>' then
        Result := otLineBreak
      else if from.Text.StartsWith('<TS') then
        Result := otTitle
      else if from.Text = '<Q>' then
        Result := otInterlinearBlock
      else if (from.Text = '<E>') or (from.Text = '<T>') then
        Result := otTranslationBlock
      else if from.Text = '<X>' then
        Result := otTransliterationBlock
      else if from.Text = '<H>' then
        Result := otHebrewBlock
      else if from.Text = '<G>' then
        Result := otGreekBlock
      else
        Result := otMetaData;
  end;
end;

function TONTParser.GetEndingTag(tag: string): string;
begin
  result := copy(tag, 1, tag.Length-2) + copy(tag, tag.Length-1, 1).ToLower + '>';
end;

constructor TONTParser.Create(text: string);
begin
  inherited Create;
  FTokenizer := TONTTokenizer.Create(text);
  FChunk.Text := '';
  FChunk.Kind := otNull;
end;

destructor TONTParser.Destroy;
begin
  FTokenizer.Free;
end;

end.

