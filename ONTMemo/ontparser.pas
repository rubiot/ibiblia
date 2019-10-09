unit ONTParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, ONTTokenizer, Graphics, HTMLColors, typinfo, LazLogger;

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
    otStrong,
    otMorphology,
    otPunctuation,
    otSpace,
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
      otText: (Tags: string[32]; Punctuation: string[8]);
  end;

  { TONTParser }

  TONTParser = class
  private
    FTokenizer: TONTTokenizer;

    function Translate(from: TToken): TONTTokenKind;
    function GetEndingTag(tag: string): string;
    function TokenToString(t: TONTToken): string;
  public
    constructor Create(text: string);
    destructor Destroy; override;
    function ReadNext: TONTToken;
  end;

implementation

{ TONTParser }

function TONTParser.ReadNext: TONTToken;
begin
  FTokenizer.ReadToken;
  Result.Kind := Translate(FTokenizer.Token);

  case Result.Kind of
    otText:
    begin
      Result.Text := FTokenizer.Token.Text;
      Result.Tags := '';
      Result.Punctuation := '';
      while FTokenizer.ReadToken <> ttNull do
      begin
        case Translate(FTokenizer.Token) of
          otStrong, otMorphology:
            Result.Tags := Result.Tags + FTokenizer.Token.Text;
          otPunctuation:
            Result.Punctuation := Result.Punctuation + FTokenizer.Token.Text;
        else
          FTokenizer.UnreadToken;
          break;
        end;
      end;
    end;
    otAddedWords, otOTQuote, otJesusWords, otInterlinearBlock, otTranslationBlock, otTransliterationBlock:
    begin
      FTokenizer.ReadUntilTag(GetEndingTag(FTokenizer.Token.Text));
      Result.Text := FTokenizer.Token.Text;
    end;
    otNote:
    begin
      Result.q := FTokenizer.ReadProperty('q');
      FTokenizer.ReadUntilTag('<Rf>');
      Result.Text := FTokenizer.Token.Text;
    end;
    otTitle:
    begin
      if FTokenizer.Token.Text = '<TS>' then
        Result.Level := 0
      else
        Result.Level := Copy(FTokenizer.Token.Text, 4, 1).ToInteger;
      FTokenizer.ReadUntilTag('<Ts>');
      Result.Text := FTokenizer.Token.Text;
    end;
    otNull, otEOF, otParagraph, otLineBreak, otMetaData:
      Result.Text := FTokenizer.Token.Text;
  end;
end;

function TONTParser.Translate(from: TToken): TONTTokenKind;
begin
  Result := otNull;
  case from.Kind of
    ttNull:
      Result:= otNull;
    ttEOF:
      Result:= otEOF;
    ttPunctuation, ttSpace:
      Result := otPunctuation;
    //ttSpace:
    //  Result := otSpace;
    ttSyntagm:
      Result := otText;
    ttTag:
      if from.Text = '<FI>' then
        Result := otAddedWords
      else if from.Text.StartsWith('<WG') then
        Result := otStrong
      else if from.Text.StartsWith('<WT') then
        Result := otMorphology
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

function TONTParser.TokenToString(t: TONTToken): string;
begin
  Result := Format('{text="%s", kind=%s', [t.Text, GetEnumName(TypeInfo(TONTTokenKind), ord(t.Kind))]);
  case t.Kind of
    otTitle:
      Result := Result + Format(', level=%d', [t.Level]);
    otNote:
      Result := Result + Format(', Q="%s"', [t.q]);
    otText:
      Result := Result + Format(', tags="%s", punctuation="%s"', [t.Tags, t.Punctuation]);
  end;
  Result := Result + '}';
end;

constructor TONTParser.Create(text: string);
begin
  inherited Create;
  FTokenizer := TONTTokenizer.Create(text);
end;

destructor TONTParser.Destroy;
begin
  FTokenizer.Free;
end;

end.

