unit ONTParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, ONTTokenizer, Graphics, HTMLColors, typinfo, LazLogger;

type

  TONTTokenKind = (
    otNull,
    otEOF,
    otEOL,
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
    otWT,         // <wt> tag
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
      otText: (Tags: string[32]; Strongs: string[32]; Morpho: string[32]; Lemma: string[32]; Punctuation: string[8]);
  end;

  { TONTParser }

  TONTParser = class
  private
    FTokenizer: TONTTokenizer;

    function Translate(from: TToken): TONTTokenKind;
    function GetEndingTag(tag: string): string;
    function TokenToString(t: TONTToken): string;
    function ExtractStrong(tag: string): string;
    function ExtractMorpho(tag: string): string;
    function ExtractLemma(tag: string): string;
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
  FillByte(Result, SizeOf(Result), 0);
  Result.Kind := Translate(FTokenizer.Token);

  case Result.Kind of
    otText, otWT:
    begin
      if Result.Kind = otWT then // multiple words
      begin
        FTokenizer.ReadUntilTag(ttTag);
        Result.Kind := otText;
      end;
      Result.Text := FTokenizer.Token.Text;
      while FTokenizer.ReadToken <> ttNull do
      begin
        case Translate(FTokenizer.Token) of
          otStrong:
          begin
            Result.Strongs := ExtractStrong(FTokenizer.Token.Text);
            Result.Tags    := Result.Tags + FTokenizer.Token.Text;
          end;
          otMorphology:
          begin
            Result.Morpho  := ExtractMorpho(FTokenizer.Token.Text);
            Result.Lemma   := ExtractLemma(FTokenizer.Token.Text);
            Result.Tags    := Result.Tags + FTokenizer.Token.Text;
          end;
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
    otNull, otEOF, otEOL, otParagraph, otLineBreak, otMetaData, otPunctuation:
      Result.Text := FTokenizer.Token.Text;
    otStrong, otMorphology: // ignoring for now. TODO: handles astray strong/morpho tags
      Result.Text := FTokenizer.Token.Text;
    else
      raise Exception.Create(Format('unhandled token in TONTParser.ReadNext: [%s]', [FTokenizer.Token.Text]));
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
    ttEOL:
      Result:= otEOL;
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
      else if from.Text = '<wt>' then
        Result := otWT
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

function TONTParser.ExtractStrong(tag: string): string;
begin
  if not tag.StartsWith('<WG') and not tag.StartsWith('<WH') then
    raise Exception.Create(Format('Invalid Strong''s tag: %s', [tag]));
  result := Copy(tag, 3, tag.Length-3);
end;

function TONTParser.ExtractMorpho(tag: string): string;
var
  p: Integer;
begin
  if not tag.StartsWith('<WT') then
    raise Exception.Create(Format('Invalid morphology tag: %s', [tag]));
  p := Pos(' ', tag);
  if p > 0 then
    result := Copy(tag, 4, p-4)
  else
    result := Copy(tag, 4, tag.Length-4);
end;

function TONTParser.ExtractLemma(tag: string): string;
var
  p: Integer;
begin
  if not tag.StartsWith('<WT') then
    raise Exception.Create(Format('Invalid morphology tag: %s', [tag]));
  result := '';
  p := Pos('"', tag);
  if p > 0 then
    result := Copy(tag, p+1, tag.Length-p-2);
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

