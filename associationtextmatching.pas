unit AssociationTextMatching;

{$mode objfpc}{$H+}

interface

function NormalizeAssociationApostrophes(const Value: string): string;
function AssociationApostropheCanAttach(const WordText,
  ApostropheText: string): Boolean;
function AssociationSyntagmTextsEquivalent(const LeftText,
  RightText: string): Boolean;
function AssociationPhraseOccursInText(const Phrase,
  FullText: string): Boolean;

implementation

uses
  SysUtils, LazUTF8, Character;

function IsCombiningMark(Codepoint: Cardinal): Boolean;
begin
  Result := ((Codepoint >= $0300) and (Codepoint <= $036F)) or
    ((Codepoint >= $1DC0) and (Codepoint <= $1DFF)) or
    ((Codepoint >= $20D0) and (Codepoint <= $20FF)) or
    ((Codepoint >= $FE20) and (Codepoint <= $FE2F));
end;

function IsGreekLetterCodepoint(Codepoint: Cardinal): Boolean;
var
  Category: TUnicodeCategory;
  CharacterText: UnicodeString;
begin
  if not (((Codepoint >= $0370) and (Codepoint <= $03FF)) or
          ((Codepoint >= $1F00) and (Codepoint <= $1FFF))) then
    Exit(False);

  CharacterText := UnicodeString(WideChar(Codepoint));
  Category := TCharacter.GetUnicodeCategory(CharacterText, 1);
  Result := Category in [
    TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter];
end;

function IsLegacyGreekApostrophe(Codepoint: Cardinal): Boolean;
begin
  Result := (Codepoint = $02BC) or (Codepoint = $1FBD) or
    (Codepoint = $1FBF);
end;

function IsRecognizedApostrophe(Codepoint: Cardinal): Boolean;
begin
  Result := (Codepoint = $0027) or (Codepoint = $2019) or
    IsLegacyGreekApostrophe(Codepoint);
end;

function IsWordBoundary(Codepoint: Cardinal): Boolean;
begin
  case Codepoint of
    $0000, $0009, $000A, $000D, $0020, $0022, $0027, $0028, $0029,
    $002C, $002D, $002E, $003A, $003B, $003C, $003E, $003F, $005B,
    $005D, $00A0, $2018, $2019, $2028, $2029, $202F, $205F, $3000,
    $2000..$200A:
      Result := True;
  else
    Result := False;
  end;
end;

function CodepointAt(const Value: string; ByteIndex: Integer;
  out CodepointLength: Integer): Cardinal;
begin
  if (ByteIndex < 1) or (ByteIndex > Length(Value)) then
  begin
    CodepointLength := 0;
    Exit(0);
  end;
  Result := UTF8CodepointToUnicode(@Value[ByteIndex], CodepointLength);
end;

function EndsWithGreekBase(const Value: string): Boolean;
var
  ByteIndex, CodepointLength: Integer;
  Codepoint, LastBase: Cardinal;
begin
  ByteIndex := 1;
  LastBase := 0;
  while ByteIndex <= Length(Value) do
  begin
    Codepoint := CodepointAt(Value, ByteIndex, CodepointLength);
    if CodepointLength <= 0 then
      Break;
    if IsGreekLetterCodepoint(Codepoint) then
      LastBase := Codepoint
    else if not IsCombiningMark(Codepoint) then
      LastBase := 0;
    Inc(ByteIndex, CodepointLength);
  end;
  Result := IsGreekLetterCodepoint(LastBase);
end;

function NormalizeAssociationApostrophes(const Value: string): string;
var
  ByteIndex, CodepointLength, NextLength: Integer;
  Codepoint, NextCodepoint, PreviousGreekBase: Cardinal;
begin
  Result := '';
  ByteIndex := 1;
  PreviousGreekBase := 0;
  while ByteIndex <= Length(Value) do
  begin
    Codepoint := CodepointAt(Value, ByteIndex, CodepointLength);
    if CodepointLength <= 0 then
      Break;
    NextCodepoint := CodepointAt(Value, ByteIndex + CodepointLength,
      NextLength);

    if (Codepoint = $2019) or
       (IsLegacyGreekApostrophe(Codepoint) and
        IsGreekLetterCodepoint(PreviousGreekBase) and
        IsWordBoundary(NextCodepoint)) then
      Result := Result + ''''
    else
      Result := Result + Copy(Value, ByteIndex, CodepointLength);

    if IsGreekLetterCodepoint(Codepoint) then
      PreviousGreekBase := Codepoint
    else if not IsCombiningMark(Codepoint) then
      PreviousGreekBase := 0;
    Inc(ByteIndex, CodepointLength);
  end;
end;

function AssociationApostropheCanAttach(const WordText,
  ApostropheText: string): Boolean;
var
  Codepoint: Cardinal;
  CodepointLength: Integer;
begin
  Codepoint := CodepointAt(ApostropheText, 1, CodepointLength);
  Result := EndsWithGreekBase(WordText) and
    (CodepointLength = Length(ApostropheText)) and
    IsRecognizedApostrophe(Codepoint);
end;

function AssociationSyntagmTextsEquivalent(const LeftText,
  RightText: string): Boolean;
begin
  Result := AnsiSameText(NormalizeAssociationApostrophes(LeftText),
    NormalizeAssociationApostrophes(RightText));
end;

function AssociationPhraseOccursInText(const Phrase,
  FullText: string): Boolean;
var
  NormalizedPhrase, NormalizedFullText: string;
begin
  NormalizedPhrase := NormalizeAssociationApostrophes(Phrase);
  NormalizedFullText := NormalizeAssociationApostrophes(FullText);
  Result := (NormalizedPhrase <> '') and
    (Pos(NormalizedPhrase, NormalizedFullText) > 0);
end;

end.
