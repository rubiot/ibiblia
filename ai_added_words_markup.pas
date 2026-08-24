unit ai_added_words_markup;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TAddedWordMask = array of Boolean;

  TAddedWordMarkup = record
    CanonicalText: string;
    VisibleText: string;
    AddedMask: TAddedWordMask;
  end;

function TryParseAddedWordMarkup(const MarkedText: string;
  out Markup: TAddedWordMarkup; out ErrorMessage: string): Boolean;
function HasStyledOccurrence(const FullText,
  Phrase: TAddedWordMarkup): Boolean;

implementation

uses
  StrUtils;

procedure AppendVisibleByte(var Markup: TAddedWordMarkup; Value: Char;
  IsAdded: Boolean);
var
  MaskIndex: Integer;
begin
  Markup.CanonicalText := Markup.CanonicalText + Value;
  Markup.VisibleText := Markup.VisibleText + Value;
  MaskIndex := Length(Markup.AddedMask);
  SetLength(Markup.AddedMask, MaskIndex + 1);
  Markup.AddedMask[MaskIndex] := IsAdded;
end;

function TryParseAddedWordMarkup(const MarkedText: string;
  out Markup: TAddedWordMarkup; out ErrorMessage: string): Boolean;
var
  I, ContentStart: Integer;
  InAddedSpan: Boolean;
  Content: string;
begin
  Markup := Default(TAddedWordMarkup);
  ErrorMessage := '';
  InAddedSpan := False;
  ContentStart := 0;

  for I := 1 to Length(MarkedText) do
  begin
    case MarkedText[I] of
      '[':
        begin
          if InAddedSpan then
          begin
            ErrorMessage := 'Added-word brackets cannot be nested';
            Exit(False);
          end;
          InAddedSpan := True;
          ContentStart := I + 1;
          Markup.CanonicalText := Markup.CanonicalText + '<FI>';
        end;
      ']':
        begin
          if not InAddedSpan then
          begin
            ErrorMessage := 'Added-word closing bracket has no opening bracket';
            Exit(False);
          end;
          Content := Copy(MarkedText, ContentStart, I - ContentStart);
          if Content = '' then
          begin
            ErrorMessage := 'Added-word brackets cannot be empty';
            Exit(False);
          end;
          if Trim(Content) <> Content then
          begin
            ErrorMessage :=
              'Added-word brackets cannot contain leading or trailing whitespace';
            Exit(False);
          end;
          if (Pos(#10, Content) > 0) or (Pos(#13, Content) > 0) then
          begin
            ErrorMessage := 'Added-word brackets cannot span lines';
            Exit(False);
          end;
          Markup.CanonicalText := Markup.CanonicalText + '<Fi>';
          InAddedSpan := False;
          ContentStart := 0;
        end;
    else
      AppendVisibleByte(Markup, MarkedText[I], InAddedSpan);
    end;
  end;

  if InAddedSpan then
  begin
    ErrorMessage := 'Added-word opening bracket has no closing bracket';
    Exit(False);
  end;

  Result := True;
end;

function MasksMatchAt(const FullText, Phrase: TAddedWordMarkup;
  StartIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Length(FullText.AddedMask) <> Length(FullText.VisibleText)) or
     (Length(Phrase.AddedMask) <> Length(Phrase.VisibleText)) then
    Exit;
  if (StartIndex < 1) or
     (StartIndex + Length(Phrase.VisibleText) - 1 >
       Length(FullText.VisibleText)) then
    Exit;

  for I := 0 to Length(Phrase.VisibleText) - 1 do
    if FullText.AddedMask[StartIndex + I - 1] <>
       Phrase.AddedMask[I] then
      Exit;
  Result := True;
end;

function HasStyledOccurrence(const FullText,
  Phrase: TAddedWordMarkup): Boolean;
var
  CandidateIndex, SearchFrom: Integer;
begin
  Result := False;
  if Phrase.VisibleText = '' then
    Exit;

  SearchFrom := 1;
  repeat
    CandidateIndex := PosEx(Phrase.VisibleText, FullText.VisibleText,
      SearchFrom);
    if CandidateIndex = 0 then
      Exit;
    if MasksMatchAt(FullText, Phrase, CandidateIndex) then
      Exit(True);
    SearchFrom := CandidateIndex + 1;
  until SearchFrom > Length(FullText.VisibleText);
end;

end.
