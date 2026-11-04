unit rf_note_scanner;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, ONTTokenizer;

type
  TRFOpeningTagInfo = record
    HasAttributes: Boolean;
    HasQ: Boolean;
    Q: UnicodeString;
  end;

  ERFEditorialCommentProjectionError = class(Exception);

  TRFNoteNumberer = class
  private
    FAutomaticCount: Integer;
  public
    procedure Reset(ABaseAutomaticCount: Integer = 0);
    function ResolveMarker(const AOpeningTag: UnicodeString;
      out AMarker: UnicodeString): Boolean;
    property AutomaticCount: Integer read FAutomaticCount;
  end;

function TryParseRFOpeningTag(const ATag: UnicodeString;
  out AInfo: TRFOpeningTagInfo): Boolean;
function IsRFOpeningTag(const ATag: UnicodeString): Boolean;
function IsRFClosingTag(const ATag: UnicodeString): Boolean;
function ReadRFNoteBody(ATokenizer: TONTTokenizer; out ABody,
  AClosingTag: string): Boolean;
procedure ReadRFNoteBlock(ATokenizer: TONTTokenizer; var AOpeningToken: TSyntagmToken);
function NormalizeEditorialCommentForRF(
  const AComments: UnicodeString): UnicodeString;
function ContainsStructuralRFTag(
  const AText: UnicodeString): Boolean;
function AppendEditorialCommentAsRF(const AVerseText, AComments: UnicodeString;
  AEnabled: Boolean): UnicodeString;
function CountAutomaticRFNotes(const AText: UnicodeString): Integer;

implementation

function IsAsciiWhitespace(AChar: WideChar): Boolean;
begin
  Result := AChar in [' ', #9, #10, #13];
end;

function FoldAscii(AChar: WideChar): WideChar;
begin
  if (AChar >= 'A') and (AChar <= 'Z') then
    Result := WideChar(Ord(AChar) + Ord('a') - Ord('A'))
  else
    Result := AChar;
end;

function AsciiSameText(const ALeft, ARight: UnicodeString): Boolean;
var
  Index: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for Index := 1 to Length(ALeft) do
    if FoldAscii(ALeft[Index]) <> FoldAscii(ARight[Index]) then
      Exit(False);
  Result := True;
end;

function StartsAsciiTextAt(const AText, AValue: UnicodeString;
  AIndex: Integer): Boolean;
var
  Index: Integer;
begin
  if (AIndex < 1) or
     (AIndex + Length(AValue) - 1 > Length(AText)) then
    Exit(False);
  for Index := 1 to Length(AValue) do
    if FoldAscii(AText[AIndex + Index - 1]) <> FoldAscii(AValue[Index]) then
      Exit(False);
  Result := True;
end;

function ReadAttribute(const AText: UnicodeString; var AIndex: Integer;
  out AName, AValue: UnicodeString; out AHasValue: Boolean): Boolean;
var
  NameStart, ValueStart: Integer;
  Quote: WideChar;
begin
  Result := False;
  AName := '';
  AValue := '';
  AHasValue := False;
  while (AIndex <= Length(AText)) and IsAsciiWhitespace(AText[AIndex]) do
    Inc(AIndex);
  if AIndex > Length(AText) then
    Exit;
  NameStart := AIndex;
  while (AIndex <= Length(AText)) and
        not IsAsciiWhitespace(AText[AIndex]) and
        (AText[AIndex] <> '=') do
    Inc(AIndex);
  AName := Copy(AText, NameStart, AIndex - NameStart);
  if AName = '' then
    Exit;
  while (AIndex <= Length(AText)) and IsAsciiWhitespace(AText[AIndex]) do
    Inc(AIndex);
  if (AIndex > Length(AText)) or (AText[AIndex] <> '=') then
    Exit(True);
  AHasValue := True;
  Inc(AIndex);
  while (AIndex <= Length(AText)) and IsAsciiWhitespace(AText[AIndex]) do
    Inc(AIndex);
  if AIndex > Length(AText) then
    Exit(False);
  Quote := #0;
  if AText[AIndex] in ['"', ''''] then
  begin
    Quote := AText[AIndex];
    Inc(AIndex);
  end;
  ValueStart := AIndex;
  if Quote <> #0 then
  begin
    while (AIndex <= Length(AText)) and (AText[AIndex] <> Quote) do
      Inc(AIndex);
    if AIndex > Length(AText) then
      Exit(False);
    AValue := Copy(AText, ValueStart, AIndex - ValueStart);
    Inc(AIndex);
  end
  else
  begin
    while (AIndex <= Length(AText)) and
          not IsAsciiWhitespace(AText[AIndex]) do
      Inc(AIndex);
    AValue := Copy(AText, ValueStart, AIndex - ValueStart);
  end;
  Result := True;
end;

function TryParseRFOpeningTag(const ATag: UnicodeString;
  out AInfo: TRFOpeningTagInfo): Boolean;
var
  Attributes, Name, Value: UnicodeString;
  HasValue: Boolean;
  Index, LastIndex, RFStart: Integer;
begin
  AInfo.HasAttributes := False;
  AInfo.HasQ := False;
  AInfo.Q := '';
  Result := False;
  if Length(ATag) < 4 then
    Exit;
  Index := 1;
  if ATag[Index] <> '<' then
    Exit;
  Inc(Index);
  while (Index <= Length(ATag)) and IsAsciiWhitespace(ATag[Index]) do
    Inc(Index);
  RFStart := Index;
  if (Index + 1 > Length(ATag)) or
     (ATag[Index] <> 'R') or (ATag[Index + 1] <> 'F') then
    Exit;
  Inc(Index, 2);
  if (Index <= Length(ATag)) and
     not IsAsciiWhitespace(ATag[Index]) and (ATag[Index] <> '>') then
    Exit;
  LastIndex := Length(ATag);
  while (LastIndex > RFStart + 1) and IsAsciiWhitespace(ATag[LastIndex]) do
    Dec(LastIndex);
  if ATag[LastIndex] <> '>' then
    Exit;
  Dec(LastIndex);
  while (LastIndex >= Index) and IsAsciiWhitespace(ATag[LastIndex]) do
    Dec(LastIndex);
  Attributes := Copy(ATag, Index, LastIndex - Index + 1);
  AInfo.HasAttributes := Attributes <> '';
  Index := 1;
  while Index <= Length(Attributes) do
  begin
    if not ReadAttribute(Attributes, Index, Name, Value, HasValue) then
      Exit(False);
    if (Name = 'q') and HasValue and not AInfo.HasQ then
    begin
      AInfo.HasQ := True;
      AInfo.Q := Trim(Value);
    end;
  end;
  Result := True;
end;

function IsRFOpeningTag(const ATag: UnicodeString): Boolean;
var
  Info: TRFOpeningTagInfo;
begin
  Result := TryParseRFOpeningTag(ATag, Info);
end;

function IsRFClosingTag(const ATag: UnicodeString): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (Length(ATag) < 4) or (ATag[1] <> '<') then
    Exit;

  Index := 2;
  while (Index <= Length(ATag)) and IsAsciiWhitespace(ATag[Index]) do
    Inc(Index);
  if (Index + 1 > Length(ATag)) or
     (ATag[Index] <> 'R') or (ATag[Index + 1] <> 'f') then
    Exit;
  Inc(Index, 2);
  while (Index <= Length(ATag)) and IsAsciiWhitespace(ATag[Index]) do
    Inc(Index);
  if Index > Length(ATag) then
    Exit;
  Result := (Index = Length(ATag)) and (ATag[Index] = '>');
end;

function ReadRFNoteBody(ATokenizer: TONTTokenizer; out ABody,
  AClosingTag: string): Boolean;
var
  Token: TSyntagmToken;
begin
  Result := False;
  ABody := '';
  AClosingTag := '';
  if not Assigned(ATokenizer) then
    Exit;

  while ATokenizer.ReadSyntagm(Token) <> skNull do
  begin
    if (Token.Kind = skTag) and IsRFClosingTag(Token.Value) then
    begin
      AClosingTag := Token.Value;
      Exit(True);
    end;
    ABody := ABody + Token.Value;
  end;
end;

procedure ReadRFNoteBlock(ATokenizer: TONTTokenizer;
  var AOpeningToken: TSyntagmToken);
var
  Body: string;
  ClosingTag: string;
begin
  if not IsRFOpeningTag(AOpeningToken.Value) then
    Exit;
  ReadRFNoteBody(ATokenizer, Body, ClosingTag);
  AOpeningToken.Value := AOpeningToken.Value + Body + ClosingTag;
end;

function NormalizeEditorialCommentForRF(
  const AComments: UnicodeString): UnicodeString;
var
  Index: Integer;
begin
  Result := '';
  Index := 1;
  while Index <= Length(AComments) do
  begin
    if AComments[Index] = #13 then
    begin
      Result := Result + '<p>';
      Inc(Index);
      if (Index <= Length(AComments)) and (AComments[Index] = #10) then
        Inc(Index);
      Continue;
    end;
    if AComments[Index] = #10 then
    begin
      Result := Result + '<p>';
      Inc(Index);
      Continue;
    end;
    if StartsAsciiTextAt(AComments, '<crlf/>', Index) then
    begin
      Result := Result + '<p>';
      Inc(Index, Length('<crlf/>'));
      Continue;
    end;
    if StartsAsciiTextAt(AComments, '<lf/>', Index) then
    begin
      Result := Result + '<p>';
      Inc(Index, Length('<lf/>'));
      Continue;
    end;
    if StartsAsciiTextAt(AComments, '<br/>', Index) then
    begin
      Result := Result + '<CL>';
      Inc(Index, Length('<br/>'));
      Continue;
    end;
    if StartsAsciiTextAt(AComments, '<br>', Index) then
    begin
      Result := Result + '<CL>';
      Inc(Index, Length('<br>'));
      Continue;
    end;
    Result := Result + AComments[Index];
    Inc(Index);
  end;
end;

function AppendEditorialCommentAsRF(const AVerseText,
  AComments: UnicodeString; AEnabled: Boolean): UnicodeString;
begin
  Result := AVerseText;
  if AEnabled and (AComments <> '') then
  begin
    if ContainsStructuralRFTag(AComments) then
      raise ERFEditorialCommentProjectionError.Create(
        'Editorial comment contains a structural RF opening or closing tag. ' +
        'Remove that tag or disable editorial-comment export.');
    Result := Result + '<RF>' + NormalizeEditorialCommentForRF(AComments) +
      '<Rf>';
  end;
end;

function FindTagEnd(const AText: UnicodeString; AStart: Integer): Integer;
var
  Index: Integer;
  Quote: WideChar;
begin
  Result := 0;
  Quote := #0;
  for Index := AStart + 1 to Length(AText) do
  begin
    if Quote <> #0 then
    begin
      if AText[Index] = Quote then
        Quote := #0;
      Continue;
    end;
    if AText[Index] in ['"', ''''] then
      Quote := AText[Index]
    else if AText[Index] = '>' then
      Exit(Index);
  end;
end;

function ContainsStructuralRFTag(
  const AText: UnicodeString): Boolean;
var
  Index, TagEnd: Integer;
  RawTag: UnicodeString;
begin
  Result := False;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if AText[Index] <> '<' then
    begin
      Inc(Index);
      Continue;
    end;

    TagEnd := FindTagEnd(AText, Index);
    if TagEnd = 0 then
      Exit;
    RawTag := Copy(AText, Index, TagEnd - Index + 1);
    if IsRFOpeningTag(RawTag) or IsRFClosingTag(RawTag) then
      Exit(True);
    Index := TagEnd + 1;
  end;
end;

function FindRFClosingTag(const AText: UnicodeString; AStart: Integer;
  out ACloseEnd: Integer): Integer;
var
  Index, TagEnd: Integer;
  RawTag: UnicodeString;
begin
  Result := 0;
  ACloseEnd := 0;
  Index := AStart;
  while Index <= Length(AText) do
  begin
    if AText[Index] <> '<' then
    begin
      Inc(Index);
      Continue;
    end;
    TagEnd := FindTagEnd(AText, Index);
    if TagEnd = 0 then
      Exit;
    RawTag := Copy(AText, Index, TagEnd - Index + 1);
    if IsRFClosingTag(RawTag) then
    begin
      Result := Index;
      ACloseEnd := TagEnd;
      Exit;
    end;
    Index := TagEnd + 1;
  end;
end;

function CountAutomaticRFNotes(const AText: UnicodeString): Integer;
var
  CloseEnd, CloseIndex, Index, TagEnd: Integer;
  Info: TRFOpeningTagInfo;
  RawTag: UnicodeString;
begin
  Result := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if AText[Index] <> '<' then
    begin
      Inc(Index);
      Continue;
    end;
    TagEnd := FindTagEnd(AText, Index);
    if TagEnd = 0 then
      Exit;
    RawTag := Copy(AText, Index, TagEnd - Index + 1);
    if TryParseRFOpeningTag(RawTag, Info) then
    begin
      if not Info.HasAttributes then
        Inc(Result);
      { Match theWord's non-greedy RF container: a nested-looking opening in
        the note body does not consume a second automatic number. }
      CloseIndex := FindRFClosingTag(AText, TagEnd + 1, CloseEnd);
      if CloseIndex = 0 then
        Exit;
      Index := CloseEnd + 1;
      Continue;
    end;
    Index := TagEnd + 1;
  end;
end;

procedure TRFNoteNumberer.Reset(ABaseAutomaticCount: Integer);
begin
  if ABaseAutomaticCount < 0 then
    ABaseAutomaticCount := 0;
  FAutomaticCount := ABaseAutomaticCount;
end;

function TRFNoteNumberer.ResolveMarker(const AOpeningTag: UnicodeString;
  out AMarker: UnicodeString): Boolean;
var
  Info: TRFOpeningTagInfo;
begin
  AMarker := '';
  Result := TryParseRFOpeningTag(AOpeningTag, Info);
  if not Result then
    Exit;
  if not Info.HasAttributes then
  begin
    Inc(FAutomaticCount);
    AMarker := UnicodeString(IntToStr(FAutomaticCount));
  end
  else if Info.HasQ then
    AMarker := Info.Q;
end;

end.
