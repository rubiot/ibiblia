unit theword_inline_format_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils;

type
  TTheWordInlineFormatTagKindV2 = (
    twiftv2Unknown,
    twiftv2Bold,
    twiftv2Italic,
    twiftv2Underline,
    twiftv2Strikeout,
    twiftv2Superscript,
    twiftv2Subscript,
    twiftv2Font
  );

  { Spans are 1-based UTF-16 code-unit ranges relative to Tag.Raw.
    The value 0/0 denotes an absent span. }
  TTheWordUTF16SpanV2 = record
    StartUTF16: Integer;
    LengthUTF16: Integer;
  end;

  TTheWordInlineFormatAttributeV2 = record
    Raw: UnicodeString;
    NameRaw: UnicodeString;
    CanonicalName: UnicodeString;
    ValueRaw: UnicodeString;
    CanonicalValue: UnicodeString;
    QuoteChar: WideChar;
    WasQuoted: Boolean;
    FullSpanUTF16: TTheWordUTF16SpanV2;
    LeadingSeparatorSpanUTF16: TTheWordUTF16SpanV2;
    NameSpanUTF16: TTheWordUTF16SpanV2;
    BeforeEqualsTriviaSpanUTF16: TTheWordUTF16SpanV2;
    EqualsSpanUTF16: TTheWordUTF16SpanV2;
    AfterEqualsTriviaSpanUTF16: TTheWordUTF16SpanV2;
    LexicalValueSpanUTF16: TTheWordUTF16SpanV2;
    PayloadValueSpanUTF16: TTheWordUTF16SpanV2;
  end;

  TTheWordInlineFormatAttributeArrayV2 =
    array of TTheWordInlineFormatAttributeV2;

  TTheWordInlineFormatTagV2 = record
    Raw: UnicodeString;
    NameRaw: UnicodeString;
    CanonicalName: UnicodeString;
    Kind: TTheWordInlineFormatTagKindV2;
    IsClosing: Boolean;
    RawSpanUTF16: TTheWordUTF16SpanV2;
    NameSpanUTF16: TTheWordUTF16SpanV2;
    ClosingTriviaSpanUTF16: TTheWordUTF16SpanV2;
    Attributes: TTheWordInlineFormatAttributeArrayV2;
    HasColor: Boolean;
    ColorNameRaw: UnicodeString;
    ResolvedColor: LongInt;
    HasSize: Boolean;
    RelativeSize: Integer;
    SizeSignRaw: WideChar;
    HasFontName: Boolean;
    FontNameRaw: UnicodeString;
  end;

function TryClassifyTheWordSimpleInlineFormatNameV2(const AName: UnicodeString;
  out AKind: TTheWordInlineFormatTagKindV2): Boolean;
function TryResolveTheWordInlineColorV2(const AValue: UnicodeString;
  out AResolvedColor: LongInt; out AColorNameRaw: UnicodeString;
  out ACanonicalValue: UnicodeString): Boolean;
function TryParseTheWordInlineFormatTagV2(const ARaw: UnicodeString;
  out ATag: TTheWordInlineFormatTagV2;
  out AError: UnicodeString): Boolean;

implementation

type
  TTheWordNamedColorV2 = record
    Name: UnicodeString;
    Value: LongInt;
  end;

const
  THEWORD_NAMED_COLORS_V2: array[0..15] of TTheWordNamedColorV2 = (
    (Name: 'white'; Value: $00FFFFFF),
    (Name: 'black'; Value: $00000000),
    (Name: 'red'; Value: $000000FF),
    (Name: 'green'; Value: $00008000),
    (Name: 'blue'; Value: $00FF0000),
    (Name: 'aqua'; Value: $00FFFF00),
    (Name: 'fuchsia'; Value: $00FF00FF),
    (Name: 'gray'; Value: $00808080),
    (Name: 'lime'; Value: $0000FF00),
    (Name: 'maroon'; Value: $00000080),
    (Name: 'navy'; Value: $00800000),
    (Name: 'olive'; Value: $00008080),
    (Name: 'purple'; Value: $00800080),
    (Name: 'silver'; Value: $00808080),
    (Name: 'teal'; Value: $00808000),
    (Name: 'yellow'; Value: $0000FFFF)
  );

function MakeSpan(AStartUTF16, ALengthUTF16: Integer):
  TTheWordUTF16SpanV2;
begin
  Result.StartUTF16 := AStartUTF16;
  Result.LengthUTF16 := ALengthUTF16;
end;

function IsTagWhitespace(AChar: WideChar): Boolean;
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
  IndexUTF16: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for IndexUTF16 := 1 to Length(ALeft) do
    if FoldAscii(ALeft[IndexUTF16]) <> FoldAscii(ARight[IndexUTF16]) then
      Exit(False);
  Result := True;
end;

function LowerAscii(const AValue: UnicodeString): UnicodeString;
var
  IndexUTF16: Integer;
begin
  SetLength(Result, Length(AValue));
  for IndexUTF16 := 1 to Length(AValue) do
    Result[IndexUTF16] := FoldAscii(AValue[IndexUTF16]);
end;

function TrimTagWhitespace(const AValue: UnicodeString): UnicodeString;
var
  FirstUTF16, LastUTF16: Integer;
begin
  FirstUTF16 := 1;
  LastUTF16 := Length(AValue);
  while (FirstUTF16 <= LastUTF16) and
    IsTagWhitespace(AValue[FirstUTF16]) do
    Inc(FirstUTF16);
  while (LastUTF16 >= FirstUTF16) and
    IsTagWhitespace(AValue[LastUTF16]) do
    Dec(LastUTF16);
  Result := Copy(AValue, FirstUTF16, LastUTF16 - FirstUTF16 + 1);
end;

function HexDigitValue(AChar: WideChar; out AValue: Integer): Boolean;
begin
  if AChar in ['0'..'9'] then
    AValue := Ord(AChar) - Ord('0')
  else if AChar in ['a'..'f'] then
    AValue := Ord(AChar) - Ord('a') + 10
  else if AChar in ['A'..'F'] then
    AValue := Ord(AChar) - Ord('A') + 10
  else
    Exit(False);
  Result := True;
end;

function UpperHexDigit(AValue: Integer): WideChar;
const
  DIGITS: UnicodeString = '0123456789ABCDEF';
begin
  Result := DIGITS[AValue + 1];
end;

function TryClassifyTheWordSimpleInlineFormatNameV2(const AName: UnicodeString;
  out AKind: TTheWordInlineFormatTagKindV2): Boolean;
begin
  if AsciiSameText(AName, 'b') then
    AKind := twiftv2Bold
  else if AsciiSameText(AName, 'i') then
    AKind := twiftv2Italic
  else if AsciiSameText(AName, 'u') then
    AKind := twiftv2Underline
  else if AsciiSameText(AName, 's') then
    AKind := twiftv2Strikeout
  else if AsciiSameText(AName, 'sup') then
    AKind := twiftv2Superscript
  else if AsciiSameText(AName, 'sub') then
    AKind := twiftv2Subscript
  else
  begin
    AKind := twiftv2Unknown;
    Exit(False);
  end;
  Result := True;
end;

function TryResolveTheWordInlineColorV2(const AValue: UnicodeString;
  out AResolvedColor: LongInt; out AColorNameRaw: UnicodeString;
  out ACanonicalValue: UnicodeString): Boolean;
var
  B, G, IndexUTF16, OffsetUTF16, R, TableIndex, Value: Integer;
  Work: UnicodeString;
begin
  AResolvedColor := 0;
  AColorNameRaw := '';
  ACanonicalValue := '';
  Work := TrimTagWhitespace(AValue);
  for TableIndex := Low(THEWORD_NAMED_COLORS_V2) to
    High(THEWORD_NAMED_COLORS_V2) do
    if AsciiSameText(Work, THEWORD_NAMED_COLORS_V2[TableIndex].Name) then
    begin
      AColorNameRaw := Work;
      ACanonicalValue := THEWORD_NAMED_COLORS_V2[TableIndex].Name;
      AResolvedColor := THEWORD_NAMED_COLORS_V2[TableIndex].Value;
      Exit(True);
    end;
  OffsetUTF16 := 1;
  if (Length(Work) = 7) and (Work[1] = '#') then
    OffsetUTF16 := 2
  else if Length(Work) <> 6 then
    OffsetUTF16 := 0;
  if OffsetUTF16 <> 0 then
  begin
    for IndexUTF16 := OffsetUTF16 to OffsetUTF16 + 5 do
      if not HexDigitValue(Work[IndexUTF16], Value) then
        Exit(False);
    HexDigitValue(Work[OffsetUTF16], Value);
    R := Value shl 4;
    HexDigitValue(Work[OffsetUTF16 + 1], Value);
    R := R or Value;
    HexDigitValue(Work[OffsetUTF16 + 2], Value);
    G := Value shl 4;
    HexDigitValue(Work[OffsetUTF16 + 3], Value);
    G := G or Value;
    HexDigitValue(Work[OffsetUTF16 + 4], Value);
    B := Value shl 4;
    HexDigitValue(Work[OffsetUTF16 + 5], Value);
    B := B or Value;
    AResolvedColor := R or (G shl 8) or (B shl 16);
    ACanonicalValue := '#';
    for IndexUTF16 := OffsetUTF16 to OffsetUTF16 + 5 do
    begin
      HexDigitValue(Work[IndexUTF16], Value);
      ACanonicalValue := ACanonicalValue + UpperHexDigit(Value);
    end;
    Exit(True);
  end;
  Result := False;
end;

function TryParseRelativeSize(const AValue: UnicodeString;
  out ARelativeSize: Integer; out ASignRaw: WideChar;
  out ACanonicalValue: UnicodeString): Boolean;
var
  Digit, IndexUTF16, Limit, Magnitude: Integer;
begin
  ARelativeSize := 0;
  ASignRaw := #0;
  ACanonicalValue := '';
  if (Length(AValue) < 2) or not (AValue[1] in ['+', '-']) then
    Exit(False);
  ASignRaw := AValue[1];
  if ASignRaw = '-' then
    Limit := 4
  else
    Limit := 10;
  Magnitude := 0;
  for IndexUTF16 := 2 to Length(AValue) do
  begin
    if not (AValue[IndexUTF16] in ['0'..'9']) then
      Exit(False);
    Digit := Ord(AValue[IndexUTF16]) - Ord('0');
    if Magnitude > (Limit - Digit) div 10 then
      Exit(False);
    Magnitude := Magnitude * 10 + Digit;
  end;
  if Magnitude > Limit then
    Exit(False);
  if ASignRaw = '-' then
    ARelativeSize := -Magnitude
  else
    ARelativeSize := Magnitude;
  ACanonicalValue := UnicodeString(ASignRaw) + IntToStr(Magnitude);
  Result := True;
end;

function IsValidFontName(const AValue: UnicodeString): Boolean;
var
  IndexUTF16: Integer;
begin
  if TrimTagWhitespace(AValue) = '' then
    Exit(False);
  for IndexUTF16 := 1 to Length(AValue) do
    if (Ord(AValue[IndexUTF16]) < 32) or
       (Ord(AValue[IndexUTF16]) = 127) or
       (AValue[IndexUTF16] in ['<' , '>']) then
      Exit(False);
  Result := True;
end;

procedure ClearTag(out ATag: TTheWordInlineFormatTagV2);
begin
  ATag.Raw := '';
  ATag.NameRaw := '';
  ATag.CanonicalName := '';
  ATag.Kind := twiftv2Unknown;
  ATag.IsClosing := False;
  ATag.RawSpanUTF16 := MakeSpan(0, 0);
  ATag.NameSpanUTF16 := MakeSpan(0, 0);
  ATag.ClosingTriviaSpanUTF16 := MakeSpan(0, 0);
  SetLength(ATag.Attributes, 0);
  ATag.HasColor := False;
  ATag.ColorNameRaw := '';
  ATag.ResolvedColor := 0;
  ATag.HasSize := False;
  ATag.RelativeSize := 0;
  ATag.SizeSignRaw := #0;
  ATag.HasFontName := False;
  ATag.FontNameRaw := '';
end;

function FailParse(const AMessage: UnicodeString;
  out AError: UnicodeString): Boolean;
begin
  AError := AMessage;
  Result := False;
end;

function TryParseTheWordInlineFormatTagV2(const ARaw: UnicodeString;
  out ATag: TTheWordInlineFormatTagV2;
  out AError: UnicodeString): Boolean;
var
  AttributeCount, AttributeStartUTF16, BeforeEqualsStartUTF16: Integer;
  IndexUTF16, LeadingStartUTF16, LexicalStartUTF16, NameStartUTF16: Integer;
  PayloadEndUTF16, PayloadStartUTF16, TriviaStartUTF16: Integer;
  Attribute: TTheWordInlineFormatAttributeV2;
  CanonicalColor, ColorNameRaw: UnicodeString;
  Kind: TTheWordInlineFormatTagKindV2;
  ResolvedColor: LongInt;
begin
  ClearTag(ATag);
  AError := '';
  ATag.Raw := ARaw;
  ATag.RawSpanUTF16 := MakeSpan(1, Length(ARaw));
  if (Length(ARaw) >= 3) and (ARaw[1] = '<') and
     (ARaw[Length(ARaw)] = '>') then
  begin
    if ARaw[2] = '/' then
    begin
      ATag.NameRaw := Copy(ARaw, 3, Length(ARaw) - 3);
      ATag.NameSpanUTF16 := MakeSpan(3, Length(ATag.NameRaw));
      if TryClassifyTheWordSimpleInlineFormatNameV2(ATag.NameRaw, Kind) then
      begin
        ATag.Kind := Kind;
        ATag.CanonicalName := LowerAscii(ATag.NameRaw);
        ATag.IsClosing := True;
        Exit(True);
      end;
    end
    else
    begin
      ATag.NameRaw := Copy(ARaw, 2, Length(ARaw) - 2);
      ATag.NameSpanUTF16 := MakeSpan(2, Length(ATag.NameRaw));
      if TryClassifyTheWordSimpleInlineFormatNameV2(ATag.NameRaw, Kind) then
      begin
        ATag.Kind := Kind;
        ATag.CanonicalName := LowerAscii(ATag.NameRaw);
        Exit(True);
      end;
    end;
  end;
  if ARaw = '</font>' then
  begin
    ATag.NameRaw := 'font';
    ATag.CanonicalName := 'font';
    ATag.Kind := twiftv2Font;
    ATag.IsClosing := True;
    ATag.NameSpanUTF16 := MakeSpan(3, 4);
    Exit(True);
  end;
  if (Length(ARaw) < 7) or (Copy(ARaw, 1, 5) <> '<font') or
     not IsTagWhitespace(ARaw[6]) or (ARaw[Length(ARaw)] <> '>') then
    Exit(FailParse('Unsupported or malformed inline format tag', AError));
  ATag.NameRaw := 'font';
  ATag.CanonicalName := 'font';
  ATag.Kind := twiftv2Font;
  ATag.NameSpanUTF16 := MakeSpan(2, 4);
  AttributeCount := 0;
  IndexUTF16 := 6;
  while IndexUTF16 <= Length(ARaw) do
  begin
    LeadingStartUTF16 := IndexUTF16;
    while (IndexUTF16 < Length(ARaw)) and
      IsTagWhitespace(ARaw[IndexUTF16]) do
      Inc(IndexUTF16);
    if ARaw[IndexUTF16] = '>' then
    begin
      if IndexUTF16 <> Length(ARaw) then
        Exit(FailParse('Trailing data after font tag', AError));
      if AttributeCount = 0 then
        Exit(FailParse('A font opening tag requires an attribute', AError));
      ATag.ClosingTriviaSpanUTF16 := MakeSpan(LeadingStartUTF16,
        IndexUTF16 - LeadingStartUTF16);
      Exit(True);
    end;
    if (IndexUTF16 >= Length(ARaw)) or (ARaw[IndexUTF16] = '/') then
      Exit(FailParse('Self-closing font tags are not supported', AError));
    AttributeStartUTF16 := LeadingStartUTF16;
    NameStartUTF16 := IndexUTF16;
    while (IndexUTF16 < Length(ARaw)) and
      (ARaw[IndexUTF16] in ['a'..'z', 'A'..'Z']) do
      Inc(IndexUTF16);
    if IndexUTF16 = NameStartUTF16 then
      Exit(FailParse('Malformed font attribute name', AError));
    Attribute.NameRaw := Copy(ARaw, NameStartUTF16,
      IndexUTF16 - NameStartUTF16);
    Attribute.CanonicalName := Attribute.NameRaw;
    BeforeEqualsStartUTF16 := IndexUTF16;
    while (IndexUTF16 < Length(ARaw)) and
      IsTagWhitespace(ARaw[IndexUTF16]) do
      Inc(IndexUTF16);
    if (IndexUTF16 >= Length(ARaw)) or (ARaw[IndexUTF16] <> '=') then
      Exit(FailParse('A font attribute requires an equals sign', AError));
    Attribute.BeforeEqualsTriviaSpanUTF16 := MakeSpan(
      BeforeEqualsStartUTF16, IndexUTF16 - BeforeEqualsStartUTF16);
    Attribute.EqualsSpanUTF16 := MakeSpan(IndexUTF16, 1);
    Inc(IndexUTF16);
    TriviaStartUTF16 := IndexUTF16;
    while (IndexUTF16 < Length(ARaw)) and
      IsTagWhitespace(ARaw[IndexUTF16]) do
      Inc(IndexUTF16);
    Attribute.AfterEqualsTriviaSpanUTF16 := MakeSpan(TriviaStartUTF16,
      IndexUTF16 - TriviaStartUTF16);
    if (IndexUTF16 >= Length(ARaw)) or (ARaw[IndexUTF16] = '>') then
      Exit(FailParse('A font attribute value cannot be empty', AError));
    LexicalStartUTF16 := IndexUTF16;
    Attribute.WasQuoted := ARaw[IndexUTF16] in ['"', ''''];
    if Attribute.WasQuoted then
    begin
      Attribute.QuoteChar := ARaw[IndexUTF16];
      Inc(IndexUTF16);
      PayloadStartUTF16 := IndexUTF16;
      while (IndexUTF16 < Length(ARaw)) and
        (ARaw[IndexUTF16] <> Attribute.QuoteChar) do
        Inc(IndexUTF16);
      if IndexUTF16 >= Length(ARaw) then
        Exit(FailParse('Unclosed quoted font attribute value', AError));
      PayloadEndUTF16 := IndexUTF16 - 1;
      Inc(IndexUTF16);
    end
    else
    begin
      Attribute.QuoteChar := #0;
      PayloadStartUTF16 := IndexUTF16;
      while (IndexUTF16 < Length(ARaw)) and
        not IsTagWhitespace(ARaw[IndexUTF16]) and
        (ARaw[IndexUTF16] <> '>') do
      begin
        if ARaw[IndexUTF16] in ['"', '''', '<', '='] then
          Exit(FailParse('Malformed unquoted font attribute value', AError));
        Inc(IndexUTF16);
      end;
      PayloadEndUTF16 := IndexUTF16 - 1;
    end;
    if PayloadEndUTF16 < PayloadStartUTF16 then
      Exit(FailParse('A font attribute value cannot be empty', AError));
    if (not Attribute.WasQuoted) and
       (ARaw[PayloadEndUTF16] = '/') then
      Exit(FailParse('Self-closing font tags are not supported', AError));
    if (IndexUTF16 < Length(ARaw)) and
       not IsTagWhitespace(ARaw[IndexUTF16]) and
       (ARaw[IndexUTF16] <> '>') then
      Exit(FailParse('Font attributes require separating whitespace', AError));
    Attribute.ValueRaw := Copy(ARaw, PayloadStartUTF16,
      PayloadEndUTF16 - PayloadStartUTF16 + 1);
    Attribute.LeadingSeparatorSpanUTF16 := MakeSpan(LeadingStartUTF16,
      NameStartUTF16 - LeadingStartUTF16);
    Attribute.NameSpanUTF16 := MakeSpan(NameStartUTF16,
      Length(Attribute.NameRaw));
    Attribute.LexicalValueSpanUTF16 := MakeSpan(LexicalStartUTF16,
      IndexUTF16 - LexicalStartUTF16);
    Attribute.PayloadValueSpanUTF16 := MakeSpan(PayloadStartUTF16,
      PayloadEndUTF16 - PayloadStartUTF16 + 1);
    Attribute.FullSpanUTF16 := MakeSpan(AttributeStartUTF16,
      IndexUTF16 - AttributeStartUTF16);
    Attribute.Raw := Copy(ARaw, Attribute.FullSpanUTF16.StartUTF16,
      Attribute.FullSpanUTF16.LengthUTF16);
    if Attribute.NameRaw = 'color' then
    begin
      if ATag.HasColor then
        Exit(FailParse('Duplicate color attribute', AError));
      if not TryResolveTheWordInlineColorV2(Attribute.ValueRaw,
        ResolvedColor, ColorNameRaw, CanonicalColor) then
        Exit(FailParse('Invalid font color', AError));
      Attribute.CanonicalValue := CanonicalColor;
      ATag.HasColor := True;
      ATag.ColorNameRaw := ColorNameRaw;
      ATag.ResolvedColor := ResolvedColor;
    end
    else if Attribute.NameRaw = 'size' then
    begin
      if ATag.HasSize then
        Exit(FailParse('Duplicate size attribute', AError));
      if not TryParseRelativeSize(Attribute.ValueRaw, ATag.RelativeSize,
        ATag.SizeSignRaw, Attribute.CanonicalValue) then
        Exit(FailParse('Invalid relative font size', AError));
      ATag.HasSize := True;
    end
    else if Attribute.NameRaw = 'name' then
    begin
      if ATag.HasFontName then
        Exit(FailParse('Duplicate name attribute', AError));
      if not IsValidFontName(Attribute.ValueRaw) then
        Exit(FailParse('Invalid font name', AError));
      Attribute.CanonicalValue := Attribute.ValueRaw;
      ATag.HasFontName := True;
      ATag.FontNameRaw := Attribute.ValueRaw;
    end
    else
      Exit(FailParse('Unknown or incorrectly cased font attribute', AError));
    SetLength(ATag.Attributes, AttributeCount + 1);
    ATag.Attributes[AttributeCount] := Attribute;
    Inc(AttributeCount);
  end;
  Result := FailParse('Unclosed font opening tag', AError);
end;

end.
