unit theword_translator_note_body_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, Contnrs, SysUtils;

type
  TTheWordNoteBodyCapability = (
    twnbcFullWysiwyg,
    twnbcProtectedRegions,
    twnbcRawOnly
  );

  TTheWordNoteBodyTokenKind = (
    twnbtkText,
    twnbtkTagOpen,
    twnbtkTagClose,
    twnbtkParagraphBreak,
    twnbtkLineBreak,
    twnbtkOpaque
  );

  TTheWordNoteBodyTagKind = (
    twnbBold,
    twnbItalic,
    twnbUnderline,
    twnbStrike,
    twnbSuperscript,
    twnbSubscript,
    twnbFont,
    twnbSpanBackground,
    twnbLink,
    twnbLtr,
    twnbRtl,
    twnbLtrParagraph,
    twnbRtlParagraph
  );

  TTheWordNoteBodyToken = class
  private
    FKind: TTheWordNoteBodyTokenKind;
    FTagKind: TTheWordNoteBodyTagKind;
    FText: UnicodeString;
    FRawLexeme: UnicodeString;
    FFontName: UnicodeString;
    FFontSize: UnicodeString;
    FForegroundColor: UnicodeString;
    FBackgroundColor: UnicodeString;
    FHref: UnicodeString;
  public
    property Kind: TTheWordNoteBodyTokenKind read FKind write FKind;
    property TagKind: TTheWordNoteBodyTagKind read FTagKind write FTagKind;
    property Text: UnicodeString read FText write FText;
    property RawLexeme: UnicodeString read FRawLexeme write FRawLexeme;
    property FontName: UnicodeString read FFontName write FFontName;
    property FontSize: UnicodeString read FFontSize write FFontSize;
    property ForegroundColor: UnicodeString read FForegroundColor
      write FForegroundColor;
    property BackgroundColor: UnicodeString read FBackgroundColor
      write FBackgroundColor;
    property Href: UnicodeString read FHref write FHref;
  end;

  TTheWordTranslatorNoteBodyDocument = class
  private
    FTokens: TObjectList;
    FOriginalRaw: UnicodeString;
    FCapability: TTheWordNoteBodyCapability;
    FModified: Boolean;
    function GetToken(AIndex: Integer): TTheWordNoteBodyToken;
    function GetTokenCount: Integer;
    function AddToken(AKind: TTheWordNoteBodyTokenKind):
      TTheWordNoteBodyToken;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddText(const AText: UnicodeString): TTheWordNoteBodyToken;
    function AddOpenTag(AKind: TTheWordNoteBodyTagKind):
      TTheWordNoteBodyToken;
    function AddCloseTag(AKind: TTheWordNoteBodyTagKind):
      TTheWordNoteBodyToken;
    function AddParagraphBreak: TTheWordNoteBodyToken;
    function AddLineBreak: TTheWordNoteBodyToken;
    procedure MarkModified;
    property TokenCount: Integer read GetTokenCount;
    property Tokens[AIndex: Integer]: TTheWordNoteBodyToken read GetToken;
    property OriginalRaw: UnicodeString read FOriginalRaw;
    property Capability: TTheWordNoteBodyCapability read FCapability;
    property Modified: Boolean read FModified;
  end;

  TTheWordTranslatorNoteBodyCodec = class
  public
    class function Decode(const ARaw: UnicodeString):
      TTheWordTranslatorNoteBodyDocument; static;
    class function Encode(ADocument: TTheWordTranslatorNoteBodyDocument):
      UnicodeString; static;
    class function Analyze(const ARaw: UnicodeString):
      TTheWordNoteBodyCapability; static;
    class function TryCanonicalize(const ARaw: UnicodeString;
      out ACanonical: UnicodeString;
      out ACapability: TTheWordNoteBodyCapability): Boolean; static;
  end;

function TryTheWordTranslatorNoteHtmlColor(const AValue: UnicodeString;
  out AColor: LongInt): Boolean;

implementation

type
  TTheWordNoteBodyStackEntry = class
  public
    Kind: TTheWordNoteBodyTagKind;
  end;

  TTheWordNamedColor = record
    Name: string;
    Value: LongInt;
  end;

const
  { These are the 16 names handled by theWord's HTML parser. Its "silver"
    intentionally resolves to the same COLORREF as "gray". }
  THEWORD_NAMED_COLORS: array[0..15] of TTheWordNamedColor = (
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

function TrimUnicode(const AValue: UnicodeString): UnicodeString;
var
  FirstIndex, LastIndex: Integer;
begin
  FirstIndex := 1;
  LastIndex := Length(AValue);
  while (FirstIndex <= LastIndex) and
    (AValue[FirstIndex] in [' ', #9, #10, #13]) do
    Inc(FirstIndex);
  while (LastIndex >= FirstIndex) and
    (AValue[LastIndex] in [' ', #9, #10, #13]) do
    Dec(LastIndex);
  Result := Copy(AValue, FirstIndex, LastIndex - FirstIndex + 1);
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
    if (AText[Index] = '"') or (AText[Index] = '''') then
      Quote := AText[Index]
    else if AText[Index] = '>' then
      Exit(Index);
  end;
end;

function DecodeEntities(const AValue: UnicodeString): UnicodeString;
begin
  { The module pipeline's extractTagValue returns the literal attribute
    payload.  It does not HTML-decode entities, so doing so here would make
    the iBiblia preview disagree with theWord. }
  Result := AValue;
end;

function EncodeAttribute(const AValue: UnicodeString): UnicodeString;
var
  Index: Integer;
begin
  for Index := 1 to Length(AValue) do
    if AValue[Index] in ['"', '<', '>', #10, #13] then
      raise EArgumentException.Create(
        'Attribute cannot be represented by theWord without loss');
  { Ampersands and apostrophes are literal in the parser used by theWord. }
  Result := AValue;
end;

function TryReadAttribute(const AText, AName: UnicodeString;
  out AValue: UnicodeString): Boolean;
var
  EqualsIndex, Index, NameStart, ValueStart: Integer;
  Quote: WideChar;
  Name: UnicodeString;
begin
  Result := False;
  AValue := '';
  Index := 1;
  while Index <= Length(AText) do
  begin
    while (Index <= Length(AText)) and
      (AText[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    NameStart := Index;
    while (Index <= Length(AText)) and not
      (AText[Index] in [' ', #9, #10, #13, '=']) do
      Inc(Index);
    Name := Copy(AText, NameStart, Index - NameStart);
    while (Index <= Length(AText)) and
      (AText[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    EqualsIndex := Index;
    if (Index <= Length(AText)) and (AText[Index] = '=') then
      Inc(Index)
    else
    begin
      if AsciiSameText(Name, AName) then
        Exit(False);
      Continue;
    end;
    while (Index <= Length(AText)) and
      (AText[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    if Index > Length(AText) then
      Exit(False);
    Quote := #0;
    if AText[Index] in ['"', ''''] then
    begin
      Quote := AText[Index];
      Inc(Index);
    end;
    ValueStart := Index;
    if Quote <> #0 then
    begin
      while (Index <= Length(AText)) and (AText[Index] <> Quote) do
        Inc(Index);
      if Index > Length(AText) then
        Exit(False);
      if AsciiSameText(Name, AName) then
      begin
        AValue := DecodeEntities(Copy(AText, ValueStart,
          Index - ValueStart));
        Exit(True);
      end;
      Inc(Index);
    end
    else
    begin
      while (Index <= Length(AText)) and not
        (AText[Index] in [' ', #9, #10, #13]) do
        Inc(Index);
      if AsciiSameText(Name, AName) then
      begin
        AValue := DecodeEntities(Copy(AText, ValueStart,
          Index - ValueStart));
        Exit(True);
      end;
    end;
    if EqualsIndex = 0 then
      Break;
  end;
end;

function AttributesAreOnly(const AText: UnicodeString;
  const AAllowed: array of UnicodeString): Boolean;
var
  AllowedIndex, Index, NameStart, ValueStart: Integer;
  Found: Boolean;
  Name: UnicodeString;
  Quote: WideChar;
  Seen: array of Boolean;
begin
  Seen := nil;
  SetLength(Seen, Length(AAllowed));
  Index := 1;
  while Index <= Length(AText) do
  begin
    while (Index <= Length(AText)) and
      (AText[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    if Index > Length(AText) then
      Break;
    NameStart := Index;
    while (Index <= Length(AText)) and not
      (AText[Index] in [' ', #9, #10, #13, '=']) do
      Inc(Index);
    if Index = NameStart then
      Exit(False);
    Name := Copy(AText, NameStart, Index - NameStart);
    Found := False;
    for AllowedIndex := 0 to High(AAllowed) do
      if Name = AAllowed[AllowedIndex] then
      begin
        if Seen[AllowedIndex] then
          Exit(False);
        Seen[AllowedIndex] := True;
        Found := True;
        Break;
      end;
    if not Found then
      Exit(False);
    while (Index <= Length(AText)) and
      (AText[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    if (Index > Length(AText)) or (AText[Index] <> '=') then
      Exit(False);
    Inc(Index);
    while (Index <= Length(AText)) and
      (AText[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    if Index > Length(AText) then
      Exit(False);
    Quote := #0;
    if AText[Index] in ['"', ''''] then
    begin
      Quote := AText[Index];
      Inc(Index);
    end;
    ValueStart := Index;
    if Quote <> #0 then
    begin
      while (Index <= Length(AText)) and (AText[Index] <> Quote) do
        Inc(Index);
      if Index > Length(AText) then
        Exit(False);
      Inc(Index);
      if (Index <= Length(AText)) and not
        (AText[Index] in [' ', #9, #10, #13]) then
        Exit(False);
    end
    else
    begin
      while (Index <= Length(AText)) and not
        (AText[Index] in [' ', #9, #10, #13]) do
        Inc(Index);
      if Index = ValueStart then
        Exit(False);
    end;
  end;
  Result := True;
end;

function IsSafeLink(const AValue: UnicodeString): Boolean;
var
  Index, PrefixLength: Integer;
  Work: string;
begin
  Work := LowerCase(UTF8Encode(AValue));
  if Copy(Work, 1, 7) = 'http://' then
    PrefixLength := 7
  else if Copy(Work, 1, 8) = 'https://' then
    PrefixLength := 8
  else if Copy(Work, 1, 5) = 'tw://' then
    PrefixLength := 5
  else
    Exit(False);
  if Length(AValue) <= PrefixLength then Exit(False);
  for Index := 1 to Length(AValue) do
    if (Ord(AValue[Index]) <= 32) or (AValue[Index] = '"') or
       (AValue[Index] = '''') or (AValue[Index] = '<') or
       (AValue[Index] = '>') then Exit(False);
  Result := True;
end;

function IsRepresentableAttribute(const AValue: UnicodeString): Boolean;
var
  Index: Integer;
begin
  for Index := 1 to Length(AValue) do
    if AValue[Index] in ['"', '<', '>', #10, #13] then
      Exit(False);
  Result := True;
end;

function TryTheWordTranslatorNoteHtmlColor(const AValue: UnicodeString;
  out AColor: LongInt): Boolean;
var
  B, G, Index, R, TableIndex: Integer;
  Work: string;
begin
  AColor := 0;
  Work := UTF8Encode(TrimUnicode(AValue));
  Result := (Length(Work) = 7) and (Work[1] = '#');
  if Result then
  begin
    for Index := 2 to 7 do
      if not (Work[Index] in ['0'..'9', 'a'..'f', 'A'..'F']) then
        Exit(False);
    if not TryStrToInt('$' + Copy(Work, 2, 2), R) or
       not TryStrToInt('$' + Copy(Work, 4, 2), G) or
       not TryStrToInt('$' + Copy(Work, 6, 2), B) then
      Exit(False);
    AColor := R or (G shl 8) or (B shl 16);
  end
  else
  begin
    for TableIndex := Low(THEWORD_NAMED_COLORS) to
      High(THEWORD_NAMED_COLORS) do
      if SameText(THEWORD_NAMED_COLORS[TableIndex].Name, Work) then
      begin
        Result := True;
        AColor := THEWORD_NAMED_COLORS[TableIndex].Value;
        Break;
      end;
    if not Result then Exit;
  end;
end;

function IsVisualHtmlColor(const AValue: UnicodeString): Boolean;
var
  Color: LongInt;
begin
  Result := TryTheWordTranslatorNoteHtmlColor(AValue, Color);
end;

function IsVisualRelativeFontSize(const AValue: UnicodeString): Boolean;
var
  Index, Value: Integer;
  Work: UnicodeString;
begin
  Work := TrimUnicode(AValue);
  Result := (Length(Work) >= 2) and (Work[1] in ['+', '-']);
  if not Result then Exit;
  for Index := 2 to Length(Work) do
    if not (Work[Index] in ['0'..'9']) then Exit(False);
  if not TryStrToInt(UTF8Encode(Work), Value) then Exit(False);
  Result := ((Value >= -4) and (Value <= -1)) or
    ((Value >= 1) and (Value <= 10));
end;

function ContainsFreeRtfPayload(const AValue: UnicodeString): Boolean;
begin
  Result := Pos('{\rtf', LowerCase(UTF8Encode(AValue))) > 0;
end;

function TrySimpleTagKind(const AName: UnicodeString;
  out AKind: TTheWordNoteBodyTagKind): Boolean;
begin
  Result := True;
  if AsciiSameText(AName, 'b') then AKind := twnbBold
  else if AsciiSameText(AName, 'i') then AKind := twnbItalic
  else if AsciiSameText(AName, 'u') then AKind := twnbUnderline
  else if AsciiSameText(AName, 's') then AKind := twnbStrike
  else if AsciiSameText(AName, 'sup') then AKind := twnbSuperscript
  else if AsciiSameText(AName, 'sub') then AKind := twnbSubscript
  else if AsciiSameText(AName, 'ltr') then AKind := twnbLtr
  else if AsciiSameText(AName, 'rtl') then AKind := twnbRtl
  else if AsciiSameText(AName, 'ltrpar') then AKind := twnbLtrParagraph
  else if AsciiSameText(AName, 'rtlpar') then AKind := twnbRtlParagraph
  else Result := False;
end;

function IsKnownProtectedTag(const AName: UnicodeString): Boolean;
var
  Work: UnicodeString;
begin
  Work := UpperCase(UTF8Encode(AName));
  Result := (Work = 'FI') or (Work = 'FR') or (Work = 'FO') or
    (Work = 'FU') or (Work = 'TS') or (Work = 'RX') or
    (Copy(Work, 1, 2) = 'PI') or (Copy(Work, 1, 2) = 'PF') or
    (Copy(Work, 1, 2) = 'WG') or (Copy(Work, 1, 2) = 'WH') or
    (Copy(Work, 1, 2) = 'WT') or (Work = 'RTF');
end;

function CanonicalTagName(AKind: TTheWordNoteBodyTagKind): UnicodeString;
begin
  case AKind of
    twnbBold: Result := 'b';
    twnbItalic: Result := 'i';
    twnbUnderline: Result := 'u';
    twnbStrike: Result := 's';
    twnbSuperscript: Result := 'sup';
    twnbSubscript: Result := 'sub';
    twnbFont: Result := 'font';
    twnbSpanBackground: Result := 'span';
    twnbLink: Result := 'a';
    twnbLtr: Result := 'ltr';
    twnbRtl: Result := 'rtl';
    twnbLtrParagraph: Result := 'ltrpar';
    twnbRtlParagraph: Result := 'rtlpar';
  end;
end;

{ TTheWordTranslatorNoteBodyDocument }

constructor TTheWordTranslatorNoteBodyDocument.Create;
begin
  inherited Create;
  FTokens := TObjectList.Create(True);
  FCapability := twnbcFullWysiwyg;
end;

destructor TTheWordTranslatorNoteBodyDocument.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

function TTheWordTranslatorNoteBodyDocument.GetToken(AIndex: Integer):
  TTheWordNoteBodyToken;
begin
  Result := TTheWordNoteBodyToken(FTokens[AIndex]);
end;

function TTheWordTranslatorNoteBodyDocument.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

function TTheWordTranslatorNoteBodyDocument.AddToken(
  AKind: TTheWordNoteBodyTokenKind): TTheWordNoteBodyToken;
begin
  Result := TTheWordNoteBodyToken.Create;
  Result.FKind := AKind;
  FTokens.Add(Result);
  FModified := True;
end;

procedure TTheWordTranslatorNoteBodyDocument.Clear;
begin
  FTokens.Clear;
  FModified := True;
end;

function TTheWordTranslatorNoteBodyDocument.AddText(
  const AText: UnicodeString): TTheWordNoteBodyToken;
begin
  Result := AddToken(twnbtkText);
  Result.FText := AText;
end;

function TTheWordTranslatorNoteBodyDocument.AddOpenTag(
  AKind: TTheWordNoteBodyTagKind): TTheWordNoteBodyToken;
begin
  Result := AddToken(twnbtkTagOpen);
  Result.FTagKind := AKind;
end;

function TTheWordTranslatorNoteBodyDocument.AddCloseTag(
  AKind: TTheWordNoteBodyTagKind): TTheWordNoteBodyToken;
begin
  Result := AddToken(twnbtkTagClose);
  Result.FTagKind := AKind;
end;

function TTheWordTranslatorNoteBodyDocument.AddParagraphBreak:
  TTheWordNoteBodyToken;
begin
  Result := AddToken(twnbtkParagraphBreak);
end;

function TTheWordTranslatorNoteBodyDocument.AddLineBreak:
  TTheWordNoteBodyToken;
begin
  Result := AddToken(twnbtkLineBreak);
end;

procedure TTheWordTranslatorNoteBodyDocument.MarkModified;
begin
  FModified := True;
end;

{ TTheWordTranslatorNoteBodyCodec }

class function TTheWordTranslatorNoteBodyCodec.Decode(
  const ARaw: UnicodeString): TTheWordTranslatorNoteBodyDocument;
var
  Attrs, Name, RawTag, Style, Value: UnicodeString;
  Closing: Boolean;
  Index, NameEnd, TagEnd, TextStart: Integer;
  Kind: TTheWordNoteBodyTagKind;
  Entry: TTheWordNoteBodyStackEntry;
  Stack: TObjectList;
  Token: TTheWordNoteBodyToken;

  procedure RaiseCapability(ACapability: TTheWordNoteBodyCapability);
  begin
    if Ord(ACapability) > Ord(Result.FCapability) then
      Result.FCapability := ACapability;
  end;

  procedure AddOpaque(const AValue: UnicodeString;
    ACapability: TTheWordNoteBodyCapability);
  begin
    Token := Result.AddToken(twnbtkOpaque);
    Token.FRawLexeme := AValue;
    RaiseCapability(ACapability);
  end;

begin
  Result := TTheWordTranslatorNoteBodyDocument.Create;
  Stack := TObjectList.Create(True);
  try
    try
    Result.FOriginalRaw := ARaw;
    if ContainsFreeRtfPayload(ARaw) then
      RaiseCapability(twnbcRawOnly);
    Index := 1;
    TextStart := 1;
    while Index <= Length(ARaw) do
    begin
      if ARaw[Index] <> '<' then
      begin
        Inc(Index);
        Continue;
      end;
      TagEnd := FindTagEnd(ARaw, Index);
      if TagEnd = 0 then
      begin
        if Index > TextStart then
          Result.AddText(Copy(ARaw, TextStart, Index - TextStart));
        AddOpaque(Copy(ARaw, Index, MaxInt), twnbcRawOnly);
        TextStart := Length(ARaw) + 1;
        Break;
      end;
      if Index > TextStart then
        Result.AddText(Copy(ARaw, TextStart, Index - TextStart));
      RawTag := Copy(ARaw, Index, TagEnd - Index + 1);
      Name := TrimUnicode(Copy(RawTag, 2, Length(RawTag) - 2));
      Closing := (Name <> '') and (Name[1] = '/');
      if Closing then
        Name := TrimUnicode(Copy(Name, 2, MaxInt));
      NameEnd := 1;
      while (NameEnd <= Length(Name)) and not
        (Name[NameEnd] in [' ', #9, #10, #13]) do
        Inc(NameEnd);
      Attrs := TrimUnicode(Copy(Name, NameEnd, MaxInt));
      Name := Copy(Name, 1, NameEnd - 1);

      if not Closing and (AsciiSameText(Name, 'p')) and (Attrs = '') then
        Result.AddToken(twnbtkParagraphBreak)
      else if not Closing and
        (AsciiSameText(Name, 'CL') or AsciiSameText(Name, 'br') or
         AsciiSameText(Name, 'br/')) and (Attrs = '') then
        Result.AddToken(twnbtkLineBreak)
      else if TrySimpleTagKind(Name, Kind) and (Attrs = '') then
      begin
        if Closing then
        begin
          if (Stack.Count = 0) or
             (TTheWordNoteBodyStackEntry(Stack[Stack.Count - 1]).Kind <>
              Kind) then
            AddOpaque(RawTag, twnbcRawOnly)
          else
          begin
            Stack.Delete(Stack.Count - 1);
            Token := Result.AddToken(twnbtkTagClose);
            Token.FTagKind := Kind;
            Token.FRawLexeme := RawTag;
          end;
        end
        else
        begin
          Entry := TTheWordNoteBodyStackEntry.Create;
          Entry.Kind := Kind;
          Stack.Add(Entry);
          Token := Result.AddToken(twnbtkTagOpen);
          Token.FTagKind := Kind;
          Token.FRawLexeme := RawTag;
        end;
      end
      else if AsciiSameText(Name, 'font') then
      begin
        Kind := twnbFont;
        if Closing then
        begin
          if (Attrs <> '') or (Stack.Count = 0) or
             (TTheWordNoteBodyStackEntry(Stack[Stack.Count - 1]).Kind <>
              Kind) then
            AddOpaque(RawTag, twnbcRawOnly)
          else
          begin
            Stack.Delete(Stack.Count - 1);
            Token := Result.AddToken(twnbtkTagClose);
            Token.FTagKind := Kind;
            Token.FRawLexeme := RawTag;
          end;
        end
        else if not AttributesAreOnly(Attrs,
          ['name', 'size', 'color']) then
          AddOpaque(RawTag, twnbcRawOnly)
        else
        begin
          Token := Result.AddToken(twnbtkTagOpen);
          Token.FTagKind := Kind;
          Token.FRawLexeme := RawTag;
          TryReadAttribute(Attrs, 'name', Token.FFontName);
          TryReadAttribute(Attrs, 'size', Token.FFontSize);
          TryReadAttribute(Attrs, 'color', Token.FForegroundColor);
          if (Token.FFontName = '') and (Token.FFontSize = '') and
             (Token.FForegroundColor = '') then
          begin
            Token.FKind := twnbtkOpaque;
            RaiseCapability(twnbcRawOnly);
          end
          else
          begin
            if ((Token.FFontName <> '') and
                not IsRepresentableAttribute(Token.FFontName)) or
               ((Token.FFontSize <> '') and
                not IsVisualRelativeFontSize(Token.FFontSize)) or
               ((Token.FForegroundColor <> '') and
                not IsVisualHtmlColor(Token.FForegroundColor)) then
              RaiseCapability(twnbcProtectedRegions);
            Entry := TTheWordNoteBodyStackEntry.Create;
            Entry.Kind := Kind;
            Stack.Add(Entry);
          end;
        end;
      end
      else if AsciiSameText(Name, 'span') then
      begin
        Kind := twnbSpanBackground;
        if Closing then
        begin
          if (Attrs <> '') or (Stack.Count = 0) or
             (TTheWordNoteBodyStackEntry(Stack[Stack.Count - 1]).Kind <>
              Kind) then
            AddOpaque(RawTag, twnbcRawOnly)
          else
          begin
            Stack.Delete(Stack.Count - 1);
            Token := Result.AddToken(twnbtkTagClose);
            Token.FTagKind := Kind;
            Token.FRawLexeme := RawTag;
          end;
        end
        else if not AttributesAreOnly(Attrs, ['style']) then
          AddOpaque(RawTag, twnbcRawOnly)
        else if TryReadAttribute(Attrs, 'style', Style) then
        begin
          Value := TrimUnicode(Style);
          if Pos('background:', LowerCase(UTF8Encode(Value))) = 1 then
          begin
            Token := Result.AddToken(twnbtkTagOpen);
            Token.FTagKind := Kind;
            Token.FRawLexeme := RawTag;
            Token.FBackgroundColor := TrimUnicode(Copy(Value,
              Length('background:') + 1, MaxInt));
            if not IsVisualHtmlColor(Token.FBackgroundColor) then
              RaiseCapability(twnbcProtectedRegions);
            Entry := TTheWordNoteBodyStackEntry.Create;
            Entry.Kind := Kind;
            Stack.Add(Entry);
          end
          else
            AddOpaque(RawTag, twnbcProtectedRegions);
        end
        else
          AddOpaque(RawTag, twnbcRawOnly);
      end
      else if AsciiSameText(Name, 'a') then
      begin
        Kind := twnbLink;
        if Closing then
        begin
          if (Attrs <> '') or (Stack.Count = 0) or
             (TTheWordNoteBodyStackEntry(Stack[Stack.Count - 1]).Kind <>
              Kind) then
            AddOpaque(RawTag, twnbcRawOnly)
          else
          begin
            Stack.Delete(Stack.Count - 1);
            Token := Result.AddToken(twnbtkTagClose);
            Token.FTagKind := Kind;
            Token.FRawLexeme := RawTag;
          end;
        end
        else if AttributesAreOnly(Attrs, ['href']) and
          TryReadAttribute(Attrs, 'href', Value) and IsSafeLink(Value) then
        begin
          Token := Result.AddToken(twnbtkTagOpen);
          Token.FTagKind := Kind;
          Token.FRawLexeme := RawTag;
          Token.FHref := Value;
          Entry := TTheWordNoteBodyStackEntry.Create;
          Entry.Kind := Kind;
          Stack.Add(Entry);
        end
        else
          AddOpaque(RawTag, twnbcRawOnly);
      end
      else if IsKnownProtectedTag(Name) then
        AddOpaque(RawTag, twnbcProtectedRegions)
      else
        AddOpaque(RawTag, twnbcRawOnly);

      Index := TagEnd + 1;
      TextStart := Index;
    end;
    if TextStart <= Length(ARaw) then
      Result.AddText(Copy(ARaw, TextStart, MaxInt));
    if Stack.Count > 0 then
      RaiseCapability(twnbcRawOnly);
    Result.FModified := False;
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    Stack.Free;
  end;
end;

class function TTheWordTranslatorNoteBodyCodec.Encode(
  ADocument: TTheWordTranslatorNoteBodyDocument): UnicodeString;
var
  Index: Integer;
  Name: UnicodeString;
  Token: TTheWordNoteBodyToken;
begin
  if ADocument = nil then
    raise EArgumentNilException.Create('ADocument');
  if not ADocument.Modified then
    Exit(ADocument.OriginalRaw);
  Result := '';
  for Index := 0 to ADocument.TokenCount - 1 do
  begin
    Token := ADocument.Tokens[Index];
    case Token.Kind of
      twnbtkText: Result := Result + Token.Text;
      twnbtkParagraphBreak: Result := Result + '<p>';
      twnbtkLineBreak: Result := Result + '<CL>';
      twnbtkOpaque: Result := Result + Token.RawLexeme;
      twnbtkTagClose:
        Result := Result + '</' + CanonicalTagName(Token.TagKind) + '>';
      twnbtkTagOpen:
        begin
          Name := CanonicalTagName(Token.TagKind);
          Result := Result + '<' + Name;
          case Token.TagKind of
            twnbFont:
              begin
                if Token.FontName <> '' then
                  Result := Result + ' name="' +
                    EncodeAttribute(Token.FontName) + '"';
                if Token.FontSize <> '' then
                  Result := Result + ' size="' +
                    EncodeAttribute(Token.FontSize) + '"';
                if Token.ForegroundColor <> '' then
                  Result := Result + ' color="' +
                    EncodeAttribute(Token.ForegroundColor) + '"';
              end;
            twnbSpanBackground:
              Result := Result + ' style="background:' +
                EncodeAttribute(Token.BackgroundColor) + '"';
            twnbLink:
              Result := Result + ' href="' + EncodeAttribute(Token.Href) + '"';
          end;
          Result := Result + '>';
        end;
    end;
  end;
end;

class function TTheWordTranslatorNoteBodyCodec.Analyze(
  const ARaw: UnicodeString): TTheWordNoteBodyCapability;
var
  Document: TTheWordTranslatorNoteBodyDocument;
begin
  Document := Decode(ARaw);
  try
    Result := Document.Capability;
  finally
    Document.Free;
  end;
end;

class function TTheWordTranslatorNoteBodyCodec.TryCanonicalize(
  const ARaw: UnicodeString; out ACanonical: UnicodeString;
  out ACapability: TTheWordNoteBodyCapability): Boolean;
var
  Document: TTheWordTranslatorNoteBodyDocument;
begin
  ACanonical := ARaw;
  Document := Decode(ARaw);
  try
    ACapability := Document.Capability;
    Result := ACapability = twnbcFullWysiwyg;
    if Result then
    begin
      Document.MarkModified;
      ACanonical := Encode(Document);
    end;
  finally
    Document.Free;
  end;
end;

end.


