unit ONTTokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Dialogs, LazUTF8, Graphics;

type
  TCodepointArray = array of Cardinal;

type
  // Legacy enum member names are retained because they are exposed in copied JSON.
  TSyntagmKind = (tsNulo, tsSintagma, tsTag, tsEspaco, tsPontuacao, tsMetaDado, tsStrongCount);
  // Compatibility alias for existing integrations.
  TTipoSintagma = TSyntagmKind;

const
  skNull = tsNulo;
  skSyntagm = tsSintagma;
  skTag = tsTag;
  skWhitespace = tsEspaco;
  skPunctuation = tsPontuacao;
  skMetadata = tsMetaDado;
  skStrongCount = tsStrongCount;

type
  TSyntagmToken = record
    Kind: TSyntagmKind;
    Value: string;
    Color: TColor;
    Superscript: Boolean;
    Italic: Boolean;
  end;
  // Compatibility alias for existing integrations.
  TTagSintagma = TSyntagmToken;

  { TONTTokenizer }
  TONTTokenizer = class
  public
    constructor Create(XML: string); virtual; abstract;
    constructor Create(XML: PChar); virtual; abstract;
    destructor Destroy; override;
    function ReadSyntagm(var Token: TSyntagmToken): TSyntagmKind; virtual;
    function ReadTagProperty(const PropertyName: string; const Token: TSyntagmToken): string; virtual;
    procedure ReadToTag(var Token: TSyntagmToken; const EndTag: string); virtual;
    // Legacy virtual extension points retained for source compatibility.
    function LerSintagma(var Token: TTagSintagma): TTipoSintagma; virtual; abstract;
    function LerPropriedadeTag(PropertyName: string; Token: TTagSintagma): string; virtual; abstract;
    procedure LerAteTag(var Token: TTagSintagma; EndTag: string); virtual; abstract;
    function ReadUntilExclusive(EndTag: string): string; virtual; abstract;
  end;

  { TONTTokenizerFactory }
  TTokenizerFactory = class
  private
    class var FPreferredVersion: Integer;
  public
    const LEGACY_VERSION = 1;
    const LATEST_VERSION = 2;
    const MAX_VERSION = 3;
    class constructor Create;
    class function NormalizeVersion(version: Integer): Integer;
    class function GetVersionCaption(version: Integer): string;
    class procedure PopulateVersionCaptions(items: TStrings);
    class function CreateTokenizer(version: Integer; XML: string): TONTTokenizer; overload;
    class function CreateTokenizer(version: Integer; XML: PChar): TONTTokenizer; overload;
    class function CreatePreferredTokenizer(XML: string): TONTTokenizer; overload;
    class function CreatePreferredTokenizer(XML: PChar): TONTTokenizer; overload;
    class property PreferredVersion: Integer read FPreferredVersion write FPreferredVersion;
  end;

  { TONTTokenizerV1 }

  TONTTokenizerV1 = class(TONTTokenizer)
  protected
    FXML: PChar;
    FPXML: PChar;
    function CurrentCodepoint(out Codepoint: Cardinal; out CodepointLength: Integer): Boolean;
    function IsSpaceCodepoint(Codepoint: Cardinal): Boolean;
    function IsTokenizerPunctuation(Codepoint: Cardinal): Boolean; virtual;
    function ConsumeSinglePunctuationCodepoint: Boolean; virtual;
    procedure InitializeToken(var Token: TSyntagmToken);
    procedure ReadGroupedPunctuation(var Token: TSyntagmToken);
    procedure ReadPunctuationToken(var Token: TSyntagmToken);
    procedure SkipSpaces;
    procedure SkipCurrentTag;
    procedure ReadSpaces(var Token: TSyntagmToken);
    procedure ReadPunctuation(var Token: TSyntagmToken); virtual;
    procedure ReadTag(var Token: TSyntagmToken);
    procedure ReadText(var Token: TSyntagmToken); virtual;
    function ReadSyntagmCore(var Token: TSyntagmToken): TSyntagmKind;
    function ReadTagPropertyCore(const PropertyName: string; const Token: TSyntagmToken): string;
    procedure ReadToTagCore(var Token: TSyntagmToken; const EndTag: string);
  public
    constructor Create(XML: string); override;
    constructor Create(XML: PChar); override;
    destructor Destroy; override;
    function LerSintagma(var Token: TTagSintagma): TTipoSintagma; override;
    function LerPropriedadeTag(PropertyName: string; Token: TTagSintagma): string; override;
    procedure LerAteTag(var Token: TTagSintagma; EndTag: string); override;
    function ReadUntilExclusive(EndTag: string): string; override;
  end;

  { TONTTokenizerV2 }
  TONTTokenizerV2 = class(TONTTokenizerV1)
  protected
    function IsTokenizerPunctuation(Codepoint: Cardinal): Boolean; override;
    function ConsumeSinglePunctuationCodepoint: Boolean; override;
  end;

  { TONTTokenizerV3 }
  TONTTokenizerV3 = class(TONTTokenizerV2)
  protected
    function IsTokenizerPunctuation(Codepoint: Cardinal): Boolean; override;
  end;

implementation

function ContainsCodepoint(Codepoint: Cardinal; const values: array of Cardinal): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := Low(values) to High(values) do
    if values[Index] = Codepoint then
      Exit(True);
end;

resourcestring
  STokenizerVersion1Caption = 'Version 1 - Original tokenizer';
  STokenizerVersion2Caption = 'Version 2 - Hebrew punctuation support';
  STokenizerVersion3Caption = 'Version 3 - English quote punctuation';
  STokenizerVersionFallbackCaption = 'Version %d';

{ TONTTokenizer }

destructor TONTTokenizer.Destroy;
begin
  inherited Destroy;
end;

function TONTTokenizer.ReadSyntagm(var Token: TSyntagmToken): TSyntagmKind;
begin
  Result := LerSintagma(Token);
end;

function TONTTokenizer.ReadTagProperty(const PropertyName: string;
  const Token: TSyntagmToken): string;
begin
  Result := LerPropriedadeTag(PropertyName, Token);
end;

procedure TONTTokenizer.ReadToTag(var Token: TSyntagmToken;
  const EndTag: string);
begin
  LerAteTag(Token, EndTag);
end;
{ TTokenizerFactory }

class constructor TTokenizerFactory.Create;
begin
  FPreferredVersion := LATEST_VERSION;
end;

class function TTokenizerFactory.NormalizeVersion(version: Integer): Integer;
begin
  case version of
    1, 2, 3:
      Result := version;
  else
    Result := 1;
  end;
end;

class function TTokenizerFactory.GetVersionCaption(version: Integer): string;
begin
  case NormalizeVersion(version) of
    1: Result := STokenizerVersion1Caption;
    2: Result := STokenizerVersion2Caption;
    3: Result := STokenizerVersion3Caption;
  else
    Result := Format(STokenizerVersionFallbackCaption, [version]);
  end;
end;

class procedure TTokenizerFactory.PopulateVersionCaptions(items: TStrings);
var
  version: Integer;
begin
  items.Clear;
  for version := 1 to MAX_VERSION do
    items.AddObject(GetVersionCaption(version), TObject(PtrInt(version)));
end;

class function TTokenizerFactory.CreateTokenizer(version: Integer; XML: string): TONTTokenizer;
begin
  case NormalizeVersion(version) of
    1: Result := TONTTokenizerV1.Create(XML);
    2: Result := TONTTokenizerV2.Create(XML);
    3: Result := TONTTokenizerV3.Create(XML);
  else
    raise Exception.Create('Unsupported tokenizer version');
  end;
end;

class function TTokenizerFactory.CreateTokenizer(version: Integer; XML: PChar): TONTTokenizer;
begin
  case NormalizeVersion(version) of
    1: Result := TONTTokenizerV1.Create(XML);
    2: Result := TONTTokenizerV2.Create(XML);
    3: Result := TONTTokenizerV3.Create(XML);
  else
    raise Exception.Create('Unsupported tokenizer version');
  end;
end;

class function TTokenizerFactory.CreatePreferredTokenizer(XML: string): TONTTokenizer;
begin
  Result := CreateTokenizer(FPreferredVersion, XML);
end;

class function TTokenizerFactory.CreatePreferredTokenizer(XML: PChar): TONTTokenizer;
begin
  Result := CreateTokenizer(FPreferredVersion, XML);
end;

{ TONTTokenizerV1 }

constructor TONTTokenizerV1.Create(XML: string);
begin
  Create(Pchar(XML));
end;

constructor TONTTokenizerV1.Create(XML: PChar);
begin
  FXML := XML;
  FPXML := FXML;
  if pos(#239#187#191, FPXML) = 1 then // Skip the UTF-8 BOM
     inc(FPXML, 3);
end;

destructor TONTTokenizerV1.Destroy;
begin
  inherited Destroy;
end;

function TONTTokenizerV1.CurrentCodepoint(out Codepoint: Cardinal;
  out CodepointLength: Integer): Boolean;
begin
  Codepoint := UTF8CodepointToUnicode(FPXML, CodepointLength);
  Result := Codepoint <> 0;
end;

function TONTTokenizerV1.IsSpaceCodepoint(Codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(Codepoint, [Ord(#32), Ord(#9), Ord('|')]);
end;

function TONTTokenizerV1.IsTokenizerPunctuation(Codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(Codepoint,
    [Ord('"'), Ord('.'), Ord(','), Ord(';'), Ord(':'), Ord('!'), Ord('?'),
     Ord('('), Ord(')'), 183, 903, 8220, 8221]);
end;

function TONTTokenizerV1.ConsumeSinglePunctuationCodepoint: Boolean;
begin
  Result := False;
end;

procedure TONTTokenizerV1.InitializeToken(var Token: TSyntagmToken);
begin
  Token.Value := '';
  Token.Color := clDefault;
  Token.Italic := False;
  Token.Superscript := False;
end;

procedure TONTTokenizerV1.ReadGroupedPunctuation(var Token: TSyntagmToken);
var
  Start: PChar;
  Codepoint: Cardinal;
  CodepointLength: Integer;
begin
  Start := FPXML;
  while CurrentCodepoint(Codepoint, CodepointLength) and IsTokenizerPunctuation(Codepoint) do
    Inc(FPXML, CodepointLength);

  Token.Value := Copy(Start, 0, FPXML - Start);
  Token.Kind := skPunctuation;
end;

procedure TONTTokenizerV1.ReadPunctuationToken(var Token: TSyntagmToken);
var
  Codepoint: Cardinal;
  CodepointLength: Integer;
begin
  if ConsumeSinglePunctuationCodepoint and CurrentCodepoint(Codepoint, CodepointLength) then
  begin
    Inc(FPXML, CodepointLength);
    Token.Value := Copy(FPXML - CodepointLength, 0, CodepointLength);
    Token.Kind := skPunctuation;
    Exit;
  end;

  ReadGroupedPunctuation(Token);
end;

procedure TONTTokenizerV1.SkipSpaces;
begin
  while (FPXML^ in [#13, #10, #32, #9, '|']) do
  begin
    inc(FPXML);
  end;
end;

procedure TONTTokenizerV1.SkipCurrentTag;
begin
  while (FPXML^ <> #0) and (FPXML^ <> '>') do
  begin
    inc(FPXML);
  end;
  if FPXML^ = '>' then
    inc(FPXML);
end;

procedure TONTTokenizerV1.ReadSpaces(var Token: TSyntagmToken);
var
  CharacterCount: Integer;
begin
  CharacterCount := 0;
  while FPXML^ in [#32, #9, '|'] do
  begin
    Inc(CharacterCount);
    inc(FPXML);
  end;
  Token.Value := Copy(FPXML - CharacterCount, 0, CharacterCount);
  Token.Kind  := skWhitespace;
end;

procedure TONTTokenizerV1.ReadPunctuation(var Token: TSyntagmToken);
begin
  ReadGroupedPunctuation(Token);
end;

procedure TONTTokenizerV1.ReadTag(var Token: TSyntagmToken);
var
  CharacterCount: Integer;
begin
  CharacterCount := 0;

  while not (FPXML^ in [#0, '>']) do
  begin
    inc(FPXML);
    Inc(CharacterCount);
  end;

  if FPXML^ = '>' then
  begin
    inc(FPXML);
    Inc(CharacterCount);
  end;
  Token.Value := Copy(FPXML - CharacterCount, 0, CharacterCount);
  Token.Kind := skTag;

end;

procedure TONTTokenizerV1.ReadText(var Token: TSyntagmToken);
var
  Start: PChar;
  Codepoint: Cardinal;
  CodepointLength: Integer;
  Hyphen: string;
begin
  if FPXML^ = '-' then
  begin
    Hyphen := '-';
    Inc(FPXML);
  end
  else
    Hyphen := '';

  Start := FPXML;
  while CurrentCodepoint(Codepoint, CodepointLength)
    and not IsSpaceCodepoint(Codepoint)
    and (Codepoint <> Ord('<'))
    and (Codepoint <> Ord('-'))
    and not IsTokenizerPunctuation(Codepoint) do
    Inc(FPXML, CodepointLength);

  Token.Value := Hyphen + Copy(Start, 0, FPXML - Start);
  Token.Kind := skSyntagm;
end;

function TONTTokenizerV1.LerSintagma(var Token: TTagSintagma): TTipoSintagma;
begin
  Result := ReadSyntagmCore(Token);
end;

function TONTTokenizerV1.LerPropriedadeTag(PropertyName: string;
  Token: TTagSintagma): string;
begin
  Result := ReadTagPropertyCore(PropertyName, Token);
end;

procedure TONTTokenizerV1.LerAteTag(var Token: TTagSintagma; EndTag: string);
begin
  ReadToTagCore(Token, EndTag);
end;

function TONTTokenizerV1.ReadSyntagmCore(var Token: TSyntagmToken): TSyntagmKind;
var
  Codepoint: Cardinal;
  CodepointLength: Integer;
begin
  InitializeToken(Token);

  if not CurrentCodepoint(Codepoint, CodepointLength) then
    Token.Kind := skNull
  else if Codepoint = Ord('<') then
    Token.Kind := skTag
  else if IsSpaceCodepoint(Codepoint) then
    Token.Kind := skWhitespace
  else if IsTokenizerPunctuation(Codepoint) then
    Token.Kind := skPunctuation
  else
    Token.Kind := skSyntagm;

  case Token.Kind of
    skWhitespace:
      ReadSpaces(Token);
    skPunctuation:
      ReadPunctuationToken(Token);
    skTag:
      ReadTag(Token);
    skSyntagm:
      ReadText(Token);
  end;

  Result := Token.Kind;
end;

function TONTTokenizerV1.ReadTagPropertyCore(const PropertyName: string; const Token: TSyntagmToken): string;
var
  StartIndex, EndIndex: SmallInt;
begin
  Result := '';

  if Token.Value[1] <> '<' then
    exit;

  StartIndex := Pos(PropertyName + '=', Token.Value);
  if StartIndex = 0 then
    exit;

  StartIndex := StartIndex + Length(PropertyName) + 1;
  if Token.Value[StartIndex] = '"' then
  begin
    Inc(StartIndex);
    EndIndex := StartIndex + Pos('"', Copy(Token.Value, StartIndex, Length(Token.Value))) - 1;
  end else
  begin
    EndIndex := StartIndex + Pos(' ', Copy(Token.Value, StartIndex, Length(Token.Value))) - 1;
    if EndIndex = (StartIndex - 1) then
      EndIndex := StartIndex + Pos('>', Copy(Token.Value, StartIndex, Length(Token.Value))) - 1;
  end;


  Result := Copy(Token.Value, StartIndex, EndIndex - StartIndex);
end;

procedure TONTTokenizerV1.ReadToTagCore(var Token: TSyntagmToken; const EndTag: string);
var
  Value: string;
begin
  Value := '';
  repeat
    Value := Concat(Value, Token.Value);
    ReadSyntagm(Token);
  until (Token.Kind = skNull) or (Token.Value = EndTag);
  Token.Value := Value + Token.Value;
end;

function TONTTokenizerV1.ReadUntilExclusive(EndTag: string): string;
var
  Token: TSyntagmToken;
begin
  ReadToTag(Token, EndTag);
  Result := Token.Value.Replace(EndTag, '');
end;

{ TONTTokenizerV2 }

function TONTTokenizerV2.IsTokenizerPunctuation(Codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(Codepoint,
    [Ord('"'), Ord('.'), Ord(','), Ord(';'), Ord(':'), Ord('!'), Ord('?'),
     Ord('('), Ord(')'), Ord('['), Ord(']'), 183, 903, 8220, 8221, 1470, 1472, 1475]);
end;

function TONTTokenizerV2.ConsumeSinglePunctuationCodepoint: Boolean;
begin
  Result := True;
end;

{ TONTTokenizerV3 }

function TONTTokenizerV3.IsTokenizerPunctuation(Codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(Codepoint,
    [Ord('"'), Ord('.'), Ord(','), Ord(';'), Ord(':'), Ord('!'), Ord('?'),
     Ord('('), Ord(')'), Ord('['), Ord(']'), 183, 8216, 8217, 903, 8220, 8221, 1470, 1472, 1475]);
end;
end.


