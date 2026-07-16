unit ONTTokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Dialogs, LazUTF8, Graphics;

type
  Simbolos = array of cardinal;

type
  TTipoSintagma = (tsNulo, tsSintagma, tsTag, tsEspaco, tsPontuacao, tsMetaDado, tsStrongCount);

type
  TTagSintagma = record
    tipo: TTipoSintagma;
    valor: string;
    cor: TColor;
    sobrescrito: boolean;
    italico: boolean;
  end;

  { TONTTokenizer }
  TONTTokenizer = class
  public
    constructor Create(XML: string); virtual; abstract;
    constructor Create(XML: PChar); virtual; abstract;
    destructor Destroy; override;
    function LerSintagma(var s: TTagSintagma): TTipoSintagma; virtual; abstract;
    function LerPropriedadeTag(p: string; s: TTagSintagma): string; virtual; abstract;
    procedure LerAteTag(var s: TTagSintagma; AteTag: string); virtual; abstract;
    function ReadUntilExclusive(endTag: string): string; virtual; abstract;
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
    function CurrentCodepoint(out codepoint: Cardinal; out codepointLength: Integer): Boolean;
    function IsSpaceCodepoint(codepoint: Cardinal): Boolean;
    function IsTokenizerPunctuation(codepoint: Cardinal): Boolean; virtual;
    function ConsumeSinglePunctuationCodepoint: Boolean; virtual;
    procedure InitializeToken(var s: TTagSintagma);
    procedure ReadGroupedPunctuation(var s: TTagSintagma);
    procedure ReadPunctuationToken(var s: TTagSintagma);
    procedure PularEspacos;
    procedure PularTagAtual;
    procedure LerEspacos(var s: TTagSintagma);
    procedure LerPontuacao(var s: TTagSintagma); virtual;
    procedure LerTag(var s: TTagSintagma);
    procedure LerTexto(var s: TTagSintagma); virtual;
  public
    constructor Create(XML: string); override;
    constructor Create(XML: PChar); override;
    destructor Destroy; override;
    function LerSintagma(var s: TTagSintagma): TTipoSintagma; override;
    function LerPropriedadeTag(p: string; s: TTagSintagma): string; override;
    procedure LerAteTag(var s: TTagSintagma; AteTag: string); override;
    function ReadUntilExclusive(endTag: string): string; override;
  end;

  { TONTTokenizerV2 }
  TONTTokenizerV2 = class(TONTTokenizerV1)
  protected
    function IsTokenizerPunctuation(codepoint: Cardinal): Boolean; override;
    function ConsumeSinglePunctuationCodepoint: Boolean; override;
  end;

  { TONTTokenizerV3 }
  TONTTokenizerV3 = class(TONTTokenizerV2)
  protected
    function IsTokenizerPunctuation(codepoint: Cardinal): Boolean; override;
  end;

implementation

function ContainsCodepoint(codepoint: Cardinal; const values: array of Cardinal): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(values) to High(values) do
    if values[i] = codepoint then
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
  if pos(#239#187#191, FPXML) = 1 then // saltando o UTF-8 BOM
     inc(FPXML, 3);
end;

destructor TONTTokenizerV1.Destroy;
begin
  inherited Destroy;
end;

function TONTTokenizerV1.CurrentCodepoint(out codepoint: Cardinal;
  out codepointLength: Integer): Boolean;
begin
  codepoint := UTF8CodepointToUnicode(FPXML, codepointLength);
  Result := codepoint <> 0;
end;

function TONTTokenizerV1.IsSpaceCodepoint(codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(codepoint, [Ord(#32), Ord(#9), Ord('|')]);
end;

function TONTTokenizerV1.IsTokenizerPunctuation(codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(codepoint,
    [Ord('"'), Ord('.'), Ord(','), Ord(';'), Ord(':'), Ord('!'), Ord('?'),
     Ord('('), Ord(')'), 183, 903, 8220, 8221]);
end;

function TONTTokenizerV1.ConsumeSinglePunctuationCodepoint: Boolean;
begin
  Result := False;
end;

procedure TONTTokenizerV1.InitializeToken(var s: TTagSintagma);
begin
  s.valor := '';
  s.cor := clDefault;
  s.italico := False;
  s.sobrescrito := False;
end;

procedure TONTTokenizerV1.ReadGroupedPunctuation(var s: TTagSintagma);
var
  start: PChar;
  codepoint: Cardinal;
  codepointLength: Integer;
begin
  start := FPXML;
  while CurrentCodepoint(codepoint, codepointLength) and IsTokenizerPunctuation(codepoint) do
    Inc(FPXML, codepointLength);

  s.valor := Copy(start, 0, FPXML - start);
  s.tipo := tsPontuacao;
end;

procedure TONTTokenizerV1.ReadPunctuationToken(var s: TTagSintagma);
var
  codepoint: Cardinal;
  codepointLength: Integer;
begin
  if ConsumeSinglePunctuationCodepoint and CurrentCodepoint(codepoint, codepointLength) then
  begin
    Inc(FPXML, codepointLength);
    s.valor := Copy(FPXML - codepointLength, 0, codepointLength);
    s.tipo := tsPontuacao;
    Exit;
  end;

  ReadGroupedPunctuation(s);
end;

procedure TONTTokenizerV1.PularEspacos;
begin
  while (FPXML^ in [#13, #10, #32, #9, '|']) do
  begin
    inc(FPXML);
  end;
end;

procedure TONTTokenizerV1.PularTagAtual;
begin
  while (FPXML^ <> #0) and (FPXML^ <> '>') do
  begin
    inc(FPXML);
  end;
  if FPXML^ = '>' then
    inc(FPXML);
end;

procedure TONTTokenizerV1.LerEspacos(var s: TTagSintagma);
var
  c: integer;
begin
  c := 0;
  while FPXML^ in [#32, #9, '|'] do
  begin
    inc(c);
    inc(FPXML);
  end;
  s.valor := copy(FPXML-c, 0, c);
  s.tipo  := tsEspaco;
end;

procedure TONTTokenizerV1.LerPontuacao(var s: TTagSintagma);
begin
  ReadGroupedPunctuation(s);
end;

procedure TONTTokenizerV1.LerTag(var s: TTagSintagma);
var
  c: integer;
begin
  c := 0;

  while not (FPXML^ in [#0, '>']) do
  begin
    inc(FPXML);
    inc(c);
  end;

  if FPXML^ = '>' then
  begin
    inc(FPXML);
    inc(c);
  end;
  s.valor := copy(FPXML-c, 0, c);
  s.tipo := tsTag;

  //if s.valor = '<FI>' then
  //  s.valor := '['
  //else if s.valor = '<Fi>' then
  //  s.valor := ']';
end;

procedure TONTTokenizerV1.LerTexto(var s: TTagSintagma);
var
  start: PChar;
  codepoint: Cardinal;
  codepointLength: Integer;
  hifen: string;
begin
  if FPXML^ = '-' then
  begin
    hifen := '-';
    Inc(FPXML);
  end
  else
    hifen := '';

  start := FPXML;
  while CurrentCodepoint(codepoint, codepointLength)
    and not IsSpaceCodepoint(codepoint)
    and (codepoint <> Ord('<'))
    and (codepoint <> Ord('-'))
    and not IsTokenizerPunctuation(codepoint) do
    Inc(FPXML, codepointLength);

  s.valor := hifen + Copy(start, 0, FPXML - start);
  s.tipo := tsSintagma;
end;

function TONTTokenizerV1.LerSintagma(var s: TTagSintagma): TTipoSintagma;
var
  codepoint: Cardinal;
  codepointLength: Integer;
begin
  InitializeToken(s);

  if not CurrentCodepoint(codepoint, codepointLength) then
    s.tipo := tsNulo
  else if codepoint = Ord('<') then
    s.tipo := tsTag
  else if IsSpaceCodepoint(codepoint) then
    s.tipo := tsEspaco
  else if IsTokenizerPunctuation(codepoint) then
    s.tipo := tsPontuacao
  else
    s.tipo := tsSintagma;

  case s.tipo of
    tsEspaco:
      LerEspacos(s);
    tsPontuacao:
      ReadPunctuationToken(s);
    tsTag:
      LerTag(s);
    tsSintagma:
      LerTexto(s);
  end;

  Result := s.tipo;
end;

function TONTTokenizerV1.LerPropriedadeTag(p: string; s: TTagSintagma): string;
var
  i, f: smallint;
  //quote: boolean;
begin
  result := '';
  //quote := false;

  if s.valor[1] <> '<' then
    exit;

  i := pos(p+'=', s.valor);
  if i = 0 then
    exit;

  i := i + length(p) + 1;
  if s.valor[i] = '"' then
  begin
    inc(i);
    //quote := true;
    f := i + pos('"', copy(s.valor, i, length(s.valor))) - 1;
  end else
  begin
    f := i + pos(' ', copy(s.valor, i, length(s.valor))) - 1; // será que temos mais de uma propriedade?
    if f = (i-1) then
      f := i + pos('>', copy(s.valor, i, length(s.valor))) - 1;
  end;

  //i := i + pos('"', copy(s.valor, i, length(s.valor)));
  //f := i + pos('"', copy(s.valor, i, length(s.valor))) - 1;

  result := copy(s.valor, i, f-i);
end;

procedure TONTTokenizerV1.LerAteTag(var s: TTagSintagma; AteTag: string);
var
  valor: string;
begin
  valor := '';
  repeat
    valor := concat(valor, s.valor);
    LerSintagma(s);
  until (s.tipo = tsNulo) or (s.valor = AteTag);
  s.valor := valor + s.valor;
end;

function TONTTokenizerV1.ReadUntilExclusive(endTag: string): string;
var
  token: TTagSintagma;
begin
  LerAteTag(token, endTag);
  result := token.valor.Replace(endTag, '');
end;

{ TONTTokenizerV2 }

function TONTTokenizerV2.IsTokenizerPunctuation(codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(codepoint,
    [Ord('"'), Ord('.'), Ord(','), Ord(';'), Ord(':'), Ord('!'), Ord('?'),
     Ord('('), Ord(')'), Ord('['), Ord(']'), 183, 903, 8220, 8221, 1470, 1472, 1475]);
end;

function TONTTokenizerV2.ConsumeSinglePunctuationCodepoint: Boolean;
begin
  Result := True;
end;

{ TONTTokenizerV3 }

function TONTTokenizerV3.IsTokenizerPunctuation(codepoint: Cardinal): Boolean;
begin
  Result := ContainsCodepoint(codepoint,
    [Ord('"'), Ord('.'), Ord(','), Ord(';'), Ord(':'), Ord('!'), Ord('?'),
     Ord('('), Ord(')'), Ord('['), Ord(']'), 183, 8216, 8217, 903, 8220, 8221, 1470, 1472, 1475]);
end;
end.


