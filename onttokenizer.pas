unit ONTTokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LCLProc, Dialogs, LazUTF8, Graphics;

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
    const LATEST_VERSION = 2;
    class constructor Create;
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
    procedure LerPontuacao(var s: TTagSintagma); override;
    procedure LerTexto(var s: TTagSintagma); override;
  public
    function LerSintagma(var s: TTagSintagma): TTipoSintagma; override;
  end;

implementation

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

class function TTokenizerFactory.CreateTokenizer(version: Integer; XML: string): TONTTokenizer;
begin
  case version of
    1: Result := TONTTokenizerV1.Create(XML);
    2: Result := TONTTokenizerV2.Create(XML);
  else
    raise Exception.Create('Unsupported tokenizer version');
  end;
end;

class function TTokenizerFactory.CreateTokenizer(version: Integer; XML: PChar): TONTTokenizer;
begin
  case version of
    1: Result := TONTTokenizerV1.Create(XML);
    2: Result := TONTTokenizerV2.Create(XML);
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
var
  c: integer;
begin
  c := 0;
  while (FPXML^ in [{'''',} '"', '.', ',', ';', ':', '!', '?', {'-',} '(', ')']) or
        (pos(#194#183, FPXML) = 1) or      // ·
        (pos(#226#128#156, FPXML) = 1) or  // “
        (pos(#226#128#157, FPXML) = 1) or  // ”
        (pos(#206#135, FPXML) = 1)         // ·
  do
  begin
    if AnsiStartsStr(#194#183, FPXML) or
       AnsiStartsStr(#206#135, FPXML) then // hack para sinais de pontuação gregos
    begin
      inc(FPXML, 2);
      inc(c, 2);
    end else if AnsiStartsStr(#226#128#156, FPXML) or
                AnsiStartsStr(#226#128#157, FPXML) then // hack para “ e ”
    begin
      inc(FPXML, 3);
      inc(c, 3);
    end else
    begin
      inc(FPXML);
      inc(c);
    end;
  end;
  s.valor := copy(FPXML-c, 0, c);
  s.tipo  := tsPontuacao;
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
  c: integer;
begin
  c := 0;
  while not (FPXML^ in [#0, #32, #9, '|', '<', {'''',} '"', '.', ',', ';', ':', '!', '?', {'-',} '(', ')']) do
  begin
    if AnsiStartsStr(#194#183, FPXML) or     // ·
       AnsiStartsStr(#226#128#156, FPXML) or // “
       AnsiStartsStr(#226#128#157, FPXML) or // ”
       AnsiStartsStr(#206#135, FPXML)        // ·
    then // hack para wide chars
      break;

    inc(c);
    inc(FPXML);
  end;
  s.valor := copy(FPXML-c, 0, c);
  s.tipo  := tsSintagma;
end;

function TONTTokenizerV1.LerSintagma(var s: TTagSintagma): TTipoSintagma;

  function Contido(c: cardinal; v: array of cardinal): boolean;
  var
    i: integer;
  begin
    result := false;
    for i:=low(v) to high(v) do
      if v[i] = c then
        result := true;
  end;

  procedure lerAte(var s: TTagSintagma; ate: array of cardinal);
  var
    t: integer;
    ini: PChar;
  begin
    ini := FPXML;
    while (FPXML^ <> #0) and not Contido(UTF8CharacterToUnicode(FPXML, t), ate) do
      inc(FPXML, t);

    s.valor := copy(ini, 0, FPXML-ini);
  end;

  procedure lerAteInclusive(var s: TTagSintagma; ate: array of cardinal);
  var
    t: integer;
  begin
    lerAte(s, ate);
    if UTF8CharacterToUnicode(FPXML, t) <> 0 then
    begin
      s.valor := s.valor + copy(FPXML, 0, t);
      inc(FPXML, t);
    end;
  end;

  procedure lerEnquanto(var s: TTagSintagma; enquanto: array of cardinal);
  var
    t: integer;
    ini: PChar;
  begin
    ini := FPXML;
    while (FPXML^ <> #0) and Contido(UTF8CharacterToUnicode(FPXML, t), enquanto) do
      inc(FPXML, t);

    s.valor := copy(ini, 0, FPXML-ini);
  end;

  function tipoChar: TTipoSintagma;
  var
    t: integer;
    c: Cardinal;
  begin
    result := tsSintagma;
    c := UTF8CharacterToUnicode(FPXML, t);
    if c = 0 then
      result := tsNulo
    else if c = ord('<') then
      result := tsTag
    else if Contido(c, [ord(#32), ord(#9), ord('|')]) then
      result := tsEspaco
    else if Contido(c, [ord('"'), ord('.'), ord(','), ord(';'), ord(':'),
                        ord('!'), ord('?'), ord('('), ord(')'),
                        183, 903, 8220, 8221{, ord('-')}]) // hack para ··“”
    then
      result := tsPontuacao;
  end;
var
  hifen: string;
begin
  s.valor      := '';
  s.cor        := clDefault;
  s.italico    := false;
  s.sobrescrito:= false;
  s.tipo       := tipoChar;

  case s.tipo of
    tsEspaco:
      lerEnquanto(s, [ord(#32), ord(#9), ord('|')]);
    tsPontuacao:
      lerEnquanto(s, [ord('"'), ord('.'), ord(','), ord(';'), ord(':'),
                      ord('!'), ord('?'), ord('('), ord(')'),
                      183, 903, 8220, 8221{, ord('-')}]); // hack para ··“”
    tsTag:
      lerAteInclusive(s, [ord('>')]);
  else
    if FPXML^ = '-' then
    begin
      hifen := '-';
      Inc(FPXML);
    end else
      hifen := '';

    lerAte(s, [ord('<'), ord(#32), ord(#9), ord('|'), ord('"'), ord('.'),
               ord(','), ord(';'), ord(':'), ord('!'), ord('?'), ord('-'),
               ord('('), ord(')'), 183, 903, 8220, 8221]);  // hack para ··“”
    s.valor := hifen + s.valor;
  end;

  //MessageDlg('Fechar projeto', '"'+s.valor+'"', mtConfirmation, [], 0);
  result := s.tipo;
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

procedure TONTTokenizerV2.LerPontuacao(var s: TTagSintagma);
var
  c: integer;
begin
  c := 0;
  while (FPXML^ in ['"', '.', ',', ';', ':', '!', '?', '(', ')']) or
        (pos(#194#183, FPXML) = 1) or      // ·
        (pos(#226#128#156, FPXML) = 1) or  // “
        (pos(#226#128#157, FPXML) = 1) or  // ”
        (pos(#206#135, FPXML) = 1) or      // ·
        (pos(#215#128, FPXML) = 1) or      // ׀ paseq
        (pos(#215#131, FPXML) = 1) or      // ׃ soft pasuq
        (pos(#214#190, FPXML) = 1)         // – maqaf
  do
  begin
    if AnsiStartsStr(#194#183, FPXML) or
       AnsiStartsStr(#215#128, FPXML) or  // paseq
       AnsiStartsStr(#215#131, FPXML) or  // soft pasuq
       AnsiStartsStr(#214#190, FPXML) or  // maqaf
       AnsiStartsStr(#206#135, FPXML) then // hack para sinais de pontuação gregos/hebraicos
    begin
      inc(FPXML, 2);
      inc(c, 2);
    end else if AnsiStartsStr(#226#128#156, FPXML) or
                AnsiStartsStr(#226#128#157, FPXML) then // hack para “ e ”
    begin
      inc(FPXML, 3);
      inc(c, 3);
    end else
    begin
      inc(FPXML);
      inc(c);
    end;
  end;
  s.valor := copy(FPXML-c, 0, c);
  s.tipo  := tsPontuacao;
end;

procedure TONTTokenizerV2.LerTexto(var s: TTagSintagma);
var
  c: integer;
begin
  c := 0;
  while not (FPXML^ in [#0, #32, #9, '|', '<', '"', '.', ',', ';', ':', '!', '?', '(', ')']) do
  begin
    if AnsiStartsStr(#194#183, FPXML) or     // ·
       AnsiStartsStr(#226#128#156, FPXML) or // “
       AnsiStartsStr(#226#128#157, FPXML) or // ”
       AnsiStartsStr(#206#135, FPXML) or     // ·
       AnsiStartsStr(#215#128, FPXML) or     // paseq
       AnsiStartsStr(#215#131, FPXML) or     // soft pasuq
       AnsiStartsStr(#214#190, FPXML)        // maqaf
    then // hack para wide chars
      break;

    inc(c);
    inc(FPXML);
  end;
  s.valor := copy(FPXML-c, 0, c);
  s.tipo  := tsSintagma;
end;

function TONTTokenizerV2.LerSintagma(var s: TTagSintagma): TTipoSintagma;

  function Contido(c: cardinal; v: array of cardinal): boolean;
  var
    i: integer;
  begin
    result := false;
    for i:=low(v) to high(v) do
      if v[i] = c then
        result := true;
  end;

  procedure lerAte(var s: TTagSintagma; ate: array of cardinal);
  var
    t: integer;
    ini: PChar;
  begin
    ini := FPXML;
    while (FPXML^ <> #0) and not Contido(UTF8CharacterToUnicode(FPXML, t), ate) do
      inc(FPXML, t);

    s.valor := copy(ini, 0, FPXML-ini);
  end;

  procedure lerAteInclusive(var s: TTagSintagma; ate: array of cardinal);
  var
    t: integer;
  begin
    lerAte(s, ate);
    if UTF8CharacterToUnicode(FPXML, t) <> 0 then
    begin
      s.valor := s.valor + copy(FPXML, 0, t);
      inc(FPXML, t);
    end;
  end;

  procedure lerEnquanto(var s: TTagSintagma; enquanto: array of cardinal);
  var
    t: integer;
    ini: PChar;
  begin
    ini := FPXML;
    while (FPXML^ <> #0) and Contido(UTF8CharacterToUnicode(FPXML, t), enquanto) do
      inc(FPXML, t);

    s.valor := copy(ini, 0, FPXML-ini);
  end;

  function tipoChar: TTipoSintagma;
  var
    t: integer;
    c: Cardinal;
  begin
    result := tsSintagma;
    c := UTF8CharacterToUnicode(FPXML, t);
    if c = 0 then
      result := tsNulo
    else if c = ord('<') then
      result := tsTag
    else if Contido(c, [ord(#32), ord(#9), ord('|')]) then
      result := tsEspaco
    else if Contido(c, [ord('"'), ord('.'), ord(','), ord(';'), ord(':'),
                        ord('!'), ord('?'), ord('('), ord(')'),
                        183, 903, 8220, 8221, 1470, 1472, 1475]) // hack para ··“”־׀׃
    then
      result := tsPontuacao;
  end;
var
  hifen: string;
begin
  s.valor      := '';
  s.cor        := clDefault;
  s.italico    := false;
  s.sobrescrito:= false;
  s.tipo       := tipoChar;

  case s.tipo of
    tsEspaco:
      lerEnquanto(s, [ord(#32), ord(#9), ord('|')]);
    tsPontuacao:
      lerEnquanto(s, [ord('"'), ord('.'), ord(','), ord(';'), ord(':'),
                      ord('!'), ord('?'), ord('('), ord(')'),
                      183, 903, 8220, 8221, 1470, 1472, 1475]); // hack para ··“”־׃׀
    tsTag:
      lerAteInclusive(s, [ord('>')]);
  else
    if FPXML^ = '-' then
    begin
      hifen := '-';
      Inc(FPXML);
    end else
      hifen := '';

    lerAte(s, [ord('<'), ord(#32), ord(#9), ord('|'), ord('"'), ord('.'),
               ord(','), ord(';'), ord(':'), ord('!'), ord('?'), ord('-'),
               ord('('), ord(')'), 1470, 1472, 1475, 183, 903, 8220, 8221]);  // hack para ··“”־׃׀
    s.valor := hifen + s.valor;
  end;

  //MessageDlg('Fechar projeto', '"'+s.valor+'"', mtConfirmation, [], 0);
  result := s.tipo;
end;
end.


