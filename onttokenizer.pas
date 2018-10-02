unit ONTTokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LCLProc, Dialogs, LazUTF8, Graphics;

type
  Simbolos = array of cardinal;

type
  TTipoSintagma = (tsNulo, tsSintagma, tsTag, tsEspaco, tsPontuacao, tsMetaDado);

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
  private
    FXML: PChar;
    FPXML: PChar;
    procedure PularEspacos;
    procedure PularTagAtual;
    procedure LerEspacos(var s: TTagSintagma);
    procedure LerPontuacao(var s: TTagSintagma);
    procedure LerTag(var s: TTagSintagma);
    procedure LerTexto(var s: TTagSintagma);
  public
    constructor Criar(XML: string); overload;
    constructor Criar(XML: PChar); overload;
    destructor Destruir;
    function LerSintagma(var s: TTagSintagma): TTipoSintagma;
    function LerPropriedadeTag(p: string; s: TTagSintagma): string;
    procedure LerAteTag(var s: TTagSintagma; AteTag: string);
  end;

implementation

constructor TONTTokenizer.Criar(XML: string);
begin
  Criar(Pchar(XML));
end;

constructor TONTTokenizer.Criar(XML: PChar);
begin
  FXML := XML;
  FPXML := FXML;
  if pos(#239#187#191, FPXML) = 1 then // saltando o UTF-8 BOM
     inc(FPXML, 3);
end;

destructor TONTTokenizer.Destruir;
begin

end;

procedure TONTTokenizer.PularEspacos;
begin
  while (FPXML^ in [#13, #10, #32, #9, '|']) do
  begin
    inc(FPXML);
  end;
end;

procedure TONTTokenizer.PularTagAtual;
begin
  while (FPXML^ <> #0) and (FPXML^ <> '>') do
  begin
    inc(FPXML);
  end;
  if FPXML^ = '>' then
    inc(FPXML);
end;

procedure TONTTokenizer.LerEspacos(var s: TTagSintagma);
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

procedure TONTTokenizer.LerPontuacao(var s: TTagSintagma);
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

procedure TONTTokenizer.LerTag(var s: TTagSintagma);
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

procedure TONTTokenizer.LerTexto(var s: TTagSintagma);
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

function TONTTokenizer.LerSintagma(var s: TTagSintagma): TTipoSintagma;

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

function TONTTokenizer.LerPropriedadeTag(p: string; s: TTagSintagma): string;
var
  i, f: smallint;
  //quote: boolean;
begin
  result := '';
  //quote := false;

  if s.valor[1] <> '<' then
    exit;

  i := pos(p+'=', s.valor) + length(p) + 1;
  if i = 0 then
    exit;

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

procedure TONTTokenizer.LerAteTag(var s: TTagSintagma; AteTag: string);
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

end.


