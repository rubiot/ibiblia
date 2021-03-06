unit ONTTokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LCLProc, Dialogs, LazUTF8, Graphics;

type
  TTokenKind = (ttNull, ttSyntagm, ttTag, ttSpace, ttPunctuation, ttMetadata, ttEOL, ttEOF);

type
  TToken = record
    Kind: TTokenKind;
    Text: string;
  end;

  TTokenMarker = record
    Token: TToken;
    Address: PChar;
  end;

  { TONTTokenizer }

  TONTTokenizer = class
  private
    FXML: PChar;
    FPXML: PChar;
    FToken: TToken;
    FLastToken: TTokenMarker;
    procedure SkipSpaces;
    procedure SkipCurrentTag;
    procedure ReadSpaces(var s: TToken);
    procedure ReadPunctuation(var s: TToken);
    procedure ReadTag(var s: TToken);
    procedure ReadText(var s: TToken);
  public
    constructor Create(XML: string); overload;
    constructor Create(XML: PChar); overload;
    destructor Destroy; override;
    function ReadToken: TTokenKind;
    procedure UnreadToken;
    function ReadProperty(prop: string): string;
    procedure ReadUntilTag(UntilTag: string; KeepSurroundingTags: Boolean = False);
    procedure ReadUntilTag(UntilType: TTokenKind);

    property Token: TToken read FToken;
  end;

implementation

constructor TONTTokenizer.Create(XML: string);
begin
  Create(Pchar(XML));
end;

constructor TONTTokenizer.Create(XML: PChar);
begin
  FXML := XML;
  FPXML := FXML;
  if pos(#239#187#191, FPXML) = 1 then // skipping UTF-8 byte-order-marker
     inc(FPXML, 3);
  FToken.Kind := ttNull;
  FToken.Text := '';
  FLastToken.Address := nil;
end;

destructor TONTTokenizer.Destroy;
begin

end;

procedure TONTTokenizer.SkipSpaces;
begin
  while (FPXML^ in [#13, #10, #32, #9, '|']) do
  begin
    inc(FPXML);
  end;
end;

procedure TONTTokenizer.SkipCurrentTag;
begin
  while (FPXML^ <> #0) and (FPXML^ <> '>') do
  begin
    inc(FPXML);
  end;
  if FPXML^ = '>' then
    inc(FPXML);
end;

procedure TONTTokenizer.ReadSpaces(var s: TToken);
var
  c: integer;
begin
  c := 0;
  while FPXML^ in [#32, #9, '|'] do
  begin
    inc(c);
    inc(FPXML);
  end;
  s.Text := copy(FPXML-c, 0, c);
  s.Kind := ttSpace;
end;

procedure TONTTokenizer.ReadPunctuation(var s: TToken);
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
  s.Text := copy(FPXML-c, 0, c);
  s.Kind := ttPunctuation;
end;

procedure TONTTokenizer.ReadTag(var s: TToken);
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
  s.Text := copy(FPXML-c, 0, c);
  s.Kind := ttTag;

  //if s.Text = '<FI>' then
  //  s.Text := '['
  //else if s.Text = '<Fi>' then
  //  s.Text := ']';
end;

procedure TONTTokenizer.ReadText(var s: TToken);
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
  s.Text := copy(FPXML-c, 0, c);
  s.Kind := ttSyntagm;
end;

function TONTTokenizer.ReadToken: TTokenKind;

  function Contained(c: cardinal; v: array of cardinal): boolean;
  var
    i: integer;
  begin
    result := false;
    for i:=low(v) to high(v) do
      if v[i] = c then
        result := true;
  end;

  procedure ReadUntil(var s: TToken; ate: array of cardinal);
  var
    t: integer;
    ini: PChar;
  begin
    ini := FPXML;
    while (FPXML^ <> #0) and not Contained(UTF8CodePointToUnicode(FPXML, t), ate) do
      inc(FPXML, t);

    s.Text := copy(ini, 0, FPXML-ini);
  end;

  procedure ReadUntilInclusive(var s: TToken; ate: array of cardinal);
  var
    t: integer;
  begin
    ReadUntil(s, ate);
    if UTF8CodePointToUnicode(FPXML, t) <> 0 then
    begin
      s.Text := s.Text + copy(FPXML, 0, t);
      inc(FPXML, t);
    end;
  end;

  procedure ReadWhile(var s: TToken; enquanto: array of cardinal);
  var
    t: integer;
    ini: PChar;
  begin
    ini := FPXML;
    while (FPXML^ <> #0) and Contained(UTF8CodePointToUnicode(FPXML, t), enquanto) do
      inc(FPXML, t);

    s.Text := copy(ini, 0, FPXML-ini);
  end;

  function charKind: TTokenKind;
  var
    t: integer;
    c: Cardinal;
  begin
    result := ttSyntagm;
    c := UTF8CodePointToUnicode(FPXML, t);
    if c = 0 then
      result := ttEOF
    else if c = ord('<') then
      result := ttTag
    else if Contained(c, [ord(#32), ord(#9), ord('|')]) then
      result := ttSpace
    else if Contained(c, [ord(#13), ord(#10)]) then
      result := ttEOL
    else if Contained(c, [ord('"'), ord('.'), ord(','), ord(';'), ord(':'),
                        ord('!'), ord('?'), ord('('), ord(')'),
                        183, 903, 8220, 8221{, ord('-')}]) // hack para ··“”
    then
      result := ttPunctuation;
  end;
var
  hyphen: string;
begin
  FLastToken.Token   := FToken;
  FLastToken.Address := FPXML;

  FToken.Text := '';
  FToken.Kind := charKind;

  case FToken.Kind of
    ttSpace:
      ReadWhile(FToken, [ord(#32), ord(#9), ord('|')]);
    ttPunctuation:
      ReadWhile(FToken, [ord('"'), ord('.'), ord(','), ord(';'), ord(':'),
                      ord('!'), ord('?'), ord('('), ord(')'),
                      183, 903, 8220, 8221{, ord('-')}]); // hack para ··“”
    ttTag:
      ReadUntilInclusive(FToken, [ord('>'), ord(#13), ord(#10)]);
    ttEOL:
      ReadWhile(FToken, [ord(#13), ord(#10)]);
    ttEOF:
      ;
  else
    if FPXML^ = '-' then
    begin
      hyphen := '-';
      Inc(FPXML);
    end else
      hyphen := '';

    ReadUntil(FToken, [ord('<'), ord(#32), ord(#9), ord(#13), ord(#10), ord('|'), ord('"'), ord('.'),
               ord(','), ord(';'), ord(':'), ord('!'), ord('?'), ord('-'),
               ord('('), ord(')'), 183, 903, 8220, 8221]);  // hack para ··“”
    FToken.Text := hyphen + FToken.Text;
  end;

  //MessageDlg('Fechar projeto', '"'+FToken.Text+'"', mtConfirmation, [], 0);
  result := FToken.Kind;
end;

procedure TONTTokenizer.UnreadToken;
begin
  if not assigned(FLastToken.Address) then
    raise Exception.Create('There'' no previous token!');

  FToken := FLastToken.Token;
  FPXML  := FLastToken.Address;
  FLastToken.Address := nil;
end;

function TONTTokenizer.ReadProperty(prop: string): string;
var
  i, f: smallint;
  //quote: boolean;
begin
  result := '';
  //quote := false;

  if FToken.Text[1] <> '<' then
    exit;

  i := pos(prop+'=', FToken.Text);
  if i = 0 then
    exit;

  i := i + length(prop) + 1;

  if FToken.Text[i] = '"' then
  begin
    inc(i);
    //quote := true;
    f := i + pos('"', copy(FToken.Text, i, length(FToken.Text))) - 1;
  end else
  begin
    f := i + pos(' ', copy(FToken.Text, i, length(FToken.Text))) - 1; // será que temos mais de uma propriedade?
    if f = (i-1) then
      f := i + pos('>', copy(FToken.Text, i, length(FToken.Text))) - 1;
  end;

  //i := i + pos('"', copy(FToken.Text, i, length(FToken.Text)));
  //f := i + pos('"', copy(FToken.Text, i, length(FToken.Text))) - 1;

  result := copy(FToken.Text, i, f-i);
end;

procedure TONTTokenizer.ReadUntilTag(UntilTag: string; KeepSurroundingTags: Boolean = False);
var
  valor: string;
begin
  valor := IfThen(KeepSurroundingTags, FToken.Text, '');

  ReadToken;
  while (FToken.Kind <> ttNull) and (FToken.Text <> UntilTag) do
  begin
    valor := concat(valor, FToken.Text);
    ReadToken;
  end;

  FToken.Text := valor + IfThen(KeepSurroundingTags, FToken.Text, '');
end;

procedure TONTTokenizer.ReadUntilTag(UntilType: TTokenKind);
var
  valor: string;
begin
  valor := '';

  ReadToken;
  while not (FToken.Kind in [ttNull, UntilType]) do
  begin
    valor := concat(valor, FToken.Text);
    ReadToken;
  end;
  UnreadToken;
end;

end.


