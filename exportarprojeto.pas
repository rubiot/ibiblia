unit ExportarProjeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, contnrs, strutils, PCRE, LazUTF8;

function UnicodeToRTF(s: string): string;
function MakeStrongLink(strong: string; anchor: string): string;
function MakeMorphoLink(morpho: string; anchor: string): string;

type

  TConcordancia = class;

  { TLocucao }

  TLocucao = class(TObject)
  private
    FPai: TConcordancia;
    FLocucao: string;
    FHashTranslation: TFPHashList;
    function GetChave(index: Integer): string;
    function GetChaveFmt(index: Integer): string;
    function GetItem(index: Integer): TStringList;
    function GetQtde: Integer;
    function GetRef(chave: string): TStringList;
    procedure PutRef(chave: string; const AValue: TStringList);
  public
    constructor Create(pai: TConcordancia; locucao: string);
    destructor Destroy; override;
    function GetLocucaoRTF: string;
    property Ref[chave: string]: TStringList read GetRef write PutRef; default;
    property Chaves[index: Integer]: string read GetChave;
    property ChavesFmt[index: Integer]: string read GetChaveFmt;
    property Items[index: Integer]: TStringList read GetItem;
    property Qtde: Integer read GetQtde;
    property Locucao: string read FLocucao;
  end;

type

  { TConcordancia }

  TConcordancia = class(TObject)
  private
    FHashLocucoes: TFPHashList;
    FDetalhada: boolean;
    reRemoveMorfo: IRegex;
    reUnicodeChars: IRegex;
    reSintagma: IRegex;
    reTag: IRegex;
    function GetChave(i: Integer): string;
    function GetItem(i: Integer): TLocucao;
    function GetLocucao(chave: string): TLocucao;
    function GetQtde: integer;
    procedure PutLocucao(chave: string; const AValue: TLocucao);

  public
    constructor Create;
    destructor Destroy; override;
    //property Locucoes[chave: string]: TLocucao read GetLocucao write PutLocucao; default;
    function GetStrongRTF(lema: string; s: string): string;
    //property Strong[s: string]: string read GetStrongRTF;
    property Chaves[i: Integer]: string read GetChave;
    property Items[i: Integer]: TLocucao read GetItem;
    property Qtde: integer read GetQtde;
    property Detalhada: boolean read FDetalhada write FDetalhada;
    procedure AdicionarLocucao(pares: TStringList; ref: string);
    procedure Limpar;
    function Str: string;
  end;

implementation

function UnicodeToRTF(s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: integer;
begin
  result := '';
  p:=PChar(s);
  repeat
    unicode:=UTF8CodepointToUnicode(p,CharLen);
    if unicode > 127 then
      result := result + format('\u%d?', [unicode])
    else if unicode > 0 then
      result := result + chr(unicode);
    inc(p,CharLen);
  until (CharLen=0) or (unicode=0);
end;

function MakeStrongLink(strong: string; anchor: string): string;
var
  font: string;
begin
  if RegexCreate('\p{Greek}', [rcoUTF8]).Matches(anchor).Count > 0 then
    font := '\f1'
  else if RegexCreate('\p{Hebrew}', [rcoUTF8]).Matches(anchor).Count > 0 then
    font := '\f2'
  else
    font := '\f0';

  result := format('{\field{\*\fldinst HYPERLINK "tw://[strong]?%s"}{\fldrslt%s\b %s}}',
                   [UpperCase(strong), font, UnicodeToRTF(anchor)]);
end;

function MakeMorphoLink(morpho: string; anchor: string): string;
begin
  result := format('{\field{\*\fldinst HYPERLINK "tw://[morph]?%s"}{\fldrslt\cf6 %s}}', [UpperCase(morpho), UnicodeToRTF(anchor)]);
end;

function ExtrairTexto(s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: integer;
  tag: boolean;
begin
  result := '';
  tag := false;
  p:=PChar(s);
  repeat
    unicode:=UTF8CodepointToUnicode(p,CharLen);
    if not tag and (CharLen = 1) and (chr(unicode) = '<') then
      tag := true;

    if (unicode > 0) and not tag then
    begin
      if CharLen >= 1 then result := result + chr(byte(P[0]));
      if CharLen >= 2 then result := result + chr(byte(P[1]));
      if CharLen >= 3 then result := result + chr(byte(P[2]));
      //result := result + chr(unicode);
    end;
    if tag and (CharLen = 1) and (chr(unicode) = '>') then
      tag := false;

    inc(p,CharLen);

    if not tag and (unicode = ord('>')) and (P[0] <> '<') then
      result := result + ' ';
  until (CharLen=0) or (unicode=0);
end;

function ExtrairTags(s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: integer;
  tag: boolean;
begin
  result := '';
  tag := false;
  p:=PChar(s);
  repeat
    unicode:=UTF8CodepointToUnicode(p,CharLen);
    if not tag and (CharLen = 1) and (chr(unicode) = '<') then
      tag := true;

    if (unicode > 0) and tag then
    begin
      if CharLen >= 1 then result := result + chr(byte(P[0]));
      if CharLen >= 2 then result := result + chr(byte(P[1]));
      if CharLen >= 3 then result := result + chr(byte(P[2]));
      //result := result + chr(unicode);
    end;
    if tag and (CharLen = 1) and (chr(unicode) = '>') then
      tag := false;
    inc(p,CharLen);
  until (CharLen=0) or (unicode=0);
end;

{ TConcordancia }

function TConcordancia.GetLocucao(chave: string): TLocucao;
var
  tags{, texto}: string;
begin
  tags := ExtrairTags(chave);
  //texto := ExtrairTexto(chave);

  result := TLocucao(FHashLocucoes.Find(tags));
  if result = nil then
  begin
    result := TLocucao.Create(Self, IfThen(FDetalhada, chave, tags));
    FHashLocucoes.Add(tags, result);
  end;
end;

function TConcordancia.GetQtde: integer;
begin
  result := FHashLocucoes.Count;
end;

function TConcordancia.GetItem(i: Integer): TLocucao;
begin
  result := nil;
  if (i >= 0) and (i < Self.Qtde) then
    result := TLocucao(FHashLocucoes[i]);
end;

function TConcordancia.GetChave(i: Integer): string;
begin
  result := '';
  if (i >= 0) and (i < Self.Qtde) then
    result := FHashLocucoes.NameOfIndex(i);
end;

function TConcordancia.GetStrongRTF(lema: string; s: string): string;
var
  l, p, r, u, c: integer;
  locucao: TLocucao;
  strong, ultima: string;
begin
  c := 0;
  strong := format('<W%s>', [s]).ToLower;
  result := MakeStrongLink(s, lema) + ' (__OCORRENCIAS__)\par';

  for l:=0 to FHashLocucoes.Count-1 do
  begin
    if Chaves[l].Contains(strong) then
    begin
      locucao := Self.Items[l];
      result := result + locucao.GetLocucaoRTF;

      for p:=0 to locucao.Qtde-1 do
      begin
        result := result +
                  format('{\i %s (%d)} \par\li500 ', [UnicodeToRTF(locucao.ChavesFmt[p].Replace(';',' ')), locucao.Items[p].Count]);
        ultima := '';
        u := 0;

        for r:=0 to locucao.Items[p].Count-1 do
        begin
          inc(c); // total de ocorrências
          inc(u); // ocorrências no versículo
          if ultima <> locucao.Items[p].Strings[r] then
          begin
            if u > 1 then
              result := result + format(' (%d)', [u]);
            u := 0;
            ultima := locucao.Items[p].Strings[r];
            if r <> 0 then
              result := result + '; ';
            result := result + format('{\field{\*\fldinst HYPERLINK "tw://bible.*?id=%s"}{\fldrslt\cf2 [vref]}}', [ AnsiReplaceText(ultima, ',', '.') ]);
          end;
        end;
        result := result + '\par\li250 ';
      end;
    end;
  end;

  if c > 0 then
    result := result.Replace('__OCORRENCIAS__', IntToStr(c))
  else
    result := '';
end;

procedure TConcordancia.PutLocucao(chave: string; const AValue: TLocucao);
begin

end;

constructor TConcordancia.Create;
begin
  FHashLocucoes  := TFPHashList.Create;
  reRemoveMorfo  := RegexCreate('<WT[^>]+>', [rcoUTF8, rcoIgnoreCase]);
  reUnicodeChars := RegexCreate('[^[:ascii:]]', [rcoUTF8]);
  reSintagma     := RegexCreate('^([^<]+)?((?:<[^>]+>)+)$', [rcoUTF8, rcoIgnoreCase]);
  reTag          := RegexCreate('<W([GH][^<]+)>|<WT([^ <]+)(?: l="([^"]+)")?>', [rcoUTF8, rcoIgnoreCase]);
  FDetalhada     := true;
end;

destructor TConcordancia.Destroy;
var
  i: integer;
begin
  for i:= 0 to Self.Qtde do
    Self.Items[i].Free;

  FHashLocucoes.Free;
  inherited Destroy;
end;

procedure TConcordancia.AdicionarLocucao(pares: TStringList; ref: string);
var
  p: smallint;
  s1, s2: string;
begin
  for p:=0 to pares.Count-1 do
    if (p mod 2) = 0 then // pares estão alternados na lista
    begin
      s1 := pares.Strings[p];     // grego<wg1492><wtv-2aap-nsm>
      s2 := pares.Strings[p+1];   // portugues

      if not FDetalhada then
        s1 := reRemoveMorfo.Replace(s1, '');

      GetLocucao(s1)[s2].Add(ref);
    end;
end;

procedure TConcordancia.Limpar;
begin

end;

function TConcordancia.Str: string;
begin
  result := FHashLocucoes.Strs;
end;

{ TLocucao }

function TLocucao.GetRef(chave: string): TStringList;
begin
  result := TStringList(FHashTranslation.Find(chave));
  if result = nil then
  begin
    result := TStringList.Create;
    FHashTranslation.Add(chave, result);
  end;
end;

function TLocucao.GetQtde: Integer;
begin
  result := FHashTranslation.Count;
end;

function TLocucao.GetItem(index: Integer): TStringList;
begin
  result := nil;
  if (index >= 0) and (index < Self.Qtde) then
    result := TStringList(FHashTranslation[index]);
end;

function TLocucao.GetChave(index: Integer): string;
begin
  result := '';
  if (index >= 0) and (index < Self.Qtde) then
    result := FHashTranslation.NameOfIndex(index);
end;

function TLocucao.GetChaveFmt(index: Integer): string;
var
  chave: string;
begin
  chave := Self.Chaves[index];
  result := chave
              .Replace('-;-','-')
              // mesóclises
              .Replace('r-ei', 'rei')
              .Replace('r-ás', 'rás')
              .Replace('r-á', 'rá')
              .Replace('r-emos', 'remos')
              .Replace('r-eis', 'reis')
              .Replace('r-ão', 'rão')
              .Replace('r-ia', 'ria')
              .Replace('r-ias', 'rias')
              .Replace('r-íamos', 'ríamos')
              .Replace('r-íeis', 'ríeis')
              .Replace('r-iam', 'riam')
              .Replace('á-ei', 'arei')
              .Replace('á-ás', 'arás')
              .Replace('á-á', 'ará')
              .Replace('á-emos', 'aremos')
              .Replace('á-eis', 'areis')
              .Replace('á-ão', 'arão')
              .Replace('á-ia', 'aria')
              .Replace('á-ias', 'arias')
              .Replace('á-íamos', 'aríamos')
              .Replace('á-íeis', 'aríeis')
              .Replace('á-iam', 'ariam')
              // ênclises
              {.Replace('á-á', 'ar-l')
              .Replace('ê-l', 'er-l')
              .Replace('i-l', 'ir-l')
              .Replace('ô-l', 'or-l')}
              // casos restantes
              .Replace(';',' ')
              .Replace('- ', ' ')
              {
              // ênclises
              //.Replace('á-l', 'ar-l')
              //.Replace('ê-l', 'er-l')
              //.Replace('i-l', 'ir-l')
              //.Replace('ô-l', 'or-l')}
              ;

  if result.EndsWith('á-') then
    result := result.Replace('á-','ar');
  if result.EndsWith('ê-') then
    result := result.Replace('ê-','er');
  if result.EndsWith('-') then
    result := Copy(result, 1, Length(result)-1);

  if chave.Contains('-') then
    DebugLn(Format('"%s" => "%s"', [chave, result]));
end;

procedure TLocucao.PutRef(chave: string; const AValue: TStringList);
begin

end;

constructor TLocucao.Create(pai: TConcordancia; locucao: string);
begin
  FPai := pai;
  FLocucao := locucao;
  FHashTranslation := TFPHashList.Create;
end;

destructor TLocucao.Destroy;
var
  i: integer;
begin
  for i:=0 to Self.Qtde do
    Self.Items[i].Free;

  FHashTranslation.Free;
  inherited Destroy;
end;

function TLocucao.GetLocucaoRTF: string;
var
  m, m1: IMatchCollection;
  i: smallint;
  words: TStringArray;
  w, wt, tag: string;
begin
  result := '';

  // detailed: word with link to strong + morphology tag with link to dictionary
  //           word1<WG1><WTX>;word2<WG2><WTZ>
  // simple:  Strong's tag with link
  //          <WG1>;<WG2>
  result := '\par\li0 {\b\''95  }';
  words := FLocucao.Split(';');
  for w in words do
  begin
    m := FPai.reSintagma.Matches(w);
    if m.Count = 0 then
      break;
    wt := m[0].Groups[1].GetValue;
    tag := m[0].Groups[2].GetValue;

    m1 := FPai.reTag.Matches(tag);
    for i:=0 to m1.Count-1 do
    begin
      if m1[i].Groups[1].GetMatched then // strong
        result := result + IfThen(not result.EndsWith(' '), ' ') + MakeStrongLink(m1[i].Groups[1].GetValue, IfThen(FPai.Detalhada, wt, UpperCase(m1[i].Groups[1].GetValue)))
      else if m1[i].Groups[2].GetMatched then // morphology
        result := result + format(' {\super %s }', [MakeMorphoLink(m1[i].Groups[2].GetValue, UpperCase(m1[i].Groups[2].GetValue))]);
    end;
  end;
  result := result + '\par\li250 ';
end;

end.

