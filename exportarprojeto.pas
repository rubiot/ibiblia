unit ExportarProjeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, contnrs, strutils, PCRE, LazUTF8;

function UnicodeToRTF(s: string): string;

type

  TConcordancia = class;

  { TLocucao }

  TLocucao = class(TObject)
  private
    FPai: TConcordancia;
    FLocucao: string;
    FHashLocucoes: TFPHashList;
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
    result := TLocucao.Create(Self, chave);
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
  result := format('{\field{\*\fldinst HYPERLINK "tw://[strong]?%s"}{\fldrslt%s\b %s}} (__OCORRENCIAS__)\par',
                   [s, IfThen(s.StartsWith('G'), '\f1', '\f2'), UnicodeToRTF(lema)]);

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
  reRemoveMorfo  := RegexCreate('<WT[^>]+>', [rcoUTF8,rcoIgnoreCase]);
  reSintagma     := RegexCreate('(^|;)([^<]+)(<W([HG]\d+)>)(<W([HG]\d+)>)?(<W([HG]\d+)>)?(<WT([^\ >]+)>)?(<W([HG]\d+)>)?(<WT([^\ >]+)>)?', [rcoUTF8,rcoIgnoreCase]);
  reUnicodeChars := RegexCreate('[^[:ascii:]]', [rcoUTF8]);
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
  result := TStringList(FHashLocucoes.Find(chave));
  if result = nil then
  begin
    result := TStringList.Create;
    FHashLocucoes.Add(chave, result);
  end;
end;

function TLocucao.GetQtde: Integer;
begin
  result := FHashLocucoes.Count;
end;

function TLocucao.GetItem(index: Integer): TStringList;
begin
  result := nil;
  if (index >= 0) and (index < Self.Qtde) then
    result := TStringList(FHashLocucoes[index]);
end;

function TLocucao.GetChave(index: Integer): string;
begin
  result := '';
  if (index >= 0) and (index < Self.Qtde) then
    result := FHashLocucoes.NameOfIndex(index);
end;

function TLocucao.GetChaveFmt(index: Integer): string;
begin
  result := Self.Chaves[index]
              .Replace(';', ' ')
              // pronomes átonos
              .Replace('- ','-')
              .Replace(' -','-')
              // mesóclises
              .Replace('-ei', 'ei')
              .Replace('-ás', 'ás')
              .Replace('-á', 'á')
              .Replace('-emos', 'emos')
              .Replace('-eis', 'eis')
              .Replace('-ão', 'ão')
              .Replace('-ia', 'ia')
              .Replace('-ias', 'ias')
              .Replace('-íamos', 'íamos')
              .Replace('-íeis', 'íeis')
              .Replace('-iam', 'iam')
              // ênclises
              //.Replace('á-l', 'ar-l')
              //.Replace('ê-l', 'er-l')
              //.Replace('i-l', 'ir-l')
              //.Replace('ô-l', 'or-l')
              ;
end;

procedure TLocucao.PutRef(chave: string; const AValue: TStringList);
begin

end;

constructor TLocucao.Create(pai: TConcordancia; locucao: string);
begin
  FPai := pai;
  FLocucao := locucao;
  FHashLocucoes := TFPHashList.Create;
end;

destructor TLocucao.Destroy;
var
  i: integer;
begin
  for i:=0 to Self.Qtde do
    Self.Items[i].Free;

  FHashLocucoes.Free;
  inherited Destroy;
end;

function TLocucao.GetLocucaoRTF: string;
  function iif(c: boolean; v, f: string): string;
  begin
    if c then
      result := v
    else
      result := f;
  end;
var
  m: IMatchCollection;
  i: smallint;
begin
  result := '\par\li0 {\b\''95  }';
  m := FPai.reSintagma.Matches(FLocucao);
  for i:=0 to m.Count-1 do
  begin
    if m[i].Groups[4].GetMatched then // strong
      result := result + format('{\field{\*\fldinst HYPERLINK "tw://[strong]?%s"}{\fldrslt\cf2 %s }}',
             [ UpperCase(m[i].Groups[4].GetValue), iif(FPai.Detalhada, UnicodeToRTF(m[i].Groups[2].GetValue), UpperCase(m[i].Groups[4].GetValue)+ ' ') ]);
    if m[i].Groups[10].GetMatched then // morfologia
      result := result + format(' {\super {\field{\*\fldinst HYPERLINK "tw://[morph]?%s"}{\fldrslt\cf6 %s}}} ',
             [ UpperCase(m[i].Groups[10].GetValue), UpperCase(m[i].Groups[10].GetValue) ]);
  end;
  result := result + '\par\li250 ';
end;

end.

