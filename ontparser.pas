unit ONTParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, Sintagma, StrUtils, Graphics, HTMLColors;

type

  { TONTParser }

  TONTParser = class
  private
  public
    constructor Create;
    function ParseLine(line: string; owner: TObject): TSintagmaList;
  end;

implementation

{ TONTParser }

function TONTParser.ParseLine(line: string; owner: TObject): TSintagmaList;
var
  tokenizer: TONTTokenizer;
  token: TTagSintagma; // token
  sintagma: TSintagma;
  sintagmas: TSintagmaList;
  FTmpCor: TColor;
  FTmpItalico: Boolean;
  FTmpSobrescrito: Boolean;
begin
  FTmpCor         := clDefault;
  FTmpItalico     := false;
  FTmpSobrescrito := false;
  sintagma := nil;
  sintagmas := TSintagmaList.Create;
  tokenizer := TONTTokenizer.Criar(line);
  while tokenizer.LerSintagma(token) <> tsNulo do
  begin
    if token.tipo = tsTag then
    begin
      if token.valor.StartsWith('<RF') then
      begin
        tokenizer.LerAteTag(token, '<Rf>');
        token.tipo := tsMetaDado;
      end else if AnsiStartsStr('<TS', token.valor) then
      begin
        tokenizer.LerAteTag(token, '<Ts>');
        token.tipo := tsMetaDado;
      end else if (AnsiStartsStr('<WG', token.valor) or AnsiStartsStr('<WH', token.valor)) and
                   assigned(sintagma) and assigned(sintagma.Strong) and
                   (sintagma.Tipo = tsSintagma) then // atualizar sintagma anterior
      begin
        //if (sintagma.Tipo <> tsSintagma) then
        //  raise Exception.Create(sintagma.Gist);
        sintagma.Strong.Add(copy(token.valor, 3, length(token.valor)-3));
        sintagma.TextoBruto := sintagma.TextoBruto + token.valor;
        continue;
      end else if AnsiStartsStr('<WT', token.valor) and assigned(sintagma) and assigned(sintagma.Morf) then // atualizar sintagma anterior
      begin
        sintagma.Morf.Add(copy(token.valor, 4, length(token.valor)-4));
        sintagma.TextoBruto := sintagma.TextoBruto + token.valor;
        continue;
      end else if AnsiStartsStr('<font color', token.valor) then
      begin
        FTmpCor := HTML2Color(tokenizer.LerPropriedadeTag('color', token));
      end else if token.valor = '</font>' then
      begin
        FTmpCor := clDefault;
      end else if token.valor = '<sup>' then
      begin
        FTmpSobrescrito := true;
      end else if token.valor = '</sup>' then
      begin
        FTmpSobrescrito := false;
      end else if (token.valor = '<FI>') or (token.valor = '<Fi>') then
      begin
        FTmpItalico := token.valor[3] = 'I';
        // nada a fazer. Para <FI> e <Fi>, queremos criar o label
      end else
        token.tipo := tsMetaDado;
    end else if (token.tipo = tsEspaco) and (token.valor = '|') then
    begin
      if assigned(sintagma) then
        sintagma.TextoBruto := sintagma.TextoBruto + token.valor;
      continue;
    end;
    token.cor         := FTmpCor;
    token.italico     := FTmpItalico;
    token.sobrescrito := FTmpSobrescrito;
    sintagma := TSintagma.Criar(token, owner);
    sintagmas.Add(sintagma);
  end;
  tokenizer.Destruir;
  result := sintagmas;
end;

constructor TONTParser.Create;
begin
 // default
end;

end.

