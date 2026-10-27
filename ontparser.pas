unit ONTParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, Syntagm, StrUtils, Graphics, HTMLColors;

type

  { TONTParser }

  TONTParser = class
  private
  public
    constructor Create;
    function ParseLine(line: string; owner: TObject): TSyntagmList;
  end;

implementation

{ TONTParser }

function TONTParser.ParseLine(line: string; owner: TObject): TSyntagmList;
var
  tokenizer: TONTTokenizer;
  token: TTagSintagma; // token
  sintagma, lastWordSyntagm: TSyntagm;
  sintagmas: TSyntagmList;
  FTmpCor: TColor;
  FTmpItalico: Boolean;
  FTmpSobrescrito: Boolean;
begin
  FTmpCor         := clDefault;
  FTmpItalico     := false;
  FTmpSobrescrito := false;
  sintagma := nil;
  lastWordSyntagm := nil;
  sintagmas := TSyntagmList.Create;
  tokenizer := TTokenizerFactory.CreatePreferredTokenizer(line);
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
      end else if (AnsiStartsText('<WG', token.valor) or AnsiStartsText('<WH', token.valor)) and
                   assigned(lastWordSyntagm) and assigned(lastWordSyntagm.Strong) and
                   (lastWordSyntagm.Kind = tsSintagma) then // atualizar sintagma anterior
      begin
        lastWordSyntagm.Strong.Add(copy(token.valor, 3, length(token.valor)-3));
        lastWordSyntagm.RawText := lastWordSyntagm.RawText + token.valor;
        continue;
      end else if AnsiStartsText('<WT', token.valor) and assigned(lastWordSyntagm) and assigned(lastWordSyntagm.Morph) then // atualizar sintagma anterior
      begin
        lastWordSyntagm.Morph.Add(copy(token.valor, 4, length(token.valor)-4));
        lastWordSyntagm.RawText := lastWordSyntagm.RawText + token.valor;
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
        sintagma.RawText := sintagma.RawText + token.valor;
      continue;
    end;
    token.cor         := FTmpCor;
    token.italico     := FTmpItalico;
    token.sobrescrito := FTmpSobrescrito;
    sintagma := TSyntagm.Create(token, owner);
    if sintagma.Kind = tsSintagma then
      lastWordSyntagm := sintagma
    else if sintagma.Kind = tsEspaco then
      lastWordSyntagm := nil;
    sintagmas.Add(sintagma);
  end;
  tokenizer.Destroy;
  result := sintagmas;
end;

constructor TONTParser.Create;
begin
 // default
end;

end.

