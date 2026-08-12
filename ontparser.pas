unit ONTParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, Syntagm, StrUtils, Graphics, HTMLColors;

type

  { TONTParser }

  TONTParser = class
  private
    FTokenizerVersion: Integer;
  public
    constructor Create(ATokenizerVersion: Integer = TTokenizerFactory.LATEST_VERSION);
    function ParseLine(line: string; owner: TObject): TSyntagmList;
    property TokenizerVersion: Integer read FTokenizerVersion write FTokenizerVersion;
  end;

implementation

{ TONTParser }

function TONTParser.ParseLine(line: string; owner: TObject): TSyntagmList;
var
  i: Integer;
  tokenizer: TONTTokenizer;
  token: TSyntagmToken;
  sintagma: TSyntagm;
  sintagmas: TSyntagmList;
  FTmpCor: TColor;
  FTmpItalico: Boolean;
  FTmpSobrescrito: Boolean;
begin
  FTmpCor         := clDefault;
  FTmpItalico     := false;
  FTmpSobrescrito := false;
  sintagma := nil;
  sintagmas := TSyntagmList.Create;
  tokenizer := nil;
  try
    tokenizer := TTokenizerFactory.CreateTokenizer(FTokenizerVersion, line);
    while tokenizer.ReadSyntagm(token) <> skNull do
    begin
      if token.Kind = skTag then
      begin
        if token.Value.StartsWith('<RF') then
        begin
          tokenizer.ReadToTag(token, '<Rf>');
          token.Kind := skMetadata;
        end else if AnsiStartsStr('<TS', token.Value) then
        begin
          tokenizer.ReadToTag(token, '<Ts>');
          token.Kind := skMetadata;
        end else if (AnsiStartsStr('<WG', token.Value) or AnsiStartsStr('<WH', token.Value)) and
                     assigned(sintagma) and assigned(sintagma.Strong) and
                     (sintagma.Kind = skSyntagm) then // atualizar sintagma anterior
        begin
          //if (sintagma.Type <> skSyntagm) then
          //  raise Exception.Create(sintagma.Gist);
          sintagma.Strong.Add(copy(token.Value, 3, length(token.Value)-3));
          sintagma.RawText := sintagma.RawText + token.Value;
          continue;
        end else if AnsiStartsStr('<WT', token.Value) and assigned(sintagma) and assigned(sintagma.Morph) then // atualizar sintagma anterior
        begin
          sintagma.Morph.Add(copy(token.Value, 4, length(token.Value)-4));
          sintagma.RawText := sintagma.RawText + token.Value;
          continue;
        end else if AnsiStartsStr('<font color', token.Value) then
        begin
          FTmpCor := HTML2Color(tokenizer.ReadTagProperty('color', token));
        end else if token.Value = '</font>' then
        begin
          FTmpCor := clDefault;
        end else if token.Value = '<sup>' then
        begin
          FTmpSobrescrito := true;
        end else if token.Value = '</sup>' then
        begin
          FTmpSobrescrito := false;
        end else if (token.Value = '<FI>') or (token.Value = '<Fi>') then
        begin
          FTmpItalico := token.Value[3] = 'I';
          // nada a fazer. Para <FI> e <Fi>, queremos criar o label
        end else
          token.Kind := skMetadata;
      end else if (token.Kind = skWhitespace) and (token.Value = '|') then
      begin
        if assigned(sintagma) then
          sintagma.RawText := sintagma.RawText + token.Value;
        continue;
      end;
      token.Color         := FTmpCor;
      token.Italic     := FTmpItalico;
      token.Superscript := FTmpSobrescrito;
      sintagma := TSyntagm.Create(token, owner);
      sintagmas.Add(sintagma);
    end;
    Result := sintagmas;
    sintagmas := nil;
  finally
    tokenizer.Free;
    if Assigned(sintagmas) then
    begin
      for i := sintagmas.Count - 1 downto 0 do
        sintagmas[i].Free;
      sintagmas.Free;
    end;
  end;
end;

constructor TONTParser.Create(ATokenizerVersion: Integer = TTokenizerFactory.LATEST_VERSION);
begin
  FTokenizerVersion := TTokenizerFactory.NormalizeVersion(ATokenizerVersion);
end;

end.

