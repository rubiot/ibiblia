unit ONTParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, Graphics, HTMLColors;

type

  { TONTParser }

  TONTParser = class
  private
    FTokenizer: TONTTokenizer;
    FToken: TToken;
    FChunk: TToken;

    function SameKind(current, new: TTokenKind): boolean;
  public
    constructor Create(text: string);
    destructor Destroy; override;
    function ReadChunk: TTokenKind;
    property Chunk: TToken read FChunk;
  end;

implementation

{ TONTParser }

function TONTParser.ReadChunk: TTokenKind;
begin
  FChunk := FTokenizer.Token;

  if FChunk.Kind = ttEOF then
  begin
    Result := ttNull;
    Exit;
  end;

  while FTokenizer.ReadToken <> ttEOF do
    if SameKind(FChunk.Kind, FTokenizer.Token.Kind) then
    begin
      FChunk.Kind := FTokenizer.Token.Kind;
      FChunk.Text := FChunk.Text + FTokenizer.Token.Text;
    end else
    begin
      Result := FChunk.Kind;
      Exit;
    end;

  Result := FChunk.Kind;
end;

function TONTParser.SameKind(current, new: TTokenKind): boolean;
begin
  case current of
    ttNull:
      Result := true;
    ttTag:
      Result := false;
    ttPunctuation, ttSpace, ttSyntagm:
      Result := new in [ttPunctuation, ttSpace, ttSyntagm];
  end;
end;

constructor TONTParser.Create(text: string);
begin
  inherited Create;
  FTokenizer := TONTTokenizer.Create(text);
  FToken.Text := '';
  FToken.Kind := ttNull;
end;

destructor TONTParser.Destroy;
begin
  FTokenizer.Free;
end;

end.

