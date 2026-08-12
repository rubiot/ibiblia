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
    function ParseLine(const Line: string; Owner: TObject): TSyntagmList;
    property TokenizerVersion: Integer read FTokenizerVersion write FTokenizerVersion;
  end;

implementation

{ TONTParser }

function TONTParser.ParseLine(const Line: string; Owner: TObject): TSyntagmList;
var
  Index: Integer;
  Tokenizer: TONTTokenizer;
  Token: TSyntagmToken;
  CurrentSyntagm: TSyntagm;
  Syntagms: TSyntagmList;
  TemporaryColor: TColor;
  TemporaryItalic: Boolean;
  TemporarySuperscript: Boolean;
begin
  TemporaryColor       := clDefault;
  TemporaryItalic      := False;
  TemporarySuperscript := False;
  CurrentSyntagm := nil;
  Syntagms := TSyntagmList.Create;
  Tokenizer := nil;
  try
    Tokenizer := TTokenizerFactory.CreateTokenizer(FTokenizerVersion, Line);
    while Tokenizer.ReadSyntagm(Token) <> skNull do
    begin
      if Token.Kind = skTag then
      begin
        if Token.Value.StartsWith('<RF') then
        begin
          Tokenizer.ReadToTag(Token, '<Rf>');
          Token.Kind := skMetadata;
        end else if AnsiStartsStr('<TS', Token.Value) then
        begin
          Tokenizer.ReadToTag(Token, '<Ts>');
          Token.Kind := skMetadata;
        end else if (AnsiStartsStr('<WG', Token.Value) or AnsiStartsStr('<WH', Token.Value)) and
                     Assigned(CurrentSyntagm) and Assigned(CurrentSyntagm.Strong) and
                     (CurrentSyntagm.Kind = skSyntagm) then // update the previous syntagm
        begin
          //if (CurrentSyntagm.Type <> skSyntagm) then
          //  raise Exception.Create(CurrentSyntagm.Gist);
          CurrentSyntagm.Strong.Add(Copy(Token.Value, 3, Length(Token.Value)-3));
          CurrentSyntagm.RawText := CurrentSyntagm.RawText + Token.Value;
          Continue;
        end else if AnsiStartsStr('<WT', Token.Value) and Assigned(CurrentSyntagm) and Assigned(CurrentSyntagm.Morph) then // update the previous syntagm
        begin
          CurrentSyntagm.Morph.Add(Copy(Token.Value, 4, Length(Token.Value)-4));
          CurrentSyntagm.RawText := CurrentSyntagm.RawText + Token.Value;
          Continue;
        end else if AnsiStartsStr('<font color', Token.Value) then
        begin
          TemporaryColor := HTML2Color(Tokenizer.ReadTagProperty('color', Token));
        end else if Token.Value = '</font>' then
        begin
          TemporaryColor := clDefault;
        end else if Token.Value = '<sup>' then
        begin
          TemporarySuperscript := True;
        end else if Token.Value = '</sup>' then
        begin
          TemporarySuperscript := False;
        end else if (Token.Value = '<FI>') or (Token.Value = '<Fi>') then
        begin
          TemporaryItalic := Token.Value[3] = 'I';
          // No additional action; <FI> and <Fi> still require labels.
        end else
          Token.Kind := skMetadata;
      end else if (Token.Kind = skWhitespace) and (Token.Value = '|') then
      begin
        if Assigned(CurrentSyntagm) then
          CurrentSyntagm.RawText := CurrentSyntagm.RawText + Token.Value;
        Continue;
      end;
      Token.Color       := TemporaryColor;
      Token.Italic      := TemporaryItalic;
      Token.Superscript := TemporarySuperscript;
      CurrentSyntagm := TSyntagm.Create(Token, Owner);
      Syntagms.Add(CurrentSyntagm);
    end;
    Result := Syntagms;
    Syntagms := nil;
  finally
    Tokenizer.Free;
    if Assigned(Syntagms) then
    begin
      for Index := Syntagms.Count - 1 downto 0 do
        Syntagms[Index].Free;
      Syntagms.Free;
    end;
  end;
end;

constructor TONTParser.Create(ATokenizerVersion: Integer = TTokenizerFactory.LATEST_VERSION);
begin
  FTokenizerVersion := TTokenizerFactory.NormalizeVersion(ATokenizerVersion);
end;

end.

