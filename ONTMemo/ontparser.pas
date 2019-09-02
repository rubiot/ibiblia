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
  public
    constructor Create(text: string);
    destructor Destroy; override;
    function NextChunk: TToken;
  end;

implementation

{ TONTParser }

function TONTParser.NextChunk: TToken;
begin
  FTokenizer.ReadToken;

  case FTokenizer.Token.Kind of
    ttNull:
    begin
      if not FToken.Text.IsEmpty then
      begin
        Result := FToken;
        FToken.Kind := ttNull;
        FToken.Text := '';
      end else
        Result := FToken;
      exit;
    end;
    ttMetadata:
      ;
    ttSpace:
      if FTokenizer.Token.Text <> '|' then
        FToken.Text := FToken.Text + FTokenizer.Token.Text;
    ttPunctuation, ttSyntagm:
      FToken.Text := FToken.Text + FTokenizer.Token.Text;
    ttTag:
    begin
      if FToken.Text.Length > 0 then
      begin
        Result := FToken;
        FToken.Text := '';
      end else
        Result := FTokenizer.Token;
      FToken := FTokenizer.Token;
      exit;
    end;
  end;

  {
    while FTokenizer.ReadToken <> ttNull do
    begin
      case FTokenizer.Token.Kind of
        tsMetaDado:
          ;
        tsEspaco:
          if token.valor <> '|' then
            chunk := chunk + token.valor;
        tsPontuacao, tsSintagma:
          chunk := chunk + token.valor;
        tsTag:
        begin
          if chunk.Length > 0 then
          begin
            Blocks.AddTextBlock(chunk).TextStyle.Assign(CurrentStyle);
            chunk := '';
          end;
          if token.valor.StartsWith('<TS') then
          begin
            if not (Blocks[Blocks.Count-1] is TKMemoParagraph) then
              Blocks.AddParagraph();
            Blocks[Blocks.Count-1].ParaStyle.FirstIndent := 0;

            with PushNewStyle do
            begin
              case token.valor[4] of
                '>','0':
                begin
                  Font.Size := Font.Size + 2;
                  Font.Style := [fsBold];
                  Font.Color := $424242;
                end;
                '1':
                begin
                  Font.Size := Font.Size + 1;
                  Font.Style := [fsBold, fsItalic];
                  Font.Color := clDefault;
                end;
                '2':
                begin
                  Font.Style := [fsBold, fsItalic];
                  Font.Color := clDefault;
                end;
                '3':
                begin
                  Font.Style := [fsItalic];
                  Font.Color := clDefault;
                end;
              end;
            end;
          end else if token.valor = '<Ts>' then
          begin
            ResetStyleStack;
            // TODO: FirstIndent changing previous paragraph...
            Blocks.AddParagraph().ParaStyle.FirstIndent := IfThen(FVerseMode = vmParagraph, 20);
          end else if token.valor.StartsWith('<RF') then
          begin
            linktext := FTokenizer.LerPropriedadeTag('q', token);
            if linktext.IsEmpty then
              linktext := FNoteID.ToString;
            token.valor := '';
            FTokenizer.LerAteTag(token, '<Rf>');
            token.valor := token.valor.Replace('<Rf>',  '');

            FNotes.Add(TNoteInfo.Create(FNoteID, FCurrentVerse, token.valor, linktext));
            with Blocks.AddHyperlink(linktext, (FNoteID-1).ToString) do
            begin
              TextStyle.Assign(TextStyle);
              TextStyle.Font.Color := NoteLinkColor;
              TextStyle.Font.Style := [fsBold];
              TextStyle.ScriptPosition := tpoSuperscript;
              OnClick := @HandleNoteLinkClick;
            end;
            inc(FNoteID);
          end
          else if token.valor = '<FI>' then
          begin
            with PushInheritedStyle do
            begin
              Font.Style := Font.Style + [fsItalic];
              Font.Color := clGrayText;
            end;
          end else if token.valor = '<FR>' then
          begin
            PushInheritedStyle.Font.Color := clRed
          end else if token.valor = '<FO>' then
          begin
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsBold]
          end
          else if token.valor = '<CM>' then
          begin
            if FVerseMode = vmParagraph then
              Blocks.AddParagraph().ParaStyle.FirstIndent := 20;
          end else if token.valor = '<CL>' then
          begin
            if FVerseMode = vmParagraph then
              Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
              //Blocks.InsertNewLine(Text.Length);
          end else
          begin
            token.valor := token.valor.ToLower;
            if token.valor.StartsWith('<font ') then
            begin
              if token.valor.Contains(' color=') then
                PushInheritedStyle.Font.Color := HTML2Color(FTokenizer.LerPropriedadeTag('color', token));
              if token.valor.Contains(' size=') then
              begin
                prop := FTokenizer.LerPropriedadeTag('size', token);
                if prop.Chars[0] in ['-', '+'] then
                  size := CurrentStyle.Font.Size + prop.ToInteger
                else
                  size := prop.ToInteger;
                PushInheritedStyle.Font.Size := size;
              end;
            end else if token.valor = '<b>' then
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsBold]
            else if token.valor = '<i>' then
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsItalic]
            else if token.valor = '<u>' then
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsUnderline]
            else if token.valor = '<s>' then
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsStrikeOut]
            else if token.valor = '<sup>' then
              PushInheritedStyle.ScriptPosition := tpoSuperscript
            else if token.valor = '<sub>' then
              PushInheritedStyle.ScriptPosition := tpoSubscript
            else if token.valor = '<br/>' then
              Blocks.AddParagraph().ParaStyle.FirstIndent := 0
            else if (token.valor = '</sub>')
                     or (token.valor = '</sup>')
                     or (token.valor = '</s>')
                     or (token.valor = '</u>')
                     or (token.valor = '</i>')
                     or (token.valor = '</b>')
                     or (token.valor = '<fo>')
                     or (token.valor = '<fr>')
                     or (token.valor = '<fi>')
                     or (token.valor = '</font>') then
              PopStyle;
          end;
        end;
    end;
    }
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

