unit FormChapterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RichMemo, ONTTokenizer, RichMemoUtils, Projeto;

type

  { TFormChapterView }

  TFormChapterView = class(TForm)
    Button1: TButton;
    RichMemo1: TRichMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTokenizer: TONTTokenizer;
    procedure ReadTagContent(var s: TTagSintagma);
    procedure RenderChapter(verses: TStringList);
    procedure RenderVerse(txt: string);
  public
    procedure LoadChapter(verses: TStringList);
  end;

var
  FrmChapterView: TFormChapterView;

implementation

{ TFormChapterView }

procedure TFormChapterView.FormCreate(Sender: TObject);
begin
end;

procedure TFormChapterView.FormDestroy(Sender: TObject);
begin

end;

procedure TFormChapterView.ReadTagContent(var s: TTagSintagma);
var
  content, closeTag: string;
begin
  content := '';

  case s.valor of
    '<TS>', '<TS1>', '<TS2>', '<TS3>', '<TS4>', '<TS5>', '<TS6>', '<TS7>':
      closeTag := '<Ts>';
    '<FI>':
      closeTag := '<Fi>';
  end;

  FTokenizer.LerSintagma(s);
  while (s.tipo <> tsNulo) and (s.valor <> closeTag) do
  begin
    content := concat(content, s.valor);
    FTokenizer.LerSintagma(s);
  end;
  s.valor := content;
end;

procedure TFormChapterView.RenderChapter(verses: TStringList);
var
  verse: string;
begin
  RichMemo1.Clear;
  RichMemo1.Lines.BeginUpdate;

  for verse in verses do
    RenderVerse(verse);

  FreeAndNil(verses);
  RichMemo1.Lines.EndUpdate;
end;

procedure TFormChapterView.RenderVerse(txt: string);
var
  token: TTagSintagma;
begin
  FTokenizer := TONTTokenizer.Criar(txt);
  while FTokenizer.LerSintagma(token) <> tsNulo do
  begin
    case token.tipo of
      tsEspaco, tsPontuacao, tsSintagma: InsertStyledText(RichMemo1, token.valor, []);
      tsTag:
        begin
          if token.valor.StartsWith('<TS') then
          begin
            ReadTagContent(token);
            InsertColorStyledText(RichMemo1, token.valor + #13, clGray, [fsBold, fsItalic]);
          end else if token.valor.StartsWith('<FI>') then
          begin
            ReadTagContent(token);
            InsertColorStyledText(RichMemo1, token.valor, clGray, [fsItalic]);
          end; // else raise Exception.Create(Format('Unhandled tag: %s', [token.valor]));
        end;
      tsMetaDado: ;
    end;
  end;
  FreeAndNil(FTokenizer);
end;

procedure TFormChapterView.LoadChapter(verses: TStringList);
begin
  RenderChapter(verses);
  ShowModal;
end;

procedure TFormChapterView.Button1Click(Sender: TObject);
begin
  RenderVerse('<TS>The creation<Ts>In the beginning created God the heavens and the earth.');
end;

initialization
  {$I formchapterview.lrs}

end.

