unit HTMLRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, KMemo, KEditCommon, ONTTokenizer, HTMLColors,
  Graphics, Contnrs;

type
  { THTMLRenderer }
  
  THTMLRenderer = class(TKMemo)
  private
    FStyleStack: TObjectStack;
  protected
    function GetCurrentStyle: TKMemoTextStyle;
    function PushNewStyle: TKMemoTextStyle;
    function PushInheritedStyle: TKMemoTextStyle;
    procedure PopStyle;
    procedure ResetStyleStack;
    procedure SetupDefaultStyle; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure RenderHTML(const HTMLText: string);
    procedure RenderSpan(cont: TKMemoBlocks; txt: string); virtual;
    
    property CurrentStyle: TKMemoTextStyle read GetCurrentStyle;
  end;

implementation

{ THTMLRenderer }

constructor THTMLRenderer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := true;
  BorderStyle := bsNone;
  FStyleStack := TObjectStack.Create;
end;

destructor THTMLRenderer.Destroy;
begin
  FStyleStack.Free;
  inherited Destroy;
end;

function THTMLRenderer.GetCurrentStyle: TKMemoTextStyle;
begin
  if FStyleStack.Count > 0 then
    result := TKMemoTextStyle(FStyleStack.Peek)
  else
    result := TextStyle;
end;

function THTMLRenderer.PushNewStyle: TKMemoTextStyle;
begin
  result := TKMemoTextStyle(FStyleStack.Push(TKMemoTextStyle.Create));
  result.Assign(TextStyle);
end;

function THTMLRenderer.PushInheritedStyle: TKMemoTextStyle;
var
  current: TKMemoTextStyle;
begin
  current := CurrentStyle;
  result := PushNewStyle;
  result.Assign(current);
end;

procedure THTMLRenderer.PopStyle;
begin
  if FStyleStack.Count > 0 then
    FStyleStack.Pop.Free;
end;

procedure THTMLRenderer.ResetStyleStack;
begin
  while FStyleStack.Count > 0 do
    PopStyle;
  SetupDefaultStyle;
  PushNewStyle; // pushing default style onto stack
end;

procedure THTMLRenderer.SetupDefaultStyle;
begin
  // Override in subclasses to set specific default styling
end;

procedure THTMLRenderer.RenderHTML(const HTMLText: string);
begin
  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;
    RenderSpan(Blocks, HTMLText);
  finally
    Blocks.UnLockUpdate;
  end;
end;

procedure THTMLRenderer.RenderSpan(cont: TKMemoBlocks; txt: string);
var
  FTokenizer: TONTTokenizer;
  token: TTagSintagma;
  prop: string;
  chunk: string;
  size: integer;
begin
  chunk := '';
  FTokenizer := TTokenizerFactory.CreatePreferredTokenizer(txt);

  cont.LockUpdate;
  try
    while FTokenizer.LerSintagma(token) <> tsNulo do
    begin
      case token.tipo of
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
            cont.AddTextBlock(chunk).TextStyle.Assign(CurrentStyle);
            chunk := '';
          end;
          
          token.valor := token.valor.ToLower;
          if token.valor.StartsWith('<font ') then { font change }
          begin
            PushInheritedStyle;
            if token.valor.Contains(' color=') then
              CurrentStyle.Font.Color := HTML2Color(FTokenizer.LerPropriedadeTag('color', token));
            if token.valor.Contains(' size=') then
            begin
              prop := FTokenizer.LerPropriedadeTag('size', token);
              if prop.Chars[0] in ['-', '+'] then
                size := CurrentStyle.Font.Size + prop.ToInteger
              else
                size := prop.ToInteger;
              CurrentStyle.Font.Size := size;
            end;
          end else if token.valor = '<b>' then { bold }
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsBold]
          else if token.valor = '<i>' then { italic }
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsItalic]
          else if token.valor = '<u>' then { underline }
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsUnderline]
          else if token.valor = '<s>' then { strikeout }
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsStrikeOut]
          else if token.valor = '<sup>' then { superscript }
            PushInheritedStyle.ScriptPosition := tpoSuperscript
          else if token.valor = '<sub>' then { subscript }
            PushInheritedStyle.ScriptPosition := tpoSubscript
          else if token.valor = '<br/>' then { line break }
            cont.AddParagraph().ParaStyle.FirstIndent := 0
          else if (token.valor = '</sub>')
               or (token.valor = '</sup>')
               or (token.valor = '</s>')
               or (token.valor = '</u>')
               or (token.valor = '</i>')
               or (token.valor = '</b>')
               or (token.valor = '</font>') then
            PopStyle;
        end;
      end;
    end;

    if chunk.Length > 0 then
      cont.AddTextBlock(chunk).TextStyle.Assign(CurrentStyle);
  finally
    FreeAndNil(FTokenizer);
    cont.UnLockUpdate;
  end;
end;

{$IFDEF FPC}
initialization
  // Component registration not needed for FPC/Lazarus runtime
{$ENDIF}

end.
