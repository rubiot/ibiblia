unit Versiculo;

{$mode objfpc}{$H+}

{TODO -cObrigatório: Rever algoritmo de associação/desassociação }

interface

uses
  Classes, SysUtils, StrUtils, ExtCtrls, StdCtrls, Controls, Graphics,
  iBibliaXML, Forms, LCLType, Math, LCLProc, Dialogs, LazUTF8;

type
  TSintagma = class;
  TVersiculo = class;

  PSintagma = ^TSintagma;
  PVersiculo = ^TVersiculo;

  TOnSintagmaEvent = procedure (Sender: TSintagma) of object;
  TOnAlterarVersiculoEvent = procedure () of object;

  TTipoListaPares =
  (
    tlMetaDados, // somente metadados. Ex.: "<WG1><WTADJ-NSM>"
    tlStrong,    // somente strong#.   Ex.: "<WG1>"
    tlTexto,     // somente texto.     Ex.: "Jesus"
    tlTudo       // texto e metadados. Ex.: "Jesus<WG1088><WTNPI>"
  );

  { TSintagmaList }

  TSintagmaList = class(TList)
  protected
    function GetS(Index: Integer): TSintagma;
    procedure PutS(Index: Integer; Item: TSintagma);
  public
    property Itens[Index: Integer]: TSintagma read GetS write PutS; default;
    Procedure AddList(AList : TSintagmaList); //override;
  end;

  { TSintagma }

  TSintagma = class
  private
    FTipo: TTipoSintagma;
    FTexto: string;
    FTextoCru: string;
    FPares: TSintagmaList;
    FIrmaos: TSintagmaList;
    FLabel: TLabel;
    FSelecionado: boolean;
    FVersiculo: TVersiculo;
    FStrong: TStringList;
    FMorf: TStringList;
    FCor: TColor;
    FSobrescrito: boolean;
    FItalico: boolean;
    procedure DoOnDblClick(Sender: TObject);
    procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure DoOnMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure DoOnMouseEnter(Sender: TObject);
    procedure DoOnMouseLeave(Sender: TObject);
    procedure DoOnLeftClick(Sender: TObject; Shift: TShiftState);
    procedure DoOnRightClick(Sender: TObject; Shift: TShiftState);
    function GetCorrelacionado: boolean;
    procedure SetApontado(const AValue: boolean);
    procedure SetCorrelacionado(const AValue: boolean);
  public
    constructor Criar(TheOwner: TVersiculo; s: TTagSintagma);
    destructor Destruir;
    procedure SelecaoMais;
    procedure SelecaoMenos;
    procedure InverterSelecao;
    procedure Desassociar;
    procedure DesassociarPares;
    function GetProximo: TSintagma;
    function GetChaveSugestao(t: TTipoListaPares): string;
    property Strong: TStringList read FStrong write FStrong;
    property Morf: TStringList read FMorf write FMorf;
    property LabelRef: TLabel read FLabel write FLabel;
    property Pares: TSintagmaList read FPares write FPares;
    property Irmaos: TSintagmaList read FIrmaos write FIrmaos;
    property VersiculoRef: TVersiculo read FVersiculo write FVersiculo;
    property Selecionado: boolean read FSelecionado;
    property Correlacionado: boolean read GetCorrelacionado write SetCorrelacionado;
    property Apontado: boolean write SetApontado;
    property Texto: string read FTexto write FTexto;
    //property ChaveSugestao: string read GetChaveSugestao;
    property Tipo: TTipoSintagma read FTipo write FTipo;
    property Italico: boolean read FItalico;
  published

  end;

  { TVersiculo }

  TVersiculo = class
  private
    { Private declarations }
    FMostrarDicas: boolean;
    FOnAlterarVersiculo: TOnAlterarVersiculoEvent;
    //FStrongMorfoComoChave: boolean;
    FVersiculoRef: TVersiculo;
    FSintagmas: TSintagmaList;
    FSelecao: TSintagmaList;
    FModificado: boolean;
    FXMLModificado: boolean;
    FPanel: TScrollBox;
    FEdit: TEdit;
    FAtivo: boolean;
    FDestruindo: boolean;
    FExibirErro: boolean;
    //FFontePadrao: TFont;
    FTmpCor: TColor;
    FTmpSobrescrito: boolean;
    FTmpItalico: boolean;
    FCorAssociado: TColor;
    FCorDesassociado: TColor;
    FOnSintagmaClick: TOnSintagmaEvent;
    FOnSintagmaMouseEnter: TOnSintagmaEvent;
    FOnSintagmaMouseLeave: TOnSintagmaEvent;
    FXML: string;
    //FOnNovaAssociacao: TOnAssociacaoEvent;
    //FOnRemoverAssociacao: TOnAssociacaoEvent;
    function GetAndamentoAssociacao: Single;
    function GetAtivo: boolean;
    function GetFonte: TFont;
    function GetPares: string;
    function GetTextoSimples: string;
    procedure SetAtivo(const AValue: boolean);
    procedure SetFonte(const AValue: TFont);
    procedure SetModificado(const AValue: boolean);
    procedure SetPares(const AValue: string);
    procedure SetTexto(_XML: string);
    function GetTexto: string;
    procedure SetVersiculoPar(Par: TVersiculo);
    procedure SelecionarListaSintagmas(lst: string);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
    procedure EditConfirm;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Criar(TheOwner: TScrollBox);
    destructor Destruir;
    procedure LimparSintagmas;
    procedure AssociarSintagmas;
    procedure LimparSelecao;
    procedure DesassociarPares;
    procedure LimparAssociacoes;
    procedure OrganizarSintagmas;
    procedure SelecionarSintagmas(s: TSintagmaList);
    function GetListaPares(tipo: TTipoListaPares): TStringList;

    function GetLinhaInterlinear: string;
    function GetLinhaONT: string;
    function GetLinhaONT(morfo: boolean; autoitalico: boolean; strongsreutilizados: boolean;
          strongsnaotraduzidos: boolean): string;

    property VersiculoPar: TVersiculo read FVersiculoRef write SetVersiculoPar;
    property XML: string read FXML;
    property Texto: string read GetTexto write SetTexto;
    property Pares: string read GetPares write SetPares;
    property Painel: TScrollBox read FPanel write FPanel;
    property Sintagmas: TSintagmaList read FSintagmas;
    property Selecao: TSintagmaList read FSelecao write FSelecao;
    property Modificado: boolean read FModificado write SetModificado;
    property XMLModificado: boolean read FXMLModificado;
    property OnClick: TOnSintagmaEvent read FOnSintagmaClick write FOnSintagmaClick;
    property OnMouseEnter: TOnSintagmaEvent read FOnSintagmaMouseEnter write FOnSintagmaMouseEnter;
    property OnMouseLeave: TOnSintagmaEvent read FOnSintagmaMouseLeave write FOnSintagmaMouseLeave;
    property OnAlterarVersiculo: TOnAlterarVersiculoEvent read FOnAlterarVersiculo write FOnAlterarVersiculo;
    //property OnNovaAssociacao: TOnAssociacaoEvent read FOnNovaAssociacao write FOnNovaAssociacao;
    //property OnRemoverAssociacao: TOnAssociacaoEvent read FOnRemoverAssociacao write FOnRemoverAssociacao;
    property MostrarDicas: boolean read FMostrarDicas write FMostrarDicas;
    property Ativo: boolean read GetAtivo write SetAtivo;
    property TextoSimples: string read GetTextoSimples;
    //property LinhaONT: string read GetLinhaONT;
    //property LinhaInterlinear: string read GetLinhaInterlinear;
    property CorAssociado: TColor read FCorAssociado write FCorAssociado;
    property CorDesassociado: TColor read FCorDesassociado write FCorDesassociado;
    //property StrongMorfoComoChave: boolean read FStrongMorfoComoChave write FStrongMorfoComoChave;
    property AndamentoAssociacao: Single read GetAndamentoAssociacao;
    property Fonte: TFont read GetFonte write SetFonte;
 published
    { Published declarations }
  end;

implementation

type
  RColor = record
    Name: String;
    Value: Integer;
  end;

const
  // the following list shows 140 basic html color names and their corresponding
  // HTML hex-values
  ColorTable: array[0..140] of RColor = (
    (Name: 'aliceblue';            Value: $F0F8FF),
    (Name: 'antiquewhite';         Value: $FAEBD7),
    (Name: 'aqua';                 Value: $00FFFF),
    (Name: 'aquamarine';           Value: $7FFFD4),
    (Name: 'azure';                Value: $F0FFFF),
    (Name: 'beige';                Value: $F5F5DC),
    (Name: 'bisque';               Value: $FFE4C4),
    (Name: 'black';                Value: $000000),
    (Name: 'blanchedalmond';       Value: $FFFFCD),
    (Name: 'blue';                 Value: $0000FF),
    (Name: 'blueviolet';           Value: $8A2BE2),
    (Name: 'brown';                Value: $A52A2A),
    (Name: 'burlywood';            Value: $DEB887),
    (Name: 'cadetblue';            Value: $5F9EA0),
    (Name: 'chartreuse';           Value: $7FFF00),
    (Name: 'chocolate';            Value: $D2691E),
    (Name: 'coral';                Value: $FF7F50),
    (Name: 'cornflowerblue';       Value: $6495ED),
    (Name: 'cornsilk';             Value: $FFF8DC),
    (Name: 'crimson';              Value: $DC143C),
    (Name: 'cyan';                 Value: $00FFFF),
    (Name: 'darkblue';             Value: $00008B),
    (Name: 'darkcyan';             Value: $008B8B),
    (Name: 'darkgoldenrod';        Value: $B8860B),
    (Name: 'darkgray';             Value: $A9A9A9),
    (Name: 'darkgreen';            Value: $006400),
    (Name: 'darkkhaki';            Value: $BDB76B),
    (Name: 'darkmagenta';          Value: $8B008B),
    (Name: 'darkolivegreen';       Value: $556B2F),
    (Name: 'darkorange';           Value: $FF8C00),
    (Name: 'darkorchid';           Value: $9932CC),
    (Name: 'darkred';              Value: $8B0000),
    (Name: 'darksalmon';           Value: $E9967A),
    (Name: 'darkseagreen';         Value: $8FBC8F),
    (Name: 'darkslateblue';        Value: $483D8B),
    (Name: 'darkslategray';        Value: $2F4F4F),
    (Name: 'darkturquoise';        Value: $00CED1),
    (Name: 'darkviolet';           Value: $9400D3),
    (Name: 'deeppink';             Value: $FF1493),
    (Name: 'deepskyblue';          Value: $00BFFF),
    (Name: 'dimgray';              Value: $696969),
    (Name: 'dodgerblue';           Value: $1E90FF),
    (Name: 'firebrick';            Value: $B22222),
    (Name: 'floralwhite';          Value: $FFFAF0),
    (Name: 'forestgreen';          Value: $228B22),
    (Name: 'fuchsia';              Value: $FF00FF),
    (Name: 'gainsboro';            Value: $DCDCDC),
    (Name: 'ghostwhite';           Value: $F8F8FF),
    (Name: 'gold';                 Value: $FFD700),
    (Name: 'goldenrod';            Value: $DAA520),
    (Name: 'gray';                 Value: $808080),
    (Name: 'green';                Value: $008000),
    (Name: 'greenyellow';          Value: $ADFF2F),
    (Name: 'honeydew';             Value: $F0FFF0),
    (Name: 'hotpink';              Value: $FF69B4),
    (Name: 'indianred';            Value: $CD5C5C),
    (Name: 'indigo';               Value: $4B0082),
    (Name: 'ivory';                Value: $FFF0F0),
    (Name: 'khaki';                Value: $F0E68C),
    (Name: 'lavender';             Value: $E6E6FA),
    (Name: 'lavenderblush';        Value: $FFF0F5),
    (Name: 'lawngreen';            Value: $7CFC00),
    (Name: 'lemonchiffon';         Value: $FFFACD),
    (Name: 'lightblue';            Value: $ADD8E6),
    (Name: 'lightcoral';           Value: $F08080),
    (Name: 'lightcyan';            Value: $E0FFFF),
    (Name: 'lightgoldenrodyellow'; Value: $FAFAD2),
    (Name: 'lightgreen';           Value: $90EE90),
    (Name: 'lightgrey';            Value: $D3D3D3),
    (Name: 'lightpink';            Value: $FFB6C1),
    (Name: 'lightsalmon';          Value: $FFA07A),
    (Name: 'lightseagreen';        Value: $20B2AA),
    (Name: 'lightskyblue';         Value: $87CEFA),
    (Name: 'lightslategray';       Value: $778899),
    (Name: 'lightsteelblue';       Value: $B0C4DE),
    (Name: 'lightyellow';          Value: $FFFFE0),
    (Name: 'lime';                 Value: $00FF00),
    (Name: 'limegreen';            Value: $32CD32),
    (Name: 'linen';                Value: $FAF0E6),
    (Name: 'magenta';              Value: $FF00FF),
    (Name: 'maroon';               Value: $800000),
    (Name: 'mediumaquamarine';     Value: $66CDAA),
    (Name: 'mediumblue';           Value: $0000CD),
    (Name: 'mediumorchid';         Value: $BA55D3),
    (Name: 'mediumpurple';         Value: $9370DB),
    (Name: 'mediumseagreen';       Value: $3CB371),
    (Name: 'mediumpurple';         Value: $9370DB),
    (Name: 'mediumslateblue';      Value: $7B68EE),
    (Name: 'mediumspringgreen';    Value: $00FA9A),
    (Name: 'mediumturquoise';      Value: $48D1CC),
    (Name: 'mediumvioletred';      Value: $C71585),
    (Name: 'midnightblue';         Value: $191970),
    (Name: 'mintcream';            Value: $F5FFFA),
    (Name: 'mistyrose';            Value: $FFE4E1),
    (Name: 'moccasin';             Value: $FFE4B5),
    (Name: 'navajowhite';          Value: $FFDEAD),
    (Name: 'navy';                 Value: $000080),
    (Name: 'oldlace';              Value: $FDF5E6),
    (Name: 'olive';                Value: $808000),
    (Name: 'olivedrab';            Value: $6B8E23),
    (Name: 'orange';               Value: $FFA500),
    (Name: 'orangered';            Value: $FF4500),
    (Name: 'orchid';               Value: $DA70D6),
    (Name: 'palegoldenrod';        Value: $EEE8AA),
    (Name: 'palegreen';            Value: $98FB98),
    (Name: 'paleturquoise';        Value: $AFEEEE),
    (Name: 'palevioletred';        Value: $DB7093),
    (Name: 'papayawhip';           Value: $FFEFD5),
    (Name: 'peachpuff';            Value: $FFDBBD),
    (Name: 'peru';                 Value: $CD853F),
    (Name: 'pink';                 Value: $FFC0CB),
    (Name: 'plum';                 Value: $DDA0DD),
    (Name: 'powderblue';           Value: $B0E0E6),
    (Name: 'purple';               Value: $800080),
    (Name: 'red';                  Value: $FF0000),
    (Name: 'rosybrown';            Value: $BC8F8F),
    (Name: 'royalblue';            Value: $4169E1),
    (Name: 'saddlebrown';          Value: $8B4513),
    (Name: 'salmon';               Value: $FA8072),
    (Name: 'sandybrown';           Value: $F4A460),
    (Name: 'seagreen';             Value: $2E8B57),
    (Name: 'seashell';             Value: $FFF5EE),
    (Name: 'sienna';               Value: $A0522D),
    (Name: 'silver';               Value: $C0C0C0),
    (Name: 'skyblue';              Value: $87CEEB),
    (Name: 'slateblue';            Value: $6A5ACD),
    (Name: 'slategray';            Value: $708090),
    (Name: 'snow';                 Value: $FFFAFA),
    (Name: 'springgreen';          Value: $00FF7F),
    (Name: 'steelblue';            Value: $4682B4),
    (Name: 'tan';                  Value: $D2B48C),
    (Name: 'teal';                 Value: $008080),
    (Name: 'thistle';              Value: $D8BFD8),
    (Name: 'tomato';               Value: $FD6347),
    (Name: 'turquoise';            Value: $40E0D0),
    (Name: 'violet';               Value: $EE82EE),
    (Name: 'wheat';                Value: $F5DEB3),
    (Name: 'white';                Value: $FFFFFF),
    (Name: 'whitesmoke';           Value: $F5F5F5),
    (Name: 'yellow';               Value: $FFFF00),
    (Name: 'yellowgreen';          Value: $9ACD32)
  );

var Hint: THintWindow;

function TranslateColorName(Name: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(ColorTable) to High(ColorTable) do
    // find matching color name
    if ColorTable[I].Name = Name then
    begin
      // return RGB colors
      Result :=
        Byte(ColorTable[I].Value shr 16) +
        Byte(ColorTable[I].Value shr 8) shl 8 +
        Byte(ColorTable[I].Value) shl 16;
      Break;
    end;
end;

function HTML2Color(const HTML: String): Integer;
var
  Offset: Integer;
begin
  try
    // check for leading '#'
    if Copy(HTML, 1, 1) = '#' then
      Offset := 1
    else
      Offset := 0;
    // convert hexa-decimal values to RGB
    Result :=
      Integer(StrToInt('$' + Copy(HTML, Offset + 1, 2))) +
      Integer(StrToInt('$' + Copy(HTML, Offset + 3, 2))) shl 8 +
      Integer(StrToInt('$' + Copy(HTML, Offset + 5, 2))) shl 16;
  except
    // try for color names
    Result := TranslateColorName(LowerCase(HTML));
  end;
end;

{ TVersiculo }

constructor TVersiculo.Criar(TheOwner : TScrollBox);
begin
  FPanel           := TheOwner;
  FPanel.Color     := clWindow;
  FPanel.Caption   := '';

  FEdit            := TEdit.Create(FPanel);
  FEdit.Visible    := false;
  FEdit.AutoSize   := false;
  FEdit.OnKeyDown  := @EditKeyDown;
  FEdit.OnExit     := @EditExit;
  FPanel.InsertControl(FEdit);

  FVersiculoRef         := nil;
  FOnSintagmaClick      := nil;
  FOnSintagmaMouseEnter := nil;
  FOnSintagmaMouseLeave := nil;
  FModificado           := false;
  FXMLModificado        := false;
  FSintagmas            := TSintagmaList.Create;
  FSelecao              := TSintagmaList.Create;
  FAtivo                := true;
  FMostrarDicas         := false;
  //FFontePadrao          := TFont.Create;
  //FFontePadrao.Assign(TheOwner.Font);
  FCorAssociado         := clWindowText;
  FCorDesassociado      := clGrayText;
  FTmpCor               := clDefault;
  FTmpSobrescrito       := false;
  FTmpItalico           := false;
  //FStrongMorfoComoChave := false;
  FDestruindo           := false;
end;

destructor TVersiculo.Destruir;
var
  i: smallint;
begin
  FDestruindo := true;

  for i:=0 to FSintagmas.Count-1 do
    FSintagmas[i].Destruir;

  FSintagmas.free;
  FSelecao.free;
  FEdit.free;
  //FFontePadrao.Free;
end;

procedure TVersiculo.LimparSintagmas;
var
  i: smallint;
begin
  for i:=0 to FSintagmas.Count-1 do
    FSintagmas[i].Destruir;

  FSintagmas.Clear;
  FSelecao.Clear;
end;

procedure TVersiculo.AssociarSintagmas;
var
  i, c: smallint;
begin
  if not Assigned(VersiculoPar) then
    exit;

  c := 0;
  for i:=0 to Selecao.Count-1 do
  begin
    with Selecao[i] do
    begin
      Pares.Clear;
      Pares.AddList(VersiculoPar.Selecao);
      Correlacionado:=true;
      Irmaos.Clear;
      Irmaos.AddList(Selecao);
      Irmaos.Remove(Selecao[i]);
      inc(c);
    end;
  end;
  for i:=0 to VersiculoPar.Selecao.Count-1 do
  begin
    with TSintagma(VersiculoPar.Selecao[i]) do
    begin
      Pares.Clear;
      Pares.AddList(Selecao);
      Correlacionado:=true;
      Irmaos.Clear;
      Irmaos.AddList(VersiculoPar.Selecao);
      Irmaos.Remove(VersiculoPar.Selecao[i]);
      inc(c);
    end;
  end;

  //if c > 0 then
  //  Modificado := true;
  {
  if FOn <> nil then
  begin
    t := TStrings.Create;

    for i:=0 to Pares.Count-1 do
      t.Add(Pares[i].FLabel.Caption);

    FOnRemoverAssociacao(FLabel.Caption, t);
    t.Destroy;
  end;
  }
end;

procedure TVersiculo.LimparSelecao;
begin
  while Selecao.Count > 0 do
    TSintagma(Selecao.First).SelecaoMenos;
end;

procedure TVersiculo.DesassociarPares;
var
  i: smallint;
begin
  LimparSelecao;
  VersiculoPar.LimparSelecao;

  for i:=0 to FSintagmas.Count-1 do
    FSintagmas[i].Desassociar;

  for i:=0 to VersiculoPar.FSintagmas.Count-1 do
    VersiculoPar.FSintagmas[i].Desassociar;
end;

procedure TVersiculo.LimparAssociacoes;
var
  s: smallint;
begin
  for s:=0 to Sintagmas.Count-1 do
  begin
    if (Sintagmas[s].Pares.Count > 0) then
    begin // desassociar somente se houver associacoes, para evitar falsos 'modificado = true'
      DesassociarPares;
      Modificado := true;
      break;
    end;
  end;
end;

procedure TVersiculo.SelecionarSintagmas(s: TSintagmaList);
var
  i: smallint;
begin
  for i:=0 to s.Count-1 do
    s[i].SelecaoMais;
end;

function TVersiculo.GetListaPares(tipo: TTipoListaPares): TStringList;
var
  t: string;
  s, p: smallint;
  stg: TSintagma;
  tmp: TSintagmaList;
begin
  result := TStringList.Create;

  tmp := TSintagmaList.Create;
  for s:=0 to Sintagmas.Count-1 do
  begin
    stg := Sintagmas[s];
    if (stg.Pares.Count > 0) and (tmp.IndexOf(stg) < 0) then
    begin
      t := stg.GetChaveSugestao(tipo);
      tmp.Add(stg);
      for p:=0 to stg.Irmaos.Count-1 do
      begin
        t := t + ';' + stg.Irmaos[p].GetChaveSugestao(tipo);
        tmp.Add(stg.Irmaos[p]);
      end;
      result.Add(t);
      t := '';
      for p:=0 to stg.Pares.Count-1 do
      begin
        //par := stg.Pares[p];
        t := t + stg.Pares[p].GetChaveSugestao(tipo);
        if p <> stg.Pares.Count-1 then
          t := t + ';';
      end;
      if t = '' then // par sem associacao! Nao deveria acontecer!
        result.Delete(result.Count-1)
      else
        result.Add(t);
    end;
  end;
  tmp.Destroy;
end;

procedure TVersiculo.SetTexto(_XML: string);
var
  varredorXML: TVarredorXML;
  s: TTagSintagma;
  sintagma: TSintagma;
  //valor : string;
begin
  sintagma := nil;
  FXML := _XML;
  LimparSintagmas;
  varredorXML := TVarredorXML.Criar(_XML);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if s.tipo = tsTag then
    begin
      if s.valor = '<RF>' then
      begin
        varredorXML.LerAteTag(s, '<Rf>');
        s.tipo := tsMetaDado;
      end else if AnsiStartsStr('<TS', s.valor) then
      begin
        varredorXML.LerAteTag(s, '<Ts>');
        s.tipo := tsMetaDado;
      end else if AnsiStartsStr('<WG', s.valor) and assigned(sintagma) and assigned(sintagma.FStrong) then // atualizar sintagma anterior
      begin
        sintagma.FStrong.Add(copy(s.valor, 3, length(s.valor)-3));
        sintagma.FTextoCru := sintagma.FTextoCru + s.valor;
        continue;
      end else if AnsiStartsStr('<WT', s.valor) and assigned(sintagma) and assigned(sintagma.FMorf) then // atualizar sintagma anterior
      begin
        sintagma.FMorf.Add(copy(s.valor, 4, length(s.valor)-4));
        sintagma.FTextoCru := sintagma.FTextoCru + s.valor;
        continue;
      end else if AnsiStartsStr('<font color', s.valor) then
      begin
        FTmpCor := HTML2Color(varredorXML.LerPropriedadeTag('color', s));
        //continue;
      end else if s.valor = '</font>' then
      begin
        FTmpCor := clDefault;
        //continue;
      end else if s.valor = '<sup>' then
      begin
        FTmpSobrescrito := true;
        //continue;
      end else if s.valor = '</sup>' then
      begin
        FTmpSobrescrito := false;
        //continue;
      end else if (s.valor = '<FI>') or (s.valor = '<Fi>') then
      begin
        FTmpItalico := s.valor[3] = 'I';
        // nada a fazer. Para <FI> e <Fi>, queremos criar o label
      end else
        s.tipo := tsMetaDado;
    end else if (s.tipo = tsEspaco) and (s.valor = '|') then
    begin
      if assigned(sintagma) then
        sintagma.FTextoCru := sintagma.FTextoCru + s.valor;
      continue;
    end;
    sintagma := TSintagma.Criar(self, s);
    FSintagmas.Add(sintagma);
  end;
  varredorXML.Destruir;
  { !! associar sintagmas conforme armazenado !! }
  Modificado := false;
  FXMLModificado := false;
  OrganizarSintagmas;
end;

function TVersiculo.GetTexto: string;
var
  linha: TStringStream;
  s: smallint;
begin
  result := '';
  try
    linha := TStringStream.Create('');
    for s:=0 to Sintagmas.Count-1 do
      linha.WriteString(Sintagmas[s].Texto);
  finally
    result := linha.DataString;
    linha.Destroy;
  end;
end;

procedure TVersiculo.SetPares(const AValue: string);
var
  varredorXML: TVarredorXML;
  s: TTagSintagma;
begin
  if not (assigned(VersiculoPar)) or (length(AValue) = 0) then
    exit;

  FExibirErro := true;
  VersiculoPar.FExibirErro := true;

  varredorXML := TVarredorXML.Criar(AValue);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if AnsiStartsStr('<par ', s.valor) then
    begin
      SelecionarListaSintagmas(varredorXML.LerPropriedadeTag('a', s));
      VersiculoPar.SelecionarListaSintagmas(varredorXML.LerPropriedadeTag('b', s));
      AssociarSintagmas;

      LimparSelecao;
      VersiculoPar.LimparSelecao;

      OrganizarSintagmas;
      VersiculoPar.OrganizarSintagmas;
    end;
  end;
  varredorXML.Destruir;
end;

function TVersiculo.GetPares: string;
var
  s, p: smallint;
  stg, par: TSintagma;
  _xml: TStringStream;
  tmp: TSintagmaList;
begin
  _xml := TStringStream.Create('');

  // gerar tags de Texto <par a="1,2,3" b="1,2,3">
  tmp := TSintagmaList.Create;
  for s:=0 to Sintagmas.Count-1 do
  begin
    stg := Sintagmas[s];
    if (stg.Pares.Count > 0) and (tmp.IndexOf(stg) < 0) then
    begin
      _xml.WriteString(Format('<par a="%d', [s]));
      tmp.Add(stg);
      for p:=0 to stg.Irmaos.Count-1 do
      begin
        _xml.WriteString(Format(',%d', [stg.VersiculoRef.Sintagmas.IndexOf(stg.Irmaos[p])]));
        tmp.Add(stg.Irmaos[p]);
      end;
      _xml.WriteString('" b="');
      for p:=0 to stg.Pares.Count-1 do
      begin
        par := stg.Pares[p];
        _xml.WriteString(Format('%d', [par.VersiculoRef.Sintagmas.IndexOf(par)]));
        if p <> stg.Pares.Count-1 then
          _xml.WriteString(',');
      end;
      _xml.WriteString('">');
    end;
  end;
  tmp.Destroy;

  result := _xml.DataString;
  _xml.Destroy;
end;

function TVersiculo.GetTextoSimples: string;
var
  linha: TStringStream;
  s: smallint;
begin
  result := '';
  try
    linha := TStringStream.Create('');
    for s:=0 to Sintagmas.Count-1 do
      if Sintagmas[s].Tipo in [tsEspaco, tsSintagma, tsPontuacao] then
        linha.WriteString(Sintagmas[s].Texto);
  finally
    result := linha.DataString;
    linha.Destroy;
  end;
end;

function TVersiculo.GetAtivo: boolean;
begin
  result := FAtivo;
end;

function TVersiculo.GetFonte: TFont;
begin
  result := FPanel.Font;
end;

function TVersiculo.GetAndamentoAssociacao: Single;
var
  t, p, s: smallint;
begin
  t := 0;
  p := 0;
  result := 0;

  for s:=0 to Sintagmas.Count-1 do
    if Sintagmas[s].Tipo = tsSintagma then
    begin
      inc(t);
      if Sintagmas[s].Correlacionado then
        inc(p);
    end;

  if t > 0 then
    result := p/t;
end;

function TVersiculo.GetLinhaInterlinear: string;
var
  linha: TStringStream;
  stg, prox: TSintagma;
  s, m, p: smallint;
begin
  result := '';

  try
    linha := TStringStream.Create('');

    for s:=0 to Sintagmas.Count-1 do
    begin
      stg := Sintagmas[s];
      linha.WriteString(stg.Texto);
      if (assigned(stg.Strong) and (stg.Strong.Count > 0)) or (assigned(stg.Morf) and (stg.Morf.Count > 0)) then
      begin // se este texto contém strongs
        for m:=0 to stg.Strong.Count-1 do // strongs
          linha.WriteString(format('<W%s>', [stg.Strong.Strings[m]]));
        for m:=0 to stg.Morf.Count-1 do // morfologia
          linha.WriteString(format('<WT%s>', [stg.Morf.Strings[m]]));
      end else
      begin // caso contrário, tentemos o texto relacionado
        for p:=0 to stg.Pares.Count-1 do
        begin
          for m:=0 to stg.Pares[p].Strong.Count-1 do // strongs
            linha.WriteString(format('<W%s>', [stg.Pares[p].Strong.Strings[m]]));
          for m:=0 to stg.Pares[p].Morf.Count-1 do   // morfologia
            linha.WriteString(format('<WT%s>', [stg.Pares[p].Morf.Strings[m]]));
        end;
      end;

      if (s < (Sintagmas.Count-1)) and // não é o último sintagma e
         (stg.Irmaos.Count > 0) then   // tem irmãos
      begin
        prox := nil;
        for m:=s+1 to Sintagmas.Count-1 do
        begin // procurando o próximo sintagma (saltando espaços, pontuação, etc.)
          if Sintagmas[m].Tipo = tsSintagma then
          begin
            prox := Sintagmas[m];
            break;
          end;
        end;
        if (prox <> nil) and (stg.Irmaos.IndexOf(prox) >= 0) then // o próximo sintagma é irmão deste?
          continue;
      end;

      for p:=0 to stg.Pares.Count-1 do
      begin // pares
        linha.WriteString('<sup>' + stg.Pares[p].Texto + '</sup> ');
      end;
    end;

  finally
    result := AnsiReplaceStr(
                AnsiReplaceStr(
                  AnsiReplaceStr(
                    AnsiReplaceStr(
                      linha.DataString, '</sup> <sup>', ' '
                    ), '  ', ' '
                  ), '<sup>', '<font size=+1 color="CC3300"><sup>'
                ), '</sup>', '</font></sup>'
              );
    linha.Destroy;
  end;
end;

function TVersiculo.GetLinhaONT: string;
begin
  result := GetLinhaONT(true, false, false, false);
end;

function TVersiculo.GetLinhaONT(morfo: boolean; autoitalico: boolean; strongsreutilizados: boolean;
          strongsnaotraduzidos: boolean
 ): string;
var
  linha: TStringStream;
  stg, prox: TSintagma;
  s, p, m: smallint;
  ls: TList;
begin
  result := '';
  ls := TList.Create; // usado para armazenar os Strongs utilizados no versículo
  prox := nil;
  try
    linha := TStringStream.Create('');

    for s:=0 to Sintagmas.Count-1 do
    begin // sintagmas
      stg := Sintagmas[s];

      if (prox = nil) and (stg.Pares.Count > 0) then
        linha.WriteString('<wt>');

      if autoitalico and (stg.Pares.Count = 0) and (stg.Tipo = tsSintagma) then
        linha.WriteString('<FI>');

      if autoitalico and (stg.Tipo = tsTag) and ((stg.Texto = '<FI>') or (stg.Texto = '<Fi>')) then
        continue; // quando auto-itálico, ignorar os itálicos originais

      linha.WriteString(stg.Texto);

      if autoitalico and (stg.Pares.Count = 0) and (stg.Tipo = tsSintagma) then
         linha.WriteString('<Fi>');

      if stg.Pares.Count > 0 then
      begin
        if (s < (Sintagmas.Count-1)) and // não é o último sintagma e
           (stg.Irmaos.Count > 0) then   // tem irmãos
        begin
          prox := nil;
          for m:=s+1 to Sintagmas.Count-1 do
          begin // procurando o próximo sintagma (saltando espaços, pontuação, etc.)
            if Sintagmas[m].Tipo = tsSintagma then
            begin
              prox := Sintagmas[m];
              break;
            end;
          end;
          if (prox <> nil) and (stg.Irmaos.IndexOf(prox) >= 0) then // o próximo sintagma é irmão deste?
            continue;
        end;
        prox := nil;
        for p:=0 to stg.Pares.Count-1 do
        begin // pares
          for m:=0 to stg.Pares[p].Strong.Count-1 do // strongs
          begin
            if not strongsreutilizados or (ls.IndexOf(stg.Pares[p].Strong) = -1) then // este Strong já foi utilizado antes?
            begin // não
              linha.WriteString(format('<W%s>', [stg.Pares[p].Strong.Strings[m]]));
              ls.Add(stg.Pares[p].Strong)
            end else // strong reutilizado
              linha.WriteString(format('<W%ss>', [stg.Pares[p].Strong.Strings[m]]));
          end;

          if not morfo then continue;

          for m:=0 to stg.Pares[p].Morf.Count-1 do   // morfologia
            linha.WriteString(format('<WT%s>', [stg.Pares[p].Morf.Strings[m]]));
        end;
      end;
    end;

    if strongsnaotraduzidos then
    begin
      // verificando se ficaram palavras do original que não foram traduzidas
      for s:=0 to VersiculoPar.Sintagmas.Count-1 do
      begin // sintagmas
        stg := VersiculoPar.Sintagmas[s];
        if assigned(stg.Strong) and (ls.IndexOf(stg.Strong) = -1) then
          for m:=0 to stg.Strong.Count-1 do // strongs
            linha.WriteString(format('<W%sx>', [stg.Strong.Strings[m]]));
      end;
    end;
  finally
    result := linha.DataString;
    linha.Destroy;
    ls.Destroy;
  end;
end;

procedure TVersiculo.SetAtivo(const AValue: boolean);
begin
  if (AValue <> FAtivo) then
  begin
    FAtivo := AValue;
    if (AValue) then
    begin // ativar

    end
    else
    begin // desativar

    end;
  end;
end;

procedure TVersiculo.SetFonte(const AValue: TFont);
var
  i: smallint;
begin
  FPanel.Font := AValue;
  for i:=0 to FSintagmas.Count-1 do
  begin
    with FSintagmas[i] do
    begin
      if not assigned(FLabel) then
         continue;

      LabelRef.ParentFont := true;
      if FSobrescrito then
         LabelRef.Font.Size := round(LabelRef.Font.Size * 0.7);
      Correlacionado := Correlacionado; // remarcando sintagmas correlacionados
    end;
  end;
  OrganizarSintagmas;
end;

procedure TVersiculo.SetModificado(const AValue: boolean);
begin
  if FModificado = AValue then exit;
  FModificado := AValue;
  if assigned(FOnAlterarVersiculo) and FModificado then
     FOnAlterarVersiculo;
end;

procedure TVersiculo.OrganizarSintagmas;
var
  s: TSintagma;
  x, y, i, a: smallint;
begin
  if not Ativo or FDestruindo then
    exit;

  x := 5;
  y := 0;
  a := 0;
  for i:=0 to FSintagmas.Count-1 do
  begin
    s := FSintagmas[i];

    if not assigned(s.LabelRef) then //s.Tipo in [tsMetaDado, tsTag] then
      continue;

    a := max(a, s.LabelRef.Height);
    if (x > 0) and (s.Tipo <> tsPontuacao) and ((x + s.LabelRef.Width) > (Painel.Width-15)) then
    begin
      inc(y, a);
      a := 0;
      x := 5;
    end;

    s.LabelRef.Left:=x;
    s.LabelRef.Top:=y;

    if (x = 5) and (s.Tipo = tsEspaco) then
      s.LabelRef.Visible := false // ocultemos o espaço do início da linha
    else begin
      s.LabelRef.Show;
      inc(x, s.LabelRef.Width);
    end;
  end;
end;

procedure TVersiculo.SetVersiculoPar(Par: TVersiculo);
begin
  FVersiculoRef := Par;
  if (Par.VersiculoPar <> self) then
    Par.VersiculoPar := self;
end;

procedure TVersiculo.SelecionarListaSintagmas(lst: string);
  function indiceTmp(s: TSintagmaList; i: smallint): smallint;
  var
    j, h: smallint;
    //t: string;
  begin
    j := 0;
    h := 0;
    while (h < i) do
    begin
      //t := s[j].Texto;
      if AnsiContainsStr(s[j].Texto, '-') then
      begin
        if (h+2) < i then
          inc(h, 2)
        else
          h := i;
        //if h = i then
        //begin
        //  dec(h);
        //  break;
        //end;
      end
      else
        inc(h);

      inc(j);
    end;

    result := j;
  end;

var
  ini, i, s: smallint;
begin
  ini := 1;
  i   := 1;
  while i <= length(lst) do
  begin
    if lst[i] = ',' then
    begin
      s := StrToInt(copy(lst, ini, i-ini));
      if (s >= Sintagmas.Count) or (Sintagmas[s].Tipo <> tsSintagma) then
      begin
        if FExibirErro then
           MessageDlg('Erro', 'Dados inconsistentes, algumas associações serão perdidas.'#13#10 +
                              'Isso pode ocorrer por várias razões:'#13#10 +
                              ' 1. O texto origem e/ou destino foi editado fora do iBiblia'#13#10 +
                              ' 2. Você carregou um novo texto origem/destino'#13#10 +
                              ' 3. O projeto foi criado/editado numa versão diferente do iBiblia'#13#10 +
                              ' 4. Pode ser um bug no iBiblia, por favor, relatar no github.'#13#10#13#10 +
                              TextoSimples + #13#10#13#10 +
                              VersiculoPar.TextoSimples, mtError, [mbOK], 0);
        FExibirErro := false;
        VersiculoPar.FExibirErro:=false;
      end
      else
        Sintagmas[s].SelecaoMais;
      //Sintagmas[indiceTmp(Sintagmas, StrToInt(copy(lst, ini, i-ini)))].SelecaoMais;
      ini := i+1;
    end;
    inc(i);
  end;
  //Sintagmas[indiceTmp(Sintagmas, StrToInt(copy(lst, ini, i-ini)))].SelecaoMais;
  s := StrToInt(copy(lst, ini, i-ini));
  if (s >= Sintagmas.Count) or (Sintagmas[s].Tipo <> tsSintagma) then
  begin
    if FExibirErro then
       MessageDlg('Erro', 'Dados inconsistentes, algumas associações serão perdidas.'#13#10 +
                          'Isso pode ocorrer por várias razões:'#13#10 +
                          ' 1. O texto origem e/ou destino foi editado fora do iBiblia'#13#10 +
                          ' 2. Você carregou um novo texto origem/destino'#13#10 +
                          ' 3. O projeto foi criado/editado numa versão diferente do iBiblia'#13#10 +
                          ' 4. Pode ser um bug no iBiblia, por favor, relatar no github.'#13#10#13#10 +
                          TextoSimples + #13#10#13#10 +
                          VersiculoPar.TextoSimples, mtError, [mbOK], 0);
    FExibirErro := false;
    VersiculoPar.FExibirErro:=false;
  end
  else
    Sintagmas[s].SelecaoMais;
end;

procedure TVersiculo.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    FEdit.Visible := False
  else if Key = VK_RETURN then
    EditConfirm;
end;

procedure TVersiculo.EditExit(Sender: TObject);
begin
  FEdit.Visible := False;
end;

procedure TVersiculo.EditConfirm;
var
  s: TTagSintagma;
  varredorXML: TVarredorXML;
  sintagma, novo: TSintagma;
  i, offset, j: integer;
begin
  sintagma := TSintagma(FEdit.Tag);
  i := FSintagmas.IndexOf(sintagma);
  FSintagmas.Remove(sintagma);
  FSelecao.Remove(sintagma);
  offset := 0;
  novo := nil;

  { varredura simplificada do texto editado do sintagma para quebrar as palavras - suporte a tags incompleto }
  varredorXML := TVarredorXML.Criar(FEdit.Caption);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if (s.tipo = tsEspaco) and (s.valor = '|') then
    begin
      if assigned(novo) then
        novo.FTextoCru := novo.FTextoCru + s.valor;
      continue;
    end;

    novo := TSintagma.Criar(self, s);
    FSintagmas.Insert(i, novo);
    Inc(i); Inc(Offset);
  end;
  varredorXML.Destruir;

  { copiando dados do sintagma clicado para o primeiro sintagma parseado }
  for j:=(i-Offset) to i+Offset do
  begin
    novo := FSintagmas[j];
    if novo.Tipo = tsSintagma then
    begin
      for i:=0 to sintagma.Irmaos.Count-1 do
      begin
        novo.Irmaos.Add(sintagma.Irmaos[i]);
        sintagma.Irmaos[i].Irmaos.Remove(sintagma);
        sintagma.Irmaos[i].Irmaos.Add(novo);
      end;
      for i:=0 to sintagma.Pares.Count-1 do
      begin
        novo.SetCorrelacionado(true);
        novo.Pares.Add(sintagma.Pares[i]);
        sintagma.Pares[i].Pares.Remove(sintagma);
        sintagma.Pares[i].Pares.Add(novo);
      end;
      break;
    end;
  end;

  sintagma.Destruir;

  // atualizando o texto do versículo com o texto editado
  FXML := '';
  for j:=0 to FSintagmas.Count-1 do
    FXML := FXML + FSintagmas[j].FTextoCru;

  EditExit(Self);
  OrganizarSintagmas;
  FXMLModificado := true;
end;

{ TSintagma }

procedure TSintagma.DoOnLeftClick(Sender: TObject; Shift: TShiftState);
//var
//  d: string;
begin
  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VersiculoRef.Selecao.Count, VersiculoRef.VersiculoPar.Selecao.Count, Irmaos.Count, Pares.Count]);
  if (ssShift in Shift) and Assigned(VersiculoRef.VersiculoPar) then
  begin
    DesassociarPares;
    if (VersiculoRef.Selecao.Count > 0) then
      VersiculoRef.Selecao[0].DesassociarPares;
  end;
  if not (ssCtrl in Shift) then // Ctrl não pressionado
    VersiculoRef.LimparSelecao;

  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VersiculoRef.Selecao.Count, VersiculoRef.VersiculoPar.Selecao.Count, Irmaos.Count, Pares.Count]);
  InverterSelecao;
  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VersiculoRef.Selecao.Count, VersiculoRef.VersiculoPar.Selecao.Count, Irmaos.Count, Pares.Count]);

  if (ssShift in Shift) and (Assigned(VersiculoRef.VersiculoPar)) and
     (VersiculoRef.VersiculoPar.Selecao.Count > 0) and
     (VersiculoRef.Selecao.Count > 0) then // Shift pressionado
  begin
    VersiculoRef.AssociarSintagmas;
    VersiculoRef.Modificado:=true;
    VersiculoRef.VersiculoPar.Modificado:=true;
  end
  else if Assigned(VersiculoRef.VersiculoPar) then // Shift não pressionado
  begin
    VersiculoRef.VersiculoPar.LimparSelecao;
    if Pares.Count > 0 then
    begin
      //VersiculoRef.SelecionarSintagmas(Pares[0].Pares);
      VersiculoRef.SelecionarSintagmas(Irmaos);
      VersiculoRef.VersiculoPar.SelecionarSintagmas(Pares);
    end;
  end;

  VersiculoRef.OrganizarSintagmas;

  if assigned(VersiculoRef.FOnSintagmaClick) then
    VersiculoRef.FOnSintagmaClick(self);
end;

procedure TSintagma.DoOnDblClick(Sender: TObject);
begin
  if (FVersiculo.Ativo) then
  begin
    //LabelRef.Font.StrikeTrough := not LabelRef.Font.StrikeTrough;
    with FVersiculo.FEdit do
    begin
      Tag       := PtrInt(Self);
      Top       := FLabel.Top;
      Left      := FLabel.Left;
      Height    := FLabel.Height + 5;
      Width     := FLabel.Width + 15;
      Text      := FLabel.Caption;
      Visible   := true;
      SetFocus;
    end;
  end;
end;

procedure TSintagma.DoOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  VersiculoRef.FEdit.OnExit(Self);
  if Button = mbRight then
    DoOnRightClick(Sender, Shift)
  else if Button = mbLeft then
    DoOnLeftClick(Sender, Shift);
end;

procedure TSintagma.DoOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TSintagma.DoOnMouseEnter(Sender: TObject);
var
  rect: TRect;
  pos: TPoint;
  txt: string;
  i: smallint;
begin
  SetApontado(true);

  for i:=0 to Pares.Count-1 do
    Pares[i].SetApontado(true);
  for i:=0 to Irmaos.Count-1 do
    Irmaos[i].SetApontado(true);

  if assigned(VersiculoRef.FOnSintagmaMouseEnter) then
    VersiculoRef.FOnSintagmaMouseEnter(self);

  if not VersiculoRef.MostrarDicas then exit;

  if not assigned(Hint) then
    Hint := THintWindow.Create(Self.LabelRef);

  //if FStrong.Count > 0 then
  //begin
    txt := '';
    //if assigned(VersiculoRef.OnStrong) then
    //  txt := VersiculoRef.OnStrong(FStrong); //FStrong + #13#10 + FMorf;

    txt := 'Pares:';
    for i:=0 to Pares.Count-1 do
      txt := Concat(txt, #13#10, Pares[i].LabelRef.Caption);
    txt := Concat(txt, #13#10#13#10'Irmãos:');
    for i:=0 to Irmaos.Count-1 do
      txt := Concat(txt, #13#10, Irmaos[i].LabelRef.Caption);

    if length(txt) > 0 then
    begin
      Rect := Hint.CalcHintRect(0, txt, nil);  // no maxwidth
      Pos := Mouse.CursorPos;
      Rect.Left := Pos.X+10;
      Rect.Top := Pos.Y+5;
      Rect.Right := Rect.Left + Rect.Right;
      Rect.Bottom := Rect.Top + Rect.Bottom;

      Hint.ActivateHint(Rect, txt);
    end;
  //end;
end;

procedure TSintagma.DoOnMouseLeave(Sender: TObject);
var
  i: smallint;
begin
  SetApontado(False);
  for i:=0 to Pares.Count-1 do
    Pares[i].SetApontado(False);
  for i:=0 to Irmaos.Count-1 do
    Irmaos[i].SetApontado(False);

  if assigned(Hint) then
  begin
    Hint.Destroy;
    Hint := nil;
  end;

  if assigned(VersiculoRef.FOnSintagmaMouseLeave) then
    VersiculoRef.FOnSintagmaMouseLeave(self);
end;

procedure TSintagma.DoOnRightClick(Sender: TObject; Shift: TShiftState);
var
  UltimoSelecionado: TSintagma;
begin
  DesassociarPares;
  if (VersiculoRef.Selecao.Count > 0) then
    VersiculoRef.Selecao[0].DesassociarPares;

  if not (ssCtrl in Shift) then // Ctrl não pressionado
    VersiculoRef.LimparSelecao;

  InverterSelecao;

  if Assigned(VersiculoRef.VersiculoPar) and
     (VersiculoRef.VersiculoPar.Selecao.Count > 0) and
     (VersiculoRef.Selecao.Count > 0) then
  begin
    VersiculoRef.AssociarSintagmas;
    VersiculoRef.Modificado:=true;
    VersiculoRef.VersiculoPar.Modificado:=true;
  end;

  if not (ssCtrl in Shift) and (VersiculoRef.VersiculoPar.Selecao.Count > 0) then
  begin
    UltimoSelecionado := VersiculoRef.VersiculoPar.Selecao[VersiculoRef.VersiculoPar.Selecao.Count-1];
    VersiculoRef.LimparSelecao;
    VersiculoRef.VersiculoPar.LimparSelecao;
    if Assigned(UltimoSelecionado) and Assigned(UltimoSelecionado.GetProximo) then
      UltimoSelecionado.GetProximo.SelecaoMais;
  end;
end;

function TSintagma.GetChaveSugestao(t: TTipoListaPares): string;
var
  s: smallint;
begin
  result := '';

  if t in [tlTexto, tlTudo] then
    result := Texto;

  if t <> tlTexto then
  //if FVersiculo.StrongMorfoComoChave then
  begin
    //result := '';

    if assigned(Strong) and assigned(Morf) then
    begin
      for s:=0 to max(Strong.Count, Morf.Count) do
      begin
        if s < Strong.Count then
          result := format('%s<W%s>', [result, Strong.Strings[s]]);
        if (t <> tlStrong) and (s < Morf.Count) then
          result := format('%s<WT%s>', [result, Morf.Strings[s]]);
      end;

      {for s:=0 to Strong.Count-1 do // strongs
        result := format('%s<W%s>', [result, Strong.Strings[s]]);
      for s:=0 to Morf.Count-1 do // morfologia
        result := format('%s<WT%s>', [result, Morf.Strings[s]]);}

      if result = '' then
        result := Texto;
    end;
  end;
  //else
  //  result := Texto;

  result := UTF8LowerCase(result); //lowercase(result);
end;

function TSintagma.GetCorrelacionado: boolean;
begin
  result := FVersiculo.Ativo and (Pares.Count > 0){ (not FLabel.Font.Italic)};
end;

procedure TSintagma.SetApontado(const AValue: boolean);
begin
  if AValue then
  begin // apontado
    FLabel.Font.Underline := true;
    if not Selecionado then
      FLabel.Color:= clHighlight;//clInfoBk;
    //FLabel.Color:= clMoneyGreen; // clInfoBk;
    //FLabel.Font.Color := clRed;
  end
  else
  begin // não apontado
    FLabel.Font.Underline := false;
    if Selecionado then
      FLabel.Color:= clYellow
    else
      FLabel.Color:= clDefault;
    //Correlacionado := Correlacionado; // é isso mesmo que queremos
  end;
end;

procedure TSintagma.SetCorrelacionado(const AValue: boolean);
begin
  if (not FVersiculo.Ativo) then
    exit;

  if AValue then // correlacionado
  begin
    //FLabel.Font.Italic := false;
    //FLabel.Font.Color := FCor; //clWindowText;
    if FCor = clDefault then
      FLabel.Font.Color := FVersiculo.FCorAssociado //clGrayText
    else
      FLabel.Font.Color := FCor;
  end else // nao correlacionado
  begin
    //FLabel.Font.Italic := true;
    if FCor = clDefault then
      FLabel.Font.Color := FVersiculo.FCorDesassociado //clGrayText
    else
      FLabel.Font.Color := FCor;
  end;
end;

constructor TSintagma.Criar(TheOwner : TVersiculo; s : TTagSintagma);
begin
  FVersiculo   := TheOwner;
  FPares       := TSintagmaList.Create;
  FIrmaos      := TSintagmaList.Create;
  FTexto       := s.valor;
  FTextoCru    := s.valor;
  FTipo        := s.tipo;
  FCor         := FVersiculo.FTmpCor;
  FSobrescrito := FVersiculo.FTmpSobrescrito;
  FItalico     :=  FVersiculo.FTmpItalico;

  if (FVersiculo.Ativo) and
     (not (FTipo in [tsMetaDado, tsTag]) or (s.valor = '<FI>') or (s.valor = '<Fi>')) then
  begin
    FLabel := TLabel.Create(TheOwner.Painel);
    FLabel.Caption     := FTexto;
    if FTexto = '<FI>' then
      FLabel.Caption := '['
    else if FTexto = '<Fi>' then
      FLabel.Caption := ']';
    FLabel.ParentColor := false;
    FLabel.ParentFont  := true;
    FLabel.Parent      := TheOwner.Painel;
    FLabel.AutoSize    := true;
    if FSobrescrito then
    begin
      FLabel.Font.Size := round(FLabel.Font.Size * 0.7);
    end;
    //FLabel.Font        := TheOwner.Painel.Font;
    Correlacionado     := false;
    //FLabel.Font.Color  := clGrayText;
    //FLabel.Font.Italic := true;
  end else
    FLabel := nil;

  if s.tipo = tsSintagma then
  begin
    FStrong := TStringList.Create;
    FMorf   := TStringList.Create;
    if (FVersiculo.Ativo) then
    begin
      //FLabel.OnClick     := @DoOnClick;
      FLabel.OnDblClick  := @DoOnDblClick;
      FLabel.OnMouseDown := @DoOnMouseDown;
      FLabel.OnMouseUp   := @DoOnMouseUp;
      FLabel.OnMouseEnter:= @DoOnMouseEnter;
      FLabel.OnMouseLeave:= @DoOnMouseLeave;
    end;
  end else
  begin
    FStrong := nil;
    FMorf   := nil;
  end;
end;

destructor TSintagma.Destruir;
begin
  if assigned(FLabel) then
  begin
    FLabel.Destroy;
    FLabel := nil;
  end;
  FPares.Destroy;
  FPares := nil;
  FIrmaos.Destroy;
  FIrmaos := nil;
  if assigned(FStrong) then
  begin
     FStrong.Destroy;
     FStrong := nil;
  end;
  if assigned(FMorf) then
  begin
    FMorf.Destroy;
    FMorf := nil;
  end;
end;

procedure TSintagma.SelecaoMais;
begin
  FSelecionado := true;
  if FVersiculo.Ativo and assigned(FLabel) then
    FLabel.Color:= clYellow;
  VersiculoRef.Selecao.Add(Self);
end;

procedure TSintagma.SelecaoMenos;
begin
  FSelecionado := false;
  if FVersiculo.Ativo and assigned(FLabel) then
    FLabel.Color:= clWindow;
  VersiculoRef.Selecao.Remove(Self);
end;

procedure TSintagma.InverterSelecao;
begin
  if Selecionado then
    SelecaoMenos
  else
    SelecaoMais;
end;

procedure TSintagma.DesassociarPares;
var
  i: smallint;
  //t: TStringList;
begin
  {if VersiculoRef.FOnRemoverAssociacao <> nil then
  begin
    t := TStringList.Create;

    for i:=0 to Pares.Count-1 do
      t.Add(Pares[i].Texto);

    VersiculoRef.FOnRemoverAssociacao(Texto, t);
    t.Destroy;
  end;}

  for i:=0 to Irmaos.Count-1 do
    Irmaos[i].Desassociar;

  if Pares.Count > 0 then
  begin
    for i:=0 to Pares[0].Irmaos.Count-1 do
      Pares[0].Irmaos[i].Desassociar;

    Pares[0].Desassociar;
  end;

  Desassociar;
end;

function TSintagma.GetProximo: TSintagma;
var
  i: Integer;
begin
  result := nil;
  i := VersiculoRef.Sintagmas.IndexOf(Self);
  if (i >= 0) and (i < VersiculoRef.Sintagmas.Count) then
  begin
    for i := i+1 to VersiculoRef.Sintagmas.Count-1 do
    begin
      if VersiculoRef.Sintagmas[i].Tipo = tsSintagma then
      begin
        result := VersiculoRef.Sintagmas[i];
        break;
      end;
    end;
  end;
end;

procedure TSintagma.Desassociar;
begin
  if FTipo <> tsSintagma then
    exit;

  Correlacionado := false;
  Apontado := false;
  Irmaos.Clear;
  Pares.Clear;
end;

{ TSintagmaList }

function TSintagmaList.GetS(Index: Integer): TSintagma;
begin
  result := TSintagma(Get(Index));
end;

procedure TSintagmaList.PutS(Index: Integer; Item: TSintagma);
begin
  if IndexOf(Item) = -1 then  // evitando duplicatas
    Put(Index, Item);
end;

procedure TSintagmaList.AddList(AList: TSintagmaList);
var
  i: smallint;
begin
  for i:=0 to AList.Count-1 do
    if IndexOf(AList[i]) = -1 then
       Add(AList[i]);
end;

end.


