unit Sintagma;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, StdCtrls, Controls, Graphics, Forms, Math,
  LazUTF8;

type
  TSintagma = class;
  TSintagmaList = class;

  TTipoListaPares =
  (
    tlMetaDados, // somente metadados. Ex.: "<WG1><WTADJ-NSM>"
    tlStrong,    // somente strong#.   Ex.: "<WG1>"
    tlTexto,     // somente texto.     Ex.: "Jesus"
    tlTudo       // texto e metadados. Ex.: "Jesus<WG1088><WTNPI>"
  );

  { TSintagmaListEnumerator }

  TSintagmaListEnumerator = class(TListEnumerator)
  private
    function GetCurrentSintagma: TSintagma;
  public
    property Current: TSintagma read GetCurrentSintagma;
  end;

  { TSintagmaListReverseEnumerator }

  TSintagmaListReverseEnumerator = class
  private
    FList: TSintagmaList;
    FPosition: integer;
    function GetCurrent: TSintagma;
  public
    constructor Create(AList: TSintagmaList);
    function MoveNext: Boolean;
    property Current: TSintagma read GetCurrent;
    function GetEnumerator: TSintagmaListReverseEnumerator;
  end;

  { TSintagmaList }

  TSintagmaList = class(TList)
  private
    function GetEmpty: boolean;
  protected
    function GetS(Index: Integer): TSintagma;
    procedure PutS(Index: Integer; Item: TSintagma);
  public
    function GetEnumerator: TSintagmaListEnumerator;
    function GetReverseEnumerator: TSintagmaListReverseEnumerator;
    property Itens[Index: Integer]: TSintagma read GetS write PutS; default;
    property Empty: boolean read GetEmpty;
    procedure AddList(AList : TSintagmaList); //override;
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
    FVersiculo: TObject; // TVersiculo;
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
    procedure DoOnMiddleClick(Sender: TObject; Shift: TShiftState);
    function GetCorrelacionado: boolean;
    function GetParesTemStrongs: boolean;
    function GetTags: string;
    function GetTemStrongs: boolean;
    function GetTemMorfo: boolean;
    procedure SetApontado(const AValue: boolean);
    procedure SetCorrelacionado(const AValue: boolean);
    function GetGist: string;
  public
    constructor Criar(s: TTagSintagma; owner: TObject);
    procedure Renderizar;
    destructor Destruir;
    procedure SelecaoMais;
    procedure SelecaoMenos;
    procedure InverterSelecao;
    procedure Desassociar;
    procedure DesassociarPares;
    procedure HighlightStrong(strong: string);
    procedure ToggleStrongHighlight(enable: boolean);
    function GetProximo: TSintagma;
    function GetChaveSugestao(t: TTipoListaPares): string;
    function Igual(other : TSintagma): boolean;
    property Strong: TStringList read FStrong write FStrong;
    property Morf: TStringList read FMorf write FMorf;
    property Tags: string read GetTags;
    property LabelRef: TLabel read FLabel write FLabel;
    property Pares: TSintagmaList read FPares write FPares;
    property Irmaos: TSintagmaList read FIrmaos write FIrmaos;
    property VersiculoRef: TObject{TVersiculo} read FVersiculo write FVersiculo;
    property Selecionado: boolean read FSelecionado;
    property Correlacionado: boolean read GetCorrelacionado write SetCorrelacionado;
    property Apontado: boolean write SetApontado;
    property Texto: string read FTexto write FTexto;
    property TextoBruto: string read FTextoCru write FTextoCru;
    //property ChaveSugestao: string read GetChaveSugestao;
    property Tipo: TTipoSintagma read FTipo write FTipo;
    property Italico: boolean read FItalico;
    property Sobrescrito: boolean read FSobrescrito;
    property TemStrongs: boolean read GetTemStrongs;
    property TemMorfs: boolean read GetTemMorfo;
    property ParesTemStrongs: boolean read GetParesTemStrongs;
    property Gist: string read GetGist;
  published

  end;

implementation

uses Versiculo;

var Hint: THintWindow;

{ TSintagmaListReverseEnumerator }

function TSintagmaListReverseEnumerator.GetCurrent: TSintagma;
begin
  Result := FList[FPosition];
end;

constructor TSintagmaListReverseEnumerator.Create(AList: TSintagmaList);
begin
  inherited Create;
  FList := AList;
  FPosition := FList.Count;
end;

function TSintagmaListReverseEnumerator.MoveNext: Boolean;
begin
  Dec(FPosition);
  Result := (FPosition >= 0) and (FPosition < FList.Count);
end;

function TSintagmaListReverseEnumerator.GetEnumerator: TSintagmaListReverseEnumerator;
begin
  Result := Self;
end;

{ TSintagmaListEnumerator }

function TSintagmaListEnumerator.GetCurrentSintagma: TSintagma;
begin
  result := TSintagma(GetCurrent);
end;

{ TSintagma }

procedure TSintagma.DoOnLeftClick(Sender: TObject; Shift: TShiftState);
//var
//  d: string;
var
  versiculo: TVersiculo;
begin
  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VersiculoRef.Selecao.Count, VersiculoRef.VersiculoPar.Selecao.Count, Irmaos.Count, Pares.Count]);
  versiculo := TVersiculo(VersiculoRef);
  if (ssShift in Shift) and Assigned(Versiculo.VersiculoPar) then
  begin
    DesassociarPares;
    if (versiculo.Selecao.Count > 0) then
      versiculo.Selecao[0].DesassociarPares;
  end;
  if not (ssCtrl in Shift) then // Ctrl não pressionado
    versiculo.LimparSelecao;

  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VersiculoRef.Selecao.Count, VersiculoRef.VersiculoPar.Selecao.Count, Irmaos.Count, Pares.Count]);
  InverterSelecao;
  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VersiculoRef.Selecao.Count, VersiculoRef.VersiculoPar.Selecao.Count, Irmaos.Count, Pares.Count]);

  if (ssShift in Shift) and (Assigned(versiculo.VersiculoPar)) and
     (versiculo.VersiculoPar.Selecao.Count > 0) and
     (versiculo.Selecao.Count > 0) then // Shift pressionado
  begin
    versiculo.AssociarSintagmas;
    versiculo.Modificado:=true;
    versiculo.VersiculoPar.Modificado:=true;
  end
  else if Assigned(versiculo.VersiculoPar) then // Shift não pressionado
  begin
    versiculo.VersiculoPar.LimparSelecao;
    if Pares.Count > 0 then
    begin
      //VersiculoRef.SelecionarSintagmas(Pares[0].Pares);
      versiculo.SelecionarSintagmas(Irmaos);
      versiculo.VersiculoPar.SelecionarSintagmas(Pares);
    end;
  end;

  versiculo.Renderizar;
  //versiculo.VersiculoPar.Renderizar;

  if assigned(versiculo.OnClick) then
    versiculo.OnClick(self);
end;

procedure TSintagma.DoOnDblClick(Sender: TObject);
begin
  {
  if (TVersiculo(FVersiculo).Ativo) then
  begin
    //LabelRef.Font.StrikeTrough := not LabelRef.Font.StrikeTrough;
    with TVersiculo(FVersiculo).Edit do
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
  }
end;

procedure TSintagma.DoOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TVersiculo(FVersiculo).Edit.OnExit(Self);
  if Button = mbRight then
    DoOnRightClick(Sender, Shift)
  else if Button = mbLeft then
    DoOnLeftClick(Sender, Shift)
  else if Button = mbMiddle then
    DoOnMiddleClick(Sender, Shift);
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
  versiculo: TVersiculo;
begin
  SetApontado(true);

  for i:=0 to Pares.Count-1 do
    Pares[i].SetApontado(true);
  for i:=0 to Irmaos.Count-1 do
    Irmaos[i].SetApontado(true);

  versiculo := TVersiculo(VersiculoRef);
  if assigned(versiculo.OnMouseEnter) then
    versiculo.OnMouseEnter(self);

  if not versiculo.MostrarDicas then exit;

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

  if assigned(TVersiculo(VersiculoRef).OnMouseLeave) then
    TVersiculo(VersiculoRef).OnMouseLeave(self);
end;

procedure TSintagma.DoOnRightClick(Sender: TObject; Shift: TShiftState);
var
  UltimoSelecionado: TSintagma;
  versiculo: TVersiculo;
begin
  {if ssShift in Shift then
  begin
    TVersiculo(FVersiculo).OnSintagmaPopupMenu(self);
    exit;
  end;
  }

  versiculo := TVersiculo(VersiculoRef);
  if not assigned(versiculo.VersiculoPar) then
    exit;

  DesassociarPares;
  if (versiculo.Selecao.Count > 0) then
    versiculo.Selecao[0].DesassociarPares;

  if not (ssCtrl in Shift) then // Ctrl não pressionado
    versiculo.LimparSelecao;

  InverterSelecao;

  if Assigned(versiculo.VersiculoPar) and
     (versiculo.VersiculoPar.Selecao.Count > 0) and
     (versiculo.Selecao.Count > 0) then
  begin
    versiculo.AssociarSintagmas;
    versiculo.Modificado:=true;
    versiculo.VersiculoPar.Modificado:=true;
  end;

  if not (ssCtrl in Shift) and (versiculo.VersiculoPar.Selecao.Count > 0) then
  begin
    UltimoSelecionado := versiculo.VersiculoPar.Selecao[versiculo.VersiculoPar.Selecao.Count-1];
    versiculo.LimparSelecao;
    versiculo.VersiculoPar.LimparSelecao;
    if Assigned(UltimoSelecionado) and Assigned(UltimoSelecionado.GetProximo) then
      UltimoSelecionado.GetProximo.SelecaoMais;
  end;

  versiculo.Renderizar;
end;

procedure TSintagma.DoOnMiddleClick(Sender: TObject; Shift: TShiftState);
begin
  TVersiculo(FVersiculo).OnSintagmaPopupMenu(self);
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
  result := TVersiculo(FVersiculo).Ativo and assigned(Pares) and (Pares.Count > 0){ (not FLabel.Font.Italic)};
end;

function TSintagma.GetParesTemStrongs: boolean;
var
  p: TSintagma;
begin
  if not assigned(Pares) then
    exit;

  for p in Pares do
    if p.TemStrongs then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TSintagma.GetTags: string;
var
  tagsstr: TStringStream;
  t: string;
begin
  result := '';
  try
    tagsstr := TStringStream.Create('');
    if assigned(FStrong) then
      for t in FStrong do
        tagsstr.WriteString(format('<W%s>', [t]));
    if assigned(FMorf) then
      for t in FMorf do
        tagsstr.WriteString(format('<W%s>', [t]));
  finally
    result := tagsstr.DataString;
    tagsstr.Destroy;
  end;
end;

function TSintagma.GetTemStrongs: boolean;
begin
  result := assigned(FStrong) and (FStrong.Count > 0);
end;

function TSintagma.GetTemMorfo: boolean;
begin
    result := assigned(FMorf) and (FMorf.Count > 0);
end;

procedure TSintagma.SetApontado(const AValue: boolean);
begin
  //TVersiculo(FVersiculo).Painel.Refresh; // ocultando tags, se estiverem sendo exibidas

  if not assigned(FLabel) then
    exit;

  if AValue then
  begin // apontado
    FLabel.Font.Underline := true;
    if not Selecionado then
      FLabel.Color:= clHighlight;
  end
  else
  begin // não apontado
    FLabel.Font.Underline := false;
    if Selecionado then
      FLabel.Color:= clYellow
    else
      FLabel.Color:= clDefault;
  end;
end;

procedure TSintagma.SetCorrelacionado(const AValue: boolean);
begin
  if (not assigned(FVersiculo)) or
     (not TVersiculo(FVersiculo).Ativo) or
     (not assigned(FLabel)) then
    exit;

  if AValue then // correlacionado
  begin
    //FLabel.Font.Italic := false;
    //FLabel.Font.Color := FCor; //clWindowText;
    if FCor = clDefault then
      FLabel.Font.Color := TVersiculo(FVersiculo).CorAssociado //clGrayText
    else
      FLabel.Font.Color := FCor;
  end else // nao correlacionado
  begin
    //FLabel.Font.Italic := true;
    if FCor = clDefault then
      FLabel.Font.Color := TVersiculo(FVersiculo).CorDesassociado //clGrayText
    else
      FLabel.Font.Color := FCor;
  end;
end;

function TSintagma.GetGist: string;
  function TipoStr(t: TTipoSintagma): string;
  begin
    case t of
      tsNulo:     result := 'tsNulo';
      tsSintagma: result := 'tsSintagma';
      tsTag:      result := 'tsTag';
      tsEspaco:   result := 'tsEspaco';
      tsPontuacao:result := 'tsPontuacao';
      tsMetaDado: result := 'tsMetaDado';
      tsStrongCount: result := 'tsStrongCount';
    end;
  end;
begin
  result := format('{indice:%d,tipo:%s,texto:"%s"}',
    [TVersiculo(FVersiculo).Sintagmas.IndexOf(self), TipoStr(FTipo), FTexto]);
end;

procedure TSintagma.ToggleStrongHighlight(enable: boolean);
begin
  if not assigned(FLabel) then
    exit;

  FLabel.Font.Underline := enable;
end;

function TSintagma.Igual(other: TSintagma): boolean;
begin
  result := (FTipo = other.FTipo) and
            (FTextoCru = other.FTextoCru) and
            (FCor = other.FCor) and
            (FSobrescrito = other.FSobrescrito) and
            (FItalico = other.FItalico);
end;

{ cria uma instância básica do sintagma, sem label, eventos, etc. }
constructor TSintagma.Criar(s: TTagSintagma; owner: TObject);
begin
  FVersiculo   := owner;
  FTexto       := s.valor;
  FTextoCru    := s.valor;
  FTipo        := s.tipo;
  FCor         := s.cor;
  FSobrescrito := s.sobrescrito;
  FItalico     := s.italico;
  FLabel       := nil;
  FPares       := TSintagmaList.Create;
  FIrmaos      := TSintagmaList.Create;
  FStrong      := TStringList.Create;
  FMorf        := TStringList.Create;
end;

{ Instancia a parte visual do sintagma (Label) }
procedure TSintagma.Renderizar;
begin
  if (FTipo in [tsSintagma, tsEspaco, tsPontuacao, tsStrongCount]) or
     (FTexto = '<FI>') or (FTexto = '<Fi>') then
  begin
    if not assigned(FLabel) then
      FLabel := TLabel.Create(TVersiculo(FVersiculo).Painel);

    FLabel.Caption     := FTexto;
    if FTexto = '<FI>' then
      FLabel.Caption := '['
    else if FTexto = '<Fi>' then
      FLabel.Caption := ']';
    FLabel.ParentColor := false;
    FLabel.Font.Color  := FCor;
    FLabel.Parent      := TVersiculo(FVersiculo).Painel;
    FLabel.ParentFont  := FTipo <> tsStrongCount;
    FLabel.AutoSize    := true;
    if FSobrescrito then
      FLabel.Font.Size := round(FLabel.Font.Size * 0.7);
    if TVersiculo(FVersiculo).PalavrasComStrongEmNegrito then
      FLabel.Font.Bold := TemStrongs;
    Correlacionado     := FPares.Count > 0;
  end else
    FLabel := nil;

  if FTipo = tsSintagma then
  begin
    //FLabel.OnClick     := @DoOnClick;
    //FLabel.OnDblClick   := @DoOnDblClick;
    FLabel.OnMouseDown  := @DoOnMouseDown;
    FLabel.OnMouseUp    := @DoOnMouseUp;
    FLabel.OnMouseEnter := @DoOnMouseEnter;
    FLabel.OnMouseLeave := @DoOnMouseLeave;
  end else
  begin
    FStrong := nil;
    FMorf   := nil;
  end;
end;

destructor TSintagma.Destruir;
begin
  DesassociarPares;
  if assigned(FLabel) then
  begin
    FLabel.Destroy;
    FLabel := nil;
  end;
  if assigned(FPares) then
  begin
    FPares.Destroy;
    FPares := nil;
  end;
  if assigned(FIrmaos) then
  begin
    FIrmaos.Destroy;
    FIrmaos := nil;
  end;
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
  if TVersiculo(FVersiculo).Ativo and assigned(FLabel) then
    FLabel.Color:= clYellow;
  TVersiculo(FVersiculo).Selecao.Add(Self);
end;

procedure TSintagma.SelecaoMenos;
begin
  FSelecionado := false;
  if TVersiculo(FVersiculo).Ativo and assigned(FLabel) then
    FLabel.Color:= clWindow;
  TVersiculo(FVersiculo).Selecao.Remove(Self);
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
  s: TSintagma;
begin
  for s in Irmaos do
    s.Desassociar;

  if Pares.Count > 0 then
  begin
    for s in Pares[0].Irmaos do
      s.Desassociar;
    Pares[0].Desassociar;
  end;

  Desassociar;
end;

procedure TSintagma.HighlightStrong(strong: string);
var
  s: string;
begin
  if not assigned(FStrong) then
    exit;

  for s in FStrong do
    if s = strong then
      ToggleStrongHighlight(true);
end;

function TSintagma.GetProximo: TSintagma;
var
  i: Integer;
begin
  result := nil;
  with TVersiculo(FVersiculo) do
  begin
    i := Sintagmas.IndexOf(Self);
    if (i >= 0) and (i < Sintagmas.Count) then
    begin
      for i := i+1 to Sintagmas.Count-1 do
      begin
        if (Sintagmas[i].Tipo = tsSintagma) and
           Sintagmas[i].TemStrongs and
           (not Sintagmas[i].Correlacionado) then
        begin
          result := Sintagmas[i];
          break;
        end;
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

function TSintagmaList.GetEmpty: boolean;
begin
  result := Count = 0;
end;

function TSintagmaList.GetS(Index: Integer): TSintagma;
begin
  result := TSintagma(Get(Index));
end;

procedure TSintagmaList.PutS(Index: Integer; Item: TSintagma);
begin
  if IndexOf(Item) = -1 then  // evitando duplicatas
    Put(Index, Item);
end;

function TSintagmaList.GetEnumerator: TSintagmaListEnumerator;
begin
  Result := TSintagmaListEnumerator.Create(Self);
end;

function TSintagmaList.GetReverseEnumerator: TSintagmaListReverseEnumerator;
begin
  Result := TSintagmaListReverseEnumerator.Create(Self);
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

