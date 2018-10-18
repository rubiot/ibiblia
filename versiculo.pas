unit Versiculo;

{$mode objfpc}{$H+}

{TODO -cObrigatório: Rever algoritmo de associação/desassociação }

interface

uses
  Classes, SysUtils, StrUtils, ExtCtrls, StdCtrls, Controls, Graphics,
  ONTTokenizer, Sintagma, Forms, LCLType, Math, LCLProc, Dialogs, LazUTF8,
  ONTParser, dbugintf;

type

  TOnSintagmaEvent = procedure (Sender: TSintagma) of object;
  TOnAlterarVersiculoEvent = procedure () of object;

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
    FPalavrasComStrongEmNegrito: boolean;
    FMostrarQtdStrongs: boolean;
    //FFontePadrao: TFont;
    FCorAssociado: TColor;
    FCorDesassociado: TColor;
    FOnSintagmaClick: TOnSintagmaEvent;
    FOnSintagmaMouseEnter: TOnSintagmaEvent;
    FOnSintagmaMouseLeave: TOnSintagmaEvent;
    FXML: string;
    FONTParser: TONTParser;
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
    procedure SetMostrarQtdStrongs(AValue: boolean);
    procedure SetPalavrasComStrongEmNegrito(AValue: boolean);
    procedure SetPares(const AValue: string);
    procedure SetTexto(_XML: string);
    function GetTexto: string;
    procedure SetVersiculoPar(Par: TVersiculo);
    procedure SelecionarListaSintagmas(lst: string);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
    procedure EditConfirm;
    procedure AtualizarXMLInterno;
    procedure Renderizar;
    procedure AtualizarStrongCount;
    function GetTokens: string;
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
    procedure SelecionarSintagmas(list: TSintagmaList);
    procedure AlterarTexto(_XML: string);
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
    property Edit: TEdit read FEdit write FEdit;
    property PalavrasComStrongEmNegrito: boolean read FPalavrasComStrongEmNegrito write SetPalavrasComStrongEmNegrito;
    property MostrarQtdStrongs: boolean read FMostrarQtdStrongs write SetMostrarQtdStrongs;
    property DebugTokens: string read GetTokens;
 published
    { Published declarations }
  end;

implementation

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
  FSelecao              := TSintagmaList.Create;
  FAtivo                := true;
  FMostrarDicas         := false;
  //FFontePadrao          := TFont.Create;
  //FFontePadrao.Assign(TheOwner.Font);
  FCorAssociado         := clWindowText;
  FCorDesassociado      := clGrayText;
  //FStrongMorfoComoChave := false;
  FDestruindo           := false;
  FONTParser := TONTParser.Create;
end;

destructor TVersiculo.Destruir;
var
  s: TSintagma;
begin
  FDestruindo := true;

  if assigned(FSintagmas) then
  begin
    for s in FSintagmas do
      s.Destruir;
    FSintagmas.free;
  end;

  FSelecao.free;
  FEdit.free;
  FONTParser.Destroy;
  //FFontePadrao.Free;
end;

procedure TVersiculo.LimparSintagmas;
var
  s: TSintagma;
begin
  if assigned(FSintagmas) then
  begin
    for s in FSintagmas do
      s.Destruir;
    FSintagmas.Clear;
  end;
  FSelecao.Clear;
end;

procedure TVersiculo.AssociarSintagmas;
var
  s: TSintagma;
begin
  if not Assigned(VersiculoPar) then
    exit;

  for s in Selecao do
  begin
    with s do
    begin
      Pares.Clear;
      Pares.AddList(VersiculoPar.Selecao);
      Correlacionado:=true;
      Irmaos.Clear;
      Irmaos.AddList(Selecao);
      Irmaos.Remove(s);
    end;
  end;
  for s in VersiculoPar.Selecao do
  begin
    with s do
    begin
      Pares.Clear;
      Pares.AddList(Selecao);
      Correlacionado:=true;
      Irmaos.Clear;
      Irmaos.AddList(VersiculoPar.Selecao);
      Irmaos.Remove(s);
    end;
  end;
end;

procedure TVersiculo.LimparSelecao;
begin
  while not Selecao.Empty do
    TSintagma(Selecao.First).SelecaoMenos;
end;

procedure TVersiculo.DesassociarPares;
var
  s: TSintagma;
begin
  LimparSelecao;
  VersiculoPar.LimparSelecao;

  for s in FSintagmas do
    s.Desassociar;

  for s in VersiculoPar.FSintagmas do
    s.Desassociar;
end;

procedure TVersiculo.LimparAssociacoes;
var
  s: TSintagma;
begin
  for s in FSintagmas do
    if not s.Pares.Empty then
    begin // desassociar somente se houver associacoes, para evitar falsos 'modificado = true'
      DesassociarPares;
      Modificado := true;
      break;
    end;
end;

procedure TVersiculo.SelecionarSintagmas(list: TSintagmaList);
var
  s: TSintagma;
begin
  for s in list do
    s.SelecaoMais;
end;

function TVersiculo.GetListaPares(tipo: TTipoListaPares): TStringList;
var
  t: string;
  s, p: TSintagma;
  tmp: TSintagmaList;
begin
  result := TStringList.Create;

  tmp := TSintagmaList.Create;
  for s in FSintagmas do
  begin
    if assigned(s.Pares) and (tmp.IndexOf(s) < 0) then
    begin
      t := s.GetChaveSugestao(tipo);
      tmp.Add(s);
      for p in s.Irmaos do
      begin
        t := t + ';' + p.GetChaveSugestao(tipo);
        tmp.Add(p);
      end;
      result.Add(t);
      t := '';
      for p in s.Pares do
      begin
        t := t + p.GetChaveSugestao(tipo);
        if s.Pares.IndexOf(p) <> s.Pares.Count-1 then
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
begin
  FXML := _XML;
  LimparSintagmas;
  FSintagmas := FONTParser.ParseLine(FXML, self);
  Renderizar;
  Modificado := false;
  FXMLModificado := false;
end;

{ Substitui o texto do versículo mantendo as associações existentes }
procedure TVersiculo.AlterarTexto(_XML: string);
var
  new, result: TSintagmaList;
  s: TSintagma;
  found: boolean;
begin
  new := TSintagmaList.Create;
  new := FONTParser.ParseLine(_XML, self);

  LimparSelecao;

  if FSintagmas[FSintagmas.Count-1].Tipo = tsStrongCount then
  begin
    FSintagmas[FSintagmas.Count-1].Destruir;
    FSintagmas.Delete(FSintagmas.Count-1);
  end;

  result := TSintagmaList.Create;

  while not (new.Empty and FSintagmas.Empty) do
  begin
    if new.Count = 0 then
    begin
      FSintagmas[0].Destruir;
      FSintagmas.Delete(0);
    end
    else
    if FSintagmas.Count = 0 then
    begin
     result.Add(new[0]);
     new.Delete(0);
    end
    else
    if new[0].Igual(FSintagmas[0]) then
    begin
      result.Add(FSintagmas[0]);
      FSintagmas.Delete(0);
      new[0].Destruir;
      new.Delete(0);
    end
    else
    begin
      // verificando se o velho sintagma ainda será usado
      found := false;
      for s in new do
        if s.Igual(FSintagmas[0]) then
        begin
          found := true;
          break;
        end;
      if not found then // não será utilizado, apagando-o
      begin
        FSintagmas[0].Destruir;
        FSintagmas.Delete(0);
      end;
      result.Add(new[0]);
      new.Delete(0);
    end;
  end;

  if new.Count > 0 then
    raise Exception.Create('Ainda há sintagmas novos que não foram liberados!');

  if FSintagmas.Count > 0 then
    raise Exception.Create('Ainda há sintagmas antigos que não foram liberados!');

  FSintagmas.Destroy;
  FSintagmas := result;

  FXML := _XML;
  FXMLModificado := true;
  FModificado := true;
  VersiculoPar.Modificado := true;

  Renderizar;
end;

function TVersiculo.GetTexto: string;
var
  linha: TStringStream;
  s: TSintagma;
begin
  result := '';
  try
    linha := TStringStream.Create('');
    for s in FSintagmas do
      if s.Tipo <> tsStrongCount then
        linha.WriteString(s.Texto);
  finally
    result := linha.DataString;
    linha.Destroy;
  end;
end;

procedure TVersiculo.SetPares(const AValue: string);
var
  varredorXML: TONTTokenizer;
  s: TTagSintagma;
begin
  if not (assigned(VersiculoPar)) or (length(AValue) = 0) then
    exit;

  FExibirErro := true;
  VersiculoPar.FExibirErro := true;

  varredorXML := TONTTokenizer.Criar(AValue);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if AnsiStartsStr('<par ', s.valor) then
    begin
      DebugLn('selecionando par: %s', [s.valor]);
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
  s, p: TSintagma;
  _xml: TStringStream;
  tmp: TSintagmaList;
begin
  _xml := TStringStream.Create('');

  // gerar tags de Texto <p a="1,2,3" b="1,2,3">
  tmp := TSintagmaList.Create;
  for s in FSintagmas do
  begin
    if (s.Pares.Count > 0) and (tmp.IndexOf(s) < 0) then
    begin
      _xml.WriteString(Format('<par a="%d', [FSintagmas.IndexOf(s)]));
      tmp.Add(s);
      for p in s.Irmaos do
      begin
        _xml.WriteString(Format(',%d', [TVersiculo(s.VersiculoRef).Sintagmas.IndexOf(p)]));
        tmp.Add(p);
      end;
      _xml.WriteString('" b="');
      for p in s.Pares do
      begin
        _xml.WriteString(Format('%d', [TVersiculo(p.VersiculoRef).Sintagmas.IndexOf(p)]));
        if s.Pares.IndexOf(p) <> s.Pares.Count-1 then
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
  s: TSintagma;
begin
  result := '';
  try
    linha := TStringStream.Create('');
    for s in FSintagmas do
      if s.Tipo in [tsEspaco, tsSintagma, tsPontuacao] then
        linha.WriteString(s.Texto);
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
  s: TSintagma;
  t, p: smallint;
begin
  t := 0;
  p := 0;
  result := 0;

  for s in FSintagmas do
    if s.Tipo = tsSintagma then
    begin
      inc(t);
      if s.Correlacionado then
        inc(p);
    end;

  if t > 0 then
    result := p/t;
end;

function TVersiculo.GetLinhaInterlinear: string;
var
  linha: TStringStream;
  s, p, prox: TSintagma;
  m: string;
  n: integer;
begin
  result := '';

  try
    linha := TStringStream.Create('');

    for s in FSintagmas do
    begin
      linha.WriteString(s.Texto);
      if (assigned(s.Strong) and (s.Strong.Count > 0)) or (assigned(s.Morf) and (s.Morf.Count > 0)) then
      begin // se este texto contém strongs
        for m in s.Strong do // strongs
          linha.WriteString(format('<W%s>', [m]));
        for m in s.Morf do // morfologia
          linha.WriteString(format('<WT%s>', [m]));
      end else
      begin // caso contrário, tentemos o texto relacionado
        for p in s.Pares do
        begin
          for m in p.Strong do // strongs
            linha.WriteString(format('<W%s>', [m]));
          for m in p.Morf do   // morfologia
            linha.WriteString(format('<WT%s>', [m]));
        end;
      end;

      if (FSintagmas.IndexOf(s) < (FSintagmas.Count-1)) and // não é o último sintagma e
         (s.Irmaos.Count > 0) then   // tem irmãos
      begin
        prox := nil;
        for n:=FSintagmas.IndexOf(s)+1 to Sintagmas.Count-1 do
        begin // procurando o próximo sintagma (saltando espaços, pontuação, etc.)
          if Sintagmas[n].Tipo = tsSintagma then
          begin
            prox := Sintagmas[n];
            break;
          end;
        end;
        if (prox <> nil) and (s.Irmaos.IndexOf(prox) >= 0) then // o próximo sintagma é irmão deste?
          continue;
      end;

      for p in s.Pares do // pares
        linha.WriteString('<sup>' + p.Texto + '</sup> ');
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

    for stg in FSintagmas do
    begin // sintagmas
      if stg.Tipo = tsStrongCount then
        continue;

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
begin
  FPanel.Font := AValue;
  Renderizar;
end;

procedure TVersiculo.SetModificado(const AValue: boolean);
begin
  if FModificado = AValue then exit;
  FModificado := AValue;
  if assigned(FOnAlterarVersiculo) and FModificado then
     FOnAlterarVersiculo;
end;

procedure TVersiculo.SetMostrarQtdStrongs(AValue: boolean);
begin
  if FMostrarQtdStrongs = AValue
     then Exit;
  FMostrarQtdStrongs:=AValue;
  Renderizar;
end;

procedure TVersiculo.SetPalavrasComStrongEmNegrito(AValue: boolean);
begin
  if FPalavrasComStrongEmNegrito=AValue then Exit;
  FPalavrasComStrongEmNegrito:=AValue;
  Renderizar;
end;

procedure TVersiculo.OrganizarSintagmas;
var
  s: TSintagma;
  x, y, a: smallint;
begin
  if not Ativo or FDestruindo or not assigned(FSintagmas) then
    exit;

  x := 5;
  y := 0;
  a := 0;
  for s in FSintagmas do
  begin
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
var
  tokens: TStringList;
  i, s: smallint;
begin
  tokens := TStringList.Create;
  tokens.DelimitedText := lst;
  for i:=0 to tokens.Count-1 do
  begin
    s := StrToInt(tokens[i]);
    if (s >= Sintagmas.Count) or (Sintagmas[s].Tipo <> tsSintagma) then
      raise Exception.Create(format('Índice %d inválido: %s', [s, Sintagmas[s].Gist]))
    else
      Sintagmas[s].SelecaoMais;
  end;
  tokens.Destroy;
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
{var
  s: TTagSintagma;
  varredorXML: TONTTokenizer;
  sintagma, novo: TSintagma;
  i, offset, j: integer;}
begin
  {
  sintagma := TSintagma(FEdit.Tag);
  i := FSintagmas.IndexOf(sintagma);
  FSintagmas.Remove(sintagma);
  FSelecao.Remove(sintagma);
  offset := 0;
  novo := nil;

  varredorXML := TONTTokenizer.Criar(FEdit.Caption);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if (s.tipo = tsEspaco) and (s.valor = '|') then
    begin
      if assigned(novo) then
        novo.TextoBruto := novo.TextoBruto + s.valor;
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
        novo.Correlacionado := true;
        novo.Pares.Add(sintagma.Pares[i]);
        sintagma.Pares[i].Pares.Remove(sintagma);
        sintagma.Pares[i].Pares.Add(novo);
      end;
      break;
    end;
  end;

  sintagma.Destruir;
  AtualizarXMLInterno;
  EditExit(Self);
  OrganizarSintagmas;
  }
end;

{ Atualiza o XML interno a partir do texto dos sintagmas }
procedure TVersiculo.AtualizarXMLInterno;
var
  s: TSintagma;
begin
  FXML := '';
  for s in FSintagmas do
    FXML := FXML + s.TextoBruto;

  FXMLModificado := true;
  FModificado := true;
end;

procedure TVersiculo.Renderizar;
var
  s: TSintagma;
begin
  if not FAtivo or not assigned(FSintagmas) then
    exit;

  AtualizarStrongCount;

  for s in FSintagmas do
    s.Renderizar;

  OrganizarSintagmas;
end;

procedure TVersiculo.AtualizarStrongCount;
var
  token: TTagSintagma;
  s: TSintagma;
  count, i: integer;
begin
  if FSintagmas.Empty then
    exit;

  if FSintagmas[FSintagmas.Count-1].Tipo = tsStrongCount then
  begin
    FSintagmas[FSintagmas.Count-1].Destruir;
    FSintagmas.Delete(FSintagmas.Count-1);
  end;

  if not FMostrarQtdStrongs then
    exit;

  count := 0;
  for s in FSintagmas do
    if s.TemStrongs then
      inc(count);

  with token do
  begin
    tipo       := tsStrongCount;
    valor      := format(' (%d strongs)', [count]);
    cor        := clRed;
    sobrescrito:= true;
    italico    := false;
  end;
  FSintagmas.Add(TSintagma.Criar(token, self));
end;

function TVersiculo.GetTokens: string;
var
  s: TSintagma;
  tokens: TStringStream;
begin
  tokens := TStringStream.Create('');
  for s in FSintagmas do
    tokens.WriteString(s.Gist);
  result:= tokens.DataString;
  tokens.Destroy;
end;


end.


