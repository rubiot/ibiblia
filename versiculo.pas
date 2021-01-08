unit Versiculo;

{$mode objfpc}{$H+}

{TODO -cObrigatório: Rever algoritmo de associação/desassociação }

interface

uses
  Classes, SysUtils, StrUtils, ExtCtrls, StdCtrls, Controls, Graphics,
  ONTTokenizer, Sintagma, Forms, LCLType, Math, LCLProc, Dialogs, LazUTF8,
  ONTParser, dbugintf, Menus, Clipbrd;

type

  TVersiculo = class;

  TOnSintagmaEvent = procedure (Sender: TSintagma) of object;
  TOnAlterarVersiculoEvent = procedure () of object;
  TOnExportTextEvent = procedure (Sender: TVersiculo) of object;

  TStrongsCountMode = (scNone, scCountWords, scCountStrongs);

  { TVersiculo }

  TVersiculo = class
  private
    { Private declarations }
    FMostrarDicas: boolean;
    FOnAlterarVersiculo: TOnAlterarVersiculoEvent;
    //FStrongMorfoComoChave: boolean;
    FVersiculoRef: TVersiculo;
    FSintagmas: TSintagmaList;
    FStrongCount: TSintagma;
    FSelecao: TSintagmaList;
    FModificado: boolean;
    FXMLModificado: boolean;
    FPanel: TScrollBox;
    FEdit: TEdit;
    FAtivo: boolean;
    FDestruindo: boolean;
    FExibirErro: boolean;
    FPalavrasComStrongEmNegrito: boolean;
    FStrongsCountMode: TStrongsCountMode;
    //FFontePadrao: TFont;
    FCorAssociado: TColor;
    FCorDesassociado: TColor;
    FOnSintagmaClick: TOnSintagmaEvent;
    FOnSintagmaMouseEnter: TOnSintagmaEvent;
    FOnSintagmaMouseLeave: TOnSintagmaEvent;
    FOnExportText: TOnExportTextEvent;
    FXML: string;
    FONTParser: TONTParser;
    FSyntagmPopupMenu: TPopupMenu;
    FVersePopupMenu: TPopupMenu;
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
    procedure SetStrongsCountMode(AValue: TStrongsCountMode);
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
    procedure AtualizarStrongCount;
    function GetTokens: string;
    procedure OnCopySyntagmTags(Sender: TObject);
    procedure OnPasteSyntagmTags(Sender: TObject);
    procedure OnSaveTextToFile(Sender: TObject);
    procedure OnVerseMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnResize(Sender: TObject);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure InitSyntagmPopupMenu;
    procedure InitVersePopupMenu;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Criar;
    constructor Criar(TheOwner: TScrollBox);
    destructor Destruir;
    procedure LimparSintagmas;
    procedure AssociarSintagmas;
    procedure LimparSelecao;
    procedure DesassociarPares;
    procedure LimparAssociacoes;
    procedure Renderizar;
    procedure RenderizarStrongCount;
    procedure ClearStrongCount;
    procedure OrganizarSintagmas;
    procedure SelecionarSintagmas(list: TSintagmaList);
    procedure AlterarTexto(_XML: string);
    function GetListaPares(tipo: TTipoListaPares): TStringList;
    procedure OnSintagmaPopupMenu(s: TSintagma);
    procedure MostrarTags;
    procedure OcultarTags;
    procedure EnableStrongHighlight(strong: string);
    procedure DisableStrongHighlight;

    function GetTheWordInterlinearLine: string;
    function GetMySwordInterlinearLine: string;
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
    property XMLModificado: boolean read FXMLModificado write FXMLModificado;
    property OnClick: TOnSintagmaEvent read FOnSintagmaClick write FOnSintagmaClick;
    property OnMouseEnter: TOnSintagmaEvent read FOnSintagmaMouseEnter write FOnSintagmaMouseEnter;
    property OnMouseLeave: TOnSintagmaEvent read FOnSintagmaMouseLeave write FOnSintagmaMouseLeave;
    property OnExportText: TOnExportTextEvent read FOnExportText write FOnExportText;
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
    property StrongsCountMode: TStrongsCountMode read FStrongsCountMode write SetStrongsCountMode;
    property DebugTokens: string read GetTokens;
 published
    { Published declarations }
  end;

resourcestring
  SCopyTags ='&Copy tags';
  SPasteAllTags = 'Paste &all tags';
  SPasteStrongTags = '&Paste &Strong''s tags';
  SPasteMorphoTags = 'Paste &morphology tags';
  SSaveToFile = '&Save text to file...';

implementation

var
  SintagmaClipboard: TSintagma;

function CompareIndexes(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result := List[Index1].ToInteger - List[Index2].ToInteger;
end;

{ TVersiculo }

constructor TVersiculo.Criar(TheOwner : TScrollBox);
begin
  FPanel := TheOwner;

  FAtivo := assigned(FPanel);

  if (FAtivo) then
  begin
    FPanel.Color   := clWindow;
    FPanel.Caption := '';

    InitSyntagmPopupMenu;
    InitVersePopupMenu;

    FEdit            := TEdit.Create(FPanel);
    FEdit.Visible    := false;
    FEdit.AutoSize   := false;
    FEdit.OnKeyDown  := @EditKeyDown;
    FEdit.OnExit     := @EditExit;
    FPanel.InsertControl(FEdit);
    FPanel.OnMouseDown  := @OnVerseMouseDown;
    FPanel.OnResize     := @OnResize;
    FPanel.OnMouseWheel := @OnMouseWheel;
  end;

  FStrongCount          := nil;
  FStrongsCountMode     := scNone;
  FVersiculoRef         := nil;
  FOnSintagmaClick      := nil;
  FOnSintagmaMouseEnter := nil;
  FOnSintagmaMouseLeave := nil;
  FModificado           := false;
  FXMLModificado        := false;
  FSelecao              := TSintagmaList.Create;
  FMostrarDicas         := false;
  //FFontePadrao          := TFont.Create;
  //FFontePadrao.Assign(TheOwner.Font);
  FCorAssociado         := clWindowText;
  FCorDesassociado      := $A3A3A3;//clGrayText;
  //FStrongMorfoComoChave := false;
  FDestruindo           := false;
  FONTParser := TONTParser.Create;
end;

destructor TVersiculo.Destruir;
begin
  FDestruindo := true;
  LimparSintagmas;
  FSintagmas.Free;
  FSelecao.Destroy;
  if assigned(FPanel) then
  begin
    FEdit.Free;
    FSyntagmPopupMenu.Free;
    FVersePopupMenu.Free;
    FPanel.OnMouseDown  := nil;
    FPanel.OnResize     := nil;
    FPanel.OnMouseWheel := nil;
  end;
  FONTParser.Destroy;
  //FFontePadrao.Free;
end;

procedure TVersiculo.LimparSintagmas;
var
  s: TSintagma;
begin
  if assigned(FSintagmas) then
  begin
    if FAtivo then
      FPanel.DisableAutoSizing;

    for s in FSintagmas do
      s.Destruir;

    ClearStrongCount;

    FSintagmas.Clear;

    if FAtivo then
      FPanel.EnableAutoSizing;
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

  Modificado := true;
  VersiculoPar.Modificado := true;
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

  Renderizar;
  VersiculoPar.Renderizar;
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
  src, dst: string;
  s, p: TSintagma;
  tmp: TSintagmaList;
begin
  result := TStringList.Create;

  tmp := TSintagmaList.Create;
  for s in FSintagmas do
  begin
    if not assigned(s.Pares) or s.Italico or (tmp.IndexOf(s) >= 0) then
      continue;

    src := s.GetChaveSugestao(tipo);
    tmp.Add(s);
    for p in s.Irmaos do
    begin
      if p.Italico then
        continue;
      src := src + ';' + p.GetChaveSugestao(tipo);
      tmp.Add(p);
    end;
    dst := '';
    for p in s.Pares do
    begin
      if p.Italico then
        continue;
      dst := dst + p.GetChaveSugestao(tipo) + ';'
    end;
    if not src.IsEmpty and not dst.IsEmpty then
    begin
      result.Add(src);
      result.Add(dst.TrimRight(';'));
    end;
  end;
  tmp.Destroy;
end;

procedure TVersiculo.OnSintagmaPopupMenu(s: TSintagma);
var
  p: TPoint;
begin
  FSyntagmPopupMenu.Tag := PtrInt(s);
  p := s.LabelRef.ClientToScreen(s.LabelRef.ClientRect.BottomRight);
  FSyntagmPopupMenu.PopUp(p.x, p.y);
end;

procedure TVersiculo.MostrarTags;
var
  s: TSintagma;
  p: TPoint;
  tags: string;
begin
  if not FAtivo then
    exit;

  for s in FSintagmas do
  begin
    if not assigned(s.LabelRef) or not s.TemStrongs then
      continue;
    p := s.LabelRef.ClientToParent(s.LabelRef.ClientRect.TopLeft, FPanel);
    tags := s.Strong.CommaText;
    with FPanel.Canvas do
    begin
      // Link para solução overriding Paint: http://forum.lazarus.freepascal.org/index.php?topic=23894.0
      Brush.Color := clYellow;
      Font.Name   := DefFontData.Name;
      Font.Height := round(s.LabelRef.Font.Height * 0.6);
      Font.Color  := clBlack;
      Rectangle(p.x, p.y, p.x + TextWidth(tags) + 4, p.y + TextHeight(tags));
      Brush.Style := bsClear;
      TextOut(p.x+2, p.y, tags);
    end;
  end;
end;

procedure TVersiculo.OcultarTags;
begin
  if FAtivo then
    FPanel.Refresh;
end;

procedure TVersiculo.EnableStrongHighlight(strong: string);
var
  s: TSintagma;
begin
  for s in FSintagmas do
    s.HighlightStrong(strong);
end;

procedure TVersiculo.DisableStrongHighlight;
var
  s: TSintagma;
begin
  for s in FSintagmas do
    s.ToggleStrongHighlight(false);
end;

procedure TVersiculo.SetTexto(_XML: string);
begin
  FXML := _XML;
  LimparSintagmas;
  FSintagmas := FONTParser.ParseLine(FXML, self);
  Renderizar;
  Modificado := true;
  FXMLModificado := true;
  SintagmaClipboard := nil;
end;

{ Replaces the verse text trying to keep existing associations }
procedure TVersiculo.AlterarTexto(_XML: string);

  procedure DeleteSyntagm(i: integer; newlist: TSintagmaList);
  var
    s, s1, pair: TSintagma;
  begin
    s := FSintagmas[i];
    if assigned(s.Pares) then
    begin
      for pair in FVersiculoRef.Sintagmas do
      begin
        pair.Pares.Remove(s);
        if pair.Pares.Count = 0 then
          pair.Correlacionado := false;
      end;
      s.Pares.Clear;
    end;

    if s.Tipo = tsSintagma then
      for s1 in newlist do
        if assigned(s1.Irmaos) then
          s1.Irmaos.Remove(s);

    s.Destruir;
    FSintagmas.Delete(i);
  end;

var
  new, result: TSintagmaList;
  s, old: TSintagma;
  found: boolean;
begin
  if FXML = _XML then
    exit;

  new := TSintagmaList.Create;
  new := FONTParser.ParseLine(_XML, self);

  LimparSelecao;

  if FSintagmas.Empty then
    FSintagmas := new
  else
  begin
    result := TSintagmaList.Create;

    while not (new.Empty and FSintagmas.Empty) do
    begin
      if new.Empty then
        DeleteSyntagm(0, result)
      else if FSintagmas.Empty then
      begin
       result.Add(new[0]);
       new.Delete(0);
      end
      else if FSintagmas.First.Tipo <> tsSintagma then
        DeleteSyntagm(0, result)
      else if new.First.Tipo <> tsSintagma then
      begin
        result.Add(new[0]);
        new.Delete(0);
      end
      else if new.First.Igual(FSintagmas.First) then
      begin
        with FSintagmas.First do
        begin
          Strong.Free;
          Morf.Free;
          Strong := new.First.Strong;
          Morf   := new.First.Morf;
          new.First.Strong := nil;
          new.First.Morf   := nil;
        end;
        result.Add(FSintagmas.First);
        FSintagmas.Delete(0);
        new.First.Destruir;
        new.Delete(0);
      end
      else
      begin
        { checking if the old syntagm will be used in the future }
        found := false;
        old := FSintagmas.First;
        if not old.Pares.Empty then // does it have associations?
        begin
          for s in new do
            if old.Igual(s) then
            begin
              result.Add(new[0]);
              new.Delete(0);
              found := true;
              break;
            end;
        end;
        if not found then { nope, wasting it }
          DeleteSyntagm(0, result);
      end;
    end;

    if not new.Empty then
      raise Exception.Create('Ainda há sintagmas novos que não foram liberados!');

    if not FSintagmas.Empty then
      raise Exception.Create('Ainda há sintagmas antigos que não foram liberados!');

    LimparSintagmas;
    FSintagmas.Destroy;
    FSintagmas := result;
  end;

  FXML := _XML;
  FXMLModificado := true;
  FModificado := true;
  if assigned(VersiculoPar) then
    VersiculoPar.Modificado := true;
  SintagmaClipboard := nil;

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
  if not assigned(VersiculoPar) or AValue.IsEmpty then
    exit;

  FExibirErro := true;
  VersiculoPar.FExibirErro := true;

  varredorXML := TONTTokenizer.Criar(AValue);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if AnsiStartsStr('<par ', s.valor) then
    begin
      //DebugLn('selecionando par: %s', [s.valor]);
      SelecionarListaSintagmas(varredorXML.LerPropriedadeTag('a', s));
      VersiculoPar.SelecionarListaSintagmas(varredorXML.LerPropriedadeTag('b', s));
      AssociarSintagmas;

      LimparSelecao;
      VersiculoPar.LimparSelecao;
    end;
  end;
  varredorXML.Destruir;

  Renderizar;
  VersiculoPar.Renderizar;
end;

function TVersiculo.GetPares: string;
var
  b: integer;
  s, p: TSintagma;
  _xml: TStringStream;
  tmp: TSintagmaList;
begin
  _xml := TStringStream.Create('');

  // gerar tags de Texto <p a="1,2,3" b="1,2,3">
  tmp := TSintagmaList.Create;
  for s in FSintagmas do
  begin
    if not s.Pares.Empty and (tmp.IndexOf(s) < 0) then
    begin
      _xml.WriteString(Format('<par a="%d', [FSintagmas.IndexOf(s)]));
      tmp.Add(s);
      for p in s.Irmaos do
      begin
        _xml.WriteString(Format(',%d', [FSintagmas.IndexOf(p)]));
        tmp.Add(p);
      end;
      _xml.WriteString('" b="');
      for p in s.Pares do
      begin
        b := FVersiculoRef.Sintagmas.IndexOf(p);
        if b < 0 then
           raise exception.Create('Pair not found in other verse');
        _xml.WriteString(Format('%d', [b]));
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
  if FAtivo then
    result := FPanel.Font
  else
    result := nil;
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

function TVersiculo.GetTheWordInterlinearLine: string;

  function GetSpacingBefore(syn, pair: TSintagma): string;
  var
    i: integer;
  begin
    result := '';
    if pair = syn.Pares[0] then exit;
    for i:=FVersiculoRef.Sintagmas.IndexOf(pair)-1 downto 0 do
    begin
      if FVersiculoRef.Sintagmas[i].Tipo = tsSintagma then
        break;
      if FVersiculoRef.Sintagmas[i].Tipo = tsEspaco then
      begin
        result := ' ';
        break;
      end;
    end;
  end;

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
        linha.WriteString(GetSpacingBefore(s, p) + '<sup>' + IfThen(p.Italico, format('<FI>%s<Fi>', [p.Texto]), p.Texto) + '</sup>');
    end;

  finally
    result := linha.DataString.Replace('</sup> <sup>', ' ')
                              .Replace('  ', ' ')
                              .Replace('<sup>', '<font size=+1 color="CC3300"><sup>')
                              .Replace('</sup>', '</sup></font>')
                              .Replace('<Fi><FI>', '')
                              .Replace(' -', '-');
    linha.Destroy;
  end;
end;

function TVersiculo.GetMySwordInterlinearLine: string;

  function GetSpacingBefore(syn, pair: TSintagma): string;
  var
    i: integer;
  begin
    result := '';
    if pair = syn.Pares[0] then exit;
    for i:=FVersiculoRef.Sintagmas.IndexOf(pair)-1 downto 0 do
    begin
      if FVersiculoRef.Sintagmas[i].Tipo = tsSintagma then
        break;
      if FVersiculoRef.Sintagmas[i].Tipo = tsEspaco then
      begin
        result := ' ';
        break;
      end;
    end;
  end;

var
  line: TStringStream;
  s, p: TSintagma;
  m: string;
begin
  result := '';

  {Sample: <TS>The Creation<Ts><Q><wg>εν<WG1722><E> In<e><q> <Q><wg>αρχή<WG746><E> <FI>the<Fi> beginning<e><q>}
  try
    line := TStringStream.Create('');

    for s in FSintagmas do
    begin
      case s.Tipo of
        tsSintagma:
        begin
          line.WriteString('<Q>');
          line.WriteString(s.Texto);

          for m in s.Strong do
            line.WriteString(format('<W%s>', [m]));
          for m in s.Morf do
            line.WriteString(format('<WT%s>', [m]));

          if s.Pares.Count > 0 then
          begin
            line.WriteString('<T>');
            if (s.Irmaos.Count = 0) or (FSintagmas.IndexOf(s) < FSintagmas.IndexOf(s.Irmaos[0])) then
            begin // no siblings of the first sibling
              for p in s.Pares do
              begin
                line.WriteString(GetSpacingBefore(s, p));
                line.WriteString(IfThen(p.Italico, format('<FI>%s<Fi', [p.Texto]), p.Texto));
              end;
            end else // not a first sibling
              line.WriteString('←');
            line.WriteString('<t>');
          end;
          line.WriteString('<q>');
        end;
        tsTag:       line.WriteString(s.TextoBruto);
        tsEspaco:    line.WriteString(s.TextoBruto);
        tsPontuacao: line.WriteString(s.TextoBruto);
        tsMetaDado:  if s.Texto <> '<wt>' then line.WriteString(s.TextoBruto);
      end;
    end;
  finally
    result := line.DataString;
    line.Free;
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

    s := -1;
    for stg in FSintagmas do
    begin // sintagmas
      Inc(s);

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
              linha.WriteString(format('<W%s>', [stg.Pares[p].Strong[m]]));
              ls.Add(stg.Pares[p].Strong)
            end else // strong reutilizado
              linha.WriteString(format('<W%ss>', [stg.Pares[p].Strong[m]]));
          end;

          if not morfo then continue;

          for m:=0 to stg.Pares[p].Morf.Count-1 do   // morfologia
            linha.WriteString(format('<WT%s>', [stg.Pares[p].Morf[m]]));
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
            linha.WriteString(format('<W%sx>', [stg.Strong[m]]));
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
  if not FAtivo then
    exit;
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

procedure TVersiculo.SetStrongsCountMode(AValue: TStrongsCountMode);
begin
  if FStrongsCountMode = AValue
     then Exit;
  FStrongsCountMode := AValue;
  Renderizar;
end;

procedure TVersiculo.SetPalavrasComStrongEmNegrito(AValue: boolean);
begin
  if FPalavrasComStrongEmNegrito=AValue then Exit;
  FPalavrasComStrongEmNegrito:=AValue;
  Renderizar;
end;

procedure TVersiculo.OrganizarSintagmas;

  procedure PositionSyntagm(s: TSintagma; var x, y, a: smallint);
  begin
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

var
  s: TSintagma;
  x, y, a: smallint;
begin
  if not Ativo or FDestruindo or not assigned(FSintagmas) then
    exit;

  x := 5; // start x position
  y := 0; // y position
  a := 0; // line width
  for s in FSintagmas do
  begin
    if not assigned(s.LabelRef) then //s.Tipo in [tsMetaDado, tsTag] then
      continue;
    PositionSyntagm(s, x, y, a);
  end;

  if (FStrongsCountMode <> scNone) and assigned(FStrongCount) then
    PositionSyntagm(FStrongCount, x, y, a);
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
  tokens.CustomSort(@CompareIndexes);
  for i:=0 to tokens.Count-1 do
  begin
    s := StrToInt(tokens[i]);
    if (s < 0) or (s >= Sintagmas.Count) or not (Sintagmas[s].Tipo in [tsSintagma, tsPontuacao]) then
      raise Exception.Create(format('Invalid syntagm index: %d', [s]))
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

  FPanel.DisableAutoSizing;

  for s in FSintagmas do
    s.Renderizar;
  RenderizarStrongCount;

  FPanel.EnableAutoSizing;

  OrganizarSintagmas;
end;

procedure TVersiculo.RenderizarStrongCount;
begin
  ClearStrongCount;
  if FStrongsCountMode = scNone then
    exit;
  if FSintagmas.Empty then
    exit;

  AtualizarStrongCount;
  FStrongCount.Renderizar;
end;

procedure TVersiculo.ClearStrongCount;
begin
  if not assigned(FStrongCount) then
    exit;

  FStrongCount.Destruir;
  FStrongCount := nil;
end;

procedure TVersiculo.AtualizarStrongCount;

  function GetStrongsCount(s: TSintagma): integer;
  begin
    result := 0;
    case FStrongsCountMode of
      scCountStrongs:
        result := s.StrongsCount;
      scCountWords:
        result := s.StrongsWordsCount;
    end;
  end;

var
  token: TTagSintagma;
  s, p: TSintagma;
  count: integer;
  unique: TSintagmaList;
begin
  count := 0;
  unique := TSintagmaList.Create;
  for s in FSintagmas do
  begin
    if s.Tipo <> tsSintagma then
      continue;
    if unique.IndexOf(s) <> -1 then
      continue;
    inc(count, GetStrongsCount(s));
    if not s.TemStrongs and assigned(s.Irmaos) then
      for p in s.Irmaos do
        if not p.TemStrongs then
          unique.Add(p);
  end;
  unique.Destroy;

  with token do
  begin
    tipo       := tsStrongCount;
    valor      := format(' (%d strongs)', [count]);
    cor        := clRed;
    sobrescrito:= true;
    italico    := false;
  end;
  FStrongCount := TSintagma.Criar(token, self);
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

procedure TVersiculo.OnCopySyntagmTags(Sender: TObject);
begin
  SintagmaClipboard := TSintagma(FSyntagmPopupMenu.Tag);
end;

procedure TVersiculo.OnPasteSyntagmTags(Sender: TObject);
var
  s: TSintagma;
  t: string;
begin
  if not assigned(SintagmaClipboard) then
    exit;

  s := TSintagma(FSyntagmPopupMenu.Tag);

  s.TextoBruto := s.Texto;
  if TMenuItem(Sender).Caption <> SPasteMorphoTags then
  begin
    s.Strong.Clear;
    for t in SintagmaClipboard.Strong do
    begin
      s.Strong.Add(t);
      s.TextoBruto := Format('%s<W%s>', [s.TextoBruto, t]);
    end;
  end;

  if TMenuItem(Sender).Caption <> SPasteStrongTags then
  begin
    s.Morf.Clear;
    for t in SintagmaClipboard.Morf do
    begin
      s.Morf.Add(t);
      s.TextoBruto := Format('%s<W%s>', [s.TextoBruto, t]);
    end;
  end;

  AtualizarXMLInterno;
  FModificado := true;
  VersiculoPar.Modificado := true;

  LimparSelecao;
  VersiculoPar.LimparSelecao;;
  s.SelecaoMais;

  Renderizar;
end;

procedure TVersiculo.OnSaveTextToFile(Sender: TObject);
begin
  if assigned(FOnExportText) then
     FOnExportText(Self);
end;

procedure TVersiculo.OnVerseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    FVersePopupMenu.PopUp;
end;

procedure TVersiculo.OnResize(Sender: TObject);
begin
  Renderizar;
end;

procedure TVersiculo.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    FPanel.Font.Size := FPanel.Font.Size + Sign(WheelDelta) * 1;
    Renderizar;
  end;
end;

procedure TVersiculo.InitSyntagmPopupMenu;
var
  Item: TMenuItem;
begin
  FSyntagmPopupMenu := TPopupMenu.Create(FPanel);
  FSyntagmPopupMenu.Parent := FPanel;

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SCopyTags;
  Item.OnClick := @OnCopySyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SPasteAllTags;
  Item.OnClick := @OnPasteSyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SPasteStrongTags;
  Item.OnClick := @OnPasteSyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SPasteMorphoTags;
  Item.OnClick := @OnPasteSyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);
end;

procedure TVersiculo.InitVersePopupMenu;
var
  Item: TMenuItem;
begin
  FVersePopupMenu := TPopupMenu.Create(FPanel);
  FVersePopupMenu.Parent := FPanel;
  FVersePopupMenu.Tag := PtrInt(Self);

  Item := TMenuItem.Create(FVersePopupMenu);
  Item.Caption := SSaveToFile;
  Item.OnClick := @OnSaveTextToFile;
  FVersePopupMenu.Items.Add(Item);
end;

constructor TVersiculo.Criar;
begin
  Criar(nil);
end;


end.


