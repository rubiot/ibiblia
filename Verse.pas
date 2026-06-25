unit Verse;

{$mode objfpc}{$H+}

{TODO -cObrigatório: Rever algoritmo de associação/desassociação }

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, StrUtils, ExtCtrls, StdCtrls, Controls, Graphics,
  ONTTokenizer, Syntagm, Forms, LCLType, Math, LCLProc, Dialogs, LazUTF8,
  ONTParser, dbugintf, Menus, Clipbrd, PCRE, ai_debug;

type

  TVerse = class;

  TOnSyntagmEvent = procedure (Sender: TSyntagm) of object;
  TOnVerseChangedEvent = procedure () of object;
  TOnExportTextEvent = procedure (Sender: TVerse) of object;
  TOnSintagmaEvent = TOnSyntagmEvent;
  TOnAlterarVersiculoEvent = TOnVerseChangedEvent;

  TStrongsCountMode = (scNone, scCountWords, scCountStrongs);

  { TVerse }

  TVerse = class
  private
    FReferencia: string;
    FReferenciaID: string;
    FRightToLeft: Boolean;
    { Private declarations }
    FMostrarDicas: boolean;
    FOnAlterarVersiculo: TOnVerseChangedEvent;
    //FStrongMorfoComoChave: boolean;
    FVersiculoRef: TVerse;
    FSintagmas: TSyntagmList;
    FStrongCount: TSyntagm;
    FSelecao: TSyntagmList;
    FModificado: boolean;
    FXMLModificado: boolean;
    FPanel: TScrollBox;
    FEdit: TEdit;
    FAtivo: boolean;
    FDestruindo: boolean;
    FRendering: Boolean;
    FExibirErro: boolean;
    FPalavrasComStrongEmNegrito: boolean;
    FStrongsCountMode: TStrongsCountMode;
    //FFontePadrao: TFont;
    FCorAssociado: TColor;
    FCorDesassociado: TColor;
    FOnSintagmaClick: TOnSyntagmEvent;
    FOnSintagmaMouseEnter: TOnSyntagmEvent;
    FOnSintagmaMouseLeave: TOnSyntagmEvent;
    FOnExportText: TOnExportTextEvent;
    FXML: string;
    FONTParser: TONTParser;
    FTokenizerVersion: Integer;
    FSyntagmPopupMenu: TPopupMenu;
    FVersePopupMenu: TPopupMenu;
    FRTLMenuItem: TMenuItem;
    FFixWrongFiTag: IRegex;
    FReadOnly: Boolean;
    { determines the auto select behaviour on right-click association }
    FNextAutoSelectWordMustHaveStrongMorpho: Boolean;
    // function CompareSyntagmsInFSintagmas(Item1, Item2: Pointer): Integer; // Removed
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
    procedure SetRightToLeft(AValue: Boolean);
    procedure SetStrongsCountMode(AValue: TStrongsCountMode);
    procedure SetPalavrasComStrongEmNegrito(AValue: boolean);
    procedure SetPares(const AValue: string);
    procedure SetTokenizerVersion(AValue: Integer);
    procedure SetTexto(_XML: string);
    function GetTexto: string;
    procedure SetVersiculoPar(Par: TVerse);
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
    procedure OnCopyTokensToClipboard(Sender: TObject);
    procedure OnCopyInterlinearVerseToClipboard(Sender: TObject);
    procedure OnCopyXMLToClipboard(Sender: TObject);
    procedure OnRightToLeft(Sender: TObject);
    procedure OnVerseMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnResize(Sender: TObject);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure InitSyntagmPopupMenu;
    procedure InitVersePopupMenu;
    procedure OnCopyAssociationsJSONToClipboard(Sender: TObject);
    procedure OnCopySintagmasAsJSON(Sender: TObject);
    function CreateTokenizer(const XML: string): TONTTokenizer;
    procedure ReplaceTextPreservingAssociations(const _XML: string; ForceRetokenize: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const AReferencia: string = ''); overload;
    constructor Create(TheOwner: TScrollBox; const AReferencia: string = ''); overload;
    destructor Destroy; override;
    procedure MarkDestroying;
    procedure ClearSyntagms;
    procedure AssociateSyntagms;
    procedure ClearSelection;
    procedure DisassociatePairs;
    procedure ClearAssociations;
    procedure Render;
    procedure RenderStrongCount;
    procedure OrganizeSyntagms;
    procedure SelectSyntagms(list: TSyntagmList);
    procedure ChangeText(_XML: string);
    function GetPairsList(tipo: TPairsListType): TStringList;
    procedure OnSyntagmPopupMenu(s: TSyntagm);
    procedure ShowTags;
    procedure HideTags;
    function GetONTLine: string; overload;
    function GetONTLine(morfo: boolean; autoitalico: boolean; strongsreutilizados: boolean;
      strongsnaotraduzidos: boolean): string; overload;
    procedure LimparSintagmas;
    procedure AssociarSintagmas;
    procedure LimparSelecao;
    procedure DesassociarPares;
    procedure LimparAssociacoes;
    procedure Renderizar;
    procedure RenderizarStrongCount;
    procedure ClearStrongCount;
    procedure OrganizarSintagmas;
    procedure SelecionarSintagmas(list: TSyntagmList);
    procedure AlterarTexto(_XML: string);
    procedure RetokenizePreservingAssociations;
    function GetListaPares(tipo: TPairsListType): TStringList;
    procedure OnSintagmaPopupMenu(s: TSyntagm);
    procedure MostrarTags;
    procedure OcultarTags;
    procedure EnableStrongHighlight(strong: string);
    procedure DisableStrongHighlight;
    function GetAssociationsJSON: string;
    function HasAssociations: boolean;

    procedure FindAndSelectSyntagms(Words: TStringList; Partial: Boolean = False; JoinConnected: Boolean = True);

    function GetTheWordInterlinearLine: string;
    function GetMySwordInterlinearLine: string;
    function GetLinhaONT: string;
    function GetLinhaONT(morfo: boolean; autoitalico: boolean; strongsreutilizados: boolean;
          strongsnaotraduzidos: boolean): string;

        property PairedVerse: TVerse read FVersiculoRef write SetVersiculoPar;
        property Reference: string read FReferencia write FReferencia;
        property ReferenceID: string read FReferenciaID write FReferenciaID;
        property Text: string read GetTexto write SetTexto;
        property Pairs: string read GetPares write SetPares;
        property Panel: TScrollBox read FPanel write FPanel;
        property Syntagms: TSyntagmList read FSintagmas;
        property Selection: TSyntagmList read FSelecao write FSelecao;
        property Modified: boolean read FModificado write SetModificado;
        property XMLModified: boolean read FXMLModificado write FXMLModificado;
        property OnClick: TOnSyntagmEvent read FOnSintagmaClick write FOnSintagmaClick;
        property OnMouseEnter: TOnSyntagmEvent read FOnSintagmaMouseEnter write FOnSintagmaMouseEnter;
        property OnMouseLeave: TOnSyntagmEvent read FOnSintagmaMouseLeave write FOnSintagmaMouseLeave;
        property OnVerseChanged: TOnVerseChangedEvent read FOnAlterarVersiculo write FOnAlterarVersiculo;
        property ShowHints: boolean read FMostrarDicas write FMostrarDicas;
        property Active: boolean read GetAtivo write SetAtivo;
        property PlainText: string read GetTextoSimples;
        property AssociatedColor: TColor read FCorAssociado write FCorAssociado;
        property UnassociatedColor: TColor read FCorDesassociado write FCorDesassociado;
        property AssociationProgress: Single read GetAndamentoAssociacao;
        property Font: TFont read GetFonte write SetFonte;
        property BoldWordsWithStrong: boolean read FPalavrasComStrongEmNegrito write SetPalavrasComStrongEmNegrito;
        property VersiculoPar: TVerse read FVersiculoRef write SetVersiculoPar;
    property Referencia: string read FReferencia write FReferencia;
    property ReferenciaID: string read FReferenciaID write FReferenciaID;
    property XML: string read FXML;
    property Texto: string read GetTexto write SetTexto;
    property Pares: string read GetPares write SetPares;
    property Painel: TScrollBox read FPanel write FPanel;
    property VersePopupMenu: TPopupMenu read FVersePopupMenu;
    property Sintagmas: TSyntagmList read FSintagmas;
    property Selecao: TSyntagmList read FSelecao write FSelecao;
    property Modificado: boolean read FModificado write SetModificado;
    property XMLModificado: boolean read FXMLModificado write FXMLModificado;
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
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property RightToLeft: Boolean read FRightToLeft write SetRightToLeft default False;
    property NextAutoSelectWordMustHaveStrongMorpho: boolean read FNextAutoSelectWordMustHaveStrongMorpho write FNextAutoSelectWordMustHaveStrongMorpho;
    property TokenizerVersion: Integer read FTokenizerVersion write SetTokenizerVersion;
     property Destroying: boolean read FDestruindo;
 published
    { Published declarations }
  end;

resourcestring
  SCopyTags ='&Copy tags';
  SPasteAllTags = 'Paste &all tags';
  SPasteStrongTags = '&Paste &Strong''s tags';
  SPasteMorphoTags = 'Paste &morphology tags';
  SSaveToFile = '&Save text to file...';
  SRightToLeft = 'Right to left text';
  SCopyTokensToClipboard = 'Copy &tokens to clipboard';
  SCopySyntagmasJSONToClipboard = 'Copy syntagms as &JSON to clipboard';
  SCopyInterlinearVerseToClipboard = 'Copy &interlinear verse to clipboard';
  SCopyXMLToClipboard = 'Copy association &XML to clipboard';
  SCopyAssociationsJSONToClipboard = 'Copy association &JSON to clipboard';

implementation

uses
  TypInfo, SyntagmSelection;

const
  MarginWidth = 10; // text's distance from the margin

var
  SyntagmClipboard: TSyntagm;

function CompareIndexes(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := List[Index1].ToInteger - List[Index2].ToInteger;
end;

{ TVerse }

constructor TVerse.Create(TheOwner : TScrollBox; const AReferencia: string = '');
begin
  AIDebugLogFmt('[TVerse] Create called. Self=%s, Owner=%s, Reference=%s',
    [IntToHex(PtrUInt(Pointer(Self)), SizeOf(Pointer)*2),
     IntToHex(PtrUInt(Pointer(TheOwner)), SizeOf(Pointer)*2),
     AReferencia]);
  FReferencia := AReferencia;
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
  FSelecao              := TSyntagmList.Create;
  FMostrarDicas         := false;
  FNextAutoSelectWordMustHaveStrongMorpho := true;
  FCorAssociado         := clWindowText;
  FCorDesassociado      := $A3A3A3;
  FDestruindo           := false;
  FRendering            := false;
  FTokenizerVersion := TTokenizerFactory.PreferredVersion;
  FONTParser := TONTParser.Create(FTokenizerVersion);

  { regex to fix cases in which a <Fi> tag ends up misplaced }
  FFixWrongFiTag := RegexCreate('((?:<W[THG][^>]+>)+)(<Fi>)', [rcoUTF8]);
end;

destructor TVerse.Destroy;
begin
  AIDebugLogFmt('[TVerse] Destroy called. Self=%s, Panel=%s, Reference=%s',
    [IntToHex(PtrUInt(Pointer(Self)), SizeOf(Pointer)*2),
     IntToHex(PtrUInt(Pointer(FPanel)), SizeOf(Pointer)*2),
     FReferencia]);
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
  inherited Destroy;
end;

procedure TVerse.MarkDestroying;
begin
  FDestruindo := true;
end;

procedure TVerse.LimparSintagmas;
var
  s: TSyntagm;
begin
  if assigned(FSintagmas) then
  begin
    if FAtivo and assigned(FPanel) then
      FPanel.DisableAutoSizing;

    for s in FSintagmas do
      s.Destroy;

    ClearStrongCount;
    FSintagmas.Clear;

    if FAtivo and assigned(FPanel) then
      FPanel.EnableAutoSizing;
  end;
  FSelecao.Clear;
end;

procedure TVerse.AssociarSintagmas;
var
  s: TSyntagm;
begin
  if not Assigned(VersiculoPar) then
    exit;

  for s in Selecao do
  begin
    with s do
    begin
      Pairs.Clear;
      Pairs.AddList(VersiculoPar.Selecao);
      IsAssociated:=true;
      Siblings.Clear;
      Siblings.AddList(Selecao);
      Siblings.Remove(s);
    end;
  end;
  for s in VersiculoPar.Selecao do
  begin
    with s do
    begin
      Pairs.Clear;
      Pairs.AddList(Selecao);
      IsAssociated:=true;
      Siblings.Clear;
      Siblings.AddList(VersiculoPar.Selecao);
      Siblings.Remove(s);
    end;
  end;

  Modificado := true;
  VersiculoPar.Modificado := true;
end;

procedure TVerse.LimparSelecao;
begin
  while not Selecao.Empty do
    TSyntagm(Selecao.First).RemoveFromSelection;
end;

procedure TVerse.DesassociarPares;
var
  s: TSyntagm;
begin
  LimparSelecao;
  VersiculoPar.LimparSelecao;

  for s in FSintagmas do
    s.Disassociate;

  for s in VersiculoPar.FSintagmas do
    s.Disassociate;

  Renderizar;
  VersiculoPar.Renderizar;
end;

procedure TVerse.LimparAssociacoes;
var
  s: TSyntagm;
begin
  for s in FSintagmas do
    if not s.Pairs.Empty then
    begin // desassociar somente se houver associacoes, para evitar falsos 'modificado = true'
      DesassociarPares;
      Modificado := true;
      break;
    end;
end;

procedure TVerse.SelecionarSintagmas(list: TSyntagmList);
var
  s: TSyntagm;
begin
  for s in list do
    s.AddToSelection;
end;

function TVerse.GetListaPares(tipo: TPairsListType): TStringList;
var
  src, dst: string;
  s, p: TSyntagm;
  tmp: TSyntagmList;
begin
  result := TStringList.Create;

  tmp := TSyntagmList.Create;
  for s in FSintagmas do
  begin
    if not assigned(s.Pairs) or (s.Kind <> tsSintagma) or s.IsItalic or (tmp.IndexOf(s) >= 0) then
      continue;

    src := s.GetSuggestionKey(tipo);
    tmp.Add(s);
    for p in s.Siblings do
    begin
      if p.IsItalic or (p.Kind <> tsSintagma) then
        continue;
      src := src + ';' + p.GetSuggestionKey(tipo);
      tmp.Add(p);
    end;
    dst := '';
    for p in s.Pairs do
    begin
      if p.IsItalic or (p.Kind <> tsSintagma) then
        continue;
      dst := dst + p.GetSuggestionKey(tipo) + ';'
    end;
    if not src.IsEmpty and not dst.IsEmpty then
    begin
      result.Add(src);
      result.Add(dst.TrimRight(';'));
    end;
  end;
  tmp.Destroy;
end;

procedure TVerse.OnSintagmaPopupMenu(s: TSyntagm);
var
  p: TPoint;
begin
  FSyntagmPopupMenu.Tag := PtrInt(s);
  p := s.LabelRef.ClientToScreen(s.LabelRef.ClientRect.BottomRight);
  FSyntagmPopupMenu.PopUp(p.x, p.y);
end;

procedure TVerse.MostrarTags;
var
  s: TSyntagm;
  p: TPoint;
  tags: string;
begin
  if not FAtivo then
    exit;

  for s in FSintagmas do
  begin
    if not assigned(s.LabelRef) or not s.HasStrongs then
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

procedure TVerse.OcultarTags;
begin
  if FAtivo then
    FPanel.Refresh;
end;

procedure TVerse.EnableStrongHighlight(strong: string);
var
  s: TSyntagm;
begin
  for s in FSintagmas do
    s.HighlightStrong(strong);
end;

procedure TVerse.DisableStrongHighlight;
var
  s: TSyntagm;
begin
  for s in FSintagmas do
    s.ToggleStrongHighlight(false);
end;

procedure TVerse.SetTexto(_XML: string);
begin
  FXML := _XML;
  LimparSintagmas;
  FSintagmas := FONTParser.ParseLine(FXML, self);
  Renderizar;
  Modificado := true;
  FXMLModificado := true;
  SyntagmClipboard := nil;
end;

{ Replaces the verse text trying to keep existing associations }
procedure TVerse.ReplaceTextPreservingAssociations(const _XML: string;
  ForceRetokenize: Boolean);

  procedure DeleteSyntagm(i: integer; newlist: TSyntagmList);
  var
    s: TSyntagm;
  begin
    s := FSintagmas[i];
    s.Disassociate;
    s.Destroy;
    FSintagmas.Delete(i);
  end;

var
  new, result: TSyntagmList;
  s, old: TSyntagm;
  found, oldActive: boolean;
begin
  if (not ForceRetokenize) and (FXML = _XML) then
    exit;

  oldActive := FAtivo;
  FAtivo := false; // we're potentially deleting labels

  new := FONTParser.ParseLine(_XML, self);

  LimparSelecao;

  if FSintagmas.Empty then
    FSintagmas := new
  else
  begin
    result := TSyntagmList.Create;

    while not (new.Empty and FSintagmas.Empty) do
    begin
      if new.Empty then
        DeleteSyntagm(0, result)
      else if FSintagmas.Empty then
      begin
       result.Add(new[0]);
       new.Delete(0);
      end
      else if FSintagmas.First.IsEqualTo(new.First) then
      begin
        with FSintagmas.First do
        begin
          if Kind = tsSintagma then
          begin
            Strong.Free;
            Morph.Free;
            Strong := new.First.Strong;
            Morph   := new.First.Morph;
            new.First.Strong := nil;
            new.First.Morph   := nil;
          end;
        end;
        result.Add(FSintagmas.First);
        FSintagmas.Delete(0);
        new.First.Destroy;
        new.Delete(0);
      end
      else if FSintagmas.First.Kind <> tsSintagma then
        DeleteSyntagm(0, result)
      else if new.First.Kind <> tsSintagma then
      begin
        result.Add(new[0]);
        new.Delete(0);
      end
      else
      begin
        { checking if the old syntagm will be used in the future }
        found := false;
        old := FSintagmas.First;
        if not old.Pairs.Empty then // does it have associations?
        begin
          for s in new do
            if old.IsEqualTo(s) then
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
  SyntagmClipboard := nil;

  FAtivo := oldActive;
  Renderizar;
end;

procedure TVerse.AlterarTexto(_XML: string);
begin
  ReplaceTextPreservingAssociations(_XML, False);
end;

procedure TVerse.RetokenizePreservingAssociations;
begin
  ReplaceTextPreservingAssociations(FXML, True);
end;

function TVerse.GetTexto: string;
var
  linha: TStringStream;
  s: TSyntagm;
begin
  result := '';
  try
    linha := TStringStream.Create('');
    for s in FSintagmas do
      linha.WriteString(s.Text);
  finally
    result := linha.DataString;
    linha.Destroy;
  end;
end;

function TVerse.CreateTokenizer(const XML: string): TONTTokenizer;
begin
  Result := TTokenizerFactory.CreateTokenizer(FTokenizerVersion, XML);
end;

procedure TVerse.SetPares(const AValue: string);
var
  varredorXML: TONTTokenizer;
  s: TTagSintagma;
begin
  if not assigned(VersiculoPar) or AValue.IsEmpty then
    exit;

  FExibirErro := true;
  VersiculoPar.FExibirErro := true;

  LimparAssociacoes;

  varredorXML := CreateTokenizer(AValue);
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
  varredorXML.Destroy;

  Renderizar;
  VersiculoPar.Renderizar;
end;

procedure TVerse.SetTokenizerVersion(AValue: Integer);
begin
  AValue := TTokenizerFactory.NormalizeVersion(AValue);
  if FTokenizerVersion = AValue then
    Exit;

  FTokenizerVersion := AValue;
  if Assigned(FONTParser) then
    FONTParser.TokenizerVersion := AValue;
end;

function TVerse.GetPares: string;
var
  b: integer;
  s, p: TSyntagm;
  _xml: TStringStream;
  tmp: TSyntagmList;
begin
  _xml := TStringStream.Create('');

  // gerar tags de Texto <p a="1,2,3" b="1,2,3">
  tmp := TSyntagmList.Create;
  for s in FSintagmas do
  begin
    if not s.Pairs.Empty and (tmp.IndexOf(s) < 0) then
    begin
      _xml.WriteString(Format('<par a="%d', [FSintagmas.IndexOf(s)]));
      tmp.Add(s);
      for p in s.Siblings do
      begin
        _xml.WriteString(Format(',%d', [FSintagmas.IndexOf(p)]));
        tmp.Add(p);
      end;
      _xml.WriteString('" b="');
      for p in s.Pairs do
      begin
        b := FVersiculoRef.Sintagmas.IndexOf(p);
        if b < 0 then
           raise exception.Create('Pair not found in other verse');
        _xml.WriteString(Format('%d', [b]));
        if s.Pairs.IndexOf(p) <> s.Pairs.Count-1 then
          _xml.WriteString(',');
      end;
      _xml.WriteString('">');
    end;
  end;
  tmp.Destroy;

  result := _xml.DataString;
  _xml.Destroy;
end;

function TVerse.GetTextoSimples: string;
var
  linha: TStringStream;
  s: TSyntagm;
begin
  result := '';
  if not Assigned(FSintagmas) then
    Exit;

  linha := TStringStream.Create('');
  try
    for s in FSintagmas do
      if s.Kind in [tsEspaco, tsSintagma, tsPontuacao] then
        linha.WriteString(s.Text);
    result := linha.DataString;
  finally
    linha.Destroy;
  end;
end;

function TVerse.GetAtivo: boolean;
begin
  result := FAtivo;
end;

function TVerse.GetFonte: TFont;
begin
  if FAtivo then
    result := FPanel.Font
  else
    result := nil;
end;

function TVerse.GetAndamentoAssociacao: Single;
var
  s: TSyntagm;
  t, p: smallint;
begin
  t := 0;
  p := 0;
  result := 0;

  for s in FSintagmas do
    if s.Kind = tsSintagma then
    begin
      inc(t);
      if s.IsAssociated then
        inc(p);
    end;

  if t > 0 then
    result := p/t;
end;

function TVerse.GetTheWordInterlinearLine: string;

  function GetSpacingBefore(syn, pair: TSyntagm): string;
  var
    i: integer;
  begin
    result := '';
    if pair = syn.Pairs[0] then exit;
    for i:=FVersiculoRef.Sintagmas.IndexOf(pair)-1 downto 0 do
    begin
      if FVersiculoRef.Sintagmas[i].Kind = tsSintagma then
        break;
      if FVersiculoRef.Sintagmas[i].Kind = tsEspaco then
      begin
        result := ' ';
        break;
      end;
    end;
  end;

var
  linha: TStringStream;
  s, p, prox: TSyntagm;
  m: string;
  n: integer;
begin
  result := '';

  try
    linha := TStringStream.Create('');

    for s in FSintagmas do
    begin
      linha.WriteString(s.Text);
      if (assigned(s.Strong) and (s.Strong.Count > 0)) or (assigned(s.Morph) and (s.Morph.Count > 0)) then
      begin // se este texto contém strongs
        for m in s.Strong do // strongs
          linha.WriteString(format('<W%s>', [m]));
        for m in s.Morph do // morfologia
          linha.WriteString(format('<WT%s>', [m]));
      end else
      begin // caso contrário, tentemos o texto relacionado
        for p in s.Pairs do
        begin
          for m in p.Strong do // strongs
            linha.WriteString(format('<W%s>', [m]));
          for m in p.Morph do   // morfologia
            linha.WriteString(format('<WT%s>', [m]));
        end;
      end;

      if (FSintagmas.IndexOf(s) < (FSintagmas.Count-1)) and // não é o último sintagma e
         (s.Siblings.Count > 0) then   // tem irmãos
      begin
        prox := nil;
        for n:=FSintagmas.IndexOf(s)+1 to Sintagmas.Count-1 do
        begin // procurando o próximo sintagma (saltando espaços, pontuação, etc.)
          if Sintagmas[n].Kind = tsSintagma then
          begin
            prox := Sintagmas[n];
            break;
          end;
        end;
        if (prox <> nil) and (s.Siblings.IndexOf(prox) >= 0) then // o próximo sintagma é irmão deste?
          continue;
      end;

      for p in s.Pairs do // pares
        linha.WriteString(GetSpacingBefore(s, p) + '<sup>' + IfThen(p.IsItalic, format('<FI>%s<Fi>', [p.Text]), p.Text) + '</sup>');
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

function TVerse.GetMySwordInterlinearLine: string;

  function GetSpacingBefore(syn, pair: TSyntagm): string;
  var
    i: integer;
  begin
    result := '';
    if pair = syn.Pairs[0] then exit;
    for i:=FVersiculoRef.Sintagmas.IndexOf(pair)-1 downto 0 do
    begin
      if FVersiculoRef.Sintagmas[i].Kind = tsSintagma then
        break;
      if FVersiculoRef.Sintagmas[i].Kind = tsEspaco then
      begin
        result := ' ';
        break;
      end;
    end;
  end;

  function TestAndUnset(out flag: boolean): Boolean;
  begin
    result := flag;
    if flag then
      flag := false;
  end;

  function CollectConsecutiveSiblings(startSyntagm: TSyntagm; var skipIndices: TList): string;
  var
    currentIdx, nextIdx, lookAheadIdx: Integer;
    currentSyn, nextSyn, lookAheadSyn: TSyntagm;
    combinedText: string;
    foundSpace: Boolean;
  begin
    combinedText := startSyntagm.Text;
    currentIdx := FSintagmas.IndexOf(startSyntagm);

    // If we have siblings, check if any are consecutive
    if startSyntagm.Siblings.Count = 0 then
    begin
      Result := combinedText;
      Exit;
    end;

    // Check next syntagms to find consecutive siblings
    nextIdx := currentIdx + 1;
    while nextIdx < FSintagmas.Count do
    begin
      nextSyn := FSintagmas[nextIdx];

      // Add any spaces between siblings
      if nextSyn.Kind = tsEspaco then
      begin
        combinedText := combinedText + nextSyn.Text;
        skipIndices.Add(Pointer(nextIdx));
        Inc(nextIdx);
        continue;
      end
      // Handle '<wt>' tags between siblings - look ahead to see if followed by a sibling
      else if (nextSyn.Kind = tsMetaDado) and (nextSyn.Text = '<wt>') then
      begin
        // Look ahead to find next syntagm after the tag
        lookAheadIdx := nextIdx + 1;
        lookAheadSyn := nil;

        while (lookAheadIdx < FSintagmas.Count) do
        begin
          if FSintagmas[lookAheadIdx].Kind = tsSintagma then
          begin
            lookAheadSyn := FSintagmas[lookAheadIdx];
            break;
          end;
          Inc(lookAheadIdx);
        end;

        // Only add the '<wt>' tag if followed by a sibling
        if (lookAheadSyn <> nil) and (startSyntagm.Siblings.IndexOf(lookAheadSyn) >= 0) then
        begin
          combinedText := combinedText + nextSyn.Text;
          skipIndices.Add(Pointer(nextIdx));
        end;

        Inc(nextIdx);
        continue;
      end
      // If next syntagm is not a sibling or not a syntagm, we're done
      else if (nextSyn.Kind <> tsSintagma) or
              (startSyntagm.Siblings.IndexOf(nextSyn) < 0) then
        break;

      // Found consecutive sibling, add it to result
      combinedText := combinedText + nextSyn.Text;
      skipIndices.Add(Pointer(nextIdx));
      Inc(nextIdx);
    end;

    Result := combinedText;
  end;

var
  line: TStringStream;
  s, p, nextSyntagm: TSyntagm;
  m: string;
  skip: boolean;
  skipIndices: TList;
  i, currentIndex: Integer;
  combinedText: string;
begin
  result := '';

  {Sample: <TS>The Creation<Ts><Q><wg>εν<WG1722><E> In<e><q> <Q><wg>αρχή<WG746><E> <FI>the<Fi> beginning<e><q>}
  skipIndices := TList.Create;
  try
    line := TStringStream.Create('');

    skip := false;
    i := 0;
    while i < FSintagmas.Count do
    begin
      if skipIndices.IndexOf(Pointer(i)) >= 0 then
      begin
        Inc(i);
        continue;
      end;

      s := FSintagmas[i];

      if TestAndUnset(skip) then
      begin
        Inc(i);
        continue;
      end;

      case s.Kind of
        tsSintagma:
        begin
          // Collect text from consecutive siblings
          combinedText := CollectConsecutiveSiblings(s, skipIndices);

          line.WriteString('<Q>');
          // Use non-breakable spaces in the text
          line.WriteString(combinedText);

          // Output Strong's numbers and morphology for this syntagm
          for m in s.Strong do
            line.WriteString(format('<W%s>', [m]));
          for m in s.Morph do
            line.WriteString(format('<WT%s>', [m]));

          if assigned(s.GetNext) and (s.GetNext.Kind = tsPontuacao) then
          begin
            line.WriteString(s.GetNext.Text);
            skip := true;
          end;

          if s.Pairs.Count > 0 then
          begin
            line.WriteString('<T>');
            if (s.Siblings.Count = 0) or (i < FSintagmas.IndexOf(s.Siblings[0])) then
            begin // no siblings or is the first sibling
              for p in s.Pairs do
              begin
                line.WriteString(GetSpacingBefore(s, p));
                // Also use non-breakable spaces in the translated text
                if p.IsItalic then
                  line.WriteString(Format('<FI>%s<Fi>', [p.Text]))
                else
                  line.WriteString(p.Text);
              end;
            end else // not a first sibling
              line.WriteString('←');
            line.WriteString('<t>');
          end
          else
            line.WriteString('<T>•<t>');

          line.WriteString('<q>');
        end;
        tsTag:       line.WriteString(s.RawText);
        tsEspaco:    line.WriteString(s.RawText);
        tsPontuacao: line.WriteString(s.RawText);
        tsMetaDado:  if s.Text <> '<wt>' then line.WriteString(s.RawText);
      end;

      Inc(i);
    end;

    result := line.DataString;
  finally
    skipIndices.Free;
    line.Free;
  end;
end;

function TVerse.GetLinhaONT: string;
begin
  result := GetLinhaONT(true, false, false, false);
end;

function TVerse.GetLinhaONT(morfo: boolean; autoitalico: boolean; strongsreutilizados: boolean;
      strongsnaotraduzidos: boolean): string;
var
  linha: TStringStream;
  stg, prox: TSyntagm;
  s, p, m: smallint;
  ls: TList;
  temp: string;
  label NextSyntagm;
begin
  result := '';
  ls := TList.Create; // usado para armazenar os Strongs utilizados no versículo
  prox := nil;

  try
    linha := TStringStream.Create('');

    s := -1;
    for stg in FSintagmas do
    begin // para cada sintagma, computa sua saída em uma variável temporária
      Inc(s);
      temp := '';

      if autoitalico and (stg.Pairs.Count = 0) and (stg.Kind = tsSintagma) then
        temp := temp + '<FI>';

      if autoitalico and (stg.Kind = tsTag) and ((stg.Text = '<FI>') or (stg.Text = '<Fi>')) then
        goto NextSyntagm; // quando auto-itálico, ignorar os itálicos originais

      temp := temp + stg.Text;

      if autoitalico and (stg.Pairs.Count = 0) and (stg.Kind = tsSintagma) then
        temp := temp + '<Fi>';

      if (stg.Pairs.Count > 0) and (stg.Kind = tsSintagma) then
      begin
        if (s < (Sintagmas.Count-1)) and // não é o último sintagma e
           (stg.Siblings.Count > 0) then // tem irmãos
        begin
          prox := nil;
          for m := s+1 to Sintagmas.Count-1 do
          begin // procurando o próximo sintagma (saltando espaços, pontuação, etc.)
            if Sintagmas[m].Kind = tsSintagma then
            begin
              prox := Sintagmas[m];
              break;
            end;
          end;
          if (prox <> nil) and (stg.Siblings.IndexOf(prox) >= 0) then // o próximo sintagma é irmão deste?
            goto NextSyntagm;
        end;

        prox := nil;
        for p := 0 to stg.Pairs.Count-1 do
        begin // pares
          for m := 0 to stg.Pairs[p].Strong.Count-1 do // strongs
          begin
            if (not strongsreutilizados) or (ls.IndexOf(stg.Pairs[p].Strong) = -1) then // este Strong já foi utilizado antes?
            begin // não
              temp := temp + format('<W%s>', [stg.Pairs[p].Strong[m]]);
              ls.Add(stg.Pairs[p].Strong)
            end else // strong reutilizado
              temp := temp + format('<W%ss>', [stg.Pairs[p].Strong[m]]);
          end;

          if not morfo then
            continue;

          for m := 0 to stg.Pairs[p].Morph.Count-1 do // morfologia
            temp := temp + format('<WT%s>', [stg.Pairs[p].Morph[m]]);
        end;
      end;

      NextSyntagm:
        // escreve o resultado do sintagma em 'linha' após computá-lo integralmente
        if temp.Contains('<W') then // se houver strongs ou morfologia, então é um sintagma
          linha.WriteString('<wt>');
        linha.WriteString(temp);
    end;

    if strongsnaotraduzidos then
    begin
      // verificando se ficaram palavras do original que não foram traduzidas
      for s := 0 to VersiculoPar.Sintagmas.Count-1 do
      begin
      stg := VersiculoPar.Sintagmas[s];
      if assigned(stg.Strong) and (ls.IndexOf(stg.Strong) = -1) then
        for m := 0 to stg.Strong.Count-1 do // strongs
        linha.WriteString(format('<W%sx>', [stg.Strong[m]]));
      end;
    end;

    // handling special cases for italics
    result := FFixWrongFiTag.Replace(linha.DataString, '$2$1'); { fix '<wt>bla <FI>ble<WG1><Fi>' case }
    result := result.Replace('<FI><wt>', '<wt><FI>', [rfReplaceAll]);
  finally
    linha.Destroy;
    ls.Destroy;
  end;
end;

procedure TVerse.SetAtivo(const AValue: boolean);
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

procedure TVerse.SetFonte(const AValue: TFont);
begin
  if not FAtivo then
    exit;
  FPanel.Font := AValue;
  Renderizar;
end;

procedure TVerse.SetModificado(const AValue: boolean);
begin
  if FModificado = AValue then exit;
  FModificado := AValue;
  if assigned(FOnAlterarVersiculo) and FModificado then
     FOnAlterarVersiculo;
end;

procedure TVerse.SetRightToLeft(AValue: Boolean);
begin
  if FRightToLeft=AValue then Exit;
  FRightToLeft:=AValue;
  FRTLMenuItem.Checked := FRightToLeft;
  Renderizar;
end;

procedure TVerse.SetStrongsCountMode(AValue: TStrongsCountMode);
begin
  if FStrongsCountMode = AValue
     then Exit;
  FStrongsCountMode := AValue;
  Renderizar;
end;

procedure TVerse.SetPalavrasComStrongEmNegrito(AValue: boolean);
begin
  if FPalavrasComStrongEmNegrito=AValue then Exit;
  FPalavrasComStrongEmNegrito:=AValue;
  Renderizar;
end;

procedure TVerse.OrganizarSintagmas;

  function StartingXPos: smallint;
  begin
    if FRightToLeft then
      result := Painel.Width-MarginWidth
    else
      result := MarginWidth;
  end;

  function WrapLine(s: TSyntagm; x: smallint): boolean;
  var
    next, prev: TSyntagm;
    punctWidth: smallint;
  begin
    result := false;
    prev := s.GetPrevVisible;
    if (s.Kind = tsPontuacao) and
       (not assigned(prev) or (prev.Kind <> tsPontuacao)) then // and not following another punctiation
       exit;

    next := s.GetNextVisible;
    if assigned(next) and (next.Kind = tsPontuacao) then
      punctWidth := next.LabelRef.Width
    else
      punctWidth := 0;

    if FRightToLeft then
      result := (x - punctWidth) < 15
    else
      result := (x + s.LabelRef.Width + punctWidth) > (Painel.Width-15);
  end;

  procedure PositionSyntagm(s: TSyntagm; var x, y, a: smallint);
  var
    firstLabel: boolean;
  begin
    a := max(a, s.LabelRef.Height);

    firstLabel := x = StartingXPos;
    if FRightToLeft then
      dec(x, s.LabelRef.Width);

    if WrapLine(s, x) then
    begin
      inc(y, a);
      a := 0;
      x := IfThen(FRightToLeft, StartingXPos-s.LabelRef.Width, StartingXPos);
      firstLabel := true;
    end;

    s.LabelRef.SetBounds(x, y, s.LabelRef.Width, s.LabelRef.Height);

    if firstLabel and (s.Kind = tsEspaco) then
      s.LabelRef.Visible := false // hiding spaces at the beginning of the line
    else begin
      s.LabelRef.Show;
      if not FRightToLeft then
        inc(x, s.LabelRef.Width);
    end;
  end;

var
  syntagm: TSyntagm;
  x, y, h: smallint;
begin
  if not Ativo or FDestruindo or not assigned(FSintagmas) then
    exit;

  x := StartingXPos; // starting x position
  y := 0;            // starting y position
  h := 0;            // line height
  for syntagm in FSintagmas do
    if assigned(syntagm.LabelRef) then
      PositionSyntagm(syntagm, x, y, h);

  if (FStrongsCountMode <> scNone) and assigned(FStrongCount) then
    PositionSyntagm(FStrongCount, x, y, h);
end;

procedure TVerse.SetVersiculoPar(Par: TVerse);
begin
  FVersiculoRef := Par;
  if (Par.VersiculoPar <> self) then
    Par.VersiculoPar := self;
end;

procedure TVerse.SelecionarListaSintagmas(lst: string);
var
  tokens: TStringList;
  i: Integer;
  idx: Integer;
  ok: Boolean;
begin
  tokens := TStringList.Create;
  try
    tokens.DelimitedText := lst;
    tokens.CustomSort(@CompareIndexes);
    for i := 0 to tokens.Count - 1 do
    begin
      ok := TryStrToInt(tokens[i], idx);
      if not ok then
      begin
        //AIDebugLogFmt('[SelecionarListaSintagmas] Invalid token (not integer): %s', [tokens[i]]);
        Continue;
      end;
      if (idx < 0) or (idx >= Sintagmas.Count) then
      begin
        //AIDebugLogFmt('[SelecionarListaSintagmas] Index out of range: %d (Count=%d)', [idx, Sintagmas.Count]);
        Continue;
      end;
      if not (Sintagmas[idx].Kind in [tsSintagma, tsPontuacao]) then
      begin
        //AIDebugLogFmt('[SelecionarListaSintagmas] Index %d is not selectable (Kind=%d)', [idx, Ord(Sintagmas[idx].Kind)]);
        Continue;
      end;
      Sintagmas[idx].AddToSelection;
    end;
  finally
    tokens.Free;
  end;
end;

procedure TVerse.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    FEdit.Visible := False
  else if Key = VK_RETURN then
    EditConfirm;
end;

procedure TVerse.EditExit(Sender: TObject);
begin
  FEdit.Visible := False;
end;

procedure TVerse.EditConfirm;
{var
  s: TTagSintagma;
  varredorXML: TONTTokenizer;
  sintagma, novo: TSyntagm;
  i, offset, j: integer;}
begin
  {
  sintagma := TSyntagm(FEdit.Tag);
  i := FSintagmas.IndexOf(sintagma);
  FSintagmas.Remove(sintagma);
  FSelecao.Remove(sintagma);
  offset := 0;
  novo := nil;

  varredorXML := TTokenizerFactory.CreateTokenizer(FTokenizerVersion, FEdit.Caption);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if (s.tipo = tsEspaco) and (s.valor = '|') then
    begin
      if assigned(novo) then
        novo.TextoBruto := novo.TextoBruto + s.valor;
      continue;
    end;

    novo := TSyntagm.Create(self, s);
    FSintagmas.Insert(i, novo);
    Inc(i); Inc(Offset);
  end;
  varredorXML.Destruir;

  // copiando dados do sintagma clicado para o primeiro sintagma parseado
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
procedure TVerse.AtualizarXMLInterno;
var
  s: TSyntagm;
begin
  FXML := '';
  for s in FSintagmas do
    FXML := FXML + s.RawText;

  FXMLModificado := true;
  FModificado := true;
end;

procedure TVerse.Renderizar;
var
  s: TSyntagm;
  //starttime: DWord;
begin
  if not FAtivo or not assigned(FSintagmas) or FRendering then
    exit;

  //starttime := getTickCount;
  FRendering := True;
  try
    if assigned(FPanel) then
      FPanel.DisableAutoSizing;
    for s in FSintagmas do
      s.Render;
    RenderizarStrongCount;
    if assigned(FPanel) then
      FPanel.EnableAutoSizing;
    OrganizarSintagmas;
  finally
    FRendering := False;
  end;
  //DebugLn('  TVerse.Render: %d milliseconds', [getTickCount-starttime]);
end;

procedure TVerse.RenderizarStrongCount;
begin
  ClearStrongCount;
  if FStrongsCountMode = scNone then
    exit;
  if FSintagmas.Empty then
    exit;

  AtualizarStrongCount;
  FStrongCount.Render;
end;

procedure TVerse.ClearStrongCount;
begin
  if not assigned(FStrongCount) then
    exit;

  FStrongCount.Destroy;
  FStrongCount := nil;
end;

procedure TVerse.AtualizarStrongCount;

  function GetStrongsCount(s: TSyntagm): integer;
  begin
    case FStrongsCountMode of
      scCountStrongs:
        result := s.StrongsCount;
      scCountWords:
        result := s.StrongsWordsCount;
    else
      result := 0;
    end;
  end;

var
  token: TTagSintagma;
  s, p: TSyntagm;
  count: integer;
  unique: TSyntagmList;
begin
  count := 0;
  unique := TSyntagmList.Create;
  for s in FSintagmas do
  begin
    if s.Kind <> tsSintagma then
      continue;
    if unique.IndexOf(s) <> -1 then
      continue;
    inc(count, GetStrongsCount(s));
    if not s.HasStrongs and assigned(s.Siblings) then
      for p in s.Siblings do
        if not p.HasStrongs then
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
  FStrongCount := TSyntagm.Create(token, self);
end;

function TVerse.GetTokens: string;
var
  s: TSyntagm;
  tokens: TStringStream;
begin
  tokens := TStringStream.Create('');
  for s in FSintagmas do
    tokens.WriteString(s.Gist);
  result:= tokens.DataString;
  tokens.Destroy;
end;

procedure TVerse.FindAndSelectSyntagms(Words: TStringList; Partial: Boolean = False; JoinConnected: Boolean = True);
var
  I: Integer;
  SelectedIndexes: TSyntagmIndexArray;

begin
  LimparSelecao;

  if not Assigned(Words) or (Words.Count = 0) then
    Exit;

  AIDebugLogFmt('[FindAndSelectSyntagms] Verse: %s - Searching for %d words, Partial=%s, JoinConnected=%s',
                [Referencia, Words.Count, BoolToStr(Partial, True), BoolToStr(JoinConnected, True)]);
  for I := 0 to Words.Count - 1 do
    AIDebugLogFmt('[FindAndSelectSyntagms]   Word[%d]: "%s"', [I, Words[I]]);
  AIDebugLogFmt('[FindAndSelectSyntagms] Total syntagms in verse: %d', [FSintagmas.Count]);

  SelectedIndexes := ResolveSyntagmSelectionIndexes(FSintagmas, Words, Partial, JoinConnected);
  AIDebugLogVerboseFmt('[FindAndSelectSyntagms] Resolved %d syntagm indexes', [Length(SelectedIndexes)]);

  for I := 0 to High(SelectedIndexes) do
  begin
    if (SelectedIndexes[I] < 0) or (SelectedIndexes[I] >= FSintagmas.Count) then
      Continue;

    AIDebugLogVerboseFmt('[FindAndSelectSyntagms]   Adding index %d to selection: "%s"',
      [SelectedIndexes[I], FSintagmas[SelectedIndexes[I]].Text]);
    FSintagmas[SelectedIndexes[I]].AddToSelection;
  end;

  AIDebugLogVerbose('[FindAndSelectSyntagms] Completed successfully');
end;

procedure TVerse.OnCopySyntagmTags(Sender: TObject);
begin
  SyntagmClipboard := TSyntagm(FSyntagmPopupMenu.Tag);
end;

procedure TVerse.OnPasteSyntagmTags(Sender: TObject);
var
  s: TSyntagm;
  t: string;
begin
  if not assigned(SyntagmClipboard) then
    exit;

  s := TSyntagm(FSyntagmPopupMenu.Tag);

  s.RawText := s.Text;
  if TMenuItem(Sender).Caption <> SPasteMorphoTags then
  begin
    s.Strong.Clear;
    for t in SyntagmClipboard.Strong do
    begin
      s.Strong.Add(t);
      s.RawText := Format('%s<W%s>', [s.RawText, t]);
    end;
  end;

  if TMenuItem(Sender).Caption <> SPasteStrongTags then
  begin
    s.Morph.Clear;
    for t in SyntagmClipboard.Morph do
    begin
      s.Morph.Add(t);
      s.RawText := Format('%s<W%s>', [s.RawText, t]);
    end;
  end;

  AtualizarXMLInterno;
  FModificado := true;
  VersiculoPar.Modificado := true;

  LimparSelecao;
  VersiculoPar.LimparSelecao;
  s.AddToSelection;

  Renderizar;
end;

procedure TVerse.OnSaveTextToFile(Sender: TObject);
begin
  if assigned(FOnExportText) then
     FOnExportText(Self);
end;

procedure TVerse.OnCopyTokensToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetTokens();
end;

procedure TVerse.OnCopyInterlinearVerseToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetMySwordInterlinearLine;
end;

procedure TVerse.OnCopyXMLToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetPares;
end;

procedure TVerse.OnRightToLeft(Sender: TObject);
begin
  RightToLeft := not FRightToLeft;
end;

procedure TVerse.OnVerseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    if Assigned(FVersePopupMenu) then
      FVersePopupMenu.PopUp;
end;

procedure TVerse.OnResize(Sender: TObject);
begin
  if not FRendering then
    OrganizarSintagmas;
end;

procedure TVerse.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    FPanel.Font.Size := FPanel.Font.Size + Sign(WheelDelta) * 1;
    Renderizar;
  end;
end;

procedure TVerse.InitSyntagmPopupMenu;
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

procedure TVerse.InitVersePopupMenu;
var
  Item: TMenuItem;
begin
  FVersePopupMenu := TPopupMenu.Create(FPanel);
  FVersePopupMenu.Parent := FPanel;
  FVersePopupMenu.Tag := PtrInt(Self);

  FRTLMenuItem := TMenuItem.Create(FVersePopupMenu);
  FRTLMenuItem.Caption := SRightToLeft;
  FRTLMenuItem.OnClick := @OnRightToLeft;
  FVersePopupMenu.Items.Add(FRTLMenuItem);

  Item := TMenuItem.Create(FVersePopupMenu);
  Item.Caption := SSaveToFile;
  Item.OnClick := @OnSaveTextToFile;
  FVersePopupMenu.Items.Add(Item);

  {$IFDEF DEBUG}
    FVersePopupMenu.Items.AddSeparator;

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyTokensToClipboard;
    Item.OnClick := @OnCopyTokensToClipboard;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopySyntagmasJSONToClipboard;
    Item.OnClick := @OnCopySintagmasAsJSON;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyInterlinearVerseToClipboard;
    Item.OnClick := @OnCopyInterlinearVerseToClipboard;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyXMLToClipboard;
    Item.OnClick := @OnCopyXMLToClipboard;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyAssociationsJSONToClipboard;
    Item.OnClick := @OnCopyAssociationsJSONToClipboard;
    FVersePopupMenu.Items.Add(Item);

  {$ENDIF}
end;

constructor TVerse.Create(const AReferencia: string = '');
begin
  Create(nil, AReferencia);
end;

procedure TVerse.OnCopySintagmasAsJSON(Sender: TObject);
  // local helper to convert a TStringList to a JSON array
  function StringListToJSONArray(lst: TStringList): string;
  var
    i: Integer;
    arr: string;
  begin
    if lst = nil then
    begin
      Result := '[]';
      Exit;
    end;
    arr := '[';
    for i := 0 to lst.Count - 1 do
    begin
      if i > 0 then
        arr := arr + ',';
      arr := arr + QuotedStr(lst[i]);
    end;
    arr := arr + ']';
    Result := arr;
  end;
var
  i: Integer;
  s: TSyntagm;
  jsonItems: TStringList;
  jsonStr: string;
  strongsJSON, morphJSON, kindStr: string;
begin
  jsonItems := TStringList.Create;
  try
    for i := 0 to FSintagmas.Count - 1 do
    begin
      s := FSintagmas[i];
      strongsJSON := StringListToJSONArray(s.Strong);
      morphJSON := StringListToJSONArray(s.Morph);
      kindStr := GetEnumName(TypeInfo(TTipoSintagma), Ord(s.Kind)); // convert kind to text
      jsonItems.Add(Format('{"text": "%s", "index": %d, "kind": "%s", "strongs": %s, "morph": %s}'#13#10,
        [s.Text, i, kindStr, strongsJSON, morphJSON]));
    end;
    // Prevent TStringList from adding extra quotes by disabling quoting
    jsonItems.Delimiter := ',';
    jsonItems.StrictDelimiter := True;
    jsonItems.QuoteChar := #0;
    jsonStr := '{"syntagmas": [' + jsonItems.DelimitedText + ']}';
    Clipboard.AsText := jsonStr;
  finally
    jsonItems.Free;
  end;
end;

function TVerse.GetAssociationsJSON: string;
var
  associations: TStringList;
  processed: TList; // list to avoid duplicate processing
  s, p: TSyntagm;
  i: integer;
  wordA, wordB: string;
  jsonList: string;
begin
  associations := TStringList.Create;
  processed := TList.Create;
  try
    for i := 0 to FSintagmas.Count - 1 do
    begin
      s := FSintagmas[i];
      if (s.Pairs.Count = 0) or (processed.IndexOf(s) <> -1) then
        Continue;
      // Mark the group as processed
      processed.Add(s);
      for p in s.Siblings do
        processed.Add(p);
      // Build side A by concatenating s.Text and its siblings
      wordA := s.Text;
      for p in s.Siblings do
        wordA := wordA + ' ' + p.Text;
      // Build side B by concatenating texts of each paired syntagm
      wordB := '';
      for p in s.Pairs do
      begin
        if wordB = '' then
          wordB := p.Text
        else
          wordB := wordB + ' ' + p.Text;
      end;
      // Add association in desired format
      associations.Add(Format('["%s", "%s"]', [wordA, wordB]));
    end;
    // Manually join the association elements without extra quotes
    jsonList := '';
    for i := 0 to associations.Count - 1 do
    begin
      if i > 0 then
        jsonList := jsonList + ',';
      jsonList := jsonList + associations[i];
    end;
    Result := '{"data": [' + jsonList + ']}';
  finally
    processed.Free;
    associations.Free;
  end;
end;

function TVerse.HasAssociations: boolean;
var
  s: TSyntagm;
begin
  Result := False;
  if not Assigned(FSintagmas) then Exit;
  for s in FSintagmas do
    if s.Pairs.Count > 0 then
    begin
      Result := True;
      Break;
    end;
end;

procedure TVerse.OnCopyAssociationsJSONToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetAssociationsJSON;
end;

procedure TVerse.ClearSyntagms;
begin
  LimparSintagmas;
end;

procedure TVerse.AssociateSyntagms;
begin
  AssociarSintagmas;
end;

procedure TVerse.ClearSelection;
begin
  LimparSelecao;
end;

procedure TVerse.DisassociatePairs;
begin
  DesassociarPares;
end;

procedure TVerse.ClearAssociations;
begin
  LimparAssociacoes;
end;

procedure TVerse.Render;
begin
  Renderizar;
end;

procedure TVerse.RenderStrongCount;
begin
  RenderizarStrongCount;
end;

procedure TVerse.OrganizeSyntagms;
begin
  OrganizarSintagmas;
end;

procedure TVerse.SelectSyntagms(list: TSyntagmList);
begin
  SelecionarSintagmas(list);
end;

procedure TVerse.ChangeText(_XML: string);
begin
  AlterarTexto(_XML);
end;

function TVerse.GetPairsList(tipo: TPairsListType): TStringList;
begin
  Result := GetListaPares(tipo);
end;

procedure TVerse.OnSyntagmPopupMenu(s: TSyntagm);
begin
  OnSintagmaPopupMenu(s);
end;

procedure TVerse.ShowTags;
begin
  MostrarTags;
end;

procedure TVerse.HideTags;
begin
  OcultarTags;
end;

function TVerse.GetONTLine: string;
begin
  Result := GetLinhaONT;
end;

function TVerse.GetONTLine(morfo: boolean; autoitalico: boolean; strongsreutilizados: boolean;
  strongsnaotraduzidos: boolean): string;
begin
  Result := GetLinhaONT(morfo, autoitalico, strongsreutilizados, strongsnaotraduzidos);
end;

end.


