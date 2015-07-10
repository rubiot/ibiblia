unit Projeto;

{$mode objfpc}{$H+}{$M+}

{TODO -cDesejável: Remover dependência com formpopup, criá-lo dinamicamente }
{TODO -cDesejável: Usar descendente de THintWindow para exibição de definições }

interface

uses
  Classes,
  {$IFDEF UNIX}
  LCLType, LCLIntf,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils, Sqlite3DS, sqlite3conn, sqldb, db, StrUtils,
  ExtCtrls, Controls, ComCtrls, StdCtrls, Graphics, Forms, Versiculo, Sugestao,
  MemoVersiculo, iBibliaXML, Dialogs, dos, PCRE, ExportarProjeto, LazLogger;

type

  TProjeto = class;

  { TTipoTextoBiblico }

  TTipoTextoBiblico =
  (
    tbOrigem,
    tbDestino,
    tbConsulta1,
    tbConsulta2
  );

  TOpcaoExportacao =
  (
    oeExportarMorfologia,      // incluir morfologia
    oeExportarNAComoItalicos,  // palavras não associadas como itálicos
    oeExportarComentarios,     // comentários como notas de rodapé
    oeConcordDetalhada,        // concordância aberta por declinação
    oeStrongsReutilizados,     // sinalizar Strongs reutilizados
    oeStrongsNaoTraduzidos     // sinalizar Strongs não traduzidos
  );

  TOpcoesExportacao = set of TOpcaoExportacao;

  TOnNovoVersiculoEvent = procedure (Sender: TProjeto) of object;
  //TOnAlterarVersiculoEvent = procedure (Sender: TProjeto) of object;

  AVersiculo = array[tbOrigem..tbConsulta2] of TVersiculo;
  ACamposTexto = array[tbOrigem..tbConsulta2] of integer;
  ADicStrong = array[tbOrigem..tbConsulta2] of TSQLQuery;
  ADicMorfo = array[tbOrigem..tbConsulta2] of TSQLQuery;
  STextosBiblicos = set of TTipoTextoBiblico;

  { TProjeto }

  TProjeto = class
  private
    FMemoVersiculo: TMemoVersiculo;
    FAtivo: boolean;
    FOnAlterarVersiculo: TOnAlterarVersiculoEvent;
    FSugerirAssociacaoAuto: boolean;
    FExibirDefComCtrl: boolean;
    FTblPares: TSqlite3Dataset;
    FTblInfo: TSqlite3Dataset;
    FAVersiculo: AVersiculo;
    FACamposTexto: ACamposTexto;
    FADicStrong: ADicStrong;
    FADicMorfo: ADicMorfo;
    FSugeridor: TGerSugestoes;
    FParesAntigos: TStringList;
    FArvore: TTreeView;
    FMemoComentarios: TMemo;
    FRadioGroupSituacao: TRadioGroup;
    FTemporizador: TTimer;
    FAtrasoExibicao: Cardinal;
    FOnNovoVersiculo: TOnNovoVersiculoEvent;
    function GetCaminho: string;
    function GetComentarios: string;
    function GetID: string;
    function GetModificado: boolean;
    function GetReferencia: string;
    function GetSituacao: Integer;
    procedure SetArvore(const AValue: TTreeView);
    procedure SetAtrasoExibicao(const AValue: Cardinal);
    procedure SetComentarios(const AValue: string);
    procedure SetOnAlterarVersiculo(const AValue: TOnAlterarVersiculoEvent);
    procedure SetOnNovoVersiculo(const AValue: TOnNovoVersiculoEvent);
    procedure SetSituacao(const AValue: Integer);
  protected
    procedure CopiarArquivo(origem, destino: string);
    function CriarObjetoTabela(db, tabela, chave: string): TSqlite3Dataset;
    function CriarObjetoQuery(db: string): TSQLQuery;
    function InserirInfo(info, valor: string): boolean;
    function AtualizarInfo(info, valor: string): boolean;
    function ResgatarInfo(info: string): string;
    function ObterDefinicaoStrong(strong: string; texto: TTipoTextoBiblico): string;
    function ObterDefinicaoMorfo(morfo: string; texto: TTipoTextoBiblico): string;
    procedure InserirVersiculoTexto(versiculo: string; texto: TTipoTextoBiblico);
    procedure PreRolagemVersiculo(DataSet: TDataSet);
    procedure PosRolagemVersiculo(DataSet: TDataSet);
    procedure SalvarPares;
    procedure SalvarTexto;
    procedure SalvarAssociacoes;
    procedure RemoverAssociacoes;
    procedure HabilitarEventosRolagem;
    procedure DesabilitarEventosRolagem;
    procedure SintagmaOnMouseEnter(Sender: TSintagma);
    procedure SintagmaOnMouseLeave(Sender: TSintagma);
    procedure SintagmaOnClick(Sender: TSintagma);
    procedure OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
    procedure OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
    procedure OnExibirDefinicao(Sender: TObject);
    procedure OnRedimensionarVersiculo(Sender: TObject);
    procedure OnDblClickVersiculo(Sender: TObject);
    procedure AtribuirDicStrong(dic: string; t: TTipoTextoBiblico);
    procedure AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico);
    procedure AtualizarArvore;
    procedure AtualizarArvore(id: string);
  public
    constructor Criar;
    constructor Criar(paineis: array of TScrollbox; navegador: TTreeView; rsituacao: TRadioGroup; rcomentarios: TMemo);
    destructor Destruir;
    procedure Novo(nomedb, descricao: string);
    procedure Novo(nomedbvelho, nomedbnovo, descricao: string);
    procedure Abrir(Nome: string);
    procedure Fechar(_Commit: boolean);
    procedure Commit;
    procedure Rollback;
    procedure IrPara(Referencia: string);
    procedure VersiculoSeguinte;
    procedure VersiculoAnterior;
    procedure VersiculoInicial;
    procedure VersiculoFinal;
    procedure Atualizar;
    procedure Limpar;
    procedure Salvar;
    procedure SugerirAssociacao;
    procedure RecriarBaseSugestoes;
    procedure RecriarBaseSugestoes(pb: TProgressBar);
    procedure ImportarModuloTheWord(arquivo: string; texto: TTipoTextoBiblico);
    procedure ImportarModuloTheWord(arquivo: string; texto: TTipoTextoBiblico; pb: TProgressBar);
    procedure NovoObjetoVersiculo(owner: TScrollBox; texto: TTipoTextoBiblico);
    procedure ExportarTextoDestinoComStrongs(arquivo: string; opcoes: TOpcoesExportacao);
    procedure ExportarTextoDestinoComStrongs(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao);
    procedure ExportarTextoInterlinear(arquivo: string; opcoes: TOpcoesExportacao);
    procedure ExportarTextoInterlinear(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao);
    procedure ExportarConcordancia(arquivo: string; opcoes: TOpcoesExportacao);
    procedure ExportarConcordancia(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao);
    procedure AtribuirFonteTexto(fonte: TFont; textos: STextosBiblicos);
    procedure LimparTexto(texto: TTipoTextoBiblico);
    function ObterFonteTexto(texto: TTipoTextoBiblico): TFont;
    procedure AtribuirDicStrong(dic: string; textos: STextosBiblicos);
    procedure AtribuirDicMorfo(dic: string; textos: STextosBiblicos);
    procedure AtribuirInfo(info, valor: string);
    function ObterInfo(info: string): string;
    function ObterTextoVersiculo(texto: TTipoTextoBiblico): string;
    function ObterTextoVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
    function ObterTextoSimplesVersiculo(texto: TTipoTextoBiblico): string;
    function ObterTextoSimplesVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
    property Referencia: string read GetReferencia;
    property ID: string read GetID;
    property Modificado: boolean read GetModificado;
    property Arvore: TTreeView read FArvore write SetArvore;
    property OnAlterarVersiculo: TOnAlterarVersiculoEvent read FOnAlterarVersiculo write SetOnAlterarVersiculo;
    property OnNovoVersiculo: TOnNovoVersiculoEvent read FOnNovoVersiculo write SetOnNovoVersiculo;
    property AtrasoExibicaoDefinicao: Cardinal read FAtrasoExibicao write SetAtrasoExibicao;
    property Ativo: boolean read FAtivo;
    property Caminho: string read GetCaminho;
    property Situacao: Integer read GetSituacao write SetSituacao;
    property Comentarios: string read GetComentarios write SetComentarios;
    property MemoComentarios: TMemo read FMemoComentarios write FMemoComentarios;
    property RadioGroupSituacao: TRadioGroup read FRadioGroupSituacao write FRadioGroupSituacao;
    property ExibirDefinicoesSoComCtrl: boolean read FExibirDefComCtrl write FExibirDefComCtrl;
    property SugerirAssociacaoAutomaticamente: boolean read FSugerirAssociacaoAuto write FSugerirAssociacaoAuto;
  end;

const
  QLivros: array[1..27] of string = (
    'Mateus','Marcos','Lucas','João','Atos dos apóstolos','Romanos',
    '1 Coríntios','2 Coríntios','Gálatas','Efésios',
    'Filipenses','Colossensses','1 Tessalonicenses','2 Tessalonicenses',
    '1 Timóteo','2 Timóteo','Tito','Filemon','Hebreus','Tiago',
    '1 Pedro','2 Pedro','1 João','2 João','3 João','Judas','Apocalipse'
  );
  QCapitulos: array[1..27] of smallint = (
    28,16,24,21,28,16,16,13,6,6,4,4,5,3,6,4,3,1,13,5,5,3,5,1,1,1,22
  );
  QCapLivros: array[1..27] of smallint = (
    1,29,45,69,90,118,134,150,163,169,175,179,183,188,191,197,201,204,205,218,
    223,228,231,236,237,238,239
  );
  QVersiculos: array[1..260] of smallint = (
    25,23,17,25,48,34,29,34,38,42,30,50,58,36,39,28,27,35,30,34,46,46,39,51,46,
    75,66,20,45,28,35,41,43,56,37,38,50,52,33,44,37,72,47,20,80,52,38,44,39,49,
    50,56,62,42,54,59,35,35,32,31,37,43,48,47,38,71,56,53,51,25,36,54,47,71,53,
    59,41,42,57,50,38,31,27,33,26,40,42,31,25,26,47,26,37,42,15,60,40,43,48,30,
    25,52,28,41,40,34,28,41,38,40,30,35,27,27,32,44,31,32,29,31,25,21,23,25,39,
    33,21,36,21,14,23,33,27,31,16,23,21,13,20,40,13,27,33,34,31,13,40,58,24,24,
    17,18,18,21,18,16,24,15,18,33,21,14,24,21,29,31,26,18,23,22,21,32,33,24,30,
    30,21,23,29,23,25,18,10,20,13,18,28,12,17,18,20,15,16,16,25,21,18,26,17,22,
    16,15,15,25,14,18,19,16,14,20,28,13,28,39,40,29,25,27,26,18,17,20,25,25,22,
    19,14,21,22,18,10,29,24,21,21,13,14,25,20,29,22,11,14,17,17,13,21,11,19,17,
    18,20,8,21,18,24,21,15,27,21
  );

implementation

uses formpopup, formverserules;

{ TProjeto }

procedure TProjeto.Novo(nomedb, descricao: string);
begin
  Novo('.\projeto.modelo', nomedb, descricao);
end;

procedure TProjeto.Novo(nomedbvelho, nomedbnovo, descricao: string);
begin
  CopiarArquivo(nomedbvelho, nomedbnovo);

  Abrir(nomedbnovo);

  AtribuirInfo('descricao', descricao);
  AtribuirInfo('marcador', '40,1,1');
  FExibirDefComCtrl := false;  //FTblInfo.ExecuteDirect('COMMIT;');
  Commit;
end;

function TProjeto.GetReferencia: string;
begin
  result := FTblPares.FieldByName('pare_ref').AsString;
end;

function TProjeto.GetSituacao: Integer;
begin
  result := FTblPares.FieldByName('pare_situacao').AsInteger;
end;

procedure TProjeto.SetArvore(const AValue: TTreeView);
var
  l, c, v{, id}: smallint;
  nl, nc, nv: TTreeNode;
begin
  if FArvore = AValue then exit;
  FArvore := AValue;

  if FArvore.Items.Count = 0 then // evitando preencher outra vez a mesma árvore
  begin // preenchendo árvore
    FArvore.Visible := false;
    //id := 0;
    for l:=low(QLivros) to high(QLivros) do
    begin
      nl := FArvore.Items.Add(nil, QLivros[l]);
      nl.ImageIndex := 0; nl.SelectedIndex := nl.ImageIndex + 4;
      //nl.Data := Pointer(id);
      for c:=1 to QCapitulos[l] do
      begin
        nc := FArvore.Items.AddChild(nl, IntToStr(c));
        nc.ImageIndex := 0; nc.SelectedIndex := nc.ImageIndex + 4;
        //nc.Data := Pointer(id);
        for v:=1 to QVersiculos[QCapLivros[l] + c - 1] do
        begin
          nv := FArvore.Items.AddChild(nc, IntToStr(v));
          nv.ImageIndex := 0; nv.SelectedIndex := nv.ImageIndex + 4;
          //nv.Data := Pointer(id);
          //inc(id);
        end;
      end;
    end;
    //FArvore.SaveToFile('.\arvore.txt');
    FArvore.Visible := true;
  end;

  FArvore.OnChange := @OnMudancaVersiculo;
end;

procedure TProjeto.SetAtrasoExibicao(const AValue: Cardinal);
begin
  if FAtrasoExibicao = AValue then exit;
  FAtrasoExibicao := AValue;
  FTemporizador.Interval := AValue;
end;

procedure TProjeto.SetComentarios(const AValue: string);
begin
  FTblPares.Edit;
  FTblPares.FieldByName('pare_comentarios').AsString := AValue;
  FTblPares.Post;
end;

procedure TProjeto.SetOnAlterarVersiculo(const AValue: TOnAlterarVersiculoEvent
  );
begin
  if FOnAlterarVersiculo = AValue then exit;
  FOnAlterarVersiculo := AValue;

  if assigned(FAVersiculo[tbOrigem]) then
    FAVersiculo[tbOrigem].OnAlterarVersiculo := OnAlterarVersiculo;
end;

procedure TProjeto.SetOnNovoVersiculo(const AValue: TOnNovoVersiculoEvent);
begin
  if FOnNovoVersiculo = AValue then exit;
  FOnNovoVersiculo := AValue;
end;

procedure TProjeto.SetSituacao(const AValue: Integer);
begin
  FTblPares.Edit;
  FTblPares.FieldByName('pare_situacao').AsInteger := AValue;
  FTblPares.Post;
end;

function TProjeto.GetModificado: boolean;
begin
  result := FTblPares.UpdatesPending or FTblInfo.UpdatesPending or (assigned(FAVersiculo[tbOrigem]) and FAVersiculo[tbOrigem].Modificado);
end;

function TProjeto.GetCaminho: string;
begin
  result := '';
  if FTblInfo.Active then
    result := FTblInfo.FileName;
end;

function TProjeto.GetComentarios: string;
begin
  result := FTblPares.FieldByName('pare_comentarios').AsString;
end;

function TProjeto.GetID: string;
begin
  result := FTblPares.FieldByName('pare_id').AsString;
end;

procedure TProjeto.CopiarArquivo(origem, destino: string);
begin
  with TMemoryStream.Create do
  try
    LoadFromFile(origem);
    SaveToFile(destino);
  finally
    Free;
  end;
end;

function TProjeto.CriarObjetoTabela(db, tabela, chave: string
  ): TSqlite3Dataset;
begin
  result := TSqlite3Dataset.Create(nil);
  result.FileName := db;
  result.TableName := tabela;
  result.PrimaryKey := chave;
end;

function TProjeto.CriarObjetoQuery(db: string): TSQLQuery;
begin
  result := TSQLQuery.Create(nil);
  result.DataBase := TSQLite3Connection.Create(nil);
  result.DataBase.DatabaseName := db;
  result.DataBase.Open;
  TSQLite3Connection(result.DataBase).Transaction := TSQLTransaction.Create(nil);
  result.Transaction := TSQLite3Connection(result.DataBase).Transaction;
end;

function TProjeto.InserirInfo(info, valor: string): boolean;
begin
  if FTblInfo.Locate('id', info, []) then
  begin
     result := false;
     exit;
  end;

  FTblInfo.InsertRecord([info, valor]);

  result := true;
  {result := true;
  try
    FTblInfo.ExecuteDirect(format('insert into info (id, valor) values (''%s'', ''%s'')', [AnsiReplaceStr(info, '''', ''''''), AnsiReplaceStr(valor, '''', '''''')]));
  except
    result := false;
  end;}
end;

function TProjeto.AtualizarInfo(info, valor: string): boolean;
begin
  result := FTblInfo.Locate('id', info, []);

  if result then
  begin
    FTblInfo.Edit;
    FTblInfo.Fields[1].AsString := valor;
    FTblInfo.Post;
  end;

  {FTblInfo.ExecuteDirect(format('update info set valor = ''%s'' where id = ''%s''', [AnsiReplaceStr(valor, '''', ''''''), AnsiReplaceStr(info, '''', '''''')]));
  result := FTblInfo.RowsAffected = 1;}
end;

function TProjeto.ResgatarInfo(info: string): string;
begin
  result := '';
  if FTblInfo.Locate('id', info, []) then
    result := FTblInfo.Fields[1].AsString;
  {result := FTblInfo.QuickQuery(format('select valor from info where id = ''%s''', [AnsiReplaceStr(info, '''', '''''')]));}
end;

function TProjeto.ObterDefinicaoStrong(strong: string; texto: TTipoTextoBiblico
  ): string;
begin
  if (FADicStrong[texto] = nil) or (strong = '') then
  begin
    result := ''; //'{\rtf1\ansi\ansicpg1252\deff0\deflang1046{\fonttbl{\f0\fswiss\fcharset0 Arial;}}\viewkind4\uc1\pard\f0\fs20\par}';
    exit;
  end;

  with FADicStrong[texto] do
  begin
    Close;
    Params.ParamByName('strong').AsString := strong;
    Open;
    if AnsiStartsStr('{\rtf1', Fields[0].AsString) then
      result := Fields[0].AsString
    else
      result := '{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Tahoma;}' +
        '{\f1\fswiss\fprq2\fcharset0 Tahoma;}{\f2\fswiss\fcharset0 Arial;}}' +

        '{\colortbl ;' +
            '\red0\green0\blue0;' +
            '\red0\green0\blue255;' +
            '\red150\green60\blue100;' + //maroon;
            '\red0\green255\blue0;' +
            '\red255\green0\blue255;' +
            '\red255\green0\blue0;' +
            '\red255\green255\blue0;' +
            '\red255\green255\blue255;' +
            '\red0\green0\blue128;' +
            '\red0\green128\blue128;' +
            '\red255\green0\blue0;' + //dark green
            '\red128\green0\blue128;' +
            '\red128\green0\blue0;' +
            '\red128\green128\blue0;' +
            '\red128\green128\blue128;' +
            '\red192\green192\blue192;}' +

        '\viewkind4\uc1\pard\lang1046\f0\fs20 ' +
        Fields[0].AsString + '}';
  end;
end;

function TProjeto.ObterDefinicaoMorfo(morfo: string; texto: TTipoTextoBiblico
  ): string;
var
  m: string;
  //p, l: sizeint;
begin
  if (not assigned(FADicMorfo[texto])) or (morfo = '') then
  begin
    result := ''; //'{\rtf1\ansi\ansicpg1252\deff0\deflang1046{\fonttbl{\f0\fswiss\fcharset0 Arial;}}\viewkind4\uc1\pard\f0\fs20\par}';
    exit;
  end;

  // tratar morfologia com lema <WTXXXXX l="yyyyyy">
  m := morfo;
  //p := pos(' ', m);
  //l := length(m);
  if pos(' ', m) > 1 then
     m := copy(m, 1, pos(' ', m)-1);

  with FADicMorfo[texto] do
  begin
    Close;
    Params.ParamByName('morf').AsString := m;
    Open;
    result :=
      '{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Tahoma;}' +
      '{\f1\fswiss\fprq2\fcharset0 Tahoma;}{\f2\fswiss\fcharset0 Arial;}}' +
      '{\colortbl ;\red128\green0\blue64;\red128\green0\blue0;}\viewkind4\uc1\pard\lang1046\f0\fs20 ' +
      Fields[0].AsString + '}';
  end;
end;

procedure TProjeto.InserirVersiculoTexto(versiculo: string;
  texto: TTipoTextoBiblico);
begin
  FTblPares.Edit;
  FTblPares.Fields[FACamposTexto[texto]].AsString := versiculo;
  FTblPares.Post;
end;

procedure TProjeto.PreRolagemVersiculo(DataSet: TDataSet);
begin
  if FAVersiculo[tbOrigem] = nil then
    exit;

  if FAVersiculo[tbOrigem].Modificado then
  begin
    SalvarAssociacoes;
    SalvarPares;
  end;
  SalvarTexto; // salvar alterações nos textos dos versículos

  FParesAntigos.Free; // liberando pares antigos do versículo
  FParesAntigos := nil;

  if assigned(FMemoComentarios) then
    Comentarios := FMemoComentarios.Text;

  if assigned(FRadioGroupSituacao) then
  begin
    Situacao := FRadioGroupSituacao.ItemIndex;
    AtualizarArvore(ID);
  end;
end;

procedure TProjeto.PosRolagemVersiculo(DataSet: TDataSet);
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].Texto := FTblPares.Fields[FACamposTexto[v]].AsString;

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbOrigem]) then
  begin
    FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
    FParesAntigos := FAVersiculo[tbOrigem].GetListaPares(tlMetaDados);
  end;

  if assigned(FMemoComentarios) then
    FMemoComentarios.Text := Comentarios;

  if assigned(FRadioGroupSituacao) then
    FRadioGroupSituacao.ItemIndex := Situacao;

  if assigned(OnNovoVersiculo) then
    OnNovoVersiculo(self);
end;

procedure TProjeto.SalvarPares;
begin
  FTblPares.Edit;
  FTblPares.FieldByName('pare_pares').AsString := FAVersiculo[tbOrigem].Pares;
  FTblPares.Post;
end;

procedure TProjeto.SalvarTexto;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) and FAVersiculo[v].XMLModificado then
    begin
      FTblPares.Edit;
      FTblPares.Fields[FACamposTexto[v]].AsString := FAVersiculo[v].XML;
      FTblPares.Post;
    end;
end;

procedure TProjeto.SalvarAssociacoes;
var
  pares: TStringList;
  p: smallint;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  if Assigned(FParesAntigos) then
    RemoverAssociacoes;

  pares := FAVersiculo[tbOrigem].GetListaPares(tlMetaDados);

  for p:=0 to pares.Count-1 do
    if (p mod 2) = 0 then // pares estão alternados na lista
      FSugeridor.InserirPar(pares.Strings[p], pares.Strings[p+1]);

  pares.Free;
end;

procedure TProjeto.RemoverAssociacoes;
var
  p: smallint;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbOrigem] = nil) then
    exit;

  for p:=0 to FParesAntigos.Count-1 do
    if (p mod 2) = 0 then // pares estão alternados na lista
      FSugeridor.RemoverPar(FParesAntigos.Strings[p], FParesAntigos.Strings[p+1]);
end;

procedure TProjeto.HabilitarEventosRolagem;
begin
  FTblPares.BeforeScroll := @PreRolagemVersiculo;
  FTblPares.AfterScroll  := @PosRolagemVersiculo;
  FArvore.OnChange       := @OnMudancaVersiculo;
end;

procedure TProjeto.DesabilitarEventosRolagem;
begin
  FTblPares.BeforeScroll := nil;
  FTblPares.AfterScroll  := nil;
  FArvore.OnChange       := nil;
end;

procedure TProjeto.SintagmaOnMouseEnter(Sender: TSintagma);
begin
  //if (GetKeyState(VK_CAPITAL) and 1) <> 0 then // capslock ativo?
  if not FExibirDefComCtrl or ((
  {$IFDEF UNIX}
  LCLIntf.GetKeyState(VK_CONTROL)
  {$ELSE}
  GetKeyState(VK_CONTROL)
  {$ENDIF}
  and $8000) <> 0) then // CONTROL pressionado?
  begin
    FTemporizador.Enabled := false;
    FTemporizador.Tag := PtrInt(Sender);
    FTemporizador.Enabled := true;
  end;
end;

procedure TProjeto.SintagmaOnMouseLeave(Sender: TSintagma);
begin
  FTemporizador.Enabled := false;
  if (GetKeyState(VK_CONTROL) and $8000) = 0 then
    frmDictionaryPopup.Visible := false;
end;

procedure TProjeto.SintagmaOnClick(Sender: TSintagma);
begin
  if assigned(FAVersiculo[tbOrigem]) then
  begin
    if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
      FRadioGroupSituacao.ItemIndex := 1 // associando
    else if (FRadioGroupSituacao.ItemIndex > 1) and (FAVersiculo[tbOrigem].AndamentoAssociacao = 0) then
      FRadioGroupSituacao.ItemIndex := 0 // sem associação
    else if (FRadioGroupSituacao.ItemIndex = 1) and ((FAVersiculo[tbOrigem].AndamentoAssociacao = 1) or (FAVersiculo[tbDestino].AndamentoAssociacao = 1)) then
      FRadioGroupSituacao.ItemIndex := 3; // associado
  end;
end;

procedure TProjeto.OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
var
  //l, c, v, id: smallint;
  l, c, v: TTreeNode;
  i: smallint;
begin
  if not assigned(FTblPares) then
    exit;

  if Node.HasChildren then // capítulo ou livro
  begin
    if assigned(Node.Parent) then // capitulo
    begin
      c := Node;
      l := c.Parent;
      v := c.GetFirstChild;
    end else begin // livro
      l := Node;
      c := l.GetFirstChild;
      v := c.GetFirstChild;
    end;
  end else begin // versículo
    v := Node;
    c := v.Parent;
    l := c.Parent;
  end;

  for i:=low(QLivros) to high(QLivros) do
    if QLivros[i] = l.Text then
      break;

  IrPara(format('%d,%s,%s', [i+39, c.Text, v.Text]));

  {
  id := 0;
  for l:=low(QLivros) to high(QLivros) do
    for c:=1 to QCapitulos[l] do
      for v:=1 to QVersiculos[QCapLivros[l] + c - 1] do
      begin
        if id = Integer(Node.Data) then
        begin
          IrPara(format('%d,%d,%d', [l+39, c, v]));
          exit;
        end;
        inc(id);
      end;
  }
end;

procedure TProjeto.OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if FAVersiculo[v] = Sender.Versiculo then
    begin
      Sender.Versiculo.DesassociarPares;
      //Sender.Versiculo.Texto := Sender.Texto;
      Sender.Versiculo.Texto := Sender.Texto;
      FTblPares.Edit;
      FTblPares.Fields[FACamposTexto[v]].AsString := Sender.Texto;
      FTblPares.Post;
      break;
    end;
  //MessageDlg('Versículo modificado', format('Versiculo modificado: %s', [Sender.Texto]), mtError, [mbOK], 0);
end;

procedure TProjeto.OnExibirDefinicao(Sender: TObject);
var
  v: TTipoTextoBiblico;
  p: TPoint;
  s: TSintagma;
begin
  if frmDictionaryPopup.Visible then
     exit;

  s := TSintagma(TTimer(Sender).Tag);
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if (FAVersiculo[v] = s.VersiculoRef) and
       (FADicStrong[v] <> nil) and
       (s.Strong.Count > 0) then
    begin
      if s.Morf.Count > 0 then
        frmDictionaryPopup.Morfo1 := ObterDefinicaoMorfo(s.Morf.Strings[0], v);

      if s.Strong.Count > 0 then
      begin
        frmDictionaryPopup.Strong1 := ObterDefinicaoStrong(s.Strong.Strings[0], v);
        p := s.LabelRef.ClientToScreen(s.LabelRef.ClientRect.BottomRight);
        //frmDictionaryPopup.Left := p.x;
        //frmDictionaryPopup.Top := p.y;
        frmDictionaryPopup.Caption := s.Texto;
        //frmDictionaryPopup.Visible := true;
        frmDictionaryPopup.MostrarEm(p.x, p.y);
      end;
    end;
end;

procedure TProjeto.OnRedimensionarVersiculo(Sender: TObject);
begin
  TVersiculo(TScrollBox(Sender).Tag).OrganizarSintagmas;
end;

procedure TProjeto.OnDblClickVersiculo(Sender: TObject);
begin
  if not Ativo then
    exit;

  FMemoVersiculo.Ativar(TScrollBox(Sender), TVersiculo(TScrollBox(Sender).Tag));
end;

procedure TProjeto.AtribuirDicStrong(dic: string; t: TTipoTextoBiblico);
//var
//  t1: TTipoTextoBiblico;
begin
  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    MessageDlg('Dicionário de Strong', format('O dicionário selecionado não existe: %s', [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  if assigned(FADicStrong[t]) then // liberando query se não estiver mais sendo utilizada
  begin
    FADicStrong[t].Tag := FADicStrong[t].Tag - 1;
    if (FADicStrong[t].Tag = 0) then
    begin
      FADicStrong[t].Close;
      FADicStrong[t].DataBase.Close;
      TSQLite3Connection(FADicStrong[t].DataBase).Transaction.Free;
      FADicStrong[t].DataBase.Free;
      FADicStrong[t].Free;
      FADicStrong[t] := nil;
    end;
  end;
  { desabilitando reaproveitamento até corrigir erro de desalocação múltipla }
  {for t1:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    if assigned(FADicStrong[t1]) and (FADicStrong[t1].DataBase.DatabaseName = dic) then
    begin // reaproveitando TQueries que usem o mesmo db
      FADicStrong[t] := FADicStrong[t1];
      FADicStrong[t].Tag := FADicStrong[t].Tag + 1;
      break;
    end;}

  if not assigned(FADicStrong[t]) then
  begin // primeira ocorrência deste db
    FADicStrong[t] := CriarObjetoQuery(dic);
    FADicStrong[t].SQL.Text := 'select data from topics, content where subject = :strong and id = topic_id';
    FADicStrong[t].Prepare;
    FADicStrong[t].Tag := 1;
  end;

  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;
end;

procedure TProjeto.AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico);
//var
//  t1: TTipoTextoBiblico;
begin
  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    MessageDlg('Dicionário de Morfologia', format('O dicionário selecionado não existe: %s', [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  if assigned(FADicMorfo[t]) then // liberando query se não estiver mais sendo utilizada
  begin
    FADicMorfo[t].Tag := FADicMorfo[t].Tag - 1;
    if (FADicMorfo[t].Tag = 0) then
    begin
      FADicMorfo[t].Close;
      FADicMorfo[t].DataBase.Close;
      TSQLite3Connection(FADicMorfo[t].DataBase).Transaction.Free;
      FADicMorfo[t].DataBase.Free;
      FADicMorfo[t].Free;
      FADicMorfo[t] := nil;
    end;
  end;
  // desabilitando reaproveitamento até corrigir erro de desalocação múltipla
  {for t1:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    if assigned(FADicMorfo[t1]) and (FADicMorfo[t1].DataBase.DatabaseName = dic) then
    begin // reaproveitando TQueries que usem o mesmo db
      FADicMorfo[t] := FADicMorfo[t1];
      FADicMorfo[t].Tag := FADicStrong[t].Tag + 1;
      break;
    end;}

  if not assigned(FADicMorfo[t]) then
  begin // primeira ocorrência deste db
    FADicMorfo[t] := CriarObjetoQuery(dic);
    FADicMorfo[t].SQL.Text := 'select data from topics, content where subject = :morf and id = topic_id';
    FADicMorfo[t].Prepare;
    FADicMorfo[t].Tag := 1;
 end;
  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;
end;

procedure TProjeto.AtualizarArvore;
var
  n, ul, uc: TTreeNode;
  qv, qc, l, c, v, s: smallint;
  sl, sc: array[0..3] of smallint;
  marcador: string;
  b: boolean;
begin
  if not assigned(FArvore) then exit;
  n := FArvore.TopItem.GetFirstChild.GetFirstChild; // nó versículo atual
  l := 40; // livro atual
  c := 1;  // capítulo atual
  v := 1;  // versículo atual
  ul := FArvore.TopItem; // nó do livro atual
  uc := FArvore.TopItem.GetFirstChild; // nó do capítulo atual
  qv := 1; // índice no vetor QVersiculos
  qc := 1; // índice no vetor QCapitulos

  sc[0] := 0;  // contagem de versículos por situação no capítulo
  sc[1] := 0;
  sc[2] := 0;
  sc[3] := 0;

  sl[0] := 0;  // contagem de versículos por situação no livro
  sl[1] := 0;
  sl[2] := 0;
  sl[3] := 0;

  //DesabilitarEventosRolagem;
  marcador := FTblPares.FieldByName('pare_id').AsString;
  VersiculoInicial;

  while not FTblPares.EOF do
  begin
    if v > QVersiculos[qv] then // novo capítulo
    begin
      { atualizando nó do capítulo }
      //if qv = 69 then
      //  MessageDlg('sc', format('[%d][%d][%d][%d] - QVersiculos[%d]=%d', [sc[0],sc[1],sc[2],sc[3], qv, QVersiculos[qv]]), mtInformation, [mbOK], 0);
      b := false;
      for s:=0 to 3 do // todos os versículos estão na mesma situação?
        if sc[s] = QVersiculos[qv] then
        begin
          uc.ImageIndex := s; uc.SelectedIndex := uc.ImageIndex + 4;
          b := true;
          break;
        end;
      if not b then // várias situacoes diferentes
      begin
        if sc[2] > 0 then //(sc[0] = 0) and (sc[1] = 0) then
          uc.ImageIndex := 2
        else
          uc.ImageIndex := 1;
        uc.SelectedIndex := uc.ImageIndex + 4;
      end;
      sc[0] := 0;
      sc[1] := 0;
      sc[2] := 0;
      sc[3] := 0;
      inc(sl[uc.ImageIndex]);

      v := 1;
      inc(c);
      if c > QCapitulos[qc] then // novo livro
      begin
        { atualizando nó do livro }
        //if qc = 27 then
        //  MessageDlg('sl', format('[%d][%d][%d][%d] - [%d]', [sl[0],sl[1],sl[2],sl[3], QCapitulos[qc]]), mtInformation, [mbOK], 0);
        b := false;
        for s:=0 to 3 do // todos os capítulos estão na mesma situação?
          if sl[s] = QCapitulos[qc] then
          begin
            ul.ImageIndex := s; ul.SelectedIndex := ul.ImageIndex + 4;
            b := true;
            break;
          end;
        if not b then // várias situações diferentes
        begin
          if sl[2] > 0 then //(sl[0] = 0) and (sl[1] = 0) then
            ul.ImageIndex := 2
          else
            ul.ImageIndex := 1;
          ul.SelectedIndex := ul.ImageIndex + 4;
        end;
        sl[0] := 0;
        sl[1] := 0;
        sl[2] := 0;
        sl[3] := 0;

        c := 1;
        inc(l);
        inc(qc);
        ul := ul.GetNextSibling;
        uc := ul.GetFirstChild;
      end else
        uc := uc.GetNextSibling;

      n := uc.GetFirstChild;
      inc(qv);
    end;
    inc(sc[Situacao]);
    n.ImageIndex := Situacao;
    n.SelectedIndex := n.ImageIndex + 4;

    VersiculoSeguinte;
    inc(v);
    n := n.GetNextSibling;
  end;

  { atualizando o último capítulo - Apocalipse 22 }
  b := false;
  for s:=0 to 3 do // todos os versículos estão na mesma situação?
    if sc[s] = QVersiculos[qv] then
    begin
      uc.ImageIndex := s; uc.SelectedIndex := uc.ImageIndex + 4;
      b := true;
      break;
    end;
  if not b then // várias situacoes diferentes
  begin
    if sc[2] > 0 then //(sc[0] = 0) and (sc[1] = 0) then
      uc.ImageIndex := 2
    else
      uc.ImageIndex := 1;

    uc.SelectedIndex := uc.ImageIndex + 4;
  end;

  { atualizando último livro - Apocalipse }
  b := false;
  inc(sl[Situacao]);
  //MessageDlg('sl', format('[%d][%d][%d][%d] - [%d]', [sl[0],sl[1],sl[2],sl[3], QCapitulos[qc]]), mtInformation, [mbOK], 0);
  for s:=0 to 3 do // todos os capítulos estão na mesma situação?
    if sl[s] = QCapitulos[qc] then
    begin
      ul.ImageIndex := s; ul.SelectedIndex := ul.ImageIndex + 4;
      b := true;
      break;
    end;
  if not b then // várias situações diferentes
  begin
    if sl[2] > 0 then //(sl[0] = 0) and (sl[1] = 0) then
      ul.ImageIndex := 2
    else
      ul.ImageIndex := 1;
    ul.SelectedIndex := ul.ImageIndex + 4;
  end;

  IrPara(marcador);
  //HabilitarEventosRolagem;
end;

procedure TProjeto.AtualizarArvore(id: string);
var
  l, c, v, i: smallint;
  al, ac: array[0..3] of smallint;
  nl, nc, nv, n: TTreeNode;
  b: boolean;
begin
  if not assigned(FArvore) then
    exit;

  SScanf(id, '%d,%d,%d', [@l, @c, @v]);

  al[0] := 0; al[1] := 0; al[2] := 0; al[3] := 0;
  ac[0] := 0; ac[1] := 0; ac[2] := 0; ac[3] := 0;

  nl := FArvore.Items[0];
  for i:=l-39-1 downto 1 do
    nl := nl.GetNextSibling;

  nc := nl.GetFirstChild;
  for i:=c-1 downto 1 do
  begin
    inc(al[nc.ImageIndex]);
    nc := nc.GetNextSibling;
  end;

  nv := nc.GetFirstChild;
  for i:=v-1 downto 1 do
  begin
    inc(ac[nv.ImageIndex]);
    nv := nv.GetNextSibling;
  end;

  if nv.ImageIndex <> Situacao then
  begin
    { atualizando versículo }
    nv.ImageIndex := Situacao; nv.SelectedIndex := nv.ImageIndex + 4;
    { atualizando capítulo }
    n := nv;
    while assigned(n) do // percorrendo últimos versículos do capítulo
    begin
      inc(ac[n.ImageIndex]);
      n := n.GetNextSibling;
    end;

    b := false;
    for i:=0 to 3 do // todos os versículos estão na mesma situação?
    begin
      if ac[i] = QVersiculos[ QCapLivros[l-39]+c-1 ] then
      begin
        nc.ImageIndex := i; nc.SelectedIndex := nc.ImageIndex + 4;
        b := true;
        break;
      end;
    end;
    if not b then // várias situações diferentes
    begin
      if ac[2] > 0 then
        nc.ImageIndex := 2
      else
        nc.ImageIndex := 1;
      nc.SelectedIndex := nc.ImageIndex + 4;
    end;
    { atualizando livro }
    n := nc;
    while assigned(n) do // percorrendo últimos capítulos do livro
    begin
      inc(al[n.ImageIndex]);
      n := n.GetNextSibling;
    end;

    b := false;
    for i:=0 to 3 do // todos os versículos estão na mesma situação?
    begin
      if al[i] = QCapitulos[l-39] then
      begin
        nl.ImageIndex := i; nl.SelectedIndex := nl.ImageIndex + 4;
        b := true;
        break;
      end;
    end;
    if not b then // várias situações diferentes
    begin
      if al[2] > 0 then
        nl.ImageIndex := 2
      else
        nl.ImageIndex := 1;
      nl.SelectedIndex := nl.ImageIndex + 4;
    end;
  end;
end;

constructor TProjeto.Criar;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    FAVersiculo[v]   := nil;
    FADicStrong[v]   := nil;
    FADicMorfo[v]    := nil;
    FACamposTexto[v] := 0;
  end;

  FParesAntigos       := nil;
  FOnNovoVersiculo    := nil;
  FOnAlterarVersiculo := nil;
  FSugeridor          := nil;
  FTblPares           := nil;
  FTblInfo            := nil;

  FTemporizador          := TTimer.Create(nil);
  FTemporizador.Enabled  := false;
  FTemporizador.Interval := 600;
  FTemporizador.OnTimer  := @OnExibirDefinicao;

  FMemoVersiculo := TMemoVersiculo.Criar;
  FMemoVersiculo.QuandoModificarVersiculo := @OnAlterarTextoVersiculo;
end;

constructor TProjeto.Criar(paineis: array of TScrollbox; navegador: TTreeView;
  rsituacao: TRadioGroup; rcomentarios: TMemo);
var
  v: integer;
begin
  Criar;

  for v:=low(paineis) to high(paineis) do
    if (v <= integer(high(TTipoTextoBiblico))) and assigned(paineis[v]) then
      NovoObjetoVersiculo(paineis[v], TTipoTextoBiblico(v));

  Arvore              := navegador;
  FRadioGroupSituacao := rsituacao;
  FMemoComentarios    := rcomentarios;
end;

destructor TProjeto.Destruir;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].Destruir;

    if assigned(FADicStrong[v]) then
    begin
      FADicStrong[v].Close;
      FADicStrong[v].DataBase.Close;
      TSQLite3Connection(FADicStrong[v].DataBase).Transaction.Free;
      FADicStrong[v].DataBase.Free;
      FADicStrong[v].Free;
      FADicStrong[v] := nil;
    end;

    if assigned(FADicMorfo[v]) then
    begin
      FADicMorfo[v].Close;
      FADicMorfo[v].DataBase.Close;
      TSQLite3Connection(FADicMorfo[v].DataBase).Transaction.Free;
      FADicMorfo[v].DataBase.Free;
      FADicMorfo[v].Free;
      FADicMorfo[v] := nil;
    end;
  end;

  if assigned(FParesAntigos) then
    FParesAntigos.Free;

  FSugeridor.Free;
  FTemporizador.Free;
  FMemoVersiculo.Destruir;
end;

procedure TProjeto.Abrir(Nome: string);
var
  t: TTipoTextoBiblico;
  s: string;
  f: TFont;
begin
  if not FileExists(Nome) then
  begin
    MessageDlg('Abrir projeto', format('O projeto selecionado não existe: %s', [Nome]), mtError, [mbOK], 0);
    exit;
  end;

  FTblPares := CriarObjetoTabela(Nome, 'pares', 'pare_id');
  FTblInfo  := CriarObjetoTabela(Nome, 'info', 'id');
  DesabilitarEventosRolagem;
  FSugeridor := TGerSugestoes.Criar(Nome);

  FTblPares.Open;
  FTblInfo.Open;

  f := TFont.Create;
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
  begin
    if not assigned(FAVersiculo[t]) then
       continue;

    { definindo fontes }
    s := ObterInfo(format('fonte%d.tamanho', [t]));
    if s = '' then
    begin
      f.Name := 'default';
      f.Size := 12;
    end else begin
      f.Name := ObterInfo(format('fonte%d.nome', [t]));
      f.Size := StrToInt(s);
    end;
    FAVersiculo[t].Fonte := f;

    { definindo dicionários }
    AtribuirDicStrong(ObterInfo(format('dicstrong%d', [t])), t);
    AtribuirDicMorfo(ObterInfo(format('dicmorfo%d', [t])), t);
  end;
  f.free;

  FACamposTexto[tbOrigem]    := FTblPares.FindField('pare_texto_origem').Index;
  FACamposTexto[tbDestino]   := FTblPares.FindField('pare_texto_destino').Index;
  FACamposTexto[tbConsulta1] := FTblPares.FindField('pare_texto_consulta1').Index;
  FACamposTexto[tbConsulta2] := FTblPares.FindField('pare_texto_consulta2').Index;

  if assigned(FArvore) then
  begin
    AtualizarArvore;
    //FArvore.FullCollapse;
    FArvore.Enabled := true;
  end;

  if assigned(FMemoComentarios) then
    FMemoComentarios.Enabled := true;
  if assigned(FRadioGroupSituacao) then
    FRadioGroupSituacao.Enabled := true;

  IrPara(ObterInfo('marcador'));
  PosRolagemVersiculo(nil);
  HabilitarEventosRolagem;

  FAtivo := true;
end;

procedure TProjeto.Fechar(_Commit: boolean);
begin
  PreRolagemVersiculo(nil);

  if _Commit then
    Commit
  else
    Rollback;

  AtribuirInfo('marcador', FTblPares.FieldByName('pare_id').AsString);
  Commit;

  FTblPares.Close;
  FTblPares.Free;
  FTblPares := nil;

  FTblInfo.Close;
  FTblInfo.Free;
  FTblInfo := nil;

  FMemoVersiculo.Desativar;

  if assigned(FArvore) then
  begin
    //FArvore.FullCollapse;
    FArvore.Enabled := false;
    FArvore.Items.Clear;
  end;
  if assigned(FMemoComentarios) then
  begin
    FMemoComentarios.Enabled := false;
    FMemoComentarios.Caption := '';
  end;
  if assigned(FRadioGroupSituacao) then
  begin
    FRadioGroupSituacao.Enabled := false;
    FRadioGroupSituacao.ItemIndex := 0;
  end;

  FAtivo := false;
end;

procedure TProjeto.Commit;
begin
  if assigned(FSugeridor) then
    FSugeridor.Commit;
  if assigned(FTblPares) then
    FTblPares.ApplyUpdates;
  if assigned(FTblInfo) then
    FTblInfo.ApplyUpdates;
end;

procedure TProjeto.Rollback;
begin
  FTblPares.ClearUpdates;
  FTblInfo.ClearUpdates;
  FSugeridor.Rollback;
end;

procedure TProjeto.IrPara(Referencia: string);
begin
  FTblPares.Locate('pare_id', Referencia, []);
end;

procedure TProjeto.VersiculoSeguinte;
begin
  if not FTblPares.EOF then
    FTblPares.Next;
  if SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoAnterior;
begin
  if not FTblPares.BOF then
    FTblPares.Prior;
  if SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoInicial;
begin
  if not FTblPares.BOF then
    FTblPares.First;
  if SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoFinal;
begin
  if not FTblPares.EOF then
    FTblPares.Last;
  if SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.Atualizar;
begin
  PosRolagemVersiculo(FTblPares);
end;

procedure TProjeto.Limpar;
begin
  if assigned(FAVersiculo[tbOrigem]) then
    FAVersiculo[tbOrigem].LimparAssociacoes;
end;

procedure TProjeto.Salvar;
begin
  PreRolagemVersiculo(nil);
  AtribuirInfo('marcador', FTblPares.FieldByName('pare_id').AsString);
  Commit;

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbDestino]) then
  begin
    FAVersiculo[tbOrigem].Modificado  := false;
    FAVersiculo[tbDestino].Modificado := false;
  end;
  //AtualizarArvore;
end;

procedure TProjeto.SugerirAssociacao;
begin
  FSugeridor.SugerirAssociacoes(FAVersiculo[tbOrigem]);

  if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
    FRadioGroupSituacao.ItemIndex := 1 // associando
end;

procedure TProjeto.RecriarBaseSugestoes;
begin
  RecriarBaseSugestoes(nil);
end;

procedure TProjeto.RecriarBaseSugestoes(pb: TProgressBar);
var
  marcador: string;
  pares: TStringList;
  p: smallint;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  FSugeridor.LimparBaseAssociacoes;

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := 7956;
      pb.Step := 1;
      pb.Visible := true;
    end;

    FAVersiculo[tbOrigem].Ativo := false;
    FAVersiculo[tbDestino].Ativo := false;
    PreRolagemVersiculo(nil);

    DesabilitarEventosRolagem;
    marcador := GetID;
    VersiculoInicial;
    while not FTblPares.EOF do
    begin
      if length(FTblPares.FieldByName('pare_pares').AsString) > 0 then
      begin
        FAVersiculo[tbOrigem].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
        FAVersiculo[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
        FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;

        pares := FAVersiculo[tbOrigem].GetListaPares(tlMetaDados);

        for p:=0 to pares.Count-1 do
          if (p mod 2) = 0 then // pares estão alternados na lista
            FSugeridor.InserirPar(pares.Strings[p], pares.Strings[p+1]);

        pares.Free;
      end;
      if assigned(pb) then
      begin
         pb.StepIt;
         if (pb.Position mod 50) = 0 then
           Application.ProcessMessages;
      end;
      VersiculoSeguinte;
    end;
  finally
    FAVersiculo[tbOrigem].Ativo := true;
    FAVersiculo[tbDestino].Ativo := true;
    IrPara(marcador);
    HabilitarEventosRolagem;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
       pb.Visible := false;
  end;
end;

procedure TProjeto.ImportarModuloTheWord(arquivo: string;
  texto: TTipoTextoBiblico);
begin
  ImportarModuloTheWord(arquivo, texto, nil);
end;

procedure TProjeto.ImportarModuloTheWord(arquivo: string;
  texto: TTipoTextoBiblico; pb: TProgressBar);
var
  modulo: TStringList;
  i, offset: smallint;
  reVazio, reComments, {reShortTitle,} reVerseRules, reVerseRule: IRegex;
  mtVerseRules: IMatch;
  verseRulesDe: TList;
  verseRulesPara: TStringList;
  propriedades: TStringStream;
  m: smallint;
begin
  if AnsiEndsText('.ont', arquivo) then
    offset := 23145 // saltando o velho testamento
  else if AnsiEndsText('.nt', arquivo) then
    offset := 0
  else
    exit; // não parece ser um módulo do theWord

  try
    modulo := TStringList.Create;
    modulo.LoadFromFile(arquivo);

    if modulo.Count < 7956 then
    begin
      MessageDlg('Erro', 'Arquivo inválido, precisa ter ao menos 7956 linhas', mtError, [mbOK], 0);
      exit;
    end;

    reVazio        := RegexCreate('^\s*$', [rcoUTF8]);
    reComments     := RegexCreate('#.*$', [rcoUTF8]);
    //reShortTitle   := RegexCreate('(short.title)\s*=\s*(.*)$', [rcoUTF8]);
    reVerseRules   := RegexCreate('^\s*verse.rule\s*=\s*"(.*)"(?!")\s*"(.*)"(?!")', [rcoUTF8]);
    FrmEscolherVerseRules.Reset;
    verseRulesDe   := TList.Create;
    verseRulesPara := TStringList.Create;

    propriedades := TStringStream.Create('');
    for i:=offset+7956+1 to modulo.Count-1 do
    begin
      { eliminando comentários }
      modulo.Strings[i] := reComments.Replace(modulo.Strings[i], '');

      //modulo.Strings[i] := reShortTitle.Replace(modulo.Strings[i], '$1=$2i');

      if reVazio.IsMatch(modulo.Strings[i]) then
        continue;

      propriedades.WriteString(modulo.Strings[i] + #13#10);

      mtVerseRules := reVerseRules.Match(modulo.Strings[i]);
      if mtVerseRules.Success then
         FrmEscolherVerseRules.AddVerseRule(mtVerseRules.Groups[1].Value, mtVerseRules.Groups[2].Value);

      //if mtVerseRules.Success {and AnsiStartsStr('^', mtVerseRules.Groups[2])} then
      //begin
      //  verseRulesDe.Add(RegexCreate(mtVerseRules.Groups[1].Value, [rcoUTF8]));
      //  verseRulesPara.Add(mtVerseRules.Groups[2].Value);
      //  //for m:=0 to mtVerseRules.Groups.Count-1 do
      //  //  MessageDlg('Erro', format('%d:%s', [m, mtVerseRules.Groups[m].Value]), mtError, [mbOK], 0);
      //end;
    end;

    if ((texto = tbOrigem)  and (ObterInfo('propriedades.origem')  = '')) or // não importar propriedades quando já existem
       ((texto = tbDestino) and (ObterInfo('propriedades.destino') = '')) then
    begin
      case texto of
        tbOrigem:  AtribuirInfo('propriedades.origem', propriedades.DataString);
        tbDestino: AtribuirInfo('propriedades.destino', propriedades.DataString);
      end;
    end;
    propriedades.Free;

    if (FrmEscolherVerseRules.VerseRuleDe.Count > 0) and (FrmEscolherVerseRules.ShowModal = mrOK) then
    begin
      for m:=0 to FrmEscolherVerseRules.ckgVerseRules.Items.Count-1 do
        if FrmEscolherVerseRules.ckgVerseRules.Checked[m] then
        begin
          reVerseRule := RegexCreate(FrmEscolherVerseRules.VerseRuleDe[m], [rcoUTF8]);
          verseRulesDe.Add( @reVerseRule );
          verseRulesPara.Add( FrmEscolherVerseRules.VerseRulePara[m] );
          DebugLn('regex criado (%s -------- %s)', [FrmEscolherVerseRules.VerseRuleDe[m], FrmEscolherVerseRules.VerseRulePara[m]]);
        end;
    end;
    //modulo.SaveToFile(arquivo + '.txt');
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := 7956;
      pb.Step := 1;
      pb.Visible := true;
    end;

    DesabilitarEventosRolagem;
    VersiculoInicial;
    for i:=offset to offset + 7956 do
    begin
      { aplicando verse.rules }
      for m:=0 to verseRulesDe.Count-1 do
        modulo[i] := IRegex(verseRulesDe[m]^).Replace(modulo[i], verseRulesPara[m]);

      InserirVersiculoTexto(modulo[i], texto);
      VersiculoSeguinte;
      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 75) = 0 then
           Application.ProcessMessages;
      end;
    end;
    VersiculoInicial;
    HabilitarEventosRolagem;
  finally
    modulo.Free;
    if assigned(pb) then
       pb.Visible:=false;
    verseRulesDe.Free;
    verseRulesPara.Free;
  end;
  //FTblPares.ApplyUpdates;
end;

procedure TProjeto.NovoObjetoVersiculo(owner: TScrollBox;
  texto: TTipoTextoBiblico);
begin
  FAVersiculo[texto] := TVersiculo.Criar(owner);
  owner.Tag := ptrint(FAVersiculo[texto]);
  owner.OnResize := @OnRedimensionarVersiculo;
  owner.OnDblClick := @OnDblClickVersiculo;

  //FAVersiculo[texto].MostrarDicas := true;
  if texto in [tbOrigem, tbDestino] then
    FAVersiculo[texto].OnClick := @SintagmaOnClick
  else if texto in [tbConsulta1, tbConsulta2] then
    FAVersiculo[texto].CorDesassociado := clWindowText;

  //if texto = tbOrigem then
  //  FAVersiculo[texto].StrongMorfoComoChave := true;

  if (texto = tbDestino) and assigned(FAVersiculo[tbOrigem]) then
    FAVersiculo[tbDestino].VersiculoPar := FAVersiculo[tbOrigem]
  else if (texto = tbOrigem) and assigned(FAVersiculo[tbDestino]) then
    FAVersiculo[tbOrigem].VersiculoPar := FAVersiculo[tbDestino];
end;

procedure TProjeto.ExportarTextoDestinoComStrongs(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportarTextoDestinoComStrongs(arquivo, nil, opcoes);
end;

procedure TProjeto.ExportarTextoDestinoComStrongs(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao);
var
  ONT: TStringList;
  marcador: string;
begin
  if not assigned(FAVersiculo[tbOrigem]) or not assigned(FAVersiculo[tbDestino]) then
    exit;

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := 7956;
      pb.Step := 1;
      pb.Visible := true;
    end;
    FAVersiculo[tbOrigem].Ativo := false;
    FAVersiculo[tbDestino].Ativo := false;
    ONT := TStringList.Create;
    PreRolagemVersiculo(nil);

    with FTblPares do
    begin
      DesabilitarEventosRolagem;
      marcador := GetID;
      VersiculoInicial;
      while not FTblPares.EOF do
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) = 0 then
          ONT.Add(FTblPares.Fields[FACamposTexto[tbDestino]].AsString)
        else
        begin
          FAVersiculo[tbOrigem].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
          FAVersiculo[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
          ONT.Add(FAVersiculo[tbDestino].GetLinhaONT(
            (oeExportarMorfologia in opcoes),
            (oeExportarNAComoItalicos in opcoes),
            (oeStrongsReutilizados in opcoes),
            (oeStrongsNaoTraduzidos in opcoes))
          );
        end;

        if (oeExportarComentarios in opcoes) and (length(FTblPares.FieldByName('pare_comentarios').AsString) > 0) then
          ONT.Strings[ONT.Count-1] := ONT.Strings[ONT.Count-1] + '<RF>' +
            AnsiReplaceStr(FTblPares.FieldByName('pare_comentarios').AsString, #13#10, '<CM>') + '<Rf>';

        if assigned(pb) then
        begin
          pb.StepIt;
          if (pb.Position mod 50) = 0 then
             Application.ProcessMessages;
        end;
        VersiculoSeguinte;
      end;
      IrPara(marcador);
      HabilitarEventosRolagem;
    end;

    if pos(ONT.Strings[0], '<WG') = 0 then { adicionando tag inócua para que o theWord exiba definições mesmo sem associações no primeiro versículo }
      ONT.Strings[0] := ONT.Strings[0] + '<_MORPH_>';

    ONT.Add('');

    ONT.Add(ResgatarInfo('propriedades.destino'));
    //ONT.Add('lang=por');
    //ONT.Add('short.title=ACF2007+');
    //ONT.Add('description=Almeida Corrigida Fiel Edição 2007, com números de Strong e morfologia');
    //ONT.Add('version.major=1');
    //ONT.Add('version.minor=0');
    //ONT.Add('version.date=2011-01-24');
    //ONT.Add('creator=Costas Stergiou (root@theword.gr), Rúbio Terra (rubio.terra@gmail.com)');
    //ONT.Add('font=Gentium,GentiumAlt,TITUS Cyberbit Basic,Vusillys,Cardo,Georgia Greek,Galatia SIL,Galilee Unicode Gk,@Code2000,Palatino Linotype,Athena Unicode,Athena');

    //ONT.Add('verse.rule="([^\s<>]+)(<W(G[^>]+)>(?:<WT([^>]+)>)?)(<W(G[^>]+)><WT([^>]+)>)?" "<wt><a href=_ORIGWORD_$1|_STRONG_$3|_MORPH_$4|_STRONG2_$6|_MORPH2_$7|_NOLINK_>$1</a>$2$5"');

    ONT.Strings[0] := #239#187#191 + ONT.Strings[0]; // adicionando BOM
    ONT.SaveToFile(arquivo);
  finally
    ONT.Destroy;
    FAVersiculo[tbOrigem].Ativo := true;
    FAVersiculo[tbDestino].Ativo := true;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;
end;

procedure TProjeto.ExportarTextoInterlinear(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportarTextoInterlinear(arquivo, nil, opcoes);
end;

procedure TProjeto.ExportarTextoInterlinear(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao);
var
  ONT: TStringList;
  marcador: string;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := 7956;
      pb.Step := 1;
      pb.Visible := true;
    end;
    FAVersiculo[tbOrigem].Ativo := false;
    FAVersiculo[tbDestino].Ativo := false;
    ONT := TStringList.Create;
    PreRolagemVersiculo(nil);

    with FTblPares do
    begin
      DesabilitarEventosRolagem;
      marcador := GetID;
      VersiculoInicial;
      while not FTblPares.EOF do
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) = 0 then
          ONT.Add(FTblPares.Fields[FACamposTexto[tbOrigem]].AsString)
        else
        begin
          FAVersiculo[tbOrigem].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
          FAVersiculo[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
          ONT.Add(FAVersiculo[tbOrigem].GetLinhaInterlinear);
        end;
        if pb <> nil then
        begin
          pb.StepIt;
          if (pb.Position mod 50) = 0 then
             Application.ProcessMessages;
        end;
        VersiculoSeguinte;
      end;
      IrPara(marcador);
      HabilitarEventosRolagem;
    end;

    ONT.Add('');

    ONT.Add(ResgatarInfo('propriedades.origem'));

    //ONT.Add('lang=por');
    //ONT.Add('short.title=ACF2007i');
    //ONT.Add('description=Almeida Corrigida Fiel Edição 2007, interlinear');
    //ONT.Add('version.major=1');
    //ONT.Add('version.minor=0');
    ////ONT.Add('version.date=2011-01-24');
    ////ONT.Add('creator=Costas Stergiou (root@theword.gr), Rúbio Terra (rubio.terra@gmail.com)');
    //ONT.Add('font=Gentium,GentiumAlt,TITUS Cyberbit Basic,Vusillys,Cardo,Georgia Greek,Galatia SIL,Galilee Unicode Gk,@Code2000,Palatino Linotype,Athena Unicode,Athena');
    //ONT.Add('verse.rule="((^|(\ ))([αβγδεζηθικλμνξοπρστυφχψως\[\]]+))" "$3<wt>$4"');
    //ONT.Add('verse.rule="<wt>([αβγδεζηθικλμνξοπρστυφχψως\[\]]+)(.*?)(<W(G\d+)>)?(<W(G\d+)>)?(<W(G\d+)>)?(<WT([^\ >]+)>)" "<wt><a href=_ORIGWORD_$1|_MORPH_$10|_STRONG_$4|_STRONG2_$6|_STRONG3_$8|_NOLINK_>$1</a>$2$3$5$7$9"');

    ONT.Strings[0] := #239#187#191 + ONT.Strings[0]; // adicionando BOM
    ONT.SaveToFile(arquivo);
  finally
    ONT.Destroy;
    FAVersiculo[tbOrigem].Ativo := true;
    FAVersiculo[tbDestino].Ativo := true;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;
end;

procedure TProjeto.ExportarConcordancia(arquivo: string;
  opcoes: TOpcoesExportacao);
begin
  ExportarConcordancia(arquivo, nil, opcoes);
end;

procedure TProjeto.ExportarConcordancia(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao);
var
  Concordancia: TConcordancia;
  tblTopicos: TSqlite3Dataset;
  tblConteudo: TSqlite3Dataset;
  marcador: string;
  pares: TStringList;
  s: string;
begin
  if not FileExists(arquivo) then
  begin
    try
      CopiarArquivo('concordancia.modelo', arquivo);
      //CopiarArquivo('D:\Dropbox\Programas\iBiblia\concordancia.modelo', arquivo);
    except
      MessageDlg('Erro', 'Falha na criação do arquivo:'#13#10 + arquivo, mtError, [mbOK], 0);
      exit;
    end;
  end;

  tblConteudo := CriarObjetoTabela(arquivo, 'content', 'topic_id');
  tblConteudo.Open;
  tblTopicos := CriarObjetoTabela(arquivo, 'topics', 'id');
  tblTopicos.Open;

  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := 7956;
      pb.Step := 1;
      pb.Visible := true;
    end;
    Concordancia := TConcordancia.Create;
    Concordancia.Detalhada := oeConcordDetalhada in opcoes;

    FAVersiculo[tbOrigem].Ativo := false;
    FAVersiculo[tbDestino].Ativo := false;
    PreRolagemVersiculo(nil);

    DesabilitarEventosRolagem;
    marcador := GetID;
    VersiculoInicial;
    while not FTblPares.EOF do
    begin
      if length(FTblPares.FieldByName('pare_pares').AsString) > 0 then
      begin
        FAVersiculo[tbOrigem].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
        FAVersiculo[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
        FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;

        pares := FAVersiculo[tbOrigem].GetListaPares(tlTudo);
        Concordancia.AdicionarLocucao(pares, GetID);
        pares.Free;
      end;
      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 10) = 0 then
          Application.ProcessMessages;
      end;
      VersiculoSeguinte;
    end;

    tblTopicos.First;
    while not tblTopicos.EOF do
    begin
      if not tblConteudo.Locate('topic_id', tblTopicos.FieldByName('id').AsInteger, []) then
        continue;

      s := Concordancia.GetStrongRTF(tblTopicos.FieldByName('strong_orig_word').AsString, tblTopicos.FieldByName('subject').AsString);
      if s <> '' then
      begin
        tblConteudo.Edit;
        tblConteudo.FieldByName('data').AsString := s;
        tblConteudo.Post;
      end;

      tblTopicos.Next;
    end;
  finally
    tblConteudo.ApplyUpdates;
    tblConteudo.Close;
    tblTopicos.Close;
    Concordancia.Free;
    FAVersiculo[tbOrigem].Ativo := true;
    FAVersiculo[tbDestino].Ativo := true;
    IrPara(marcador);
    HabilitarEventosRolagem;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;
end;

procedure TProjeto.AtribuirFonteTexto(fonte: TFont; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    if t in textos then
    begin
      FAVersiculo[t].Fonte := fonte;
      //PosRolagemVersiculo(nil);
      AtribuirInfo(format('fonte%d.nome', [t]), fonte.Name);
      AtribuirInfo(format('fonte%d.tamanho', [t]), IntToStr(fonte.Size));
    end;
end;

procedure TProjeto.LimparTexto(texto: TTipoTextoBiblico);
begin
  with FTblPares do
  begin
    DesabilitarEventosRolagem;
    VersiculoInicial;
    while not FTblPares.EOF do
    begin
      FTblPares.Edit;
      FTblPares.Fields[FACamposTexto[texto]].AsString := '';
      FTblPares.Post;
      VersiculoSeguinte;
    end;
    HabilitarEventosRolagem;
  end;
end;

function TProjeto.ObterFonteTexto(texto: TTipoTextoBiblico): TFont;
begin
   if assigned(FAVersiculo[texto]) then
     result := FAVersiculo[texto].Fonte
   else
     result := nil;
end;

procedure TProjeto.AtribuirDicStrong(dic: string; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
  begin
    if t in textos then
    begin
      AtribuirDicStrong(dic, t);
      AtribuirInfo(format('dicstrong%d', [t]), dic);
    end;
  end;
end;

procedure TProjeto.AtribuirDicMorfo(dic: string; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
  begin
    if t in textos then
    begin
      AtribuirDicMorfo(dic, t);
      AtribuirInfo(format('dicmorfo%d', [t]), dic);
    end;
  end;
end;

procedure TProjeto.AtribuirInfo(info, valor: string);
begin
  if not AtualizarInfo(info, valor) then
    InserirInfo(info, valor);
end;

function TProjeto.ObterInfo(info: string): string;
begin
  result := ResgatarInfo(info);
end;

function TProjeto.ObterTextoVersiculo(texto: TTipoTextoBiblico): string;
begin
  result := ObterTextoVersiculo('', texto);
end;

function TProjeto.ObterTextoVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
var
  marcador: string;
begin
  if Referencia <> '' then
  begin
    DesabilitarEventosRolagem;
    marcador := FTblPares.FieldByName('pare_id').AsString;
    IrPara(Referencia);
  end;

  result := FTblPares.Fields[FACamposTexto[texto]].AsString;

  if Referencia <> '' then
  begin
    IrPara(marcador);
    HabilitarEventosRolagem;
  end;
end;

function TProjeto.ObterTextoSimplesVersiculo(texto: TTipoTextoBiblico): string;
begin
  result := ObterTextoSimplesVersiculo('', texto);
end;

function TProjeto.ObterTextoSimplesVersiculo(Referencia: string;
  texto: TTipoTextoBiblico): string;
var
  varredorXML: TVarredorXML;
  s: TTagSintagma;
begin
  result := FAVersiculo[texto].TextoSimples;
  exit;
  result := '';
  varredorXML := TVarredorXML.Criar(FTblPares.Fields[FACamposTexto[texto]].AsString);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if s.tipo = tsTag then
    begin
      if s.valor = '<RF>' then
      begin
        varredorXML.LerAteTag(s, '<Rf>');
        s.tipo := tsMetaDado;
        continue;
      end else if AnsiStartsStr('<TS', s.valor) then
      begin
        varredorXML.LerAteTag(s, '<Ts>');
        s.tipo := tsMetaDado;
        continue;
      end;
    end;
    if s.tipo in [tsEspaco, tsSintagma, tsPontuacao] then
      result := result + s.valor;
  end;
  varredorXML.Destruir;
end;

end.

