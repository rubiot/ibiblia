unit Sugestao;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sqldb, sqlite3conn, Versiculo,
  Dialogs, LazLogger, PCRE, Sintagma, ONTTokenizer, ExportarProjeto;

type

  { TGerSugestoes }

  TGerSugestoes = class
  private
    FConn: TSQLConnection;
    FConnTmp: TSQLite3Connection;
    FConnVerbos: TSQLite3Connection;
    FQryFormaVerbo: TSQLQuery;
    FQry: TSQLQuery;
    FQryRemoverPar: TSQLQuery;
    FQryObterLocucaoOrigem: TSQLQuery;
    FQryObterLocucaoDestino: TSQLQuery;
    FQryMaxLoriID: TSQLQuery;
    FQryMaxLdesID: TSQLQuery;
    FQryInserirLori: TSQLQuery;
    FQryInserirLdes: TSQLQuery;
    FQryInserirSlor: TSQLQuery;
    FQryInserirSdes: TSQLQuery;
    FQryInserirTrad: TSQLQuery;
    FQryAtualizarTrad: TSQLQuery;
    FQrySugestoes: TSQLQuery;
    FQrySugestoesSemMorfo: TSQLQuery;
    FStrList: TStringList;
    FVerbosList: TStringList;
    FRecFormasVerbais: boolean;
    FRETirarMorfo: IRegex;
    function ObterLocucaoOrigem(locucao: string): integer;
    function LocalizarLocucaoOrigem(locucao: string): integer;
    function InserirLocucaoOrigem(locucao: string): integer;
    function ObterLocucaoDestino(locucao: string): integer;
    function LocalizarLocucaoDestino(locucao: string): integer;
    function InserirLocucaoDestino(locucao: string): integer;
    function AtualizarTraducao(lori_id, ldes_id: integer): boolean;
    function InserirTraducao(lori_id, ldes_id: integer): boolean;
    function IdentificarVerbo(forma: string): boolean;
    function NormalizarVerbo(locucao: string): string;
    function ObterSugestoes(chave: string): TSQLQuery;
  public
    constructor Criar(Owner: TComponent; conn: TSQLConnection);
    constructor Criar(db: string);
    destructor Destruir;
    property ReconhecerFormasVerbais: boolean read FRecFormasVerbais write FRecFormasVerbais;
    procedure InserirPar(origem, destino: string);
    procedure RemoverPar(origem, destino: string);
    procedure SugerirAssociacoes(v1: TVersiculo);
    procedure LimparBaseAssociacoes;
    procedure Commit;
    procedure Rollback;
    function GetTranslationAlternatives(syntagm: TSintagma): string;
  end;


implementation

{ TGerSugestoes }

constructor TGerSugestoes.Criar(Owner: TComponent; conn: TSQLConnection);
begin
  FConn := conn;
  FQry := TSQLQuery.Create(Owner);
  FQry.DataBase := FConn;

  FQryRemoverPar := TSQLQuery.Create(Owner);
  FQryRemoverPar.DataBase := FConn;
  FQryRemoverPar.SQL.Text := 'update traducoes set trad_ocorrencias = trad_ocorrencias - 1 where trad_lori_id = :lori and trad_ldes_id = :ldes';
  FQryRemoverPar.Prepare;

  FQryObterLocucaoOrigem := TSQLQuery.Create(Owner);
  FQryObterLocucaoOrigem.DataBase := FConn;
  FQryObterLocucaoOrigem.SQL.Text := 'select lori_id from locucoes_origem where lori_locucao = :locucao';
  FQryObterLocucaoOrigem.Prepare;

  FQryObterLocucaoDestino := TSQLQuery.Create(Owner);
  FQryObterLocucaoDestino.DataBase := FConn;
  FQryObterLocucaoDestino.SQL.Text := 'select ldes_id from locucoes_destino where ldes_locucao = :locucao';
  FQryObterLocucaoDestino.Prepare;

  FQryMaxLoriID := TSQLQuery.Create(Owner);
  FQryMaxLoriID.DataBase := FConn;
  FQryMaxLoriID.SQL.Text := 'select max(lori_id) + 1 from locucoes_origem';
  FQryMaxLoriID.Prepare;

  FQryMaxLdesID := TSQLQuery.Create(Owner);
  FQryMaxLdesID.DataBase := FConn;
  FQryMaxLdesID.SQL.Text := 'select max(ldes_id) + 1 from locucoes_destino';
  FQryMaxLdesID.Prepare;

  FQryInserirLori := TSQLQuery.Create(Owner);
  FQryInserirLori.DataBase := FConn;
  FQryInserirLori.SQL.Text := 'insert into locucoes_origem (lori_id, lori_qtde_sintagmas, lori_locucao) values (:id, :qt, :lc)';
  FQryInserirLori.Prepare;

  FQryInserirLdes := TSQLQuery.Create(Owner);
  FQryInserirLdes.DataBase := FConn;
  FQryInserirLdes.SQL.Text := 'insert into locucoes_destino (ldes_id, ldes_qtde_sintagmas, ldes_locucao) values (:id, :qt, :lc)';
  FQryInserirLdes.Prepare;

  FQryInserirSlor := TSQLQuery.Create(Owner);
  FQryInserirSlor.DataBase := FConn;
  FQryInserirSlor.SQL.Text := 'insert into sintagmas_locucoes_origem (slor_lori_id, slor_sori_id, slor_ordem) values (:id, :st, :od)';
  FQryInserirSlor.Prepare;

  FQryInserirSdes := TSQLQuery.Create(Owner);
  FQryInserirSdes.DataBase := FConn;
  FQryInserirSdes.SQL.Text := 'insert into sintagmas_locucoes_destino (slde_ldes_id, slde_sdes_id, slde_ordem) values (:id, :st, :od)';
  FQryInserirSdes.Prepare;

  FQryInserirTrad := TSQLQuery.Create(Owner);
  FQryInserirTrad.DataBase := FConn;
  FQryInserirTrad.SQL.Text := 'insert into traducoes (trad_lori_id, trad_ldes_id, trad_ocorrencias) values (:lori, :ldes, 1)';
  FQryInserirTrad.Prepare;

  FQryAtualizarTrad := TSQLQuery.Create(Owner);
  FQryAtualizarTrad.DataBase := FConn;
  FQryAtualizarTrad.SQL.Text := 'update traducoes set trad_ocorrencias = trad_ocorrencias + 1 where trad_lori_id = :lori and trad_ldes_id = :ldes';
  FQryAtualizarTrad.Prepare;

  FQrySugestoes := TSQLQuery.Create(Owner);
  FQrySugestoes.DataBase := FConn;
  FQrySugestoes.SQL.Text := 'select lori_locucao, ldes_locucao ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where slor_sori_id = :sori and slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id ' +
      'order by lori_qtde_sintagmas desc, trad_ocorrencias desc';
  FQrySugestoes.Prepare;

  FQrySugestoesSemMorfo := TSQLQuery.Create(Owner);
  FQrySugestoesSemMorfo.DataBase := FConn;
  FQrySugestoesSemMorfo.SQL.Text := 'select lori_locucao, ldes_locucao ' +
      'from sintagmas_locucoes_origem, traducoes, locucoes_origem, locucoes_destino ' +
      'where slor_sori_id like :sori and slor_ordem = 0 and slor_lori_id = trad_lori_id' +
      '  and slor_lori_id = lori_id and trad_ldes_id = ldes_id ' +
      'order by lori_qtde_sintagmas desc, trad_ocorrencias desc';
  FQrySugestoesSemMorfo.Prepare;

  if FileExists('formas_verbos.db3') then
  begin
    FConnVerbos := TSQLite3Connection.Create(nil);
    FConnVerbos.DatabaseName := 'formas_verbos.db3';
    FConnVerbos.Transaction := TSQLTransaction.Create(nil);
    FConnVerbos.StartTransaction;
    FConnVerbos.Open;

    FQryFormaVerbo := TSQLQuery.Create(Owner);
    FQryFormaVerbo.DataBase := FConnVerbos;
    FQryFormaVerbo.SQL.Text := 'select ver_verbo from verbo_forma, verbos where vfo_for_id = ' +
                            '(select for_id from formas where for_forma = :forma) and vfo_ver_id = ver_id';
    FQryFormaVerbo.Prepare;
  end
  else
    FQryFormaVerbo := nil;

  FStrList := TStringList.Create;
  FStrList.Delimiter := ';';
  FStrList.StrictDelimiter := true;

  FVerbosList := TStringList.Create;

  FRecFormasVerbais := true;

  FRETirarMorfo := RegexCreate('<wt.*?>', [rcoUTF8]);
end;

constructor TGerSugestoes.Criar(db: string);
begin
  FConnTmp := TSQLite3Connection.Create(nil);
  FConnTmp.DatabaseName := db;
  FConnTmp.Transaction := TSQLTransaction.Create(nil);
  FConnTmp.StartTransaction;
  //FConnTmp.Transaction.Action;
  FConnTmp.Open;
  Criar(nil, FConnTmp);
end;

destructor TGerSugestoes.Destruir;
begin
  if assigned(FConnTmp) then
  begin
    FConnTmp.Transaction.Free;
    FConnTmp.Close;
    FConnTmp.Free;
  end;
  FQry.Free;
  FQryRemoverPar.Free;
  FQryObterLocucaoOrigem.Free;
  FQryObterLocucaoDestino.Free;
  FQryMaxLoriID.Free;
  FQryMaxLdesID.Free;
  FQryInserirLori.Free;
  FQryInserirLdes.Free;
  FQryInserirSlor.Free;
  FQryInserirSdes.Free;
  FQryInserirTrad.Free;
  FQryAtualizarTrad.Free;
  FQrySugestoes.Free;
  FQrySugestoesSemMorfo.Free;

  if FQryFormaVerbo <> nil then
  begin
    FQryFormaVerbo.Close;
    FQryFormaVerbo.Free;
    FConnVerbos.Transaction.Free;
    FConnVerbos.Close;
    FConnVerbos.Free;
  end;

  FStrList.Free;
  FVerbosList.Free;
end;

procedure TGerSugestoes.InserirPar(origem, destino: string);
var
  lori_id, ldes_id : integer;
begin
  lori_id := ObterLocucaoOrigem(lowercase(AnsiReplaceStr(origem, '''', '''''')));
  ldes_id := ObterLocucaoDestino(lowercase(AnsiReplaceStr(destino, '''', '''''')));

  if not AtualizarTraducao(lori_id, ldes_id) then
    InserirTraducao(lori_id, ldes_id);
end;

procedure TGerSugestoes.RemoverPar(origem, destino: string);
var
  lori_id, ldes_id : integer;
begin
  lori_id := ObterLocucaoOrigem(lowercase(AnsiReplaceStr(origem, '''', '''''')));
  ldes_id := ObterLocucaoDestino(lowercase(AnsiReplaceStr(destino, '''', '''''')));

  if (lori_id > 0) and (ldes_id > 0) then
  begin
    FQryRemoverPar.Close;
    //FQry.SQL.Text := format('update traducoes set trad_ocorrencias = trad_ocorrencias - 1 where trad_lori_id = %d and trad_ldes_id = %d', [lori_id, ldes_id]);
    FQryRemoverPar.Params[0].AsInteger := lori_id;
    FQryRemoverPar.Params[1].AsInteger := ldes_id;
    FQryRemoverPar.ExecSQL;
  end;
end;

function TGerSugestoes.ObterLocucaoOrigem(locucao: string): integer;
begin
  result := LocalizarLocucaoOrigem(locucao);

  if result <= 0 then
    result := InserirLocucaoOrigem(locucao);
end;

function TGerSugestoes.LocalizarLocucaoOrigem(locucao: string): integer;
begin
  FQryObterLocucaoOrigem.Close;
  //FQry.SQL.Text:= 'select lori_id from locucoes_origem where lori_locucao = ''' + locucao + '''';
  FQryObterLocucaoOrigem.Params[0].AsString := locucao;
  FQryObterLocucaoOrigem.Open;

  result := FQryObterLocucaoOrigem.Fields[0].AsInteger;
end;

function TGerSugestoes.InserirLocucaoOrigem(locucao: string): integer;
var
  s: smallint;
begin
  FStrList.DelimitedText := locucao;

  // inserindo o registros da locução
  FQryMaxLoriID.Close;
  //FQry.SQL.Text:= 'select max(lori_id) + 1 from locucoes_origem';
  FQryMaxLoriID.Open;

  try
    result := FQryMaxLoriID.Fields[0].AsInteger;
  except
    result := 1;
  end;

  FQryInserirLori.Close;
  //FQry.SQL.Text:= format('insert into locucoes_origem (lori_id, lori_qtde_sintagmas, lori_locucao) values (%d, %d, ''%s'')', [result, FStrList.Count, locucao]);
  FQryInserirLori.Params[0].AsInteger := result;
  FQryInserirLori.Params[1].AsInteger := FStrList.Count;
  FQryInserirLori.Params[2].AsString := locucao;
  FQryInserirLori.ExecSQL;

  // sintagmas locucao idioma origem
  for s:=0 to FStrList.Count-1 do
  begin
    FQryInserirSlor.Close;
    //FQry.SQL.Text:= format('insert into sintagmas_locucoes_origem (slor_lori_id, slor_sori_id, slor_ordem) values (%d, ''%s'', %d)', [result, FStrList.Strings[s], s]);
    FQryInserirSlor.Params[0].AsInteger := result;
    FQryInserirSlor.Params[1].AsString  := FStrList.Strings[s];
    FQryInserirSlor.Params[2].AsInteger := s;
    FQryInserirSlor.ExecSQL;
  end;
end;

function TGerSugestoes.ObterLocucaoDestino(locucao: string): integer;
begin
  //locucao := NormalizarVerbo(locucao);
  result := LocalizarLocucaoDestino(locucao);

  if result <= 0 then
    result := InserirLocucaoDestino(locucao);
end;

function TGerSugestoes.LocalizarLocucaoDestino(locucao: string): integer;
begin
  FQryObterLocucaoDestino.Close;
  //FQry.SQL.Text:= 'select ldes_id from locucoes_destino where ldes_locucao = ''' + locucao + '''';
  FQryObterLocucaoDestino.Params[0].AsString := locucao;
  FQryObterLocucaoDestino.Open;

  result := FQryObterLocucaoDestino.Fields[0].AsInteger;
end;

function TGerSugestoes.InserirLocucaoDestino(locucao: string): integer;
var
  s: smallint;
begin
  FStrList.DelimitedText := locucao;

  // inserindo o registros da locução
  FQryMaxLdesID.Close;
  //FQry.SQL.Text:= 'select max(ldes_id) + 1 from locucoes_destino';
  FQryMaxLdesID.Open;

  try
    result := FQryMaxLdesID.Fields[0].AsInteger;
  except
    result := 1;
  end;

  FQryInserirLdes.Close;
  //FQry.SQL.Text:= format('insert into locucoes_destino (ldes_id, ldes_qtde_sintagmas, ldes_locucao) values (%d, %d, ''%s'')', [result, FStrList.Count, locucao]);
  FQryInserirLdes.Params[0].AsInteger := result;
  FQryInserirLdes.Params[1].AsInteger := FStrList.Count;
  FQryInserirLdes.Params[2].AsString := locucao;
  FQryInserirLdes.ExecSQL;

  // sintagmas locucao idicma origem
  for s:=0 to FStrList.Count-1 do
  begin
    FQryInserirSdes.Close;
    //FQry.SQL.Text:= format('insert into sintagmas_locucoes_destino (slde_ldes_id, slde_sdes_id, slde_ordem) values (%d, ''%s'', %d)', [result, FStrList.Strings[s], s]);
    FQryInserirSdes.Params[0].AsInteger := result;
    FQryInserirSdes.Params[1].AsString  := FStrList.Strings[s];
    FQryInserirSdes.Params[2].AsInteger := s;
    FQryInserirSdes.ExecSQL;
  end;
end;

function TGerSugestoes.AtualizarTraducao(lori_id, ldes_id: integer): boolean;
begin
  FQryAtualizarTrad.Close;
  //FQry.SQL.Text := format('update traducoes set trad_ocorrencias = trad_ocorrencias + 1 where trad_lori_id = %d and trad_ldes_id = %d', [lori_id, ldes_id]);
  FQryAtualizarTrad.Params[0].AsInteger := lori_id;
  FQryAtualizarTrad.Params[1].AsInteger := ldes_id;
  FQryAtualizarTrad.ExecSQL;

  result := FQryAtualizarTrad.RowsAffected = 1;
end;

function TGerSugestoes.InserirTraducao(lori_id, ldes_id: integer): boolean;
begin
  FQryInserirTrad.Close;
  //FQry.SQL.Text := format('insert into traducoes (trad_lori_id, trad_ldes_id, trad_ocorrencias) values (%d, %d, 1)', [lori_id, ldes_id]);
  FQryInserirTrad.Params[0].AsInteger := lori_id;
  FQryInserirTrad.Params[1].AsInteger := ldes_id;
  FQryInserirTrad.ExecSQL;

  result := FQryInserirTrad.RowsAffected = 1;
end;

function TGerSugestoes.IdentificarVerbo(forma: string): boolean;
begin
  FVerbosList.Clear;
  FQryFormaVerbo.Close;
  FQryFormaVerbo.Params[0].AsString := forma;
  FQryFormaVerbo.Open;
  while not FQryFormaVerbo.EOF do
  begin
    FVerbosList.Add(FQryFormaVerbo.Fields[0].AsString);
    FQryFormaVerbo.Next;
  end;
  result := FVerbosList.Count > 0;
end;

function TGerSugestoes.NormalizarVerbo(locucao: string): string;
begin
  result := locucao;
  FStrList.DelimitedText := locucao;
  if IdentificarVerbo(FStrList.Strings[0]) then
  begin
    FStrList.Strings[0] := FVerbosList.Strings[0];
    result := FStrList.DelimitedText;
  end;
end;

function TGerSugestoes.ObterSugestoes(chave: string): TSQLQuery;
begin
  FQrySugestoes.Close;
  FQrySugestoes.Params[0].AsString := chave; //AnsiReplaceStr(stg.ChaveSugestao, '''', '''''');
  FQrySugestoes.Open;
  DebugLn('%d sugestoes de traducao encontradas na busca com morfologia', [FQrySugestoes.RecordCount]);
  result := FQrySugestoes;
  if FQrySugestoes.RecordCount = 0 then
  begin
    result := FQrySugestoesSemMorfo;

    chave := FRETirarMorfo.Replace(chave, '');
    FQrySugestoesSemMorfo.Close;
    FQrySugestoesSemMorfo.Params[0].AsString := chave + '%'; //AnsiReplaceStr(stg.ChaveSugestao, '''', '''''');
    FQrySugestoesSemMorfo.Open;
    DebugLn('%d sugestoes de traducao encontradas na busca sem morfologia', [FQrySugestoesSemMorfo.RecordCount]);
  end;
end;

procedure TGerSugestoes.SugerirAssociacoes(v1: TVersiculo);
  function compararDestino(a, b: string): boolean;
  var a1, b1: string;
  begin
    DebugLn('  comparando destino [%s] x [%s]', [a, b]);
    if a = b then
      exit(true);

    // tentando com verbos normalizados
    if FQryFormaVerbo <> nil then
    begin
      a1 := NormalizarVerbo(a);
      b1 := NormalizarVerbo(b);
      DebugLn('  comparando destino [%s] x [%s]', [a1, b1]);
      if a1 = b1 then
        exit(true);
    end;

    // tentando com plurais normalizados
    a1 := a;
    b1 := b;
    if AnsiEndsStr('s', a) then
      a1 := copy(a, 0, length(a)-1);
    if AnsiEndsStr('s', b) then
      b1 := copy(b, 0, length(b)-1);
    DebugLn('  comparando destino [%s] x [%s]', [a1, b1]);
    if a1 = b1 then
      exit(true);

    result := false;
  end;

  function compararOrigem(a, b: string): boolean;
  begin
    DebugLn('  comparando origem com morfologia [%s] x [%s]', [a, b]);
    result := false;
    if a = b then
       result := true
    else
    begin
      a := FRETirarMorfo.Replace(a, '');
      b := FRETirarMorfo.Replace(b, '');
      DebugLn('  comparando origem sem morfologia [%s] x [%s]', [a, b]);
      result := a = b;
    end;
  end;

var
  s1, s2, o, d, d1, d2: smallint;
  stg: TSintagma;
  v2: TVersiculo;
  associado: boolean;
  QSugestoes: TSQLQuery;
  loc1, loc2: TStringList;
begin
  {TODO -cObrigatório: Não associar palavras em itálico do idioma destino}

  DebugLn(#13#10'iniciando sugestao de associacao');

  loc1 := TStringList.Create;
  loc2 := TStringList.Create;
  loc1.Delimiter:=';';
  loc1.StrictDelimiter := true;
  loc2.Delimiter:=';';
  loc2.StrictDelimiter := true;

  v2 := v1.VersiculoPar;
  //v1.Ativo := false;
  //v2.Ativo := false;
  s1 := -1;
  while s1 < v1.Sintagmas.Count-1 do
  begin // sintagmas
    associado := false;
    inc(s1);
    v1.LimparSelecao;
    stg := v1.Sintagmas[s1];

    if (stg.Tipo <> tsSintagma) or (stg.Pares.Count > 0) then
      continue;

    stg.SelecaoMais;

    DebugLn('novo sintagma origem sem associacao: %s', [stg.Texto]);

    QSugestoes := ObterSugestoes(stg.GetChaveSugestao(tlMetaDados));

    while not QSugestoes.EOF do
    begin
      DebugLn('testando sugestao: %s => %s', [QSugestoes.Fields[0].AsString, QSugestoes.Fields[1].AsString]);
      //MessageDlg('Nova sugestão', FQrySugestoes.Fields[0].AsString, mtInformation, [mbOK], 0);

      loc1.DelimitedText := QSugestoes.Fields[0].AsString; // origem

      // procuremos a locução no idioma origem
      s2 := s1 + 1;
      o := 1;
      while (s2 < v1.Sintagmas.Count)
        and (o < loc1.Count) do
      begin
        if (v1.Sintagmas[s2].Tipo = tsSintagma) and (v1.Sintagmas[s2].Italico = false) then
        begin
          if not compararOrigem(v1.Sintagmas[s2].GetChaveSugestao(tlMetaDados), loc1.Strings[o]) then
            break;

          v1.Sintagmas[s2].SelecaoMais;
          inc(o);
        end;
        inc(s2);
      end;

      if (o = loc1.Count) then // encontramos a locução no idioma origem?
      begin
        DebugLn('locucao %s localizada no texto origem', [QSugestoes.Fields[0].AsString]);
        // procuremos o primeiro sintagma da locução no idioma destino
        loc2.DelimitedText := QSugestoes.Fields[1].AsString; // destino
        for d1:=0 to v2.Sintagmas.Count-1 do
        begin
          v2.LimparSelecao;
          stg := v2.Sintagmas[d1];
          if (stg.Tipo <> tsSintagma) or stg.Italico or (stg.Pares.Count > 0) then
            continue;

          if compararDestino(stg.GetChaveSugestao(tlMetaDados), loc2.Strings[0]) then
          begin
            DebugLn('inicio da locucao %s localizada no texto destino', [loc2.Strings[0]]);
            stg.SelecaoMais;
            // procuremos a locução no idioma destino
            d2 := d1 + 1;
            d := 1; // começando do segundo termo da locução
            DebugLn('    procurando demais termos da locução %s', [loc2.DelimitedText]);
            while (d2 < v2.Sintagmas.Count)
              and (d < loc2.Count) do
              //and ((v2.Sintagmas[d2].Tipo <> tsSintagma) or comparar(v2.Sintagmas[d2].GetChaveSugestao(tlStrong), FStrList.Strings[d])) do
            begin
              if (v2.Sintagmas[d2].Tipo = tsSintagma) then
              begin
                DebugLn('    procurando termo da locução: %s', [loc2.Strings[d]]);
                if compararDestino(v2.Sintagmas[d2].GetChaveSugestao(tlMetaDados), loc2.Strings[d]) then
                  v2.Sintagmas[d2].SelecaoMais
                else
                  break;
                inc(d);
              end;
              inc(d2);
            end;

            if (d = loc2.Count) then // !!! teste errado !!! encontramos a locução no idioma destino!
            begin
              DebugLn('locucao %s localizada no texto destino', [QSugestoes.Fields[1].AsString]);
              v1.AssociarSintagmas;
              associado := true;
              break;
            end;
          end;
        end;
      end;

      if associado then
        break;

      QSugestoes.Next;
    end;
  end;
  QSugestoes.Close;
  v1.LimparSelecao;
  v2.LimparSelecao;
  loc1.Free;
  loc2.Free;
  //v1.Ativo := true;
  //v2.Ativo := true;
end;

procedure TGerSugestoes.LimparBaseAssociacoes;
begin
  with FQry do
  begin
    Close;
    SQL.Text := 'delete from traducoes';
    ExecSQL;

    Close;
    SQL.Text := 'delete from sintagmas_locucoes_origem';
    ExecSQL;

    Close;
    SQL.Text := 'delete from sintagmas_locucoes_destino';
    ExecSQL;

    Close;
    SQL.Text := 'delete from locucoes_origem';
    ExecSQL;

    Close;
    SQL.Text := 'delete from locucoes_destino';
    ExecSQL;
  end;
end;

procedure TGerSugestoes.Commit;
begin
  FConn.Transaction.Commit;
  FConn.StartTransaction;
end;

procedure TGerSugestoes.Rollback;
begin
  FConn.Transaction.Rollback;
  FConn.StartTransaction;
end;

{*
function TGerSugestoes.GetTranslationAlternatives(syntagm: TSintagma): string;
var
  QSugestoes: TSQLQuery;
  locucao, key: string;
begin
  result := '';
  key := syntagm.GetChaveSugestao(tlMetaDados);
  if key.IsEmpty then
    exit;

  QSugestoes := ObterSugestoes(syntagm.GetChaveSugestao(tlMetaDados));
  while not QSugestoes.EOF do
  begin
    locucao := AnsiReplaceStr(QSugestoes.Fields[1].AsString, ';', ' ');
    result := format('%s%s%s', [result, IfThen(result.Length = 0, '', '; '), locucao]);
    QSugestoes.Next;
  end;
  QSugestoes.Close;
end;
*}
function TGerSugestoes.GetTranslationAlternatives(syntagm: TSintagma): string;
var
  QSugestoes: TSQLQuery;
  locucao, key: string;
begin
  result := '';
  key := syntagm.GetChaveSugestao(tlMetaDados);
  if key.IsEmpty then
    exit;

  QSugestoes := ObterSugestoes(syntagm.GetChaveSugestao(tlMetaDados));
  while not QSugestoes.EOF do
  begin
    locucao := AnsiReplaceStr(QSugestoes.Fields[1].AsString, ';', ' ');
    result := format('%s\par\li0 {\b\''95  } %s', [result, UnicodeToRTF(locucao)]);
    QSugestoes.Next;
  end;
  QSugestoes.Close;
end;

end.


