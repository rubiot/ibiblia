unit project_database_bootstrap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, sqlite3conn, sqldb, BibleScope;

type
  TProjectDatabaseBootstrap = class
  private
    class procedure RequireValidScope(const Scope: TEscopoTexto); static;
    class procedure CreateSchema(Connection: TSQLite3Connection); static;
    class procedure PopulatePairs(Connection: TSQLite3Connection;
      Transaction: TSQLTransaction; const Scope: TEscopoTexto); static;
  public
    class procedure CreateProjectDatabase(const FileName: string;
      const Scope: TEscopoTexto); static;
  end;

implementation

{$INCLUDE include/bookdefs.inc}

const
  ProjectSchemaStatements: array[0..9] of string = (
    'CREATE TABLE traducoes (' +
    ' trad_lori_id integer not null,' +
    ' trad_ldes_id integer not null,' +
    ' trad_ocorrencias integer' +
    ')',
    'CREATE TABLE sintagmas_locucoes_origem (' +
    ' slor_lori_id integer not null,' +
    ' slor_sori_id integer not null,' +
    ' slor_ordem integer not null' +
    ')',
    'CREATE TABLE sintagmas_locucoes_destino (' +
    ' slde_ldes_id integer not null,' +
    ' slde_sdes_id integer not null,' +
    ' slde_ordem integer not null' +
    ')',
    'CREATE TABLE pares (' +
    ' pare_id text primary key,' +
    ' pare_ref text,' +
    ' pare_texto_origem text,' +
    ' pare_texto_destino text,' +
    ' pare_texto_consulta1 text,' +
    ' pare_texto_consulta2 text,' +
    ' pare_pares text,' +
    ' pare_situacao number,' +
    ' pare_comentarios text' +
    ')',
    'CREATE TABLE locucoes_origem (' +
    ' lori_id integer not null primary key,' +
    ' lori_qtde_sintagmas integer,' +
    ' lori_locucao varchar(200) unique' +
    ')',
    'CREATE TABLE locucoes_destino (' +
    ' ldes_id integer not null primary key,' +
    ' ldes_qtde_sintagmas integer,' +
    ' ldes_locucao varchar(200) unique' +
    ')',
    'CREATE TABLE info (' +
    ' id text primary key,' +
    ' valor text' +
    ')',
    'CREATE UNIQUE INDEX trad_uk on traducoes(trad_lori_id, trad_ldes_id)',
    'CREATE UNIQUE INDEX slor_uk on sintagmas_locucoes_origem(slor_lori_id, slor_sori_id, slor_ordem)',
    'CREATE UNIQUE INDEX slde_uk on sintagmas_locucoes_destino(slde_ldes_id, slde_sdes_id, slde_ordem)'
  );

  ProjectSchemaIndexStatements: array[0..1] of string = (
    'CREATE INDEX lori_locucao_idx ON locucoes_origem (lori_locucao)',
    'CREATE INDEX ldes_locucao_idx ON locucoes_destino (ldes_locucao)'
  );

resourcestring
  SProjectOverrideError = 'A project with this name already exists. Delete it first or choose another name';
  SProjectScopeRequired = 'Project scope must be defined before creating a project database';

class procedure TProjectDatabaseBootstrap.RequireValidScope(const Scope: TEscopoTexto);
begin
  if Scope = etNone then
    raise Exception.Create(SProjectScopeRequired);
end;

class procedure TProjectDatabaseBootstrap.CreateSchema(
  Connection: TSQLite3Connection);
var
  Statement: string;
begin
  for Statement in ProjectSchemaStatements do
    Connection.ExecuteDirect(Statement);

  for Statement in ProjectSchemaIndexStatements do
    Connection.ExecuteDirect(Statement);
end;

class procedure TProjectDatabaseBootstrap.PopulatePairs(
  Connection: TSQLite3Connection; Transaction: TSQLTransaction;
  const Scope: TEscopoTexto);
var
  InsertQuery: TSQLQuery;
  BookIndex: Integer;
  BookID: Integer;
  ChapterNumber: Integer;
  VerseNumber: Integer;
  VerseCount: Integer;
begin
  InsertQuery := TSQLQuery.Create(nil);
  try
    InsertQuery.DataBase := Connection;
    InsertQuery.Transaction := Transaction;
    InsertQuery.SQL.Text :=
      'INSERT INTO pares (' +
      ' pare_id, pare_ref, pare_texto_origem, pare_texto_destino,' +
      ' pare_texto_consulta1, pare_texto_consulta2, pare_pares,' +
      ' pare_situacao, pare_comentarios' +
      ') VALUES (' +
      ' :pare_id, :pare_ref, :pare_texto_origem, :pare_texto_destino,' +
      ' :pare_texto_consulta1, :pare_texto_consulta2, :pare_pares,' +
      ' :pare_situacao, :pare_comentarios' +
      ')';
    InsertQuery.Prepare;

    for BookIndex := 0 to QLivros[Scope] - 1 do
    begin
      BookID := BookIndex + OffsetLivros[Scope] + 1;
      for ChapterNumber := 1 to QCapitulos[Scope][BookIndex] do
      begin
        VerseCount := QVersiculos[Scope][QCapLivros[Scope][BookIndex] + ChapterNumber - 2];
        for VerseNumber := 1 to VerseCount do
        begin
          InsertQuery.Params.ParamByName('pare_id').AsString :=
            Format('%d,%d,%d', [BookID, ChapterNumber, VerseNumber]);
          InsertQuery.Params.ParamByName('pare_ref').AsString :=
            InsertQuery.Params.ParamByName('pare_id').AsString;
          InsertQuery.Params.ParamByName('pare_texto_origem').AsString := '';
          InsertQuery.Params.ParamByName('pare_texto_destino').AsString := '';
          InsertQuery.Params.ParamByName('pare_texto_consulta1').AsString := '';
          InsertQuery.Params.ParamByName('pare_texto_consulta2').AsString := '';
          InsertQuery.Params.ParamByName('pare_pares').AsString := '';
          InsertQuery.Params.ParamByName('pare_situacao').AsInteger := 0;
          InsertQuery.Params.ParamByName('pare_comentarios').AsString := '';
          InsertQuery.ExecSQL;
        end;
      end;
    end;
  finally
    InsertQuery.Free;
  end;
end;

class procedure TProjectDatabaseBootstrap.CreateProjectDatabase(
  const FileName: string; const Scope: TEscopoTexto);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  RequireValidScope(Scope);

  if FileExists(FileName) then
    raise Exception.Create(SProjectOverrideError);

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FileName;
    Connection.Open;

    Transaction.StartTransaction;
    try
      CreateSchema(Connection);
      PopulatePairs(Connection, Transaction, Scope);
      Transaction.Commit;
    except
      on E: Exception do
      begin
        if Transaction.Active then
          Transaction.Rollback;
        if FileExists(FileName) then
          DeleteFile(FileName);
        raise;
      end;
    end;
  finally
    Transaction.Free;
    Connection.Free;
  end;
end;

end.