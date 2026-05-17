unit project_persistence;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Sqlite3DS, sqlite3conn, sqldb;

type
  TProjectPersistence = class
  private
    FPairsTable: TSqlite3Dataset;
    FInfoTable: TSqlite3Dataset;
    FFileName: string;
    procedure CloseTable(var Table: TSqlite3Dataset);
  public
    destructor Destroy; override;
    class function CreateTableDataset(const db, tableName, primaryKey: string): TSqlite3Dataset; static;
    class function CreateQueryObject(const db: string): TSQLQuery; static;
    class function UpdateConfigTable(table: TSqlite3Dataset; const name, value: string): Boolean; static;
    procedure Open(const AFileName: string);
    procedure Close;
    function LocateReference(const reference: string): Boolean;
    function InsertInfo(const info, value: string): Boolean;
    function UpdateInfo(const info, value: string): Boolean;
    function ReadInfo(const info: string): string;
    function CurrentPairId: string;
    function CurrentPairs: string;
    function CurrentComments: string;
    function CurrentStatus: Integer;
    procedure WriteCurrentPairs(const pairs: string);
    procedure WriteCurrentComments(const comments: string);
    procedure Commit;
    procedure Rollback;
    property PairsTable: TSqlite3Dataset read FPairsTable;
    property InfoTable: TSqlite3Dataset read FInfoTable;
    property FileName: string read FFileName;
  end;

implementation

procedure TProjectPersistence.CloseTable(var Table: TSqlite3Dataset);
begin
  if not Assigned(Table) then
    Exit;

  if Table.Active then
    Table.Close;

  FreeAndNil(Table);
end;

destructor TProjectPersistence.Destroy;
begin
  Close;
  inherited Destroy;
end;

class function TProjectPersistence.CreateTableDataset(const db, tableName,
  primaryKey: string): TSqlite3Dataset;
begin
  Result := TSqlite3Dataset.Create(nil);
  Result.FileName := db;
  Result.TableName := tableName;
  Result.PrimaryKey := primaryKey;
end;

class function TProjectPersistence.CreateQueryObject(const db: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.DataBase := TSQLite3Connection.Create(nil);
  Result.DataBase.DatabaseName := db;
  Result.DataBase.Open;
  TSQLite3Connection(Result.DataBase).Transaction := TSQLTransaction.Create(nil);
  Result.Transaction := TSQLite3Connection(Result.DataBase).Transaction;
end;

class function TProjectPersistence.UpdateConfigTable(table: TSqlite3Dataset;
  const name, value: string): Boolean;
begin
  Result := table.Locate('name', name, []);

  if Result then
  begin
    table.Edit;
    table.Fields[1].AsString := value;
    table.Post;
  end;
end;

procedure TProjectPersistence.Open(const AFileName: string);
begin
  if SameText(FFileName, AFileName) and Assigned(FPairsTable) and Assigned(FInfoTable) then
    Exit;

  Close;

  FInfoTable := CreateTableDataset(AFileName, 'info', 'id');
  FInfoTable.Open;

  FPairsTable := CreateTableDataset(AFileName, 'pares', 'pare_id');
  FPairsTable.Open;

  FFileName := AFileName;
end;

procedure TProjectPersistence.Close;
begin
  CloseTable(FPairsTable);
  CloseTable(FInfoTable);
  FFileName := '';
end;

function TProjectPersistence.LocateReference(const reference: string): Boolean;
begin
  if not Assigned(FPairsTable) then
    raise Exception.Create('Project pairs table is not open');

  Result := FPairsTable.Locate('pare_id', reference, []);
end;

function TProjectPersistence.InsertInfo(const info, value: string): Boolean;
begin
  if not Assigned(FInfoTable) then
    raise Exception.Create('Project info table is not open');

  if FInfoTable.Locate('id', info, []) then
  begin
    Result := False;
    Exit;
  end;

  FInfoTable.InsertRecord([info, value]);
  Result := True;
end;

function TProjectPersistence.UpdateInfo(const info, value: string): Boolean;
begin
  if not Assigned(FInfoTable) then
    raise Exception.Create('Project info table is not open');

  Result := FInfoTable.Locate('id', info, []);

  if Result then
  begin
    FInfoTable.Edit;
    FInfoTable.Fields[1].AsString := value;
    FInfoTable.Post;
  end;
end;

function TProjectPersistence.ReadInfo(const info: string): string;
begin
  Result := '';
  if not Assigned(FInfoTable) then
    Exit;

  if FInfoTable.Locate('id', info, []) then
    Result := FInfoTable.Fields[1].AsString;
end;

function TProjectPersistence.CurrentPairId: string;
begin
  if not Assigned(FPairsTable) then
    Exit('');

  Result := FPairsTable.FieldByName('pare_id').AsString;
end;

function TProjectPersistence.CurrentPairs: string;
begin
  if not Assigned(FPairsTable) then
    Exit('');

  Result := FPairsTable.FieldByName('pare_pares').AsString;
end;

function TProjectPersistence.CurrentComments: string;
begin
  if not Assigned(FPairsTable) then
    Exit('');

  Result := FPairsTable.FieldByName('pare_comentarios').AsString;
end;

function TProjectPersistence.CurrentStatus: Integer;
begin
  if not Assigned(FPairsTable) then
    Exit(0);

  Result := FPairsTable.FieldByName('pare_situacao').AsInteger;
end;

procedure TProjectPersistence.WriteCurrentPairs(const pairs: string);
begin
  if not Assigned(FPairsTable) then
    raise Exception.Create('Project pairs table is not open');

  FPairsTable.Edit;
  FPairsTable.FieldByName('pare_pares').AsString := pairs;
  FPairsTable.Post;
end;

procedure TProjectPersistence.WriteCurrentComments(const comments: string);
begin
  if not Assigned(FPairsTable) then
    raise Exception.Create('Project pairs table is not open');

  FPairsTable.Edit;
  FPairsTable.FieldByName('pare_comentarios').AsString := comments;
  FPairsTable.Post;
end;

procedure TProjectPersistence.Commit;
var
  retryCount: Integer;
  maxRetries: Integer = 5;
  delayMs: Integer;
begin
  retryCount := 0;
  delayMs := 50;
  while retryCount < maxRetries do
  begin
    try
      if Assigned(FPairsTable) then
        FPairsTable.ApplyUpdates;
      if Assigned(FInfoTable) then
        FInfoTable.ApplyUpdates;
      Break;
    except
      on E: Exception do
      begin
        if (Pos('SQLITE_BUSY', E.Message) > 0) or (Pos('database is locked', E.Message) > 0) then
        begin
          Inc(retryCount);
          if retryCount >= maxRetries then
            raise;
          Sleep(delayMs);
          delayMs := delayMs * 2;
        end
        else
          raise;
      end;
    end;
  end;
end;

procedure TProjectPersistence.Rollback;
begin
  if Assigned(FPairsTable) then
    FPairsTable.ClearUpdates;
  if Assigned(FInfoTable) then
    FInfoTable.ClearUpdates;
end;

end.