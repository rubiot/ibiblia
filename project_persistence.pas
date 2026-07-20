unit project_persistence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, sqlite3conn, sqldb, db;

type
  TProjectPersistence = class
  private
    FPairsTable: TSqlite3Dataset;
    FInfoTable: TSqlite3Dataset;
    FFileName: string;
    FPairOriginalValues: TStringList;
    FStagedInfo: TStringList;
    FCommittedInfo: TStringList;
    FImmediateInfoPending: TStringList;
    FOnContentPendingChanged: TNotifyEvent;
    FLastContentPending: Boolean;
    procedure CloseTable(var Table: TSqlite3Dataset);
    procedure SetNamedValue(List: TStringList; const Name, Value: string);
    function PairChangeKey(const fieldName: string): string;
    procedure TrackPairChange(const fieldName, originalValue,
      newValue: string);
    procedure NotifyContentPendingChanged;
    procedure SetOnContentPendingChanged(const AValue: TNotifyEvent);
    function InfoKeyExists(const info: string): Boolean;
    function ReadCommittedInfo(const info: string): string;
    function IsDatabaseBusy(const MessageText: string): Boolean;
    procedure WriteInfoValues(const Values: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateTableDataset(const db, tableName, primaryKey: string): TSqlite3Dataset; static;
    class function CreateQueryObject(const db: string): TSQLQuery; static;
    class function UpdateConfigTable(table: TSqlite3Dataset; const name, value: string): Boolean; static;
    procedure Open(const AFileName: string);
    procedure Close;
    function UpdatesPending: Boolean;
    function ContentUpdatesPending: Boolean;
    function LocateReference(const reference: string): Boolean;
    procedure Next;
    procedure Prior;
    procedure First;
    procedure Last;
    function EOF: Boolean;
    function BOF: Boolean;
    procedure BeginCurrentEdit;
    procedure PostCurrentEdit;
    procedure CancelCurrentEdit;
    function InsertInfo(const info, value: string): Boolean;
    function UpdateInfo(const info, value: string): Boolean;
    function StageInfo(const info, value: string): Boolean;
    function WriteInfoImmediate(const info, value: string): Boolean;
    procedure FlushImmediateInfo;
    function ReadInfo(const info: string): string;
    function CurrentPairId: string;
    function CurrentPairs: string;
    function CurrentComments: string;
    function CurrentStatus: Integer;
    function CurrentText(fieldIndex: Integer): string;
    function CurrentFieldAsString(const fieldName: string): string;
    function CurrentFieldAsInteger(const fieldName: string): Integer;
    procedure SetCurrentText(fieldIndex: Integer; const value: string);
    procedure SetCurrentFieldAsString(const fieldName, value: string);
    procedure SetCurrentFieldAsInteger(const fieldName: string; value: Integer);
    procedure WriteCurrentPairs(const pairs: string);
    procedure WriteCurrentComments(const comments: string);
    procedure WriteCurrentStatus(status: Integer);
    procedure CommitPairs;
    procedure CommitInfo;
    procedure Commit;
    procedure Rollback;
    property PairsTable: TSqlite3Dataset read FPairsTable;
    property InfoTable: TSqlite3Dataset read FInfoTable;
    property FileName: string read FFileName;
    property OnContentPendingChanged: TNotifyEvent
      read FOnContentPendingChanged write SetOnContentPendingChanged;
  end;

implementation

constructor TProjectPersistence.Create;
begin
  inherited Create;
  FPairOriginalValues := TStringList.Create;
  FStagedInfo := TStringList.Create;
  FCommittedInfo := TStringList.Create;
  FImmediateInfoPending := TStringList.Create;
  FOnContentPendingChanged := nil;
  FLastContentPending := False;
end;

procedure TProjectPersistence.SetOnContentPendingChanged(
  const AValue: TNotifyEvent);
begin
  FOnContentPendingChanged := AValue;
  FLastContentPending := ContentUpdatesPending;
end;

procedure TProjectPersistence.NotifyContentPendingChanged;
var
  Pending: Boolean;
begin
  Pending := ContentUpdatesPending;
  if Pending = FLastContentPending then
    Exit;
  FLastContentPending := Pending;
  if Assigned(FOnContentPendingChanged) then
    FOnContentPendingChanged(Self);
end;

procedure TProjectPersistence.SetNamedValue(List: TStringList;
  const Name, Value: string);
var
  Index: Integer;
begin
  Index := List.IndexOfName(Name);
  if Index < 0 then
    List.Add(Name + '=' + Value)
  else
    List.Strings[Index] := Name + '=' + Value;
end;

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
  FImmediateInfoPending.Free;
  FCommittedInfo.Free;
  FStagedInfo.Free;
  FPairOriginalValues.Free;
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
  FPairOriginalValues.Clear;
  FStagedInfo.Clear;
  FCommittedInfo.Clear;
  FImmediateInfoPending.Clear;
  FLastContentPending := False;
end;

procedure TProjectPersistence.Close;
begin
  CloseTable(FPairsTable);
  CloseTable(FInfoTable);
  FPairOriginalValues.Clear;
  FStagedInfo.Clear;
  FCommittedInfo.Clear;
  FImmediateInfoPending.Clear;
  FLastContentPending := False;
  FFileName := '';
end;

function TProjectPersistence.UpdatesPending: Boolean;
begin
  Result := ContentUpdatesPending;
end;

function TProjectPersistence.ContentUpdatesPending: Boolean;
begin
  Result := (FPairOriginalValues.Count > 0) or (FStagedInfo.Count > 0);
end;

function TProjectPersistence.LocateReference(const reference: string): Boolean;
begin
  if not Assigned(FPairsTable) then
    raise Exception.Create('Project pairs table is not open');

  Result := FPairsTable.Locate('pare_id', reference, []);
end;

procedure TProjectPersistence.Next;
begin
  if Assigned(FPairsTable) then
    FPairsTable.Next;
end;

procedure TProjectPersistence.Prior;
begin
  if Assigned(FPairsTable) then
    FPairsTable.Prior;
end;

procedure TProjectPersistence.First;
begin
  if Assigned(FPairsTable) then
    FPairsTable.First;
end;

procedure TProjectPersistence.Last;
begin
  if Assigned(FPairsTable) then
    FPairsTable.Last;
end;

function TProjectPersistence.EOF: Boolean;
begin
  Result := not Assigned(FPairsTable) or FPairsTable.EOF;
end;

function TProjectPersistence.BOF: Boolean;
begin
  Result := not Assigned(FPairsTable) or FPairsTable.BOF;
end;

procedure TProjectPersistence.BeginCurrentEdit;
begin
  if not Assigned(FPairsTable) then
    raise Exception.Create('Project pairs table is not open');

  if not (FPairsTable.State in dsEditModes) then
    FPairsTable.Edit;
end;

procedure TProjectPersistence.PostCurrentEdit;
begin
  if not Assigned(FPairsTable) then
    raise Exception.Create('Project pairs table is not open');

  if FPairsTable.State in dsEditModes then
    FPairsTable.Post;
  NotifyContentPendingChanged;
end;

procedure TProjectPersistence.CancelCurrentEdit;
begin
  if Assigned(FPairsTable) and (FPairsTable.State in dsEditModes) then
    FPairsTable.Cancel;
end;

function TProjectPersistence.InsertInfo(const info, value: string): Boolean;
begin
  Result := not InfoKeyExists(info);
  if Result then
    StageInfo(info, value);
end;

function TProjectPersistence.UpdateInfo(const info, value: string): Boolean;
begin
  Result := InfoKeyExists(info);
  if Result then
    StageInfo(info, value);
end;

function TProjectPersistence.InfoKeyExists(const info: string): Boolean;
begin
  Result := (FStagedInfo.IndexOfName(info) >= 0) or
    (FCommittedInfo.IndexOfName(info) >= 0);
  if not Result and Assigned(FInfoTable) then
    Result := FInfoTable.Locate('id', info, []);
end;

function TProjectPersistence.ReadCommittedInfo(const info: string): string;
var
  Index: Integer;
begin
  Result := '';
  Index := FCommittedInfo.IndexOfName(info);
  if Index >= 0 then
    Exit(FCommittedInfo.ValueFromIndex[Index]);

  if Assigned(FInfoTable) and FInfoTable.Locate('id', info, []) then
    Result := FInfoTable.Fields[1].AsString;
end;

function TProjectPersistence.IsDatabaseBusy(
  const MessageText: string): Boolean;
begin
  Result := (Pos('SQLITE_BUSY', UpperCase(MessageText)) > 0) or
    (Pos('DATABASE IS LOCKED', UpperCase(MessageText)) > 0);
end;

function TProjectPersistence.StageInfo(const info, value: string): Boolean;
var
  Index: Integer;
  CommittedValue: string;
begin
  if not Assigned(FInfoTable) then
    raise Exception.Create('Project info table is not open');

  CommittedValue := ReadCommittedInfo(info);
  Index := FStagedInfo.IndexOfName(info);
  if value = CommittedValue then
  begin
    Result := Index >= 0;
    if Result then
      FStagedInfo.Delete(Index);
    NotifyContentPendingChanged;
    Exit;
  end;

  Result := (Index < 0) or (FStagedInfo.ValueFromIndex[Index] <> value);
  if Result then
    SetNamedValue(FStagedInfo, info, value);
  NotifyContentPendingChanged;
end;

procedure TProjectPersistence.WriteInfoValues(const Values: TStrings);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  Index: Integer;
begin
  if Values.Count = 0 then
    Exit;
  if FFileName.IsEmpty then
    raise Exception.Create('Project persistence is not open');

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  Query := TSQLQuery.Create(nil);
  try
    try
      Connection.DatabaseName := FFileName;
      Connection.Transaction := Transaction;
      Query.DataBase := Connection;
      Query.Transaction := Transaction;
      Query.SQL.Text :=
        'insert or replace into info (id, valor) values (:id, :valor)';
      Connection.Open;
      Connection.ExecuteDirect('pragma busy_timeout = 250');
      if not Transaction.Active then
        Transaction.StartTransaction;
      Query.Prepare;
      for Index := 0 to Values.Count - 1 do
      begin
        Query.ParamByName('id').AsString := Values.Names[Index];
        Query.ParamByName('valor').AsString := Values.ValueFromIndex[Index];
        Query.ExecSQL;
      end;
      Transaction.Commit;
    except
      if Transaction.Active then
        Transaction.Rollback;
      raise;
    end;
  finally
    Query.Free;
    Connection.Free;
  end;
end;

function TProjectPersistence.WriteInfoImmediate(const info,
  value: string): Boolean;
var
  Values: TStringList;
  StagedIndex: Integer;
begin
  StagedIndex := FStagedInfo.IndexOfName(info);
  if StagedIndex >= 0 then
  begin
    FStagedInfo.Delete(StagedIndex);
    NotifyContentPendingChanged;
  end;

  Result := ReadCommittedInfo(info) <> value;
  if not Result then
    Exit;

  Values := TStringList.Create;
  try
    SetNamedValue(Values, info, value);
    try
      WriteInfoValues(Values);
      StagedIndex := FImmediateInfoPending.IndexOfName(info);
      if StagedIndex >= 0 then
        FImmediateInfoPending.Delete(StagedIndex);
    except
      on E: Exception do
        if IsDatabaseBusy(E.Message) then
          SetNamedValue(FImmediateInfoPending, info, value)
        else
          raise;
    end;
    SetNamedValue(FCommittedInfo, info, value);
  finally
    Values.Free;
  end;
end;

procedure TProjectPersistence.FlushImmediateInfo;
begin
  if FImmediateInfoPending.Count = 0 then
    Exit;
  WriteInfoValues(FImmediateInfoPending);
  FImmediateInfoPending.Clear;
end;

function TProjectPersistence.ReadInfo(const info: string): string;
var
  Index: Integer;
begin
  Index := FStagedInfo.IndexOfName(info);
  if Index >= 0 then
    Exit(FStagedInfo.ValueFromIndex[Index]);
  Result := ReadCommittedInfo(info);
end;

function TProjectPersistence.PairChangeKey(const fieldName: string): string;
begin
  Result := CurrentPairId + #1 + fieldName;
end;

procedure TProjectPersistence.TrackPairChange(const fieldName, originalValue,
  newValue: string);
var
  Key: string;
  Index: Integer;
  BaselineValue: string;
begin
  Key := PairChangeKey(fieldName);
  Index := FPairOriginalValues.IndexOfName(Key);
  if Index < 0 then
  begin
    BaselineValue := originalValue;
    SetNamedValue(FPairOriginalValues, Key, BaselineValue);
    Index := FPairOriginalValues.IndexOfName(Key);
  end
  else
    BaselineValue := FPairOriginalValues.ValueFromIndex[Index];

  if newValue = BaselineValue then
    FPairOriginalValues.Delete(Index);
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

function TProjectPersistence.CurrentText(fieldIndex: Integer): string;
begin
  if not Assigned(FPairsTable) then
    Exit('');

  Result := FPairsTable.Fields[fieldIndex].AsString;
end;

function TProjectPersistence.CurrentFieldAsString(const fieldName: string): string;
begin
  if not Assigned(FPairsTable) then
    Exit('');

  Result := FPairsTable.FieldByName(fieldName).AsString;
end;

function TProjectPersistence.CurrentFieldAsInteger(const fieldName: string): Integer;
begin
  if not Assigned(FPairsTable) then
    Exit(0);

  Result := FPairsTable.FieldByName(fieldName).AsInteger;
end;

procedure TProjectPersistence.SetCurrentText(fieldIndex: Integer; const value: string);
var
  Field: TField;
  OriginalValue: string;
begin
  Field := FPairsTable.Fields[fieldIndex];
  OriginalValue := Field.AsString;
  if OriginalValue = value then
    Exit;
  BeginCurrentEdit;
  TrackPairChange(Field.FieldName, OriginalValue, value);
  Field.AsString := value;
end;

procedure TProjectPersistence.SetCurrentFieldAsString(const fieldName, value: string);
var
  Field: TField;
  OriginalValue: string;
begin
  Field := FPairsTable.FieldByName(fieldName);
  OriginalValue := Field.AsString;
  if OriginalValue = value then
    Exit;
  BeginCurrentEdit;
  TrackPairChange(fieldName, OriginalValue, value);
  Field.AsString := value;
end;

procedure TProjectPersistence.SetCurrentFieldAsInteger(const fieldName: string; value: Integer);
var
  Field: TField;
  OriginalValue: string;
begin
  Field := FPairsTable.FieldByName(fieldName);
  if Field.AsInteger = value then
    Exit;
  OriginalValue := Field.AsString;
  BeginCurrentEdit;
  TrackPairChange(fieldName, OriginalValue, IntToStr(value));
  Field.AsInteger := value;
end;

procedure TProjectPersistence.WriteCurrentPairs(const pairs: string);
begin
  SetCurrentFieldAsString('pare_pares', pairs);
  PostCurrentEdit;
end;

procedure TProjectPersistence.WriteCurrentComments(const comments: string);
begin
  SetCurrentFieldAsString('pare_comentarios', comments);
  PostCurrentEdit;
end;

procedure TProjectPersistence.WriteCurrentStatus(status: Integer);
begin
  SetCurrentFieldAsInteger('pare_situacao', status);
  PostCurrentEdit;
end;

procedure ApplyPendingUpdates(Table: TSqlite3Dataset);
var
  retryCount: Integer;
  maxRetries: Integer = 5;
  delayMs: Integer;
begin
  if not Assigned(Table) or not Table.UpdatesPending then
    Exit;

  retryCount := 0;
  delayMs := 50;
  while retryCount < maxRetries do
  begin
    try
      Table.ApplyUpdates;
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

procedure TProjectPersistence.CommitPairs;
begin
  ApplyPendingUpdates(FPairsTable);
  FPairOriginalValues.Clear;
  NotifyContentPendingChanged;
end;

procedure TProjectPersistence.CommitInfo;
var
  Index: Integer;
begin
  if FStagedInfo.Count = 0 then
    Exit;

  for Index := 0 to FStagedInfo.Count - 1 do
  begin
    if FInfoTable.Locate('id', FStagedInfo.Names[Index], []) then
      FInfoTable.Edit
    else
    begin
      FInfoTable.Append;
      FInfoTable.FieldByName('id').AsString := FStagedInfo.Names[Index];
    end;
    FInfoTable.FieldByName('valor').AsString :=
      FStagedInfo.ValueFromIndex[Index];
    FInfoTable.Post;
  end;
  ApplyPendingUpdates(FInfoTable);
  for Index := 0 to FStagedInfo.Count - 1 do
    SetNamedValue(FCommittedInfo, FStagedInfo.Names[Index],
      FStagedInfo.ValueFromIndex[Index]);
  FStagedInfo.Clear;
  NotifyContentPendingChanged;
end;

procedure TProjectPersistence.Commit;
begin
  CommitPairs;
  CommitInfo;
end;

procedure TProjectPersistence.Rollback;
var
  CurrentReference: string;
begin
  CurrentReference := CurrentPairId;
  if Assigned(FPairsTable) then
  begin
    if FPairsTable.State in dsEditModes then
      FPairsTable.Cancel;
    FPairsTable.ClearUpdates;
    if FPairsTable.Active then
    begin
      FPairsTable.Close;
      FPairsTable.Open;
    end;
  end;
  FPairOriginalValues.Clear;
  FStagedInfo.Clear;
  if Assigned(FInfoTable) and
    ((FInfoTable.State in dsEditModes) or FInfoTable.UpdatesPending) then
  begin
    if FInfoTable.State in dsEditModes then
      FInfoTable.Cancel;
    FInfoTable.ClearUpdates;
    if FInfoTable.Active then
    begin
      FInfoTable.Close;
      FInfoTable.Open;
    end;
  end;
  if not CurrentReference.IsEmpty then
    LocateReference(CurrentReference);
  NotifyContentPendingChanged;
end;

end.
