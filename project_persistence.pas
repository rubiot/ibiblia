unit project_persistence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, sqlite3conn, sqldb, db;

type
  EProjectPersistenceRecoveryRequired = class(Exception);

  TProjectPersistence = class
  private
    FPairsTable: TSqlite3Dataset;
    FInfoTable: TSqlite3Dataset;
    FFileName: string;
    FPairOriginalValues: TStringList;
    FPairCurrentValues: TStringList;
    FStagedInfo: TStringList;
    FSilentStagedInfo: TStringList;
    FCommittedInfo: TStringList;
    FImmediateInfoPending: TStringList;
    FInfoTableNeedsRefresh: Boolean;
    FOnContentPendingChanged: TNotifyEvent;
    FLastContentPending: Boolean;
    FPairCommitMayBePartial: Boolean;
    FVerseContentRevision: QWord;
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
    procedure RebuildPendingPairUpdates;
    procedure RestorePairBaselineAfterFailedCommit;
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
    function StageInfoSilently(const info, value: string): Boolean;
    function WriteInfoImmediate(const info, value: string): Boolean;
    procedure FlushImmediateInfo;
    function ReadInfo(const info: string): string;
    function EffectivePairFieldValue(const reference, fieldName,
      committedValue: string): string;
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
    procedure CommitCredentialReferences(const References: TStrings;
      RemoveLegacy: Boolean); virtual;
    procedure FinishCredentialCleanup; virtual;
    procedure Commit;
    procedure Rollback;
    property PairsTable: TSqlite3Dataset read FPairsTable;
    property InfoTable: TSqlite3Dataset read FInfoTable;
    property FileName: string read FFileName;
    property VerseContentRevision: QWord read FVerseContentRevision;
    property OnContentPendingChanged: TNotifyEvent
      read FOnContentPendingChanged write SetOnContentPendingChanged;
  end;

implementation

uses
  performance_debug, ai_credential_store, sqlite3dyn;

procedure RequireNonSecretInfo(const Key: string);
begin
  if IsLegacyAICredentialKey(Key) then
    raise EAICredentialError.Create(
      'API keys must be stored through the local credential service.');
end;

constructor TProjectPersistence.Create;
begin
  inherited Create;
  FPairOriginalValues := TStringList.Create;
  FPairCurrentValues := TStringList.Create;
  FStagedInfo := TStringList.Create;
  FSilentStagedInfo := TStringList.Create;
  FCommittedInfo := TStringList.Create;
  FImmediateInfoPending := TStringList.Create;
  FInfoTableNeedsRefresh := False;
  FOnContentPendingChanged := nil;
  FLastContentPending := False;
  FPairCommitMayBePartial := False;
  FVerseContentRevision := 1;
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
  IBibliaPerfIncrement(ipcPendingChangeNotifications);
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
  FSilentStagedInfo.Free;
  FStagedInfo.Free;
  FPairOriginalValues.Free;
  FPairCurrentValues.Free;
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
  RequireNonSecretInfo(name);
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
  FPairCurrentValues.Clear;
  FStagedInfo.Clear;
  FSilentStagedInfo.Clear;
  FCommittedInfo.Clear;
  FImmediateInfoPending.Clear;
  FInfoTableNeedsRefresh := False;
  FLastContentPending := False;
  FPairCommitMayBePartial := False;
  Inc(FVerseContentRevision);
end;

procedure TProjectPersistence.Close;
begin
  CloseTable(FPairsTable);
  CloseTable(FInfoTable);
  FPairOriginalValues.Clear;
  FPairCurrentValues.Clear;
  FStagedInfo.Clear;
  FSilentStagedInfo.Clear;
  FCommittedInfo.Clear;
  FImmediateInfoPending.Clear;
  FInfoTableNeedsRefresh := False;
  FLastContentPending := False;
  FPairCommitMayBePartial := False;
  FFileName := '';
  Inc(FVerseContentRevision);
end;

function TProjectPersistence.UpdatesPending: Boolean;
begin
  Result := ContentUpdatesPending;
end;

function TProjectPersistence.ContentUpdatesPending: Boolean;
begin
  Result := FPairCommitMayBePartial or (FPairOriginalValues.Count > 0) or
    (FStagedInfo.Count > 0);
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
    (FSilentStagedInfo.IndexOfName(info) >= 0) or
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
  RequireNonSecretInfo(info);
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
  for Index := 0 to Values.Count - 1 do
    RequireNonSecretInfo(Values.Names[Index]);
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
  RequireNonSecretInfo(info);
  StagedIndex := FStagedInfo.IndexOfName(info);
  if StagedIndex >= 0 then
  begin
    FStagedInfo.Delete(StagedIndex);
    NotifyContentPendingChanged;
  end;
  StagedIndex := FSilentStagedInfo.IndexOfName(info);
  if StagedIndex >= 0 then
    FSilentStagedInfo.Delete(StagedIndex);

  Result := ReadCommittedInfo(info) <> value;
  if not Result then
    Exit;

  Values := TStringList.Create;
  try
    SetNamedValue(Values, info, value);
    try
      WriteInfoValues(Values);
      FInfoTableNeedsRefresh := True;
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
  FInfoTableNeedsRefresh := True;
end;

function TProjectPersistence.ReadInfo(const info: string): string;
var
  Index: Integer;
begin
  Index := FSilentStagedInfo.IndexOfName(info);
  if Index >= 0 then
    Exit(FSilentStagedInfo.ValueFromIndex[Index]);
  Index := FStagedInfo.IndexOfName(info);
  if Index >= 0 then
    Exit(FStagedInfo.ValueFromIndex[Index]);
  Result := ReadCommittedInfo(info);
end;

function TProjectPersistence.EffectivePairFieldValue(const reference,
  fieldName, committedValue: string): string;
var
  Index: Integer;
begin
  Index := FPairCurrentValues.IndexOfName(reference + #1 + fieldName);
  if Index >= 0 then
    Result := FPairCurrentValues.ValueFromIndex[Index]
  else
    Result := committedValue;
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
  CurrentIndex: Integer;
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
  begin
    // Once ApplyUpdates has failed, some earlier batches may already be
    // durable. Keep the original value until retry or Rollback has reconciled
    // that possibly partial commit.
    if not FPairCommitMayBePartial then
    begin
      FPairOriginalValues.Delete(Index);
      CurrentIndex := FPairCurrentValues.IndexOfName(Key);
      if CurrentIndex >= 0 then
        FPairCurrentValues.Delete(CurrentIndex);
    end
    else
      SetNamedValue(FPairCurrentValues, Key, newValue);
  end;
  if newValue <> BaselineValue then
    SetNamedValue(FPairCurrentValues, Key, newValue);
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
  Inc(FVerseContentRevision);
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
  if (fieldName = 'pare_pares') or
     fieldName.StartsWith('pare_texto_') then
    Inc(FVerseContentRevision);
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
begin
  if not Assigned(Table) or not Table.UpdatesPending then
    Exit;

  // TSqlite3Dataset clears its internal update lists before it checks whether
  // the final COMMIT succeeded. Retrying ApplyUpdates after SQLITE_BUSY would
  // therefore report success while silently discarding the pending rows.
  Table.ApplyUpdates;
end;

function PairReferenceFromChangeKey(const ChangeKey: string): string;
var
  SeparatorIndex: Integer;
begin
  SeparatorIndex := Pos(#1, ChangeKey);
  if SeparatorIndex <= 0 then
    Exit('');
  Result := Copy(ChangeKey, 1, SeparatorIndex - 1);
end;

function PairFieldFromChangeKey(const ChangeKey: string): string;
var
  SeparatorIndex: Integer;
begin
  SeparatorIndex := Pos(#1, ChangeKey);
  if SeparatorIndex <= 0 then
    Exit('');
  Result := Copy(ChangeKey, SeparatorIndex + 1, MaxInt);
end;

procedure TProjectPersistence.RebuildPendingPairUpdates;
var
  References: TStringList;
  ChangeKey: string;
  Reference: string;
  CurrentReference: string;
  ControlsWereDisabled: Boolean;
  BeforeScrollHandler: TDataSetNotifyEvent;
  AfterScrollHandler: TDataSetNotifyEvent;
  Index: Integer;
begin
  if FPairOriginalValues.Count = 0 then
    raise EProjectPersistenceRecoveryRequired.Create(
      'The failed pair commit cannot be reconstructed. Roll back and reload ' +
      'the project before making more changes.');

  References := TStringList.Create;
  try
    References.Sorted := True;
    References.Duplicates := dupIgnore;
    for Index := 0 to FPairOriginalValues.Count - 1 do
    begin
      ChangeKey := FPairOriginalValues.Names[Index];
      Reference := PairReferenceFromChangeKey(ChangeKey);
      if not Reference.IsEmpty then
        References.Add(Reference);
    end;

    CurrentReference := CurrentPairId;
    ControlsWereDisabled := FPairsTable.ControlsDisabled;
    BeforeScrollHandler := FPairsTable.BeforeScroll;
    AfterScrollHandler := FPairsTable.AfterScroll;
    if not ControlsWereDisabled then
      FPairsTable.DisableControls;
    FPairsTable.BeforeScroll := nil;
    FPairsTable.AfterScroll := nil;
    try
      try
        for Index := 0 to References.Count - 1 do
        begin
          if not FPairsTable.Locate('pare_id', References[Index], []) then
            raise EProjectPersistenceRecoveryRequired.CreateFmt(
              'Verse Pair %s could not be reloaded after a failed commit.',
              [References[Index]]);
          // InternalPost adds this record back to FUpdatedItems even when the
          // values are unchanged. ApplyUpdates may have cleared that list
          // before propagating SQLITE_BUSY.
          FPairsTable.Edit;
          FPairsTable.Post;
        end;
      finally
        if not CurrentReference.IsEmpty then
          FPairsTable.Locate('pare_id', CurrentReference, []);
      end;
    finally
      FPairsTable.BeforeScroll := BeforeScrollHandler;
      FPairsTable.AfterScroll := AfterScrollHandler;
      if not ControlsWereDisabled then
        FPairsTable.EnableControls;
    end;
  finally
    References.Free;
  end;
end;

procedure TProjectPersistence.RestorePairBaselineAfterFailedCommit;
var
  ChangeKey: string;
  Reference: string;
  FieldName: string;
  CurrentReference: string;
  ActiveReference: string;
  ControlsWereDisabled: Boolean;
  BeforeScrollHandler: TDataSetNotifyEvent;
  AfterScrollHandler: TDataSetNotifyEvent;
  Index: Integer;
begin
  if FPairOriginalValues.Count = 0 then
    raise EProjectPersistenceRecoveryRequired.Create(
      'The original Verse Pair values are unavailable after a failed commit.');

  CurrentReference := CurrentPairId;
  ControlsWereDisabled := FPairsTable.ControlsDisabled;
  BeforeScrollHandler := FPairsTable.BeforeScroll;
  AfterScrollHandler := FPairsTable.AfterScroll;
  if not ControlsWereDisabled then
    FPairsTable.DisableControls;
  FPairsTable.BeforeScroll := nil;
  FPairsTable.AfterScroll := nil;
  try
    if FPairsTable.State in dsEditModes then
      FPairsTable.Cancel;
    FPairsTable.ClearUpdates;
    FPairsTable.Close;
    FPairsTable.Open;

    ActiveReference := '';
    for Index := 0 to FPairOriginalValues.Count - 1 do
    begin
      ChangeKey := FPairOriginalValues.Names[Index];
      Reference := PairReferenceFromChangeKey(ChangeKey);
      FieldName := PairFieldFromChangeKey(ChangeKey);
      if Reference.IsEmpty or FieldName.IsEmpty then
        raise EProjectPersistenceRecoveryRequired.Create(
          'A tracked Verse Pair change is invalid and cannot be restored.');

      if Reference <> ActiveReference then
      begin
        if FPairsTable.State = dsEdit then
          FPairsTable.Post;
        if not FPairsTable.Locate('pare_id', Reference, []) then
          raise EProjectPersistenceRecoveryRequired.CreateFmt(
            'Verse Pair %s could not be restored after a failed commit.',
            [Reference]);
        FPairsTable.Edit;
        ActiveReference := Reference;
      end;
      FPairsTable.FieldByName(FieldName).AsString :=
        FPairOriginalValues.ValueFromIndex[Index];
    end;
    if FPairsTable.State = dsEdit then
      FPairsTable.Post;

    ApplyPendingUpdates(FPairsTable);
  finally
    try
      if FPairsTable.State in dsEditModes then
        FPairsTable.Cancel;
      if not CurrentReference.IsEmpty and FPairsTable.Active then
        FPairsTable.Locate('pare_id', CurrentReference, []);
    finally
      FPairsTable.BeforeScroll := BeforeScrollHandler;
      FPairsTable.AfterScroll := AfterScrollHandler;
      if not ControlsWereDisabled then
        FPairsTable.EnableControls;
    end;
  end;
end;

procedure TProjectPersistence.CommitPairs;
begin
  if not FPairCommitMayBePartial and (FPairOriginalValues.Count = 0) then
  begin
    // TSqlite3Dataset can retain an update item after a field was changed and
    // then restored to its baseline. There is no logical change to publish;
    // discarding that no-op avoids entering recovery merely because another
    // SQLite writer is active.
    if Assigned(FPairsTable) and FPairsTable.UpdatesPending then
      FPairsTable.ClearUpdates;
    NotifyContentPendingChanged;
    Exit;
  end;

  if FPairCommitMayBePartial and not FPairsTable.UpdatesPending then
    RebuildPendingPairUpdates;

  try
    ApplyPendingUpdates(FPairsTable);
  except
    FPairCommitMayBePartial := True;
    try
      RebuildPendingPairUpdates;
    finally
      NotifyContentPendingChanged;
    end;
    raise;
  end;
  FPairCommitMayBePartial := False;
  FPairOriginalValues.Clear;
  FPairCurrentValues.Clear;
  NotifyContentPendingChanged;
end;

function TProjectPersistence.StageInfoSilently(const info,
  value: string): Boolean;
var
  Index: Integer;
  CommittedValue: string;
begin
  RequireNonSecretInfo(info);
  if not Assigned(FInfoTable) then
    raise Exception.Create('Project info table is not open');

  CommittedValue := ReadCommittedInfo(info);
  Index := FSilentStagedInfo.IndexOfName(info);
  if value = CommittedValue then
  begin
    Result := Index >= 0;
    if Result then
      FSilentStagedInfo.Delete(Index);
    Exit;
  end;

  Result := (Index < 0) or
    (FSilentStagedInfo.ValueFromIndex[Index] <> value);
  if Result then
    SetNamedValue(FSilentStagedInfo, info, value);
end;

procedure TProjectPersistence.CommitInfo;
var
  Index: Integer;
  HadContentUpdates: Boolean;
begin
  if (FStagedInfo.Count = 0) and (FSilentStagedInfo.Count = 0) then
    Exit;

  HadContentUpdates := FStagedInfo.Count > 0;

  { Immediate settings are written through a short-lived connection so they
    do not make project content dirty. TSqlite3Dataset keeps its original
    result set, however, and would try to append an already-created row when
    the same key is staged later (the navigation marker is the usual case).
    Refresh once before applying staged values; no staged row has been posted
    to the dataset yet, so this cannot discard pending project content. }
  if FInfoTableNeedsRefresh and Assigned(FInfoTable) and FInfoTable.Active then
  begin
    if FInfoTable.State in dsEditModes then
      FInfoTable.Cancel;
    FInfoTable.Close;
    FInfoTable.Open;
    FInfoTableNeedsRefresh := False;
  end;

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
  for Index := 0 to FSilentStagedInfo.Count - 1 do
  begin
    if FInfoTable.Locate('id', FSilentStagedInfo.Names[Index], []) then
      FInfoTable.Edit
    else
    begin
      FInfoTable.Append;
      FInfoTable.FieldByName('id').AsString :=
        FSilentStagedInfo.Names[Index];
    end;
    FInfoTable.FieldByName('valor').AsString :=
      FSilentStagedInfo.ValueFromIndex[Index];
    FInfoTable.Post;
  end;
  ApplyPendingUpdates(FInfoTable);
  for Index := 0 to FStagedInfo.Count - 1 do
    SetNamedValue(FCommittedInfo, FStagedInfo.Names[Index],
      FStagedInfo.ValueFromIndex[Index]);
  for Index := 0 to FSilentStagedInfo.Count - 1 do
    SetNamedValue(FCommittedInfo, FSilentStagedInfo.Names[Index],
      FSilentStagedInfo.ValueFromIndex[Index]);
  FStagedInfo.Clear;
  FSilentStagedInfo.Clear;
  if HadContentUpdates then
    NotifyContentPendingChanged;
end;

procedure TProjectPersistence.CommitCredentialReferences(
  const References: TStrings; RemoveLegacy: Boolean);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  Index: Integer;
  Scope: TAICredentialScope;
  AllowedKey: Boolean;
begin
  if ContentUpdatesPending then
    raise EAICredentialError.Create(
      'Save project changes before migrating AI credentials.');
  if FFileName = '' then
    raise EAICredentialError.Create('The project is not open.');
  for Index := 0 to References.Count - 1 do
  begin
    AllowedKey := False;
    for Scope in TAICredentialScope do
      AllowedKey := AllowedKey or
        (References.Names[Index] = AICredentialReferenceKey(Scope));
    if not AllowedKey then
      raise EAICredentialError.Create('Only AI credential references may be saved here.');
  end;
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  Query := TSQLQuery.Create(Connection);
  try
    Connection.DatabaseName := FFileName;
    Connection.Transaction := Transaction;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Connection.ExecuteDirect('pragma busy_timeout = 250');
    Connection.ExecuteDirect('pragma secure_delete = on');
    Query.SQL.Text := 'insert or replace into info (id, valor) values (:id, :value)';
    for Index := 0 to References.Count - 1 do
    begin
      Query.ParamByName('id').AsString := References.Names[Index];
      Query.ParamByName('value').AsString := References.ValueFromIndex[Index];
      Query.ExecSQL;
    end;
    if RemoveLegacy then
    begin
      Query.SQL.Text := 'delete from info where id = :id';
      for Scope in TAICredentialScope do
      begin
        Query.ParamByName('id').AsString := AICredentialKey(Scope);
        Query.ExecSQL;
      end;
      Query.ParamByName('id').AsString := AI_LEGACY_COMMON_CREDENTIAL_KEY;
      Query.ExecSQL;
      Connection.ExecuteDirect(
        'insert or replace into info (id, valor) values (' +
        '''ai.credentials.cleanup_pending'', ''1'')');
    end;
    Transaction.Commit;
    { Refresh only metadata; the live Verse and the pairs cursor are unchanged. }
    FCommittedInfo.Clear;
    FInfoTableNeedsRefresh := True;
    FInfoTable.Close;
    FInfoTable.Open;
    FInfoTableNeedsRefresh := False;
  finally
    if Transaction.Active then Transaction.Rollback;
    Connection.Free;
  end;
end;

procedure TProjectPersistence.FinishCredentialCleanup;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Code: Integer;
  procedure ExecOutsideTransaction(const SQL: string);
  begin
    Code := sqlite3_exec(psqlite3(Connection.Handle), PChar(SQL), nil, nil, nil);
    if Code <> SQLITE_OK then
      raise EAICredentialError.CreateFmt(
        'Credential cleanup is pending (SQLite error %d).', [Code]);
  end;
begin
  if ReadInfo(AI_CREDENTIAL_CLEANUP_PENDING_KEY) <> '1' then Exit;
  if ContentUpdatesPending then
    raise EAICredentialError.Create('Credential cleanup requires saved project changes.');
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  try
    Connection.DatabaseName := FFileName;
    Connection.Transaction := Transaction;
    Connection.Open;
    sqlite3_busy_timeout(psqlite3(Connection.Handle), 250);
    { SQLDB ExecuteDirect starts a transaction; VACUUM must run outside one. }
    ExecOutsideTransaction('pragma secure_delete = on');
    ExecOutsideTransaction('vacuum');
    if not Assigned(sqlite3_wal_checkpoint_v2) then
      raise EAICredentialError.Create('SQLite does not support credential WAL cleanup.');
    Code := sqlite3_wal_checkpoint_v2(psqlite3(Connection.Handle), nil,
      SQLITE_CHECKPOINT_TRUNCATE, nil, nil);
    if Code <> SQLITE_OK then
      raise EAICredentialError.CreateFmt(
        'Credential WAL cleanup is pending (SQLite error %d).', [Code]);
    ExecOutsideTransaction(
      'delete from info where id = ''ai.credentials.cleanup_pending''');
    FCommittedInfo.Clear;
    FInfoTableNeedsRefresh := True;
    FInfoTable.Close;
    FInfoTable.Open;
    FInfoTableNeedsRefresh := False;
  finally
    Connection.Free;
  end;
end;

procedure TProjectPersistence.Commit;
begin
  CommitPairs;
  CommitInfo;
end;

procedure TProjectPersistence.Rollback;
var
  CurrentReference: string;
  ContentChanged: Boolean;
begin
  ContentChanged := FPairCurrentValues.Count > 0;
  CurrentReference := CurrentPairId;
  if Assigned(FPairsTable) then
  begin
    if FPairCommitMayBePartial then
      RestorePairBaselineAfterFailedCommit
    else
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
  end;
  FPairCommitMayBePartial := False;
  FPairOriginalValues.Clear;
  FPairCurrentValues.Clear;
  FStagedInfo.Clear;
  FSilentStagedInfo.Clear;
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
  if ContentChanged then
    Inc(FVerseContentRevision);
  NotifyContentPendingChanged;
end;

end.
