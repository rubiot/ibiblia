unit ai_batch_orchestrator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ai_flows, ai_flow_config,
  ai_processing_manager, ai_batch_work_item, project_config;

type
  TCancelAIBatchProc = procedure of object;
  TFindTreeNodeByReferenceFunc = function(const Ref: string): TTreeNode of object;
  TMarkNodePendingProc = procedure(Node: TTreeNode; const Ref: string) of object;

  IAIBatchContext = interface
    ['{A3F9D21C-6B7E-4C1A-89D4-F2E51B730A8F}']
    procedure CancelCurrentBatch;
    function FindTreeNodeByReference(const Ref: string): TTreeNode;
    procedure MarkNodePending(Node: TTreeNode; const Ref: string);
    function GetAutoSaveCallback: TOnAutoSaveNeeded;
    function GetVerseReferenceCallback: TGetVerseReferenceFunc;
  end;

  TAIBatchContextAdapter = class(TInterfacedObject, IAIBatchContext)
  private
    FCancelProc: TCancelAIBatchProc;
    FFindNodeFunc: TFindTreeNodeByReferenceFunc;
    FMarkPendingProc: TMarkNodePendingProc;
    FAutoSaveProc: TOnAutoSaveNeeded;
    FGetRefFunc: TGetVerseReferenceFunc;
  public
    constructor Create(
      ACancelProc: TCancelAIBatchProc;
      AFindNodeFunc: TFindTreeNodeByReferenceFunc;
      AMarkPendingProc: TMarkNodePendingProc;
      AAutoSaveProc: TOnAutoSaveNeeded;
      AGetRefFunc: TGetVerseReferenceFunc);
    procedure CancelCurrentBatch;
    function FindTreeNodeByReference(const Ref: string): TTreeNode;
    procedure MarkNodePending(Node: TTreeNode; const Ref: string);
    function GetAutoSaveCallback: TOnAutoSaveNeeded;
    function GetVerseReferenceCallback: TGetVerseReferenceFunc;
  end;

  TAIBatchPreparation = record
    Flow: IAIVerseFlow;
    Manager: TAIProcessingManager;
  end;

  TAIBatchOrchestrator = class
  private
    FConfig: TProjectConfig;
    FContext: IAIBatchContext;
    procedure MarkPendingNodes(WorkItems: TAIBatchWorkItemList);
  public
    constructor Create(AConfig: TProjectConfig; AContext: IAIBatchContext);
    function PrepareBatch(const FlowId: string; WorkItems: TAIBatchWorkItemList;
      MaxParallel: Integer): TAIBatchPreparation;
  end;

implementation

{ TAIBatchContextAdapter }

constructor TAIBatchContextAdapter.Create(
  ACancelProc: TCancelAIBatchProc;
  AFindNodeFunc: TFindTreeNodeByReferenceFunc;
  AMarkPendingProc: TMarkNodePendingProc;
  AAutoSaveProc: TOnAutoSaveNeeded;
  AGetRefFunc: TGetVerseReferenceFunc);
begin
  inherited Create;
  FCancelProc := ACancelProc;
  FFindNodeFunc := AFindNodeFunc;
  FMarkPendingProc := AMarkPendingProc;
  FAutoSaveProc := AAutoSaveProc;
  FGetRefFunc := AGetRefFunc;
end;

procedure TAIBatchContextAdapter.CancelCurrentBatch;
begin
  if Assigned(FCancelProc) then FCancelProc;
end;

function TAIBatchContextAdapter.FindTreeNodeByReference(const Ref: string): TTreeNode;
begin
  if Assigned(FFindNodeFunc) then
    Result := FFindNodeFunc(Ref)
  else
    Result := nil;
end;

procedure TAIBatchContextAdapter.MarkNodePending(Node: TTreeNode; const Ref: string);
begin
  if Assigned(FMarkPendingProc) then FMarkPendingProc(Node, Ref);
end;

function TAIBatchContextAdapter.GetAutoSaveCallback: TOnAutoSaveNeeded;
begin
  Result := FAutoSaveProc;
end;

function TAIBatchContextAdapter.GetVerseReferenceCallback: TGetVerseReferenceFunc;
begin
  Result := FGetRefFunc;
end;

{ TAIBatchOrchestrator }

constructor TAIBatchOrchestrator.Create(AConfig: TProjectConfig; AContext: IAIBatchContext);
begin
  inherited Create;
  FConfig := AConfig;
  FContext := AContext;
end;

procedure TAIBatchOrchestrator.MarkPendingNodes(WorkItems: TAIBatchWorkItemList);
var
  I: Integer;
  Node: TTreeNode;
  VerseData: TVerseData;
begin
  if not Assigned(WorkItems) then
    Exit;

  for I := 0 to WorkItems.Count - 1 do
  begin
    VerseData := WorkItems[I].VerseData;
    if VerseData.ReferenceID.IsEmpty then
      Continue;

    if not Assigned(FContext) then
      Continue;

    Node := FContext.FindTreeNodeByReference(VerseData.ReferenceID);
    if Assigned(Node) then
      FContext.MarkNodePending(Node, VerseData.ReferenceID);
  end;
end;

function TAIBatchOrchestrator.PrepareBatch(const FlowId: string;
  WorkItems: TAIBatchWorkItemList; MaxParallel: Integer): TAIBatchPreparation;
var
  FlowConfig: IAIFlowConfig;
begin
  Result.Flow := nil;
  Result.Manager := nil;

  if not Assigned(WorkItems) or (WorkItems.Count = 0) then
    Exit;

  if MaxParallel <= 0 then
    MaxParallel := 1;

  if Assigned(FContext) then
    FContext.CancelCurrentBatch;

  MarkPendingNodes(WorkItems);

  FlowConfig := GetFlowConfigById(FlowId);
  if not Assigned(FlowConfig) then
    Exit;

  Result.Flow := FlowConfig.CreateFlowInstance(CreateAIFlowContext(FConfig));
  if not Assigned(Result.Flow) then
    Exit;

  Result.Manager := TAIProcessingManager.Create(
    FConfig.ReadString(AI_COMMON_CONFIG.Model),
    FConfig.ReadString(AI_COMMON_CONFIG.ApiKey),
    MaxParallel
  );
  Result.Manager.MaxRetries := StrToIntDef(FConfig.ReadString(AI_COMMON_CONFIG.MaxRetries), 3);
  Result.Manager.RetryBaseDelay := StrToIntDef(FConfig.ReadString(AI_COMMON_CONFIG.RetryDelay), 1000);
  Result.Manager.AutoSaveInterval := StrToIntDef(FConfig.ReadString(AI_COMMON_CONFIG.AutoSaveInterval), 10);
  if Assigned(FContext) then
  begin
    Result.Manager.OnAutoSaveNeeded := FContext.GetAutoSaveCallback;
    Result.Manager.GetVerseReference := FContext.GetVerseReferenceCallback;
  end;
end;

end.