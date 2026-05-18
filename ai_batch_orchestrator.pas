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

  TAIBatchPreparation = record
    Flow: IAIVerseFlow;
    Manager: TAIProcessingManager;
  end;

  TAIBatchOrchestrator = class
  private
    FConfig: TProjectConfig;
    FCancelCurrentBatch: TCancelAIBatchProc;
    FFindTreeNodeByReference: TFindTreeNodeByReferenceFunc;
    FMarkNodePending: TMarkNodePendingProc;
    FOnAutoSaveNeeded: TOnAutoSaveNeeded;
    FGetVerseReference: TGetVerseReferenceFunc;
    procedure MarkPendingNodes(WorkItems: TAIBatchWorkItemList);
  public
    constructor Create(AConfig: TProjectConfig;
      const ACancelCurrentBatch: TCancelAIBatchProc;
      const AFindTreeNodeByReference: TFindTreeNodeByReferenceFunc;
      const AMarkNodePending: TMarkNodePendingProc;
      const AOnAutoSaveNeeded: TOnAutoSaveNeeded;
      const AGetVerseReference: TGetVerseReferenceFunc);
    function PrepareBatch(const FlowId: string; WorkItems: TAIBatchWorkItemList;
      MaxParallel: Integer): TAIBatchPreparation;
  end;

implementation

constructor TAIBatchOrchestrator.Create(AConfig: TProjectConfig;
  const ACancelCurrentBatch: TCancelAIBatchProc;
  const AFindTreeNodeByReference: TFindTreeNodeByReferenceFunc;
  const AMarkNodePending: TMarkNodePendingProc;
  const AOnAutoSaveNeeded: TOnAutoSaveNeeded;
  const AGetVerseReference: TGetVerseReferenceFunc);
begin
  inherited Create;
  FConfig := AConfig;
  FCancelCurrentBatch := ACancelCurrentBatch;
  FFindTreeNodeByReference := AFindTreeNodeByReference;
  FMarkNodePending := AMarkNodePending;
  FOnAutoSaveNeeded := AOnAutoSaveNeeded;
  FGetVerseReference := AGetVerseReference;
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

    if not Assigned(FFindTreeNodeByReference) or not Assigned(FMarkNodePending) then
      Continue;

    Node := FFindTreeNodeByReference(VerseData.ReferenceID);
    if Assigned(Node) then
      FMarkNodePending(Node, VerseData.ReferenceID);
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

  if Assigned(FCancelCurrentBatch) then
    FCancelCurrentBatch;

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
  Result.Manager.OnAutoSaveNeeded := FOnAutoSaveNeeded;
  Result.Manager.GetVerseReference := FGetVerseReference;
end;

end.