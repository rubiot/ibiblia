unit ai_processing_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, llmclient, versiculo, contnrs,
  ai_batch_work_item;

type

  TOnAutoSaveNeeded = procedure(Sender: TObject) of object;
  TGetVerseReferenceFunc = function(Verse: TVersiculo): string of object;
  TOnJobStateChange = procedure(Sender: TObject; JobIndex: Integer; NewState: TAIJobState; ErrorMsg: string) of object;
  TOnJobCompleted = procedure(Sender: TObject; JobIndex: Integer; Result: TAIFlowResult) of object;

  TAIJob = class
  public
    Verse: TVersiculo;
    VersePair: TVersiculo; // separate reference to paired verse (for batch temp verses)
    Flow: IAIVerseFlow;
    State: TAIJobState;
    RawContent: string;
    Parsed: TAIFlowResult;
    ErrorMsg: string;
    AttemptCount: Integer;
    NextRetryTime: TDateTime;
    RequestStartTime: TDateTime;
    WorkItem: TAIBatchWorkItem; // explicit batch input and temp-verse creation seam
    Index: Integer;             // Index in batch for callbacks
    Usage: TLLMUsage;
    destructor Destroy; override;
  end;

  TAIProcessingManager = class; // fwd

  TAIWorkerThread = class(TThread)
  private
    FClient: TBaseLLMClient;
    FModel: string;
    FApiKey: string;
  protected
    FManager: TAIProcessingManager;
    procedure Execute; override;
  public
    constructor Create(AManager: TAIProcessingManager; const AModel, AApiKey: string);
    destructor Destroy; override;
  end;

  TAIProcessingManager = class
  private
    // Fields
    FModel: string;
    FApiKey: string;
    FPending: TObjectQueue; // queue of TAIJob
    FApplying: TObjectQueue; // completed awaiting apply
    FWorkers: TList;
    FLock: TRTLCriticalSection;
    FLockInitialized: Boolean;
    FCancel: Boolean;
    FMaxParallel: Integer;
    FTotalJobs: Integer;
    FCompleted: Integer;
    FFailed: Integer;
    FInFlight: Integer;
    FErrors: TStringList;
    FFlowName: string;
    FMaxRetries: Integer;
    FRetryBaseDelay: Integer; // milliseconds
    FFailedJobs: TList; // list of TAIJob that failed permanently
    // Metrics
    FBatchStartTime: TDateTime;
    FTotalRequestTime: Double; // total seconds spent on requests
    FRequestCount: Integer; // successful request count
    FTotalPromptTokens: Int64;
    FTotalCompletionTokens: Int64;
    FTotalTokens: Int64;
    // Autosave
    FAutoSaveInterval: Integer; // save every N completed jobs
    FLastAutoSaveCount: Integer;
    FOnAutoSaveNeeded: TOnAutoSaveNeeded;
    FGetVerseReference: TGetVerseReferenceFunc;
    // Callbacks
    FOnJobStateChange: TOnJobStateChange;
    FOnJobCompleted: TOnJobCompleted;

    // Private helper methods
    function SafeEnterLock: Boolean;
    procedure SafeLeaveLock;
    procedure EnqueuePending(Job: TAIJob);
    function DequeuePending: TAIJob;
    procedure EnqueueApplying(Job: TAIJob);
    function DequeueApplying: TAIJob;
    procedure NotifyStateChange(Job: TAIJob);
    procedure NotifyJobCompleted(Job: TAIJob);

  public
    constructor Create(const AModel, AApiKey: string; AMaxParallel: Integer = 2);
    destructor Destroy; override;
    procedure StartSingle(Flow: IAIVerseFlow; Verse: TVersiculo); // processes one verse async (apply after PumpApply)
    procedure StartBatch(Flow: IAIVerseFlow; WorkItems: TAIBatchWorkItemList);
    procedure PumpApply; // Call periodically in main thread
    procedure CancelAll;
    procedure RetryFailedJobs; // Re-queue all permanently failed jobs
  // Progress / metrics
  function TotalJobs: Integer;
  function CompletedJobs: Integer;
  function FailedJobs: Integer;
  function PendingJobs: Integer;
  function InFlightJobs: Integer;
  function IsIdle: Boolean;
  function Errors: TStrings; // returns reference (do not free)
  function FlowName: string;
    // Config
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property RetryBaseDelay: Integer read FRetryBaseDelay write FRetryBaseDelay; // milliseconds
    // Metrics
    function AverageRequestTime: Double; // seconds per request
    function EstimatedTimeRemaining: Double; // seconds
    function GetETAFormatted: string; // returns formatted as hh:mm:ss
    function PromptTokens: Int64;
    function CompletionTokens: Int64;
    function TotalTokens: Int64;
    // Autosave
    property AutoSaveInterval: Integer read FAutoSaveInterval write FAutoSaveInterval;
    property OnAutoSaveNeeded: TOnAutoSaveNeeded read FOnAutoSaveNeeded write FOnAutoSaveNeeded;
    property GetVerseReference: TGetVerseReferenceFunc read FGetVerseReference write FGetVerseReference;
    // Callbacks
    property OnJobStateChange: TOnJobStateChange read FOnJobStateChange write FOnJobStateChange;
    property OnJobCompleted: TOnJobCompleted read FOnJobCompleted write FOnJobCompleted;
  end;

implementation

uses
  ai_debug;

{ TAIJob }

destructor TAIJob.Destroy;
begin
  // Free temporary verse objects created for batch processing
  if Assigned(WorkItem) then
  begin
    if Assigned(Verse) then
      Verse.MarkDestroying;
    if Assigned(VersePair) then
      VersePair.MarkDestroying;

    // Free paired verse first (stored separately to avoid double-free)
    if Assigned(VersePair) then
      VersePair.Destroy;
    // Then free main verse
    if Assigned(Verse) then
      Verse.Destroy;
  end;
  if Assigned(WorkItem) then
    WorkItem.Free;
  // Free parsed result
  if Assigned(Parsed) then
    Parsed.Free;
  inherited Destroy;
end;

{ TAIWorkerThread }

constructor TAIWorkerThread.Create(AManager: TAIProcessingManager; const AModel, AApiKey: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FManager := AManager;
  FModel := AModel;
  FApiKey := AApiKey;
  FClient := nil;  // Will be created in Execute to ensure it's in worker thread context
  Start;
end;

destructor TAIWorkerThread.Destroy;
begin
  if Assigned(FClient) then
    FClient.Free;
  inherited;
end;

procedure TAIWorkerThread.Execute;
var
  job: TAIJob;
  msgs: TLLMMessageArray;
  resp: TLLMResponse;
  retryDelay: Integer;
  effectiveProvider, effectiveModel, effectiveApiKey: string;
begin
  AIDebugLog('[Worker] Thread started');
  // Client will be created/switched on demand inside the loop
  FClient := nil;
  
  while not Terminated do
  begin
    if FManager.FCancel then Exit;
    job := FManager.DequeuePending;
    if job = nil then
    begin
      Sleep(100);
      Continue;
    end;
    
    AIDebugLog('[Worker] Got job from queue');
    
    // Check if this is a retry job that's not ready yet
    if (job.AttemptCount > 0) and (Now < job.NextRetryTime) then
    begin
      // Decrement InFlight (was incremented by DequeuePending) before re-queuing,
      // otherwise FInFlight grows by 1 each 100ms sleep cycle.
      FManager.SafeEnterLock;
      try
        Dec(FManager.FInFlight);
      finally
        FManager.SafeLeaveLock;
      end;
      FManager.EnqueuePending(job);
      Sleep(100);
      Continue;
    end;
    
    if FManager.FCancel then Exit;
    try
      // Determine effective provider, model and key for this flow (handling overrides)
      effectiveProvider := job.Flow.ProviderOverride;
      if effectiveProvider = '' then
      begin
        // If no provider override, use the global default which comes from the model info
        // or just pass empty and let CreateBackend infer it.
        // Actually, TAIProcessingManager should probably store the global provider too.
        // For now, let's just use empty string which triggers inference in CreateBackend.
        effectiveProvider := ''; 
      end;

      effectiveModel := job.Flow.ModelOverride;
      if effectiveModel = '' then effectiveModel := FModel; // fallback to global default
      
      effectiveApiKey := job.Flow.ApiKeyOverride;
      if effectiveApiKey = '' then effectiveApiKey := FApiKey; // fallback to global default

      AIDebugLogFmt('[Worker] Processing job index %d, provider=%s, model=%s', 
        [job.Index, effectiveProvider, effectiveModel]);

      // Switch client if needed
      if (FClient = nil) or 
         ((effectiveProvider <> '') and (FClient.Provider <> effectiveProvider)) or
         (FClient.Model <> effectiveModel) or 
         (FClient.ApiKey <> effectiveApiKey) then
      begin
        if Assigned(FClient) then FClient.Free;
        AIDebugLogFmt('[Worker] Creating/Switching LLM client for provider: %s, model: %s', [effectiveProvider, effectiveModel]);
        FClient := llmclient.CreateBackend(effectiveProvider, effectiveModel, effectiveApiKey);
      end;

      job.State := ajsFetching;
      Inc(job.AttemptCount);
      job.RequestStartTime := Now;
      job.Usage.PromptTokens := 0;
      job.Usage.CompletionTokens := 0;
      job.Usage.TotalTokens := 0;
      
      // Batch work items own the lazy verse-creation seam.
      // NOTE: Each verse creates its own TONTParser instance, so there should be no shared state.
      // However, if deadlocks persist, consider adding a global mutex around ParseLine calls.
      if Assigned(job.WorkItem) then
      begin
        AIDebugLogFmt('[Worker] Materializing batch work item for job index %d (%s)',
          [job.Index, job.WorkItem.VerseData.ReferenceID]);
        job.WorkItem.CreateTempVersePair(job.Verse, job.VersePair);
        AIDebugLogFmt('[Worker] Verses created for job index %d', [job.Index]);
      end;
      
      AIDebugLogFmt('[Worker] Building messages for job index %d', [job.Index]);
      msgs := job.Flow.BuildMessages(job.Verse);
      AIDebugLogFmt('[Worker] Calling API for job index %d', [job.Index]);
      resp := FClient.AskChatCompletions(msgs);  // Use worker's own client
      AIDebugLogFmt('[Worker] API response received for job index %d, length=%d', [job.Index, Length(resp.Content)]);
      job.RawContent := resp.Content;
      job.Usage := resp.Usage;
      job.Parsed := job.Flow.ParseResponse(resp.Content, job.Verse);
      if Assigned(job.Parsed) and (job.Parsed.ParseError <> '') then
        raise Exception.Create('Parse error: ' + job.Parsed.ParseError);
      job.State := ajsCompleted;
      AIDebugLog('[Worker] Job completed, enqueuing for apply');
      // Track metrics
        FManager.SafeEnterLock;
      try
        FManager.FTotalRequestTime := FManager.FTotalRequestTime + (Now - job.RequestStartTime) * 24 * 60 * 60; // Convert to seconds
        Inc(FManager.FRequestCount);
      finally
        FManager.SafeLeaveLock;
      end;
    except
      on E: Exception do
      begin
        AIDebugLogFmt('[Worker] Exception in job processing: %s', [E.Message]);
        job.ErrorMsg := E.Message;
        // Decide whether to retry
        if job.AttemptCount < FManager.FMaxRetries then
        begin
          // Calculate exponential backoff: base_delay * 2^(attempt-1)
          retryDelay := FManager.FRetryBaseDelay * (1 shl (job.AttemptCount - 1));
          job.NextRetryTime := Now + (retryDelay / (24 * 60 * 60 * 1000)); // Convert ms to days
          job.State := ajsPending;
          // Decrement FInFlight before re-queueing (will be incremented again when dequeued)
          FManager.SafeEnterLock;
          try
            Dec(FManager.FInFlight);
            AIDebugLogFmt('[Worker] Decremented FInFlight for retry. Now: %d', [FManager.FInFlight]);
          finally
            FManager.SafeLeaveLock;
          end;
          FManager.EnqueuePending(job); // Re-queue for retry
          Continue; // Don't send to apply queue yet
        end
        else
        begin
          job.State := ajsFailed;
          // Store in failed jobs list for potential manual retry
          if FManager.SafeEnterLock then
          try
            FManager.FFailedJobs.Add(job);
          finally
            FManager.SafeLeaveLock;
          end;
        end;
      end;
    end;
    FManager.EnqueueApplying(job);
      AIDebugLogFmt('[Worker] Job enqueued to applying queue. JobPtr=%s, Index=%d', [IntToHex(PtrUInt(Pointer(job)), SizeOf(Pointer)*2), job.Index]);
  end;
end;

{ TAIProcessingManager }

constructor TAIProcessingManager.Create(const AModel, AApiKey: string; AMaxParallel: Integer);
begin
  inherited Create;
  
  // Initialize OpenSSL in main thread BEFORE creating any worker threads
  // This prevents race condition when multiple threads try to initialize simultaneously
  llmclient.EnsureSSLInitialized;
  
  FModel := AModel;
  FApiKey := AApiKey;
  FPending := TObjectQueue.Create;
  FApplying := TObjectQueue.Create;
  FWorkers := TList.Create;
  FFailedJobs := TList.Create;
  InitCriticalSection(FLock);
  FLockInitialized := True;
  FCancel := False;
  FMaxParallel := AMaxParallel;
  FTotalJobs := 0;
  FCompleted := 0;
  FFailed := 0;
  FInFlight := 0;
  FErrors := TStringList.Create;
  FMaxRetries := 3;
  FRetryBaseDelay := 1000; // 1 second base delay
  FBatchStartTime := 0;
  FTotalRequestTime := 0;
  FRequestCount := 0;
  FTotalPromptTokens := 0;
  FTotalCompletionTokens := 0;
  FTotalTokens := 0;
  FAutoSaveInterval := 10;
  FLastAutoSaveCount := 0;
  FOnAutoSaveNeeded := nil;
  FGetVerseReference := nil;
end;

destructor TAIProcessingManager.Destroy;
var
  i: Integer;
begin
  CancelAll;
  for i := 0 to FWorkers.Count - 1 do
    TThread(FWorkers[i]).WaitFor;
  for i := 0 to FWorkers.Count - 1 do
    TObject(FWorkers[i]).Free;
  FWorkers.Free;
  if FLockInitialized then
  begin
    // Mark as uninitialized before calling DoneCriticalSection to reduce
    // the race window where another thread might try to enter the lock
    // while it's being destroyed.
    FLockInitialized := False;
    try
      DoneCriticalSection(FLock);
    except
      on E: Exception do
        AIDebugLogFmt('[Manager] Destroy: DoneCriticalSection raised exception: %s', [E.Message]);
    end;
  end
  else
    AIDebugLog('[Manager] Destroy: FLock was not initialized or already done');
  while FPending.Count > 0 do TObject(FPending.Pop).Free;
  while FApplying.Count > 0 do TObject(FApplying.Pop).Free;
  FPending.Free;
  FApplying.Free;
  for i := 0 to FFailedJobs.Count - 1 do
    TObject(FFailedJobs[i]).Free;
  FFailedJobs.Free;
  FErrors.Free;
  inherited Destroy;
end;

function TAIProcessingManager.SafeEnterLock: Boolean;
begin
  Result := False;
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] SafeEnterLock: Attempt to enter uninitialized lock detected');
    Exit;
  end;
  try
    EnterCriticalSection(FLock);
    Result := True;
  except
    on E: Exception do
    begin
      AIDebugLogFmt('[Manager] SafeEnterLock: EnterCriticalSection failed: %s', [E.Message]);
      Result := False;
    end;
  end;
end;

procedure TAIProcessingManager.SafeLeaveLock;
begin
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] SafeLeaveLock: Attempt to leave uninitialized lock detected');
    Exit;
  end;
  try
    LeaveCriticalSection(FLock);
  except
    on E: Exception do
      AIDebugLogFmt('[Manager] SafeLeaveLock: LeaveCriticalSection failed: %s', [E.Message]);
  end;
end;

procedure TAIProcessingManager.EnqueuePending(Job: TAIJob);
begin
  AIDebugLogFmt('[Manager] EnqueuePending called. LockAddr=%s, InFlight=%d, PendingCount(before)=%d', [IntToHex(PtrUInt(@FLock), SizeOf(Pointer)*2), FInFlight, FPending.Count]);
  if not SafeEnterLock then
  begin
    AIDebugLog('[Manager] EnqueuePending: Failed to acquire lock, aborting');
    Exit;
  end;
  try
    FPending.Push(Job);
    AIDebugLogFmt('[Manager] EnqueuePending pushed. PendingCount(after)=%d', [FPending.Count]);
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.DequeuePending: TAIJob;
begin
  Result := nil;
  if not SafeEnterLock then
  begin
    AIDebugLog('[Manager] DequeuePending: Failed to acquire lock');
    Exit;
  end;
  try
    if FPending.Count > 0 then
    begin
      Result := TAIJob(FPending.Pop);
      Inc(FInFlight); // Increment when job actually starts processing
      AIDebugLogFmt('[Manager] DequeuePending: Job popped from pending queue. Remaining: %d, InFlight: %d', [FPending.Count, FInFlight]);
    end;
  finally
    SafeLeaveLock;
  end;
end;

procedure TAIProcessingManager.EnqueueApplying(Job: TAIJob);
begin
  AIDebugLogFmt('[Manager] EnqueueApplying called. LockAddr=%s, ApplyingCount(before)=%d', [IntToHex(PtrUInt(@FLock), SizeOf(Pointer)*2), FApplying.Count]);
  if not SafeEnterLock then
  begin
    AIDebugLog('[Manager] EnqueueApplying: Failed to acquire lock, aborting');
    Exit;
  end;
  try
    FApplying.Push(Job);
    AIDebugLogFmt('[Manager] Job pushed to applying queue. Queue size: %d, JobState: %d', [FApplying.Count, Ord(Job.State)]);
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.DequeueApplying: TAIJob;
begin
  Result := nil;
  if not SafeEnterLock then
  begin
    AIDebugLog('[Manager] DequeueApplying: could not acquire lock, returning nil');
    Exit;
  end;
  try
    if FApplying.Count > 0 then
    begin
      Result := TAIJob(FApplying.Pop);
      AIDebugLogFmt('[Manager] DequeueApplying: Job popped from applying queue. Remaining: %d', [FApplying.Count]);
    end;
  finally
    SafeLeaveLock;
  end;
end;

procedure TAIProcessingManager.StartSingle(Flow: IAIVerseFlow; Verse: TVersiculo);
var
  job: TAIJob;
begin
  AIDebugLog('[Manager] StartSingle called');
  job := TAIJob.Create;
  FFlowName := Flow.Name;
  job.Flow := Flow;
  job.Verse := Verse;
  // Single jobs use visual verses (WorkItem = nil) so destructor won't free them
  job.State := ajsPending;
  job.AttemptCount := 0;
  job.NextRetryTime := 0;
  job.Usage.PromptTokens := 0;
  job.Usage.CompletionTokens := 0;
  job.Usage.TotalTokens := 0;
  SafeEnterLock; try Inc(FTotalJobs); finally SafeLeaveLock; end;
  EnqueuePending(job);
  AIDebugLogFmt('[Manager] Job enqueued, creating worker. TotalJobs=%d', [FTotalJobs]);
  if FWorkers.Count = 0 then
    FWorkers.Add(TAIWorkerThread.Create(Self, FModel, FApiKey));
  AIDebugLogFmt('[Manager] Workers count: %d', [FWorkers.Count]);
end;

procedure TAIProcessingManager.StartBatch(Flow: IAIVerseFlow; WorkItems: TAIBatchWorkItemList);
var
  i: Integer;
  job: TAIJob;
  item: TAIBatchWorkItem;
  activeWorkersNeeded: Integer;
  jobCount: Integer;
begin
  if (Flow = nil) or not Assigned(WorkItems) or (WorkItems.Count = 0) then Exit;
  jobCount := WorkItems.Count;
  AIDebugLogFmt('[Manager] StartBatch called. Jobs count: %d, MaxParallel configuration: %d', [jobCount, FMaxParallel]);
  FFlowName := Flow.Name;
  FBatchStartTime := Now;
  for i := 0 to jobCount - 1 do
  begin
    item := WorkItems.ExtractItem(0);
    job := TAIJob.Create;
    job.Flow := Flow;
    job.State := ajsPending;
    job.AttemptCount := 0;
    job.NextRetryTime := 0;
    job.Usage.PromptTokens := 0;
    job.Usage.CompletionTokens := 0;
    job.Usage.TotalTokens := 0;
    // WorkItem set means batch job — destructor will free temp verses
    job.Index := i; // Store index for callbacks
    job.WorkItem := item;
    job.Verse := nil;
    job.VersePair := nil;
    
    EnqueuePending(job);
    NotifyStateChange(job); // Notify that job is pending
  end;
  SafeEnterLock; try Inc(FTotalJobs, jobCount); finally SafeLeaveLock; end;
  
  // Create worker threads up to FMaxParallel
  activeWorkersNeeded := jobCount;
  if activeWorkersNeeded > FMaxParallel then
    activeWorkersNeeded := FMaxParallel;

  if FWorkers.Count < activeWorkersNeeded then
    AIDebugLogFmt('[Manager] Starting %d worker threads (batch size=%d, MaxParallel=%d)', 
      [activeWorkersNeeded - FWorkers.Count, jobCount, FMaxParallel]);

  while (FWorkers.Count < activeWorkersNeeded) do
    FWorkers.Add(TAIWorkerThread.Create(Self, FModel, FApiKey));
end;

procedure TAIProcessingManager.PumpApply;
var
  job: TAIJob;
  shouldFree: Boolean;
  jobsProcessed: Integer;
  queueSize: Integer;
  triggerAutoSave: Boolean;
begin
  if not SafeEnterLock then
  begin
    AIDebugLog('[Manager] PumpApply: could not acquire lock to read queue size; aborting PumpApply');
    Exit;
  end;
  try
    queueSize := FApplying.Count;
  finally
    SafeLeaveLock;
  end;
  
  if queueSize > 0 then
    AIDebugLogFmt('[Manager] PumpApply starting. Applying queue size: %d', [queueSize]);
    
  jobsProcessed := 0;
  repeat
    job := DequeueApplying;
    if job <> nil then
    begin
      Inc(jobsProcessed);
      AIDebugLogFmt('[Manager] PumpApply processing job (ptr=%s), State=%d', [IntToHex(PtrUInt(Pointer(job)), SizeOf(Pointer)*2), Ord(job.State)]);
      shouldFree := True;
      triggerAutoSave := False;
      try
        SafeEnterLock;
        try
          FTotalPromptTokens := FTotalPromptTokens + job.Usage.PromptTokens;
          FTotalCompletionTokens := FTotalCompletionTokens + job.Usage.CompletionTokens;
          if job.Usage.TotalTokens > 0 then
            FTotalTokens := FTotalTokens + job.Usage.TotalTokens
          else
            FTotalTokens := FTotalTokens + job.Usage.PromptTokens + job.Usage.CompletionTokens;
        finally
          SafeLeaveLock;
        end;

        if job.State = ajsCompleted then
        begin
          AIDebugLog('[Manager] Job completed, calling ApplyResult');
          try
            // Defensive checks: ensure flow and parsed result are present
            if not Assigned(job.Flow) then
              raise Exception.Create('Missing Flow in job when applying result');
            if not Assigned(job.Parsed) then
              raise Exception.Create('Missing Parsed result in job when applying result');

            // Diagnostic logging: job metadata right before ApplyResult
            AIDebugLogFmt('[Manager] About to call ApplyResult. JobPtr=%s, JobIndex=%d, BatchJob=%s, VerseAssigned=%s, RawContentLen=%d, FlowPtr=%s, ParsedPtr=%s',
              [IntToHex(PtrUInt(Pointer(job)), SizeOf(Pointer)*2), job.Index, BoolToStr(Assigned(job.WorkItem), True), BoolToStr(Assigned(job.Verse), True), Length(job.RawContent),
               IntToHex(PtrUInt(Pointer(job.Flow)), SizeOf(Pointer)*2), IntToHex(PtrUInt(Pointer(job.Parsed)), SizeOf(Pointer)*2)]);
            if Assigned(job.Verse) then
              AIDebugLogFmt('[Manager]   VersePtr=%s, Verse ref: %s (ID=%s)', [IntToHex(PtrUInt(Pointer(job.Verse)), SizeOf(Pointer)*2), job.Verse.Referencia, job.Verse.ReferenciaID]);

            // Call ApplyResult on main thread; protect against exceptions coming from UI code
            job.Flow.ApplyResult(job.Parsed, job.Verse);
            AIDebugLog('[Manager] ApplyResult returned');
            SafeEnterLock;
            try
              Inc(FCompleted);
              Dec(FInFlight); // Decrementa FInFlight quando job é completamente processado
              // Check if autosave is needed
              if (FAutoSaveInterval > 0) and Assigned(FOnAutoSaveNeeded) then
              begin
                if (FCompleted - FLastAutoSaveCount) >= FAutoSaveInterval then
                begin
                  FLastAutoSaveCount := FCompleted;
                  triggerAutoSave := True;
                end;
              end;
            finally
              SafeLeaveLock;
            end;

            NotifyStateChange(job); // Update UI icon to success
            NotifyJobCompleted(job); // Notify completion

            if triggerAutoSave then
              FOnAutoSaveNeeded(Self);
          except
            on E: Exception do
            begin
              // If ApplyResult raises, record the job as failed instead of crashing the app
              AIDebugLogFmt('[Manager] Exception during ApplyResult: %s', [E.Message]);
              job.ErrorMsg := E.Message;
              job.State := ajsFailed;
              shouldFree := False; // keep job object in failed list for inspection/retry
              SafeEnterLock;
              try
                Inc(FFailed);
                Dec(FInFlight);
                FFailedJobs.Add(job);
                if Assigned(FGetVerseReference) and Assigned(job.Verse) then
                  FErrors.Add(Format('[%s] Apply exception: %s', [FGetVerseReference(job.Verse), job.ErrorMsg]))
                else
                  FErrors.Add(Format('[Unknown verse] Apply exception: %s', [job.ErrorMsg]));
              finally
                SafeLeaveLock;
              end;
            end;
          end;
        end
        else if job.State = ajsFailed then
        begin
          AIDebugLog('[Manager] Job failed, recording error');
          NotifyStateChange(job); // Update UI icon to error
          shouldFree := False; // Don't free, it's in FFailedJobs list
          SafeEnterLock;
          try
            Inc(FFailed);
            Dec(FInFlight); // Decrementa FInFlight quando job falha definitivamente
            if Assigned(FGetVerseReference) and Assigned(job.Verse) then
              FErrors.Add(Format('[%s] Attempt %d/%d: %s', [
                FGetVerseReference(job.Verse),
                job.AttemptCount,
                FMaxRetries,
                job.ErrorMsg
              ]))
            else
              FErrors.Add(Format('[Unknown verse] Attempt %d/%d: %s', [
                job.AttemptCount,
                FMaxRetries,
                job.ErrorMsg
              ]));
          finally
            SafeLeaveLock;
          end;
        end;
      finally
        if Assigned(job.Parsed) then
        begin
          job.Parsed.Free;
          job.Parsed := nil; // Prevent double-free in destructor
        end;
        if shouldFree then
        begin
          AIDebugLogFmt('[Manager] Freeing job object at %s (Index=%d)', [IntToHex(PtrUInt(Pointer(job)), SizeOf(Pointer)*2), job.Index]);
          job.Free;
        end;
      end;
    end;
  until job = nil;
  if jobsProcessed > 0 then
    AIDebugLogFmt('[Manager] PumpApply processed %d jobs', [jobsProcessed]);
end;

procedure TAIProcessingManager.CancelAll;
var
  i: Integer;
begin
    // Signal cancellation and politely stop all worker threads, then wait for them.
    if FCancel then Exit; // already cancelling
    FCancel := True;
    // Terminate worker threads
    for i := 0 to FWorkers.Count - 1 do
    begin
      if Assigned(FWorkers[i]) then
        TThread(FWorkers[i]).Terminate;
    end;
    // Wait for them to finish
    for i := 0 to FWorkers.Count - 1 do
    begin
      if Assigned(FWorkers[i]) then
        TThread(FWorkers[i]).WaitFor;
    end;
    // Free worker thread objects
    for i := 0 to FWorkers.Count - 1 do
    begin
      if Assigned(FWorkers[i]) then
        TObject(FWorkers[i]).Free;
    end;
    FWorkers.Clear;
end;

procedure TAIProcessingManager.RetryFailedJobs;
var
  i: Integer;
  job: TAIJob;
begin
  SafeEnterLock;
  try
    for i := 0 to FFailedJobs.Count - 1 do
    begin
      job := TAIJob(FFailedJobs[i]);
      job.State := ajsPending;
      job.AttemptCount := 0; // Reset retry count
      job.NextRetryTime := 0;
      job.ErrorMsg := '';
      EnqueuePending(job);
      Dec(FFailed); // Adjust counter
    end;
    FFailedJobs.Clear; // Clear list but don't free objects (they're re-queued)
  finally
    SafeLeaveLock;
  end;
  // Ensure workers are running
  while (FWorkers.Count < FMaxParallel) do
    FWorkers.Add(TAIWorkerThread.Create(Self, FModel, FApiKey));
end;

function TAIProcessingManager.TotalJobs: Integer;
begin
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] TotalJobs called on uninitialized manager');
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    Result := FTotalJobs;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.CompletedJobs: Integer;
begin
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] CompletedJobs called on uninitialized manager');
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    Result := FCompleted;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.FailedJobs: Integer;
begin
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] FailedJobs called on uninitialized manager');
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    Result := FFailed;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.PendingJobs: Integer;
begin
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] PendingJobs called on uninitialized manager');
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    // Additional safety: check queues are still valid before accessing
    if not Assigned(FPending) or not Assigned(FApplying) then
      Result := 0 // Queues freed during shutdown
    else
      // FInFlight already counts jobs being processed (dequeued but not yet in applying)
      // So we count: pending (not started) + in-flight (being processed) + applying (done, waiting for main thread)
      Result := FPending.Count + FInFlight + FApplying.Count;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.InFlightJobs: Integer;
begin
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] InFlightJobs called on uninitialized manager');
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    Result := FInFlight;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.IsIdle: Boolean;
begin
  if not FLockInitialized then
  begin
    AIDebugLog('[Manager] IsIdle called on uninitialized manager');
    Result := True;  // Assume idle during shutdown
    Exit;
  end;
  if not SafeEnterLock then
  begin
    // During shutdown, if we can't get the lock, assume not idle (defer further processing)
    Result := False;
    Exit;
  end;
  try
    // Additional safety: check queues are still valid before accessing
    if not Assigned(FPending) or not Assigned(FApplying) then
    begin
      Result := False; // Queues freed during shutdown
      Exit;
    end;
    Result := (FPending.Count = 0) and (FApplying.Count = 0) and (FInFlight = 0);
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.Errors: TStrings;
begin
  Result := FErrors;
end;

function TAIProcessingManager.FlowName: string;
begin
  Result := FFlowName;
end;

function TAIProcessingManager.AverageRequestTime: Double;
begin
  SafeEnterLock;
  try
    if FRequestCount > 0 then
      Result := FTotalRequestTime / FRequestCount
    else
      Result := 0;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.EstimatedTimeRemaining: Double;
var
  avgTime: Double;
  remaining: Integer;
begin
  SafeEnterLock;
  try
    remaining := FTotalJobs - FCompleted - FFailed;
    if (remaining <= 0) or (FRequestCount = 0) then
    begin
      Result := 0;
      Exit;
    end;
    avgTime := FTotalRequestTime / FRequestCount;
    // Adjust for parallelism: time = (remaining * avg) / parallel_workers
    Result := (remaining * avgTime) / FMaxParallel;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.GetETAFormatted: string;
var
  etaSeconds: Double;
  hours, minutes, seconds: Integer;
begin
  etaSeconds := EstimatedTimeRemaining;
  if etaSeconds <= 0 then
  begin
    Result := '--:--:--';
    Exit;
  end;
  hours := Trunc(etaSeconds / 3600);
  minutes := Trunc((etaSeconds - hours * 3600) / 60);
  seconds := Trunc(etaSeconds - hours * 3600 - minutes * 60);
  Result := Format('%.2d:%.2d:%.2d', [hours, minutes, seconds]);
end;

function TAIProcessingManager.PromptTokens: Int64;
begin
  if not FLockInitialized then
  begin
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    Result := FTotalPromptTokens;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.CompletionTokens: Int64;
begin
  if not FLockInitialized then
  begin
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    Result := FTotalCompletionTokens;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.TotalTokens: Int64;
begin
  if not FLockInitialized then
  begin
    Result := 0;
    Exit;
  end;
  if not SafeEnterLock then
    Result := 0
  else
  try
    Result := FTotalTokens;
  finally
    SafeLeaveLock;
  end;
end;

procedure TAIProcessingManager.NotifyStateChange(Job: TAIJob);
begin
  if Assigned(FOnJobStateChange) then
    FOnJobStateChange(Self, Job.Index, Job.State, Job.ErrorMsg);
end;

procedure TAIProcessingManager.NotifyJobCompleted(Job: TAIJob);
begin
  if Assigned(FOnJobCompleted) and Assigned(Job.Parsed) then
    FOnJobCompleted(Self, Job.Index, Job.Parsed);
end;

end.
