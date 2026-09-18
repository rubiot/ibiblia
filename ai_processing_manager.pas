unit ai_processing_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, llmclient, Verse, contnrs,
  ai_batch_work_item;

type

  TOnAutoSaveNeeded = procedure(Sender: TObject) of object;
  TGetVerseReferenceFunc = function(Verse: TVerse): string of object;
  TOnJobStateChange = procedure(Sender: TObject; JobIndex: Integer; NewState: TAIJobState; ErrorMsg: string) of object;
  TOnJobCompleted = procedure(Sender: TObject; JobIndex: Integer; Result: TAIFlowResult) of object;

  TAIJob = class
  public
    Verse: TVerse;
    VersePair: TVerse; // separate reference to paired verse (for batch temp verses)
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
    procedure ReleaseTempVerses;
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
    FResolvedProvider: string;
    FConnectionResolved: Boolean;
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
    FCancelled: Integer;
    FInFlight: Integer;
    FPumping: Boolean;
    FBorrowedVerses: Boolean;
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
    procedure ReturnWorkerJob(Job: TAIJob; Retry: Boolean);
    function CancellationRequested: Boolean;
    function HasPendingWork: Boolean;
    procedure ReapFinishedWorkers;
    function DequeueApplying: TAIJob;
    procedure NotifyStateChange(Job: TAIJob);
    procedure NotifyJobCompleted(Job: TAIJob);
  protected
    function CreateLLMClient(const Provider, Model, ApiKey: string): TBaseLLMClient; virtual;
  public
    constructor Create(const AModel, AApiKey: string; AMaxParallel: Integer = 2);
    procedure ConfigureResolvedConnection(const Provider, Model, ApiKey: string);
    destructor Destroy; override;
    procedure StartSingle(Flow: IAIVerseFlow; Verse: TVerse); // processes one verse async (apply after PumpApply)
    procedure StartBatch(Flow: IAIVerseFlow; WorkItems: TAIBatchWorkItemList);
    procedure PumpApply; // Call periodically in main thread
    procedure CancelAll;
    function RetryFailedJobs(SelectedIndexes: TStrings = nil): Integer; // Re-queue permanently failed jobs
  // Progress / metrics
  function TotalJobs: Integer;
  function CompletedJobs: Integer;
  function FailedJobs: Integer;
  function CancelledJobs: Integer;
  function PendingJobs: Integer;
  function InFlightJobs: Integer;
  function IsIdle: Boolean;
  function IsStopped: Boolean;
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

resourcestring
  SAIProcessingStillStopping = 'The previous AI batch is still stopping. Wait for cancellation to finish.';
  SAIConnectionAlreadyStarted = 'The AI connection cannot change after processing starts.';

// Transfers an owned batch manager to background cleanup during forced teardown.
// An active StartSingle manager still borrows its caller's verse and is refused.
procedure ReleaseAIProcessingBatch(var Manager: TAIProcessingManager);

implementation

uses
  ai_debug;

type
  TAIManagerCleanupThread = class(TThread)
  private
    FManager: TAIProcessingManager;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TAIProcessingManager);
  end;

constructor TAIManagerCleanupThread.Create(Manager: TAIProcessingManager);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FManager := Manager;
  Start;
end;

procedure TAIManagerCleanupThread.Execute;
begin
  FManager.Free;
end;

procedure ReleaseAIProcessingBatch(var Manager: TAIProcessingManager);
var
  OwnedManager: TAIProcessingManager;
begin
  if not Assigned(Manager) then Exit;
  if Manager.FPumping then
    raise Exception.Create(SAIProcessingStillStopping);
  if Manager.FBorrowedVerses and not Manager.IsStopped then
    raise Exception.Create(SAIProcessingStillStopping);
  Manager.OnJobStateChange := nil;
  Manager.OnJobCompleted := nil;
  Manager.OnAutoSaveNeeded := nil;
  Manager.GetVerseReference := nil;
  Manager.CancelAll;
  OwnedManager := Manager;
  Manager := nil;
  if OwnedManager.IsStopped then
    OwnedManager.Free
  else
    TAIManagerCleanupThread.Create(OwnedManager);
end;

{ TAIJob }

procedure TAIJob.ReleaseTempVerses;
begin
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
    Verse := nil;
    VersePair := nil;
  end;
end;

destructor TAIJob.Destroy;
begin
  // Free temporary verse objects created for batch processing
  ReleaseTempVerses;
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
  retryDelay: Int64;
  retry: Boolean;
  effectiveProvider, effectiveModel, effectiveApiKey: string;
begin
  try
    while not Terminated do
    begin
      job := FManager.DequeuePending;
      if job = nil then
      begin
        if FManager.CancellationRequested or not FManager.HasPendingWork then
          Break;
        Sleep(50); // A retry can remain queued until its backoff expires.
        Continue;
      end;

      retry := False;
      try
        try
          if FManager.CancellationRequested then
            Continue;
          Inc(job.AttemptCount);
          job.State := ajsFetching;
          job.RequestStartTime := Now;
          job.Usage := Default(TLLMUsage);

          if FManager.FConnectionResolved then
          begin
            effectiveProvider := FManager.FResolvedProvider;
            effectiveModel := FModel;
            effectiveApiKey := FApiKey;
          end
          else
          begin
            effectiveProvider := job.Flow.ProviderOverride;
            effectiveModel := job.Flow.ModelOverride;
            if effectiveModel = '' then effectiveModel := FModel;
            effectiveApiKey := job.Flow.ApiKeyOverride;
            if effectiveApiKey = '' then effectiveApiKey := FApiKey;
          end;

          if (FClient = nil) or
             ((effectiveProvider <> '') and (FClient.Provider <> effectiveProvider)) or
             (FClient.Model <> effectiveModel) or
             (FClient.ApiKey <> effectiveApiKey) then
          begin
            FreeAndNil(FClient);
            FClient := FManager.CreateLLMClient(effectiveProvider,
              effectiveModel, effectiveApiKey);
          end;
          if FManager.CancellationRequested then
            Continue;
          if Assigned(job.WorkItem) then
            job.WorkItem.CreateTempVersePair(job.Verse, job.VersePair);
          msgs := job.Flow.BuildMessages(job.Verse);
          if FManager.CancellationRequested then
            Continue;
          resp := FClient.AskChatCompletions(msgs);
          job.RawContent := resp.Content;
          job.Usage := resp.Usage;
          if FManager.CancellationRequested then
            Continue;
          job.Parsed := job.Flow.ParseResponse(resp.Content, job.Verse);
          if not Assigned(job.Parsed) then
            raise Exception.Create('Missing parsed AI result');
          if job.Parsed.ParseError <> '' then
            raise Exception.Create('Parse error: ' + job.Parsed.ParseError);
          job.State := ajsCompleted;
        except
          on E: Exception do
          begin
            job.ErrorMsg := E.Message;
            job.State := ajsFailed;
            retry := not FManager.CancellationRequested and
              (job.AttemptCount < FManager.FMaxRetries);
            if retry then
            begin
              FreeAndNil(job.Parsed);
              job.ReleaseTempVerses;
              retryDelay := Int64(FManager.FRetryBaseDelay) *
                (Int64(1) shl (job.AttemptCount - 1));
              job.NextRetryTime := Now + (retryDelay / 86400000);
              job.State := ajsPending;
            end;
          end;
        end;
      finally
        // Every dequeue has exactly one return, even when cancellation wins.
        FManager.ReturnWorkerJob(job, retry);
      end;
    end;
  finally
    FreeAndNil(FClient);
  end;
end;

{ TAIProcessingManager }

procedure TAIProcessingManager.ConfigureResolvedConnection(const Provider, Model, ApiKey: string);
begin
  if (FTotalJobs <> 0) or (FWorkers.Count <> 0) then
    raise Exception.Create(SAIConnectionAlreadyStarted);
  FResolvedProvider := Provider;
  FModel := Model;
  FApiKey := ApiKey;
  FConnectionResolved := True;
end;

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
  if FMaxParallel < 1 then FMaxParallel := 1;
  FTotalJobs := 0;
  FCompleted := 0;
  FFailed := 0;
  FCancelled := 0;
  FInFlight := 0;
  FPumping := False;
  FBorrowedVerses := False;
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

function TAIProcessingManager.CreateLLMClient(const Provider, Model,
  ApiKey: string): TBaseLLMClient;
begin
  Result := llmclient.CreateBackend(Provider, Model, ApiKey);
end;

function TAIProcessingManager.CancellationRequested: Boolean;
begin
  Result := True;
  if not SafeEnterLock then Exit;
  try
    Result := FCancel;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.HasPendingWork: Boolean;
begin
  Result := False;
  if not SafeEnterLock then Exit;
  try
    Result := FPending.Count > 0;
  finally
    SafeLeaveLock;
  end;
end;

procedure TAIProcessingManager.ReapFinishedWorkers;
var
  I: Integer;
  Worker: TThread;
begin
  for I := FWorkers.Count - 1 downto 0 do
  begin
    Worker := TThread(FWorkers[I]);
    if Worker.Finished then
    begin
      FWorkers.Delete(I);
      Worker.Free;
    end;
  end;
end;

procedure TAIProcessingManager.EnqueuePending(Job: TAIJob);
begin
  SafeEnterLock;
  try
    if FCancel then
    begin
      Job.State := ajsCancelled;
      FApplying.Push(Job);
    end
    else
      FPending.Push(Job);
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.DequeuePending: TAIJob;
var
  I, Count: Integer;
  Candidate: TAIJob;
begin
  Result := nil;
  if not SafeEnterLock then Exit;
  try
    if FCancel then Exit;
    Count := FPending.Count;
    for I := 1 to Count do
    begin
      Candidate := TAIJob(FPending.Pop);
      if (Candidate.AttemptCount = 0) or
         (Now >= Candidate.NextRetryTime) then
      begin
        Result := Candidate;
        Inc(FInFlight);
        Exit;
      end;
      FPending.Push(Candidate);
    end;
  finally
    SafeLeaveLock;
  end;
end;

procedure TAIProcessingManager.ReturnWorkerJob(Job: TAIJob; Retry: Boolean);
begin
  SafeEnterLock;
  try
    Dec(FInFlight);
    // Publishing completion and cancellation share the same ordering point.
    if FCancel then
    begin
      Job.State := ajsCancelled;
      Job.ErrorMsg := '';
      FApplying.Push(Job);
    end
    else if Retry then
      FPending.Push(Job)
    else
    begin
      if Job.State = ajsCompleted then
      begin
        FTotalRequestTime := FTotalRequestTime +
          (Now - Job.RequestStartTime) * 86400;
        Inc(FRequestCount);
      end;
      FApplying.Push(Job);
    end;
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

procedure TAIProcessingManager.StartSingle(Flow: IAIVerseFlow; Verse: TVerse);
var
  job: TAIJob;
begin
  if not IsStopped then
    raise Exception.Create(SAIProcessingStillStopping);
  SafeEnterLock;
  try FCancel := False; finally SafeLeaveLock; end;
  job := TAIJob.Create;
  FFlowName := Flow.Name;
  FBorrowedVerses := True;
  job.Flow := Flow;
  job.Verse := Verse;
  // Single jobs use visual verses (WorkItem = nil) so destructor won't free them
  job.State := ajsPending;
  job.AttemptCount := 0;
  job.NextRetryTime := 0;
  job.Usage.PromptTokens := 0;
  job.Usage.CompletionTokens := 0;
  job.Usage.TotalTokens := 0;
  FBatchStartTime := Now;
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
  if not IsStopped then
    raise Exception.Create(SAIProcessingStillStopping);
  SafeEnterLock;
  try FCancel := False; finally SafeLeaveLock; end;
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
  keepForRetry, triggerAutoSave: Boolean;
  reference: string;
begin
  if FPumping then Exit;
  FPumping := True;
  try
    repeat
      job := DequeueApplying;
      if job = nil then Break;
      keepForRetry := False;
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
          try
            if not Assigned(job.Flow) then
              raise Exception.Create('Missing Flow in job when applying result');
            if not Assigned(job.Parsed) then
              raise Exception.Create('Missing Parsed result in job when applying result');
            if job.Parsed.ParseError <> '' then
              raise Exception.Create('Parse error: ' + job.Parsed.ParseError);
            job.Flow.ApplyResult(job.Parsed, job.Verse);
          except
            on E: Exception do
            begin
              job.ErrorMsg := E.Message;
              job.State := ajsFailed;
            end;
          end;
        end;

        SafeEnterLock;
        try
          case job.State of
            ajsCompleted:
              begin
                Inc(FCompleted);
                if (FAutoSaveInterval > 0) and Assigned(FOnAutoSaveNeeded) and
                   (FCompleted - FLastAutoSaveCount >= FAutoSaveInterval) then
                begin
                  FLastAutoSaveCount := FCompleted;
                  triggerAutoSave := True;
                end;
              end;
            ajsFailed:
              begin
                Inc(FFailed);
                FFailedJobs.Add(job);
                keepForRetry := True;
                reference := 'Unknown verse';
                if Assigned(job.Verse) then
                  reference := job.Verse.Reference
                else if Assigned(job.WorkItem) then
                  reference := job.WorkItem.VerseData.ReferenceName;
                FErrors.Add(Format('[%s] Attempt %d/%d: %s',
                  [reference, job.AttemptCount, FMaxRetries, job.ErrorMsg]));
              end;
            ajsCancelled: Inc(FCancelled);
          end;
        finally
          SafeLeaveLock;
        end;

        NotifyStateChange(job);
        if job.State = ajsCompleted then
          NotifyJobCompleted(job);
        if triggerAutoSave then
          FOnAutoSaveNeeded(Self);
      finally
        FreeAndNil(job.Parsed);
        if not keepForRetry then job.Free;
      end;
    until False;
  finally
    ReapFinishedWorkers;
    FPumping := False;
  end;
end;

procedure TAIProcessingManager.CancelAll;
var
  I: Integer;
  Job: TAIJob;
begin
  if not SafeEnterLock then Exit;
  try
    if FCancel then Exit;
    FCancel := True;
    while FPending.Count > 0 do
    begin
      Job := TAIJob(FPending.Pop);
      Job.State := ajsCancelled;
      Job.ErrorMsg := '';
      FApplying.Push(Job);
    end;
  finally
    SafeLeaveLock;
  end;
  for I := 0 to FWorkers.Count - 1 do
    TThread(FWorkers[I]).Terminate;
end;

function TAIProcessingManager.RetryFailedJobs(SelectedIndexes: TStrings): Integer;
var
  i: Integer;
  job: TAIJob;
  JobsToRetry: TList;

  function IndexSelected(JobIndex: Integer): Boolean;
  begin
    Result := (not Assigned(SelectedIndexes)) or
      (SelectedIndexes.Count = 0) or
      (SelectedIndexes.IndexOf(IntToStr(JobIndex)) >= 0);
  end;
begin
  Result := 0;
  if not IsStopped then Exit;
  JobsToRetry := TList.Create;
  try
    if SafeEnterLock then
    begin
      try
        FCancel := False;
        for i := FFailedJobs.Count - 1 downto 0 do
        begin
          job := TAIJob(FFailedJobs[i]);
          if not IndexSelected(job.Index) then
            Continue;

          FFailedJobs.Delete(i);
          if FFailed > 0 then
            Dec(FFailed);
          job.State := ajsPending;
          job.AttemptCount := 0; // Reset retry count
          job.NextRetryTime := 0;
          job.ErrorMsg := '';
          job.RawContent := '';
          job.Usage.PromptTokens := 0;
          job.Usage.CompletionTokens := 0;
          job.Usage.TotalTokens := 0;
          job.ReleaseTempVerses;
          JobsToRetry.Add(job);
        end;
      finally
        SafeLeaveLock;
      end;
    end;

    for i := 0 to JobsToRetry.Count - 1 do
    begin
      job := TAIJob(JobsToRetry[i]);
      EnqueuePending(job);
      NotifyStateChange(job);
      Inc(Result);
    end;

    // Ensure workers are running
    while (Result > 0) and (FWorkers.Count < FMaxParallel) do
      FWorkers.Add(TAIWorkerThread.Create(Self, FModel, FApiKey));
  finally
    JobsToRetry.Free;
  end;
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

function TAIProcessingManager.CancelledJobs: Integer;
begin
  Result := 0;
  if not SafeEnterLock then Exit;
  try
    Result := FCancelled;
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

function TAIProcessingManager.IsStopped: Boolean;
begin
  ReapFinishedWorkers;
  Result := IsIdle and (FWorkers.Count = 0) and not FPumping;
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
  elapsedSeconds: Double;
  effectiveParallelism: Integer;
  remaining, processed: Integer;
begin
  SafeEnterLock;
  try
    remaining := FTotalJobs - FCompleted - FFailed - FCancelled;
    if remaining <= 0 then
    begin
      Result := 0;
      Exit;
    end;

    processed := FCompleted + FFailed;
    if (FBatchStartTime > 0) and (processed > 0) then
    begin
      elapsedSeconds := (Now - FBatchStartTime) * 24 * 60 * 60;
      if elapsedSeconds > 0 then
      begin
        // Wall-clock throughput already includes actual parallelism, API latency,
        // parsing, and main-thread application time. This avoids underestimating
        // ETA when MaxParallel is greater than the number of remaining verses.
        Result := (remaining * elapsedSeconds) / processed;
        Exit;
      end;
    end;

    if FRequestCount = 0 then
    begin
      Result := 0;
      Exit;
    end;

    avgTime := FTotalRequestTime / FRequestCount;
    effectiveParallelism := FMaxParallel;
    if effectiveParallelism < 1 then
      effectiveParallelism := 1;
    if (FTotalJobs > 0) and (effectiveParallelism > FTotalJobs) then
      effectiveParallelism := FTotalJobs;

    Result := (remaining * avgTime) / effectiveParallelism;
    if (remaining > 0) and (Result < avgTime) then
      Result := avgTime;
  finally
    SafeLeaveLock;
  end;
end;

function TAIProcessingManager.GetETAFormatted: string;
var
  etaSeconds: Double;
  hours, minutes, seconds, totalSeconds: Integer;
begin
  etaSeconds := EstimatedTimeRemaining;
  if etaSeconds <= 0 then
  begin
    Result := '--:--:--';
    Exit;
  end;
  totalSeconds := Trunc(etaSeconds);
  if etaSeconds > totalSeconds then
    Inc(totalSeconds);
  if totalSeconds < 1 then
    totalSeconds := 1;

  hours := totalSeconds div 3600;
  minutes := (totalSeconds mod 3600) div 60;
  seconds := totalSeconds mod 60;
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
