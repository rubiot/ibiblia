unit ai_processing_manager_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fpcunit, testregistry, ai_flows,
  ai_processing_manager, ai_batch_work_item, llmclient, Verse;

type
  TAIProcessingManagerTests = class(TTestCase)
  private
    FManager: TAIProcessingManager;
    FControl: TObject;
    FFlow: IAIVerseFlow;
    FCancelled, FCompleted, FFailed: Integer;
    FCallbackOnWorker: Boolean;
    FReleaseInCallback, FReleaseRefused: Boolean;
    procedure StateChanged(Sender: TObject; JobIndex: Integer;
      NewState: TAIJobState; ErrorMsg: string);
    procedure JobCompleted(Sender: TObject; JobIndex: Integer; Result: TAIFlowResult);
    procedure StartJobs(Count: Integer);
    procedure WaitStopped;
    procedure WaitCalls(Count: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CancelReturnsWhileRequestIsBlocked;
    procedure CancellationAfterDequeueDoesNotCallClient;
    procedure CompletedBeforeCancelRemainsAvailable;
    procedure CancelDuringRetryBackoffDoesNotRetry;
    procedure FailureReturningAfterCancelIsNotRetried;
    procedure LateResponseIsNotParsedOrApplied;
    procedure RetryAndNewBatchWorkAfterWorkersStop;
    procedure PendingCountsDoNotDoubleCountTerminalQueue;
    procedure CancelledFormKeepsTimerAndBlocksCloseUntilStopped;
    procedure ForcedBatchReleaseWaitsOutsideUIAndDetachesCallbacks;
    procedure ActiveBorrowedVerseCannotBeReleasedInBackground;
    procedure ReentrantReleaseIsRefusedUntilPumpReturns;
  end;

implementation

uses
  Forms, Graphics, LResources, formaiprocessing;

type
  TTestControl = class
  public
    RequestEntered, ReleaseRequest, FactoryEntered, ReleaseFactory, Disposed: TEvent;
    Calls, Parses, Applies: LongInt;
    BlockRequest: Integer;
    BlockFactory, FailFirst: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TFakeClient = class(TBaseLLMClient)
  private
    FControl: TTestControl;
  protected
    function HttpPostJson(const Url, JsonBody: string;
      const ExtraHeaders: array of string): string; override;
  public
    constructor Create(Control: TTestControl);
    function AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse; override;
  end;

  TTestManager = class(TAIProcessingManager)
  private
    FControl: TTestControl;
  protected
    function CreateLLMClient(const Provider, Model, ApiKey: string): TBaseLLMClient; override;
  public
    constructor Create(Control: TTestControl);
    destructor Destroy; override;
  end;

  TFakeFlow = class(TInterfacedObject, IAIVerseFlow)
  private
    FControl: TTestControl;
  public
    constructor Create(Control: TTestControl);
    function Name: string;
    procedure PrepareRequest(AVerse: TVerse; out Messages: array of TLLMMessage);
    function BuildMessages(AVerse: TVerse): TLLMMessageArray;
    function ParseResponse(const RawContent: string; AVerse: TVerse): TAIFlowResult;
    procedure ApplyResult(ResultObj: TAIFlowResult; AVerse: TVerse);
    function RequiresIndividualClient: Boolean;
    function ModelOverride: string;
    function ApiKeyOverride: string;
    function ProviderOverride: string;
  end;

function AtomicValue(var Value: LongInt): LongInt;
begin
  Result := InterlockedCompareExchange(Value, 0, 0);
end;

function NewItems(Count: Integer): TAIBatchWorkItemList;
var
  I: Integer;
  Data: TVerseData;
begin
  Result := TAIBatchWorkItemList.Create;
  for I := 0 to Count - 1 do
  begin
    Data := Default(TVerseData);
    Data.ReferenceID := '1,1,' + IntToStr(I + 1);
    Data.ReferenceName := 'Genesis 1:' + IntToStr(I + 1);
    Data.OriginalText := 'source';
    Data.TranslatedText := 'destination';
    Result.AddItem(TAIBatchWorkItem.Create(1, Data));
  end;
end;

constructor TTestControl.Create;
begin
  inherited Create;
  RequestEntered := TEvent.Create(nil, True, False, '');
  ReleaseRequest := TEvent.Create(nil, True, False, '');
  FactoryEntered := TEvent.Create(nil, True, False, '');
  ReleaseFactory := TEvent.Create(nil, True, False, '');
  Disposed := TEvent.Create(nil, True, False, '');
end;

destructor TTestControl.Destroy;
begin
  RequestEntered.Free;
  ReleaseRequest.Free;
  FactoryEntered.Free;
  ReleaseFactory.Free;
  Disposed.Free;
  inherited Destroy;
end;

constructor TFakeClient.Create(Control: TTestControl);
begin
  inherited Create('model', 'test-key');
  FControl := Control;
end;

function TFakeClient.AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse;
var
  Call: Integer;
begin
  Call := InterlockedIncrement(FControl.Calls);
  if Call = FControl.BlockRequest then
  begin
    FControl.RequestEntered.SetEvent;
    if FControl.ReleaseRequest.WaitFor(5000) <> wrSignaled then
      raise Exception.Create('Test request was not released');
  end;
  if FControl.FailFirst and (Call = 1) then
    raise Exception.Create('Simulated request failure');
  Result := Default(TLLMResponse);
  Result.Content := 'proposal';
end;

function TFakeClient.HttpPostJson(const Url, JsonBody: string;
  const ExtraHeaders: array of string): string;
begin
  Result := '';
  raise Exception.Create('Network access is not allowed in this test client');
end;

constructor TTestManager.Create(Control: TTestControl);
begin
  inherited Create('model', 'test-key', 1);
  FControl := Control;
end;

destructor TTestManager.Destroy;
var
  Control: TTestControl;
begin
  Control := FControl;
  inherited Destroy;
  Control.Disposed.SetEvent;
end;

function TTestManager.CreateLLMClient(const Provider, Model, ApiKey: string): TBaseLLMClient;
begin
  if FControl.BlockFactory then
  begin
    FControl.FactoryEntered.SetEvent;
    if FControl.ReleaseFactory.WaitFor(5000) <> wrSignaled then
      raise Exception.Create('Test client factory was not released');
  end;
  Result := TFakeClient.Create(FControl);
end;

constructor TFakeFlow.Create(Control: TTestControl);
begin
  inherited Create;
  FControl := Control;
end;

function TFakeFlow.Name: string;
begin Result := 'test'; end;

procedure TFakeFlow.PrepareRequest(AVerse: TVerse; out Messages: array of TLLMMessage);
begin end;

function TFakeFlow.BuildMessages(AVerse: TVerse): TLLMMessageArray;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0].Role := 'user';
  Result[0].Content := 'offline test';
end;

function TFakeFlow.ParseResponse(const RawContent: string; AVerse: TVerse): TAIFlowResult;
begin
  InterlockedIncrement(FControl.Parses);
  Result := TAIFlowResult.Create;
end;

procedure TFakeFlow.ApplyResult(ResultObj: TAIFlowResult; AVerse: TVerse);
begin
  InterlockedIncrement(FControl.Applies);
  ResultObj.SetAcceptedDestinationText('proposal');
end;

function TFakeFlow.RequiresIndividualClient: Boolean;
begin Result := False; end;
function TFakeFlow.ModelOverride: string;
begin Result := 'model'; end;
function TFakeFlow.ApiKeyOverride: string;
begin Result := 'test-key'; end;
function TFakeFlow.ProviderOverride: string;
begin Result := ''; end;

procedure TAIProcessingManagerTests.SetUp;
begin
  FControl := TTestControl.Create;
  FManager := TTestManager.Create(TTestControl(FControl));
  FManager.OnJobStateChange := @StateChanged;
  FManager.OnJobCompleted := @JobCompleted;
  FFlow := TFakeFlow.Create(TTestControl(FControl));
  FCancelled := 0;
  FCompleted := 0;
  FFailed := 0;
  FCallbackOnWorker := False;
  FReleaseInCallback := False;
  FReleaseRefused := False;
end;

procedure TAIProcessingManagerTests.TearDown;
begin
  TTestControl(FControl).ReleaseRequest.SetEvent;
  TTestControl(FControl).ReleaseFactory.SetEvent;
  if Assigned(FManager) then
  begin
    FManager.CancelAll;
    FManager.Free;
  end
  else
    TTestControl(FControl).Disposed.WaitFor(10000);
  FFlow := nil;
  FControl.Free;
end;

procedure TAIProcessingManagerTests.StateChanged(Sender: TObject; JobIndex: Integer;
  NewState: TAIJobState; ErrorMsg: string);
begin
  FCallbackOnWorker := FCallbackOnWorker or (GetCurrentThreadID <> MainThreadID);
  if FReleaseInCallback and (NewState = ajsCompleted) then
  begin
    try
      ReleaseAIProcessingBatch(FManager);
    except
      on E: Exception do FReleaseRefused := True;
    end;
  end;
  case NewState of
    ajsCompleted: Inc(FCompleted);
    ajsFailed: Inc(FFailed);
    ajsCancelled: Inc(FCancelled);
  end;
end;

procedure TAIProcessingManagerTests.JobCompleted(Sender: TObject; JobIndex: Integer;
  Result: TAIFlowResult);
begin
  FCallbackOnWorker := FCallbackOnWorker or (GetCurrentThreadID <> MainThreadID);
  AssertTrue('Completed result stays available to review', Result.AcceptedDestinationTextSet);
end;

procedure TAIProcessingManagerTests.StartJobs(Count: Integer);
var
  Items: TAIBatchWorkItemList;
begin
  Items := NewItems(Count);
  try
    FManager.StartBatch(FFlow, Items);
  finally
    Items.Free;
  end;
end;

procedure TAIProcessingManagerTests.WaitStopped;
var
  Deadline: QWord;
begin
  Deadline := GetTickCount64 + 5000;
  repeat
    FManager.PumpApply;
    if FManager.IsStopped then Exit;
    Sleep(5);
  until GetTickCount64 >= Deadline;
  Fail('Manager did not stop and drain its queues');
end;

procedure TAIProcessingManagerTests.WaitCalls(Count: Integer);
var
  Deadline: QWord;
begin
  Deadline := GetTickCount64 + 5000;
  while AtomicValue(TTestControl(FControl).Calls) < Count do
  begin
    if GetTickCount64 >= Deadline then Fail('Request did not start');
    Sleep(5);
  end;
end;

procedure TAIProcessingManagerTests.CancelReturnsWhileRequestIsBlocked;
var
  Control: TTestControl;
  Started: QWord;
begin
  Control := TTestControl(FControl);
  Control.BlockRequest := 1;
  StartJobs(3);
  AssertEquals(Ord(wrSignaled), Ord(Control.RequestEntered.WaitFor(5000)));
  Started := GetTickCount64;
  FManager.CancelAll;
  FManager.CancelAll;
  AssertTrue('Cancellation must not wait for the blocked request', GetTickCount64 - Started < 1000);
  AssertFalse(FManager.IsStopped);
  FManager.PumpApply;
  AssertEquals(2, FCancelled);
  AssertEquals(1, FManager.InFlightJobs);
  Control.ReleaseRequest.SetEvent;
  WaitStopped;
  AssertEquals(3, FManager.CancelledJobs);
  AssertEquals(0, FManager.PendingJobs);
  AssertEquals(0, FManager.InFlightJobs);
  AssertEquals(1, AtomicValue(Control.Calls));
  AssertFalse(FCallbackOnWorker);
end;

procedure TAIProcessingManagerTests.CancellationAfterDequeueDoesNotCallClient;
var
  Control: TTestControl;
begin
  Control := TTestControl(FControl);
  Control.BlockFactory := True;
  StartJobs(1);
  AssertEquals(Ord(wrSignaled), Ord(Control.FactoryEntered.WaitFor(5000)));
  FManager.CancelAll;
  Control.ReleaseFactory.SetEvent;
  WaitStopped;
  AssertEquals(0, AtomicValue(Control.Calls));
  AssertEquals(1, FCancelled);
  AssertEquals(0, FManager.InFlightJobs);
end;

procedure TAIProcessingManagerTests.CompletedBeforeCancelRemainsAvailable;
var
  Control: TTestControl;
begin
  Control := TTestControl(FControl);
  Control.BlockRequest := 2;
  StartJobs(3);
  AssertEquals(Ord(wrSignaled), Ord(Control.RequestEntered.WaitFor(5000)));
  FManager.CancelAll;
  FManager.PumpApply;
  AssertEquals(1, FCompleted);
  AssertEquals(1, AtomicValue(Control.Applies));
  Control.ReleaseRequest.SetEvent;
  WaitStopped;
  AssertEquals(1, FManager.CompletedJobs);
  AssertEquals(2, FManager.CancelledJobs);
  AssertEquals(1, AtomicValue(Control.Parses));
  AssertFalse(FCallbackOnWorker);
end;

procedure TAIProcessingManagerTests.CancelDuringRetryBackoffDoesNotRetry;
var
  Control: TTestControl;
  Deadline: QWord;
begin
  Control := TTestControl(FControl);
  Control.FailFirst := True;
  FManager.RetryBaseDelay := 10000;
  StartJobs(1);
  WaitCalls(1);
  Deadline := GetTickCount64 + 5000;
  while FManager.InFlightJobs <> 0 do
  begin
    if GetTickCount64 >= Deadline then Fail('Failed request did not return to retry queue');
    Sleep(5);
  end;
  AssertEquals(1, FManager.PendingJobs);
  FManager.CancelAll;
  WaitStopped;
  AssertEquals(1, AtomicValue(Control.Calls));
  AssertEquals(1, FCancelled);
  AssertEquals(0, FManager.RetryFailedJobs);
end;

procedure TAIProcessingManagerTests.FailureReturningAfterCancelIsNotRetried;
var
  Control: TTestControl;
begin
  Control := TTestControl(FControl);
  Control.FailFirst := True;
  Control.BlockRequest := 1;
  StartJobs(1);
  AssertEquals(Ord(wrSignaled), Ord(Control.RequestEntered.WaitFor(5000)));
  FManager.CancelAll;
  Control.ReleaseRequest.SetEvent;
  WaitStopped;
  AssertEquals(1, AtomicValue(Control.Calls));
  AssertEquals(0, FManager.FailedJobs);
  AssertEquals(1, FManager.CancelledJobs);
  AssertEquals(0, FManager.RetryFailedJobs);
end;

procedure TAIProcessingManagerTests.LateResponseIsNotParsedOrApplied;
var
  Control: TTestControl;
begin
  Control := TTestControl(FControl);
  Control.BlockRequest := 1;
  StartJobs(1);
  AssertEquals(Ord(wrSignaled), Ord(Control.RequestEntered.WaitFor(5000)));
  FManager.CancelAll;
  Control.ReleaseRequest.SetEvent;
  WaitStopped;
  AssertEquals(0, AtomicValue(Control.Parses));
  AssertEquals(0, AtomicValue(Control.Applies));
  AssertEquals(0, FCompleted);
end;

procedure TAIProcessingManagerTests.RetryAndNewBatchWorkAfterWorkersStop;
var
  Control: TTestControl;
begin
  Control := TTestControl(FControl);
  Control.FailFirst := True;
  FManager.MaxRetries := 1;
  StartJobs(1);
  WaitStopped;
  AssertEquals(1, FManager.FailedJobs);
  AssertEquals(1, FManager.RetryFailedJobs);
  WaitStopped;
  AssertEquals(0, FManager.FailedJobs);
  AssertEquals(1, FManager.CompletedJobs);
  FManager.CancelAll;
  WaitStopped;
  StartJobs(1);
  WaitStopped;
  AssertEquals(2, FManager.CompletedJobs);
  AssertEquals(3, AtomicValue(Control.Calls));
end;

procedure TAIProcessingManagerTests.PendingCountsDoNotDoubleCountTerminalQueue;
var
  Deadline: QWord;
begin
  StartJobs(1);
  WaitCalls(1);
  Deadline := GetTickCount64 + 5000;
  while FManager.InFlightJobs <> 0 do
  begin
    if GetTickCount64 >= Deadline then Fail('Request did not reach terminal queue');
    Sleep(5);
  end;
  AssertEquals(1, FManager.PendingJobs);
  AssertFalse(FManager.IsIdle);
  WaitStopped;
  AssertEquals(0, FManager.PendingJobs);
end;

procedure TAIProcessingManagerTests.CancelledFormKeepsTimerAndBlocksCloseUntilStopped;
var
  Control: TTestControl;
  Form: TFrmAIProcessing;
  Items: TAIBatchWorkItemList;
  Options: TAIProcessingPreviewOptions;
  Font: TFont;
  CanClose: Boolean;
  Deadline: QWord;
begin
  Control := TTestControl(FControl);
  Control.BlockRequest := 1;
  Forms.Application.Initialize;
  Form := TFrmAIProcessing.Create(nil);
  Items := NewItems(2);
  Font := TFont.Create;
  try
    Options := Default(TAIProcessingPreviewOptions);
    Options.TokenizerVersion := 1;
    Options.SourceFont := Font;
    Options.DestinationFont := Font;
    AssertTrue(Form.StartProcessing(FManager, FFlow, Items, Options, nil));
    FManager.StartBatch(FFlow, Items);
    AssertEquals(Ord(wrSignaled), Ord(Control.RequestEntered.WaitFor(5000)));
    Form.RequestCancellation;
    CanClose := True;
    Form.FormCloseQuery(Form, CanClose);
    AssertFalse('X/Esc cannot release a live manager', CanClose);
    AssertTrue(Form.TimerUpdate.Enabled);
    AssertFalse(Form.BtnApply.Enabled);
    Control.ReleaseRequest.SetEvent;
    Deadline := GetTickCount64 + 5000;
    repeat
      Form.TimerUpdateTimer(Form);
      if not Form.TimerUpdate.Enabled then Break;
      if GetTickCount64 >= Deadline then Fail('Cancelled form did not reach its terminal state');
      Sleep(5);
    until False;
    AssertTrue(FManager.IsStopped);
    AssertTrue(Form.BtnClose.Enabled);
    AssertFalse(Form.BtnApply.Enabled);
    AssertEquals('cancelled', Form.Batch.ProcessingState[0]);
    AssertEquals('cancelled', Form.Batch.ProcessingState[1]);
  finally
    Control.ReleaseRequest.SetEvent;
    Form.DetachManager(FManager);
    Form.Free;
    Font.Free;
    Items.Free;
  end;
end;

procedure TAIProcessingManagerTests.ForcedBatchReleaseWaitsOutsideUIAndDetachesCallbacks;
var
  Control: TTestControl;
  Started: QWord;
begin
  Control := TTestControl(FControl);
  Control.BlockRequest := 1;
  StartJobs(2);
  AssertEquals(Ord(wrSignaled), Ord(Control.RequestEntered.WaitFor(5000)));
  Started := GetTickCount64;
  ReleaseAIProcessingBatch(FManager);
  AssertTrue('Forced teardown must transfer ownership immediately', FManager = nil);
  AssertTrue(GetTickCount64 - Started < 1000);
  AssertEquals(Ord(wrTimeout), Ord(Control.Disposed.WaitFor(0)));
  Control.ReleaseRequest.SetEvent;
  AssertEquals(Ord(wrSignaled), Ord(Control.Disposed.WaitFor(5000)));
  AssertEquals(0, FCompleted);
  AssertEquals(0, FCancelled);
  AssertEquals(0, AtomicValue(Control.Applies));
end;

procedure TAIProcessingManagerTests.ActiveBorrowedVerseCannotBeReleasedInBackground;
var
  Control: TTestControl;
  SourceVerse: TVerse;
  Refused: Boolean;
begin
  Control := TTestControl(FControl);
  Control.BlockRequest := 1;
  SourceVerse := TVerse.Create;
  try
    SourceVerse.Text := 'source';
    FManager.StartSingle(FFlow, SourceVerse);
    AssertEquals(Ord(wrSignaled), Ord(Control.RequestEntered.WaitFor(5000)));
    Refused := False;
    try
      ReleaseAIProcessingBatch(FManager);
    except
      on E: Exception do Refused := True;
    end;
    AssertTrue('A caller-owned verse must stay alive until its worker stops', Refused);
    AssertTrue(Assigned(FManager));
    FManager.CancelAll;
    Control.ReleaseRequest.SetEvent;
    WaitStopped;
  finally
    Control.ReleaseRequest.SetEvent;
    WaitStopped;
    SourceVerse.Free;
  end;
end;

procedure TAIProcessingManagerTests.ReentrantReleaseIsRefusedUntilPumpReturns;
begin
  FReleaseInCallback := True;
  StartJobs(1);
  WaitStopped;
  AssertTrue(FReleaseRefused);
  AssertTrue(Assigned(FManager));
  AssertEquals(1, FManager.CompletedJobs);
  ReleaseAIProcessingBatch(FManager);
  AssertTrue(FManager = nil);
end;

initialization
  {$IFDEF FPCUNIT}
  {$I ../bibletreeview.lrs}
  {$ENDIF}
  RegisterTest(TAIProcessingManagerTests);

end.
