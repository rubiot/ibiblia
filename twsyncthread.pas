unit twSyncThread;

{$mode objfpc}{$H+}

interface

uses
  twAutomate, tanglaw_sync, Classes, SysUtils, SyncObjs;

Type
  TRefChangeEvent = procedure(Ref: String) of Object;

  TTwSyncThread = class(TThread)
  private
    fCurrentRef : string;
    fIgnoreUntilTick: QWord;
    FLock: TCriticalSection;
    FPendingRef: string;
    FOnRefChange: TRefChangeEvent;
    FWakeEvent: TSimpleEvent;
    FPollTheWord: Boolean;
    FPollTanglaw: Boolean;
    fTanglawRef: string;
    fTanglawIgnoreUntilTick: QWord;
    FPendingTanglawRef: string;
    FOnTanglawRefChange: TRefChangeEvent;
    procedure RefChange;
    procedure TanglawRefChange;
    function PollTheWordOnce: Boolean;
    function PollTanglawOnce: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;
    procedure Stop;
    procedure NotifyVerseSent(const ARef: string);
    procedure NotifyTanglawVerseSent(const ARef: string);
    { which programs iBiblia follows; set before Start }
    property PollTheWord: Boolean read FPollTheWord write FPollTheWord;
    property PollTanglaw: Boolean read FPollTanglaw write FPollTanglaw;
    property OnRefChange: TRefChangeEvent read FOnRefChange write FOnRefChange;
    property OnTanglawRefChange: TRefChangeEvent read FOnTanglawRefChange
      write FOnTanglawRefChange;
  end;

implementation

constructor TTwSyncThread.Create(CreateSuspended : boolean);
begin
  FWakeEvent := TSimpleEvent.Create;
  FLock := TCriticalSection.Create;
  fIgnoreUntilTick := 0;
  fTanglawIgnoreUntilTick := 0;
  FPollTheWord := True;
  FPollTanglaw := False;
  try
    twAutomate.TWAutomateUtils.Reset;
    inherited Create(CreateSuspended);
  except
    FWakeEvent.Free;
    FWakeEvent := nil;
    FreeAndNil(FLock);
    raise;
  end;
  FreeOnTerminate := False;  // Controle manual da destruição
end;

destructor TTwSyncThread.Destroy;
begin
  Stop;
  inherited Destroy;
  FWakeEvent.Free;
  FLock.Free;
end;

procedure TTwSyncThread.Stop;
begin
  Terminate;
  if Assigned(FWakeEvent) then
    FWakeEvent.SetEvent;
end;

{ Called by the main thread right after it sends a verse to theWord: the sent
  reference becomes the current one and polling results are ignored for one
  second, so theWord's navigation is not echoed back to iBiblia. }
procedure TTwSyncThread.NotifyVerseSent(const ARef: string);
begin
  FLock.Enter;
  try
    fCurrentRef := ARef;
    fIgnoreUntilTick := GetTickCount64 + 1000;
  finally
    FLock.Leave;
  end;
end;

procedure TTwSyncThread.NotifyTanglawVerseSent(const ARef: string);
begin
  FLock.Enter;
  try
    fTanglawRef := ARef;
    fTanglawIgnoreUntilTick := GetTickCount64 + 1000;
  finally
    FLock.Leave;
  end;
end;

procedure TTwSyncThread.TanglawRefChange;
begin
  if Assigned(FOnTanglawRefChange) then
    FOnTanglawRefChange(FPendingTanglawRef);
end;

procedure TTwSyncThread.RefChange;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnRefChange) then
    FOnRefChange(FPendingRef);
end;

function TTwSyncThread.PollTheWordOnce: Boolean;
var
  newRef: string;
  changed: Boolean;
begin
  newRef := twAutomate.TWAutomateUtils.GetTwCurrentRef();
  Result := (newRef <> '') or (twAutomate.TWAutomateUtils.IsTwRunning <> 0);
  changed := False;
  FLock.Enter;
  try
    if (newRef <> '') and (newRef <> fCurrentRef) and (GetTickCount64 >= fIgnoreUntilTick) then
    begin
      fCurrentRef := newRef;
      changed := True;
    end;
  finally
    FLock.Leave;
  end;
  if changed and not Terminated then
  begin
    FPendingRef := newRef;
    Synchronize(@RefChange);
  end;
end;

function TTwSyncThread.PollTanglawOnce: Boolean;
var
  newRef: string;
  changed: Boolean;
begin
  try
    newRef := tanglaw_sync.GetTanglawCurrentRef;
  except
    newRef := '';
  end;
  Result := (newRef <> '') or (tanglaw_sync.IsTanglawRunning <> 0);
  changed := False;
  FLock.Enter;
  try
    if (newRef <> '') and (newRef <> fTanglawRef) and
       (GetTickCount64 >= fTanglawIgnoreUntilTick) then
    begin
      fTanglawRef := newRef;
      changed := True;
    end;
  finally
    FLock.Leave;
  end;
  if changed and not Terminated then
  begin
    FPendingTanglawRef := newRef;
    Synchronize(@TanglawRefChange);
  end;
end;

procedure TTwSyncThread.Execute;
var
  anyActive: Boolean;
begin
  while not Terminated do
  begin
    anyActive := False;
    if FPollTheWord then
      anyActive := PollTheWordOnce;
    if Terminated then
      Break;
    if FPollTanglaw then
      anyActive := PollTanglawOnce or anyActive;
    if Terminated then
      Break;
    if anyActive then
      FWakeEvent.WaitFor(250)
    else
      FWakeEvent.WaitFor(1000);
  end;
end;

end.
