unit twSyncThread;

{$mode objfpc}{$H+}

interface

uses
  twAutomate, Classes, SyncObjs;

Type
  TRefChangeEvent = procedure(Ref: String) of Object;

  TTwSyncThread = class(TThread)
  private
    fCurrentRef : string;
    FOnRefChange: TRefChangeEvent;
    FWakeEvent: TSimpleEvent;
    procedure RefChange;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;
    procedure Stop;
    property OnRefChange: TRefChangeEvent read FOnRefChange write FOnRefChange;
  end;

implementation

constructor TTwSyncThread.Create(CreateSuspended : boolean);
begin
  FWakeEvent := TSimpleEvent.Create;
  try
    twAutomate.TWAutomateUtils.Reset;
    inherited Create(CreateSuspended);
  except
    FWakeEvent.Free;
    FWakeEvent := nil;
    raise;
  end;
  FreeOnTerminate := False;  // Controle manual da destruição
end;

destructor TTwSyncThread.Destroy;
begin
  Stop;
  inherited Destroy;
  FWakeEvent.Free;
end;

procedure TTwSyncThread.Stop;
begin
  Terminate;
  if Assigned(FWakeEvent) then
    FWakeEvent.SetEvent;
end;

procedure TTwSyncThread.RefChange;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnRefChange) then
    FOnRefChange(fCurrentRef);
end;

procedure TTwSyncThread.Execute;
var
  newRef: string;
begin
  while not Terminated do
  begin
    newRef := twAutomate.TWAutomateUtils.GetTwCurrentRef();
    if (newRef <> fCurrentRef) and (newRef <> '') then
    begin
      fCurrentRef := newRef;
      Synchronize(@RefChange);
    end;
    if newRef = '' then
      FWakeEvent.WaitFor(5000) // try again in 5s in case of error
    else
      FWakeEvent.WaitFor(500);
  end;
end;

end.

