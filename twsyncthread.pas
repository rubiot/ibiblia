unit twSyncThread;

{$mode objfpc}{$H+}

interface

uses
  twAutomate, Classes, syncobjs, sysutils;

type
  TRefChangeEvent = procedure(Ref: String) of Object;

  TTwSyncThread = class(TThread)
  private
    fCurrentRef : string;
    fIgnoreUntilTick: QWord;
    FLock: TCriticalSection;
    FOnRefChange: TRefChangeEvent;
    procedure RefChange;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;
    procedure NotifyVerseSent(const ARef: string);
    property OnRefChange: TRefChangeEvent read FOnRefChange write FOnRefChange;
  end;

implementation

constructor TTwSyncThread.Create(CreateSuspended : boolean);
begin
  FLock := TCriticalSection.Create;
  fCurrentRef := '';
  fIgnoreUntilTick := 0;
  FreeOnTerminate := True;
  twAutomate.TWAutomateUtils.Reset;
  inherited Create(CreateSuspended);
end;

destructor TTwSyncThread.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TTwSyncThread.NotifyVerseSent(const ARef: string);
begin
  FLock.Enter;
  try
    fCurrentRef := ARef;
    // Suppress incoming echo/stale reflection from theWord for 1000ms while theWord navigates
    fIgnoreUntilTick := GetTickCount64 + 1000;
  finally
    FLock.Leave;
  end;
end;

procedure TTwSyncThread.RefChange;
// this method is executed by the mainthread and can therefore access all GUI elements.
var
  refToSync: string;
  ignoreUntil: QWord;
begin
  FLock.Enter;
  try
    refToSync := fCurrentRef;
    ignoreUntil := fIgnoreUntilTick;
  finally
    FLock.Leave;
  end;
  if (refToSync = '') or (GetTickCount64 < ignoreUntil) then
    Exit;
  if Assigned(FOnRefChange) then
    FOnRefChange(refToSync);
end;

procedure TTwSyncThread.Execute;
var
  newRef: string;
  curRef: string;
  nowTick: QWord;
  ignoreUntil: QWord;
begin
  while not Terminated do
  begin
    try
      newRef := twAutomate.TWAutomateUtils.GetTwCurrentRef();
    except
      newRef := '';
    end;

    nowTick := GetTickCount64;
    FLock.Enter;
    try
      curRef := fCurrentRef;
      ignoreUntil := fIgnoreUntilTick;
    finally
      FLock.Leave;
    end;

    // Only process external changes when not in the middle of an iBiblia-initiated navigation
    if (newRef <> '') and (nowTick >= ignoreUntil) then
    begin
      if newRef <> curRef then
      begin
        FLock.Enter;
        try
          fCurrentRef := newRef;
        finally
          FLock.Leave;
        end;
        try
          Synchronize(@RefChange);
        except
        end;
      end;
    end
    else if (newRef <> '') and (newRef = curRef) then
    begin
      // theWord has reached the requested verse, clear cooldown
      FLock.Enter;
      try
        fIgnoreUntilTick := 0;
      finally
        FLock.Leave;
      end;
    end;

    if Terminated then Break;

    if newRef = '' then
      Sleep(1000)
    else
      Sleep(200);
  end;
end;

end.
