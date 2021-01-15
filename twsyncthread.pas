unit twSyncThread;

{$mode objfpc}{$H+}

interface

uses
  twAutomate, Classes;

Type
  TRefChangeEvent = procedure(Ref: String) of Object;

  TTwSyncThread = class(TThread)
  private
    fCurrentRef : string;
    FOnRefChange: TRefChangeEvent;
    procedure RefChange;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    property OnRefChange: TRefChangeEvent read FOnRefChange write FOnRefChange;
  end;

implementation

constructor TTwSyncThread.Create(CreateSuspended : boolean);
begin
  FreeOnTerminate := True;
  twAutomate.TWAutomateUtils.Reset;
  inherited Create(CreateSuspended);
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
      Sleep(5000) // try again in 5s in case of error
    else
      Sleep(500);
  end;
end;

end.

