unit UnitPleaseWait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TFormPleaseWait }

  TFormPleaseWait = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
  private
    FDetailLabel: TLabel;
    FOwnerForm: TCustomForm;
    FOwnerWasActive: Boolean;
    FOwnerWasEnabled: Boolean;
    FOldCursor: TCursor;
    FInOperation: Boolean;
    procedure EnsureDetailLabel;
    procedure LayoutControls;
  public
    class procedure PumpProgressPaintMessages(AControl: TWinControl); static;
    constructor Create(TheOwner: TComponent); override;
    procedure BeginOperation(AOwnerForm: TCustomForm; const ATitle,
      AMessage: string; AMax: Integer = 0; const ADetail: string = '');
    procedure SetMessage(const AMessage: string);
    procedure SetDetail(const ADetail: string);
    procedure Step(AIncrement: Integer = 1);
    procedure FinishOperation;
    property OperationActive: Boolean read FInOperation;
  end;

var
  FormPleaseWait: TFormPleaseWait;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

resourcestring
  SDefaultOperationTitle = 'Operation in progress';
  SOperationAlreadyActive =
    'Cannot begin an operation while another operation is already active.';

class procedure TFormPleaseWait.PumpProgressPaintMessages(
  AControl: TWinControl);
{$IFDEF MSWINDOWS}
var
  Message: TMsg;
  NativeHandle: HWND;
  ProcessedMessageCount: Integer;
const
  MAX_MESSAGES_PER_PUMP = 64;
begin
  if not Assigned(AControl) or not AControl.HandleAllocated then
    Exit;

  NativeHandle := HWND(AControl.Handle);
  Windows.RedrawWindow(NativeHandle, nil, 0,
    RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);

  // Dispatch only messages addressed to the progress control (and its
  // children). Application.ProcessMessages would also run queued navigation
  // and other project actions while the current operation is incomplete.
  Message := Default(TMsg);
  ProcessedMessageCount := 0;
  while (ProcessedMessageCount < MAX_MESSAGES_PER_PUMP) and
    Windows.PeekMessage(Message, NativeHandle, 0, 0, PM_REMOVE) do
  begin
    Inc(ProcessedMessageCount);
    if Message.message = WM_QUIT then
    begin
      Windows.PostQuitMessage(Message.wParam);
      Break;
    end;
    if (Message.message = WM_CLOSE) or
      ((Message.message = WM_SYSCOMMAND) and
       ((Message.wParam and $FFF0) = SC_CLOSE)) then
      Continue;

    Windows.TranslateMessage(Message);
    Windows.DispatchMessage(Message);
  end;

  Windows.RedrawWindow(NativeHandle, nil, 0,
    RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN or RDW_FRAME);
end;
{$ELSE}
begin
  if Assigned(AControl) and AControl.HandleAllocated then
    AControl.Update;
end;
{$ENDIF}

constructor TFormPleaseWait.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EnsureDetailLabel;
  LayoutControls;
end;

procedure TFormPleaseWait.EnsureDetailLabel;
begin
  if Assigned(FDetailLabel) then
    Exit;

  FDetailLabel := TLabel.Create(Self);
  with FDetailLabel do
  begin
    Parent := Self;
    AutoSize := False;
    Alignment := taCenter;
    ParentColor := False;
    Visible := False;
  end;
end;

procedure TFormPleaseWait.LayoutControls;
begin
  ClientWidth := 448;
  ClientHeight := 132;

  Label1.Left := 24;
  Label1.Top := 16;
  Label1.Width := ClientWidth - 48;
  Label1.Height := 24;
  Label1.AutoSize := False;
  Label1.Alignment := taCenter;

  EnsureDetailLabel;
  FDetailLabel.Left := 24;
  FDetailLabel.Top := 46;
  FDetailLabel.Width := ClientWidth - 48;
  FDetailLabel.Height := 18;

  ProgressBar1.Left := 32;
  ProgressBar1.Top := 78;
  ProgressBar1.Width := ClientWidth - 64;
  ProgressBar1.Height := 28;
end;

procedure TFormPleaseWait.BeginOperation(AOwnerForm: TCustomForm;
  const ATitle, AMessage: string; AMax: Integer; const ADetail: string);
begin
  if FInOperation then
    raise Exception.Create(SOperationAlreadyActive);

  FOwnerForm := AOwnerForm;
  FOwnerWasActive := Assigned(FOwnerForm) and
    (Screen.ActiveCustomForm = FOwnerForm);
  FOwnerWasEnabled := Assigned(FOwnerForm) and FOwnerForm.Enabled;
  FOldCursor := Screen.Cursor;
  FInOperation := True;

  if Trim(ATitle) = '' then
    Caption := SDefaultOperationTitle
  else
    Caption := ATitle;

  LayoutControls;
  SetMessage(AMessage);
  SetDetail(ADetail);

  ProgressBar1.Position := 0;
  ProgressBar1.Min := 0;
  if AMax > 0 then
  begin
    ProgressBar1.Style := pbstNormal;
    ProgressBar1.Max := AMax;
    ProgressBar1.Step := 1;
  end
  else
  begin
    ProgressBar1.Style := pbstMarquee;
    ProgressBar1.Max := 100;
    ProgressBar1.Step := 1;
  end;
  ProgressBar1.Visible := True;

  Screen.Cursor := crHourGlass;
  if Assigned(FOwnerForm) then
    FOwnerForm.Enabled := False;

  Show;
  BringToFront;
  PumpProgressPaintMessages(Self);
end;

procedure TFormPleaseWait.SetMessage(const AMessage: string);
begin
  Label1.Caption := AMessage;
  Label1.Update;
end;

procedure TFormPleaseWait.SetDetail(const ADetail: string);
begin
  EnsureDetailLabel;
  FDetailLabel.Caption := ADetail;
  FDetailLabel.Visible := Trim(ADetail) <> '';
  FDetailLabel.Update;
end;

procedure TFormPleaseWait.Step(AIncrement: Integer);
begin
  if AIncrement <= 0 then
    AIncrement := 1;

  if ProgressBar1.Style = pbstMarquee then
    Exit;

  if ProgressBar1.Position + AIncrement > ProgressBar1.Max then
    ProgressBar1.Position := ProgressBar1.Max
  else
    ProgressBar1.Position := ProgressBar1.Position + AIncrement;

  ProgressBar1.Update;
end;

procedure TFormPleaseWait.FinishOperation;
var
  OwnerForm: TCustomForm;
  RestoreOwnerFocus: Boolean;
begin
  OwnerForm := FOwnerForm;
  RestoreOwnerFocus := FInOperation and FOwnerWasActive and
    FOwnerWasEnabled and Assigned(OwnerForm) and Application.Active;

  if FInOperation then
  begin
    // Enable the owner before hiding this form. Otherwise Windows may choose
    // another application's window because no enabled iBiblia form is left to
    // activate when the progress form disappears.
    if Assigned(OwnerForm) then
      OwnerForm.Enabled := FOwnerWasEnabled;
    Screen.Cursor := FOldCursor;
  end;

  Hide;

  if RestoreOwnerFocus and OwnerForm.Visible then
  begin
    OwnerForm.BringToFront;
    if OwnerForm.CanSetFocus then
      OwnerForm.SetFocus;
    OwnerForm.Update;
  end;

  FOwnerForm := nil;
  FOwnerWasActive := False;
  FOwnerWasEnabled := False;
  FInOperation := False;
  if Assigned(OwnerForm) then
    PumpProgressPaintMessages(OwnerForm);
end;

initialization
{$IFNDEF FPCUNIT}
  {$I unitpleasewait.lrs}
{$ENDIF}

end.

