unit translator_note_hover_controller;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, ExtCtrls, Controls,
  theword_translator_note_editor;

const
  TRANSLATOR_NOTE_HOVER_DELAY_MS = 400;
  TRANSLATOR_NOTE_HOVER_RECHECK_MS = 100;
  TRANSLATOR_NOTE_HOVER_MARGIN_PX = 8;

type
  TResolveTranslatorNotePreviewEvent = function(Sender: TObject;
    ARevision, ANoteId: QWord; out ABodyRaw: UnicodeString;
    out AAnchorScreenRect: TRect): Boolean of object;

  { Shared lifecycle for translator-note previews. The host remains the source
    of truth for both the note body and marker geometry, so stale revisions or
    identifiers are rejected before a preview is shown or retained. }
  TTranslatorNoteHoverController = class(TComponent)
  private
    FOnResolve: TResolveTranslatorNotePreviewEvent;
    FPendingNoteId: QWord;
    FPendingRevision: QWord;
    FPreview: TTranslatorNotePreviewWindow;
    FTimer: TTimer;
    function GetPreviewVisible: Boolean;
    function GetTimerEnabled: Boolean;
    function GetTimerInterval: Integer;
    procedure Schedule(AInterval: Integer);
  protected
    function CreatePreviewWindow: TTranslatorNotePreviewWindow; virtual;
    function CurrentCursorScreenPoint: TPoint; virtual;
    procedure TimerElapsed(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Active: Boolean;
    procedure Dismiss;
    procedure Hover(ARevision, ANoteId: QWord);
    procedure Leave;
    property OnResolve: TResolveTranslatorNotePreviewEvent
      read FOnResolve write FOnResolve;
    property PendingNoteId: QWord read FPendingNoteId;
    property PendingRevision: QWord read FPendingRevision;
    property PreviewVisible: Boolean read GetPreviewVisible;
    property TimerEnabled: Boolean read GetTimerEnabled;
    property TimerInterval: Integer read GetTimerInterval;
  end;

implementation

uses
  Math;

constructor TTranslatorNoteHoverController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := TRANSLATOR_NOTE_HOVER_DELAY_MS;
  FTimer.OnTimer := @TimerElapsed;
end;

destructor TTranslatorNoteHoverController.Destroy;
begin
  Dismiss;
  inherited Destroy;
end;

function TTranslatorNoteHoverController.CreatePreviewWindow:
  TTranslatorNotePreviewWindow;
begin
  Result := TTranslatorNotePreviewWindow.Create(nil);
end;

function TTranslatorNoteHoverController.CurrentCursorScreenPoint: TPoint;
begin
  Result := Mouse.CursorPos;
end;

function TTranslatorNoteHoverController.GetPreviewVisible: Boolean;
begin
  Result := Assigned(FPreview);
end;

function TTranslatorNoteHoverController.GetTimerEnabled: Boolean;
begin
  Result := Assigned(FTimer) and FTimer.Enabled;
end;

function TTranslatorNoteHoverController.GetTimerInterval: Integer;
begin
  if Assigned(FTimer) then
    Result := FTimer.Interval
  else
    Result := 0;
end;

procedure TTranslatorNoteHoverController.Schedule(AInterval: Integer);
begin
  FTimer.Enabled := False;
  FTimer.Interval := AInterval;
  FTimer.Enabled := True;
end;

function TTranslatorNoteHoverController.Active: Boolean;
begin
  Result := Assigned(FPreview) or GetTimerEnabled or
    (FPendingNoteId <> 0);
end;

procedure TTranslatorNoteHoverController.Dismiss;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
    FTimer.Interval := TRANSLATOR_NOTE_HOVER_DELAY_MS;
  end;
  FPendingNoteId := 0;
  FPendingRevision := 0;
  FreeAndNil(FPreview);
end;

procedure TTranslatorNoteHoverController.Hover(ARevision, ANoteId: QWord);
begin
  if ANoteId = 0 then
  begin
    Leave;
    Exit;
  end;
  if (FPendingNoteId = ANoteId) and
     (FPendingRevision = ARevision) and
     (GetTimerEnabled or Assigned(FPreview)) then
    Exit;
  Dismiss;
  FPendingNoteId := ANoteId;
  FPendingRevision := ARevision;
  Schedule(TRANSLATOR_NOTE_HOVER_DELAY_MS);
end;

procedure TTranslatorNoteHoverController.Leave;
begin
  if Assigned(FPreview) and (FPendingNoteId <> 0) then
    Schedule(TRANSLATOR_NOTE_HOVER_RECHECK_MS)
  else
  begin
    FTimer.Enabled := False;
    FPendingNoteId := 0;
    FPendingRevision := 0;
  end;
end;

procedure TTranslatorNoteHoverController.TimerElapsed(Sender: TObject);
var
  AnchorRect, HoverRect: TRect;
  BodyRaw: UnicodeString;
  CursorPoint: TPoint;
begin
  FTimer.Enabled := False;
  if (FPendingNoteId = 0) or not Assigned(FOnResolve) or
     not FOnResolve(Self, FPendingRevision, FPendingNoteId, BodyRaw,
       AnchorRect) then
  begin
    Dismiss;
    Exit;
  end;
  if Assigned(FPreview) then
  begin
    HoverRect := Rect(Min(AnchorRect.Left, FPreview.Left),
      Min(AnchorRect.Top, FPreview.Top),
      Max(AnchorRect.Right, FPreview.Left + FPreview.Width),
      Max(AnchorRect.Bottom, FPreview.Top + FPreview.Height));
    Dec(HoverRect.Left, TRANSLATOR_NOTE_HOVER_MARGIN_PX);
    Dec(HoverRect.Top, TRANSLATOR_NOTE_HOVER_MARGIN_PX);
    Inc(HoverRect.Right, TRANSLATOR_NOTE_HOVER_MARGIN_PX);
    Inc(HoverRect.Bottom, TRANSLATOR_NOTE_HOVER_MARGIN_PX);
    CursorPoint := CurrentCursorScreenPoint;
    if (CursorPoint.X >= HoverRect.Left) and
       (CursorPoint.X < HoverRect.Right) and
       (CursorPoint.Y >= HoverRect.Top) and
       (CursorPoint.Y < HoverRect.Bottom) then
      Schedule(TRANSLATOR_NOTE_HOVER_RECHECK_MS)
    else
      Dismiss;
    Exit;
  end;
  FPreview := CreatePreviewWindow;
  if not Assigned(FPreview) then
  begin
    Dismiss;
    Exit;
  end;
  FPreview.ShowNote(BodyRaw, AnchorRect);
  Schedule(TRANSLATOR_NOTE_HOVER_RECHECK_MS);
end;

end.
