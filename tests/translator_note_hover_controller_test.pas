unit translator_note_hover_controller_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, Types, Forms, fpcunit, testregistry,
  translator_note_hover_controller;

type
  TTranslatorNoteHoverResolverProbe = class
  private
    FAccept: Boolean;
    FExpectedId: QWord;
    FExpectedRevision: QWord;
    FResolveCount: Integer;
  public
    function Resolve(Sender: TObject; ARevision, ANoteId: QWord;
      out ABodyRaw: UnicodeString;
      out AAnchorScreenRect: TRect): Boolean;
  end;

  TTranslatorNoteHoverControllerAccess = class(
    TTranslatorNoteHoverController)
  private
    FCursorPoint: TPoint;
    FPreviewCreateCount: Integer;
  protected
    function CreatePreviewWindow: TCustomForm; override;
    function CurrentCursorScreenPoint: TPoint; override;
  public
    procedure ProcessNow;
    property CursorPoint: TPoint read FCursorPoint write FCursorPoint;
    property PreviewCreateCount: Integer read FPreviewCreateCount;
  end;

  TTranslatorNoteHoverControllerTests = class(TTestCase)
  published
    procedure PendingHoverCarriesIdentityAndUsesInitialDelay;
    procedure LeaveBeforePreviewCancelsPendingHover;
    procedure ResolverRejectsStaleIdentityBeforeShowingPreview;
    procedure PreviewRechecksAnchorPopupUnionAndDismissesOutside;
  end;

implementation

function TTranslatorNoteHoverResolverProbe.Resolve(Sender: TObject;
  ARevision, ANoteId: QWord; out ABodyRaw: UnicodeString;
  out AAnchorScreenRect: TRect): Boolean;
begin
  Inc(FResolveCount);
  ABodyRaw := '<b>preview</b>';
  AAnchorScreenRect := Rect(100, 100, 120, 120);
  Result := FAccept and (ARevision = FExpectedRevision) and
    (ANoteId = FExpectedId);
end;

function TTranslatorNoteHoverControllerAccess.CurrentCursorScreenPoint:
  TPoint;
begin
  Result := FCursorPoint;
end;

function TTranslatorNoteHoverControllerAccess.CreatePreviewWindow:
  TCustomForm;
begin
  Inc(FPreviewCreateCount);
  Result := inherited CreatePreviewWindow;
end;

procedure TTranslatorNoteHoverControllerAccess.ProcessNow;
begin
  TimerElapsed(Self);
end;

procedure TTranslatorNoteHoverControllerTests.
  PendingHoverCarriesIdentityAndUsesInitialDelay;
var
  Controller: TTranslatorNoteHoverController;
begin
  Controller := TTranslatorNoteHoverController.Create(nil);
  try
    AssertFalse(Controller.Active);
    Controller.Hover(41, 73);
    AssertTrue(Controller.Active);
    AssertTrue(Controller.TimerEnabled);
    AssertEquals(TRANSLATOR_NOTE_HOVER_DELAY_MS,
      Controller.TimerInterval);
    AssertTrue(Controller.PendingRevision = QWord(41));
    AssertTrue(Controller.PendingNoteId = QWord(73));

    { A revision change for the same note must restart validation instead of
      retaining the stale request. }
    Controller.Hover(42, 73);
    AssertTrue(Controller.PendingRevision = QWord(42));
    AssertEquals(TRANSLATOR_NOTE_HOVER_DELAY_MS,
      Controller.TimerInterval);

    Controller.Dismiss;
    AssertFalse(Controller.Active);
    AssertFalse(Controller.TimerEnabled);
    AssertEquals(TRANSLATOR_NOTE_HOVER_DELAY_MS,
      Controller.TimerInterval);
    AssertTrue(Controller.PendingNoteId = QWord(0));
    Controller.Dismiss;
    AssertFalse(Controller.Active);
  finally
    Controller.Free;
  end;
end;

procedure TTranslatorNoteHoverControllerTests.
  LeaveBeforePreviewCancelsPendingHover;
var
  Controller: TTranslatorNoteHoverController;
begin
  Controller := TTranslatorNoteHoverController.Create(nil);
  try
    Controller.Hover(7, 9);
    AssertTrue(Controller.Active);
    Controller.Leave;
    AssertFalse(Controller.Active);
    AssertFalse(Controller.TimerEnabled);
    AssertTrue(Controller.PendingRevision = QWord(0));
    AssertTrue(Controller.PendingNoteId = QWord(0));

    Controller.Hover(7, 0);
    AssertFalse(Controller.Active);
  finally
    Controller.Free;
  end;
end;

procedure TTranslatorNoteHoverControllerTests.
  ResolverRejectsStaleIdentityBeforeShowingPreview;
var
  Controller: TTranslatorNoteHoverControllerAccess;
  Resolver: TTranslatorNoteHoverResolverProbe;
begin
  Resolver := TTranslatorNoteHoverResolverProbe.Create;
  Controller := TTranslatorNoteHoverControllerAccess.Create(nil);
  try
    Resolver.FAccept := True;
    Resolver.FExpectedRevision := 8;
    Resolver.FExpectedId := 10;
    Controller.OnResolve := @Resolver.Resolve;
    Controller.Hover(7, 10);
    Controller.ProcessNow;

    AssertEquals(1, Resolver.FResolveCount);
    AssertFalse(Controller.Active);
    AssertFalse(Controller.PreviewVisible);
    AssertTrue(Controller.PendingNoteId = QWord(0));
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslatorNoteHoverControllerTests.
  PreviewRechecksAnchorPopupUnionAndDismissesOutside;
var
  Controller: TTranslatorNoteHoverControllerAccess;
  Resolver: TTranslatorNoteHoverResolverProbe;
begin
  Application.Initialize;
  Resolver := TTranslatorNoteHoverResolverProbe.Create;
  Controller := TTranslatorNoteHoverControllerAccess.Create(nil);
  try
    Resolver.FAccept := True;
    Resolver.FExpectedRevision := 11;
    Resolver.FExpectedId := 12;
    Controller.OnResolve := @Resolver.Resolve;
    Controller.CursorPoint := Point(110, 122);
    Controller.Hover(11, 12);
    Controller.ProcessNow;

    AssertTrue(Controller.PreviewVisible);
    AssertEquals(1, Controller.PreviewCreateCount);
    AssertTrue(Controller.TimerEnabled);
    AssertEquals(TRANSLATOR_NOTE_HOVER_RECHECK_MS,
      Controller.TimerInterval);

    { The four-pixel gap between the marker and the popup is inside the
      expanded union, allowing the pointer to travel into the preview. }
    Controller.Leave;
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);
    AssertTrue(Controller.TimerEnabled);
    AssertEquals(TRANSLATOR_NOTE_HOVER_RECHECK_MS,
      Controller.TimerInterval);

    Controller.CursorPoint := Point(-10000, -10000);
    Controller.ProcessNow;
    AssertFalse(Controller.Active);
    AssertFalse(Controller.PreviewVisible);
    AssertFalse(Controller.TimerEnabled);

    Controller.CursorPoint := Point(110, 110);
    Controller.Hover(11, 12);
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);
    AssertEquals('ordinary dismissal reuses the translator-note window',
      1, Controller.PreviewCreateCount);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

initialization
  RegisterTest(TTranslatorNoteHoverControllerTests);

end.
