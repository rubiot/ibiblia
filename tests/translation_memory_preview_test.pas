unit translation_memory_preview_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, StdCtrls, Graphics, IniFiles,
  LCLType,
  fpcunit, testregistry, KMemo, SuggestionManager,
  translation_memory_model, translation_memory_preview, ChapterView
  {$IFDEF WINDOWS}, Windows{$ENDIF};

type
  TTranslationMemoryPreviewWindowProbe = class(
    TTranslationMemoryPreviewWindow)
  private
    FShowCount: Integer;
    FShownAnchor: TRect;
    FShownPayload: TTranslationMemoryPreviewPayload;
  public
    destructor Destroy; override;
    procedure ShowPayload(const APayload: TTranslationMemoryPreviewPayload;
      const AAnchorScreenRect: TRect); override;
  end;

  TTranslationMemoryFontSizeObserver = class
  private
    FChangeCount: Integer;
    FLastFontSize: Integer;
  public
    procedure FontSizeChanged(Sender: TObject; AFontSize: Integer);
  end;

  TTranslationMemoryHoverControllerAccess = class(
    TTranslationMemoryHoverController)
  private
    FApplicationActive: Boolean;
    FCursorPoint: TPoint;
    FLastPreview: TTranslationMemoryPreviewWindowProbe;
    FPreviewCreateCount: Integer;
  protected
    function ApplicationIsActive: Boolean; override;
    function CreatePreviewWindow: TTranslationMemoryPreviewWindow; override;
    function CurrentCursorScreenPoint: TPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ProcessNow;
    procedure SendApplicationKeyDown(var AKey: Word; AShift: TShiftState);
    property ApplicationActive: Boolean read FApplicationActive
      write FApplicationActive;
    property CursorPoint: TPoint read FCursorPoint write FCursorPoint;
    property LastPreview: TTranslationMemoryPreviewWindowProbe
      read FLastPreview;
    property PreviewCreateCount: Integer read FPreviewCreateCount;
  end;

  TTranslationMemoryResolverProbe = class
  private
    FAnchorRect: TRect;
    FExpectedTarget: TTranslationMemoryHoverTarget;
    FErrorMessage: string;
    FLastLoadedTarget: TTranslationMemoryHoverTarget;
    FLoadAccept: Boolean;
    FLoadCount: Integer;
    FValidateAccept: Boolean;
    FValidateCount: Integer;
  public
    constructor Create;
    function Load(const ATarget: TTranslationMemoryHoverTarget;
      out APayload: TTranslationMemoryPreviewPayload): Boolean;
    function Validate(const ATarget: TTranslationMemoryHoverTarget;
      out AAnchorScreenRect: TRect): Boolean;
  end;

  TTranslationMemoryPreviewTests = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure HoverUsesDelayCancelsOnLeaveAndOnlyLoadsLatestTarget;
    procedure InvalidGenerationAndTargetNeverOpenPreview;
    procedure CursorOutsideExactAnchorBeforeOpenNeverLoads;
    procedure InactiveApplicationOnRecheckClosesWithoutLoading;
    procedure CacheAndRecheckDoNotLoadPayloadAgain;
    procedure ErrorPayloadIsNeverCached;
    procedure GlobalEscapeClosesPreviewPreservesCacheAndKey;
    procedure PopupUnionExitInvalidateAndDestructionClosePreview;
    procedure ChangedAnchorGeometryClosesPreview;
    procedure FontSizePreferenceReadsNormalizesAndWritesLocalIni;
    procedure CtrlWheelChangesPreviewFontWithinBoundsAndNotifies;
    procedure ControllerCachesChangedFontSizeAcrossReopen;
    procedure RendererLimitsToTenAndPreservesOccurrencePresentation;
    procedure AlternativeURLParserIsStrict;
    {$IFDEF WINDOWS}
    procedure RenderedHintScrollsWithoutTakingFocus;
    {$ENDIF}
    procedure ChapterHoverOnlyAcceptsRegisteredOccurrenceCounter;
    procedure ChapterOccurrenceCounterClickCancelsHoverAndKeepsSelection;
  end;

implementation

var
  PreviewDestroyCount: Integer = 0;

function MakeTarget(AGeneration: QWord; AEntryIndex,
  AAlternativeIndex: Integer): TTranslationMemoryHoverTarget;
begin
  Result := Default(TTranslationMemoryHoverTarget);
  Result.Generation := AGeneration;
  Result.EntryIndex := AEntryIndex;
  Result.AlternativeIndex := AAlternativeIndex;
end;

function TargetsEqual(const ALeft,
  ARight: TTranslationMemoryHoverTarget): Boolean;
begin
  Result := (ALeft.Generation = ARight.Generation) and
    (ALeft.EntryIndex = ARight.EntryIndex) and
    (ALeft.AlternativeIndex = ARight.AlternativeIndex);
end;

function FindTextBlock(ABlocks: TKMemoBlocks;
  const AText: string): TKMemoTextBlock;
var
  BlockIndex: Integer;
begin
  Result := nil;
  for BlockIndex := 0 to ABlocks.Count - 1 do
    if (ABlocks[BlockIndex] is TKMemoTextBlock) and
       (TKMemoTextBlock(ABlocks[BlockIndex]).Text = AText) then
      Exit(TKMemoTextBlock(ABlocks[BlockIndex]));
end;

function CountTextBlocksWithPrefix(ABlocks: TKMemoBlocks;
  const APrefix: string): Integer;
var
  BlockIndex: Integer;
  BlockText: string;
begin
  Result := 0;
  for BlockIndex := 0 to ABlocks.Count - 1 do
    if ABlocks[BlockIndex] is TKMemoTextBlock then
    begin
      BlockText := TKMemoTextBlock(ABlocks[BlockIndex]).Text;
      if Copy(BlockText, 1, Length(APrefix)) = APrefix then
        Inc(Result);
    end;
end;

function ContainsHyperlink(ABlocks: TKMemoBlocks): Boolean;
var
  BlockIndex: Integer;
begin
  Result := False;
  for BlockIndex := 0 to ABlocks.Count - 1 do
    if ABlocks[BlockIndex] is TKMemoHyperlink then
      Exit(True);
end;

{ TTranslationMemoryPreviewWindowProbe }

destructor TTranslationMemoryPreviewWindowProbe.Destroy;
begin
  Inc(PreviewDestroyCount);
  inherited Destroy;
end;

procedure TTranslationMemoryPreviewWindowProbe.ShowPayload(
  const APayload: TTranslationMemoryPreviewPayload;
  const AAnchorScreenRect: TRect);
begin
  Inc(FShowCount);
  FShownPayload := APayload;
  FShownAnchor := AAnchorScreenRect;
  SetBounds(130, 124, 220, 100);
  Visible := True;
end;

{ TTranslationMemoryFontSizeObserver }

procedure TTranslationMemoryFontSizeObserver.FontSizeChanged(Sender: TObject;
  AFontSize: Integer);
begin
  Inc(FChangeCount);
  FLastFontSize := AFontSize;
end;

{ TTranslationMemoryHoverControllerAccess }

constructor TTranslationMemoryHoverControllerAccess.Create(
  AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApplicationActive := True;
  FCursorPoint := Types.Point(110, 110);
end;

function TTranslationMemoryHoverControllerAccess.ApplicationIsActive:
  Boolean;
begin
  Result := FApplicationActive;
end;

function TTranslationMemoryHoverControllerAccess.CreatePreviewWindow:
  TTranslationMemoryPreviewWindow;
begin
  Inc(FPreviewCreateCount);
  FLastPreview := TTranslationMemoryPreviewWindowProbe.Create(nil);
  Result := FLastPreview;
end;

function TTranslationMemoryHoverControllerAccess.CurrentCursorScreenPoint:
  TPoint;
begin
  Result := FCursorPoint;
end;

procedure TTranslationMemoryHoverControllerAccess.ProcessNow;
begin
  TimerElapsed(Self);
end;

procedure TTranslationMemoryHoverControllerAccess.SendApplicationKeyDown(
  var AKey: Word; AShift: TShiftState);
begin
  HandleApplicationKeyDown(nil, AKey, AShift);
end;

{ TTranslationMemoryResolverProbe }

constructor TTranslationMemoryResolverProbe.Create;
begin
  inherited Create;
  FAnchorRect := Types.Rect(100, 100, 120, 120);
  FLoadAccept := True;
  FValidateAccept := True;
end;

function TTranslationMemoryResolverProbe.Load(
  const ATarget: TTranslationMemoryHoverTarget;
  out APayload: TTranslationMemoryPreviewPayload): Boolean;
begin
  Inc(FLoadCount);
  FLastLoadedTarget := ATarget;
  APayload := Default(TTranslationMemoryPreviewPayload);
  APayload.TitleText := 'preview';
  APayload.ErrorMessage := FErrorMessage;
  APayload.FontSize := 10;
  SetLength(APayload.Page.Items, 1);
  APayload.Page.Items[0].ReferenceID := '1,1,1';
  APayload.Page.Items[0].DisplayReference := 'Gn 1:1';
  Result := FLoadAccept and TargetsEqual(ATarget, FExpectedTarget);
end;

function TTranslationMemoryResolverProbe.Validate(
  const ATarget: TTranslationMemoryHoverTarget;
  out AAnchorScreenRect: TRect): Boolean;
begin
  Inc(FValidateCount);
  AAnchorScreenRect := FAnchorRect;
  Result := FValidateAccept and TargetsEqual(ATarget, FExpectedTarget);
end;

{ TTranslationMemoryPreviewTests }

procedure TTranslationMemoryPreviewTests.SetUp;
begin
  inherited SetUp;
  Application.Initialize;
  PreviewDestroyCount := 0;
end;

procedure TTranslationMemoryPreviewTests.
  HoverUsesDelayCancelsOnLeaveAndOnlyLoadsLatestTarget;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  TargetA: TTranslationMemoryHoverTarget;
  TargetB: TTranslationMemoryHoverTarget;
begin
  TargetA := MakeTarget(7, 1, 2);
  TargetB := MakeTarget(7, 1, 3);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := TargetA;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;

    Controller.Hover(TargetA);
    AssertTrue(Controller.Active);
    AssertTrue(Controller.HasPendingTarget);
    AssertTrue(TargetsEqual(TargetA, Controller.PendingTarget));
    AssertTrue(Controller.TimerEnabled);
    AssertEquals(TRANSLATION_MEMORY_HOVER_DELAY_MS,
      Controller.TimerInterval);
    AssertEquals(400, Controller.TimerInterval);
    AssertEquals(0, Resolver.FLoadCount);

    Controller.Leave;
    AssertFalse(Controller.Active);
    AssertFalse(Controller.TimerEnabled);
    AssertEquals(0, Resolver.FLoadCount);

    Controller.Hover(TargetA);
    Controller.Hover(TargetB);
    Resolver.FExpectedTarget := TargetB;
    Controller.ProcessNow;
    AssertEquals(1, Resolver.FLoadCount);
    AssertTrue(TargetsEqual(TargetB, Resolver.FLastLoadedTarget));
    AssertTrue(Controller.PreviewVisible);
    AssertEquals(1, Controller.PreviewCreateCount);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  InvalidGenerationAndTargetNeverOpenPreview;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := MakeTarget(9, 2, 3);
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;

    Target := MakeTarget(8, 2, 3);
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertEquals(1, Resolver.FValidateCount);
    AssertEquals(0, Resolver.FLoadCount);
    AssertEquals(0, Controller.PreviewCreateCount);
    AssertFalse(Controller.Active);

    Controller.Hover(MakeTarget(9, -1, 3));
    AssertFalse(Controller.Active);
    Controller.Hover(MakeTarget(9, 2, -1));
    AssertFalse(Controller.Active);
    AssertEquals(1, Resolver.FValidateCount);
    AssertEquals(0, Resolver.FLoadCount);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  CursorOutsideExactAnchorBeforeOpenNeverLoads;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(10, 3, 4);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;
    Controller.CursorPoint := Types.Point(99, 110);

    Controller.Hover(Target);
    Controller.ProcessNow;

    AssertEquals(1, Resolver.FValidateCount);
    AssertEquals('the expanded recheck margin must not apply before opening',
      0, Resolver.FLoadCount);
    AssertEquals(0, Controller.PreviewCreateCount);
    AssertFalse(Controller.Active);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  InactiveApplicationOnRecheckClosesWithoutLoading;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(12, 5, 6);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);
    AssertEquals(1, Resolver.FLoadCount);

    Controller.ApplicationActive := False;
    Controller.ProcessNow;
    AssertFalse('an inactive application closes the visible preview',
      Controller.PreviewVisible);
    AssertFalse(Controller.Active);
    AssertEquals('the inactive recheck must not load again',
      1, Resolver.FLoadCount);
    Controller.ApplicationActive := True;
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  CacheAndRecheckDoNotLoadPayloadAgain;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(11, 4, 5);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;

    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertEquals(1, Resolver.FLoadCount);
    AssertTrue(Controller.CacheValid);
    AssertTrue(TargetsEqual(Target, Controller.CachedTarget));
    AssertTrue(Controller.PreviewVisible);

    Controller.ProcessNow;
    AssertEquals('recheck must only validate geometry and pointer',
      1, Resolver.FLoadCount);
    AssertTrue(Controller.PreviewVisible);
    AssertEquals(TRANSLATION_MEMORY_HOVER_RECHECK_MS,
      Controller.TimerInterval);

    Controller.Dismiss;
    AssertTrue('ordinary dismissal keeps the short-lived value cache',
      Controller.CacheValid);
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertEquals('the same target must be served from cache',
      1, Resolver.FLoadCount);
    AssertEquals(2, Controller.PreviewCreateCount);
    AssertTrue(Controller.PreviewVisible);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  ErrorPayloadIsNeverCached;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(14, 7, 8);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Resolver.FErrorMessage := 'preview unavailable';
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;

    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertEquals(1, Resolver.FLoadCount);
    AssertTrue(Controller.PreviewVisible);
    AssertFalse('error payloads must not enter the value cache',
      Controller.CacheValid);

    Controller.Dismiss;
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertEquals('the same error target must be loaded again',
      2, Resolver.FLoadCount);
    AssertTrue(Controller.PreviewVisible);
    AssertFalse(Controller.CacheValid);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  GlobalEscapeClosesPreviewPreservesCacheAndKey;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Key: Word;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(16, 9, 10);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);
    AssertTrue(Controller.CacheValid);
    AssertEquals(1, Resolver.FLoadCount);

    Key := VK_ESCAPE;
    Controller.SendApplicationKeyDown(Key, []);

    AssertEquals('the global handler must not consume Escape',
      Integer(VK_ESCAPE), Integer(Key));
    AssertFalse('global Escape closes the visible preview',
      Controller.PreviewVisible);
    AssertFalse(Controller.Active);
    AssertTrue('ordinary Escape dismissal preserves the value cache',
      Controller.CacheValid);
    AssertTrue(TargetsEqual(Target, Controller.CachedTarget));
    AssertEquals(1, Resolver.FLoadCount);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  PopupUnionExitInvalidateAndDestructionClosePreview;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  DestroyCountBeforeFree: Integer;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(13, 6, 7);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);

    Controller.CursorPoint := Types.Point(150, 140);
    Controller.Leave;
    Controller.ProcessNow;
    AssertTrue('moving from the anchor into the popup keeps it open',
      Controller.PreviewVisible);

    Controller.CursorPoint := Types.Point(-10000, -10000);
    Controller.ProcessNow;
    AssertFalse('leaving both anchor and popup closes it',
      Controller.PreviewVisible);
    AssertFalse(Controller.Active);

    Controller.CursorPoint := Types.Point(110, 110);
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);
    Controller.Invalidate;
    AssertFalse('invalidating view state closes the popup',
      Controller.PreviewVisible);
    AssertFalse(Controller.CacheValid);

    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);
    DestroyCountBeforeFree := PreviewDestroyCount;
    Controller.Free;
    Controller := nil;
    AssertEquals('controller destruction owns and closes the popup',
      DestroyCountBeforeFree + 1, PreviewDestroyCount);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  ChangedAnchorGeometryClosesPreview;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(15, 8, 9);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertTrue(Controller.PreviewVisible);
    AssertEquals(100, Controller.PreviewAnchorScreenRect.Left);

    Controller.CursorPoint := Types.Point(150, 140);
    Resolver.FAnchorRect := Types.Rect(300, 100, 320, 120);
    Controller.ProcessNow;
    AssertFalse('layout movement invalidates the original anchor',
      Controller.PreviewVisible);
    AssertFalse(Controller.Active);
    AssertEquals('geometry recheck must not reload data',
      1, Resolver.FLoadCount);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  FontSizePreferenceReadsNormalizesAndWritesLocalIni;
var
  FileName: string;
  Ini: TMemIniFile;
begin
  AssertEquals('normalization clamps the lower bound',
    TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE,
    NormalizeTranslationMemoryPreviewFontSize(0));
  AssertEquals('normalization keeps an in-range value', 18,
    NormalizeTranslationMemoryPreviewFontSize(18));
  AssertEquals('normalization clamps the upper bound',
    TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE,
    NormalizeTranslationMemoryPreviewFontSize(100));
  AssertEquals('nil settings use the normalized default', 12,
    ReadTranslationMemoryPreviewFontSize(nil, 12));
  AssertEquals('nil settings also clamp the default',
    TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE,
    ReadTranslationMemoryPreviewFontSize(nil, 1));
  WriteTranslationMemoryPreviewFontSize(nil, 14);

  FileName := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'ibiblia_translation_memory_preview_font_test.ini';
  SysUtils.DeleteFile(FileName);
  Ini := nil;
  try
    Ini := TMemIniFile.Create(FileName);
    AssertEquals('missing setting uses the caller default', 11,
      ReadTranslationMemoryPreviewFontSize(Ini, 11));

    Ini.WriteInteger(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
      TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY, 17);
    AssertEquals('valid stored setting is loaded', 17,
      ReadTranslationMemoryPreviewFontSize(Ini, 11));

    Ini.WriteInteger(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
      TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY, 5);
    AssertEquals('stored setting is clamped at the lower bound',
      TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE,
      ReadTranslationMemoryPreviewFontSize(Ini, 11));

    Ini.WriteInteger(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
      TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY, 73);
    AssertEquals('stored setting is clamped at the upper bound',
      TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE,
      ReadTranslationMemoryPreviewFontSize(Ini, 11));

    Ini.WriteString(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
      TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY, 'invalid');
    AssertEquals('malformed setting falls back to the caller default', 13,
      ReadTranslationMemoryPreviewFontSize(Ini, 13));

    WriteTranslationMemoryPreviewFontSize(Ini, 2);
    AssertEquals('writing clamps the lower bound',
      TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE,
      Ini.ReadInteger(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
        TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY, 0));
    WriteTranslationMemoryPreviewFontSize(Ini, 90);
    AssertEquals('writing clamps the upper bound',
      TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE,
      Ini.ReadInteger(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
        TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY, 0));

    Ini.UpdateFile;
    FreeAndNil(Ini);
    Ini := TMemIniFile.Create(FileName);
    AssertEquals('the normalized size survives closing and reopening the ini',
      TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE,
      ReadTranslationMemoryPreviewFontSize(Ini, 10));
  finally
    FreeAndNil(Ini);
    SysUtils.DeleteFile(FileName);
  end;
end;

procedure TTranslationMemoryPreviewTests.
  CtrlWheelChangesPreviewFontWithinBoundsAndNotifies;
var
  Handled: Boolean;
  Observer: TTranslationMemoryFontSizeObserver;
  Payload: TTranslationMemoryPreviewPayload;
  Preview: TTranslationMemoryPreviewWindow;
begin
  Payload := Default(TTranslationMemoryPreviewPayload);
  Payload.TitleText := 'source — translation';
  Payload.NoOccurrencesText := 'none';
  Payload.FontName := 'default';
  Payload.FontSize := 10;
  Preview := TTranslationMemoryPreviewWindow.Create(nil);
  Observer := TTranslationMemoryFontSizeObserver.Create;
  try
    Preview.OnFontSizeChanged := @Observer.FontSizeChanged;
    Preview.ShowPayload(Payload, Types.Rect(100, 100, 120, 120));

    Handled := False;
    Preview.Memo.OnMouseWheel(Preview.Memo, [ssCtrl], 120,
      Types.Point(0, 0), Handled);
    AssertTrue('Ctrl+wheel is consumed', Handled);
    AssertEquals('one wheel gesture increases exactly one point', 11,
      Preview.FontSize);
    AssertEquals('a real change emits one callback', 1,
      Observer.FChangeCount);
    AssertEquals('callback reports the new size', 11,
      Observer.FLastFontSize);

    Handled := False;
    Preview.Memo.OnMouseWheel(Preview.Memo, [], -120,
      Types.Point(0, 0), Handled);
    AssertFalse('plain wheel remains available for scrolling', Handled);
    AssertEquals('plain wheel does not change the font', 11,
      Preview.FontSize);
    AssertEquals('plain wheel does not notify', 1, Observer.FChangeCount);

    Payload.FontSize := TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE;
    Preview.ShowPayload(Payload, Types.Rect(100, 100, 120, 120));
    Handled := False;
    Preview.Memo.OnMouseWheel(Preview.Memo, [ssCtrl], 120,
      Types.Point(0, 0), Handled);
    AssertTrue('Ctrl+wheel at the upper bound is still consumed', Handled);
    AssertEquals(TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE,
      Preview.FontSize);
    AssertEquals('no callback is emitted when the size cannot change', 1,
      Observer.FChangeCount);

    Handled := False;
    Preview.Memo.OnMouseWheel(Preview.Memo, [ssCtrl], -120,
      Types.Point(0, 0), Handled);
    AssertTrue(Handled);
    AssertEquals(TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE - 1,
      Preview.FontSize);
    AssertEquals(2, Observer.FChangeCount);

    Payload.FontSize := TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE;
    Preview.ShowPayload(Payload, Types.Rect(100, 100, 120, 120));
    Handled := False;
    Preview.Memo.OnMouseWheel(Preview.Memo, [ssCtrl], -120,
      Types.Point(0, 0), Handled);
    AssertTrue('Ctrl+wheel at the lower bound is still consumed', Handled);
    AssertEquals(TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE,
      Preview.FontSize);
    AssertEquals('lower-bound no-op does not notify', 2,
      Observer.FChangeCount);

    Handled := False;
    Preview.Memo.OnMouseWheel(Preview.Memo, [ssCtrl], 120,
      Types.Point(0, 0), Handled);
    AssertTrue(Handled);
    AssertEquals(TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE + 1,
      Preview.FontSize);
    AssertEquals(3, Observer.FChangeCount);
    AssertEquals(TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE + 1,
      Observer.FLastFontSize);
  finally
    Observer.Free;
    Preview.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  ControllerCachesChangedFontSizeAcrossReopen;
var
  Controller: TTranslationMemoryHoverControllerAccess;
  Resolver: TTranslationMemoryResolverProbe;
  Target: TTranslationMemoryHoverTarget;
begin
  Target := MakeTarget(16, 9, 10);
  Resolver := TTranslationMemoryResolverProbe.Create;
  Controller := TTranslationMemoryHoverControllerAccess.Create(nil);
  try
    Resolver.FExpectedTarget := Target;
    Controller.OnValidateTarget := @Resolver.Validate;
    Controller.OnLoadPayload := @Resolver.Load;
    Controller.Hover(Target);
    Controller.ProcessNow;
    AssertEquals(1, Resolver.FLoadCount);
    AssertTrue(Assigned(Controller.LastPreview));
    AssertEquals(10, Controller.LastPreview.FShownPayload.FontSize);
    AssertTrue('controller subscribes to preview font changes',
      Assigned(Controller.LastPreview.OnFontSizeChanged));

    Controller.LastPreview.OnFontSizeChanged(Controller.LastPreview, 16);
    Controller.Dismiss;
    Controller.Hover(Target);
    Controller.ProcessNow;

    AssertEquals('cache reopening does not query again', 1,
      Resolver.FLoadCount);
    AssertEquals('cache reopening keeps the changed font size', 16,
      Controller.LastPreview.FShownPayload.FontSize);
    AssertEquals('reopening creates a new popup from the cached payload', 2,
      Controller.PreviewCreateCount);
  finally
    Controller.Free;
    Resolver.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  RendererLimitsToTenAndPreservesOccurrencePresentation;
const
  FOOTER_TEXT = '2 ocorrências restantes — clique para ver todas';
var
  CurrentReferenceBlock: TKMemoTextBlock;
  HighlightBlock: TKMemoTextBlock;
  Index: Integer;
  InvalidRangeBlock: TKMemoTextBlock;
  NormalReferenceBlock: TKMemoTextBlock;
  Payload: TTranslationMemoryPreviewPayload;
  Preview: TTranslationMemoryPreviewWindow;
begin
  Payload := Default(TTranslationMemoryPreviewPayload);
  Payload.TitleText := 'θεραπεύω — curar';
  Payload.TotalsText := '12 versículos · 14 ocorrências';
  Payload.NoOccurrencesText := 'Nenhuma ocorrência';
  Payload.OccurrenceCountFormat := '%d ocorrências';
  Payload.FooterText := FOOTER_TEXT;
  Payload.CurrentReferenceID := 'ref-3';
  Payload.FontName := 'default';
  Payload.FontSize := 10;
  Payload.Page.TotalAssociations := 14;
  Payload.Page.TotalVerses := 12;
  Payload.Page.HasMore := True;
  SetLength(Payload.Page.Items, 12);
  for Index := 0 to High(Payload.Page.Items) do
  begin
    Payload.Page.Items[Index].ReferenceID := Format('ref-%d', [Index + 1]);
    Payload.Page.Items[Index].DisplayReference := Format('Ref %d', [Index + 1]);
    Payload.Page.Items[Index].DestinationText := Format('destination %d',
      [Index + 1]);
    Payload.Page.Items[Index].Occurrences := 1;
  end;
  Payload.Page.Items[0].DestinationText := 'alpha beta gamma';
  Payload.Page.Items[0].Occurrences := 2;
  SetLength(Payload.Page.Items[0].HighlightRanges, 1);
  Payload.Page.Items[0].HighlightRanges[0].StartIndex := 7;
  Payload.Page.Items[0].HighlightRanges[0].Length := 4;
  Payload.Page.Items[1].DestinationText := 'plain invalid range';
  SetLength(Payload.Page.Items[1].HighlightRanges, 1);
  Payload.Page.Items[1].HighlightRanges[0].StartIndex := 999;
  Payload.Page.Items[1].HighlightRanges[0].Length := 2;

  Preview := TTranslationMemoryPreviewWindow.Create(nil);
  try
    Preview.ShowPayload(Payload, Types.Rect(100, 100, 120, 120));
    AssertTrue(Preview.Memo.ReadOnly);
    AssertFalse(Preview.Memo.TabStop);
    AssertEquals('preview renders at most ten occurrence rows', 10,
      CountTextBlocksWithPrefix(Preview.Memo.Blocks, 'Ref '));
    AssertTrue(Assigned(FindTextBlock(Preview.Memo.Blocks, 'Ref 10')));
    AssertFalse(Assigned(FindTextBlock(Preview.Memo.Blocks, 'Ref 11')));
    AssertTrue('caller-provided title is rendered',
      Assigned(FindTextBlock(Preview.Memo.Blocks, Payload.TitleText)));
    AssertTrue('caller-provided totals are rendered',
      Assigned(FindTextBlock(Preview.Memo.Blocks, Payload.TotalsText)));
    AssertTrue('per-verse count is rendered when greater than one',
      Assigned(FindTextBlock(Preview.Memo.Blocks, ' (2 ocorrências)')));
    AssertTrue('caller-provided remaining count is rendered',
      Assigned(FindTextBlock(Preview.Memo.Blocks, FOOTER_TEXT)));

    HighlightBlock := FindTextBlock(Preview.Memo.Blocks, 'beta');
    AssertTrue('valid highlight range creates a dedicated run',
      Assigned(HighlightBlock));
    AssertTrue(fsBold in HighlightBlock.TextStyle.Font.Style);
    AssertTrue(fsUnderline in HighlightBlock.TextStyle.Font.Style);

    InvalidRangeBlock := FindTextBlock(Preview.Memo.Blocks,
      'plain invalid range');
    AssertTrue('an invalid range falls back to the full plain text',
      Assigned(InvalidRangeBlock));
    AssertTrue(InvalidRangeBlock.TextStyle.Font.Style = []);

    NormalReferenceBlock := FindTextBlock(Preview.Memo.Blocks, 'Ref 1');
    CurrentReferenceBlock := FindTextBlock(Preview.Memo.Blocks, 'Ref 3');
    AssertTrue(Assigned(NormalReferenceBlock));
    AssertTrue(Assigned(CurrentReferenceBlock));
    AssertTrue(fsBold in CurrentReferenceBlock.TextStyle.Font.Style);
    AssertTrue('current verse receives a distinct visual color',
      CurrentReferenceBlock.TextStyle.Font.Color <>
      NormalReferenceBlock.TextStyle.Font.Color);
    AssertFalse('read-only preview must not contain clickable references',
      ContainsHyperlink(Preview.Memo.Blocks));
  finally
    Preview.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.AlternativeURLParserIsStrict;
var
  AlternativeIndex: Integer;
  EntryIndex: Integer;

  procedure AssertRejected(const AURL: string);
  begin
    EntryIndex := 123;
    AlternativeIndex := 456;
    AssertFalse('must reject "' + AURL + '"',
      TryParseTranslationMemoryHoverURL(AURL, EntryIndex,
        AlternativeIndex));
    AssertEquals(-1, EntryIndex);
    AssertEquals(-1, AlternativeIndex);
  end;

begin
  AssertTrue(TryParseTranslationMemoryHoverURL('0:0', EntryIndex,
    AlternativeIndex));
  AssertEquals(0, EntryIndex);
  AssertEquals(0, AlternativeIndex);
  AssertTrue(TryParseTranslationMemoryHoverURL('12:34', EntryIndex,
    AlternativeIndex));
  AssertEquals(12, EntryIndex);
  AssertEquals(34, AlternativeIndex);

  AssertRejected('');
  AssertRejected('12');
  AssertRejected(':34');
  AssertRejected('12:');
  AssertRejected('12:34:56');
  AssertRejected('-1:2');
  AssertRejected('1:-2');
  AssertRejected('+1:2');
  AssertRejected(' 1:2');
  AssertRejected('1:2 ');
  AssertRejected('entry:0');
  AssertRejected('back');
  AssertRejected('more');
  AssertRejected('1,1,1');
  AssertRejected('999999999999999999999999:1');
end;

{$IFDEF WINDOWS}
procedure TTranslationMemoryPreviewTests.
  RenderedHintScrollsWithoutTakingFocus;
var
  ActiveControlBefore: TWinControl;
  AnchorPoint: TPoint;
  Edit: TEdit;
  ExtendedStyle: PtrInt;
  FontBeforeCtrlWheel: Integer;
  FocusBefore: HWND;
  Host: TForm;
  Index: Integer;
  LongText: string;
  MemoScreenPoint: TPoint;
  Payload: TTranslationMemoryPreviewPayload;
  Preview: TTranslationMemoryPreviewWindow;
  TopBeforeWheel: Integer;
begin
  Host := TForm.CreateNew(nil);
  Preview := nil;
  try
    Host.SetBounds(80, 80, 420, 180);
    Edit := TEdit.Create(Host);
    Edit.Parent := Host;
    Edit.SetBounds(20, 20, 260, 28);
    Edit.Text := 'focus owner';
    Host.Show;
    Edit.SetFocus;
    Application.ProcessMessages;

    FocusBefore := Windows.GetFocus;
    ActiveControlBefore := Host.ActiveControl;
    AssertTrue('the edit must own Win32 focus before opening the preview',
      FocusBefore = Edit.Handle);
    AssertTrue('the edit must be the form active control',
      ActiveControlBefore = Edit);

    Payload := Default(TTranslationMemoryPreviewPayload);
    Payload.TitleText := 'source — translation';
    Payload.TotalsText := '10 verses';
    Payload.NoOccurrencesText := 'none';
    Payload.OccurrenceCountFormat := '%d occurrences';
    Payload.FontName := 'default';
    Payload.FontSize := 10;
    Payload.Page.TotalAssociations := 10;
    Payload.Page.TotalVerses := 10;
    SetLength(Payload.Page.Items, 10);
    LongText := '';
    for Index := 1 to 45 do
      LongText := LongText + 'long wrapped destination text ';
    for Index := 0 to High(Payload.Page.Items) do
    begin
      Payload.Page.Items[Index].ReferenceID := IntToStr(Index + 1);
      Payload.Page.Items[Index].DisplayReference :=
        Format('Reference %d', [Index + 1]);
      Payload.Page.Items[Index].DestinationText := LongText;
      Payload.Page.Items[Index].Occurrences := 1;
    end;

    Preview := TTranslationMemoryPreviewWindow.Create(nil);
    AnchorPoint := Edit.ClientToScreen(Types.Point(0, Edit.Height));
    Preview.ShowPayload(Payload, Types.Rect(AnchorPoint.X, AnchorPoint.Y,
      AnchorPoint.X + Edit.Width, AnchorPoint.Y + 1));
    Application.ProcessMessages;

    AssertEquals('the preview exposes an always-present vertical scrollbar',
      Ord(ssVertical), Ord(Preview.Memo.ScrollBars));
    AssertTrue('the long payload must exceed the memo viewport',
      Preview.Memo.ContentHeight > Preview.Memo.ClientHeight);
    AssertTrue('the rendered hint is visible', Preview.Visible);
    AssertTrue('opening the rendered hint must preserve Win32 focus',
      Windows.GetFocus = FocusBefore);
    AssertTrue('opening the rendered hint must preserve ActiveControl',
      Host.ActiveControl = ActiveControlBefore);

    ExtendedStyle := Windows.GetWindowLongPtr(
      Preview.Handle, GWL_EXSTYLE);
    AssertTrue('the preview keeps WS_EX_NOACTIVATE',
      (ExtendedStyle and WS_EX_NOACTIVATE) <> 0);
    AssertTrue('the preview stays out of the taskbar',
      (ExtendedStyle and WS_EX_TOOLWINDOW) <> 0);
    AssertTrue('the preview remains topmost',
      (ExtendedStyle and WS_EX_TOPMOST) <> 0);

    Preview.Memo.TopPos := 0;
    TopBeforeWheel := Preview.Memo.TopPos;
    MemoScreenPoint := Preview.Memo.ClientToScreen(
      Types.Point(Preview.Memo.ClientWidth div 2,
        Preview.Memo.ClientHeight div 2));
    Windows.SendMessage(Preview.Memo.Handle, WM_MOUSEWHEEL,
      Windows.MakeWParam(0, Word(SmallInt(-120))),
      Windows.MakeLParam(Word(MemoScreenPoint.X),
        Word(MemoScreenPoint.Y)));
    Application.ProcessMessages;

    AssertTrue('wheel over the memo scrolls the preview',
      Preview.Memo.TopPos > TopBeforeWheel);
    AssertTrue('wheel scrolling keeps the preview visible',
      Preview.Visible);
    AssertTrue('wheel scrolling must preserve Win32 focus',
      Windows.GetFocus = FocusBefore);
    AssertTrue('wheel scrolling must preserve ActiveControl',
      Host.ActiveControl = ActiveControlBefore);

    FontBeforeCtrlWheel := Preview.FontSize;
    Windows.SendMessage(Preview.Memo.Handle, WM_MOUSEWHEEL,
      Windows.MakeWParam(MK_CONTROL, Word(SmallInt(120))),
      Windows.MakeLParam(Word(MemoScreenPoint.X),
        Word(MemoScreenPoint.Y)));
    Application.ProcessMessages;

    AssertEquals('native Ctrl+wheel increases the preview font by one point',
      FontBeforeCtrlWheel + 1, Preview.FontSize);
    AssertTrue('native Ctrl+wheel keeps the preview visible',
      Preview.Visible);
    AssertTrue('native Ctrl+wheel must preserve Win32 focus',
      Windows.GetFocus = FocusBefore);
    AssertTrue('native Ctrl+wheel must preserve ActiveControl',
      Host.ActiveControl = ActiveControlBefore);
  finally
    Preview.Free;
    Host.Free;
  end;
end;
{$ENDIF}

procedure TTranslationMemoryPreviewTests.
  ChapterHoverOnlyAcceptsRegisteredOccurrenceCounter;
var
  Chapter: TChapterView;
  CountZeroLink: TKMemoHyperlink;
  Entries: TTranslationMemoryEntryArray;
  Links: TTranslationMemoryCounterLinkMatrix;
  MatchesEmptyLink: TKMemoHyperlink;
  OutsideMatrixLink: TKMemoHyperlink;
  OutOfRangeLink: TKMemoHyperlink;
  RegisteredLink: TKMemoHyperlink;
  Target: TTranslationMemoryHoverTarget;
begin
  Chapter := TChapterView.Create(nil);
  try
    SetLength(Entries, 1);
    Entries[0].DisplayTerm := 'source';
    SetLength(Entries[0].Alternatives, 3);

    Entries[0].Alternatives[0].Translation := 'valid';
    Entries[0].Alternatives[0].Occurrences := 3;
    SetLength(Entries[0].Alternatives[0].Matches, 1);
    Entries[0].Alternatives[0].Matches[0].SourcePhrase := 'source';
    Entries[0].Alternatives[0].Matches[0].TargetPhrase := 'valid';
    Entries[0].Alternatives[0].Matches[0].Occurrences := 3;

    Entries[0].Alternatives[1].Translation := 'zero';
    Entries[0].Alternatives[1].Occurrences := 0;
    SetLength(Entries[0].Alternatives[1].Matches, 1);
    Entries[0].Alternatives[1].Matches[0].SourcePhrase := 'source';
    Entries[0].Alternatives[1].Matches[0].TargetPhrase := 'zero';

    Entries[0].Alternatives[2].Translation := 'no matches';
    Entries[0].Alternatives[2].Occurrences := 2;
    SetLength(Entries[0].Alternatives[2].Matches, 0);

    RegisteredLink := Chapter.Blocks.AddHyperlink('3', '0:0');
    CountZeroLink := Chapter.Blocks.AddHyperlink('0', '0:1');
    MatchesEmptyLink := Chapter.Blocks.AddHyperlink('2', '0:2');
    OutsideMatrixLink := Chapter.Blocks.AddHyperlink('3', '0:0');
    OutOfRangeLink := Chapter.Blocks.AddHyperlink('1', '7:0');

    SetLength(Links, 1);
    SetLength(Links[0], 3);
    Links[0][0] := RegisteredLink;
    Links[0][1] := CountZeroLink;
    Links[0][2] := MatchesEmptyLink;
    Chapter.ConfigureTranslationMemoryInteractionFixture(Entries, Links);

    AssertTrue('the exact registered counter must resolve',
      Chapter.ResolveTranslationMemoryHoverTargetFixture(
        RegisteredLink, Target));
    AssertEquals(0, Target.EntryIndex);
    AssertEquals(0, Target.AlternativeIndex);

    AssertFalse('a different hyperlink with the same URL is not the counter',
      Chapter.ResolveTranslationMemoryHoverTargetFixture(
        OutsideMatrixLink, Target));
    AssertFalse('an index outside the rendered entry matrix is rejected',
      Chapter.ResolveTranslationMemoryHoverTargetFixture(
        OutOfRangeLink, Target));
    AssertFalse('a zero occurrence count is not hoverable',
      Chapter.ResolveTranslationMemoryHoverTargetFixture(
        CountZeroLink, Target));
    AssertFalse('an alternative without Matches is not hoverable',
      Chapter.ResolveTranslationMemoryHoverTargetFixture(
        MatchesEmptyLink, Target));
  finally
    Chapter.Free;
  end;
end;

procedure TTranslationMemoryPreviewTests.
  ChapterOccurrenceCounterClickCancelsHoverAndKeepsSelection;
var
  Chapter: TChapterView;
  ClickHandler: TNotifyEvent;
  Entries: TTranslationMemoryEntryArray;
  Host: TForm;
  Index: Integer;
  Limit: Integer;
  Links: TTranslationMemoryCounterLinkMatrix;
  Matches: TTranslationPairMatchArray;
  RegisteredLink: TKMemoHyperlink;
  ShowingOccurrences: Boolean;
  Target: TTranslationMemoryHoverTarget;
begin
  Host := TForm.CreateNew(nil);
  try
    Host.SetBounds(40, 40, 280, 120);
    Chapter := TChapterView.Create(Host);
    Chapter.Align := alClient;

    SetLength(Entries, 1);
    Entries[0].DisplayTerm := 'source';
    SetLength(Entries[0].Alternatives, 1);
    Entries[0].Alternatives[0].Translation := 'chosen';
    Entries[0].Alternatives[0].Occurrences := 7;
    SetLength(Entries[0].Alternatives[0].Matches, 2);
    Entries[0].Alternatives[0].Matches[0].SourcePhrase := 'source one';
    Entries[0].Alternatives[0].Matches[0].TargetPhrase := 'chosen one';
    Entries[0].Alternatives[0].Matches[0].Occurrences := 3;
    Entries[0].Alternatives[0].Matches[1].SourcePhrase := 'source two';
    Entries[0].Alternatives[0].Matches[1].TargetPhrase := 'chosen two';
    Entries[0].Alternatives[0].Matches[1].Occurrences := 4;

    RegisteredLink := Chapter.Blocks.AddHyperlink('7', '0:0');
    Chapter.Blocks.AddParagraph;
    for Index := 1 to 60 do
    begin
      Chapter.Blocks.AddTextBlock('scroll fixture ' + IntToStr(Index));
      Chapter.Blocks.AddParagraph;
    end;

    SetLength(Links, 1);
    SetLength(Links[0], 1);
    Links[0][0] := RegisteredLink;
    Chapter.ConfigureTranslationMemoryInteractionFixture(Entries, Links);

    Host.Show;
    Application.ProcessMessages;
    Chapter.TopPos := MaxInt;
    Application.ProcessMessages;
    AssertTrue('the fixture must start away from the top',
      Chapter.TopPos > 0);

    AssertTrue(Chapter.ResolveTranslationMemoryHoverTargetFixture(
      RegisteredLink, Target));
    Chapter.BeginTranslationMemoryHoverFixture(Target);
    AssertTrue('hover is active before the click',
      Chapter.TranslationMemoryPreviewActiveFixture);

    ClickHandler := RegisteredLink.OnClick;
    AssertTrue('the registered counter has its production click handler',
      Assigned(ClickHandler));
    ClickHandler(RegisteredLink);

    Chapter.ReadTranslationMemoryOccurrenceStateFixture(
      Matches, Limit, ShowingOccurrences);
    AssertFalse('click dismisses the pending preview',
      Chapter.TranslationMemoryPreviewActiveFixture);
    AssertTrue('click enters the occurrence view', ShowingOccurrences);
    AssertEquals('click restores the first-page limit', 100, Limit);
    AssertEquals('click keeps every selected Match', 2, Length(Matches));
    AssertEquals('source one', Matches[0].SourcePhrase);
    AssertEquals('chosen one', Matches[0].TargetPhrase);
    AssertEquals(3, Matches[0].Occurrences);
    AssertEquals('source two', Matches[1].SourcePhrase);
    AssertEquals('chosen two', Matches[1].TargetPhrase);
    AssertEquals(4, Matches[1].Occurrences);
    AssertEquals('click returns occurrence details to the top',
      0, Chapter.TopPos);
  finally
    Host.Free;
  end;
end;

initialization
  RegisterTest(TTranslationMemoryPreviewTests);

end.
