unit translation_memory_preview;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, ExtCtrls, StdCtrls,
  IniFiles, LCLType, LMessages, KMemo, SuggestionManager;

const
  TRANSLATION_MEMORY_HOVER_DELAY_MS = 400;
  TRANSLATION_MEMORY_HOVER_RECHECK_MS = 100;
  TRANSLATION_MEMORY_HOVER_MARGIN_PX = 8;
  TRANSLATION_MEMORY_PREVIEW_LIMIT = 10;
  TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE = 6;
  TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE = 72;
  TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION = 'leiaute';
  TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY =
    'principal.translation.memory.preview.font.size';

type
  TTranslationMemoryHoverTarget = record
    Generation: QWord;
    EntryIndex: Integer;
    AlternativeIndex: Integer;
  end;

  TTranslationMemoryPreviewPayload = record
    SourceTerm: string;
    Translation: string;
    TitleText: string;
    TotalsText: string;
    NoOccurrencesText: string;
    OccurrenceCountFormat: string;
    FooterText: string;
    CurrentReferenceID: string;
    FontName: string;
    FontSize: Integer;
    Page: TTranslationOccurrencePage;
    ErrorMessage: string;
  end;

  TTranslationMemoryValidateTargetEvent = function(
    const ATarget: TTranslationMemoryHoverTarget;
    out AAnchorScreenRect: TRect): Boolean of object;
  TTranslationMemoryLoadPayloadEvent = function(
    const ATarget: TTranslationMemoryHoverTarget;
    out APayload: TTranslationMemoryPreviewPayload): Boolean of object;
  TTranslationMemoryPreviewFontSizeChangedEvent = procedure(Sender: TObject;
    AFontSize: Integer) of object;

  { TTranslationMemoryPreviewWindow }

  TTranslationMemoryPreviewWindow = class(THintWindowRendered)
  private
    FAnchorScreenRect: TRect;
    FHasPayload: Boolean;
    FMemo: TKMemo;
    FOnFontSizeChanged: TTranslationMemoryPreviewFontSizeChangedEvent;
    FPayload: TTranslationMemoryPreviewPayload;
    procedure ApplyPayloadLayout(AResetScroll: Boolean);
    function GetFontSize: Integer;
    procedure HandleMemoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RenderPayload(const APayload: TTranslationMemoryPreviewPayload);
  protected
    procedure WMNCHitTest(var Message: TLMessage); message LM_NCHITTEST;
    {$IFDEF WINDOWS}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowPayload(const APayload: TTranslationMemoryPreviewPayload;
      const AAnchorScreenRect: TRect); virtual;
    procedure HidePreview; virtual;
    property FontSize: Integer read GetFontSize;
    property Memo: TKMemo read FMemo;
    property OnFontSizeChanged: TTranslationMemoryPreviewFontSizeChangedEvent
      read FOnFontSizeChanged write FOnFontSizeChanged;
  end;

  { TTranslationMemoryHoverController }

  TTranslationMemoryHoverController = class(TComponent)
  private
    FCachePayload: TTranslationMemoryPreviewPayload;
    FCacheTarget: TTranslationMemoryHoverTarget;
    FCacheValid: Boolean;
    FApplicationKeyHandlerRegistered: Boolean;
    FHasPendingTarget: Boolean;
    FOnFontSizeChanged: TTranslationMemoryPreviewFontSizeChangedEvent;
    FOnLoadPayload: TTranslationMemoryLoadPayloadEvent;
    FOnValidateTarget: TTranslationMemoryValidateTargetEvent;
    FPendingTarget: TTranslationMemoryHoverTarget;
    FPreview: TTranslationMemoryPreviewWindow;
    FPreviewAnchorScreenRect: TRect;
    FTimer: TTimer;
    procedure ClearCache;
    function GetActive: Boolean;
    function GetPreviewVisible: Boolean;
    function GetTimerEnabled: Boolean;
    function GetTimerInterval: Integer;
    procedure HandlePreviewFontSizeChanged(Sender: TObject;
      AFontSize: Integer);
    procedure Schedule(AInterval: Integer);
    function TryValidateTarget(const ATarget: TTranslationMemoryHoverTarget;
      out AAnchorScreenRect: TRect): Boolean;
  protected
    function ApplicationIsActive: Boolean; virtual;
    function CreatePreviewWindow: TTranslationMemoryPreviewWindow; virtual;
    function CurrentCursorScreenPoint: TPoint; virtual;
    procedure HandleApplicationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure TimerElapsed(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Dismiss;
    procedure Hover(const ATarget: TTranslationMemoryHoverTarget);
    procedure Invalidate;
    procedure Leave;
    property Active: Boolean read GetActive;
    property CacheValid: Boolean read FCacheValid;
    property CachedTarget: TTranslationMemoryHoverTarget read FCacheTarget;
    property HasPendingTarget: Boolean read FHasPendingTarget;
    property PendingTarget: TTranslationMemoryHoverTarget read FPendingTarget;
    property PreviewAnchorScreenRect: TRect read FPreviewAnchorScreenRect;
    property PreviewVisible: Boolean read GetPreviewVisible;
    property TimerEnabled: Boolean read GetTimerEnabled;
    property TimerInterval: Integer read GetTimerInterval;
    property OnFontSizeChanged: TTranslationMemoryPreviewFontSizeChangedEvent
      read FOnFontSizeChanged write FOnFontSizeChanged;
    property OnLoadPayload: TTranslationMemoryLoadPayloadEvent
      read FOnLoadPayload write FOnLoadPayload;
    property OnValidateTarget: TTranslationMemoryValidateTargetEvent
      read FOnValidateTarget write FOnValidateTarget;
  end;

procedure AppendTranslationMemoryOccurrenceText(ABlocks: TKMemoBlocks;
  const Occurrence: TTranslationOccurrenceData);
function NormalizeTranslationMemoryPreviewFontSize(AFontSize: Integer): Integer;
function ReadTranslationMemoryPreviewFontSize(AIni: TCustomIniFile;
  ADefaultFontSize: Integer): Integer;
procedure WriteTranslationMemoryPreviewFontSize(AIni: TCustomIniFile;
  AFontSize: Integer);
function TryParseTranslationMemoryHoverURL(const AURL: string;
  out AEntryIndex, AAlternativeIndex: Integer): Boolean;

implementation

uses
  Math
  {$IFDEF WINDOWS}, Windows{$ENDIF};

const
  PREVIEW_WIDTH_LOGICAL_PX = 560;
  PREVIEW_MIN_HEIGHT_LOGICAL_PX = 140;
  PREVIEW_MAX_HEIGHT_LOGICAL_PX = 360;
  PREVIEW_GAP_LOGICAL_PX = 4;
  PREVIEW_CONTENT_PADDING_LOGICAL_PX = 12;
  PreviewTitleColor = $00BE7C41;
  PreviewCurrentReferenceColor = $002CBA7D;

function NormalizeTranslationMemoryPreviewFontSize(AFontSize: Integer): Integer;
begin
  Result := EnsureRange(AFontSize, TRANSLATION_MEMORY_PREVIEW_MIN_FONT_SIZE,
    TRANSLATION_MEMORY_PREVIEW_MAX_FONT_SIZE);
end;

function ReadTranslationMemoryPreviewFontSize(AIni: TCustomIniFile;
  ADefaultFontSize: Integer): Integer;
begin
  if Assigned(AIni) then
    try
      Result := AIni.ReadInteger(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
        TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY, ADefaultFontSize);
    except
      Result := ADefaultFontSize;
    end
  else
    Result := ADefaultFontSize;
  Result := NormalizeTranslationMemoryPreviewFontSize(Result);
end;

procedure WriteTranslationMemoryPreviewFontSize(AIni: TCustomIniFile;
  AFontSize: Integer);
begin
  if Assigned(AIni) then
    AIni.WriteInteger(TRANSLATION_MEMORY_PREVIEW_SETTINGS_SECTION,
      TRANSLATION_MEMORY_PREVIEW_FONT_SIZE_SETTING_KEY,
      NormalizeTranslationMemoryPreviewFontSize(AFontSize));
end;

function HoverTargetsEqual(const ALeft,
  ARight: TTranslationMemoryHoverTarget): Boolean;
begin
  Result := (ALeft.Generation = ARight.Generation) and
    (ALeft.EntryIndex = ARight.EntryIndex) and
    (ALeft.AlternativeIndex = ARight.AlternativeIndex);
end;

function PointInExpandedRect(const APoint: TPoint; const ARect: TRect;
  AMargin: Integer): Boolean;
var
  ExpandedRect: TRect;
begin
  ExpandedRect := ARect;
  Dec(ExpandedRect.Left, AMargin);
  Dec(ExpandedRect.Top, AMargin);
  Inc(ExpandedRect.Right, AMargin);
  Inc(ExpandedRect.Bottom, AMargin);
  Result := (APoint.X >= ExpandedRect.Left) and
    (APoint.X < ExpandedRect.Right) and
    (APoint.Y >= ExpandedRect.Top) and
    (APoint.Y < ExpandedRect.Bottom);
end;

function PointInRectExact(const APoint: TPoint; const ARect: TRect): Boolean;
begin
  Result := (APoint.X >= ARect.Left) and (APoint.X < ARect.Right) and
    (APoint.Y >= ARect.Top) and (APoint.Y < ARect.Bottom);
end;

function RectsEqual(const ALeft, ARight: TRect): Boolean;
begin
  Result := (ALeft.Left = ARight.Left) and (ALeft.Top = ARight.Top) and
    (ALeft.Right = ARight.Right) and (ALeft.Bottom = ARight.Bottom);
end;

function TryParseTranslationMemoryHoverURL(const AURL: string;
  out AEntryIndex, AAlternativeIndex: Integer): Boolean;
var
  CharacterIndex: Integer;
  SeparatorIndex: Integer;
begin
  AEntryIndex := -1;
  AAlternativeIndex := -1;
  SeparatorIndex := Pos(':', AURL);
  if (SeparatorIndex <= 1) or (SeparatorIndex >= Length(AURL)) or
     (Pos(':', Copy(AURL, SeparatorIndex + 1, MaxInt)) > 0) then
    Exit(False);
  for CharacterIndex := 1 to Length(AURL) do
    if (CharacterIndex <> SeparatorIndex) and
       not (AURL[CharacterIndex] in ['0'..'9']) then
      Exit(False);
  Result := TryStrToInt(Copy(AURL, 1, SeparatorIndex - 1), AEntryIndex) and
    TryStrToInt(Copy(AURL, SeparatorIndex + 1, MaxInt), AAlternativeIndex) and
    (AEntryIndex >= 0) and (AAlternativeIndex >= 0);
  if not Result then
  begin
    AEntryIndex := -1;
    AAlternativeIndex := -1;
  end;
end;

procedure AppendTranslationMemoryOccurrenceText(ABlocks: TKMemoBlocks;
  const Occurrence: TTranslationOccurrenceData);
var
  Index: Integer;
  TextBlock: TKMemoTextBlock;
  TextCursor: Integer;
begin
  if not Assigned(ABlocks) or (Occurrence.DestinationText = '') then
    Exit;

  ABlocks.AddTextBlock(' — ');
  if Length(Occurrence.HighlightRanges) = 0 then
  begin
    ABlocks.AddTextBlock(Occurrence.DestinationText);
    Exit;
  end;

  for Index := 0 to High(Occurrence.HighlightRanges) do
    if (Occurrence.HighlightRanges[Index].StartIndex < 1) or
       (Occurrence.HighlightRanges[Index].Length <= 0) or
       (Occurrence.HighlightRanges[Index].StartIndex >
        Length(Occurrence.DestinationText)) or
       (Occurrence.HighlightRanges[Index].Length >
        Length(Occurrence.DestinationText) -
        Occurrence.HighlightRanges[Index].StartIndex + 1) or
       ((Index > 0) and
        (Occurrence.HighlightRanges[Index].StartIndex <
         Occurrence.HighlightRanges[Index - 1].StartIndex +
         Occurrence.HighlightRanges[Index - 1].Length)) then
    begin
      ABlocks.AddTextBlock(Occurrence.DestinationText);
      Exit;
    end;

  TextCursor := 1;
  for Index := 0 to High(Occurrence.HighlightRanges) do
  begin
    if Occurrence.HighlightRanges[Index].StartIndex > TextCursor then
      ABlocks.AddTextBlock(Copy(Occurrence.DestinationText, TextCursor,
        Occurrence.HighlightRanges[Index].StartIndex - TextCursor));
    TextBlock := ABlocks.AddTextBlock(Copy(Occurrence.DestinationText,
      Occurrence.HighlightRanges[Index].StartIndex,
      Occurrence.HighlightRanges[Index].Length));
    TextBlock.TextStyle.Font.Style := [fsBold, fsUnderline];
    TextCursor := Occurrence.HighlightRanges[Index].StartIndex +
      Occurrence.HighlightRanges[Index].Length;
  end;
  if TextCursor <= Length(Occurrence.DestinationText) then
    ABlocks.AddTextBlock(Copy(Occurrence.DestinationText, TextCursor,
      Length(Occurrence.DestinationText) - TextCursor + 1));
end;

{ TTranslationMemoryPreviewWindow }

constructor TTranslationMemoryPreviewWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  ShowInTaskBar := stNever;
  Position := poDesigned;
  Color := clInfoBk;
  Width := Scale96ToForm(PREVIEW_WIDTH_LOGICAL_PX);
  Height := Scale96ToForm(PREVIEW_MIN_HEIGHT_LOGICAL_PX);

  FMemo := TKMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alClient;
  FMemo.ReadOnly := True;
  FMemo.TabStop := False;
  FMemo.BorderStyle := bsSingle;
  FMemo.Background.Color := clInfoBk;
  FMemo.ScrollBars := ssVertical;
  FMemo.OnMouseWheel := @HandleMemoMouseWheel;
end;

function TTranslationMemoryPreviewWindow.GetFontSize: Integer;
begin
  if FHasPayload then
    Result := FPayload.FontSize
  else
    Result := NormalizeTranslationMemoryPreviewFontSize(
      FMemo.TextStyle.Font.Size);
end;

procedure TTranslationMemoryPreviewWindow.HandleMemoMouseWheel(
  Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
var
  NewFontSize: Integer;
begin
  if not (ssCtrl in Shift) then
    Exit;

  Handled := True;
  if (WheelDelta = 0) or not FHasPayload or not Visible then
    Exit;

  if WheelDelta > 0 then
    NewFontSize := FPayload.FontSize + 1
  else
    NewFontSize := FPayload.FontSize - 1;
  NewFontSize := NormalizeTranslationMemoryPreviewFontSize(NewFontSize);
  if NewFontSize = FPayload.FontSize then
    Exit;

  FPayload.FontSize := NewFontSize;
  ApplyPayloadLayout(False);
  if Assigned(FOnFontSizeChanged) then
    FOnFontSizeChanged(Self, NewFontSize);
end;

procedure TTranslationMemoryPreviewWindow.WMNCHitTest(var Message: TLMessage);
begin
  Message.Result := HTCLIENT;
end;

{$IFDEF WINDOWS}
procedure TTranslationMemoryPreviewWindow.CreateParams(
  var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW;
end;
{$ENDIF}

procedure TTranslationMemoryPreviewWindow.RenderPayload(
  const APayload: TTranslationMemoryPreviewPayload);
var
  ItemIndex: Integer;
  ItemLimit: Integer;
  ReferenceText: string;
  TextBlock: TKMemoTextBlock;
begin
  FMemo.Blocks.LockUpdate;
  try
    FMemo.Blocks.Clear;
    if APayload.FontName <> '' then
      FMemo.TextStyle.Font.Name := APayload.FontName
    else
      FMemo.TextStyle.Font.Name := 'default';
    FMemo.TextStyle.Font.Size := Max(1, APayload.FontSize);

    TextBlock := FMemo.Blocks.AddTextBlock(APayload.TitleText);
    TextBlock.TextStyle.Font.Size := FMemo.TextStyle.Font.Size + 2;
    TextBlock.TextStyle.Font.Bold := True;
    TextBlock.TextStyle.Font.Color := PreviewTitleColor;
    with FMemo.Blocks.AddParagraph().ParaStyle do
    begin
      FirstIndent := 0;
      BottomPadding := 6;
    end;

    if APayload.TotalsText <> '' then
    begin
      TextBlock := FMemo.Blocks.AddTextBlock(APayload.TotalsText);
      TextBlock.TextStyle.Font.Color := clGray;
      with FMemo.Blocks.AddParagraph().ParaStyle do
      begin
        FirstIndent := 0;
        BottomPadding := 8;
      end;
    end;

    if APayload.ErrorMessage <> '' then
    begin
      TextBlock := FMemo.Blocks.AddTextBlock(APayload.ErrorMessage);
      TextBlock.TextStyle.Font.Color := clRed;
      FMemo.Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
    end
    else if Length(APayload.Page.Items) = 0 then
    begin
      TextBlock := FMemo.Blocks.AddTextBlock(APayload.NoOccurrencesText);
      TextBlock.TextStyle.Font.Italic := True;
      TextBlock.TextStyle.Font.Color := clGray;
      FMemo.Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
    end
    else
    begin
      ItemLimit := Min(Length(APayload.Page.Items),
        TRANSLATION_MEMORY_PREVIEW_LIMIT);
      for ItemIndex := 0 to ItemLimit - 1 do
        with APayload.Page.Items[ItemIndex] do
        begin
          ReferenceText := APayload.Page.Items[ItemIndex].DisplayReference;
          if ReferenceText = '' then
            ReferenceText := ReferenceID;
          TextBlock := FMemo.Blocks.AddTextBlock(ReferenceText);
          TextBlock.TextStyle.Font.Bold := True;
          TextBlock.TextStyle.Font.Color := clNavy;
          if ReferenceID = APayload.CurrentReferenceID then
            TextBlock.TextStyle.Font.Color := PreviewCurrentReferenceColor;
          if (Occurrences > 1) and
             (APayload.OccurrenceCountFormat <> '') then
          begin
            TextBlock := FMemo.Blocks.AddTextBlock(' (' + Format(
              APayload.OccurrenceCountFormat, [Occurrences]) + ')');
            TextBlock.TextStyle.Font.Color := clGray;
          end;
          AppendTranslationMemoryOccurrenceText(FMemo.Blocks,
            APayload.Page.Items[ItemIndex]);
          with FMemo.Blocks.AddParagraph().ParaStyle do
          begin
            FirstIndent := 0;
            BottomPadding := 4;
          end;
        end;
    end;

    if APayload.FooterText <> '' then
    begin
      TextBlock := FMemo.Blocks.AddTextBlock(APayload.FooterText);
      TextBlock.TextStyle.Font.Size := Max(1, FMemo.TextStyle.Font.Size - 1);
      TextBlock.TextStyle.Font.Italic := True;
      TextBlock.TextStyle.Font.Color := clGray;
      with FMemo.Blocks.AddParagraph().ParaStyle do
      begin
        FirstIndent := 0;
        TopPadding := 6;
      end;
    end;
  finally
    FMemo.Blocks.UnlockUpdate;
  end;
end;

procedure TTranslationMemoryPreviewWindow.ApplyPayloadLayout(
  AResetScroll: Boolean);
var
  DesiredHeight: Integer;
  EffectiveWidth: Integer;
  Gap: Integer;
  MaximumHeight: Integer;
  MinimumHeight: Integer;
  PopupLeft: Integer;
  PopupRect: TRect;
  PopupTop: Integer;
  TargetMonitor: TMonitor;
  TargetPixelsPerInch: Integer;
  WorkArea: TRect;
  WorkHeight: Integer;
  WorkWidth: Integer;
  SavedTopPos: Integer;
  WasVisible: Boolean;
begin
  SavedTopPos := FMemo.TopPos;
  WasVisible := Visible;
  TargetMonitor := Screen.MonitorFromRect(FAnchorScreenRect);
  if Assigned(TargetMonitor) then
  begin
    WorkArea := TargetMonitor.WorkareaRect;
    TargetPixelsPerInch := TargetMonitor.PixelsPerInch;
  end
  else
  begin
    WorkArea := Screen.WorkAreaRect;
    TargetPixelsPerInch := Screen.PixelsPerInch;
  end;
  if TargetPixelsPerInch <= 0 then
    TargetPixelsPerInch := Screen.PixelsPerInch;
  if TargetPixelsPerInch <= 0 then
    TargetPixelsPerInch := 96;
  PixelsPerInch := TargetPixelsPerInch;

  WorkWidth := Max(1, WorkArea.Right - WorkArea.Left);
  WorkHeight := Max(1, WorkArea.Bottom - WorkArea.Top);
  EffectiveWidth := Max(1, Min(Scale96ToForm(PREVIEW_WIDTH_LOGICAL_PX),
    WorkWidth));
  MinimumHeight := Max(1, Min(
    Scale96ToForm(PREVIEW_MIN_HEIGHT_LOGICAL_PX), WorkHeight));
  MaximumHeight := Max(1, Min(
    Scale96ToForm(PREVIEW_MAX_HEIGHT_LOGICAL_PX), WorkHeight));

  Width := EffectiveWidth;
  Height := MaximumHeight;
  RenderPayload(FPayload);
  DesiredHeight := EnsureRange(
    FMemo.ContentHeight + Scale96ToForm(PREVIEW_CONTENT_PADDING_LOGICAL_PX),
    MinimumHeight, MaximumHeight);
  Height := DesiredHeight;

  Gap := Scale96ToForm(PREVIEW_GAP_LOGICAL_PX);
  PopupLeft := EnsureRange(FAnchorScreenRect.Left, WorkArea.Left,
    Max(WorkArea.Left, WorkArea.Right - Width));
  PopupTop := FAnchorScreenRect.Bottom + Gap;
  if PopupTop + Height > WorkArea.Bottom then
    PopupTop := FAnchorScreenRect.Top - Height - Gap;
  PopupTop := EnsureRange(PopupTop, WorkArea.Top,
    Max(WorkArea.Top, WorkArea.Bottom - Height));
  PopupRect := Types.Bounds(PopupLeft, PopupTop, Width, Height);
  HintRect := PopupRect;
  SetBounds(PopupRect.Left, PopupRect.Top,
    PopupRect.Right - PopupRect.Left, PopupRect.Bottom - PopupRect.Top);
  if AResetScroll then
    FMemo.TopPos := 0
  else
    FMemo.TopPos := SavedTopPos;
  if WasVisible then
  begin
    FMemo.Invalidate;
    Invalidate;
  end
  else
    ActivateRendered;
end;

procedure TTranslationMemoryPreviewWindow.ShowPayload(
  const APayload: TTranslationMemoryPreviewPayload;
  const AAnchorScreenRect: TRect);
begin
  FPayload := APayload;
  FPayload.FontSize := NormalizeTranslationMemoryPreviewFontSize(
    APayload.FontSize);
  FAnchorScreenRect := AAnchorScreenRect;
  FHasPayload := True;
  ApplyPayloadLayout(True);
end;

procedure TTranslationMemoryPreviewWindow.HidePreview;
begin
  Hide;
  FHasPayload := False;
  FPayload := Default(TTranslationMemoryPreviewPayload);
  FAnchorScreenRect := Types.Rect(0, 0, 0, 0);
end;

{ TTranslationMemoryHoverController }

constructor TTranslationMemoryHoverController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := TRANSLATION_MEMORY_HOVER_DELAY_MS;
  FTimer.OnTimer := @TimerElapsed;
  if Assigned(Application) then
  begin
    Application.AddOnKeyDownBeforeHandler(@HandleApplicationKeyDown);
    FApplicationKeyHandlerRegistered := True;
  end;
end;

destructor TTranslationMemoryHoverController.Destroy;
begin
  if FApplicationKeyHandlerRegistered and Assigned(Application) then
  begin
    Application.RemoveOnKeyDownBeforeHandler(@HandleApplicationKeyDown);
    FApplicationKeyHandlerRegistered := False;
  end;
  Dismiss;
  ClearCache;
  inherited Destroy;
end;

function TTranslationMemoryHoverController.ApplicationIsActive: Boolean;
begin
  Result := Assigned(Application) and Application.Active;
end;

procedure TTranslationMemoryHoverController.ClearCache;
begin
  FCacheValid := False;
  FCacheTarget := Default(TTranslationMemoryHoverTarget);
  FCachePayload := Default(TTranslationMemoryPreviewPayload);
end;

function TTranslationMemoryHoverController.CreatePreviewWindow:
  TTranslationMemoryPreviewWindow;
begin
  Result := TTranslationMemoryPreviewWindow.Create(nil);
end;

function TTranslationMemoryHoverController.CurrentCursorScreenPoint: TPoint;
begin
  Result := Mouse.CursorPos;
end;

procedure TTranslationMemoryHoverController.Dismiss;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
    FTimer.Interval := TRANSLATION_MEMORY_HOVER_DELAY_MS;
  end;
  FHasPendingTarget := False;
  FPendingTarget := Default(TTranslationMemoryHoverTarget);
  FPreviewAnchorScreenRect := Types.Rect(0, 0, 0, 0);
  FreeAndNil(FPreview);
end;

function TTranslationMemoryHoverController.GetActive: Boolean;
begin
  Result := Assigned(FPreview) or GetTimerEnabled or FHasPendingTarget;
end;

function TTranslationMemoryHoverController.GetPreviewVisible: Boolean;
begin
  Result := Assigned(FPreview) and FPreview.Visible;
end;

function TTranslationMemoryHoverController.GetTimerEnabled: Boolean;
begin
  Result := Assigned(FTimer) and FTimer.Enabled;
end;

function TTranslationMemoryHoverController.GetTimerInterval: Integer;
begin
  if Assigned(FTimer) then
    Result := FTimer.Interval
  else
    Result := 0;
end;

procedure TTranslationMemoryHoverController.HandlePreviewFontSizeChanged(
  Sender: TObject; AFontSize: Integer);
begin
  AFontSize := NormalizeTranslationMemoryPreviewFontSize(AFontSize);
  if FCacheValid then
    FCachePayload.FontSize := AFontSize;
  if Assigned(FOnFontSizeChanged) then
    FOnFontSizeChanged(Self, AFontSize);
end;

procedure TTranslationMemoryHoverController.HandleApplicationKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and Active then
    Dismiss;
end;

procedure TTranslationMemoryHoverController.Hover(
  const ATarget: TTranslationMemoryHoverTarget);
begin
  if (ATarget.EntryIndex < 0) or (ATarget.AlternativeIndex < 0) then
  begin
    Leave;
    Exit;
  end;
  if FHasPendingTarget and HoverTargetsEqual(FPendingTarget, ATarget) and
     (GetTimerEnabled or Assigned(FPreview)) then
    Exit;

  Dismiss;
  FPendingTarget := ATarget;
  FHasPendingTarget := True;
  Schedule(TRANSLATION_MEMORY_HOVER_DELAY_MS);
end;

procedure TTranslationMemoryHoverController.Invalidate;
begin
  Dismiss;
  ClearCache;
end;

procedure TTranslationMemoryHoverController.Leave;
begin
  if Assigned(FPreview) and FHasPendingTarget then
    Schedule(TRANSLATION_MEMORY_HOVER_RECHECK_MS)
  else
    Dismiss;
end;

procedure TTranslationMemoryHoverController.Schedule(AInterval: Integer);
begin
  FTimer.Enabled := False;
  FTimer.Interval := AInterval;
  FTimer.Enabled := True;
end;

procedure TTranslationMemoryHoverController.TimerElapsed(Sender: TObject);
var
  AnchorScreenRect: TRect;
  CursorPoint: TPoint;
  LoadedFromCache: Boolean;
  Payload: TTranslationMemoryPreviewPayload;
  PreviewRect: TRect;
  Target: TTranslationMemoryHoverTarget;
begin
  FTimer.Enabled := False;
  if not FHasPendingTarget or not ApplicationIsActive or
     not Assigned(FOnValidateTarget) then
  begin
    Dismiss;
    Exit;
  end;

  Target := FPendingTarget;
  if not TryValidateTarget(Target, AnchorScreenRect) then
  begin
    Dismiss;
    Exit;
  end;
  CursorPoint := CurrentCursorScreenPoint;

  if Assigned(FPreview) then
  begin
    if not RectsEqual(AnchorScreenRect, FPreviewAnchorScreenRect) then
    begin
      Dismiss;
      Exit;
    end;
    PreviewRect := Types.Rect(FPreview.Left, FPreview.Top,
      FPreview.Left + FPreview.Width, FPreview.Top + FPreview.Height);
    if PointInExpandedRect(CursorPoint, FPreviewAnchorScreenRect,
         TRANSLATION_MEMORY_HOVER_MARGIN_PX) or
       PointInExpandedRect(CursorPoint, PreviewRect,
         TRANSLATION_MEMORY_HOVER_MARGIN_PX) then
      Schedule(TRANSLATION_MEMORY_HOVER_RECHECK_MS)
    else
      Dismiss;
    Exit;
  end;

  if not PointInRectExact(CursorPoint, AnchorScreenRect) then
  begin
    Dismiss;
    Exit;
  end;

  LoadedFromCache := FCacheValid and HoverTargetsEqual(FCacheTarget, Target);
  if LoadedFromCache then
    Payload := FCachePayload
  else
  begin
    Payload := Default(TTranslationMemoryPreviewPayload);
    if not Assigned(FOnLoadPayload) or
       not FOnLoadPayload(Target, Payload) then
    begin
      Dismiss;
      Exit;
    end;
  end;

  if not FHasPendingTarget or
     not HoverTargetsEqual(FPendingTarget, Target) or
     not ApplicationIsActive or
     not TryValidateTarget(Target, AnchorScreenRect) then
  begin
    Dismiss;
    Exit;
  end;
  CursorPoint := CurrentCursorScreenPoint;
  if not PointInRectExact(CursorPoint, AnchorScreenRect) then
  begin
    Dismiss;
    Exit;
  end;

  if not LoadedFromCache and (Payload.ErrorMessage = '') then
  begin
    FCachePayload := Payload;
    FCacheTarget := Target;
    FCacheValid := True;
  end;

  FPreview := CreatePreviewWindow;
  if not Assigned(FPreview) then
  begin
    Dismiss;
    Exit;
  end;
  FPreviewAnchorScreenRect := AnchorScreenRect;
  FPreview.OnFontSizeChanged := @HandlePreviewFontSizeChanged;
  FPreview.ShowPayload(Payload, FPreviewAnchorScreenRect);
  Schedule(TRANSLATION_MEMORY_HOVER_RECHECK_MS);
end;

function TTranslationMemoryHoverController.TryValidateTarget(
  const ATarget: TTranslationMemoryHoverTarget;
  out AAnchorScreenRect: TRect): Boolean;
begin
  AAnchorScreenRect := Types.Rect(0, 0, 0, 0);
  Result := Assigned(FOnValidateTarget) and
    FOnValidateTarget(ATarget, AAnchorScreenRect);
end;

end.
