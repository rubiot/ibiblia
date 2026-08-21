unit win_directwrite_d2d;

{$mode objfpc}{$H+}
{$interfaces COM}
{$packrecords C}
{$packenum 4}

{$IFNDEF MSWINDOWS}
  {$ERROR win_directwrite_d2d is available only on Windows}
{$ENDIF}

interface

uses
  Windows, SysUtils;

type
  TDWriteUInt32 = Cardinal;
  PDWriteUInt32 = ^TDWriteUInt32;
  TDWriteBool = LongBool;
  PDWriteBool = ^TDWriteBool;
  TDWriteFloat = Single;
  PDWriteFloat = ^TDWriteFloat;
  TD2DTag = QWord;
  PD2DTag = ^TD2DTag;

  TDWriteTextRange = record
    StartPosition: TDWriteUInt32;
    Length: TDWriteUInt32;
  end;
  PDWriteTextRange = ^TDWriteTextRange;

  TDWriteTextMetrics = record
    Left: TDWriteFloat;
    Top: TDWriteFloat;
    Width: TDWriteFloat;
    WidthIncludingTrailingWhitespace: TDWriteFloat;
    Height: TDWriteFloat;
    LayoutWidth: TDWriteFloat;
    LayoutHeight: TDWriteFloat;
    MaxBidiReorderingDepth: TDWriteUInt32;
    LineCount: TDWriteUInt32;
  end;
  PDWriteTextMetrics = ^TDWriteTextMetrics;

  TDWriteHitTestMetrics = record
    TextPosition: TDWriteUInt32;
    Length: TDWriteUInt32;
    Left: TDWriteFloat;
    Top: TDWriteFloat;
    Width: TDWriteFloat;
    Height: TDWriteFloat;
    BidiLevel: TDWriteUInt32;
    IsText: TDWriteBool;
    IsTrimmed: TDWriteBool;
  end;
  PDWriteHitTestMetrics = ^TDWriteHitTestMetrics;

  TDWriteMatrix = record
    M11: TDWriteFloat;
    M12: TDWriteFloat;
    M21: TDWriteFloat;
    M22: TDWriteFloat;
    DX: TDWriteFloat;
    DY: TDWriteFloat;
  end;
  PDWriteMatrix = ^TDWriteMatrix;

  TDWriteGlyphOffset = record
    AdvanceOffset: TDWriteFloat;
    AscenderOffset: TDWriteFloat;
  end;
  PDWriteGlyphOffset = ^TDWriteGlyphOffset;

  TDWriteGlyphRun = record
    FontFace: Pointer;
    FontEmSize: TDWriteFloat;
    GlyphCount: TDWriteUInt32;
    GlyphIndices: PWord;
    GlyphAdvances: PDWriteFloat;
    GlyphOffsets: PDWriteGlyphOffset;
    IsSideways: TDWriteBool;
    BidiLevel: TDWriteUInt32;
  end;
  PDWriteGlyphRun = ^TDWriteGlyphRun;

  TDWriteGlyphRunDescription = record
    LocaleName: PWideChar;
    Text: PWideChar;
    StringLength: TDWriteUInt32;
    ClusterMap: PWord;
    TextPosition: TDWriteUInt32;
  end;
  PDWriteGlyphRunDescription = ^TDWriteGlyphRunDescription;

  TDWriteUnderline = record
    Width: TDWriteFloat;
    Thickness: TDWriteFloat;
    Offset: TDWriteFloat;
    RunHeight: TDWriteFloat;
    ReadingDirection: LongInt;
    FlowDirection: LongInt;
    LocaleName: PWideChar;
    MeasuringMode: LongInt;
  end;
  PDWriteUnderline = ^TDWriteUnderline;

  TDWriteStrikethrough = record
    Width: TDWriteFloat;
    Thickness: TDWriteFloat;
    Offset: TDWriteFloat;
    ReadingDirection: LongInt;
    FlowDirection: LongInt;
    LocaleName: PWideChar;
    MeasuringMode: LongInt;
  end;
  PDWriteStrikethrough = ^TDWriteStrikethrough;

  { DWRITE_CLUSTER_METRICS uses two packed 16-bit bit-field values after
    Length. Keeping the flags opaque preserves the native ABI while exposing
    the cluster length needed for legal caret stops. }
  TDWriteClusterMetrics = record
    Width: TDWriteFloat;
    Length: Word;
    Flags: Word;
  end;
  PDWriteClusterMetrics = ^TDWriteClusterMetrics;

  TDWriteLineMetrics = record
    Length: TDWriteUInt32;
    TrailingWhitespaceLength: TDWriteUInt32;
    NewlineLength: TDWriteUInt32;
    Height: TDWriteFloat;
    Baseline: TDWriteFloat;
    IsTrimmed: TDWriteBool;
  end;
  PDWriteLineMetrics = ^TDWriteLineMetrics;

  TDWriteInlineObjectMetrics = record
    Width: TDWriteFloat;
    Height: TDWriteFloat;
    Baseline: TDWriteFloat;
    SupportsSideways: TDWriteBool;
  end;

  TDWriteOverhangMetrics = record
    Left: TDWriteFloat;
    Top: TDWriteFloat;
    Right: TDWriteFloat;
    Bottom: TDWriteFloat;
  end;

  TD2DPoint2F = record
    X: Single;
    Y: Single;
  end;

  TD2DRectF = record
    Left: Single;
    Top: Single;
    Right: Single;
    Bottom: Single;
  end;
  PD2DRectF = ^TD2DRectF;

  TD2DColorF = record
    R: Single;
    G: Single;
    B: Single;
    A: Single;
  end;
  PD2DColorF = ^TD2DColorF;

  TD2DPixelFormat = record
    Format: LongInt;
    AlphaMode: LongInt;
  end;

  TD2DRenderTargetProperties = record
    TargetType: LongInt;
    PixelFormat: TD2DPixelFormat;
    DpiX: Single;
    DpiY: Single;
    Usage: LongInt;
    MinLevel: LongInt;
  end;
  PD2DRenderTargetProperties = ^TD2DRenderTargetProperties;

const
  DWRITE_FACTORY_TYPE_SHARED = 0;
  DWRITE_FONT_WEIGHT_NORMAL = 400;
  DWRITE_FONT_STYLE_NORMAL = 0;
  DWRITE_FONT_STRETCH_NORMAL = 5;
  DWRITE_TEXT_ALIGNMENT_LEADING = 0;
  DWRITE_PARAGRAPH_ALIGNMENT_NEAR = 0;
  DWRITE_WORD_WRAPPING_WRAP = 0;
  DWRITE_WORD_WRAPPING_NO_WRAP = 1;
  DWRITE_READING_DIRECTION_LEFT_TO_RIGHT = 0;
  DWRITE_READING_DIRECTION_RIGHT_TO_LEFT = 1;
  DWRITE_FLOW_DIRECTION_TOP_TO_BOTTOM = 0;
  DWRITE_LINE_SPACING_METHOD_UNIFORM = 1;
  DWRITE_BREAK_CONDITION_NEUTRAL = 0;

  D2D1_FACTORY_TYPE_SINGLE_THREADED = 0;
  D2D1_RENDER_TARGET_TYPE_DEFAULT = 0;
  DXGI_FORMAT_B8G8R8A8_UNORM = 87;
  D2D1_ALPHA_MODE_IGNORE = 3;
  D2D1_RENDER_TARGET_USAGE_NONE = 0;
  D2D1_FEATURE_LEVEL_DEFAULT = 0;
  D2D1_DRAW_TEXT_OPTIONS_NONE = 0;
  D2D1_DRAW_TEXT_OPTIONS_CLIP = 2;

  D2DERR_RECREATE_TARGET = HRESULT(-2003238900); // $8899000C
  E_NOT_SUFFICIENT_BUFFER = HRESULT(-2147024774); // $8007007A

  IID_IWinDWriteFactory: TGUID = '{B859EE5A-D838-4B5B-A2E8-1ADC7D93DB48}';
  IID_IWinDWriteTextFormat: TGUID = '{9C906818-31D7-4FD3-A151-7C5E225DB55A}';
  IID_IWinDWriteTextLayout: TGUID = '{53737037-6D14-410B-9BFE-0B182BB70961}';
  IID_IWinDWriteInlineObject: TGUID = '{8339FDE3-106F-47AB-8373-1C6295EB10B3}';
  IID_IWinDWritePixelSnapping: TGUID = '{EAF3A2DA-ECF4-4D24-B644-B34F6842024B}';
  IID_IWinDWriteTextRenderer: TGUID = '{EF8A8135-5CC6-45FE-8825-C5A0724EB819}';
  IID_IWinD2D1Brush: TGUID = '{2CD906A8-12E2-11DC-9FED-001143A055F9}';
  IID_IWinD2D1Factory: TGUID = '{06152247-6F50-465A-9245-118BFD3B6007}';

type
  IWinDWriteTextFormat = interface;
  IWinDWriteTextLayout = interface;
  IWinDWriteInlineObject = interface;
  IWinDWritePixelSnapping = interface;
  IWinDWriteTextRenderer = interface;
  IWinD2D1Factory = interface;
  IWinD2D1Resource = interface;
  IWinD2D1Brush = interface;
  IWinD2D1SolidColorBrush = interface;
  IWinD2D1RenderTarget = interface;
  IWinD2D1DCRenderTarget = interface;

  { Only methods used by iBiblia are typed. Opaque slots preserve the exact
    Windows 7 vtable positions without importing the rest of the DirectWrite
    object model. }
  IWinDWriteFactory = interface(IUnknown)
    ['{B859EE5A-D838-4B5B-A2E8-1ADC7D93DB48}']
    procedure _Slot00; stdcall;
    procedure _Slot01; stdcall;
    procedure _Slot02; stdcall;
    procedure _Slot03; stdcall;
    procedure _Slot04; stdcall;
    procedure _Slot05; stdcall;
    procedure _Slot06; stdcall;
    procedure _Slot07; stdcall;
    procedure _Slot08; stdcall;
    procedure _Slot09; stdcall;
    procedure _Slot10; stdcall;
    procedure _Slot11; stdcall;
    function CreateTextFormat(FontFamilyName: PWideChar;
      FontCollection: Pointer; FontWeight, FontStyle, FontStretch: LongInt;
      FontSize: Single; LocaleName: PWideChar;
      out TextFormat: IWinDWriteTextFormat): HRESULT; stdcall;
    procedure _Slot13; stdcall;
    procedure _Slot14; stdcall;
    function CreateTextLayout(Text: PWideChar; TextLength: TDWriteUInt32;
      TextFormat: IWinDWriteTextFormat; MaxWidth, MaxHeight: Single;
      out TextLayout: IWinDWriteTextLayout): HRESULT; stdcall;
  end;

  IWinDWriteTextFormat = interface(IUnknown)
    ['{9C906818-31D7-4FD3-A151-7C5E225DB55A}']
    function SetTextAlignment(Value: LongInt): HRESULT; stdcall;
    function SetParagraphAlignment(Value: LongInt): HRESULT; stdcall;
    function SetWordWrapping(Value: LongInt): HRESULT; stdcall;
    function SetReadingDirection(Value: LongInt): HRESULT; stdcall;
    function SetFlowDirection(Value: LongInt): HRESULT; stdcall;
    procedure _Slot05; stdcall;
    procedure _Slot06; stdcall;
    function SetLineSpacing(Method: LongInt; LineSpacing,
      Baseline: TDWriteFloat): HRESULT; stdcall;
    procedure _Slot08; stdcall;
    procedure _Slot09; stdcall;
    procedure _Slot10; stdcall;
    procedure _Slot11; stdcall;
    procedure _Slot12; stdcall;
    procedure _Slot13; stdcall;
    procedure _Slot14; stdcall;
    procedure _Slot15; stdcall;
    procedure _Slot16; stdcall;
    procedure _Slot17; stdcall;
    procedure _Slot18; stdcall;
    procedure _Slot19; stdcall;
    procedure _Slot20; stdcall;
    procedure _Slot21; stdcall;
    procedure _Slot22; stdcall;
    procedure _Slot23; stdcall;
    procedure _Slot24; stdcall;
  end;

  IWinDWriteTextLayout = interface(IWinDWriteTextFormat)
    ['{53737037-6D14-410B-9BFE-0B182BB70961}']
    function SetMaxWidth(Value: Single): HRESULT; stdcall;
    function SetMaxHeight(Value: Single): HRESULT; stdcall;
    procedure _LayoutSlot02; stdcall;
    function SetFontFamilyName(Value: PWideChar;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontWeight(Value: LongInt;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontStyle(Value: LongInt;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontStretch(Value: LongInt;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontSize(Value: Single;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetUnderline(Value: TDWriteBool;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetStrikethrough(Value: TDWriteBool;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetDrawingEffect(Effect: IUnknown;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetInlineObject(InlineObject: IWinDWriteInlineObject;
      Range: TDWriteTextRange): HRESULT; stdcall;
    procedure _LayoutSlot12; stdcall;
    procedure _LayoutSlot13; stdcall;
    procedure _LayoutSlot14; stdcall;
    procedure _LayoutSlot15; stdcall;
    procedure _LayoutSlot16; stdcall;
    procedure _LayoutSlot17; stdcall;
    procedure _LayoutSlot18; stdcall;
    procedure _LayoutSlot19; stdcall;
    procedure _LayoutSlot20; stdcall;
    procedure _LayoutSlot21; stdcall;
    procedure _LayoutSlot22; stdcall;
    procedure _LayoutSlot23; stdcall;
    procedure _LayoutSlot24; stdcall;
    procedure _LayoutSlot25; stdcall;
    procedure _LayoutSlot26; stdcall;
    procedure _LayoutSlot27; stdcall;
    procedure _LayoutSlot28; stdcall;
    procedure _LayoutSlot29; stdcall;
    function Draw(ClientDrawingContext: Pointer;
      Renderer: IWinDWriteTextRenderer; OriginX, OriginY: Single): HRESULT;
      stdcall;
    function GetLineMetrics(Metrics: PDWriteLineMetrics;
      MaxLineCount: TDWriteUInt32; out ActualLineCount: TDWriteUInt32): HRESULT;
      stdcall;
    function GetMetrics(out Metrics: TDWriteTextMetrics): HRESULT; stdcall;
    function GetOverhangMetrics(
      out Overhangs: TDWriteOverhangMetrics): HRESULT; stdcall;
    function GetClusterMetrics(Metrics: PDWriteClusterMetrics;
      MaxClusterCount: TDWriteUInt32;
      out ActualClusterCount: TDWriteUInt32): HRESULT; stdcall;
    procedure _LayoutSlot35; stdcall;
    function HitTestPoint(PointX, PointY: Single; out IsTrailingHit,
      IsInside: TDWriteBool; out Metrics: TDWriteHitTestMetrics): HRESULT; stdcall;
    function HitTestTextPosition(TextPosition: TDWriteUInt32;
      IsTrailingHit: TDWriteBool; out PointX, PointY: Single;
      out Metrics: TDWriteHitTestMetrics): HRESULT; stdcall;
    function HitTestTextRange(TextPosition, TextLength: TDWriteUInt32;
      OriginX, OriginY: Single; Metrics: PDWriteHitTestMetrics;
      MaxMetricsCount: TDWriteUInt32;
      out ActualMetricsCount: TDWriteUInt32): HRESULT; stdcall;
  end;

  IWinDWritePixelSnapping = interface(IUnknown)
    ['{EAF3A2DA-ECF4-4D24-B644-B34F6842024B}']
    function IsPixelSnappingDisabled(ClientDrawingContext: Pointer;
      out IsDisabled: TDWriteBool): HRESULT; stdcall;
    function GetCurrentTransform(ClientDrawingContext: Pointer;
      out Transform: TDWriteMatrix): HRESULT; stdcall;
    function GetPixelsPerDip(ClientDrawingContext: Pointer;
      out PixelsPerDip: TDWriteFloat): HRESULT; stdcall;
  end;

  IWinDWriteTextRenderer = interface(IWinDWritePixelSnapping)
    ['{EF8A8135-5CC6-45FE-8825-C5A0724EB819}']
    function DrawGlyphRun(ClientDrawingContext: Pointer;
      BaselineOriginX, BaselineOriginY: TDWriteFloat;
      MeasuringMode: LongInt; GlyphRun: PDWriteGlyphRun;
      GlyphRunDescription: PDWriteGlyphRunDescription;
      ClientDrawingEffect: IUnknown): HRESULT; stdcall;
    function DrawUnderline(ClientDrawingContext: Pointer;
      BaselineOriginX, BaselineOriginY: TDWriteFloat;
      Underline: PDWriteUnderline; ClientDrawingEffect: IUnknown): HRESULT;
      stdcall;
    function DrawStrikethrough(ClientDrawingContext: Pointer;
      BaselineOriginX, BaselineOriginY: TDWriteFloat;
      Strikethrough: PDWriteStrikethrough;
      ClientDrawingEffect: IUnknown): HRESULT; stdcall;
    function DrawInlineObject(ClientDrawingContext: Pointer;
      OriginX, OriginY: TDWriteFloat; InlineObject: IWinDWriteInlineObject;
      IsSideways, IsRightToLeft: TDWriteBool;
      ClientDrawingEffect: IUnknown): HRESULT; stdcall;
  end;

  IWinDWriteInlineObject = interface(IUnknown)
    ['{8339FDE3-106F-47AB-8373-1C6295EB10B3}']
    function Draw(ClientDrawingContext: Pointer;
      Renderer: IWinDWriteTextRenderer; OriginX, OriginY: Single;
      IsSideways, IsRightToLeft: TDWriteBool;
      ClientDrawingEffect: IUnknown): HRESULT; stdcall;
    function GetMetrics(out Metrics: TDWriteInlineObjectMetrics): HRESULT;
      stdcall;
    function GetOverhangMetrics(out Overhangs: TDWriteOverhangMetrics): HRESULT;
      stdcall;
    function GetBreakConditions(out BreakConditionBefore,
      BreakConditionAfter: LongInt): HRESULT; stdcall;
  end;

  IWinD2D1Resource = interface(IUnknown)
    ['{2CD90691-12E2-11DC-9FED-001143A055F9}']
    procedure GetFactory(out Factory: IWinD2D1Factory); stdcall;
  end;

  IWinD2D1Brush = interface(IWinD2D1Resource)
    ['{2CD906A8-12E2-11DC-9FED-001143A055F9}']
    procedure SetOpacity(Value: Single); stdcall;
    procedure SetTransform(Transform: Pointer); stdcall;
    function GetOpacity: Single; stdcall;
    procedure GetTransform(Transform: Pointer); stdcall;
  end;

  IWinD2D1SolidColorBrush = interface(IWinD2D1Brush)
    ['{2CD906A9-12E2-11DC-9FED-001143A055F9}']
    procedure SetColor(const Value: TD2DColorF); stdcall;
    function GetColor: TD2DColorF; stdcall;
  end;

  IWinD2D1RenderTarget = interface(IWinD2D1Resource)
    ['{2CD90694-12E2-11DC-9FED-001143A055F9}']
    procedure _RTSlot00; stdcall;
    procedure _RTSlot01; stdcall;
    procedure _RTSlot02; stdcall;
    procedure _RTSlot03; stdcall;
    function CreateSolidColorBrush(Color: PD2DColorF; BrushProperties: Pointer;
      out Brush: IWinD2D1SolidColorBrush): HRESULT; stdcall;
    procedure _RTSlot05; stdcall;
    procedure _RTSlot06; stdcall;
    procedure _RTSlot07; stdcall;
    procedure _RTSlot08; stdcall;
    procedure _RTSlot09; stdcall;
    procedure _RTSlot10; stdcall;
    procedure _RTSlot11; stdcall;
    procedure _RTSlot12; stdcall;
    procedure FillRectangle(Rect: PD2DRectF; Brush: IWinD2D1Brush); stdcall;
    procedure _RTSlot14; stdcall;
    procedure _RTSlot15; stdcall;
    procedure _RTSlot16; stdcall;
    procedure _RTSlot17; stdcall;
    procedure _RTSlot18; stdcall;
    procedure _RTSlot19; stdcall;
    procedure _RTSlot20; stdcall;
    procedure _RTSlot21; stdcall;
    procedure _RTSlot22; stdcall;
    procedure _RTSlot23; stdcall;
    procedure DrawTextLayout(Origin: TD2DPoint2F;
      TextLayout: IWinDWriteTextLayout; DefaultFillBrush: IWinD2D1Brush;
      Options: LongInt); stdcall;
    procedure DrawGlyphRun(BaselineOrigin: TD2DPoint2F;
      GlyphRun: PDWriteGlyphRun; ForegroundBrush: IWinD2D1Brush;
      MeasuringMode: LongInt); stdcall;
    procedure _RTSlot26; stdcall;
    procedure GetTransform(out Transform: TDWriteMatrix); stdcall;
    procedure _RTSlot28; stdcall;
    procedure _RTSlot29; stdcall;
    procedure _RTSlot30; stdcall;
    procedure _RTSlot31; stdcall;
    procedure _RTSlot32; stdcall;
    procedure _RTSlot33; stdcall;
    procedure _RTSlot34; stdcall;
    procedure _RTSlot35; stdcall;
    procedure _RTSlot36; stdcall;
    procedure _RTSlot37; stdcall;
    procedure _RTSlot38; stdcall;
    procedure _RTSlot39; stdcall;
    procedure _RTSlot40; stdcall;
    procedure _RTSlot41; stdcall;
    procedure _RTSlot42; stdcall;
    procedure Clear(Color: PD2DColorF); stdcall;
    procedure BeginDraw; stdcall;
    function EndDraw(Tag1, Tag2: PD2DTag): HRESULT; stdcall;
    procedure _RTSlot46; stdcall;
    procedure SetDpi(DpiX, DpiY: Single); stdcall;
    procedure GetDpi(out DpiX, DpiY: Single); stdcall;
    procedure _RTSlot49; stdcall;
    procedure _RTSlot50; stdcall;
    procedure _RTSlot51; stdcall;
    procedure _RTSlot52; stdcall;
  end;

  IWinD2D1DCRenderTarget = interface(IWinD2D1RenderTarget)
    ['{1C51BC64-DE61-46FD-9899-63A5D8F03950}']
    function BindDC(DC: HDC; Rect: PRect): HRESULT; stdcall;
  end;

  IWinD2D1Factory = interface(IUnknown)
    ['{06152247-6F50-465A-9245-118BFD3B6007}']
    procedure _FactorySlot00; stdcall;
    procedure _FactorySlot01; stdcall;
    procedure _FactorySlot02; stdcall;
    procedure _FactorySlot03; stdcall;
    procedure _FactorySlot04; stdcall;
    procedure _FactorySlot05; stdcall;
    procedure _FactorySlot06; stdcall;
    procedure _FactorySlot07; stdcall;
    procedure _FactorySlot08; stdcall;
    procedure _FactorySlot09; stdcall;
    procedure _FactorySlot10; stdcall;
    procedure _FactorySlot11; stdcall;
    procedure _FactorySlot12; stdcall;
    function CreateDCRenderTarget(Properties: PD2DRenderTargetProperties;
      out RenderTarget: IWinD2D1DCRenderTarget): HRESULT; stdcall;
  end;

function TryCreateDWriteFactory(out Factory: IWinDWriteFactory;
  out ErrorText: UnicodeString): Boolean;
function TryCreateD2DFactory(out Factory: IWinD2D1Factory;
  out ErrorText: UnicodeString): Boolean;
function HResultText(Value: HRESULT): UnicodeString;

implementation

type
  TDWriteCreateFactoryProc = function(FactoryType: LongInt;
    const IID: TGUID; out Factory: IWinDWriteFactory): HRESULT; stdcall;
  TD2D1CreateFactoryProc = function(FactoryType: LongInt;
    const IID: TGUID; FactoryOptions: Pointer;
    out Factory: IWinD2D1Factory): HRESULT; stdcall;

var
  DWriteModule: HMODULE = 0;
  D2DModule: HMODULE = 0;
  DWriteCreateFactoryProc: TDWriteCreateFactoryProc = nil;
  D2D1CreateFactoryProc: TD2D1CreateFactoryProc = nil;

function LoadFactoryProcedures(out ErrorText: UnicodeString): Boolean;
begin
  ErrorText := '';
  if not Assigned(DWriteCreateFactoryProc) then
  begin
    DWriteModule := LoadLibraryW('dwrite.dll');
    if DWriteModule = 0 then
    begin
      ErrorText := 'Unable to load dwrite.dll: ' +
        UnicodeString(SysErrorMessage(GetLastOSError));
      Exit(False);
    end;
    Pointer(DWriteCreateFactoryProc) := GetProcAddress(DWriteModule,
      'DWriteCreateFactory');
    if not Assigned(DWriteCreateFactoryProc) then
    begin
      ErrorText := 'DWriteCreateFactory was not found in dwrite.dll';
      Exit(False);
    end;
  end;

  if not Assigned(D2D1CreateFactoryProc) then
  begin
    D2DModule := LoadLibraryW('d2d1.dll');
    if D2DModule = 0 then
    begin
      ErrorText := 'Unable to load d2d1.dll: ' +
        UnicodeString(SysErrorMessage(GetLastOSError));
      Exit(False);
    end;
    Pointer(D2D1CreateFactoryProc) := GetProcAddress(D2DModule,
      'D2D1CreateFactory');
    if not Assigned(D2D1CreateFactoryProc) then
    begin
      ErrorText := 'D2D1CreateFactory was not found in d2d1.dll';
      Exit(False);
    end;
  end;
  Result := True;
end;

function HResultText(Value: HRESULT): UnicodeString;
begin
  Result := UnicodeString(Format('HRESULT 0x%.8x', [Cardinal(Value)]));
end;

function TryCreateDWriteFactory(out Factory: IWinDWriteFactory;
  out ErrorText: UnicodeString): Boolean;
var
  HR: HRESULT;
begin
  Factory := nil;
  if not LoadFactoryProcedures(ErrorText) then
    Exit(False);
  HR := DWriteCreateFactoryProc(DWRITE_FACTORY_TYPE_SHARED,
    IID_IWinDWriteFactory, Factory);
  Result := (HR >= 0) and Assigned(Factory);
  if not Result then
    ErrorText := 'DWriteCreateFactory failed: ' + HResultText(HR);
end;

function TryCreateD2DFactory(out Factory: IWinD2D1Factory;
  out ErrorText: UnicodeString): Boolean;
var
  HR: HRESULT;
begin
  Factory := nil;
  if not LoadFactoryProcedures(ErrorText) then
    Exit(False);
  HR := D2D1CreateFactoryProc(D2D1_FACTORY_TYPE_SINGLE_THREADED,
    IID_IWinD2D1Factory, nil, Factory);
  Result := (HR >= 0) and Assigned(Factory);
  if not Result then
    ErrorText := 'D2D1CreateFactory failed: ' + HResultText(HR);
end;

finalization
  DWriteCreateFactoryProc := nil;
  D2D1CreateFactoryProc := nil;
  if D2DModule <> 0 then
    FreeLibrary(D2DModule);
  if DWriteModule <> 0 then
    FreeLibrary(DWriteModule);

end.
