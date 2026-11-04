unit directwrite_script_renderer;

{$mode objfpc}{$H+}
{$interfaces COM}
{$codepage utf8}

{$IFNDEF MSWINDOWS}
  {$ERROR directwrite_script_renderer is available only on Windows}
{$ENDIF}

interface

uses
  Windows, win_directwrite_d2d;

type
  { The drawing effect keeps text native in the parent layout. DirectWrite
    therefore retains shaping, bidi, wrapping and caret cluster boundaries;
    only the final glyph/decorations baseline is displaced while painting. }
  IWinDWriteScriptEffect = interface(IUnknown)
    ['{A185BEDA-7266-44FE-A8DC-93E63A3DA534}']
    function GetBrush(out Brush: IWinD2D1Brush): HRESULT; stdcall;
    function GetBaselineOffset(out Offset: TDWriteFloat): HRESULT; stdcall;
  end;

  TDirectWriteScriptRenderer = class(TInterfacedObject,
    IWinDWriteTextRenderer)
  private
    FDefaultBrush: IWinD2D1Brush;
    FRenderTarget: IWinD2D1RenderTarget;
    procedure ResolveEffect(const Effect: IUnknown;
      out Brush: IWinD2D1Brush; out BaselineOffset: TDWriteFloat);
    function DrawDecoration(BaselineOriginX, BaselineOriginY, Width,
      Thickness, Offset: TDWriteFloat; const Effect: IUnknown): HRESULT;
  public
    constructor Create(const RenderTarget: IWinD2D1RenderTarget;
      const DefaultBrush: IWinD2D1Brush);

    function IsPixelSnappingDisabled(ClientDrawingContext: Pointer;
      out IsDisabled: TDWriteBool): HRESULT; stdcall;
    function GetCurrentTransform(ClientDrawingContext: Pointer;
      out Transform: TDWriteMatrix): HRESULT; stdcall;
    function GetPixelsPerDip(ClientDrawingContext: Pointer;
      out PixelsPerDip: TDWriteFloat): HRESULT; stdcall;
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

function CreateDirectWriteScriptEffect(const Brush: IWinD2D1Brush;
  BaselineOffset: TDWriteFloat): IWinDWriteScriptEffect;

implementation

const
  S_OK_VALUE = HRESULT(0);
  E_INVALIDARG_VALUE = HRESULT(-2147024809); // $80070057
  SCRIPT_EFFECT_IID: TGUID = '{A185BEDA-7266-44FE-A8DC-93E63A3DA534}';

type
  TDirectWriteScriptEffect = class(TInterfacedObject,
    IWinDWriteScriptEffect)
  private
    FBaselineOffset: TDWriteFloat;
    FBrush: IWinD2D1Brush;
  public
    constructor Create(const Brush: IWinD2D1Brush;
      BaselineOffset: TDWriteFloat);
    function GetBrush(out Brush: IWinD2D1Brush): HRESULT; stdcall;
    function GetBaselineOffset(out Offset: TDWriteFloat): HRESULT; stdcall;
  end;

constructor TDirectWriteScriptEffect.Create(const Brush: IWinD2D1Brush;
  BaselineOffset: TDWriteFloat);
begin
  inherited Create;
  FBrush := Brush;
  FBaselineOffset := BaselineOffset;
end;

function TDirectWriteScriptEffect.GetBrush(
  out Brush: IWinD2D1Brush): HRESULT; stdcall;
begin
  Brush := FBrush;
  Result := S_OK_VALUE;
end;

function TDirectWriteScriptEffect.GetBaselineOffset(
  out Offset: TDWriteFloat): HRESULT; stdcall;
begin
  Offset := FBaselineOffset;
  Result := S_OK_VALUE;
end;

function CreateDirectWriteScriptEffect(const Brush: IWinD2D1Brush;
  BaselineOffset: TDWriteFloat): IWinDWriteScriptEffect;
begin
  Result := TDirectWriteScriptEffect.Create(Brush, BaselineOffset);
end;

constructor TDirectWriteScriptRenderer.Create(
  const RenderTarget: IWinD2D1RenderTarget;
  const DefaultBrush: IWinD2D1Brush);
begin
  inherited Create;
  FRenderTarget := RenderTarget;
  FDefaultBrush := DefaultBrush;
end;

procedure TDirectWriteScriptRenderer.ResolveEffect(const Effect: IUnknown;
  out Brush: IWinD2D1Brush; out BaselineOffset: TDWriteFloat);
var
  ScriptEffect: IWinDWriteScriptEffect;
begin
  Brush := FDefaultBrush;
  BaselineOffset := 0;
  if not Assigned(Effect) then
    Exit;
  ScriptEffect := nil;
  if Effect.QueryInterface(SCRIPT_EFFECT_IID, ScriptEffect) = S_OK_VALUE then
  begin
    ScriptEffect.GetBrush(Brush);
    ScriptEffect.GetBaselineOffset(BaselineOffset);
    if not Assigned(Brush) then
      Brush := FDefaultBrush;
    Exit;
  end;
  Brush := nil;
  if Effect.QueryInterface(IID_IWinD2D1Brush, Brush) <> S_OK_VALUE then
    Brush := FDefaultBrush;
end;

function TDirectWriteScriptRenderer.IsPixelSnappingDisabled(
  ClientDrawingContext: Pointer; out IsDisabled: TDWriteBool): HRESULT;
  stdcall;
begin
  IsDisabled := False;
  Result := S_OK_VALUE;
end;

function TDirectWriteScriptRenderer.GetCurrentTransform(
  ClientDrawingContext: Pointer; out Transform: TDWriteMatrix): HRESULT;
  stdcall;
begin
  FillChar(Transform, SizeOf(Transform), 0);
  if not Assigned(FRenderTarget) then
    Exit(E_INVALIDARG_VALUE);
  FRenderTarget.GetTransform(Transform);
  Result := S_OK_VALUE;
end;

function TDirectWriteScriptRenderer.GetPixelsPerDip(
  ClientDrawingContext: Pointer; out PixelsPerDip: TDWriteFloat): HRESULT;
  stdcall;
var
  DpiX, DpiY: TDWriteFloat;
begin
  PixelsPerDip := 1;
  if not Assigned(FRenderTarget) then
    Exit(E_INVALIDARG_VALUE);
  FRenderTarget.GetDpi(DpiX, DpiY);
  if DpiX > 0 then
    PixelsPerDip := DpiX / 96;
  Result := S_OK_VALUE;
end;

function TDirectWriteScriptRenderer.DrawGlyphRun(
  ClientDrawingContext: Pointer;
  BaselineOriginX, BaselineOriginY: TDWriteFloat; MeasuringMode: LongInt;
  GlyphRun: PDWriteGlyphRun;
  GlyphRunDescription: PDWriteGlyphRunDescription;
  ClientDrawingEffect: IUnknown): HRESULT; stdcall;
var
  BaselineOffset: TDWriteFloat;
  Brush: IWinD2D1Brush;
  Origin: TD2DPoint2F;
begin
  if not Assigned(FRenderTarget) or (GlyphRun = nil) then
    Exit(E_INVALIDARG_VALUE);
  ResolveEffect(ClientDrawingEffect, Brush, BaselineOffset);
  if not Assigned(Brush) then
    Exit(E_INVALIDARG_VALUE);
  Origin.X := BaselineOriginX;
  Origin.Y := BaselineOriginY + BaselineOffset;
  FRenderTarget.DrawGlyphRun(Origin, GlyphRun, Brush, MeasuringMode);
  Result := S_OK_VALUE;
end;

function TDirectWriteScriptRenderer.DrawDecoration(BaselineOriginX,
  BaselineOriginY, Width, Thickness, Offset: TDWriteFloat;
  const Effect: IUnknown): HRESULT;
var
  BaselineOffset: TDWriteFloat;
  Brush: IWinD2D1Brush;
  Rect: TD2DRectF;
begin
  if not Assigned(FRenderTarget) or (Width < 0) or (Thickness < 0) then
    Exit(E_INVALIDARG_VALUE);
  ResolveEffect(Effect, Brush, BaselineOffset);
  if not Assigned(Brush) then
    Exit(E_INVALIDARG_VALUE);
  Rect.Left := BaselineOriginX;
  Rect.Top := BaselineOriginY + BaselineOffset + Offset;
  Rect.Right := Rect.Left + Width;
  Rect.Bottom := Rect.Top + Thickness;
  FRenderTarget.FillRectangle(@Rect, Brush);
  Result := S_OK_VALUE;
end;

function TDirectWriteScriptRenderer.DrawUnderline(
  ClientDrawingContext: Pointer;
  BaselineOriginX, BaselineOriginY: TDWriteFloat;
  Underline: PDWriteUnderline; ClientDrawingEffect: IUnknown): HRESULT;
  stdcall;
begin
  if Underline = nil then
    Exit(E_INVALIDARG_VALUE);
  Result := DrawDecoration(BaselineOriginX, BaselineOriginY,
    Underline^.Width, Underline^.Thickness, Underline^.Offset,
    ClientDrawingEffect);
end;

function TDirectWriteScriptRenderer.DrawStrikethrough(
  ClientDrawingContext: Pointer;
  BaselineOriginX, BaselineOriginY: TDWriteFloat;
  Strikethrough: PDWriteStrikethrough;
  ClientDrawingEffect: IUnknown): HRESULT; stdcall;
begin
  if Strikethrough = nil then
    Exit(E_INVALIDARG_VALUE);
  Result := DrawDecoration(BaselineOriginX, BaselineOriginY,
    Strikethrough^.Width, Strikethrough^.Thickness,
    Strikethrough^.Offset, ClientDrawingEffect);
end;

function TDirectWriteScriptRenderer.DrawInlineObject(
  ClientDrawingContext: Pointer; OriginX, OriginY: TDWriteFloat;
  InlineObject: IWinDWriteInlineObject; IsSideways,
  IsRightToLeft: TDWriteBool; ClientDrawingEffect: IUnknown): HRESULT;
  stdcall;
var
  Renderer: IWinDWriteTextRenderer;
begin
  if not Assigned(InlineObject) then
    Exit(E_INVALIDARG_VALUE);
  Renderer := Self;
  Result := InlineObject.Draw(ClientDrawingContext, Renderer,
    OriginX, OriginY, IsSideways, IsRightToLeft, ClientDrawingEffect);
end;

end.
