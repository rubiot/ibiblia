program directwrite_backend_probe;

{$mode objfpc}{$H+}
{$codepage utf8}
{$interfaces COM}

{$IFNDEF MSWINDOWS}
begin
  WriteLn('SKIP: DirectWrite backend is Windows-only');
end.
{$ELSE}

uses
  Windows, SysUtils, win_directwrite_d2d;

function CheckHR(const Operation: string; HR: HRESULT): Boolean;
begin
  Result := HR >= 0;
  if not Result then
    WriteLn(Operation, ' failed: ', UTF8Encode(HResultText(HR)));
end;

var
  Bitmap: HBITMAP;
  BitmapInfo: TBitmapInfo;
  Bits: Pointer;
  Brush: IWinD2D1SolidColorBrush;
  DC: HDC;
  D2DFactory: IWinD2D1Factory;
  DWriteFactory: IWinDWriteFactory;
  ErrorText: UnicodeString;
  FamilyName: UnicodeString;
  HR: HRESULT;
  Layout: IWinDWriteTextLayout;
  Metrics: TDWriteTextMetrics;
  MixedText: UnicodeString;
  OldBitmap: HGDIOBJ;
  Origin: TD2DPoint2F;
  Properties: TD2DRenderTargetProperties;
  Rect: TRect;
  RenderTarget: IWinD2D1DCRenderTarget;
  TextColor: TD2DColorF;
  TextFormat: IWinDWriteTextFormat;
begin
  if not TryCreateDWriteFactory(DWriteFactory, ErrorText) then
  begin
    WriteLn(UTF8Encode(ErrorText));
    Halt(1);
  end;
  if not TryCreateD2DFactory(D2DFactory, ErrorText) then
  begin
    WriteLn(UTF8Encode(ErrorText));
    Halt(1);
  end;

  FamilyName := 'Segoe UI';
  MixedText := 'Gn 1:1 בראשית ברא Deus';
  HR := DWriteFactory.CreateTextFormat(PWideChar(FamilyName), nil,
    DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL,
    DWRITE_FONT_STRETCH_NORMAL, 18, 'pt-BR', TextFormat);
  if not CheckHR('CreateTextFormat', HR) then
    Halt(1);
  if not CheckHR('SetReadingDirection', TextFormat.SetReadingDirection(
    DWRITE_READING_DIRECTION_RIGHT_TO_LEFT)) then
    Halt(1);
  HR := DWriteFactory.CreateTextLayout(PWideChar(MixedText), Length(MixedText),
    TextFormat, 320, 200, Layout);
  if not CheckHR('CreateTextLayout', HR) then
    Halt(1);
  if not CheckHR('GetMetrics', Layout.GetMetrics(Metrics)) then
    Halt(1);

  FillChar(Properties, SizeOf(Properties), 0);
  Properties.TargetType := D2D1_RENDER_TARGET_TYPE_DEFAULT;
  Properties.PixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Properties.PixelFormat.AlphaMode := D2D1_ALPHA_MODE_IGNORE;
  HR := D2DFactory.CreateDCRenderTarget(@Properties, RenderTarget);
  if not CheckHR('CreateDCRenderTarget', HR) then
    Halt(1);

  DC := CreateCompatibleDC(0);
  if DC = 0 then
    RaiseLastOSError;
  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := SizeOf(BitmapInfo.bmiHeader);
  BitmapInfo.bmiHeader.biWidth := 320;
  BitmapInfo.bmiHeader.biHeight := -200;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;
  BitmapInfo.bmiHeader.biCompression := BI_RGB;
  Bitmap := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
  if Bitmap = 0 then
    RaiseLastOSError;
  OldBitmap := SelectObject(DC, Bitmap);
  try
    Rect.Left := 0;
    Rect.Top := 0;
    Rect.Right := 320;
    Rect.Bottom := 200;
    if not CheckHR('BindDC', RenderTarget.BindDC(DC, @Rect)) then
      Halt(1);
    TextColor.R := 0;
    TextColor.G := 0;
    TextColor.B := 0;
    TextColor.A := 1;
    if not CheckHR('CreateSolidColorBrush', RenderTarget.CreateSolidColorBrush(
      @TextColor, nil, Brush)) then
      Halt(1);
    Origin.X := 0;
    Origin.Y := 0;
    RenderTarget.BeginDraw;
    RenderTarget.DrawTextLayout(Origin, Layout, Brush,
      D2D1_DRAW_TEXT_OPTIONS_NONE);
    if not CheckHR('EndDraw', RenderTarget.EndDraw(nil, nil)) then
      Halt(1);
  finally
    SelectObject(DC, OldBitmap);
    DeleteObject(Bitmap);
    DeleteDC(DC);
  end;

  WriteLn('OK: layout ', Metrics.LineCount, ' line(s), ',
    FormatFloat('0.0', Metrics.Width), ' x ',
    FormatFloat('0.0', Metrics.Height), ' DIP');
end.
{$ENDIF}
