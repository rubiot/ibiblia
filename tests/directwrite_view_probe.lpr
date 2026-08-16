program directwrite_view_probe;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  Windows, Interfaces, Forms, SysUtils, Types, Graphics,
  text_direction, verse_visual_document, verse_view_intf,
  directwrite_syntagm_view;

procedure Require(Condition: Boolean; const MessageText: string);
begin
  if Condition then
    Exit;
  WriteLn(StdErr, 'FAIL: ', MessageText);
  Halt(1);
end;

type
  TProbeDirectWriteSyntagmView = class(TDirectWriteSyntagmView)
  public
    procedure PaintNow;
  end;

procedure TProbeDirectWriteSyntagmView.PaintNow;
begin
  Paint;
end;

var
  Bitmap: TBitmap;
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  Form: TForm;
  Hit: TVerseHit;
  HebrewRects: TRectArray;
  HitPoint: TPoint;
  PortugueseRects: TRectArray;
  NormalRects: TRectArray;
  SuperscriptRects: TRectArray;
  Style: TVisualTextStyle;
  View: TProbeDirectWriteSyntagmView;
  ViewInterface: IVerseView;
  PixelColor: LongInt;
  RedValue, GreenValue, BlueValue: Byte;
  X, Y: Integer;
  MinNormalY, MinSuperscriptY: Integer;
  ViewDC: HDC;
begin
  Application.Initialize;
  Form := TForm.Create(nil);
  Builder := TVerseVisualDocumentBuilder.Create(tdRightToLeft);
  Bitmap := TBitmap.Create;
  try
    Form.SetBounds(0, 0, 260, 320);
    View := TProbeDirectWriteSyntagmView.Create(Form);
    View.Parent := Form;
    View.SetBounds(0, 0, 240, 300);
    View.Font.Name := 'Segoe UI';
    View.Font.Size := 14;

    Style := DefaultVisualTextStyle;
    Style.HasForegroundColor := True;
    Style.ForegroundColor := clNavy;
    Style.HasBackgroundColor := True;
    Style.BackgroundColor := $00E8F4FF;
    Builder.AddText(0, 'בְּרֵאשִׁית בָּרָא אֱלֹהִים אֵת הַשָּׁמַיִם וְאֵת הָאָרֶץ', Style, True, 'H1');
    Builder.AddMarkup(1, '<ltr>');
    Style := DefaultVisualTextStyle;
    Style.HasForegroundColor := True;
    Style.ForegroundColor := clMaroon;
    Builder.AddText(2, ' — no princípio Deus criou os céus e a terra, texto português suficientemente longo para quebrar em várias linhas 123', Style, True, 'P1');
    Builder.AddMarkup(3, '</ltr>');
    Document := Builder.Build(1);

    Require(Document.BaseDirection = tdRightToLeft,
      'Document did not preserve the RTL base direction');
    View.SetDocument(Document);
    Require(Supports(View, IVerseView, ViewInterface),
      'TDirectWriteSyntagmView does not expose IVerseView');
    Require(View.BackendAvailable,
      'Backend unavailable: ' + UTF8Encode(View.BackendError));

    Form.HandleNeeded;
    View.HandleNeeded;
    View.PaintNow;
    Bitmap.SetSize(View.Width, View.Height);
    View.PaintTo(Bitmap.Canvas, 0, 0);

    Require(View.ContentHeight > 0, 'ContentHeight is not positive');
    Require(View.GetLogicalRects(0, HebrewRects) and (Length(HebrewRects) > 0),
      'No visual rectangles for Hebrew syntagm');
    Require(HebrewRects[0].Right > View.ClientWidth div 2,
      'RTL base direction did not position the Hebrew line from the right');
    HitPoint.X := (HebrewRects[0].Left + HebrewRects[0].Right) div 2;
    HitPoint.Y := (HebrewRects[0].Top + HebrewRects[0].Bottom) div 2;
    Require(View.HitTest(HitPoint, Hit), 'HitTest missed the Hebrew syntagm');
    Require(Hit.LogicalIndex = 0, 'HitTest returned the wrong logical index');
    Require(View.GetLogicalRects(2, PortugueseRects) and
      (Length(PortugueseRects) > 1),
      'No visual rectangles for Portuguese syntagm');
    HitPoint.X := (PortugueseRects[High(PortugueseRects)].Left +
      PortugueseRects[High(PortugueseRects)].Right) div 2;
    HitPoint.Y := (PortugueseRects[High(PortugueseRects)].Top +
      PortugueseRects[High(PortugueseRects)].Bottom) div 2;
    Require(View.HitTest(HitPoint, Hit), 'HitTest missed Portuguese after wrapping');
    Require(Hit.LogicalIndex = 2,
      'HitTest returned the wrong logical index after wrapping');

    { Closing the verse editor removes a sibling overlay. On Win32 that does
      not necessarily repaint the DirectWrite child, while viewport changes
      invalidate the geometry used by hit testing. The editor-close path now
      forces this paint before returning control to the verse. }
    View.NotifyChanges([vvcViewport], Document.Revision);
    Require(not View.HitTest(HitPoint, Hit),
      'HitTest accepted geometry invalidated by a viewport change');
    View.PaintNow;
    Require(View.HitTest(HitPoint, Hit),
      'HitTest did not recover after the editor-close repaint');
    Require(Hit.LogicalIndex = 2,
      'Editor-close repaint changed the logical hit index');

    Builder.Free;
    Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
    Style := DefaultVisualTextStyle;
    Builder.AddText(10, 'normal ', Style, True);
    Include(Style.Flags, vtsfSuperscript);
    Style.HasForegroundColor := True;
    Style.ForegroundColor := clRed;
    Builder.AddText(11, 'sup', Style, True);
    Style := DefaultVisualTextStyle;
    Builder.AddText(12, ' normal', Style, True);
    Document := Builder.Build(2);
    View.SetDocument(Document);
    Form.Show;
    Application.ProcessMessages;
    View.PaintNow;
    Application.ProcessMessages;
    Bitmap.SetSize(View.Width, View.Height);
    ViewDC := Windows.GetDC(View.Handle);
    try
      Windows.BitBlt(Bitmap.Canvas.Handle, 0, 0, View.Width, View.Height,
        ViewDC, 0, 0, SRCCOPY);
    finally
      Windows.ReleaseDC(View.Handle, ViewDC);
    end;
    MinNormalY := MaxInt;
    MinSuperscriptY := MaxInt;
    for Y := 0 to Bitmap.Height - 1 do
      for X := 0 to Bitmap.Width - 1 do
      begin
        PixelColor := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
        RedValue := Byte(PixelColor);
        GreenValue := Byte(PixelColor shr 8);
        BlueValue := Byte(PixelColor shr 16);
        if (RedValue > 120) and (GreenValue < 100) and (BlueValue < 100) then
          MinSuperscriptY := Min(MinSuperscriptY, Y)
        else if (RedValue < 80) and (GreenValue < 80) and
          (BlueValue < 80) then
          MinNormalY := Min(MinNormalY, Y);
      end;
    Require(View.GetLogicalRects(10, NormalRects) and
      (Length(NormalRects) > 0), 'No rectangle for normal text');
    Require(View.GetLogicalRects(11, SuperscriptRects) and
      (Length(SuperscriptRects) > 0), 'No rectangle for superscript text');
    WriteLn('normal glyph top=', MinNormalY,
      ' superscript glyph top=', MinSuperscriptY);
    Require(MinNormalY <> MaxInt,
      'Normal text pixels were not rendered');
    Require(MinSuperscriptY <> MaxInt,
      'Superscript text pixels were not rendered');
    Require(MinSuperscriptY < MinNormalY,
      'Superscript text was reduced but not raised above the baseline');
    HitPoint.X := (SuperscriptRects[0].Left +
      SuperscriptRects[0].Right) div 2;
    HitPoint.Y := (SuperscriptRects[0].Top +
      SuperscriptRects[0].Bottom) div 2;
    Require(View.HitTest(HitPoint, Hit),
      'HitTest missed the superscript inline object');
    Require(Hit.LogicalIndex = 11,
      'Superscript hit-test returned the wrong logical index');

    WriteLn('OK: ', View.BackendName, '; content height=',
      View.ContentHeight, '; superscript rise=',
      MinNormalY - MinSuperscriptY, 'px');
  finally
    ViewInterface := nil;
    Document := nil;
    Bitmap.Free;
    Builder.Free;
    Form.Free;
  end;
end.
