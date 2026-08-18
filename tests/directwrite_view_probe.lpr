program directwrite_view_probe;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  Windows, Interfaces, Forms, SysUtils, Types, Graphics, Controls,
  text_direction, verse_visual_document, verse_view_intf,
  directwrite_syntagm_view, directwrite_verse_editor;

procedure Require(Condition: Boolean; const MessageText: string);
begin
  if Condition then
    Exit;
  WriteLn(StdErr, 'FAIL: ', MessageText);
  Halt(1);
end;

type
  TProbeDirectWriteSyntagmView = class(TDirectWriteVerseEditor)
  public
    procedure PaintNow;
    procedure MouseDownAt(X, Y: Integer);
    procedure MouseMoveAt(X, Y: Integer);
    procedure MouseUpAt(X, Y: Integer);
  end;

procedure TProbeDirectWriteSyntagmView.PaintNow;
begin
  Paint;
end;

procedure TProbeDirectWriteSyntagmView.MouseDownAt(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
end;

procedure TProbeDirectWriteSyntagmView.MouseMoveAt(X, Y: Integer);
begin
  MouseMove([], X, Y);
end;

procedure TProbeDirectWriteSyntagmView.MouseUpAt(X, Y: Integer);
begin
  MouseUp(mbLeft, [], X, Y);
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
  TitleRects: TRectArray;
  BodyRects: TRectArray;
  FirstBodyRects: TRectArray;
  MiddleTitleRects: TRectArray;
  SecondBodyRects: TRectArray;
  Style: TVisualTextStyle;
  View: TProbeDirectWriteSyntagmView;
  ViewInterface: IVerseView;
  PixelColor: LongInt;
  RedValue, GreenValue, BlueValue: Byte;
  X, Y: Integer;
  MinNormalY, MinSuperscriptY: Integer;
  BodyStart, BodyEnd: Integer;
  FirstRegionStart, FirstRegionEnd: Integer;
  SecondRegionStart, SecondRegionEnd: Integer;
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
    Form.Show;
    Application.ProcessMessages;
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

    { A viewport/document/style update can invalidate geometry after the last
      paint, particularly when Paint itself triggers an auto-height resize.
      Input must repair that state immediately; requiring an application
      activation change here reproduces the user-visible hover/click stall. }
    View.NotifyChanges([vvcViewport], Document.Revision);
    Require(View.HitTest(HitPoint, Hit),
      'HitTest did not recover stale geometry on first input');
    Require(Hit.LogicalIndex = 2,
      'Input geometry recovery changed the logical hit index');

    Builder.Free;
    Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
    Style := DefaultVisualTextStyle;
    Builder.AddText(10, 'normal ', Style, True);
    Include(Style.Flags, vtsfSuperscript);
    Style.FontSizeDelta := 2;
    Style.HasForegroundColor := True;
    Style.ForegroundColor := clRed;
    Builder.AddText(11, 'sup', Style, True);
    Style := DefaultVisualTextStyle;
    Builder.AddText(12, ' normal', Style, True);
    Document := Builder.Build(2);
    View.SetDocument(Document);
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

    Builder.Free;
    Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
    Style := DefaultVisualTextStyle;
    Include(Style.Flags, vtsfBold);
    Style.FontSizeDelta := 4;
    Builder.AddText(20, 'Prefácio', Style, False, 'heading');
    Builder.AddText(21, #10, DefaultVisualTextStyle, False,
      'heading-break');
    Builder.AddText(22, 'corpo ', DefaultVisualTextStyle, True);
    Builder.AddText(23, ' ', DefaultVisualTextStyle, False,
      'protected-anchor');
    Builder.AddText(24, 'editável', DefaultVisualTextStyle, True);
    Document := Builder.Build(3);
    BodyStart := Document.Spans[2].StartUTF16;
    BodyEnd := Document.Spans[4].StartUTF16 +
      Document.Spans[4].LengthUTF16;
    View.InteractionMode := vimAssociation;
    View.SetEditorDocument(Document);
    Application.ProcessMessages;
    View.PaintNow;
    Require(View.GetLogicalRects(20, TitleRects) and
      (Length(TitleRects) > 0), 'No rectangle for noninteractive heading');
    Require(View.GetLogicalRects(22, BodyRects) and
      (Length(BodyRects) > 0), 'No rectangle for interactive body');
    Require((TitleRects[0].Bottom - TitleRects[0].Top) >
      (BodyRects[0].Bottom - BodyRects[0].Top),
      'Heading FontSizeDelta was not applied');
    HitPoint.X := (TitleRects[0].Left + TitleRects[0].Right) div 2;
    HitPoint.Y := (TitleRects[0].Top + TitleRects[0].Bottom) div 2;
    Require(not View.HitTest(HitPoint, Hit),
      'Association hit-test exposed the noninteractive heading');

    View.InteractionMode := vimEdit;
    Require(View.CaretOffset = BodyStart,
      'Initial editor caret did not skip the heading');
    View.MouseDownAt(HitPoint.X, HitPoint.Y);
    Require(View.CaretOffset = BodyStart,
      'Mouse input moved the caret into the heading');
    View.SelectAll;
    Require(View.SelectionStart = BodyStart,
      'Ctrl+A selection included the heading');
    Require(View.SelectionLength = BodyEnd - BodyStart,
      'Ctrl+A did not cover the full body including its protected anchor');

    Builder.Free;
    Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
    Builder.AddText(30, 'antes', DefaultVisualTextStyle, True);
    Builder.AddText(31, ' ', DefaultVisualTextStyle, False,
      'protected-anchor');
    Builder.AddText(32, ' ainda', DefaultVisualTextStyle, True);
    Builder.AddText(33, #10, DefaultVisualTextStyle, False,
      'title-break');
    Style := DefaultVisualTextStyle;
    Include(Style.Flags, vtsfBold);
    Style.FontSizeDelta := 2;
    Builder.AddText(34, 'Intertítulo', Style, False, 'title-level-0');
    Builder.AddText(35, #10, DefaultVisualTextStyle, False,
      'title-break');
    Builder.AddText(36, 'depois', DefaultVisualTextStyle, True);
    Document := Builder.Build(4);
    FirstRegionStart := Document.Spans[0].StartUTF16;
    FirstRegionEnd := Document.Spans[2].StartUTF16 +
      Document.Spans[2].LengthUTF16;
    SecondRegionStart := Document.Spans[6].StartUTF16;
    SecondRegionEnd := SecondRegionStart + Document.Spans[6].LengthUTF16;
    View.SetEditorDocument(Document);
    View.SetSelection(0, 0);
    Application.ProcessMessages;
    View.PaintNow;
    Require(View.GetLogicalRects(30, FirstBodyRects) and
      (Length(FirstBodyRects) > 0), 'No rectangle for first body region');
    Require(View.GetLogicalRects(34, MiddleTitleRects) and
      (Length(MiddleTitleRects) > 0), 'No rectangle for middle heading');
    Require(View.GetLogicalRects(36, SecondBodyRects) and
      (Length(SecondBodyRects) > 0), 'No rectangle for second body region');

    View.SelectAll;
    Require((View.SelectionStart = FirstRegionStart) and
      (View.SelectionLength = FirstRegionEnd - FirstRegionStart),
      'Ctrl+A crossed a middle heading from the first body region');
    HitPoint.X := (MiddleTitleRects[0].Left +
      MiddleTitleRects[0].Right) div 2;
    HitPoint.Y := (MiddleTitleRects[0].Top +
      MiddleTitleRects[0].Bottom) div 2;
    View.MouseDownAt(HitPoint.X, HitPoint.Y);
    Require((View.SelectionStart = FirstRegionStart) and
      (View.SelectionLength = FirstRegionEnd - FirstRegionStart),
      'Clicking a middle heading changed the body selection');

    HitPoint.X := (FirstBodyRects[0].Left +
      FirstBodyRects[0].Right) div 2;
    HitPoint.Y := (FirstBodyRects[0].Top +
      FirstBodyRects[0].Bottom) div 2;
    View.MouseDownAt(HitPoint.X, HitPoint.Y);
    HitPoint.X := (SecondBodyRects[0].Left +
      SecondBodyRects[0].Right) div 2;
    HitPoint.Y := (SecondBodyRects[0].Top +
      SecondBodyRects[0].Bottom) div 2;
    View.MouseMoveAt(HitPoint.X, HitPoint.Y);
    View.MouseUpAt(HitPoint.X, HitPoint.Y);
    Require(View.SelectionStart + View.SelectionLength <= FirstRegionEnd,
      'Mouse drag crossed a middle heading');

    View.MouseDownAt(HitPoint.X, HitPoint.Y);
    View.MouseUpAt(HitPoint.X, HitPoint.Y);
    Require((View.CaretOffset >= SecondRegionStart) and
      (View.CaretOffset <= SecondRegionEnd),
      'Click after a middle heading did not enter its body region');
    View.SelectAll;
    Require((View.SelectionStart = SecondRegionStart) and
      (View.SelectionLength = SecondRegionEnd - SecondRegionStart),
      'Ctrl+A crossed a middle heading from the second body region');

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
