unit verse_visual_document_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry, text_direction, verse_visual_document;

type
  TVerseVisualDocumentTests = class(TTestCase)
  private
    function NewStyle: TVisualTextStyle;
  published
    procedure EmptyDocumentKeepsDirectionAndRevision;
    procedure TextSpansUseZeroBasedUTF16Ranges;
    procedure UTF16RangesCountSurrogatePairs;
    procedure DirectionMarkupProducesIsolatesAndNonInteractiveSpans;
    procedure NestedDirectionMarkupRestoresOuterDirection;
    procedure HiddenMarkupRetainsLogicalBoundary;
    procedure StylesAndInteractiveFlagsArePreserved;
    procedure MappingDistinguishesInteractiveTextFromControls;
    procedure LogicalIndexesMustIncrease;
    procedure MalformedDirectionTagsAreSafelyContained;
    procedure BuiltDocumentIsAnImmutableSnapshot;
  end;

implementation

function TVerseVisualDocumentTests.NewStyle: TVisualTextStyle;
begin
  Result := DefaultVisualTextStyle;
  Result.Flags := [vtsfBold, vtsfSuperscript, vtsfSelected];
  Result.FontSizeDelta := 4;
  Result.HasForegroundColor := True;
  Result.ForegroundColor := $00112233;
  Result.HasBackgroundColor := True;
  Result.BackgroundColor := $00445566;
end;

procedure TVerseVisualDocumentTests.EmptyDocumentKeepsDirectionAndRevision;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
begin
  Builder := TVerseVisualDocumentBuilder.Create(tdRightToLeft);
  try
    Document := Builder.Build(42);
    AssertTrue(Document.Text = '');
    AssertEquals(0, Document.SpanCount);
    AssertTrue(Document.BaseDirection = tdRightToLeft);
    AssertTrue(Document.Revision = 42);
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.TextSpansUseZeroBasedUTF16Ranges;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  LogicalIndex: Integer;
  StartUTF16: Integer;
  LengthUTF16: Integer;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(2, UnicodeString('abc'), Style, True);
    Builder.AddText(5, UnicodeString('de'), Style, False);
    Document := Builder.Build(7);

    AssertTrue(Document.Text = UnicodeString('abcde'));
    AssertEquals(2, Document.SpanCount);
    AssertEquals(0, Document.Spans[0].StartUTF16);
    AssertEquals(3, Document.Spans[0].LengthUTF16);
    AssertEquals(3, Document.Spans[1].StartUTF16);
    AssertEquals(2, Document.Spans[1].LengthUTF16);
    AssertTrue(Document.TryGetUTF16Range(5, StartUTF16, LengthUTF16));
    AssertEquals(3, StartUTF16);
    AssertEquals(2, LengthUTF16);
    AssertFalse(Document.TryGetUTF16Range(4, StartUTF16, LengthUTF16));
    AssertTrue(Document.TryGetLogicalIndexAt(4, LogicalIndex));
    AssertEquals(5, LogicalIndex);
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.UTF16RangesCountSurrogatePairs;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  EmojiText: UnicodeString;
  LogicalIndex: Integer;
  Style: TVisualTextStyle;
begin
  EmojiText := UnicodeString('A') + UnicodeChar($D83D) +
    UnicodeChar($DE00) + UnicodeString('B');
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(0, EmojiText, Style, True);
    Document := Builder.Build(1);

    AssertEquals(4, Length(Document.Text));
    AssertEquals(4, Document.Spans[0].LengthUTF16);
    AssertTrue(Document.TryGetInteractiveLogicalIndexAt(1, LogicalIndex));
    AssertEquals(0, LogicalIndex);
    AssertTrue(Document.TryGetInteractiveLogicalIndexAt(2, LogicalIndex));
    AssertEquals(0, LogicalIndex);
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.DirectionMarkupProducesIsolatesAndNonInteractiveSpans;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddMarkup(0, UnicodeString('<rtl>'));
    Builder.AddText(1, UnicodeString('אב'), Style, True);
    Builder.AddMarkup(2, UnicodeString('</rtl>'));
    Document := Builder.Build(9);

    AssertEquals(4, Length(Document.Text));
    AssertEquals(Ord(RTL_ISOLATE), Ord(Document.Text[1]));
    AssertEquals(Ord(POP_DIRECTIONAL_ISOLATE), Ord(Document.Text[4]));
    AssertTrue(Document.Spans[0].Kind = vskDirectionControl);
    AssertFalse(Document.Spans[0].Interactive);
    AssertTrue(Document.Spans[0].Direction = tdRightToLeft);
    AssertTrue(Document.Spans[1].Direction = tdRightToLeft);
    AssertTrue(Document.Spans[2].Kind = vskDirectionControl);
    AssertFalse(Document.Spans[2].Interactive);
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.NestedDirectionMarkupRestoresOuterDirection;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddMarkup(0, UnicodeString('<RTL>'));
    Builder.AddText(1, UnicodeString('א'), Style, True);
    Builder.AddMarkup(2, UnicodeString('<ltr>'));
    Builder.AddText(3, UnicodeString('PT'), Style, True);
    Builder.AddMarkup(4, UnicodeString('</LTR>'));
    Builder.AddText(5, UnicodeString('ב'), Style, True);
    Builder.AddMarkup(6, UnicodeString('</rtl>'));
    Document := Builder.Build(3);

    AssertTrue(Document.Spans[1].Direction = tdRightToLeft);
    AssertTrue(Document.Spans[3].Direction = tdLeftToRight);
    AssertTrue(Document.Spans[5].Direction = tdRightToLeft);
    AssertEquals(Ord(RTL_ISOLATE), Ord(Document.Text[1]));
    AssertEquals(Ord(LTR_ISOLATE), Ord(Document.Text[3]));
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.HiddenMarkupRetainsLogicalBoundary;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  StartUTF16: Integer;
  LengthUTF16: Integer;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdRightToLeft);
  try
    Builder.AddText(0, UnicodeString('א'), Style, True);
    Builder.AddMarkup(1, UnicodeString('<WG1>'));
    Builder.AddText(2, UnicodeString('ב'), Style, True);
    Document := Builder.Build(10);

    AssertEquals(3, Document.SpanCount);
    AssertTrue(Document.Spans[1].Kind = vskHidden);
    AssertEquals(1, Document.Spans[1].StartUTF16);
    AssertEquals(0, Document.Spans[1].LengthUTF16);
    AssertFalse(Document.Spans[1].Interactive);
    AssertTrue(Document.TryGetUTF16Range(1, StartUTF16, LengthUTF16));
    AssertEquals(1, StartUTF16);
    AssertEquals(0, LengthUTF16);
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.StylesAndInteractiveFlagsArePreserved;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  Style: TVisualTextStyle;
begin
  Style := NewStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(0, UnicodeString('word'), Style, True,
      UnicodeString('G3056, N-NSM'));
    Document := Builder.Build(1);

    AssertTrue(Document.Spans[0].Interactive);
    AssertTrue(vtsfBold in Document.Spans[0].Style.Flags);
    AssertTrue(vtsfSuperscript in Document.Spans[0].Style.Flags);
    AssertTrue(vtsfSelected in Document.Spans[0].Style.Flags);
    AssertEquals(4, Document.Spans[0].Style.FontSizeDelta);
    AssertTrue(Document.Spans[0].Style.HasForegroundColor);
    AssertEquals(LongInt($00112233),
      Document.Spans[0].Style.ForegroundColor);
    AssertTrue(Document.Spans[0].Style.HasBackgroundColor);
    AssertEquals(LongInt($00445566),
      Document.Spans[0].Style.BackgroundColor);
    AssertTrue(Document.Spans[0].Annotation =
      UnicodeString('G3056, N-NSM'));
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.MappingDistinguishesInteractiveTextFromControls;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  LogicalIndex: Integer;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdRightToLeft);
  try
    Builder.AddMarkup(0, UnicodeString('<ltr>'));
    Builder.AddText(1, UnicodeString('abc'), Style, True);
    Builder.AddMarkup(2, UnicodeString('</ltr>'));
    Document := Builder.Build(4);

    AssertTrue(Document.TryGetLogicalIndexAt(0, LogicalIndex));
    AssertEquals(0, LogicalIndex);
    AssertFalse(Document.TryGetInteractiveLogicalIndexAt(0, LogicalIndex));
    AssertTrue(Document.TryGetInteractiveLogicalIndexAt(1, LogicalIndex));
    AssertEquals(1, LogicalIndex);
    AssertFalse(Document.TryGetInteractiveLogicalIndexAt(4, LogicalIndex));
    AssertFalse(Document.TryGetLogicalIndexAt(-1, LogicalIndex));
    AssertFalse(Document.TryGetLogicalIndexAt(Length(Document.Text),
      LogicalIndex));
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.LogicalIndexesMustIncrease;
var
  Builder: TVerseVisualDocumentBuilder;
  RaisedError: Boolean;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(2, UnicodeString('a'), Style, True);
    RaisedError := False;
    try
      Builder.AddText(2, UnicodeString('b'), Style, True);
    except
      on E: EVerseVisualDocument do
        RaisedError := True;
    end;
    AssertTrue(RaisedError);
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.MalformedDirectionTagsAreSafelyContained;
var
  Builder: TVerseVisualDocumentBuilder;
  Document: IVerseVisualDocument;
  LogicalIndex: Integer;
  StartUTF16: Integer;
  LengthUTF16: Integer;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddMarkup(0, UnicodeString('<rtl>'));
    Builder.AddText(1, UnicodeString('אב'), Style, True);
    Document := Builder.Build(1);

    AssertEquals(4, Length(Document.Text));
    AssertEquals(Ord(RTL_ISOLATE), Ord(Document.Text[1]));
    AssertEquals(Ord(POP_DIRECTIONAL_ISOLATE), Ord(Document.Text[4]));
    AssertEquals(NO_LOGICAL_INDEX,
      Document.Spans[Document.SpanCount - 1].LogicalIndex);
    AssertTrue(Document.Spans[0].Annotation = '');
    AssertFalse(Document.TryGetLogicalIndexAt(3, LogicalIndex));
    AssertFalse(Document.TryGetUTF16Range(NO_LOGICAL_INDEX, StartUTF16,
      LengthUTF16));
  finally
    Builder.Free;
  end;

  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddMarkup(0, UnicodeString('</rtl>'));
    Document := Builder.Build(2);
    AssertEquals(0, Length(Document.Text));
    AssertTrue(Document.Spans[0].Kind = vskHidden);
    AssertTrue(Document.TryGetUTF16Range(0, StartUTF16, LengthUTF16));
    AssertEquals(0, LengthUTF16);
  finally
    Builder.Free;
  end;

  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddMarkup(0, UnicodeString('<rtl>'));
    Builder.AddMarkup(1, UnicodeString('</ltr>'));
    Document := Builder.Build(3);
    AssertEquals(2, Length(Document.Text));
    AssertTrue(Document.Spans[1].Kind = vskHidden);
    AssertEquals(NO_LOGICAL_INDEX,
      Document.Spans[Document.SpanCount - 1].LogicalIndex);
    AssertEquals(Ord(POP_DIRECTIONAL_ISOLATE),
      Ord(Document.Text[Length(Document.Text)]));
  finally
    Builder.Free;
  end;
end;

procedure TVerseVisualDocumentTests.BuiltDocumentIsAnImmutableSnapshot;
var
  Builder: TVerseVisualDocumentBuilder;
  FirstDocument: IVerseVisualDocument;
  SecondDocument: IVerseVisualDocument;
  Style: TVisualTextStyle;
begin
  Style := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(0, UnicodeString('first'), Style, True);
    FirstDocument := Builder.Build(1);
    Builder.Clear;
    Builder.AddText(0, UnicodeString('second'), Style, True);
    SecondDocument := Builder.Build(2);

    AssertTrue(FirstDocument.Text = UnicodeString('first'));
    AssertTrue(FirstDocument.Revision = 1);
    AssertTrue(SecondDocument.Text = UnicodeString('second'));
    AssertTrue(SecondDocument.Revision = 2);
  finally
    Builder.Free;
  end;
end;

initialization
  RegisterTest(TVerseVisualDocumentTests);

end.
