unit verse_visual_adapter_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry;

type
  TVerseVisualAdapterTests = class(TTestCase)
  published
    procedure MixedHebrewPortuguesePreservesMetadataAnnotationsAndStyles;
    procedure AddedWordBracketsShareItalicStyleAndPreserveOnt;
  end;

implementation

uses
  SysUtils, Graphics, ONTTokenizer, Syntagm, Verse, text_direction,
  verse_visual_document, verse_visual_adapter;

function FindSyntagmIndex(Syntagms: TSyntagmList;
  const Text: string): Integer;
begin
  for Result := 0 to Syntagms.Count - 1 do
    if Syntagms[Result].Text = Text then
      Exit;
  Result := -1;
end;

procedure TVerseVisualAdapterTests.
  MixedHebrewPortuguesePreservesMetadataAnnotationsAndStyles;
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  HebrewIndex, PortugueseIndex, RTLIndex, LTRIndex: Integer;
  HebrewSyntagm: TSyntagm;
  HebrewSpan, PortugueseSpan: TVisualSpan;
  ExpectedText: UnicodeString;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text :=
      '<rtl><font color=red><sup>בְּרֵאשִׁית<WH7225></sup></font> ' +
      '<ltr>no princípio</ltr></rtl>';

    HebrewIndex := FindSyntagmIndex(VerseModel.Syntagms, 'בְּרֵאשִׁית');
    PortugueseIndex := FindSyntagmIndex(VerseModel.Syntagms, 'no');
    RTLIndex := FindSyntagmIndex(VerseModel.Syntagms, '<rtl>');
    LTRIndex := FindSyntagmIndex(VerseModel.Syntagms, '<ltr>');
    AssertTrue('Hebrew syntagm parsed', HebrewIndex >= 0);
    AssertTrue('Portuguese syntagm parsed', PortugueseIndex >= 0);
    AssertTrue('RTL metadata parsed', RTLIndex >= 0);
    AssertTrue('LTR metadata parsed', LTRIndex >= 0);

    HebrewSyntagm := VerseModel.Syntagms[HebrewIndex];
    HebrewSyntagm.AddToSelection;
    HebrewSyntagm.IsPointed := True;
    HebrewSyntagm.ToggleStrongHighlight(True);

    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 73, clGreen, clGray, True);

    ExpectedText := UnicodeString(RTL_ISOLATE) +
      UnicodeString('בְּרֵאשִׁית ') + UnicodeString(LTR_ISOLATE) +
      UnicodeString('no princípio') + UnicodeString(POP_DIRECTIONAL_ISOLATE) +
      UnicodeString(POP_DIRECTIONAL_ISOLATE);
    AssertTrue('Logical UTF-16 document text with isolates',
      Document.Text = ExpectedText);
    AssertTrue('Base direction', Document.BaseDirection = tdLeftToRight);
    AssertTrue('Revision', Document.Revision = 73);
    AssertEquals('One visual span per logical syntagm',
      VerseModel.Syntagms.Count, Document.SpanCount);

    AssertTrue('RTL tag is a direction control',
      Document.Spans[RTLIndex].Kind = vskDirectionControl);
    AssertFalse('RTL tag is not interactive',
      Document.Spans[RTLIndex].Interactive);
    AssertTrue('LTR tag is a direction control',
      Document.Spans[LTRIndex].Kind = vskDirectionControl);
    AssertFalse('LTR tag is not interactive',
      Document.Spans[LTRIndex].Interactive);

    HebrewSpan := Document.Spans[HebrewIndex];
    AssertTrue('Hebrew span direction',
      HebrewSpan.Direction = tdRightToLeft);
    AssertTrue('Hebrew word is interactive', HebrewSpan.Interactive);
    AssertTrue('Strong metadata becomes the visible annotation',
      HebrewSpan.Annotation =
        UTF8Decode(HebrewSyntagm.Strong.CommaText));
    AssertTrue('Strong word is bold', vtsfBold in HebrewSpan.Style.Flags);
    AssertTrue('Superscript style is preserved',
      vtsfSuperscript in HebrewSpan.Style.Flags);
    AssertTrue('Selection style is preserved',
      vtsfSelected in HebrewSpan.Style.Flags);
    AssertTrue('Pointed style is preserved',
      vtsfPointed in HebrewSpan.Style.Flags);
    AssertTrue('Strong highlight style is preserved',
      vtsfStrongHighlight in HebrewSpan.Style.Flags);
    AssertTrue('Point/highlight produces underline',
      vtsfUnderline in HebrewSpan.Style.Flags);
    AssertTrue('Explicit text color is preserved',
      HebrewSpan.Style.HasForegroundColor);
    AssertEquals('Explicit font color', LongInt(clRed),
      HebrewSpan.Style.ForegroundColor);
    AssertTrue('Selected background is preserved',
      HebrewSpan.Style.HasBackgroundColor);
    AssertEquals('Selected background color', LongInt(clYellow),
      HebrewSpan.Style.BackgroundColor);

    PortugueseSpan := Document.Spans[PortugueseIndex];
    AssertTrue('Portuguese span direction',
      PortugueseSpan.Direction = tdLeftToRight);
    AssertTrue('Portuguese word is interactive', PortugueseSpan.Interactive);
    AssertTrue('Portuguese word has no Strong annotation',
      PortugueseSpan.Annotation = '');
    AssertEquals('Unassociated fallback color', LongInt(clGray),
      PortugueseSpan.Style.ForegroundColor);

    AssertTrue('Font tag is hidden metadata',
      Document.Spans[FindSyntagmIndex(VerseModel.Syntagms,
        '<font color=red>')].Kind = vskHidden);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseVisualAdapterTests.
  AddedWordBracketsShareItalicStyleAndPreserveOnt;
const
  RAW: string = 'Então <FI>já<Fi> veio, <FI>mas<Fi> voltou.';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  FirstOpenIndex, FirstCloseIndex, SecondOpenIndex,
    SecondCloseIndex: Integer;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    FirstOpenIndex := FindSyntagmIndex(VerseModel.Syntagms, '<FI>');
    FirstCloseIndex := FindSyntagmIndex(VerseModel.Syntagms, '<Fi>');
    AssertTrue('first FI delimiter parsed', FirstOpenIndex >= 0);
    AssertTrue('first Fi delimiter parsed', FirstCloseIndex > FirstOpenIndex);

    SecondOpenIndex := FirstCloseIndex + 1;
    while (SecondOpenIndex < VerseModel.Syntagms.Count) and
          (VerseModel.Syntagms[SecondOpenIndex].Text <> '<FI>') do
      Inc(SecondOpenIndex);
    SecondCloseIndex := SecondOpenIndex + 1;
    while (SecondCloseIndex < VerseModel.Syntagms.Count) and
          (VerseModel.Syntagms[SecondCloseIndex].Text <> '<Fi>') do
      Inc(SecondCloseIndex);
    AssertTrue('second FI delimiter parsed',
      SecondOpenIndex < VerseModel.Syntagms.Count);
    AssertTrue('second Fi delimiter parsed',
      SecondCloseIndex < VerseModel.Syntagms.Count);

    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 74, clGreen, clGray, False);
    AssertTrue('FI tags are represented by balanced brackets',
      Document.Text = UnicodeString('Então [já] veio, [mas] voltou.'));
    AssertTrue('first opening bracket is italic',
      vtsfItalic in Document.Spans[FirstOpenIndex].Style.Flags);
    AssertTrue('first closing bracket is italic',
      vtsfItalic in Document.Spans[FirstCloseIndex].Style.Flags);
    AssertTrue('first bracket pair uses identical style flags',
      Document.Spans[FirstOpenIndex].Style.Flags =
      Document.Spans[FirstCloseIndex].Style.Flags);
    AssertEquals('first bracket pair uses identical foreground color',
      Document.Spans[FirstOpenIndex].Style.ForegroundColor,
      Document.Spans[FirstCloseIndex].Style.ForegroundColor);
    AssertTrue('second opening bracket is italic',
      vtsfItalic in Document.Spans[SecondOpenIndex].Style.Flags);
    AssertTrue('second closing bracket is italic',
      vtsfItalic in Document.Spans[SecondCloseIndex].Style.Flags);
    AssertTrue('second bracket pair uses identical style flags',
      Document.Spans[SecondOpenIndex].Style.Flags =
      Document.Spans[SecondCloseIndex].Style.Flags);
    AssertEquals('second bracket pair uses identical foreground color',
      Document.Spans[SecondOpenIndex].Style.ForegroundColor,
      Document.Spans[SecondCloseIndex].Style.ForegroundColor);
    AssertEquals('visualization does not rewrite literal ONT', RAW,
      VerseModel.Text);
  finally
    VerseModel.Free;
  end;
end;

initialization
  RegisterTest(TVerseVisualAdapterTests);

end.
