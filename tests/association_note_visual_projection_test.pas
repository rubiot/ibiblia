unit association_note_visual_projection_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry;

type
  TAssociationNoteVisualProjectionTests = class(TTestCase)
  published
    procedure ProjectionCopiesTranslatorNoteMetadata;
    procedure AssociationMarkersPreserveNumberingAndLogicalIndexes;
    procedure MismatchedProjectionKeepsMetadataHidden;
  end;

implementation

uses
  SysUtils, Graphics, ONTTokenizer, Syntagm, Verse, text_direction,
  verse_visual_document, verse_visual_adapter,
  theword_effective_inline_style_v2, theword_ont_codec_v2;

function FindSyntagmIndex(Syntagms: TSyntagmList;
  const AText: string): Integer;
begin
  for Result := 0 to Syntagms.Count - 1 do
    if Syntagms[Result].Text = AText then
      Exit;
  Result := -1;
end;

function FindMetadataIndex(Syntagms: TSyntagmList;
  const ARaw: string): Integer;
begin
  for Result := 0 to Syntagms.Count - 1 do
    if (Syntagms[Result].Kind = skMetadata) and
       (Syntagms[Result].RawText = ARaw) then
      Exit;
  Result := -1;
end;

function MismatchedNoteProjection(const ARaw: UnicodeString):
  TTheWordEffectiveInlineProjectionV2;
begin
  Result := TTheWordEffectiveInlineProjectionV2.Resolve(
    'x<RF>different body<Rf>');
end;

procedure TAssociationNoteVisualProjectionTests.
  ProjectionCopiesTranslatorNoteMetadata;
const
  RAW = '😀<RF q="x"><mystery>n</mystery><Rf>z';
var
  Note: TTheWordProjectedTranslatorNoteV2;
  Projection: TTheWordEffectiveInlineProjectionV2;
begin
  Projection := TTheWordEffectiveInlineProjectionV2.Resolve(UTF8Decode(RAW));
  try
    AssertEquals(1, Projection.TranslatorNoteCount);
    AssertEquals('emoji occupies two UTF-16 code units', 2,
      Projection.TranslatorNotes[0].OffsetUTF16);
    Note := Projection.TranslatorNotes[0];
    AssertEquals(QWord(1), Note.Id);
    AssertTrue('import ordinal is preserved', Note.Ordinal >= 0);
    AssertEquals('x', UTF8Encode(Note.Q));
    AssertEquals('<mystery>n</mystery>', UTF8Encode(Note.BodyRaw));
    AssertEquals('<RF q="x">', UTF8Encode(Note.OpenLexeme));
    AssertEquals('<Rf>', UTF8Encode(Note.CloseLexeme));
    AssertEquals('raw-only capability is copied', Ord(twlcRawOnly),
      Ord(Note.Capability));
    AssertEquals('note body stays out of visible text', '😀z',
      UTF8Encode(Projection.VisibleText));
  finally
    Projection.Free;
  end;
end;

procedure TAssociationNoteVisualProjectionTests.
  AssociationMarkersPreserveNumberingAndLogicalIndexes;
const
  RAW = '<RF>head<Rf>😀<RF>one<Rf><RF q="A">two<Rf>x<RF>tail<Rf>';
  NOTE_HEAD = '<RF>head<Rf>';
  NOTE_ONE = '<RF>one<Rf>';
  NOTE_A = '<RF q="A">two<Rf>';
  NOTE_TAIL = '<RF>tail<Rf>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  Notes: TVerseRenderedTranslatorNoteArray;
  ExpectedRaw: array[0..3] of string;
  ExpectedMarker: array[0..3] of string;
  ExpectedOffset: array[0..3] of Integer;
  Index, LogicalIndex, HitIndex, EmojiIndex, XIndex: Integer;
  Span: TVisualSpan;
begin
  ExpectedRaw[0] := NOTE_HEAD;
  ExpectedRaw[1] := NOTE_ONE;
  ExpectedRaw[2] := NOTE_A;
  ExpectedRaw[3] := NOTE_TAIL;
  ExpectedMarker[0] := '6';
  ExpectedMarker[1] := '7';
  ExpectedMarker[2] := 'A';
  ExpectedMarker[3] := '8';
  ExpectedOffset[0] := 0;
  ExpectedOffset[1] := 2;
  ExpectedOffset[2] := 2;
  ExpectedOffset[3] := 3;

  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdRightToLeft, 211, clGreen, clGray, False, RAW, 5, Notes);

    AssertEquals('one visual span remains mapped to every logical unit',
      VerseModel.Syntagms.Count, Document.SpanCount);
    AssertEquals(4, Length(Notes));
    for Index := 0 to High(Notes) do
    begin
      LogicalIndex := FindMetadataIndex(VerseModel.Syntagms,
        ExpectedRaw[Index]);
      AssertTrue('metadata syntagm exists', LogicalIndex >= 0);
      AssertEquals(LogicalIndex, Notes[Index].LogicalIndex);
      AssertEquals(ExpectedMarker[Index], UTF8Encode(Notes[Index].Marker));
      AssertEquals(ExpectedOffset[Index], Notes[Index].OffsetUTF16);
      AssertEquals(ExpectedRaw[Index], UTF8Encode(Notes[Index].OpenLexeme +
        Notes[Index].BodyRaw + Notes[Index].CloseLexeme));

      Span := Document.Spans[LogicalIndex];
      AssertEquals(Ord(vsrTranslatorNote), Ord(Span.Role));
      AssertEquals(Notes[Index].Id, Span.ObjectId);
      AssertEquals(LogicalIndex, Span.LogicalIndex);
      AssertEquals('marker inherits RTL document direction',
        Ord(tdRightToLeft), Ord(Span.Direction));
      AssertFalse('translator-note markers are never associable',
        Span.Interactive);
      AssertTrue(vtsfItalic in Span.Style.Flags);
      AssertTrue(vtsfSuperscript in Span.Style.Flags);
      AssertEquals(-1, Span.Style.FontSizeDelta);
      AssertTrue(Span.Style.HasForegroundColor);
      AssertEquals(LongInt(clBlue), Span.Style.ForegroundColor);
      AssertTrue('marker keeps its logical index for object hit-testing',
        Document.TryGetLogicalIndexAt(Span.StartUTF16, HitIndex));
      AssertEquals(LogicalIndex, HitIndex);
      AssertFalse('marker is excluded from interactive syntagm hit-testing',
        Document.TryGetInteractiveLogicalIndexAt(Span.StartUTF16, HitIndex));
    end;

    AssertEquals('explicit q does not consume the automatic sequence',
      '8', UTF8Encode(Notes[3].Marker));
    AssertEquals('two notes share the same UTF-16 anchor',
      Notes[1].OffsetUTF16, Notes[2].OffsetUTF16);
    EmojiIndex := FindSyntagmIndex(VerseModel.Syntagms, '😀');
    XIndex := FindSyntagmIndex(VerseModel.Syntagms, 'x');
    AssertEquals('emoji remains two UTF-16 units', 2,
      Document.Spans[EmojiIndex].LengthUTF16);
    AssertTrue(Document.Spans[EmojiIndex].Interactive);
    AssertTrue(Document.Spans[XIndex].Interactive);
    AssertEquals(EmojiIndex, Document.Spans[EmojiIndex].LogicalIndex);
    AssertEquals(XIndex, Document.Spans[XIndex].LogicalIndex);
  finally
    VerseModel.Free;
  end;
end;

procedure TAssociationNoteVisualProjectionTests.
  MismatchedProjectionKeepsMetadataHidden;
const
  RAW = 'x<RF>actual body<Rf>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  Notes: TVerseRenderedTranslatorNoteArray;
  MetadataIndex: Integer;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    MetadataIndex := FindMetadataIndex(VerseModel.Syntagms,
      '<RF>actual body<Rf>');
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 212, clGreen, clGray, False, RAW, 0, Notes,
      @MismatchedNoteProjection);
    AssertEquals(0, Length(Notes));
    AssertTrue('unreconciled metadata stays safely hidden',
      Document.Spans[MetadataIndex].Kind = vskHidden);
    AssertEquals('logical span count is unchanged',
      VerseModel.Syntagms.Count, Document.SpanCount);
  finally
    VerseModel.Free;
  end;
end;

initialization
  RegisterTest(TAssociationNoteVisualProjectionTests);

end.
