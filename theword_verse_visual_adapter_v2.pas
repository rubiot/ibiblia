unit theword_verse_visual_adapter_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Graphics, verse_visual_document, theword_ont_codec_v2;

function BuildTheWordVerseVisualDocument(ADocument: TTheWordVerseDocument;
  ABaseDirection: TTextDirection; ARevision: QWord): IVerseVisualDocument;

function CanApplyTheWordSemanticMark(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;

function CanReplaceTheWordText(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;

implementation

uses
  SysUtils;

type
  TIntArray = array of Integer;

procedure AddBoundary(var ABoundaries: TIntArray; AValue: Integer);
var
  Index, InsertAt: Integer;
begin
  InsertAt := Length(ABoundaries);
  for Index := 0 to High(ABoundaries) do
  begin
    if ABoundaries[Index] = AValue then
      Exit;
    if ABoundaries[Index] > AValue then
    begin
      InsertAt := Index;
      Break;
    end;
  end;
  SetLength(ABoundaries, Length(ABoundaries) + 1);
  for Index := High(ABoundaries) downto InsertAt + 1 do
    ABoundaries[Index] := ABoundaries[Index - 1];
  ABoundaries[InsertAt] := AValue;
end;

procedure ApplyMarkStyle(AMark: TTheWordSemanticMark;
  var AStyle: TVisualTextStyle; var AAnnotation: UnicodeString);
const
  KIND_NAMES: array[TTheWordSemanticKind] of UnicodeString = (
    'added-words', 'words-of-jesus', 'old-testament-quote', 'underline'
  );
begin
  case AMark.Kind of
    twsmItalic:
      Include(AStyle.Flags, vtsfItalic);
    twsmWordsOfJesus:
      begin
        AStyle.HasForegroundColor := True;
        AStyle.ForegroundColor := LongInt(clRed);
      end;
    twsmOldTestamentQuote:
      begin
        Include(AStyle.Flags, vtsfBold);
        AStyle.HasForegroundColor := True;
        AStyle.ForegroundColor := LongInt(clNavy);
      end;
    twsmUnderline:
      Include(AStyle.Flags, vtsfUnderline);
  end;
  if AAnnotation <> '' then
    AAnnotation := AAnnotation + ',';
  AAnnotation := AAnnotation + KIND_NAMES[AMark.Kind];
end;

function BuildTheWordVerseVisualDocument(ADocument: TTheWordVerseDocument;
  ABaseDirection: TTextDirection; ARevision: QWord): IVerseVisualDocument;
var
  Boundaries: TIntArray;
  Builder: TVerseVisualDocumentBuilder;
  Index, MarkIndex, SegmentStart, SegmentFinish: Integer;
  Mark: TTheWordSemanticMark;
  Style: TVisualTextStyle;
  Annotation, SegmentText: UnicodeString;
begin
  if not Assigned(ADocument) then
    raise ETheWordOntCodec.Create('Cannot visualize a nil ONT document');

  Boundaries := nil;
  AddBoundary(Boundaries, 0);
  AddBoundary(Boundaries, Length(ADocument.VisibleText));
  for MarkIndex := 0 to ADocument.MarkCount - 1 do
  begin
    Mark := ADocument.Marks[MarkIndex];
    AddBoundary(Boundaries, Mark.StartOffset);
    AddBoundary(Boundaries, Mark.EndOffset);
  end;

  Builder := TVerseVisualDocumentBuilder.Create(ABaseDirection);
  try
    if Length(Boundaries) = 1 then
      Builder.AddText(0, '', DefaultVisualTextStyle, True)
    else
      for Index := 0 to High(Boundaries) - 1 do
      begin
        SegmentStart := Boundaries[Index];
        SegmentFinish := Boundaries[Index + 1];
        Style := DefaultVisualTextStyle;
        Annotation := '';
        for MarkIndex := 0 to ADocument.MarkCount - 1 do
        begin
          Mark := ADocument.Marks[MarkIndex];
          if (Mark.StartOffset <= SegmentStart) and
             (Mark.EndOffset >= SegmentFinish) and
             (Mark.EndOffset > Mark.StartOffset) then
            ApplyMarkStyle(Mark, Style, Annotation);
        end;
        SegmentText := Copy(ADocument.VisibleText, SegmentStart + 1,
          SegmentFinish - SegmentStart);
        Builder.AddText(Index, SegmentText, Style, True, Annotation);
      end;
    Result := Builder.Build(ARevision);
  finally
    Builder.Free;
  end;
end;

function PointTouchesRange(APoint, AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  RangeFinish: Integer;
begin
  RangeFinish := AStartUTF16 + ALengthUTF16;
  if ALengthUTF16 = 0 then
    Result := APoint = AStartUTF16
  else
    Result := (APoint >= AStartUTF16) and (APoint < RangeFinish);
end;

function CanApplyTheWordSemanticMark(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  Index: Integer;
begin
  Result := Assigned(ADocument) and (ALengthUTF16 > 0) and
    (AStartUTF16 >= 0) and
    (AStartUTF16 + ALengthUTF16 <= Length(ADocument.VisibleText));
  if not Result then
    Exit;
  for Index := 0 to ADocument.OpaqueItemCount - 1 do
    if PointTouchesRange(ADocument.OpaqueItems[Index].Offset,
      AStartUTF16, ALengthUTF16) then
      Exit(False);
end;

function CanReplaceTheWordText(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;
begin
  Result := Assigned(ADocument) and
    ADocument.CanReplaceVisibleText(AStartUTF16, ALengthUTF16);
end;

end.
