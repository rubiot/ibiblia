unit theword_verse_visual_adapter_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Graphics, verse_visual_document, theword_ont_codec_v2;

type
  TTheWordOffsetAffinity = (twoaBefore, twoaAfter);

  { DirectWrite may contain synthetic spacing for protected zero-width ONT
    objects. This map keeps every editor command in canonical document
    offsets instead of leaking layout offsets into the codec session. }
  ITheWordVisualOffsetMap = interface
    ['{251D7C09-E58E-4EA7-B084-E233FB8D5CD1}']
    function TryDocumentToLayoutOffset(ADocumentOffset: Integer;
      AAffinity: TTheWordOffsetAffinity; out ALayoutOffset: Integer): Boolean;
    function TryLayoutToDocumentOffset(ALayoutOffset: Integer;
      out ADocumentOffset: Integer): Boolean;
  end;

function BuildTheWordVerseVisualDocument(ADocument: TTheWordVerseDocument;
  ABaseDirection: TTextDirection; ARevision: QWord): IVerseVisualDocument;

function CanApplyTheWordSemanticMark(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;

function CanReplaceTheWordText(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;

function TryTheWordDocumentToLayoutOffset(
  const AVisual: IVerseVisualDocument; ADocumentOffset: Integer;
  AAffinity: TTheWordOffsetAffinity; out ALayoutOffset: Integer): Boolean;

function TryTheWordLayoutToDocumentOffset(
  const AVisual: IVerseVisualDocument; ALayoutOffset: Integer;
  out ADocumentOffset: Integer): Boolean;

function TryTheWordLayoutRangeToDocument(
  const AVisual: IVerseVisualDocument; ALayoutStart, ALayoutLength: Integer;
  out ADocumentStart, ADocumentLength: Integer): Boolean;

implementation

uses
  SysUtils;

type
  TIntArray = array of Integer;
  TSyntheticAnchorKind = (
    tsakNone,
    tsakProtectedSpace,
    tsakTitleBreak
  );
  TSyntheticAnchorArray = array of TSyntheticAnchorKind;

  TTheWordMappedVisualDocument = class(TInterfacedObject,
    IVerseVisualDocument, ITheWordVisualOffsetMap)
  private
    FInner: IVerseVisualDocument;
    FDocumentToLayoutBefore: TIntArray;
    FDocumentToLayoutAfter: TIntArray;
    FLayoutToDocument: TIntArray;
    function GetText: UnicodeString;
    function GetBaseDirection: TTextDirection;
    function GetRevision: QWord;
    function GetSpanCount: Integer;
    function GetSpan(Index: Integer): TVisualSpan;
  public
    constructor Create(const AInner: IVerseVisualDocument;
      const ADocumentToLayoutBefore, ADocumentToLayoutAfter,
      ALayoutToDocument: TIntArray);
    function TryGetUTF16Range(LogicalIndex: Integer; out StartUTF16,
      LengthUTF16: Integer): Boolean;
    function TryGetLogicalIndexAt(UTF16Offset: Integer;
      out LogicalIndex: Integer): Boolean;
    function TryGetInteractiveLogicalIndexAt(UTF16Offset: Integer;
      out LogicalIndex: Integer): Boolean;
    function TryDocumentToLayoutOffset(ADocumentOffset: Integer;
      AAffinity: TTheWordOffsetAffinity; out ALayoutOffset: Integer): Boolean;
    function TryLayoutToDocumentOffset(ALayoutOffset: Integer;
      out ADocumentOffset: Integer): Boolean;
  end;

constructor TTheWordMappedVisualDocument.Create(
  const AInner: IVerseVisualDocument; const ADocumentToLayoutBefore,
  ADocumentToLayoutAfter, ALayoutToDocument: TIntArray);
var
  Index: Integer;
begin
  inherited Create;
  FInner := AInner;
  SetLength(FDocumentToLayoutBefore, Length(ADocumentToLayoutBefore));
  for Index := 0 to High(ADocumentToLayoutBefore) do
    FDocumentToLayoutBefore[Index] := ADocumentToLayoutBefore[Index];
  SetLength(FDocumentToLayoutAfter, Length(ADocumentToLayoutAfter));
  for Index := 0 to High(ADocumentToLayoutAfter) do
    FDocumentToLayoutAfter[Index] := ADocumentToLayoutAfter[Index];
  SetLength(FLayoutToDocument, Length(ALayoutToDocument));
  for Index := 0 to High(ALayoutToDocument) do
    FLayoutToDocument[Index] := ALayoutToDocument[Index];
end;

function TTheWordMappedVisualDocument.GetText: UnicodeString;
begin
  Result := FInner.Text;
end;

function TTheWordMappedVisualDocument.GetBaseDirection: TTextDirection;
begin
  Result := FInner.BaseDirection;
end;

function TTheWordMappedVisualDocument.GetRevision: QWord;
begin
  Result := FInner.Revision;
end;

function TTheWordMappedVisualDocument.GetSpanCount: Integer;
begin
  Result := FInner.SpanCount;
end;

function TTheWordMappedVisualDocument.GetSpan(Index: Integer): TVisualSpan;
begin
  Result := FInner.Spans[Index];
end;

function TTheWordMappedVisualDocument.TryGetUTF16Range(
  LogicalIndex: Integer; out StartUTF16, LengthUTF16: Integer): Boolean;
begin
  Result := FInner.TryGetUTF16Range(LogicalIndex, StartUTF16, LengthUTF16);
end;

function TTheWordMappedVisualDocument.TryGetLogicalIndexAt(
  UTF16Offset: Integer; out LogicalIndex: Integer): Boolean;
begin
  Result := FInner.TryGetLogicalIndexAt(UTF16Offset, LogicalIndex);
end;

function TTheWordMappedVisualDocument.TryGetInteractiveLogicalIndexAt(
  UTF16Offset: Integer; out LogicalIndex: Integer): Boolean;
begin
  Result := FInner.TryGetInteractiveLogicalIndexAt(UTF16Offset,
    LogicalIndex);
end;

function TTheWordMappedVisualDocument.TryDocumentToLayoutOffset(
  ADocumentOffset: Integer; AAffinity: TTheWordOffsetAffinity;
  out ALayoutOffset: Integer): Boolean;
begin
  ALayoutOffset := 0;
  Result := (ADocumentOffset >= 0) and
    (ADocumentOffset < Length(FDocumentToLayoutBefore));
  if not Result then
    Exit;
  if AAffinity = twoaAfter then
    ALayoutOffset := FDocumentToLayoutAfter[ADocumentOffset]
  else
    ALayoutOffset := FDocumentToLayoutBefore[ADocumentOffset];
end;

function TTheWordMappedVisualDocument.TryLayoutToDocumentOffset(
  ALayoutOffset: Integer; out ADocumentOffset: Integer): Boolean;
begin
  ADocumentOffset := 0;
  Result := (ALayoutOffset >= 0) and
    (ALayoutOffset < Length(FLayoutToDocument));
  if Result then
    ADocumentOffset := FLayoutToDocument[ALayoutOffset];
end;

function TryTheWordDocumentToLayoutOffset(
  const AVisual: IVerseVisualDocument; ADocumentOffset: Integer;
  AAffinity: TTheWordOffsetAffinity; out ALayoutOffset: Integer): Boolean;
var
  OffsetMap: ITheWordVisualOffsetMap;
begin
  ALayoutOffset := 0;
  Result := Assigned(AVisual);
  if not Result then
    Exit;
  if Supports(AVisual, ITheWordVisualOffsetMap, OffsetMap) then
    Exit(OffsetMap.TryDocumentToLayoutOffset(ADocumentOffset, AAffinity,
      ALayoutOffset));
  Result := (ADocumentOffset >= 0) and
    (ADocumentOffset <= Length(AVisual.Text));
  if Result then
    ALayoutOffset := ADocumentOffset;
end;

function TryTheWordLayoutToDocumentOffset(
  const AVisual: IVerseVisualDocument; ALayoutOffset: Integer;
  out ADocumentOffset: Integer): Boolean;
var
  OffsetMap: ITheWordVisualOffsetMap;
begin
  ADocumentOffset := 0;
  Result := Assigned(AVisual);
  if not Result then
    Exit;
  if Supports(AVisual, ITheWordVisualOffsetMap, OffsetMap) then
    Exit(OffsetMap.TryLayoutToDocumentOffset(ALayoutOffset,
      ADocumentOffset));
  Result := (ALayoutOffset >= 0) and
    (ALayoutOffset <= Length(AVisual.Text));
  if Result then
    ADocumentOffset := ALayoutOffset;
end;

function TryTheWordLayoutRangeToDocument(
  const AVisual: IVerseVisualDocument; ALayoutStart, ALayoutLength: Integer;
  out ADocumentStart, ADocumentLength: Integer): Boolean;
var
  DocumentFinish: Integer;
begin
  ADocumentStart := 0;
  ADocumentLength := 0;
  Result := (ALayoutStart >= 0) and (ALayoutLength >= 0) and
    Assigned(AVisual) and
    (ALayoutStart <= Length(AVisual.Text)) and
    (ALayoutLength <= Length(AVisual.Text) - ALayoutStart) and
    TryTheWordLayoutToDocumentOffset(AVisual, ALayoutStart,
      ADocumentStart) and
    TryTheWordLayoutToDocumentOffset(AVisual,
      ALayoutStart + ALayoutLength, DocumentFinish) and
    (DocumentFinish >= ADocumentStart);
  if Result then
    ADocumentLength := DocumentFinish - ADocumentStart;
end;

function IsVisualWordCodeUnit(AChar: WideChar): Boolean;
begin
  { Non-ASCII letters and combining marks (notably pointed Hebrew) are word
    material even though the ANSI punctuation set below cannot classify them. }
  if Ord(AChar) >= 128 then
    Exit(True);
  Result := (AChar > ' ') and
    not (AChar in ['.', ',', ';', ':', '!', '?', '(', ')', '[', ']',
      '{', '}', '"', '''', '/', '\', '-']);
end;

function HasProtectedAnchor(ADocument: TTheWordVerseDocument;
  AOffset: Integer): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to ADocument.StructuralItemCount - 1 do
    if ADocument.StructuralItems[Index].Offset = AOffset then
      Exit(True);
  for Index := 0 to ADocument.OpaqueItemCount - 1 do
    if ADocument.OpaqueItems[Index].Offset = AOffset then
      Exit(True);
  Result := False;
end;

procedure PromoteSyntheticAnchor(var AAnchors: TSyntheticAnchorArray;
  AOffset: Integer; AKind: TSyntheticAnchorKind);
begin
  if (AOffset < 0) or (AOffset > High(AAnchors)) then
    Exit;
  if Ord(AKind) > Ord(AAnchors[AOffset]) then
    AAnchors[AOffset] := AKind;
end;

procedure BuildSyntheticAnchors(ADocument: TTheWordVerseDocument;
  const ATitles: TTheWordTitleRangeArray;
  out AAnchors: TSyntheticAnchorArray);
var
  Offset, TitleIndex: Integer;
begin
  SetLength(AAnchors, Length(ADocument.VisibleText) + 1);
  for Offset := 1 to Length(ADocument.VisibleText) - 1 do
    if IsVisualWordCodeUnit(ADocument.VisibleText[Offset]) and
      IsVisualWordCodeUnit(ADocument.VisibleText[Offset + 1]) and
      HasProtectedAnchor(ADocument, Offset) then
      PromoteSyntheticAnchor(AAnchors, Offset, tsakProtectedSpace);

  { Titles are structural blocks. A synthetic line break has no document
    width, but gives every well-formed TS block the same paragraph boundary
    used by the chapter view. One break is enough where consecutive titles
    share a closing/opening anchor. }
  for TitleIndex := 0 to High(ATitles) do
  begin
    if ATitles[TitleIndex].StartOffset > 0 then
      PromoteSyntheticAnchor(AAnchors, ATitles[TitleIndex].StartOffset,
        tsakTitleBreak);
    if ATitles[TitleIndex].EndOffset < Length(ADocument.VisibleText) then
      PromoteSyntheticAnchor(AAnchors, ATitles[TitleIndex].EndOffset,
        tsakTitleBreak);
  end;
end;

procedure BuildOffsetMaps(const AAnchors: TSyntheticAnchorArray;
  ADocumentLength: Integer; out ADocumentToLayoutBefore,
  ADocumentToLayoutAfter, ALayoutToDocument: TIntArray);
var
  DocumentOffset, LayoutOffset, SyntheticCount: Integer;
begin
  SyntheticCount := 0;
  for DocumentOffset := 0 to High(AAnchors) do
    if AAnchors[DocumentOffset] <> tsakNone then
      Inc(SyntheticCount);
  SetLength(ADocumentToLayoutBefore, ADocumentLength + 1);
  SetLength(ADocumentToLayoutAfter, ADocumentLength + 1);
  SetLength(ALayoutToDocument, ADocumentLength + SyntheticCount + 1);

  LayoutOffset := 0;
  for DocumentOffset := 0 to ADocumentLength do
  begin
    ADocumentToLayoutBefore[DocumentOffset] := LayoutOffset;
    ALayoutToDocument[LayoutOffset] := DocumentOffset;
    if AAnchors[DocumentOffset] <> tsakNone then
    begin
      Inc(LayoutOffset);
      ALayoutToDocument[LayoutOffset] := DocumentOffset;
    end;
    ADocumentToLayoutAfter[DocumentOffset] := LayoutOffset;
    if DocumentOffset < ADocumentLength then
    begin
      Inc(LayoutOffset);
      ALayoutToDocument[LayoutOffset] := DocumentOffset + 1;
    end;
  end;
end;

procedure AppendSyntheticAnchor(ABuilder: TVerseVisualDocumentBuilder;
  AKind: TSyntheticAnchorKind; var ASpanIndex: Integer);
begin
  if AKind = tsakNone then
    Exit;
  if AKind = tsakTitleBreak then
    ABuilder.AddText(ASpanIndex, #10, DefaultVisualTextStyle, False,
      'title-break')
  else
    ABuilder.AddText(ASpanIndex, ' ', DefaultVisualTextStyle, False,
      'protected-anchor');
  Inc(ASpanIndex);
end;

function TryGetTitleForSegment(const ATitles: TTheWordTitleRangeArray;
  AStartOffset, AEndOffset: Integer; out ATitle: TTheWordTitleRange): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(ATitles) do
    if (ATitles[Index].EndOffset > ATitles[Index].StartOffset) and
       (ATitles[Index].StartOffset <= AStartOffset) and
       (ATitles[Index].EndOffset >= AEndOffset) then
    begin
      ATitle := ATitles[Index];
      Exit(True);
    end;
  ATitle.StartOffset := 0;
  ATitle.EndOffset := 0;
  ATitle.Level := 0;
  ATitle.OpenOrdinal := 0;
  ATitle.CloseOrdinal := 0;
  Result := False;
end;

procedure ApplyTitleStyle(const ATitle: TTheWordTitleRange;
  var AStyle: TVisualTextStyle; var AAnnotation: UnicodeString);
begin
  case ATitle.Level of
    0:
      begin
        Include(AStyle.Flags, vtsfBold);
        AStyle.FontSizeDelta := 2;
        AStyle.HasForegroundColor := True;
        AStyle.ForegroundColor := LongInt($424242);
      end;
    1:
      begin
        Include(AStyle.Flags, vtsfBold);
        Include(AStyle.Flags, vtsfItalic);
        AStyle.FontSizeDelta := 1;
      end;
    2:
      begin
        Include(AStyle.Flags, vtsfBold);
        Include(AStyle.Flags, vtsfItalic);
      end;
    3:
      Include(AStyle.Flags, vtsfItalic);
  end;
  if AAnnotation <> '' then
    AAnnotation := AAnnotation + ',';
  AAnnotation := AAnnotation + UnicodeString('title-level-') +
    UnicodeString(IntToStr(ATitle.Level));
end;

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
  Boundaries, DocumentToLayoutBefore, DocumentToLayoutAfter,
    LayoutToDocument: TIntArray;
  Titles: TTheWordTitleRangeArray;
  Builder: TVerseVisualDocumentBuilder;
  Index, MarkIndex, SegmentStart, SegmentFinish, SpanIndex: Integer;
  Mark: TTheWordSemanticMark;
  Title: TTheWordTitleRange;
  Style: TVisualTextStyle;
  Annotation, SegmentText: UnicodeString;
  SyntheticAnchors: TSyntheticAnchorArray;
  Inner: IVerseVisualDocument;
  IsTitle: Boolean;
begin
  if not Assigned(ADocument) then
    raise ETheWordOntCodec.Create('Cannot visualize a nil ONT document');

  Boundaries := nil;
  AddBoundary(Boundaries, 0);
  AddBoundary(Boundaries, Length(ADocument.VisibleText));
  CollectTheWordTitleRanges(ADocument, Titles);
  BuildSyntheticAnchors(ADocument, Titles, SyntheticAnchors);
  for Index := 0 to High(SyntheticAnchors) do
    if SyntheticAnchors[Index] <> tsakNone then
      AddBoundary(Boundaries, Index);
  for Index := 0 to High(Titles) do
  begin
    AddBoundary(Boundaries, Titles[Index].StartOffset);
    AddBoundary(Boundaries, Titles[Index].EndOffset);
  end;
  for MarkIndex := 0 to ADocument.MarkCount - 1 do
  begin
    Mark := ADocument.Marks[MarkIndex];
    AddBoundary(Boundaries, Mark.StartOffset);
    AddBoundary(Boundaries, Mark.EndOffset);
  end;

  Builder := TVerseVisualDocumentBuilder.Create(ABaseDirection);
  try
    SpanIndex := 0;
    if Length(Boundaries) = 1 then
      Builder.AddText(0, '', DefaultVisualTextStyle, True)
    else
      for Index := 0 to High(Boundaries) - 1 do
      begin
        SegmentStart := Boundaries[Index];
        SegmentFinish := Boundaries[Index + 1];
        AppendSyntheticAnchor(Builder, SyntheticAnchors[SegmentStart],
          SpanIndex);
        Style := DefaultVisualTextStyle;
        Annotation := '';
        IsTitle := TryGetTitleForSegment(Titles, SegmentStart,
          SegmentFinish, Title);
        if IsTitle then
          ApplyTitleStyle(Title, Style, Annotation);
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
        Builder.AddText(SpanIndex, SegmentText, Style, not IsTitle,
          Annotation);
        Inc(SpanIndex);
      end;
    { A structural anchor at the document end has no following segment from
      which it can be emitted. Emit it explicitly so the visual text and the
      offset maps always describe the same layout length. }
    AppendSyntheticAnchor(Builder,
      SyntheticAnchors[Length(ADocument.VisibleText)], SpanIndex);
    Inner := Builder.Build(ARevision);
    BuildOffsetMaps(SyntheticAnchors, Length(ADocument.VisibleText),
      DocumentToLayoutBefore, DocumentToLayoutAfter, LayoutToDocument);
    Result := TTheWordMappedVisualDocument.Create(Inner,
      DocumentToLayoutBefore, DocumentToLayoutAfter, LayoutToDocument);
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
    Result := (APoint > AStartUTF16) and (APoint < RangeFinish);
end;

function CanApplyTheWordSemanticMark(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  Finish, Index: Integer;
  Titles: TTheWordTitleRangeArray;
begin
  Result := Assigned(ADocument) and
    ADocument.CanApplySemanticRange(AStartUTF16,
      AStartUTF16 + ALengthUTF16);
  if not Result then
    Exit;
  Finish := AStartUTF16 + ALengthUTF16;
  CollectTheWordTitleRanges(ADocument, Titles);
  for Index := 0 to High(Titles) do
    if (AStartUTF16 < Titles[Index].EndOffset) and
       (Finish > Titles[Index].StartOffset) then
      Exit(False);
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
