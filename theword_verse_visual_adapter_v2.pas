unit theword_verse_visual_adapter_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Graphics, verse_visual_document, theword_ont_codec_v2;

type
  TTheWordOffsetAffinity = (twoaBefore, twoaAfter);
  TTheWordVisualOffsetArray = array of Integer;

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
  ABaseDirection: TTextDirection; ARevision: QWord;
  AAutomaticNoteStart: Integer = 0): IVerseVisualDocument;

function CanApplyTheWordSemanticMark(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;

function CanReplaceTheWordText(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;

function TryResolveTheWordInsertionVisualStyle(
  ADocument: TTheWordVerseDocument; AOffsetUTF16: Integer;
  out AStyle: TVisualTextStyle): Boolean;

function TryTheWordDocumentToLayoutOffset(
  const AVisual: IVerseVisualDocument; ADocumentOffset: Integer;
  AAffinity: TTheWordOffsetAffinity; out ALayoutOffset: Integer): Boolean;

function TryTheWordLayoutToDocumentOffset(
  const AVisual: IVerseVisualDocument; ALayoutOffset: Integer;
  out ADocumentOffset: Integer): Boolean;

function TryTheWordLayoutRangeToDocument(
  const AVisual: IVerseVisualDocument; ALayoutStart, ALayoutLength: Integer;
  out ADocumentStart, ADocumentLength: Integer): Boolean;

function TryCollectTheWordParagraphMarkOffsets(
  const AVisual: IVerseVisualDocument; ALayoutStart,
  ALayoutLength: Integer; out ADocumentOffsets:
  TTheWordVisualOffsetArray): Boolean;

implementation

uses
  SysUtils, rf_note_scanner, theword_translator_note_style;

type
  TIntArray = array of Integer;
  TFontFormatRange = record
    StartOffset: Integer;
    EndOffset: Integer;
    OpenOrdinal: Integer;
    HasColor: Boolean;
    Color: LongInt;
    HasSize: Boolean;
    RelativeSize: Integer;
  end;
  TFontFormatRangeArray = array of TFontFormatRange;
  TTranslatorNoteVisual = record
    ObjectId: QWord;
    Ordinal: Integer;
    Marker: UnicodeString;
  end;
  TTranslatorNoteVisualArray = array of TTranslatorNoteVisual;
  TSyntheticAnchorKind = (
    tsakNone,
    tsakProtectedSpace,
    tsakTitleBreak
  );
  TSyntheticAnchor = record
    Kind: TSyntheticAnchorKind;
    ParagraphMarksBeforeBreak: Integer;
    ParagraphMarksAfterBreak: Integer;
    TitleBreakOrdinal: Integer;
    TranslatorNotes: TTranslatorNoteVisualArray;
  end;
  TSyntheticAnchorArray = array of TSyntheticAnchor;

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
    function TryGetSpanAt(UTF16Offset: Integer;
      out Span: TVisualSpan): Boolean;
    function TryGetInteractiveSpanAt(UTF16Offset: Integer;
      out Span: TVisualSpan): Boolean;
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

function TTheWordMappedVisualDocument.TryGetSpanAt(
  UTF16Offset: Integer; out Span: TVisualSpan): Boolean;
begin
  Result := FInner.TryGetSpanAt(UTF16Offset, Span);
end;

function TTheWordMappedVisualDocument.TryGetInteractiveSpanAt(
  UTF16Offset: Integer; out Span: TVisualSpan): Boolean;
begin
  Result := FInner.TryGetInteractiveSpanAt(UTF16Offset, Span);
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

function TryCollectTheWordParagraphMarkOffsets(
  const AVisual: IVerseVisualDocument; ALayoutStart,
  ALayoutLength: Integer; out ADocumentOffsets:
  TTheWordVisualOffsetArray): Boolean;
var
  DocumentOffset, Index, LayoutFinish, MarkerFinish, MarkerStart,
    Offset: Integer;
  Span: TVisualSpan;
begin
  SetLength(ADocumentOffsets, 0);
  Result := Assigned(AVisual) and (ALayoutStart >= 0) and
    (ALayoutLength >= 0) and (ALayoutStart <= Length(AVisual.Text)) and
    (ALayoutLength <= Length(AVisual.Text) - ALayoutStart);
  if not Result then
    Exit;

  LayoutFinish := ALayoutStart + ALayoutLength;
  for Index := 0 to AVisual.SpanCount - 1 do
  begin
    Span := AVisual.Spans[Index];
    if (Span.Annotation <> UnicodeString('paragraph-mark')) or
       (Span.LengthUTF16 <= 0) then
      Continue;
    MarkerStart := Span.StartUTF16;
    if MarkerStart < ALayoutStart then
      MarkerStart := ALayoutStart;
    MarkerFinish := Span.StartUTF16 + Span.LengthUTF16;
    if MarkerFinish > LayoutFinish then
      MarkerFinish := LayoutFinish;
    for Offset := MarkerStart to MarkerFinish - 1 do
    begin
      if not TryTheWordLayoutToDocumentOffset(AVisual, Offset,
        DocumentOffset) then
      begin
        SetLength(ADocumentOffsets, 0);
        Exit(False);
      end;
      SetLength(ADocumentOffsets, Length(ADocumentOffsets) + 1);
      ADocumentOffsets[High(ADocumentOffsets)] := DocumentOffset;
    end;
  end;
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
  if Ord(AKind) > Ord(AAnchors[AOffset].Kind) then
    AAnchors[AOffset].Kind := AKind;
end;

procedure PromoteTitleBreak(var AAnchors: TSyntheticAnchorArray;
  AOffset, AOrdinal: Integer);
begin
  if (AOffset < 0) or (AOffset > High(AAnchors)) then
    Exit;
  if (AAnchors[AOffset].Kind <> tsakTitleBreak) or
     (AOrdinal > AAnchors[AOffset].TitleBreakOrdinal) then
    AAnchors[AOffset].TitleBreakOrdinal := AOrdinal;
  AAnchors[AOffset].Kind := tsakTitleBreak;
end;

procedure AddParagraphMark(var AAnchors: TSyntheticAnchorArray;
  AOffset, AOrdinal: Integer);
begin
  if (AOffset < 0) or (AOffset > High(AAnchors)) then
    Exit;
  if (AAnchors[AOffset].Kind = tsakTitleBreak) and
     (AOrdinal > AAnchors[AOffset].TitleBreakOrdinal) then
    Inc(AAnchors[AOffset].ParagraphMarksAfterBreak)
  else
    Inc(AAnchors[AOffset].ParagraphMarksBeforeBreak);
  { The pilcrow itself separates adjacent words, so a generic protected space
    at the same anchor would only add misleading visual whitespace. }
  if AAnchors[AOffset].Kind = tsakProtectedSpace then
    AAnchors[AOffset].Kind := tsakNone;
end;

function SyntheticAnchorLength(const AAnchor: TSyntheticAnchor): Integer;
var
  Index: Integer;
begin
  Result := AAnchor.ParagraphMarksBeforeBreak +
    AAnchor.ParagraphMarksAfterBreak;
  if AAnchor.Kind <> tsakNone then
    Inc(Result);
  for Index := 0 to High(AAnchor.TranslatorNotes) do
    Inc(Result, Length(AAnchor.TranslatorNotes[Index].Marker));
end;

procedure BuildSyntheticAnchors(ADocument: TTheWordVerseDocument;
  const ATitles: TTheWordTitleRangeArray;
  AAutomaticNoteStart: Integer; out AAnchors: TSyntheticAnchorArray);
var
  InsertAt, Index, NoteIndex, Offset, TitleIndex: Integer;
  Marker: UnicodeString;
  Note: TTheWordTranslatorNote;
  Numberer: TRFNoteNumberer;
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
      PromoteTitleBreak(AAnchors, ATitles[TitleIndex].StartOffset,
        ATitles[TitleIndex].OpenOrdinal);
    if ATitles[TitleIndex].EndOffset < Length(ADocument.VisibleText) then
      PromoteTitleBreak(AAnchors, ATitles[TitleIndex].EndOffset,
        ATitles[TitleIndex].CloseOrdinal);
  end;

  for Index := 0 to ADocument.OpaqueItemCount - 1 do
    if ADocument.OpaqueItems[Index].RawLexeme = '<CM>' then
      AddParagraphMark(AAnchors, ADocument.OpaqueItems[Index].Offset,
        ADocument.OpaqueItems[Index].Ordinal);

  Numberer := TRFNoteNumberer.Create;
  try
    Numberer.Reset(AAutomaticNoteStart);
    for NoteIndex := 0 to ADocument.TranslatorNoteCount - 1 do
    begin
      Note := ADocument.TranslatorNotes[NoteIndex];
      if not Numberer.ResolveMarker(Note.OpenLexeme, Marker) then
        Continue;
      if (Note.OffsetUTF16 < 0) or
         (Note.OffsetUTF16 > High(AAnchors)) then
        Continue;
      InsertAt := Length(AAnchors[Note.OffsetUTF16].TranslatorNotes);
      SetLength(AAnchors[Note.OffsetUTF16].TranslatorNotes, InsertAt + 1);
      for Index := 0 to InsertAt - 1 do
        if AAnchors[Note.OffsetUTF16].TranslatorNotes[Index].Ordinal >
           Note.Ordinal then
        begin
          InsertAt := Index;
          Break;
        end;
      for Index := High(AAnchors[Note.OffsetUTF16].TranslatorNotes) downto
        InsertAt + 1 do
        AAnchors[Note.OffsetUTF16].TranslatorNotes[Index] :=
          AAnchors[Note.OffsetUTF16].TranslatorNotes[Index - 1];
      AAnchors[Note.OffsetUTF16].TranslatorNotes[InsertAt].ObjectId := Note.Id;
      AAnchors[Note.OffsetUTF16].TranslatorNotes[InsertAt].Ordinal :=
        Note.Ordinal;
      AAnchors[Note.OffsetUTF16].TranslatorNotes[InsertAt].Marker := Marker;
    end;
  finally
    Numberer.Free;
  end;
end;

procedure BuildOffsetMaps(const AAnchors: TSyntheticAnchorArray;
  ADocumentLength: Integer; out ADocumentToLayoutBefore,
  ADocumentToLayoutAfter, ALayoutToDocument: TIntArray);
var
  AnchorUnit, DocumentOffset, LayoutOffset, SyntheticCount: Integer;
begin
  SyntheticCount := 0;
  for DocumentOffset := 0 to High(AAnchors) do
    Inc(SyntheticCount, SyntheticAnchorLength(AAnchors[DocumentOffset]));
  SetLength(ADocumentToLayoutBefore, ADocumentLength + 1);
  SetLength(ADocumentToLayoutAfter, ADocumentLength + 1);
  SetLength(ALayoutToDocument, ADocumentLength + SyntheticCount + 1);

  LayoutOffset := 0;
  for DocumentOffset := 0 to ADocumentLength do
  begin
    ADocumentToLayoutBefore[DocumentOffset] := LayoutOffset;
    ALayoutToDocument[LayoutOffset] := DocumentOffset;
    for AnchorUnit := 1 to SyntheticAnchorLength(
      AAnchors[DocumentOffset]) do
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

procedure AppendParagraphMarks(ABuilder: TVerseVisualDocumentBuilder;
  ACount: Integer; var ASpanIndex: Integer);
var
  Index: Integer;
  MarkerText: UnicodeString;
  Style: TVisualTextStyle;
begin
  if ACount <= 0 then
    Exit;
  MarkerText := '';
  for Index := 1 to ACount do
    MarkerText := MarkerText + UnicodeString(WideChar($00B6));
  Style := DefaultVisualTextStyle;
  Style.HasForegroundColor := True;
  Style.ForegroundColor := LongInt($808080);
  ABuilder.AddRoleText(ASpanIndex, MarkerText, Style, False,
    vsrParagraphMark, 0, 'paragraph-mark');
  Inc(ASpanIndex);
end;

procedure AppendSyntheticAnchor(ABuilder: TVerseVisualDocumentBuilder;
  const AAnchor: TSyntheticAnchor; var ASpanIndex: Integer);
var
  Index: Integer;
  Style: TVisualTextStyle;
begin
  { CM and TS may share a zero-width document anchor. Their imported ordinals
    decide which side of the single visual title break receives each pilcrow. }
  AppendParagraphMarks(ABuilder, AAnchor.ParagraphMarksBeforeBreak,
    ASpanIndex);
  if AAnchor.Kind = tsakTitleBreak then
  begin
    ABuilder.AddRoleText(ASpanIndex, #10, DefaultVisualTextStyle, False,
      vsrTitleBreak, 0, 'title-break');
    Inc(ASpanIndex);
  end
  else if AAnchor.Kind = tsakProtectedSpace then
  begin
    ABuilder.AddRoleText(ASpanIndex, ' ', DefaultVisualTextStyle, False,
      vsrProtectedAnchor, 0, 'protected-anchor');
    Inc(ASpanIndex);
  end;
  AppendParagraphMarks(ABuilder, AAnchor.ParagraphMarksAfterBreak,
    ASpanIndex);
  for Index := 0 to High(AAnchor.TranslatorNotes) do
  begin
    Style := DefaultVisualTextStyle;
    Include(Style.Flags, vtsfItalic);
    Include(Style.Flags, vtsfSuperscript);
    Style.FontSizeDelta := -1;
    Style.HasForegroundColor := True;
    Style.ForegroundColor := LongInt(TheWordTranslatorNoteMarkerColor);
    ABuilder.AddObjectText(ASpanIndex,
      AAnchor.TranslatorNotes[Index].Marker, Style, vsrTranslatorNote,
      AAnchor.TranslatorNotes[Index].ObjectId, 'translator-note');
    Inc(ASpanIndex);
  end;
end;

procedure CollectFontFormatRanges(ADocument: TTheWordVerseDocument;
  out ARanges: TFontFormatRangeArray);
var
  Index: Integer;
  Mark: TTheWordFontMark;
begin
  SetLength(ARanges, ADocument.FontMarkCount);
  for Index := 0 to ADocument.FontMarkCount - 1 do
  begin
    Mark := ADocument.FontMarks[Index];
    ARanges[Index].StartOffset := Mark.StartOffset;
    ARanges[Index].EndOffset := Mark.EndOffset;
    ARanges[Index].OpenOrdinal := Mark.OpenOrdinal;
    ARanges[Index].HasColor := Mark.HasColor;
    ARanges[Index].Color := Mark.ResolvedColor;
    ARanges[Index].HasSize := Mark.HasSize;
    ARanges[Index].RelativeSize := Mark.RelativeSize;
  end;
end;
procedure ApplyFontFormatForSegment(const ARanges: TFontFormatRangeArray;
  AStartOffset, AEndOffset: Integer; var AStyle: TVisualTextStyle);
var
  BestColorIndex, BestColorOrdinal, BestSizeIndex, BestSizeOrdinal,
    Index: Integer;
begin
  BestColorIndex := -1;
  BestColorOrdinal := -1;
  BestSizeIndex := -1;
  BestSizeOrdinal := -1;
  for Index := 0 to High(ARanges) do
    if (ARanges[Index].StartOffset <= AStartOffset) and
       (ARanges[Index].EndOffset >= AEndOffset) and
       (ARanges[Index].EndOffset > ARanges[Index].StartOffset) then
    begin
      if ARanges[Index].HasColor and
         (ARanges[Index].OpenOrdinal > BestColorOrdinal) then
      begin
        BestColorIndex := Index;
        BestColorOrdinal := ARanges[Index].OpenOrdinal;
      end;
      if ARanges[Index].HasSize and
         (ARanges[Index].OpenOrdinal > BestSizeOrdinal) then
      begin
        BestSizeIndex := Index;
        BestSizeOrdinal := ARanges[Index].OpenOrdinal;
      end;
    end;
  if BestColorIndex >= 0 then
  begin
    AStyle.HasForegroundColor := True;
    AStyle.ForegroundColor := ARanges[BestColorIndex].Color;
  end;
  if BestSizeIndex >= 0 then
    AStyle.FontSizeDelta := 2 * ARanges[BestSizeIndex].RelativeSize;
end;

procedure ApplyFontFormatForInsertion(const ARanges: TFontFormatRangeArray;
  AOffset: Integer; var AStyle: TVisualTextStyle);
var
  BestColorIndex, BestColorOrdinal, BestSizeIndex, BestSizeOrdinal,
    Index: Integer;
begin
  BestColorIndex := -1;
  BestColorOrdinal := -1;
  BestSizeIndex := -1;
  BestSizeOrdinal := -1;
  for Index := 0 to High(ARanges) do
    if (ARanges[Index].StartOffset < AOffset) and
       (ARanges[Index].EndOffset > AOffset) then
    begin
      if ARanges[Index].HasColor and
         (ARanges[Index].OpenOrdinal > BestColorOrdinal) then
      begin
        BestColorIndex := Index;
        BestColorOrdinal := ARanges[Index].OpenOrdinal;
      end;
      if ARanges[Index].HasSize and
         (ARanges[Index].OpenOrdinal > BestSizeOrdinal) then
      begin
        BestSizeIndex := Index;
        BestSizeOrdinal := ARanges[Index].OpenOrdinal;
      end;
    end;
  if BestColorIndex >= 0 then
  begin
    AStyle.HasForegroundColor := True;
    AStyle.ForegroundColor := ARanges[BestColorIndex].Color;
  end;
  if BestSizeIndex >= 0 then
    AStyle.FontSizeDelta := 2 * ARanges[BestSizeIndex].RelativeSize;
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
    'added-words', 'words-of-jesus', 'old-testament-quote', 'underline',
    'strikethrough', 'superscript', 'subscript',
    'html-bold', 'html-italic', 'html-underline'
  );
begin
  case AMark.Kind of
    twsmItalic, twsmHtmlItalic:
      Include(AStyle.Flags, vtsfItalic);
    twsmOldTestamentQuote, twsmHtmlBold:
      Include(AStyle.Flags, vtsfBold);
    twsmUnderline, twsmHtmlUnderline:
      Include(AStyle.Flags, vtsfUnderline);
    twsmStrikethrough:
      Include(AStyle.Flags, vtsfStrikethrough);
    twsmSuperscript:
      begin
        Exclude(AStyle.Flags, vtsfSubscript);
        Include(AStyle.Flags, vtsfSuperscript);
      end;
    twsmSubscript:
      begin
        Exclude(AStyle.Flags, vtsfSuperscript);
        Include(AStyle.Flags, vtsfSubscript);
      end;
  end;
  if AAnnotation <> '' then
    AAnnotation := AAnnotation + ',';
  AAnnotation := AAnnotation + KIND_NAMES[AMark.Kind];
end;

function TryResolveTheWordInsertionVisualStyle(
  ADocument: TTheWordVerseDocument; AOffsetUTF16: Integer;
  out AStyle: TVisualTextStyle): Boolean;
var
  Annotation: UnicodeString;
  FontFormatRanges: TFontFormatRangeArray;
  HasOldTestamentQuote: Boolean;
  HasWordsOfJesus: Boolean;
  Mark: TTheWordSemanticMark;
  MarkIndex: Integer;
begin
  AStyle := DefaultVisualTextStyle;
  Result := Assigned(ADocument) and (AOffsetUTF16 >= 0) and
    (AOffsetUTF16 <= Length(ADocument.VisibleText));
  if not Result then
    Exit;

  CollectFontFormatRanges(ADocument, FontFormatRanges);
  Annotation := '';
  HasOldTestamentQuote := False;
  HasWordsOfJesus := False;
  ApplyFontFormatForInsertion(FontFormatRanges, AOffsetUTF16, AStyle);
  for MarkIndex := 0 to ADocument.MarkCount - 1 do
  begin
    Mark := ADocument.Marks[MarkIndex];
    { ReplaceVisibleText extends a semantic range only for an insertion
      strictly inside it. At either tag boundary the new text is outside, so
      the caret must advertise the same style that typing will actually use. }
    if (Mark.StartOffset < AOffsetUTF16) and
       (Mark.EndOffset > AOffsetUTF16) then
    begin
      ApplyMarkStyle(Mark, AStyle, Annotation);
      case Mark.Kind of
        twsmWordsOfJesus:
          HasWordsOfJesus := True;
        twsmOldTestamentQuote:
          HasOldTestamentQuote := True;
      end;
    end;
  end;


  if HasWordsOfJesus then
  begin
    AStyle.HasForegroundColor := True;
    AStyle.ForegroundColor := LongInt(clRed);
  end
  else if HasOldTestamentQuote then
  begin
    AStyle.HasForegroundColor := True;
    AStyle.ForegroundColor := LongInt(clBlack);
  end;
end;

function BuildTheWordVerseVisualDocument(ADocument: TTheWordVerseDocument;
  ABaseDirection: TTextDirection; ARevision: QWord;
  AAutomaticNoteStart: Integer): IVerseVisualDocument;
var
  Boundaries, DocumentToLayoutBefore, DocumentToLayoutAfter,
    LayoutToDocument: TIntArray;
  FontFormatRanges: TFontFormatRangeArray;
  Titles: TTheWordTitleRangeArray;
  Builder: TVerseVisualDocumentBuilder;
  Index, MarkIndex, SegmentStart, SegmentFinish, SpanIndex: Integer;
  Mark: TTheWordSemanticMark;
  Title: TTheWordTitleRange;
  Style: TVisualTextStyle;
  Annotation, SegmentText: UnicodeString;
  SyntheticAnchors: TSyntheticAnchorArray;
  Inner: IVerseVisualDocument;
  HasOldTestamentQuote, HasWordsOfJesus, IsTitle: Boolean;
begin
  if not Assigned(ADocument) then
    raise ETheWordOntCodec.Create('Cannot visualize a nil ONT document');

  Boundaries := nil;
  AddBoundary(Boundaries, 0);
  AddBoundary(Boundaries, Length(ADocument.VisibleText));
  CollectTheWordTitleRanges(ADocument, Titles);
  CollectFontFormatRanges(ADocument, FontFormatRanges);
  BuildSyntheticAnchors(ADocument, Titles, AAutomaticNoteStart,
    SyntheticAnchors);
  for Index := 0 to High(SyntheticAnchors) do
    if SyntheticAnchorLength(SyntheticAnchors[Index]) > 0 then
      AddBoundary(Boundaries, Index);
  for Index := 0 to High(Titles) do
  begin
    AddBoundary(Boundaries, Titles[Index].StartOffset);
    AddBoundary(Boundaries, Titles[Index].EndOffset);
  end;
  for Index := 0 to High(FontFormatRanges) do
  begin
    AddBoundary(Boundaries, FontFormatRanges[Index].StartOffset);
    AddBoundary(Boundaries, FontFormatRanges[Index].EndOffset);
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
    begin
      if SyntheticAnchorLength(SyntheticAnchors[0]) = 0 then
        Builder.AddText(0, '', DefaultVisualTextStyle, True);
    end
    else
      for Index := 0 to High(Boundaries) - 1 do
      begin
        SegmentStart := Boundaries[Index];
        SegmentFinish := Boundaries[Index + 1];
        AppendSyntheticAnchor(Builder, SyntheticAnchors[SegmentStart],
          SpanIndex);
        Style := DefaultVisualTextStyle;
        Annotation := '';
        HasOldTestamentQuote := False;
        HasWordsOfJesus := False;
        IsTitle := TryGetTitleForSegment(Titles, SegmentStart,
          SegmentFinish, Title);
        if IsTitle then
          ApplyTitleStyle(Title, Style, Annotation);
        ApplyFontFormatForSegment(FontFormatRanges, SegmentStart,
          SegmentFinish, Style);
        for MarkIndex := 0 to ADocument.MarkCount - 1 do
        begin
          Mark := ADocument.Marks[MarkIndex];
          if (Mark.StartOffset <= SegmentStart) and
             (Mark.EndOffset >= SegmentFinish) and
             (Mark.EndOffset > Mark.StartOffset) then
          begin
            ApplyMarkStyle(Mark, Style, Annotation);
            case Mark.Kind of
              twsmWordsOfJesus:
                HasWordsOfJesus := True;
              twsmOldTestamentQuote:
                HasOldTestamentQuote := True;
            end;
          end;
        end;
        { Semantic colors have an explicit, order-independent priority. FR
          stays red where it overlaps FO; FO still contributes bold. }
        if HasWordsOfJesus then
        begin
          Style.HasForegroundColor := True;
          Style.ForegroundColor := LongInt(clRed);
        end
        else if HasOldTestamentQuote then
        begin
          Style.HasForegroundColor := True;
          Style.ForegroundColor := LongInt(clBlack);
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
