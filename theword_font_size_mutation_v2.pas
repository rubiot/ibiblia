unit theword_font_size_mutation_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, theword_ont_codec_v2, theword_inline_format_v2;

const
  THEWORD_MIN_AUTHORIAL_SIZE_V2 = -4;
  THEWORD_MAX_AUTHORIAL_SIZE_V2 = 10;
  THEWORD_NEW_FONT_MARK_ORDINAL_V2 = -1;

type
  { Planner output only: every action is scoped to one leaf. This unit does
    not mutate a document and is not a transaction boundary. A future
    executor must snapshot first, then reconcile Mark.OpenOrdinal against the
    same decoded document before applying all leaf actions atomically. }
  TTheWordFontSizeActionKindV2 =
    theword_ont_codec_v2.TTheWordFontSizeActionKindV2;
  TTheWordFontMarkIdentityV2 =
    theword_ont_codec_v2.TTheWordFontMarkIdentityV2;
  TTheWordFontSizeMarkActionV2 =
    theword_ont_codec_v2.TTheWordFontSizeMarkActionV2;
  TTheWordFontSizeMarkActionArrayV2 =
    theword_ont_codec_v2.TTheWordFontSizeMarkActionArrayV2;
  TTheWordFontSizeLeafPlanV2 =
    theword_ont_codec_v2.TTheWordFontSizeLeafPlanV2;
  TTheWordFontSizeLeafPlanArrayV2 =
    theword_ont_codec_v2.TTheWordFontSizeLeafPlanArrayV2;

function TryPlanTheWordRelativeFontSizeV2(
  ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16, ADelta: Integer;
  out APlan: TTheWordFontSizeLeafPlanArrayV2): Boolean;

function TryRewriteTheWordFontSizeOpenV2(
  const AOpenLexeme: UnicodeString;
  AHasSize: Boolean;
  ASize: Integer;
  out ARewritten: UnicodeString;
  out AWrapperNeeded: Boolean): Boolean;

implementation

type
  TIntegerArray = array of Integer;
  TFontMarkArray = array of TTheWordFontMark;

function PointTouchesRange(APoint, AStartUTF16,
  ALengthUTF16: Integer): Boolean;
var
  RangeFinish: Integer;
begin
  RangeFinish := AStartUTF16 + ALengthUTF16;
  if ALengthUTF16 = 0 then
    Result := APoint = AStartUTF16
  else
    Result := (APoint > AStartUTF16) and
      (APoint < RangeFinish);
end;

procedure AddBoundary(var AValues: TIntegerArray; AValue: Integer);
var
  Index, InsertAt: Integer;
begin
  InsertAt := Length(AValues);
  for Index := 0 to High(AValues) do
  begin
    if AValues[Index] = AValue then
      Exit;
    if AValues[Index] > AValue then
    begin
      InsertAt := Index;
      Break;
    end;
  end;

  SetLength(AValues, Length(AValues) + 1);
  for Index := High(AValues) downto InsertAt + 1 do
    AValues[Index] := AValues[Index - 1];
  AValues[InsertAt] := AValue;
end;

procedure AddActiveMark(var AMarks: TFontMarkArray;
  AMark: TTheWordFontMark);
var
  Index, InsertAt: Integer;
begin
  InsertAt := Length(AMarks);
  for Index := 0 to High(AMarks) do
    if AMarks[Index].OpenOrdinal > AMark.OpenOrdinal then
    begin
      InsertAt := Index;
      Break;
    end;

  SetLength(AMarks, Length(AMarks) + 1);
  for Index := High(AMarks) downto InsertAt + 1 do
    AMarks[Index] := AMarks[Index - 1];
  AMarks[InsertAt] := AMark;
end;

function MarkIdentity(AMark: TTheWordFontMark;
  ALeafStart, ALeafEnd: Integer): TTheWordFontMarkIdentityV2;
begin
  if AMark = nil then
  begin
    Result.OpenOrdinal := THEWORD_NEW_FONT_MARK_ORDINAL_V2;
    Result.StartUTF16 := ALeafStart;
    Result.EndUTF16 := ALeafEnd;
  end
  else
  begin
    Result.OpenOrdinal := AMark.OpenOrdinal;
    Result.StartUTF16 := AMark.StartOffset;
    Result.EndUTF16 := AMark.EndOffset;
  end;
end;

procedure AddAction(var AActions: TTheWordFontSizeMarkActionArrayV2;
  AKind: TTheWordFontSizeActionKindV2;
  AMark: TTheWordFontMark;
  ALeafStart, ALeafEnd, ATargetSize: Integer);
var
  ActionIndex: Integer;
begin
  ActionIndex := Length(AActions);
  SetLength(AActions, ActionIndex + 1);
  AActions[ActionIndex].Kind := AKind;
  AActions[ActionIndex].Mark :=
    MarkIdentity(AMark, ALeafStart, ALeafEnd);
  AActions[ActionIndex].LeafStartUTF16 := ALeafStart;
  AActions[ActionIndex].LeafLengthUTF16 :=
    ALeafEnd - ALeafStart;
  AActions[ActionIndex].OriginalHadSize :=
    (AMark <> nil) and AMark.HasSize;
  if AActions[ActionIndex].OriginalHadSize then
  begin
    AActions[ActionIndex].OriginalSize := AMark.RelativeSize;
    AActions[ActionIndex].OriginalWasExplicitZero :=
      AMark.RelativeSize = 0;
    if AActions[ActionIndex].OriginalWasExplicitZero then
      AActions[ActionIndex].OriginalZeroSign :=
        AMark.SizeSignRaw
    else
      AActions[ActionIndex].OriginalZeroSign := #0;
  end
  else
  begin
    AActions[ActionIndex].OriginalSize := 0;
    AActions[ActionIndex].OriginalWasExplicitZero := False;
    AActions[ActionIndex].OriginalZeroSign := #0;
  end;
  AActions[ActionIndex].TargetSize := ATargetSize;
end;

function SelectionTouchesProtectedRange(
  ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  Finish, Index: Integer;
  Titles: TTheWordTitleRangeArray;
begin
  Result := True;
  { CanApplySemanticRange owns the UTF-16 boundary validation, including
    rejection of boundaries between a high and a low surrogate. }
  if not Assigned(ADocument) or
     not ADocument.CanApplySemanticRange(AStartUTF16,
       AStartUTF16 + ALengthUTF16) then
    Exit;

  Finish := AStartUTF16 + ALengthUTF16;
  CollectTheWordTitleRanges(ADocument, Titles);
  for Index := 0 to High(Titles) do
    if (AStartUTF16 < Titles[Index].EndOffset) and
       (Finish > Titles[Index].StartOffset) then
      Exit;

  for Index := 0 to ADocument.OpaqueItemCount - 1 do
    if PointTouchesRange(ADocument.OpaqueItems[Index].Offset,
      AStartUTF16, ALengthUTF16) then
      Exit;

  for Index := 0 to ADocument.StructuralItemCount - 1 do
    if PointTouchesRange(ADocument.StructuralItems[Index].Offset,
      AStartUTF16, ALengthUTF16) then
      Exit;

  for Index := 0 to ADocument.TranslatorNoteCount - 1 do
    if PointTouchesRange(
      ADocument.TranslatorNotes[Index].OffsetUTF16,
      AStartUTF16, ALengthUTF16) then
      Exit;

  Result := False;
end;

procedure BuildLeafPlan(ADocument: TTheWordVerseDocument;
  ALeafStart, ALeafEnd, ADelta: Integer;
  out ALeaf: TTheWordFontSizeLeafPlanV2);
var
  ActiveMarks: TFontMarkArray;
  Carrier, EffectiveMark, Mark: TTheWordFontMark;
  Index: Integer;
begin
  ALeaf.StartUTF16 := ALeafStart;
  ALeaf.LengthUTF16 := ALeafEnd - ALeafStart;
  ALeaf.CurrentEffectiveSize := 0;
  ALeaf.CurrentHadExplicitSize := False;
  ALeaf.CurrentExplicitZeroSign := #0;
  ALeaf.TargetEffectiveSize := 0;
  ALeaf.Carrier := MarkIdentity(nil, ALeafStart, ALeafEnd);
  ALeaf.CarrierIsNew := True;
  ALeaf.Actions := nil;

  for Index := 0 to ADocument.FontMarkCount - 1 do
  begin
    Mark := ADocument.FontMarks[Index];
    if (Mark.StartOffset <= ALeafStart) and
       (ALeafStart < Mark.EndOffset) then
      AddActiveMark(ActiveMarks, Mark);
  end;

  Carrier := nil;
  EffectiveMark := nil;
  if Length(ActiveMarks) > 0 then
    Carrier := ActiveMarks[High(ActiveMarks)];
  for Index := 0 to High(ActiveMarks) do
    if ActiveMarks[Index].HasSize then
      EffectiveMark := ActiveMarks[Index];

  if EffectiveMark <> nil then
  begin
    ALeaf.CurrentEffectiveSize := EffectiveMark.RelativeSize;
    ALeaf.CurrentHadExplicitSize := True;
    if EffectiveMark.RelativeSize = 0 then
      ALeaf.CurrentExplicitZeroSign := EffectiveMark.SizeSignRaw;
  end;
  ALeaf.TargetEffectiveSize :=
    ALeaf.CurrentEffectiveSize + ADelta;
  ALeaf.Carrier := MarkIdentity(Carrier,
    ALeafStart, ALeafEnd);
  ALeaf.CarrierIsNew := Carrier = nil;

  if ALeaf.TargetEffectiveSize = 0 then
  begin
    for Index := 0 to High(ActiveMarks) do
      if ActiveMarks[Index].HasSize then
        AddAction(ALeaf.Actions, twfsakRemoveSize,
          ActiveMarks[Index], ALeafStart, ALeafEnd, 0);
    Exit;
  end;

  for Index := 0 to High(ActiveMarks) do
  begin
    Mark := ActiveMarks[Index];
    if Mark.HasSize and (Mark <> Carrier) then
      AddAction(ALeaf.Actions, twfsakRemoveSize,
        Mark, ALeafStart, ALeafEnd, 0);
  end;

  if Carrier = nil then
    AddAction(ALeaf.Actions, twfsakAddSize, nil,
      ALeafStart, ALeafEnd, ALeaf.TargetEffectiveSize)
  else if Carrier.HasSize then
    AddAction(ALeaf.Actions, twfsakSetSize, Carrier,
      ALeafStart, ALeafEnd, ALeaf.TargetEffectiveSize)
  else
    AddAction(ALeaf.Actions, twfsakAddSize, Carrier,
      ALeafStart, ALeafEnd, ALeaf.TargetEffectiveSize);
end;

function TryPlanTheWordRelativeFontSizeV2(
  ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16, ADelta: Integer;
  out APlan: TTheWordFontSizeLeafPlanArrayV2): Boolean;
var
  Boundaries: TIntegerArray;
  EndUTF16, Index, LeafIndex: Integer;
  LocalPlan: TTheWordFontSizeLeafPlanArrayV2;
  Mark: TTheWordFontMark;
begin
  Result := False;
  APlan := nil;
  if not Assigned(ADocument) or
     ((ADelta <> -1) and (ADelta <> 1)) or
     (ALengthUTF16 <= 0) or
     (AStartUTF16 < 0) or
     (AStartUTF16 > Length(ADocument.VisibleText)) or
     (ALengthUTF16 >
       Length(ADocument.VisibleText) - AStartUTF16) then
    Exit;

  EndUTF16 := AStartUTF16 + ALengthUTF16;
  if SelectionTouchesProtectedRange(ADocument,
    AStartUTF16, ALengthUTF16) then
    Exit;

  AddBoundary(Boundaries, AStartUTF16);
  AddBoundary(Boundaries, EndUTF16);
  for Index := 0 to ADocument.FontMarkCount - 1 do
  begin
    Mark := ADocument.FontMarks[Index];
    if (Mark.StartOffset > AStartUTF16) and
       (Mark.StartOffset < EndUTF16) then
      AddBoundary(Boundaries, Mark.StartOffset);
    if (Mark.EndOffset > AStartUTF16) and
       (Mark.EndOffset < EndUTF16) then
      AddBoundary(Boundaries, Mark.EndOffset);
  end;

  SetLength(LocalPlan, Length(Boundaries) - 1);
  for LeafIndex := 0 to High(LocalPlan) do
  begin
    BuildLeafPlan(ADocument, Boundaries[LeafIndex],
      Boundaries[LeafIndex + 1], ADelta,
      LocalPlan[LeafIndex]);
    if (LocalPlan[LeafIndex].TargetEffectiveSize <
        THEWORD_MIN_AUTHORIAL_SIZE_V2) or
       (LocalPlan[LeafIndex].TargetEffectiveSize >
        THEWORD_MAX_AUTHORIAL_SIZE_V2) then
    begin
      LocalPlan := nil;
      Exit;
    end;
  end;

  if Length(LocalPlan) = 0 then
    Exit;
  APlan := LocalPlan;
  Result := True;
end;

function CanonicalRelativeSize(AValue: Integer): UnicodeString;
begin
  if AValue < 0 then
    Result := IntToStr(AValue)
  else
    Result := '+' + IntToStr(AValue);
end;

function TryRewriteTheWordFontSizeOpenV2(
  const AOpenLexeme: UnicodeString;
  AHasSize: Boolean;
  ASize: Integer;
  out ARewritten: UnicodeString;
  out AWrapperNeeded: Boolean): Boolean;
var
  Attribute: TTheWordInlineFormatAttributeV2;
  ErrorText, SizeText: UnicodeString;
  Index, InsertAt, SizeIndex: Integer;
  Parsed, Reparsed: TTheWordInlineFormatTagV2;
begin
  Result := False;
  ARewritten := '';
  AWrapperNeeded := False;
  if not TryParseTheWordInlineFormatTagV2(AOpenLexeme,
    Parsed, ErrorText) or
     (Parsed.Kind <> twiftv2Font) or Parsed.IsClosing then
    Exit;

  SizeIndex := -1;
  for Index := 0 to High(Parsed.Attributes) do
    if Parsed.Attributes[Index].NameRaw = 'size' then
    begin
      SizeIndex := Index;
      Break;
    end;

  if not AHasSize then
  begin
    if SizeIndex < 0 then
      ARewritten := AOpenLexeme
    else if Length(Parsed.Attributes) = 1 then
    begin
      Result := True;
      Exit;
    end
    else
    begin
      Attribute := Parsed.Attributes[SizeIndex];
      ARewritten := Copy(AOpenLexeme, 1,
        Attribute.FullSpanUTF16.StartUTF16 - 1) +
        Copy(AOpenLexeme,
          Attribute.FullSpanUTF16.StartUTF16 +
          Attribute.FullSpanUTF16.LengthUTF16, MaxInt);
    end;
  end
  else
  begin
    if (ASize < THEWORD_MIN_AUTHORIAL_SIZE_V2) or
       (ASize > THEWORD_MAX_AUTHORIAL_SIZE_V2) or
       (ASize = 0) then
      Exit;
    SizeText := CanonicalRelativeSize(ASize);
    if SizeIndex >= 0 then
    begin
      Attribute := Parsed.Attributes[SizeIndex];
      ARewritten := Copy(AOpenLexeme, 1,
        Attribute.PayloadValueSpanUTF16.StartUTF16 - 1) +
        SizeText +
        Copy(AOpenLexeme,
          Attribute.PayloadValueSpanUTF16.StartUTF16 +
          Attribute.PayloadValueSpanUTF16.LengthUTF16, MaxInt);
    end
    else
    begin
      InsertAt := Parsed.ClosingTriviaSpanUTF16.StartUTF16;
      if InsertAt = 0 then
        InsertAt := Length(AOpenLexeme);
      ARewritten := Copy(AOpenLexeme, 1, InsertAt - 1) +
        ' size=' + SizeText +
        Copy(AOpenLexeme, InsertAt, MaxInt);
    end;
  end;

  if ARewritten = '' then
  begin
    Result := True;
    Exit;
  end;
  if not TryParseTheWordInlineFormatTagV2(ARewritten,
    Reparsed, ErrorText) or
     (Reparsed.Kind <> twiftv2Font) or Reparsed.IsClosing then
    Exit;
  AWrapperNeeded := True;
  Result := True;
end;

end.
