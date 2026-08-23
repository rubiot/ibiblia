unit theword_font_color_mutation_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, theword_ont_codec_v2, theword_inline_format_v2;

const
  THEWORD_NEW_COLOR_FONT_MARK_ORDINAL_V2 = -1;

type
  TTheWordFontColorActionKindV2 =
    theword_ont_codec_v2.TTheWordFontColorActionKindV2;
  TTheWordFontColorMarkActionV2 =
    theword_ont_codec_v2.TTheWordFontColorMarkActionV2;
  TTheWordFontColorMarkActionArrayV2 =
    theword_ont_codec_v2.TTheWordFontColorMarkActionArrayV2;
  TTheWordFontColorLeafPlanV2 =
    theword_ont_codec_v2.TTheWordFontColorLeafPlanV2;
  TTheWordFontColorLeafPlanArrayV2 =
    theword_ont_codec_v2.TTheWordFontColorLeafPlanArrayV2;

{ Plans an absolute color assignment for every font-topology leaf touched by
  the selection.  AHasColor=False is the visual "Automatic" operation and
  removes only color attributes.  AColor uses the same COLORREF/BGR layout as
  TTheWordFontMark.ResolvedColor. }
function TryPlanTheWordFontColorV2(
  ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer;
  AHasColor: Boolean; AColor: LongInt;
  out APlan: TTheWordFontColorLeafPlanArrayV2): Boolean;

function TheWordFontColorPlanHasChangesV2(
  const APlan: TTheWordFontColorLeafPlanArrayV2): Boolean;

{ Rewrites only the color attribute.  Existing attribute quoting, whitespace,
  order, casing and every non-color attribute remain byte-for-byte unchanged.
  AWrapperNeeded=False means removing color made the font wrapper empty. }
function TryRewriteTheWordFontColorOpenV2(
  const AOpenLexeme: UnicodeString;
  AHasColor: Boolean; AColor: LongInt;
  out ARewritten: UnicodeString;
  out AWrapperNeeded: Boolean): Boolean;

function TheWordCanonicalFontColorV2(AColor: LongInt): UnicodeString;

implementation

type
  TIntegerArray = array of Integer;
  TFontMarkArray = array of TTheWordFontMark;

function IsValidResolvedColor(AColor: LongInt): Boolean;
begin
  Result := (AColor >= 0) and (AColor <= $00FFFFFF);
end;

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
    Result.OpenOrdinal := THEWORD_NEW_COLOR_FONT_MARK_ORDINAL_V2;
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

procedure AddAction(var AActions: TTheWordFontColorMarkActionArrayV2;
  AKind: TTheWordFontColorActionKindV2;
  AMark: TTheWordFontMark;
  ALeafStart, ALeafEnd: Integer;
  ATargetHasColor: Boolean; ATargetColor: LongInt);
var
  ActionIndex: Integer;
begin
  ActionIndex := Length(AActions);
  SetLength(AActions, ActionIndex + 1);
  AActions[ActionIndex].Kind := AKind;
  AActions[ActionIndex].Mark :=
    MarkIdentity(AMark, ALeafStart, ALeafEnd);
  AActions[ActionIndex].LeafStartUTF16 := ALeafStart;
  AActions[ActionIndex].LeafLengthUTF16 := ALeafEnd - ALeafStart;
  AActions[ActionIndex].OriginalHadColor :=
    (AMark <> nil) and AMark.HasColor;
  if AActions[ActionIndex].OriginalHadColor then
  begin
    AActions[ActionIndex].OriginalColor := AMark.ResolvedColor;
    AActions[ActionIndex].OriginalColorRaw := AMark.ColorRaw;
  end
  else
  begin
    AActions[ActionIndex].OriginalColor := 0;
    AActions[ActionIndex].OriginalColorRaw := '';
  end;
  AActions[ActionIndex].TargetHasColor := ATargetHasColor;
  if ATargetHasColor then
    AActions[ActionIndex].TargetColor := ATargetColor
  else
    AActions[ActionIndex].TargetColor := 0;
end;

function SelectionTouchesProtectedRange(
  ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  Finish, Index: Integer;
  Titles: TTheWordTitleRangeArray;
begin
  Result := True;
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
    if PointTouchesRange(ADocument.TranslatorNotes[Index].OffsetUTF16,
      AStartUTF16, ALengthUTF16) then
      Exit;

  Result := False;
end;

procedure BuildLeafPlan(ADocument: TTheWordVerseDocument;
  ALeafStart, ALeafEnd: Integer;
  ATargetHasColor: Boolean; ATargetColor: LongInt;
  out ALeaf: TTheWordFontColorLeafPlanV2);
var
  ActiveMarks: TFontMarkArray;
  Carrier, EffectiveMark, Mark: TTheWordFontMark;
  Index: Integer;
begin
  ALeaf.StartUTF16 := ALeafStart;
  ALeaf.LengthUTF16 := ALeafEnd - ALeafStart;
  ALeaf.CurrentHasExplicitColor := False;
  ALeaf.CurrentEffectiveColor := 0;
  ALeaf.TargetHasExplicitColor := ATargetHasColor;
  if ATargetHasColor then
    ALeaf.TargetEffectiveColor := ATargetColor
  else
    ALeaf.TargetEffectiveColor := 0;
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
    if ActiveMarks[Index].HasColor then
      EffectiveMark := ActiveMarks[Index];

  if EffectiveMark <> nil then
  begin
    ALeaf.CurrentHasExplicitColor := True;
    ALeaf.CurrentEffectiveColor := EffectiveMark.ResolvedColor;
  end;
  ALeaf.Carrier := MarkIdentity(Carrier, ALeafStart, ALeafEnd);
  ALeaf.CarrierIsNew := Carrier = nil;

  { Assigning the already effective presentation is a literal no-op.  In
    particular, redundant nested colors are not normalized opportunistically. }
  if (ALeaf.CurrentHasExplicitColor = ATargetHasColor) and
     (not ATargetHasColor or
      (ALeaf.CurrentEffectiveColor = ATargetColor)) then
    Exit;

  if not ATargetHasColor then
  begin
    for Index := 0 to High(ActiveMarks) do
      if ActiveMarks[Index].HasColor then
        AddAction(ALeaf.Actions, twfcakRemoveColor,
          ActiveMarks[Index], ALeafStart, ALeafEnd, False, 0);
    Exit;
  end;

  for Index := 0 to High(ActiveMarks) do
  begin
    Mark := ActiveMarks[Index];
    if Mark.HasColor and (Mark <> Carrier) then
      AddAction(ALeaf.Actions, twfcakRemoveColor,
        Mark, ALeafStart, ALeafEnd, False, 0);
  end;

  if Carrier = nil then
    AddAction(ALeaf.Actions, twfcakAddColor, nil,
      ALeafStart, ALeafEnd, True, ATargetColor)
  else if Carrier.HasColor then
    AddAction(ALeaf.Actions, twfcakSetColor, Carrier,
      ALeafStart, ALeafEnd, True, ATargetColor)
  else
    AddAction(ALeaf.Actions, twfcakAddColor, Carrier,
      ALeafStart, ALeafEnd, True, ATargetColor);
end;

function TryPlanTheWordFontColorV2(
  ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer;
  AHasColor: Boolean; AColor: LongInt;
  out APlan: TTheWordFontColorLeafPlanArrayV2): Boolean;
var
  Boundaries: TIntegerArray;
  EndUTF16, Index, LeafIndex: Integer;
  LocalPlan: TTheWordFontColorLeafPlanArrayV2;
  Mark: TTheWordFontMark;
begin
  Result := False;
  APlan := nil;
  if not Assigned(ADocument) or
     (AHasColor and not IsValidResolvedColor(AColor)) or
     (ALengthUTF16 <= 0) or
     (AStartUTF16 < 0) or
     (AStartUTF16 > Length(ADocument.VisibleText)) or
     (ALengthUTF16 > Length(ADocument.VisibleText) - AStartUTF16) then
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
    BuildLeafPlan(ADocument, Boundaries[LeafIndex],
      Boundaries[LeafIndex + 1], AHasColor, AColor,
      LocalPlan[LeafIndex]);

  if Length(LocalPlan) = 0 then
    Exit;
  APlan := LocalPlan;
  Result := True;
end;

function TheWordFontColorPlanHasChangesV2(
  const APlan: TTheWordFontColorLeafPlanArrayV2): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(APlan) do
    if Length(APlan[Index].Actions) > 0 then
      Exit(True);
  Result := False;
end;

function HexByte(AValue: Integer): UnicodeString;
const
  DIGITS: UnicodeString = '0123456789ABCDEF';
begin
  SetLength(Result, 2);
  Result[1] := DIGITS[((AValue shr 4) and $F) + 1];
  Result[2] := DIGITS[(AValue and $F) + 1];
end;

function TheWordCanonicalFontColorV2(AColor: LongInt): UnicodeString;
var
  Blue, Green, Red: Integer;
begin
  if not IsValidResolvedColor(AColor) then
    Exit('');
  Red := AColor and $FF;
  Green := (AColor shr 8) and $FF;
  Blue := (AColor shr 16) and $FF;
  Result := '#' + HexByte(Red) + HexByte(Green) + HexByte(Blue);
end;

function TryRewriteTheWordFontColorOpenV2(
  const AOpenLexeme: UnicodeString;
  AHasColor: Boolean; AColor: LongInt;
  out ARewritten: UnicodeString;
  out AWrapperNeeded: Boolean): Boolean;
var
  Attribute: TTheWordInlineFormatAttributeV2;
  ColorText, ErrorText: UnicodeString;
  ColorIndex, Index, InsertAt: Integer;
  Parsed, Reparsed: TTheWordInlineFormatTagV2;
begin
  Result := False;
  ARewritten := '';
  AWrapperNeeded := False;
  if AHasColor and not IsValidResolvedColor(AColor) then
    Exit;
  if not TryParseTheWordInlineFormatTagV2(AOpenLexeme,
    Parsed, ErrorText) or
     (Parsed.Kind <> twiftv2Font) or Parsed.IsClosing then
    Exit;

  ColorIndex := -1;
  for Index := 0 to High(Parsed.Attributes) do
    if Parsed.Attributes[Index].NameRaw = 'color' then
    begin
      ColorIndex := Index;
      Break;
    end;

  if not AHasColor then
  begin
    if ColorIndex < 0 then
      ARewritten := AOpenLexeme
    else if Length(Parsed.Attributes) = 1 then
    begin
      Result := True;
      Exit;
    end
    else
    begin
      Attribute := Parsed.Attributes[ColorIndex];
      ARewritten := Copy(AOpenLexeme, 1,
        Attribute.FullSpanUTF16.StartUTF16 - 1) +
        Copy(AOpenLexeme,
          Attribute.FullSpanUTF16.StartUTF16 +
          Attribute.FullSpanUTF16.LengthUTF16, MaxInt);
    end;
  end
  else
  begin
    if Parsed.HasColor and (Parsed.ResolvedColor = AColor) then
      ARewritten := AOpenLexeme
    else
    begin
      ColorText := TheWordCanonicalFontColorV2(AColor);
      if ColorIndex >= 0 then
      begin
        Attribute := Parsed.Attributes[ColorIndex];
        ARewritten := Copy(AOpenLexeme, 1,
          Attribute.PayloadValueSpanUTF16.StartUTF16 - 1) +
          ColorText +
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
          ' color=' + ColorText +
          Copy(AOpenLexeme, InsertAt, MaxInt);
      end;
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
