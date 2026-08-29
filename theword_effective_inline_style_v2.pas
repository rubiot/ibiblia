unit theword_effective_inline_style_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses SysUtils, theword_ont_codec_v2;

type
  TTheWordEffectiveInlineFlagV2 = (tweifv2Bold, tweifv2Italic,
    tweifv2Underline, tweifv2Strikeout, tweifv2Superscript, tweifv2Subscript,
    tweifv2AddedWords);
  TTheWordEffectiveInlineFlagsV2 = set of TTheWordEffectiveInlineFlagV2;
  TTheWordEffectiveInlineStyleV2 = record
    Flags: TTheWordEffectiveInlineFlagsV2;
    HasExplicitColor: Boolean;
    ExplicitColor: LongInt;
    HasSize: Boolean;
    FontSizeDeltaPoints: Integer;
  end;
  TTheWordEffectiveInlineRunV2 = record
    { Zero-based code-unit offsets in the decoded document's VisibleText. }
    VisibleStartUTF16: Integer;
    VisibleLengthUTF16: Integer;
    Style: TTheWordEffectiveInlineStyleV2;
  end;
  TTheWordProjectedTranslatorNoteV2 = record
    Id: QWord;
    OffsetUTF16: Integer;
    Ordinal: Integer;
    Q: UnicodeString;
    BodyRaw: UnicodeString;
    OpenLexeme: UnicodeString;
    CloseLexeme: UnicodeString;
    Capability: TTheWordLayerCapability;
  end;
  TTheWordEffectiveInlineProjectionV2 = class
  private
    FVisibleText: UnicodeString;
    FRuns: array of TTheWordEffectiveInlineRunV2;
    FTranslatorNotes: array of TTheWordProjectedTranslatorNoteV2;
    function GetRun(AIndex: Integer): TTheWordEffectiveInlineRunV2;
    function GetRunCount: Integer;
    function GetTranslatorNote(AIndex: Integer):
      TTheWordProjectedTranslatorNoteV2;
    function GetTranslatorNoteCount: Integer;
  public
    function Clone: TTheWordEffectiveInlineProjectionV2;
    class function Resolve(const ARaw: UnicodeString):
      TTheWordEffectiveInlineProjectionV2; overload; static;
    { ADocument remains owned by the caller. }
    class function Resolve(ADocument: TTheWordVerseDocument):
      TTheWordEffectiveInlineProjectionV2; overload; static;
    function TryStyleAtVisibleUTF16(AOffsetUTF16: Integer;
      out AStyle: TTheWordEffectiveInlineStyleV2): Boolean;
    property VisibleText: UnicodeString read FVisibleText;
    property RunCount: Integer read GetRunCount;
    property Runs[AIndex: Integer]: TTheWordEffectiveInlineRunV2 read GetRun;
    property TranslatorNoteCount: Integer read GetTranslatorNoteCount;
    property TranslatorNotes[AIndex: Integer]:
      TTheWordProjectedTranslatorNoteV2 read GetTranslatorNote;
  end;

function DefaultTheWordEffectiveInlineStyleV2: TTheWordEffectiveInlineStyleV2;
function SameTheWordEffectiveInlineStyleV2(const ALeft, ARight:
  TTheWordEffectiveInlineStyleV2): Boolean;
function FadeTheWordExplicitColorV2(AColor: LongInt): LongInt;

implementation

type TIntegerArray = array of Integer;

function TTheWordEffectiveInlineProjectionV2.Clone:
  TTheWordEffectiveInlineProjectionV2;
var
  Index: Integer;
begin
  Result := TTheWordEffectiveInlineProjectionV2.Create;
  try
    Result.FVisibleText := FVisibleText;
    SetLength(Result.FRuns, Length(FRuns));
    for Index := 0 to High(FRuns) do
      Result.FRuns[Index] := FRuns[Index];
    SetLength(Result.FTranslatorNotes, Length(FTranslatorNotes));
    for Index := 0 to High(FTranslatorNotes) do
      Result.FTranslatorNotes[Index] := FTranslatorNotes[Index];
  except
    Result.Free;
    raise;
  end;
end;

function DefaultTheWordEffectiveInlineStyleV2: TTheWordEffectiveInlineStyleV2;
begin
  Result.Flags := [];
  Result.HasExplicitColor := False;
  Result.ExplicitColor := 0;
  Result.HasSize := False;
  Result.FontSizeDeltaPoints := 0;
end;

function SameTheWordEffectiveInlineStyleV2(const ALeft, ARight:
  TTheWordEffectiveInlineStyleV2): Boolean;
begin
  Result := (ALeft.Flags = ARight.Flags) and
    (ALeft.HasExplicitColor = ARight.HasExplicitColor) and
    ((not ALeft.HasExplicitColor) or (ALeft.ExplicitColor = ARight.ExplicitColor)) and
    (ALeft.HasSize = ARight.HasSize) and
    ((not ALeft.HasSize) or
     (ALeft.FontSizeDeltaPoints = ARight.FontSizeDeltaPoints));
end;

function FadeChannel(AValue: LongInt): LongInt;
begin
  Result := AValue + ((255 - AValue) * 163 div 255);
end;

function FadeTheWordExplicitColorV2(AColor: LongInt): LongInt;
begin
  Result := FadeChannel(AColor and $FF) or
    (FadeChannel((AColor shr 8) and $FF) shl 8) or
    (FadeChannel((AColor shr 16) and $FF) shl 16);
end;

procedure AddBoundary(var AValues: TIntegerArray; AValue: Integer);
var I, At: Integer;
begin
  At := Length(AValues);
  for I := 0 to High(AValues) do begin
    if AValues[I] = AValue then Exit;
    if AValues[I] > AValue then begin At := I; Break end;
  end;
  SetLength(AValues, Length(AValues) + 1);
  for I := High(AValues) downto At + 1 do AValues[I] := AValues[I - 1];
  AValues[At] := AValue;
end;

function Contains(AStart, AEnd, AOffset: Integer): Boolean;
begin
  Result := (AStart <= AOffset) and (AOffset < AEnd);
end;

function StyleAt(ADocument: TTheWordVerseDocument; AOffset: Integer):
  TTheWordEffectiveInlineStyleV2;
var
  I, ColorOrdinal, SizeOrdinal, ScriptOrdinal: Integer;
  Font: TTheWordFontMark;
  Mark: TTheWordSemanticMark;
  HasFO, HasFR: Boolean;
begin
  Result := DefaultTheWordEffectiveInlineStyleV2;
  ColorOrdinal := -1; SizeOrdinal := -1; ScriptOrdinal := -1;
  for I := 0 to ADocument.FontMarkCount - 1 do begin
    Font := ADocument.FontMarks[I];
    if not Contains(Font.StartOffset, Font.EndOffset, AOffset) then Continue;
    if Font.HasColor and (Font.OpenOrdinal > ColorOrdinal) then begin
      ColorOrdinal := Font.OpenOrdinal;
      Result.HasExplicitColor := True;
      Result.ExplicitColor := Font.ResolvedColor;
    end;
    if Font.HasSize and (Font.OpenOrdinal > SizeOrdinal) then begin
      SizeOrdinal := Font.OpenOrdinal;
      Result.HasSize := True;
      Result.FontSizeDeltaPoints := Font.RelativeSize * 2;
    end;
  end;
  HasFO := False; HasFR := False;
  for I := 0 to ADocument.MarkCount - 1 do begin
    Mark := ADocument.Marks[I];
    if not Contains(Mark.StartOffset, Mark.EndOffset, AOffset) then Continue;
    case Mark.Kind of
      twsmItalic:
        begin
          Include(Result.Flags, tweifv2Italic);
          Include(Result.Flags, tweifv2AddedWords);
        end;
      twsmHtmlItalic: Include(Result.Flags, tweifv2Italic);
      twsmWordsOfJesus: HasFR := True;
      twsmOldTestamentQuote: begin HasFO := True; Include(Result.Flags, tweifv2Bold) end;
      twsmUnderline, twsmHtmlUnderline: Include(Result.Flags, tweifv2Underline);
      twsmStrikethrough: Include(Result.Flags, tweifv2Strikeout);
      twsmHtmlBold: Include(Result.Flags, tweifv2Bold);
      twsmSuperscript, twsmSubscript:
        if Mark.OpenOrdinal > ScriptOrdinal then begin
          ScriptOrdinal := Mark.OpenOrdinal;
          Exclude(Result.Flags, tweifv2Superscript);
          Exclude(Result.Flags, tweifv2Subscript);
          if Mark.Kind = twsmSuperscript then
            Include(Result.Flags, tweifv2Superscript)
          else Include(Result.Flags, tweifv2Subscript);
        end;
    end;
  end;
  if HasFR then begin
    Result.HasExplicitColor := True; Result.ExplicitColor := $000000FF;
  end else if HasFO then begin
    Result.HasExplicitColor := True; Result.ExplicitColor := $00000000;
  end;
end;

class function TTheWordEffectiveInlineProjectionV2.Resolve(
  const ARaw: UnicodeString): TTheWordEffectiveInlineProjectionV2;
var Codec: ITheWordOntCodecV2; Document: TTheWordVerseDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(ARaw);
  try Result := Resolve(Document) finally Document.Free end;
end;

class function TTheWordEffectiveInlineProjectionV2.Resolve(
  ADocument: TTheWordVerseDocument): TTheWordEffectiveInlineProjectionV2;
var
  B: TIntegerArray;
  I, N: Integer;
  S: TTheWordEffectiveInlineStyleV2;
begin
  if ADocument = nil then raise EArgumentNilException.Create('ADocument');
  Result := TTheWordEffectiveInlineProjectionV2.Create;
  try
    Result.FVisibleText := ADocument.VisibleText;
    SetLength(Result.FTranslatorNotes, ADocument.TranslatorNoteCount);
    for I := 0 to ADocument.TranslatorNoteCount - 1 do
    begin
      Result.FTranslatorNotes[I].Id := ADocument.TranslatorNotes[I].Id;
      Result.FTranslatorNotes[I].OffsetUTF16 :=
        ADocument.TranslatorNotes[I].OffsetUTF16;
      Result.FTranslatorNotes[I].Ordinal :=
        ADocument.TranslatorNotes[I].Ordinal;
      Result.FTranslatorNotes[I].Q := ADocument.TranslatorNotes[I].Q;
      Result.FTranslatorNotes[I].BodyRaw :=
        ADocument.TranslatorNotes[I].BodyRaw;
      Result.FTranslatorNotes[I].OpenLexeme :=
        ADocument.TranslatorNotes[I].OpenLexeme;
      Result.FTranslatorNotes[I].CloseLexeme :=
        ADocument.TranslatorNotes[I].CloseLexeme;
      Result.FTranslatorNotes[I].Capability :=
        ADocument.TranslatorNotes[I].Capability;
    end;
    AddBoundary(B, 0); AddBoundary(B, Length(ADocument.VisibleText));
    for I := 0 to ADocument.MarkCount - 1 do begin
      AddBoundary(B, ADocument.Marks[I].StartOffset);
      AddBoundary(B, ADocument.Marks[I].EndOffset);
    end;
    for I := 0 to ADocument.FontMarkCount - 1 do begin
      AddBoundary(B, ADocument.FontMarks[I].StartOffset);
      AddBoundary(B, ADocument.FontMarks[I].EndOffset);
    end;
    for I := 0 to High(B) - 1 do begin
      if B[I + 1] <= B[I] then Continue;
      S := StyleAt(ADocument, B[I]); N := Length(Result.FRuns);
      if (N > 0) and SameTheWordEffectiveInlineStyleV2(Result.FRuns[N-1].Style, S) and
        (Result.FRuns[N-1].VisibleStartUTF16 +
         Result.FRuns[N-1].VisibleLengthUTF16 = B[I]) then
        Inc(Result.FRuns[N-1].VisibleLengthUTF16, B[I+1] - B[I])
      else begin
        SetLength(Result.FRuns, N + 1);
        Result.FRuns[N].VisibleStartUTF16 := B[I];
        Result.FRuns[N].VisibleLengthUTF16 := B[I+1] - B[I];
        Result.FRuns[N].Style := S;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TTheWordEffectiveInlineProjectionV2.GetRun(AIndex: Integer):
  TTheWordEffectiveInlineRunV2;
begin
  if (AIndex < 0) or (AIndex >= Length(FRuns)) then
    raise ERangeError.CreateFmt('Inline style run index %d is out of range', [AIndex]);
  Result := FRuns[AIndex];
end;

function TTheWordEffectiveInlineProjectionV2.GetRunCount: Integer;
begin Result := Length(FRuns) end;

function TTheWordEffectiveInlineProjectionV2.GetTranslatorNote(
  AIndex: Integer): TTheWordProjectedTranslatorNoteV2;
begin
  if (AIndex < 0) or (AIndex >= Length(FTranslatorNotes)) then
    raise ERangeError.CreateFmt(
      'Projected translator note index %d is out of range', [AIndex]);
  Result := FTranslatorNotes[AIndex];
end;

function TTheWordEffectiveInlineProjectionV2.GetTranslatorNoteCount: Integer;
begin
  Result := Length(FTranslatorNotes);
end;

function TTheWordEffectiveInlineProjectionV2.TryStyleAtVisibleUTF16(
  AOffsetUTF16: Integer; out AStyle: TTheWordEffectiveInlineStyleV2): Boolean;
var I: Integer;
begin
  AStyle := DefaultTheWordEffectiveInlineStyleV2;
  for I := 0 to High(FRuns) do
    if Contains(FRuns[I].VisibleStartUTF16,
      FRuns[I].VisibleStartUTF16 + FRuns[I].VisibleLengthUTF16,
      AOffsetUTF16) then begin AStyle := FRuns[I].Style; Exit(True) end;
  Result := False;
end;

end.
