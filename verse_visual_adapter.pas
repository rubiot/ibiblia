unit verse_visual_adapter;

{$mode objfpc}{$H+}

interface

uses
  Graphics, ONTTokenizer, Syntagm, verse_visual_document,
  theword_effective_inline_style_v2;

type
  TVerseInlineProjectionResolver = function(const ARaw: UnicodeString):
    TTheWordEffectiveInlineProjectionV2;

function BuildVerseVisualDocument(Syntagms: TSyntagmList;
  StrongCount: TSyntagm; BaseDirection: TTextDirection; Revision: QWord;
  AssociatedColor, UnassociatedColor: TColor;
  BoldWordsWithStrong: Boolean; const RawXML: string = '';
  ProjectionResolver: TVerseInlineProjectionResolver = nil):
  IVerseVisualDocument;

implementation

uses
  SysUtils, theword_ont_codec_v2;

function TryResolveProjection(const ARaw: UnicodeString;
  ProjectionResolver: TVerseInlineProjectionResolver;
  out Projection: TTheWordEffectiveInlineProjectionV2): Boolean;
begin
  Projection := nil;
  try
    if Assigned(ProjectionResolver) then
      Projection := ProjectionResolver(ARaw)
    else
      Projection := TTheWordEffectiveInlineProjectionV2.Resolve(ARaw);
    Result := Assigned(Projection);
  except
    on E: ETheWordOntCodec do
    begin
      Projection.Free;
      Projection := nil;
      Result := False;
    end;
  end;
end;

function TryVisibleLengthUTF16(const ARaw: UnicodeString;
  ProjectionResolver: TVerseInlineProjectionResolver;
  out VisibleLengthUTF16: Integer): Boolean;
var
  Projection: TTheWordEffectiveInlineProjectionV2;
begin
  Projection := nil;
  VisibleLengthUTF16 := 0;
  try
    Result := TryResolveProjection(ARaw, ProjectionResolver, Projection);
    if Result then
      VisibleLengthUTF16 := Length(Projection.VisibleText);
  finally
    Projection.Free;
  end;
end;

function TryProjectedStyleAt(
  Projection: TTheWordEffectiveInlineProjectionV2;
  VisibleOffsetUTF16: Integer; var RunIndex: Integer;
  out EffectiveStyle: TTheWordEffectiveInlineStyleV2): Boolean;
var
  Run: TTheWordEffectiveInlineRunV2;
begin
  EffectiveStyle := DefaultTheWordEffectiveInlineStyleV2;
  if not Assigned(Projection) then
    Exit(False);
  if RunIndex < 0 then
    RunIndex := 0;
  while RunIndex < Projection.RunCount do
  begin
    Run := Projection.Runs[RunIndex];
    if Run.VisibleStartUTF16 + Run.VisibleLengthUTF16 <=
       VisibleOffsetUTF16 then
    begin
      Inc(RunIndex);
      Continue;
    end;
    if (Run.VisibleStartUTF16 <= VisibleOffsetUTF16) and
       (VisibleOffsetUTF16 < Run.VisibleStartUTF16 +
        Run.VisibleLengthUTF16) then
    begin
      EffectiveStyle := Run.Style;
      Exit(True);
    end;
    Break;
  end;
  Result := False;
end;

function SyntagmStyle(Syntagm: TSyntagm; AssociatedColor,
  UnassociatedColor: TColor; BoldWordsWithStrong: Boolean;
  const EffectiveStyle: TTheWordEffectiveInlineStyleV2;
  HasProjectedStyle: Boolean): TVisualTextStyle;
var
  Foreground: TColor;
begin
  Result := DefaultVisualTextStyle;
  if HasProjectedStyle then
  begin
    if tweifv2Bold in EffectiveStyle.Flags then
      Include(Result.Flags, vtsfBold);
    if tweifv2Italic in EffectiveStyle.Flags then
      Include(Result.Flags, vtsfItalic);
    if tweifv2Underline in EffectiveStyle.Flags then
      Include(Result.Flags, vtsfUnderline);
    if tweifv2Strikeout in EffectiveStyle.Flags then
      Include(Result.Flags, vtsfStrikethrough);
    if tweifv2Superscript in EffectiveStyle.Flags then
      Include(Result.Flags, vtsfSuperscript);
    if tweifv2Subscript in EffectiveStyle.Flags then
      Include(Result.Flags, vtsfSubscript);
    if EffectiveStyle.HasSize then
      Result.FontSizeDelta := EffectiveStyle.FontSizeDeltaPoints;
  end
  else
  begin
    if Syntagm.IsItalic then
      Include(Result.Flags, vtsfItalic);
    if Syntagm.IsSuperscript then
      Include(Result.Flags, vtsfSuperscript);
  end;
  if BoldWordsWithStrong and Syntagm.HasStrongs then
    Include(Result.Flags, vtsfBold);
  if Syntagm.IsSelected then
  begin
    Include(Result.Flags, vtsfSelected);
    Result.HasBackgroundColor := True;
    Result.BackgroundColor := LongInt(clYellow);
  end;
  if Syntagm.IsAssociated then
    Include(Result.Flags, vtsfAssociated);
  if Syntagm.IsPointed then
  begin
    Include(Result.Flags, vtsfPointed);
    Include(Result.Flags, vtsfUnderline);
    if not Syntagm.IsSelected then
    begin
      Result.HasBackgroundColor := True;
      Result.BackgroundColor := LongInt(clHighlight);
    end;
  end;
  if Syntagm.IsStrongHighlighted then
  begin
    Include(Result.Flags, vtsfStrongHighlight);
    Include(Result.Flags, vtsfUnderline);
  end;

  if HasProjectedStyle and EffectiveStyle.HasExplicitColor then
  begin
    if Syntagm.IsAssociated then
      Foreground := TColor(EffectiveStyle.ExplicitColor)
    else
      Foreground := TColor(FadeTheWordExplicitColorV2(
        EffectiveStyle.ExplicitColor));
  end
  else if Syntagm.TextColor <> clDefault then
    Foreground := Syntagm.TextColor
  else if Syntagm.IsAssociated then
    Foreground := AssociatedColor
  else
    Foreground := UnassociatedColor;
  Result.HasForegroundColor := True;
  Result.ForegroundColor := LongInt(Foreground);
end;

procedure AddSyntagm(Builder: TVerseVisualDocumentBuilder; LogicalIndex: Integer;
  Syntagm: TSyntagm; AssociatedColor, UnassociatedColor: TColor;
  BoldWordsWithStrong: Boolean;
  var Projection: TTheWordEffectiveInlineProjectionV2;
  ProjectionResolver: TVerseInlineProjectionResolver;
  var VisibleOffsetUTF16, ProjectionRunIndex: Integer);
var
  Annotation, DecodedDisplayText: UnicodeString;
  DisplayText: string;
  EffectiveStyle: TTheWordEffectiveInlineStyleV2;
  ConsumedVisibleLengthUTF16: Integer;
  HasProjectedStyle: Boolean;
begin
  if not Assigned(Syntagm) then
  begin
    Builder.AddHidden(LogicalIndex);
    Exit;
  end;

  if (Syntagm.Kind = skTag) or
     (LowerCase(Trim(Syntagm.Text)) = '<rtl>') or
     (LowerCase(Trim(Syntagm.Text)) = '</rtl>') or
     (LowerCase(Trim(Syntagm.Text)) = '<ltr>') or
     (LowerCase(Trim(Syntagm.Text)) = '</ltr>') then
  begin
    Builder.AddMarkup(LogicalIndex, UTF8Decode(Syntagm.Text));
    Exit;
  end;

  if Syntagm.Kind in [skSyntagm, skWhitespace, skPunctuation,
    skStrongCount] then
    DisplayText := Syntagm.Text
  else
  begin
    Builder.AddHidden(LogicalIndex);
    if Assigned(Projection) then
    begin
      if TryVisibleLengthUTF16(UTF8Decode(Syntagm.RawText),
        ProjectionResolver, ConsumedVisibleLengthUTF16) then
        Inc(VisibleOffsetUTF16, ConsumedVisibleLengthUTF16)
      else
      begin
        FreeAndNil(Projection);
        ProjectionRunIndex := 0;
      end;
    end;
    Exit;
  end;

  Annotation := '';
  if Syntagm.HasStrongs then
    Annotation := UTF8Decode(Syntagm.Strong.CommaText);

  EffectiveStyle := DefaultTheWordEffectiveInlineStyleV2;
  HasProjectedStyle := TryProjectedStyleAt(Projection,
    VisibleOffsetUTF16, ProjectionRunIndex, EffectiveStyle);

  DecodedDisplayText := UTF8Decode(DisplayText);
  Builder.AddText(LogicalIndex, DecodedDisplayText,
    SyntagmStyle(Syntagm, AssociatedColor, UnassociatedColor,
      BoldWordsWithStrong, EffectiveStyle, HasProjectedStyle),
    Syntagm.Kind in [skSyntagm, skPunctuation],
    Annotation);
  Inc(VisibleOffsetUTF16, Length(DecodedDisplayText));
end;

function BuildVerseVisualDocument(Syntagms: TSyntagmList;
  StrongCount: TSyntagm; BaseDirection: TTextDirection; Revision: QWord;
  AssociatedColor, UnassociatedColor: TColor;
  BoldWordsWithStrong: Boolean; const RawXML: string;
  ProjectionResolver: TVerseInlineProjectionResolver):
  IVerseVisualDocument;
var
  Builder: TVerseVisualDocumentBuilder;
  Index: Integer;
  Projection: TTheWordEffectiveInlineProjectionV2;
  VisibleOffsetUTF16, ProjectionRunIndex: Integer;
begin
  Builder := TVerseVisualDocumentBuilder.Create(BaseDirection);
  Projection := nil;
  try
    if RawXML <> '' then
      TryResolveProjection(UTF8Decode(RawXML), ProjectionResolver,
        Projection);
    VisibleOffsetUTF16 := 0;
    ProjectionRunIndex := 0;
    if Assigned(Syntagms) then
      for Index := 0 to Syntagms.Count - 1 do
        AddSyntagm(Builder, Index, Syntagms[Index], AssociatedColor,
          UnassociatedColor, BoldWordsWithStrong, Projection,
          ProjectionResolver, VisibleOffsetUTF16, ProjectionRunIndex);
    if Assigned(StrongCount) then
      if Assigned(Syntagms) then
        AddSyntagm(Builder, Syntagms.Count, StrongCount, AssociatedColor,
          UnassociatedColor, BoldWordsWithStrong, Projection,
          ProjectionResolver, VisibleOffsetUTF16, ProjectionRunIndex)
      else
        AddSyntagm(Builder, 0, StrongCount, AssociatedColor,
          UnassociatedColor, BoldWordsWithStrong, Projection,
          ProjectionResolver, VisibleOffsetUTF16, ProjectionRunIndex);
    Result := Builder.Build(Revision);
  finally
    Projection.Free;
    Builder.Free;
  end;
end;

end.
