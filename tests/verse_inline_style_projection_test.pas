unit verse_inline_style_projection_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry;

type
  TVerseInlineStyleProjectionTests = class(TTestCase)
  published
    procedure ProjectsInlineStyleWithoutChangingAssociationIdentity;
    procedure WordsOfJesusColorWinsOldTestamentQuoteInBothOrders;
    procedure ConsumedBlocksAndEmojiKeepEffectiveStyleOffsets;
    procedure MalformedInnerFontIsHiddenAndOuterStyleSurvives;
    procedure LegacyCallWithoutRawKeepsSyntagmState;
    procedure MonotonicRunCursorKeepsManyTransitionsAligned;
    procedure CodecFailureFallsBackToLegacySyntagmState;
    procedure CodecFailureInConsumedBlockKeepsLogicalSpans;
    procedure NonCodecResolverFailurePropagates;
    procedure EmptyZeroRunAndStrongCountPastLastRunAreSafe;
  end;

implementation

uses
  SysUtils, Graphics, ONTTokenizer, Syntagm, Verse, text_direction,
  verse_visual_document, verse_visual_adapter,
  theword_effective_inline_style_v2, theword_ont_codec_v2;

var
  ResolverCallCount: Integer;

function RaiseCodecError(const ARaw: UnicodeString):
  TTheWordEffectiveInlineProjectionV2;
begin
  raise ETheWordOntCodec.Create('injected codec failure');
end;

function RaiseCommonError(const ARaw: UnicodeString):
  TTheWordEffectiveInlineProjectionV2;
begin
  raise Exception.Create('injected non-codec failure');
end;

function FailSecondProjection(const ARaw: UnicodeString):
  TTheWordEffectiveInlineProjectionV2;
begin
  Inc(ResolverCallCount);
  if ResolverCallCount = 2 then
    raise ETheWordOntCodec.Create('injected consumed-block failure');
  Result := TTheWordEffectiveInlineProjectionV2.Resolve(ARaw);
end;

function FindSyntagmIndex(Syntagms: TSyntagmList;
  const AText: string): Integer;
begin
  for Result := 0 to Syntagms.Count - 1 do
    if Syntagms[Result].Text = AText then
      Exit;
  Result := -1;
end;

function RequiredSpan(const ADocument: IVerseVisualDocument;
  ALogicalIndex: Integer): TVisualSpan;
begin
  if ALogicalIndex < 0 then
    raise Exception.Create('Required logical index does not exist');
  Result := ADocument.Spans[ALogicalIndex];
end;

procedure TVerseInlineStyleProjectionTests.
  ProjectsInlineStyleWithoutChangingAssociationIdentity;
const
  SOURCE_RAW =
    '<font color=blue size=+1><FR><b><i><u><s><sup>a</sup>' +
    '<sub>b</sub></s></u></i></b><FO>c<Fo>d<Fr></font> ' +
    '<font size=+2>outer<font size=+1>inner</font>tail</font> ' +
    '<font color=red><FI>exact<WG123><WTN-NSM><Fi> faded</font> ' +
    'plainAssoc plainUnassoc';
var
  SourceVerse, TargetVerse: TVerse;
  Document: IVerseVisualDocument;
  AIndex, BIndex, CIndex, DIndex, ExactIndex, FadedIndex, FIIndex,
    OuterIndex, InnerIndex, TailIndex, PlainAssocIndex,
    PlainUnassocIndex, Index: Integer;
  ASpan, BSpan, CSpan, DSpan, ExactSpan, FadedSpan,
    PlainAssocSpan, PlainUnassocSpan: TVisualSpan;
  ExactSyntagm, PlainAssocSyntagm, TargetSyntagm: TSyntagm;
  HitIndex, PairCount, SiblingCount: Integer;
begin
  SourceVerse := TVerse.Create;
  TargetVerse := TVerse.Create;
  try
    SourceVerse.PairedVerse := TargetVerse;
    SourceVerse.Text := SOURCE_RAW;
    TargetVerse.Text := 'alvo';
    SourceVerse.Active := True;
    TargetVerse.Active := True;

    AIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'a');
    BIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'b');
    CIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'c');
    DIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'd');
    ExactIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'exact');
    FadedIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'faded');
    OuterIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'outer');
    InnerIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'inner');
    TailIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'tail');
    PlainAssocIndex := FindSyntagmIndex(SourceVerse.Syntagms, 'plainAssoc');
    PlainUnassocIndex := FindSyntagmIndex(SourceVerse.Syntagms,
      'plainUnassoc');
    FIIndex := FindSyntagmIndex(SourceVerse.Syntagms, '<FI>');
    TargetSyntagm := TargetVerse.Syntagms[
      FindSyntagmIndex(TargetVerse.Syntagms, 'alvo')];
    ExactSyntagm := SourceVerse.Syntagms[ExactIndex];
    PlainAssocSyntagm := SourceVerse.Syntagms[PlainAssocIndex];
    ExactSyntagm.AddToSelection;
    PlainAssocSyntagm.AddToSelection;
    TargetSyntagm.AddToSelection;
    SourceVerse.AssociateSyntagms;
    ExactSyntagm.IsPointed := True;
    ExactSyntagm.ToggleStrongHighlight(True);
    PairCount := ExactSyntagm.Pairs.Count;
    SiblingCount := ExactSyntagm.Siblings.Count;

    Document := BuildVerseVisualDocument(SourceVerse.Syntagms, nil,
      tdLeftToRight, 91, clGreen, $A3A3A3, True, SOURCE_RAW);

    AssertEquals('one span per logical index',
      SourceVerse.Syntagms.Count, Document.SpanCount);
    AssertTrue('FI is hidden',
      Document.Spans[FIIndex].Kind = vskHidden);
    for Index := 0 to SourceVerse.Syntagms.Count - 1 do
      if SourceVerse.Syntagms[Index].Kind = skTag then
        AssertTrue('ordinary tags are hidden',
          Document.Spans[Index].Kind = vskHidden);
    AssertFalse('FI brackets are not synthesized',
      Pos('[', UTF8Encode(Document.Text)) > 0);

    ASpan := RequiredSpan(Document, AIndex);
    AssertTrue(vtsfBold in ASpan.Style.Flags);
    AssertTrue(vtsfItalic in ASpan.Style.Flags);
    AssertTrue(vtsfUnderline in ASpan.Style.Flags);
    AssertTrue(vtsfStrikethrough in ASpan.Style.Flags);
    AssertTrue(vtsfSuperscript in ASpan.Style.Flags);
    AssertEquals(2, ASpan.Style.FontSizeDelta);
    AssertEquals('FR overrides font color, then fades',
      FadeTheWordExplicitColorV2($000000FF),
      ASpan.Style.ForegroundColor);

    BSpan := RequiredSpan(Document, BIndex);
    AssertTrue(vtsfSubscript in BSpan.Style.Flags);
    AssertFalse(vtsfSuperscript in BSpan.Style.Flags);
    CSpan := RequiredSpan(Document, CIndex);
    AssertTrue('FO implies bold', vtsfBold in CSpan.Style.Flags);
    AssertEquals('FR overrides FO and font, then fades',
      FadeTheWordExplicitColorV2($000000FF),
      CSpan.Style.ForegroundColor);
    DSpan := RequiredSpan(Document, DIndex);
    AssertEquals('FR restores after FO',
      FadeTheWordExplicitColorV2($000000FF),
      DSpan.Style.ForegroundColor);
    AssertEquals('outer +2 is four points', 4,
      RequiredSpan(Document, OuterIndex).Style.FontSizeDelta);
    AssertEquals('inner +1 replaces outer size', 2,
      RequiredSpan(Document, InnerIndex).Style.FontSizeDelta);
    AssertEquals('outer size restores after inner close', 4,
      RequiredSpan(Document, TailIndex).Style.FontSizeDelta);

    ExactSpan := RequiredSpan(Document, ExactIndex);
    FadedSpan := RequiredSpan(Document, FadedIndex);
    AssertTrue('associated overlay remains',
      vtsfAssociated in ExactSpan.Style.Flags);
    AssertTrue('selected overlay remains',
      vtsfSelected in ExactSpan.Style.Flags);
    AssertTrue('pointed overlay remains',
      vtsfPointed in ExactSpan.Style.Flags);
    AssertTrue('Strong highlight overlay remains',
      vtsfStrongHighlight in ExactSpan.Style.Flags);
    AssertTrue('pointed/Strong underline remains',
      vtsfUnderline in ExactSpan.Style.Flags);
    AssertTrue('bold-with-Strong overlay remains',
      vtsfBold in ExactSpan.Style.Flags);
    AssertTrue('FI applies to content', vtsfItalic in ExactSpan.Style.Flags);
    AssertEquals('associated explicit color remains exact',
      LongInt($000000FF), ExactSpan.Style.ForegroundColor);
    AssertEquals('unassociated explicit color is faded',
      FadeTheWordExplicitColorV2($000000FF),
      FadedSpan.Style.ForegroundColor);
    PlainAssocSpan := RequiredSpan(Document, PlainAssocIndex);
    PlainUnassocSpan := RequiredSpan(Document, PlainUnassocIndex);
    AssertEquals('default associated color is unchanged',
      LongInt(clGreen), PlainAssocSpan.Style.ForegroundColor);
    AssertEquals('default unassociated color is unchanged',
      LongInt($A3A3A3), PlainUnassocSpan.Style.ForegroundColor);
    AssertEquals('WG remains attached', 1, ExactSyntagm.Strong.Count);
    AssertEquals('WT remains attached', 1, ExactSyntagm.Morph.Count);
    AssertEquals('pairs unchanged by projection',
      PairCount, ExactSyntagm.Pairs.Count);
    AssertEquals('siblings unchanged by projection',
      SiblingCount, ExactSyntagm.Siblings.Count);
    AssertTrue('pair pointer remains exact',
      ExactSyntagm.Pairs[0] = TargetSyntagm);
    AssertTrue('hit-test preserves logical index',
      Document.TryGetInteractiveLogicalIndexAt(
        ExactSpan.StartUTF16, HitIndex));
    AssertEquals(ExactIndex, HitIndex);
  finally
    TargetVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  WordsOfJesusColorWinsOldTestamentQuoteInBothOrders;
const
  RAW = '<FO><FR>foOutside<Fr><Fo> ' +
    '<FR><FO>frOutside<Fo><Fr>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  FOOutsideSpan, FROutsideSpan: TVisualSpan;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 911, clGreen, clGray, False, RAW);
    FOOutsideSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'foOutside'));
    FROutsideSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'frOutside'));
    AssertTrue('FO outside FR still supplies bold',
      vtsfBold in FOOutsideSpan.Style.Flags);
    AssertEquals('FR inside FO supplies the faded red color',
      FadeTheWordExplicitColorV2($000000FF),
      FOOutsideSpan.Style.ForegroundColor);
    AssertTrue('FO inside FR still supplies bold',
      vtsfBold in FROutsideSpan.Style.Flags);
    AssertEquals('FR outside FO supplies the faded red color',
      FadeTheWordExplicitColorV2($000000FF),
      FROutsideSpan.Style.ForegroundColor);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  ConsumedBlocksAndEmojiKeepEffectiveStyleOffsets;
const
  RAW = '<font color=blue>before</font>' +
    '<RF>note<Rf><TS>title<Ts><font color=red>😀<b>after</b></font>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  BeforeSpan, EmojiSpan, AfterSpan: TVisualSpan;
  BeforeIndex, EmojiIndex, AfterIndex: Integer;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    BeforeIndex := FindSyntagmIndex(VerseModel.Syntagms, 'before');
    EmojiIndex := FindSyntagmIndex(VerseModel.Syntagms, '😀');
    AfterIndex := FindSyntagmIndex(VerseModel.Syntagms, 'after');
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 92, clGreen, clGray, False, RAW);
    BeforeSpan := RequiredSpan(Document, BeforeIndex);
    EmojiSpan := RequiredSpan(Document, EmojiIndex);
    AfterSpan := RequiredSpan(Document, AfterIndex);
    AssertEquals(FadeTheWordExplicitColorV2($00FF0000),
      BeforeSpan.Style.ForegroundColor);
    AssertEquals(FadeTheWordExplicitColorV2($000000FF),
      EmojiSpan.Style.ForegroundColor);
    AssertEquals('emoji occupies two UTF-16 code units', 2,
      EmojiSpan.LengthUTF16);
    AssertEquals('style after emoji uses the same red run',
      EmojiSpan.Style.ForegroundColor, AfterSpan.Style.ForegroundColor);
    AssertTrue('RF body is not a visible syntagm',
      FindSyntagmIndex(VerseModel.Syntagms, 'note') < 0);
    AssertTrue('TS body is not a visible syntagm',
      FindSyntagmIndex(VerseModel.Syntagms, 'title') < 0);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  LegacyCallWithoutRawKeepsSyntagmState;
const
  RAW = '<FI>legacy<Fi> <font color=red><sup>x</sup></font>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  LegacySpan, XSpan: TVisualSpan;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 94, clGreen, clGray, False);
    LegacySpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'legacy'));
    XSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'x'));
    AssertTrue('legacy IsItalic remains a fallback',
      vtsfItalic in LegacySpan.Style.Flags);
    AssertTrue('legacy IsSuperscript remains a fallback',
      vtsfSuperscript in XSpan.Style.Flags);
    AssertEquals('legacy TextColor remains a fallback',
      LongInt(clRed), XSpan.Style.ForegroundColor);
    AssertFalse('fallback still does not synthesize FI brackets',
      Pos('[', UTF8Encode(Document.Text)) > 0);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  MalformedInnerFontIsHiddenAndOuterStyleSurvives;
const
  RAW = '<font color=green>x<font color=bogus size=99>y</font>z</font>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  XSpan, YSpan, ZSpan: TVisualSpan;
  BadTagIndex: Integer;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 93, clGreen, clGray, False, RAW);
    XSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'x'));
    YSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'y'));
    ZSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'z'));
    AssertEquals(XSpan.Style.ForegroundColor, YSpan.Style.ForegroundColor);
    AssertEquals(XSpan.Style.ForegroundColor, ZSpan.Style.ForegroundColor);
    AssertEquals('outer valid green is faded for unassociated text',
      FadeTheWordExplicitColorV2($00008000),
      XSpan.Style.ForegroundColor);
    BadTagIndex := FindSyntagmIndex(VerseModel.Syntagms,
      '<font color=bogus size=99>');
    AssertTrue('malformed font remains hidden',
      Document.Spans[BadTagIndex].Kind = vskHidden);
    AssertEquals('malformed absolute size is neutral', 0,
      YSpan.Style.FontSizeDelta);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  MonotonicRunCursorKeepsManyTransitionsAligned;
const
  RAW = '<b>a</b><i>b</i><u>c</u><s>d</s><sup>e</sup>' +
    '<sub>f</sub><font size=+2>g<font size=+1>h</font>i</font>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 95, clGreen, clGray, False, RAW);
    AssertTrue(vtsfBold in RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'a')).Style.Flags);
    AssertTrue(vtsfItalic in RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'b')).Style.Flags);
    AssertTrue(vtsfUnderline in RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'c')).Style.Flags);
    AssertTrue(vtsfStrikethrough in RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'd')).Style.Flags);
    AssertTrue(vtsfSuperscript in RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'e')).Style.Flags);
    AssertTrue(vtsfSubscript in RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'f')).Style.Flags);
    AssertEquals(4, RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'g')).Style.FontSizeDelta);
    AssertEquals(2, RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'h')).Style.FontSizeDelta);
    AssertEquals(4, RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'i')).Style.FontSizeDelta);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  CodecFailureFallsBackToLegacySyntagmState;
const
  RAW = '<FI>legacy<Fi> <font color=red><sup>x</sup></font>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  LegacySpan, XSpan: TVisualSpan;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 96, clGreen, clGray, False, RAW, @RaiseCodecError);
    AssertEquals(VerseModel.Syntagms.Count, Document.SpanCount);
    LegacySpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'legacy'));
    XSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'x'));
    AssertTrue(vtsfItalic in LegacySpan.Style.Flags);
    AssertTrue(vtsfSuperscript in XSpan.Style.Flags);
    AssertEquals(LongInt(clRed), XSpan.Style.ForegroundColor);
    AssertTrue(Document.Spans[
      FindSyntagmIndex(VerseModel.Syntagms, '<FI>')].Kind = vskHidden);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  CodecFailureInConsumedBlockKeepsLogicalSpans;
const
  RAW = '<font color=red>before</font><TS>title<Ts>' +
    '<font color=blue><sup>after</sup></font>';
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  TailSpan: TVisualSpan;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := RAW;
    ResolverCallCount := 0;
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 97, clGreen, clGray, False, RAW,
      @FailSecondProjection);
    AssertEquals('failed isolated block keeps one span per logical index',
      VerseModel.Syntagms.Count, Document.SpanCount);
    AssertTrue('consumed title remains hidden after resolver failure',
      Document.Spans[FindSyntagmIndex(VerseModel.Syntagms,
        '<TS>title<Ts>')].Kind = vskHidden);
    TailSpan := RequiredSpan(Document,
      FindSyntagmIndex(VerseModel.Syntagms, 'after'));
    AssertTrue('tail remains interactive', TailSpan.Interactive);
    AssertEquals('tail uses legacy blue after projection is discarded',
      LongInt(clBlue), TailSpan.Style.ForegroundColor);
    AssertTrue('tail uses legacy superscript after projection is discarded',
      vtsfSuperscript in TailSpan.Style.Flags);
    AssertEquals('full raw plus consumed block were attempted', 2,
      ResolverCallCount);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  NonCodecResolverFailurePropagates;
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  RaisedExpected: Boolean;
begin
  VerseModel := TVerse.Create;
  try
    VerseModel.Text := 'x';
    RaisedExpected := False;
    try
      Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
        tdLeftToRight, 98, clGreen, clGray, False, 'x',
        @RaiseCommonError);
    except
      on E: Exception do
      begin
        RaisedExpected := E.Message = 'injected non-codec failure';
      end;
    end;
    AssertTrue('non-codec exceptions must propagate', RaisedExpected);
  finally
    VerseModel.Free;
  end;
end;

procedure TVerseInlineStyleProjectionTests.
  EmptyZeroRunAndStrongCountPastLastRunAreSafe;
var
  VerseModel: TVerse;
  Document: IVerseVisualDocument;
  StrongCount: TSyntagm;
  Token: TSyntagmToken;
begin
  Document := BuildVerseVisualDocument(nil, nil, tdLeftToRight, 99,
    clGreen, clGray, False, '');
  AssertEquals('empty verse has no spans', 0, Document.SpanCount);

  VerseModel := TVerse.Create;
  StrongCount := nil;
  try
    VerseModel.Text := '<FI><Fi>';
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, nil,
      tdLeftToRight, 100, clGreen, clGray, False, '<FI><Fi>');
    AssertEquals('zero-visible projection keeps tag spans',
      VerseModel.Syntagms.Count, Document.SpanCount);

    Token := Default(TSyntagmToken);
    Token.Kind := skStrongCount;
    Token.Value := '1';
    Token.Color := clDefault;
    StrongCount := TSyntagm.Create(Token, VerseModel);
    VerseModel.Text := '<b>x</b>';
    Document := BuildVerseVisualDocument(VerseModel.Syntagms, StrongCount,
      tdLeftToRight, 101, clGreen, clGray, False, '<b>x</b>');
    AssertEquals('StrongCount beyond final run has its own logical span',
      VerseModel.Syntagms.Count + 1, Document.SpanCount);
    AssertEquals('StrongCount falls back to default unassociated color',
      LongInt(clGray),
      Document.Spans[Document.SpanCount - 1].Style.ForegroundColor);
  finally
    StrongCount.Free;
    VerseModel.Free;
  end;
end;

initialization
  RegisterTest(TVerseInlineStyleProjectionTests);

end.
