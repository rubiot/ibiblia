unit chapter_inline_style_projection_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Graphics, KMemo,
  HTMLRenderer, ChapterView, theword_ont_codec_v2,
  theword_effective_inline_style_v2, theword_translator_note_style,
  SuggestionManager;

type
  TProjectionFailureMode = (pfmNone, pfmCodec, pfmOther);

  TConsumedCase = record
    OpenTag: string;
    CloseTag: string;
  end;

  THTMLRendererProbe = class(THTMLRenderer)
  private
    FFailureMode: TProjectionFailureMode;
  protected
    function ResolveInlineProjection(const ARaw: UnicodeString):
      TTheWordEffectiveInlineProjectionV2; override;
  public
    property FailureMode: TProjectionFailureMode read FFailureMode
      write FFailureMode;
  end;

  TChapterViewProbe = class(TChapterView)
  public
    procedure AppendTranslationMemoryOccurrence(
      const Occurrence: TTranslationOccurrenceData);
  end;

  TChapterInlineStyleProjectionTest = class(TTestCase)
  private
    FRenderer: THTMLRendererProbe;
    function FindTextBlock(ABlocks: TKMemoBlocks; const AText: string;
      out ABlock: TKMemoTextBlock): Boolean;
    function TextBlock(ARenderer: THTMLRenderer;
      const AText: string): TKMemoTextBlock;
    function FirstHyperlink(ABlocks: TKMemoBlocks;
      out ABlock: TKMemoTextBlock): Boolean;
    function HyperlinkAt(ABlocks: TKMemoBlocks;
      AIndex: Integer): TKMemoHyperlink;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RelativeSizeNestingRestoresFromLocalBase;
    procedure InlineFlagsAndSemanticPrecedenceAreProjected;
    procedure FontColorSizeAreIndependentAndNameIsPreserved;
    procedure EveryConsumedBlockKeepsExactTailOffset;
    procedure MalformedZeroLengthTailAndEmptyAreStable;
    procedure EmojiAndRunBoundariesUseUTF16Offsets;
    procedure BreakAndDefaultStyleRemainIntact;
    procedure CodecFailureFallsBackButOtherExceptionsPropagate;
    procedure ChapterPreservesFIAndSemanticPrecedence;
    procedure ChapterPreservesTitleNoteAndInterlinearBases;
    procedure ChapterInlineTranslatorNoteMarkersAreHoverOnly;
    procedure ChapterTranslatorNoteHoverRequiresExactMarkerInstance;
    procedure ChapterTranslatorNoteHoverInvalidatesPendingRender;
    procedure ChapterOwnsTranslatorNoteMetadata;
    procedure TranslationMemoryOccurrenceHighlightsExactRanges;
    procedure TranslationMemorySnapshotControlsOccurrenceReset;
  end;

implementation

function THTMLRendererProbe.ResolveInlineProjection(const ARaw: UnicodeString):
  TTheWordEffectiveInlineProjectionV2;
begin
  case FFailureMode of
    pfmCodec: raise ETheWordOntCodec.Create('injected codec failure');
    pfmOther: raise Exception.Create('injected ordinary failure');
  end;
  Result := inherited ResolveInlineProjection(ARaw);
end;

procedure TChapterViewProbe.AppendTranslationMemoryOccurrence(
  const Occurrence: TTranslationOccurrenceData);
begin
  AppendTranslationMemoryOccurrenceText(Occurrence);
end;

function TChapterInlineStyleProjectionTest.FindTextBlock(
  ABlocks: TKMemoBlocks; const AText: string;
  out ABlock: TKMemoTextBlock): Boolean;
var
  I: Integer;
begin
  ABlock := nil;
  for I := 0 to ABlocks.Count - 1 do
  begin
    if (ABlocks[I] is TKMemoTextBlock) and
      (ABlocks[I].Text = AText) then
    begin
      ABlock := TKMemoTextBlock(ABlocks[I]);
      Exit(True);
    end;
    if (ABlocks[I] is TKMemoContainer) and
      FindTextBlock(TKMemoContainer(ABlocks[I]).Blocks, AText, ABlock) then
      Exit(True);
  end;
  Result := False;
end;

function TChapterInlineStyleProjectionTest.FirstHyperlink(
  ABlocks: TKMemoBlocks; out ABlock: TKMemoTextBlock): Boolean;
var
  I: Integer;
begin
  ABlock := nil;
  for I := 0 to ABlocks.Count - 1 do
  begin
    if (ABlocks[I] is TKMemoTextBlock) and
      (ABlocks[I].ClassName = 'TKMemoHyperlink') then
    begin
      ABlock := TKMemoTextBlock(ABlocks[I]);
      Exit(True);
    end;
    if (ABlocks[I] is TKMemoContainer) and
      FirstHyperlink(TKMemoContainer(ABlocks[I]).Blocks, ABlock) then
      Exit(True);
  end;
  Result := False;
end;

function TChapterInlineStyleProjectionTest.HyperlinkAt(
  ABlocks: TKMemoBlocks; AIndex: Integer): TKMemoHyperlink;
var
  Remaining: Integer;

  function Scan(AScanBlocks: TKMemoBlocks; var ARemaining: Integer;
    out ALink: TKMemoHyperlink): Boolean;
  var
    BlockIndex: Integer;
  begin
    ALink := nil;
    for BlockIndex := 0 to AScanBlocks.Count - 1 do
    begin
      if AScanBlocks[BlockIndex] is TKMemoHyperlink then
      begin
        if ARemaining = 0 then
        begin
          ALink := TKMemoHyperlink(AScanBlocks[BlockIndex]);
          Exit(True);
        end;
        Dec(ARemaining);
      end;
      if (AScanBlocks[BlockIndex] is TKMemoContainer) and
        Scan(TKMemoContainer(AScanBlocks[BlockIndex]).Blocks,
          ARemaining, ALink) then
        Exit(True);
    end;
    Result := False;
  end;

begin
  Result := nil;
  Remaining := AIndex;
  if (AIndex < 0) or not Scan(ABlocks, Remaining, Result) then
    Fail(Format('Hyperlink %d not found', [AIndex]));
end;

procedure TChapterInlineStyleProjectionTest.SetUp;
begin
  FRenderer := THTMLRendererProbe.Create(nil);
  FRenderer.TextStyle.Font.Name := 'LocalBase';
  FRenderer.TextStyle.Font.Size := 10;
  FRenderer.TextStyle.Font.Color := $00554433;
end;

procedure TChapterInlineStyleProjectionTest.TearDown;
begin
  FRenderer.Free;
end;

function TChapterInlineStyleProjectionTest.TextBlock(
  ARenderer: THTMLRenderer; const AText: string): TKMemoTextBlock;
begin
  if FindTextBlock(ARenderer.Blocks, AText, Result) then
    Exit;
  Fail('Text block not found: ' + AText);
  Result := nil;
end;

procedure TChapterInlineStyleProjectionTest.RelativeSizeNestingRestoresFromLocalBase;
begin
  FRenderer.RenderHTML('<font size=+2>outer<font size=+1>inner</font>tail</font>base');
  AssertEquals(14, TextBlock(FRenderer, 'outer').TextStyle.Font.Size);
  AssertEquals(12, TextBlock(FRenderer, 'inner').TextStyle.Font.Size);
  AssertEquals(14, TextBlock(FRenderer, 'tail').TextStyle.Font.Size);
  AssertEquals(10, TextBlock(FRenderer, 'base').TextStyle.Font.Size);

  FRenderer.TextStyle.Font.Size := 8;
  FRenderer.RenderHTML('<font size=+1>note</font>');
  AssertEquals('relative size uses the local note/comment base', 10,
    TextBlock(FRenderer, 'note').TextStyle.Font.Size);
end;

procedure TChapterInlineStyleProjectionTest.InlineFlagsAndSemanticPrecedenceAreProjected;
var
  B: TKMemoTextBlock;
begin
  FRenderer.RenderHTML(
    '<b><i><u><s><sup>all</sup></s></u></i></b>' +
    '<sub>sub</sub><FR>red<Fr><font color=#00ff00><FO>old<Fo></font>' +
    '<FR><FO>frOutside<Fo><Fr><FO><FR>foOutside<Fr><Fo>' +
    '<FI>added<Fi><FU>under<Fu>');
  B := TextBlock(FRenderer, 'all');
  AssertTrue(fsBold in B.TextStyle.Font.Style);
  AssertTrue(fsItalic in B.TextStyle.Font.Style);
  AssertTrue(fsUnderline in B.TextStyle.Font.Style);
  AssertTrue(fsStrikeOut in B.TextStyle.Font.Style);
  AssertEquals(Ord(tpoSuperscript), Ord(B.TextStyle.ScriptPosition));
  AssertEquals(Ord(tpoSubscript),
    Ord(TextBlock(FRenderer, 'sub').TextStyle.ScriptPosition));
  AssertEquals(LongInt(clRed),
    LongInt(TextBlock(FRenderer, 'red').TextStyle.Font.Color));
  AssertEquals(LongInt(clBlack),
    LongInt(TextBlock(FRenderer, 'old').TextStyle.Font.Color));
  AssertTrue(fsBold in TextBlock(FRenderer, 'old').TextStyle.Font.Style);
  B := TextBlock(FRenderer, 'frOutside');
  AssertEquals('FR outside FO is red', LongInt(clRed),
    LongInt(B.TextStyle.Font.Color));
  AssertTrue('FO inside FR remains bold', fsBold in B.TextStyle.Font.Style);
  B := TextBlock(FRenderer, 'foOutside');
  AssertEquals('FR inside FO is red', LongInt(clRed),
    LongInt(B.TextStyle.Font.Color));
  AssertTrue('FO outside FR remains bold', fsBold in B.TextStyle.Font.Style);
  AssertTrue(fsItalic in TextBlock(FRenderer, 'added').TextStyle.Font.Style);
  AssertTrue(fsUnderline in TextBlock(FRenderer, 'under').TextStyle.Font.Style);
end;

procedure TChapterInlineStyleProjectionTest.FontColorSizeAreIndependentAndNameIsPreserved;
var
  B: TKMemoTextBlock;
begin
  FRenderer.RenderHTML(
    '<font name=Ignored color=#123456 size=+1>styled</font>plain');
  B := TextBlock(FRenderer, 'styled');
  AssertEquals(12, B.TextStyle.Font.Size);
  AssertEquals(LongInt($00563412), LongInt(B.TextStyle.Font.Color));
  AssertEquals('LocalBase', B.TextStyle.Font.Name);
  AssertEquals(LongInt($00554433),
    LongInt(TextBlock(FRenderer, 'plain').TextStyle.Font.Color));
end;

procedure TChapterInlineStyleProjectionTest.EveryConsumedBlockKeepsExactTailOffset;
const
  CASES: array[0..6] of TConsumedCase = (
    (OpenTag: '<Q>'; CloseTag: '<q>'),
    (OpenTag: '<E>'; CloseTag: '<e>'),
    (OpenTag: '<T>'; CloseTag: '<t>'),
    (OpenTag: '<X>'; CloseTag: '<x>'),
    (OpenTag: '<H>'; CloseTag: '<h>'),
    (OpenTag: '<G>'; CloseTag: '<g>'),
    (OpenTag: '<RF>'; CloseTag: '<Rf>'));
var
  I: Integer;
  Hidden: TKMemoTextBlock;
begin
  for I := Low(CASES) to High(CASES) do
  begin
    FRenderer.RenderHTML('<font color=#ff0000>before</font>' +
      CASES[I].OpenTag + '<b>hidden</b>' + CASES[I].CloseTag +
      '<font color=#0000ff>after</font>');
    AssertEquals(LongInt($000000FF),
      LongInt(TextBlock(FRenderer, 'before').TextStyle.Font.Color));
    AssertEquals('tail after consumed block ' + CASES[I].OpenTag,
      LongInt($00FF0000),
      LongInt(TextBlock(FRenderer, 'after').TextStyle.Font.Color));
    AssertFalse(FindTextBlock(FRenderer.Blocks, 'hidden', Hidden));
  end;
end;

procedure TChapterInlineStyleProjectionTest.MalformedZeroLengthTailAndEmptyAreStable;
var
  B: TKMemoTextBlock;
begin
  FRenderer.RenderHTML('<font color=red>A<font color=bogus>B</font>C</font>' +
    '<font size=+0></font>D');
  AssertEquals(LongInt($000000FF),
    LongInt(TextBlock(FRenderer, 'A').TextStyle.Font.Color));
  AssertEquals(LongInt($000000FF),
    LongInt(TextBlock(FRenderer, 'B').TextStyle.Font.Color));
  AssertEquals(LongInt($000000FF),
    LongInt(TextBlock(FRenderer, 'C').TextStyle.Font.Color));
  B := TextBlock(FRenderer, 'D');
  AssertEquals(LongInt($00554433), LongInt(B.TextStyle.Font.Color));
  AssertEquals(10, B.TextStyle.Font.Size);
  FRenderer.RenderHTML('');
  AssertEquals(0, FRenderer.Blocks.Count);
end;

procedure TChapterInlineStyleProjectionTest.EmojiAndRunBoundariesUseUTF16Offsets;
begin
  FRenderer.RenderHTML(
    'lead😀<font size=+1>styled😀</font><font size=+2>tail</font>');
  AssertEquals(10, TextBlock(FRenderer, 'lead😀').TextStyle.Font.Size);
  AssertEquals(12, TextBlock(FRenderer, 'styled😀').TextStyle.Font.Size);
  AssertEquals(14, TextBlock(FRenderer, 'tail').TextStyle.Font.Size);
end;

procedure TChapterInlineStyleProjectionTest.BreakAndDefaultStyleRemainIntact;
begin
  FRenderer.RenderHTML('left<br/>right');
  AssertTrue(FRenderer.Blocks.Count >= 3);
  AssertEquals(10, TextBlock(FRenderer, 'left').TextStyle.Font.Size);
  AssertEquals(LongInt($00554433),
    LongInt(TextBlock(FRenderer, 'right').TextStyle.Font.Color));
end;

procedure TChapterInlineStyleProjectionTest.CodecFailureFallsBackButOtherExceptionsPropagate;
begin
  FRenderer.FailureMode := pfmCodec;
  FRenderer.RenderHTML('<b>fallback</b>');
  AssertFalse(fsBold in
    TextBlock(FRenderer, 'fallback').TextStyle.Font.Style);
  FRenderer.FailureMode := pfmOther;
  try
    FRenderer.RenderHTML('boom');
    Fail('ordinary exceptions must propagate');
  except
    on E: Exception do
      AssertEquals('injected ordinary failure', E.Message);
  end;
end;

procedure TChapterInlineStyleProjectionTest.
  ChapterPreservesFIAndSemanticPrecedence;
var
  Chapter: TChapterView;
  B: TKMemoTextBlock;
begin
  Chapter := TChapterView.Create(nil);
  try
    Chapter.TextStyle.Font.Color := $00554433;
    Chapter.RenderHTML('<FI>added<Fi><i>html</i>' +
      '<FI><FR>red<Fr><Fi><FI><FO>old<Fo><Fi>' +
      '<FI><FR><FO>frOutside<Fo><Fr><Fi>' +
      '<FI><FO><FR>foOutside<Fr><Fo><Fi>' +
      '<FI><font color=#00ff00>green</font><Fi>plain');
    B := TextBlock(Chapter, 'added');
    AssertTrue(fsItalic in B.TextStyle.Font.Style);
    AssertEquals(LongInt(clGray), LongInt(B.TextStyle.Font.Color));
    B := TextBlock(Chapter, 'html');
    AssertTrue(fsItalic in B.TextStyle.Font.Style);
    AssertEquals(LongInt($00554433), LongInt(B.TextStyle.Font.Color));
    AssertEquals(LongInt(clRed),
      LongInt(TextBlock(Chapter, 'red').TextStyle.Font.Color));
    B := TextBlock(Chapter, 'old');
    AssertEquals(LongInt(clBlack), LongInt(B.TextStyle.Font.Color));
    AssertTrue(fsBold in B.TextStyle.Font.Style);
    B := TextBlock(Chapter, 'frOutside');
    AssertEquals(LongInt(clRed), LongInt(B.TextStyle.Font.Color));
    AssertTrue(fsBold in B.TextStyle.Font.Style);
    B := TextBlock(Chapter, 'foOutside');
    AssertEquals(LongInt(clRed), LongInt(B.TextStyle.Font.Color));
    AssertTrue(fsBold in B.TextStyle.Font.Style);
    AssertEquals(LongInt($0000FF00),
      LongInt(TextBlock(Chapter, 'green').TextStyle.Font.Color));
    AssertEquals(LongInt($00554433),
      LongInt(TextBlock(Chapter, 'plain').TextStyle.Font.Color));
  finally
    Chapter.Free;
  end;
end;

procedure TChapterInlineStyleProjectionTest.
  ChapterPreservesTitleNoteAndInterlinearBases;
var
  Chapter: TChapterView;
  B: TKMemoTextBlock;
  BlockIndex: Integer;
begin
  Chapter := TChapterView.Create(nil);
  try
    Chapter.TextStyle.Font.Color := $00554433;
    Chapter.RenderHTML('<TS>title<Ts>tail');
    B := TextBlock(Chapter, 'title');
    AssertEquals(12, B.TextStyle.Font.Size);
    AssertTrue(fsBold in B.TextStyle.Font.Style);
    AssertEquals(LongInt($00424242), LongInt(B.TextStyle.Font.Color));
    B := TextBlock(Chapter, 'tail');
    AssertEquals(10, B.TextStyle.Font.Size);
    AssertEquals(LongInt($00554433), LongInt(B.TextStyle.Font.Color));

    Chapter.RenderHTML(
      '<i><font color=#123456 size=+1><TS>wrapped<Ts>after</font></i>');
    B := TextBlock(Chapter, 'after');
    AssertTrue('inline italic enveloping a title is restored after Ts',
      fsItalic in B.TextStyle.Font.Style);
    AssertEquals('font color enveloping a title is restored after Ts',
      LongInt($00563412), LongInt(B.TextStyle.Font.Color));
    AssertEquals('relative font size enveloping a title is restored after Ts',
      12, B.TextStyle.Font.Size);

    Chapter.RenderHTML('<TS>legacy<Ts><TS1>one<Ts>' +
      '<TS2><u>two</u><Ts>' +
      '<TS3><font color=red>three</font><Ts>');
    B := TextBlock(Chapter, 'legacy');
    AssertEquals('legacy TS is level one', 12, B.TextStyle.Font.Size);
    AssertTrue(fsBold in B.TextStyle.Font.Style);
    AssertEquals(LongInt($00424242), LongInt(B.TextStyle.Font.Color));
    B := TextBlock(Chapter, 'one');
    AssertEquals('explicit TS1 matches legacy TS', 12,
      B.TextStyle.Font.Size);
    AssertTrue(fsBold in B.TextStyle.Font.Style);
    AssertEquals(LongInt($00424242), LongInt(B.TextStyle.Font.Color));
    B := TextBlock(Chapter, 'two');
    AssertEquals('TS2 uses a one-point relative increase', 11,
      B.TextStyle.Font.Size);
    AssertTrue(fsBold in B.TextStyle.Font.Style);
    AssertTrue(fsItalic in B.TextStyle.Font.Style);
    AssertTrue('inline formatting remains visible inside TS2',
      fsUnderline in B.TextStyle.Font.Style);
    B := TextBlock(Chapter, 'three');
    AssertEquals('TS3 keeps the base size', 10, B.TextStyle.Font.Size);
    AssertTrue(fsItalic in B.TextStyle.Font.Style);
    AssertEquals('explicit inline color remains visible inside TS3',
      LongInt(clRed), LongInt(B.TextStyle.Font.Color));
    for BlockIndex := 1 to Chapter.Blocks.Count - 1 do
      AssertFalse('consecutive titles do not create an empty paragraph',
        (Chapter.Blocks[BlockIndex - 1] is TKMemoParagraph) and
        (Chapter.Blocks[BlockIndex] is TKMemoParagraph));

    Chapter.RenderHTML('<RF>note<Rf>tail');
    AssertTrue('translator-note marker hyperlink',
      FirstHyperlink(Chapter.Blocks, B));
    AssertEquals('chapter RF marker uses theWord orange',
      LongInt(TheWordTranslatorNoteMarkerColor),
      LongInt(B.TextStyle.Font.Color));
    AssertTrue(fsItalic in B.TextStyle.Font.Style);
    AssertEquals(11, B.TextStyle.Font.Size);
    AssertEquals(Ord(tpoSuperscript), Ord(B.TextStyle.ScriptPosition));
    AssertEquals(LongInt($00554433),
      LongInt(TextBlock(Chapter, 'tail').TextStyle.Font.Color));

    Chapter.RenderHTML('<E><FI>unit<Fi><e>' +
      '<font color=#0000ff>tail</font>');
    AssertEquals(LongInt($00BE7C41),
      LongInt(TextBlock(Chapter, 'unit').TextStyle.Font.Color));
    AssertEquals(LongInt($00FF0000),
      LongInt(TextBlock(Chapter, 'tail').TextStyle.Font.Color));
  finally
    Chapter.Free;
  end;
end;

procedure TChapterInlineStyleProjectionTest.
  ChapterInlineTranslatorNoteMarkersAreHoverOnly;
var
  Chapter: TChapterView;
  FirstFooterLink: TKMemoHyperlink;
  FirstMarker: TKMemoHyperlink;
  SecondFooterLink: TKMemoHyperlink;
  SecondMarker: TKMemoHyperlink;
begin
  Chapter := TChapterView.Create(nil);
  try
    Chapter.RenderHTML('<RF>first note<Rf>middle<RF>second note<Rf>tail');
    FirstMarker := HyperlinkAt(Chapter.Blocks, 0);
    SecondMarker := HyperlinkAt(Chapter.Blocks, 1);

    AssertFalse('first inline translator-note marker must not handle clicks',
      Assigned(FirstMarker.OnClick));
    AssertFalse('second inline translator-note marker must not handle clicks',
      Assigned(SecondMarker.OnClick));

    Chapter.RenderTranslatorNotesFixture;
    FirstFooterLink := HyperlinkAt(Chapter.Blocks, 2);
    SecondFooterLink := HyperlinkAt(Chapter.Blocks, 3);
    AssertTrue('first footer translator-note link remains clickable',
      Assigned(FirstFooterLink.OnClick));
    AssertTrue('second footer translator-note link remains clickable',
      Assigned(SecondFooterLink.OnClick));
  finally
    Chapter.Free;
  end;
end;

procedure TChapterInlineStyleProjectionTest.
  ChapterTranslatorNoteHoverRequiresExactMarkerInstance;
var
  Chapter: TChapterView;
  FirstId: QWord;
  FirstMarker: TKMemoHyperlink;
  FirstRevision: QWord;
  SecondId: QWord;
  SecondMarker: TKMemoHyperlink;
  SecondRevision: QWord;
  SimilarLink: TKMemoHyperlink;
begin
  Chapter := TChapterView.Create(nil);
  try
    Chapter.RenderHTML('<RF>first note<Rf>middle<RF>second note<Rf>tail');
    FirstMarker := HyperlinkAt(Chapter.Blocks, 0);
    SecondMarker := HyperlinkAt(Chapter.Blocks, 1);
    SimilarLink := Chapter.Blocks.AddHyperlink('unrelated', FirstMarker.URL);

    AssertTrue('first registered marker must resolve',
      Chapter.ResolveTranslatorNoteHoverTargetFixture(FirstMarker,
        FirstRevision, FirstId));
    AssertTrue('second registered marker must resolve',
      Chapter.ResolveTranslatorNoteHoverTargetFixture(SecondMarker,
        SecondRevision, SecondId));
    AssertTrue('both markers belong to the same render revision',
      FirstRevision = SecondRevision);
    AssertTrue('different translator notes must keep different identities',
      FirstId <> SecondId);
    AssertEquals('the unrelated hyperlink must duplicate the marker URL',
      FirstMarker.URL, SimilarLink.URL);
    AssertFalse('an unrelated hyperlink with the same URL must not resolve',
      Chapter.ResolveTranslatorNoteHoverTargetFixture(SimilarLink,
        FirstRevision, FirstId));
  finally
    Chapter.Free;
  end;
end;

procedure TChapterInlineStyleProjectionTest.
  ChapterTranslatorNoteHoverInvalidatesPendingRender;
var
  Chapter: TChapterView;
  InitialRevision: QWord;
  Marker: TKMemoHyperlink;
  NoteId: QWord;
begin
  Chapter := TChapterView.Create(nil);
  try
    Chapter.RenderHTML('<RF>pending note<Rf>tail');
    Marker := HyperlinkAt(Chapter.Blocks, 0);
    AssertTrue(Chapter.ResolveTranslatorNoteHoverTargetFixture(
      Marker, InitialRevision, NoteId));

    Chapter.ViewMode := Chapter.ViewMode;
    AssertTrue('assigning the current mode must keep rendered markers valid',
      Chapter.ResolveTranslatorNoteHoverTargetFixture(
        Marker, InitialRevision, NoteId));

    Chapter.BeginTranslatorNoteHoverFixture(NoteId);
    AssertTrue('hover must remain pending during the initial delay',
      Chapter.TranslatorNotePreviewActiveFixture);
    Chapter.InvalidateTranslatorNotePreviewFixture;

    AssertFalse('a new render revision must cancel pending hover',
      Chapter.TranslatorNotePreviewActiveFixture);
    AssertTrue('invalidation must advance the render revision',
      Chapter.TranslatorNoteRevisionFixture <> InitialRevision);
  finally
    Chapter.Free;
  end;
end;

procedure TChapterInlineStyleProjectionTest.
  ChapterOwnsTranslatorNoteMetadata;
var
  Baseline: Integer;
  Chapter: TChapterView;
begin
  Baseline := TChapterView.TranslatorNoteLiveCountFixture;
  Chapter := TChapterView.Create(nil);
  try
    Chapter.RenderHTML('<RF>first<Rf>text<RF>second<Rf>');
    AssertEquals('both rendered note records are alive', Baseline + 2,
      TChapterView.TranslatorNoteLiveCountFixture);

    Chapter.ClearTranslatorNotesFixture;
    AssertEquals('discarding a rendered document releases note metadata',
      Baseline, TChapterView.TranslatorNoteLiveCountFixture);

    Chapter.RenderHTML('<RF>replacement<Rf>');
    AssertEquals('a replacement document owns only its own metadata',
      Baseline + 1, TChapterView.TranslatorNoteLiveCountFixture);
  finally
    Chapter.Free;
  end;
  AssertEquals('destroying the chapter view releases owned note records',
    Baseline, TChapterView.TranslatorNoteLiveCountFixture);
end;

procedure TChapterInlineStyleProjectionTest.
  TranslationMemoryOccurrenceHighlightsExactRanges;
const
  DESTINATION_TEXT = 'um vaso como um grande lençol';
var
  Chapter: TChapterViewProbe;
  Occurrence: TTranslationOccurrenceData;

  function TextBlockCount: Integer;
  var
    BlockIndex: Integer;
  begin
    Result := 0;
    for BlockIndex := 0 to Chapter.Blocks.Count - 1 do
      if Chapter.Blocks[BlockIndex].ClassType = TKMemoTextBlock then
        Inc(Result);
  end;

  function TextBlockAt(AIndex: Integer): TKMemoTextBlock;
  var
    BlockIndex: Integer;
    TextIndex: Integer;
  begin
    TextIndex := 0;
    for BlockIndex := 0 to Chapter.Blocks.Count - 1 do
      if Chapter.Blocks[BlockIndex].ClassType = TKMemoTextBlock then
      begin
        if TextIndex = AIndex then
          Exit(TKMemoTextBlock(Chapter.Blocks[BlockIndex]));
        Inc(TextIndex);
      end;
    Fail(Format('Text block %d not found', [AIndex]));
    Result := nil;
  end;

  procedure AssertBlock(AIndex: Integer; const AText: string;
    AStyle: TFontStyles);
  var
    Block: TKMemoTextBlock;
  begin
    Block := TextBlockAt(AIndex);
    AssertEquals(AText, Block.Text);
    AssertTrue('unexpected block style',
      Block.TextStyle.Font.Style = AStyle);
  end;

begin
  Chapter := TChapterViewProbe.Create(nil);
  try
    Chapter.Blocks.Clear;
    Occurrence := Default(TTranslationOccurrenceData);
    Occurrence.DestinationText := DESTINATION_TEXT;
    SetLength(Occurrence.HighlightRanges, 2);
    Occurrence.HighlightRanges[0].StartIndex := 14;
    Occurrence.HighlightRanges[0].Length := 2;
    Occurrence.HighlightRanges[1].StartIndex := 24;
    Occurrence.HighlightRanges[1].Length :=
      Length(UTF8Encode(UnicodeString('lençol')));

    Chapter.AppendTranslationMemoryOccurrence(Occurrence);
    AssertEquals(5, TextBlockCount);
    AssertBlock(0, ' — ', []);
    AssertBlock(1, 'um vaso como ', []);
    AssertBlock(2, 'um', [fsBold, fsUnderline]);
    AssertBlock(3, ' grande ', []);
    AssertBlock(4, 'lençol', [fsBold, fsUnderline]);

    Chapter.Blocks.Clear;
    SetLength(Occurrence.HighlightRanges, 1);
    Occurrence.HighlightRanges[0].StartIndex := 999;
    Occurrence.HighlightRanges[0].Length := 2;
    Chapter.AppendTranslationMemoryOccurrence(Occurrence);
    AssertEquals(2, TextBlockCount);
    AssertBlock(0, ' — ', []);
    AssertBlock(1, DESTINATION_TEXT, []);

    Chapter.Blocks.Clear;
    SetLength(Occurrence.HighlightRanges, 0);
    Chapter.AppendTranslationMemoryOccurrence(Occurrence);
    AssertEquals(2, TextBlockCount);
    AssertBlock(0, ' — ', []);
    AssertBlock(1, DESTINATION_TEXT, []);
  finally
    Chapter.Free;
  end;
end;

procedure TChapterInlineStyleProjectionTest.
  TranslationMemorySnapshotControlsOccurrenceReset;
var
  Current: TTranslationMemoryInputSnapshot;
  Previous: TTranslationMemoryInputSnapshot;

  procedure AssertInputChangeResets(const AMessage: string;
    const AChanged: TTranslationMemoryInputSnapshot);
  begin
    AssertFalse(AMessage + ' must invalidate the snapshot',
      TranslationMemoryInputSnapshotsEqual(Previous, AChanged));
    AssertTrue(AMessage + ' must close occurrence details',
      ShouldResetTranslationMemoryOccurrences(True, Previous, AChanged));
    AssertFalse(AMessage + ' must keep the main list mode',
      ShouldResetTranslationMemoryOccurrences(False, Previous, AChanged));
  end;

begin
  Previous := Default(TTranslationMemoryInputSnapshot);
  Previous.Initialized := True;
  Previous.ReferenceID := '40,14,16';
  Previous.SourceXML := 'chreia<WG5532>';
  Previous.DestinationXML := 'é mister';
  Previous.Associations := '0=0';

  Current := Previous;
  AssertTrue('unchanged inputs must keep the same snapshot',
    TranslationMemoryInputSnapshotsEqual(Previous, Current));
  AssertFalse('status-only refresh must preserve occurrence details',
    ShouldResetTranslationMemoryOccurrences(True, Previous, Current));

  Current := Previous;
  Current.ReferenceID := '40,14,17';
  AssertInputChangeResets('reference change', Current);

  Current := Previous;
  Current.SourceXML := 'necessidade<WG5532>';
  AssertInputChangeResets('source text change', Current);

  Current := Previous;
  Current.DestinationXML := 'é necessário';
  AssertInputChangeResets('destination text change', Current);

  Current := Previous;
  Current.Associations := '0=1';
  AssertInputChangeResets('association change', Current);

  Current := Previous;
  Current.Initialized := False;
  AssertInputChangeResets('snapshot reset', Current);
end;

initialization
  RegisterTest(TChapterInlineStyleProjectionTest);

end.
