unit theword_clipboard_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTheWordClipboardV2Test = class(TTestCase)
  private
    procedure AssertClipboardPayloadRejected(const APayload: string);
  published
    procedure SemanticPayloadIsDeterministicAndPreferred;
    procedure HtmlSemanticKindsRoundTripThroughPartialClipboardCopy;
    procedure VersionOnePayloadRemainsReadableWithoutFonts;
    procedure VersionTwoFontPayloadPreservesLosslessLexemes;
    procedure SerializerUsesExactRootAndDenseRelativeOrders;
    procedure VersionTwoFontNestingIsValidated;
    procedure VersionTwoFontFieldsAreExact;
    procedure VersionTwoLimitsAndUTF16BoundariesAreValidated;
    procedure CloseBeforeLexicalPayloadNormalizesAndReencodes;
    procedure VersionOneHistoricalOrdersAndLargeRawStillPaste;
    procedure MalformedFontRemainsAnOpaqueObject;
    procedure SemanticFontLexicalOrderRoundTrips;
    procedure LargeLegacyObjectOrderIsDeterministic;
    procedure CorruptPayloadFallsBackToUnicodePlainText;
    procedure EmojiRangesRemainUTF16Based;
    procedure FullyContainedObjectsRoundTrip;
    procedure PartialLexicalCutIsRejectedAndFullCutIsAtomic;
    procedure FontMarksSurviveCutAndUndoAtomically;
    procedure CanonicalProtectionIncludesStructuralAndLexicalRanges;
    procedure ChangesAreSemanticOnlyIsStrict;
  end;

implementation

uses
  fpjson, jsonparser, ontparser, ONTTokenizer, syntagm, theword_ont_codec_v2,
  theword_clipboard_v2, theword_wysiwyg_session_v2;

procedure TTheWordClipboardV2Test.SemanticPayloadIsDeterministicAndPreferred;
var
  Source, Target: TTheWordWysiwygSessionV2;
  Plain: UnicodeString;
  Payload, SecondPayload: string;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  Source := TTheWordWysiwygSessionV2.Create(
    '<FI>ab<Fi><FR>cd<Fr><s>e</s><sup>f</sup><sub>g</sub>');
  Target := TTheWordWysiwygSessionV2.Create('XY');
  try
    AssertTrue(Source.CopySelection(0, 7, Plain, Payload));
    AssertTrue(Source.CopySelection(0, 7, Plain, SecondPayload));
    AssertEquals(Payload, SecondPayload);
    AssertTrue(Pos('ibiblia.theword-semantic-fragment', Payload) > 0);
    AssertTrue(Target.Paste(1, 0, Payload, 'wrong', Inserted, UsedPrivate));
    AssertTrue(UsedPrivate);
    AssertEquals(7, Inserted);
    AssertEquals(UnicodeString('XabcdefgY'), Target.VisibleText);
    AssertTrue(Pos('<FI>ab<Fi>', Target.RawText) > 0);
    AssertTrue(Pos('<FR>cd<Fr>', Target.RawText) > 0);
    AssertTrue(Pos('<s>e</s>', Target.RawText) > 0);
    AssertTrue(Pos('<sup>f</sup>', Target.RawText) > 0);
    AssertTrue(Pos('<sub>g</sub>', Target.RawText) > 0);
    AssertTrue(Target.Undo);
    AssertEquals(UnicodeString('XY'), Target.RawText);
    AssertTrue(Target.Redo);
    AssertEquals(UnicodeString('XabcdefgY'), Target.VisibleText);
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TTheWordClipboardV2Test.
  HtmlSemanticKindsRoundTripThroughPartialClipboardCopy;
var
  Source, Target: TTheWordWysiwygSessionV2;
  Fragment: TTheWordClipboardFragmentV2;
  Plain: UnicodeString;
  Payload: string;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  Source := TTheWordWysiwygSessionV2.Create(
    'x<B>bold</b> <I>italic</i> <U>under</u>y');
  Target := TTheWordWysiwygSessionV2.Create('');
  Fragment := nil;
  try
    AssertTrue(Source.CopySelection(1, 17, Plain, Payload));
    AssertEquals(UnicodeString('bold italic under'), Plain);
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals(3, Fragment.MarkCount);
    AssertEquals(Ord(twsmHtmlBold), Ord(Fragment.Marks[0].Kind));
    AssertEquals(Ord(twsmHtmlItalic), Ord(Fragment.Marks[1].Kind));
    AssertEquals(Ord(twsmHtmlUnderline), Ord(Fragment.Marks[2].Kind));

    AssertTrue(Target.Paste(0, 0, Payload, '', Inserted, UsedPrivate));
    AssertTrue(UsedPrivate);
    AssertEquals(17, Inserted);
    AssertEquals(UnicodeString('<b>bold</b> <i>italic</i> <u>under</u>'),
      Target.RawText);
  finally
    Fragment.Free;
    Target.Free;
    Source.Free;
  end;
end;

procedure TTheWordClipboardV2Test.AssertClipboardPayloadRejected(
  const APayload: string);
var
  Fragment: TTheWordClipboardFragmentV2;
begin
  Fragment := nil;
  try
    AssertFalse(TTheWordClipboardCodecV2.TryDecodePayload(APayload, Fragment));
    AssertTrue(Fragment = nil);
  finally
    Fragment.Free;
  end;
end;

procedure TTheWordClipboardV2Test.VersionOnePayloadRemainsReadableWithoutFonts;
var
  Fragment: TTheWordClipboardFragmentV2;
  Payload: string;
begin
  Fragment := nil;
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":1,"text":"ab","marks":[],"objects":[' +
    '{"type":"opaque","kind":"unknownTag","offsetUTF16":0,' +
    '"raw":"<x>","order":40001},' +
    '{"type":"opaque","kind":"unknownTag","offsetUTF16":1,' +
    '"raw":"<y>","order":2147483647}]}';
  try
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals(0, Fragment.FontMarkCount);
    AssertEquals(2, Fragment.ObjectCount);
    AssertEquals(40001, Fragment.Objects[0].SourceOrder);
    AssertEquals(High(Integer), Fragment.Objects[1].SourceOrder);
  finally
    Fragment.Free;
  end;
  AssertClipboardPayloadRejected(Copy(Payload, 1, Length(Payload) - 1) +
    ',"fontMarks":[]}');
end;

procedure TTheWordClipboardV2Test.
  VersionTwoFontPayloadPreservesLosslessLexemes;
var
  Fragment: TTheWordClipboardFragmentV2;
  Payload: string;
begin
  Fragment := nil;
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"ab","marks":[],' +
    '"objects":[{"type":"lexical","kind":"strongGreek",' +
    '"offsetUTF16":2,"raw":"<WG1>","order":1}],' +
    '"fontMarks":[{"startUTF16":0,"endUTF16":2,' +
    '"openRaw":"<font  color = '' ReD '' size = +01 name=''A B''   >",' +
    '"closeRaw":"</font>","openSourceOrder":0,' +
    '"closeSourceOrder":2}]}';
  try
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals(1, Fragment.FontMarkCount);
    AssertEquals(UnicodeString(
      '<font  color = '' ReD '' size = +01 name=''A B''   >'),
      Fragment.FontMarks[0].OpenRaw);
    AssertEquals(UnicodeString('</font>'), Fragment.FontMarks[0].CloseRaw);
  finally
    Fragment.Free;
  end;
end;

procedure TTheWordClipboardV2Test.
  SerializerUsesExactRootAndDenseRelativeOrders;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Fragment: TTheWordClipboardFragmentV2;
  Data: TJSONData;
  Root: TJSONObject;
  Raw, Plain: UnicodeString;
  Payload: string;
  I: Integer;
begin
  Raw := '';
  for I := 0 to 40000 do
    Raw := Raw + '<x>';
  Raw := Raw + 'z word<WG1><WTN-NSM>';
  Codec := TTheWordOntCodecV2.Create;
  Document := Codec.DecodeVerse(Raw);
  Fragment := nil;
  Data := nil;
  try
    AssertTrue('whole word syntagm must serialize',
      TTheWordClipboardCodecV2.EncodeSelection(Document, 2, 4,
        Plain, Payload));
    Data := GetJSON(Payload);
    Root := TJSONObject(Data);
    AssertEquals(6, Root.Count);
    AssertEquals('schema', Root.Names[0]);
    AssertEquals('version', Root.Names[1]);
    AssertEquals('text', Root.Names[2]);
    AssertEquals('marks', Root.Names[3]);
    AssertEquals('objects', Root.Names[4]);
    AssertEquals('fontMarks', Root.Names[5]);
    AssertEquals(2, Root.Get('version', 0));
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals('both lexical postfix objects are selected',
      2, Fragment.ObjectCount);
    AssertEquals(0, Fragment.Objects[0].SourceOrder);
    AssertEquals(1, Fragment.Objects[1].SourceOrder);
  finally
    Data.Free;
    Fragment.Free;
    Document.Free;
  end;
end;

procedure TTheWordClipboardV2Test.VersionTwoFontNestingIsValidated;
var
  Fragment: TTheWordClipboardFragmentV2;
  Payload, Misnested: string;
begin
  Fragment := nil;
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"abc","marks":[],"objects":[],"fontMarks":[' +
    '{"startUTF16":0,"endUTF16":3,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,"closeSourceOrder":3},' +
    '{"startUTF16":1,"endUTF16":2,"openRaw":"<font size=+1>",' +
    '"closeRaw":"</font>","openSourceOrder":1,"closeSourceOrder":2}]}';
  try
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals(2, Fragment.FontMarkCount);
  finally
    Fragment.Free;
  end;
  Misnested := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"abc","marks":[],"objects":[],"fontMarks":[' +
    '{"startUTF16":0,"endUTF16":3,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,"closeSourceOrder":2},' +
    '{"startUTF16":1,"endUTF16":2,"openRaw":"<font size=+1>",' +
    '"closeRaw":"</font>","openSourceOrder":1,"closeSourceOrder":3}]}';
  AssertClipboardPayloadRejected(Misnested);
  AssertClipboardPayloadRejected(
    '{"schema":"ibiblia.theword-semantic-fragment","version":2,' +
    '"text":"abc","marks":[{"kind":"FI","startUTF16":1,' +
    '"endUTF16":3}],"objects":[],"fontMarks":[' +
    '{"startUTF16":0,"endUTF16":2,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,' +
    '"closeSourceOrder":1}]}');
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"abc","marks":[{"kind":"FI","startUTF16":0,' +
    '"endUTF16":3}],"objects":[],"fontMarks":[' +
    '{"startUTF16":0,"endUTF16":3,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,' +
    '"closeSourceOrder":1}]}';
  Fragment := nil;
  try
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
  finally
    Fragment.Free;
  end;
end;

procedure TTheWordClipboardV2Test.VersionTwoFontFieldsAreExact;
const
  Prefix = '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"a","marks":[],"objects":[],"fontMarks":[';
begin
  AssertClipboardPayloadRejected(Prefix +
    '{"startUTF16":0,"endUTF16":1,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0}]}');
  AssertClipboardPayloadRejected(Prefix +
    '{"startUTF16":0,"endUTF16":1,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,"closeSourceOrder":1,' +
    '"extra":0}]}');
  AssertClipboardPayloadRejected(Prefix +
    '{"startUTF16":0,"endUTF16":1,"OpenRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,"closeSourceOrder":1}]}');
  AssertClipboardPayloadRejected(Prefix +
    '{"startUTF16":"0","endUTF16":1,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,"closeSourceOrder":1}]}');
end;

procedure TTheWordClipboardV2Test.
  VersionTwoLimitsAndUTF16BoundariesAreValidated;
var
  Fragment: TTheWordClipboardFragmentV2;
  Emoji, OpenRaw, ObjectRaw, Payload: UnicodeString;
begin
  Fragment := nil;
  Emoji := UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00));

  OpenRaw := '<font size=+1' + StringOfChar(' ', 65535 - 14) + '>';
  AssertEquals('font MAX-1 fixture length', 65535, Length(OpenRaw));
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"a","marks":[],"objects":[],' +
    '"fontMarks":[{"startUTF16":0,"endUTF16":1,"openRaw":"' +
    OpenRaw + '","closeRaw":"</font>","openSourceOrder":0,' +
    '"closeSourceOrder":1}]}';
  try
    AssertTrue('semantic font raw at MAX-1 must decode',
      TTheWordClipboardCodecV2.TryDecodePayload(UTF8Encode(Payload), Fragment));
  finally
    FreeAndNil(Fragment);
  end;

  OpenRaw := '<font size=+1' + StringOfChar(' ', 65536 - 14) + '>';
  AssertEquals('font MAX fixture length', 65536, Length(OpenRaw));
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"a","marks":[],"objects":[],' +
    '"fontMarks":[{"startUTF16":0,"endUTF16":1,"openRaw":"' +
    OpenRaw + '","closeRaw":"</font>","openSourceOrder":0,' +
    '"closeSourceOrder":1}]}';
  try
    AssertTrue('semantic font raw at MAX must decode',
      TTheWordClipboardCodecV2.TryDecodePayload(UTF8Encode(Payload), Fragment));
  finally
    FreeAndNil(Fragment);
  end;

  OpenRaw := '<font size=+1' + StringOfChar(' ', 65537 - 14) + '>';
  AssertEquals('font MAX+1 fixture length', 65537, Length(OpenRaw));
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"a","marks":[],"objects":[],' +
    '"fontMarks":[{"startUTF16":0,"endUTF16":1,"openRaw":"' +
    OpenRaw + '","closeRaw":"</font>","openSourceOrder":0,' +
    '"closeSourceOrder":1}]}';
  AssertClipboardPayloadRejected(UTF8Encode(Payload));

  ObjectRaw := '<x a=' + StringOfChar('a', 65536 - 6) + '>';
  AssertEquals('object MAX fixture length', 65536, Length(ObjectRaw));
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"a","marks":[],"objects":[' +
    '{"type":"opaque","kind":"unknownTag","offsetUTF16":0,' +
    '"raw":"' + ObjectRaw + '","order":0}],"fontMarks":[]}';
  try
    AssertTrue('opaque object raw at MAX must decode',
      TTheWordClipboardCodecV2.TryDecodePayload(UTF8Encode(Payload), Fragment));
    AssertEquals('one MAX opaque object', 1, Fragment.ObjectCount);
  finally
    FreeAndNil(Fragment);
  end;

  ObjectRaw := '<x a=' + StringOfChar('a', 65537 - 6) + '>';
  AssertEquals('object MAX+1 fixture length', 65537, Length(ObjectRaw));
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"a","marks":[],"objects":[' +
    '{"type":"opaque","kind":"unknownTag","offsetUTF16":0,' +
    '"raw":"' + ObjectRaw + '","order":0}],"fontMarks":[]}';
  AssertClipboardPayloadRejected(UTF8Encode(Payload));

  AssertClipboardPayloadRejected(
    '{"schema":"ibiblia.theword-semantic-fragment","version":2,' +
    '"text":"a","marks":[],"objects":[],"fontMarks":[' +
    '{"startUTF16":0,"endUTF16":1,"openRaw":"<font color=red>",' +
    '"closeRaw":"</font>","openSourceOrder":0,' +
    '"closeSourceOrder":2147483648}]}');
  AssertClipboardPayloadRejected(
    '{"schema":"ibiblia.theword-semantic-fragment","version":2,' +
    '"text":"' + UTF8Encode(Emoji) + '","marks":[],"objects":[],' +
    '"fontMarks":[{"startUTF16":1,"endUTF16":2,' +
    '"openRaw":"<font color=red>","closeRaw":"</font>",' +
    '"openSourceOrder":0,"closeSourceOrder":1}]}');
end;

procedure TTheWordClipboardV2Test.MalformedFontRemainsAnOpaqueObject;
var
  Fragment: TTheWordClipboardFragmentV2;
  Payload: string;
begin
  Fragment := nil;
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":2,"text":"a","marks":[],"objects":[' +
    '{"type":"opaque","kind":"malformedTag","offsetUTF16":0,' +
    '"raw":"<font color=bogus>","order":0}],"fontMarks":[]}';
  try
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals(0, Fragment.FontMarkCount);
    AssertEquals(1, Fragment.ObjectCount);
  finally
    Fragment.Free;
  end;
end;

procedure TTheWordClipboardV2Test.
  CloseBeforeLexicalPayloadNormalizesAndReencodes;
const
  ORIGINAL_PAYLOAD =
    '{"schema":"ibiblia.theword-semantic-fragment","version":2,' +
    '"text":"word","marks":[{"kind":"B","startUTF16":0,' +
    '"endUTF16":4}],"objects":[{"type":"lexical",' +
    '"kind":"strongGreek","offsetUTF16":4,"raw":"<WG123>",' +
    '"order":2}],"fontMarks":[{"startUTF16":0,"endUTF16":4,' +
    '"openRaw":"<font size=+1>","closeRaw":"</font>",' +
    '"openSourceOrder":0,"closeSourceOrder":1}]}';
  NORMALIZED_RAW = '<font size=+1><b>word<WG123></b></font>';
var
  Fragment, ReFragment: TTheWordClipboardFragmentV2;
  Source, Target: TTheWordWysiwygSessionV2;
  Data: TJSONData;
  Root: TJSONObject;
  Objects, Fonts: TJSONArray;
  Raw, Plain: UnicodeString;
  RePayload: string;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  Fragment := nil;
  ReFragment := nil;
  Data := nil;
  Source := nil;
  Target := nil;
  try
    AssertTrue('close-before-WG payload remains structurally valid',
      TTheWordClipboardCodecV2.TryDecodePayload(ORIGINAL_PAYLOAD, Fragment));
    AssertTrue('close-before-WG fragment rebuilds',
      TTheWordClipboardCodecV2.BuildFragmentRaw(Fragment, Raw));
    AssertEquals('WG is canonicalized before same-anchor close',
      UnicodeString(NORMALIZED_RAW), Raw);

    Source := TTheWordWysiwygSessionV2.Create(Raw);
    AssertTrue('normalized fragment reencodes',
      Source.CopySelection(0, 4, Plain, RePayload));
    Data := GetJSON(RePayload);
    Root := TJSONObject(Data);
    Objects := TJSONArray(Root.Find('objects'));
    Fonts := TJSONArray(Root.Find('fontMarks'));
    AssertTrue(Assigned(Objects));
    AssertTrue(Assigned(Fonts));
    AssertEquals('normalized WG rank', 1,
      TJSONObject(Objects.Items[0]).Get('order', -1));
    AssertEquals('normalized font close rank', 2,
      TJSONObject(Fonts.Items[0]).Get('closeSourceOrder', -1));
    AssertTrue('reencoded normalized payload decodes',
      TTheWordClipboardCodecV2.TryDecodePayload(RePayload, ReFragment));
    FreeAndNil(Data);

    Target := TTheWordWysiwygSessionV2.Create(
      '<font color=blue>XY</font>');
    AssertTrue('original noncanonical payload pastes over whole destination',
      Target.Paste(0, 2, ORIGINAL_PAYLOAD, '', Inserted, UsedPrivate));
    AssertTrue(UsedPrivate);
    AssertEquals(4, Inserted);
    AssertEquals(
      UnicodeString('<font color=blue>' + NORMALIZED_RAW + '</font>'),
      Target.RawText);
  finally
    Data.Free;
    Target.Free;
    Source.Free;
    ReFragment.Free;
    Fragment.Free;
  end;
end;

procedure TTheWordClipboardV2Test.
  VersionOneHistoricalOrdersAndLargeRawStillPaste;
var
  Fragment: TTheWordClipboardFragmentV2;
  Target: TTheWordWysiwygSessionV2;
  LargeRaw, Payload, ExpectedRaw, BuiltRaw: UnicodeString;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  LargeRaw := '<x a=' + StringOfChar('a', 65537 - 6) + '>';
  AssertEquals('legacy raw is above the v2 limit', 65537,
    Length(LargeRaw));
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":1,"text":"ab","marks":[],"objects":[' +
    '{"type":"structural","kind":"syntagmBoundary",' +
    '"offsetUTF16":1,"raw":"|","order":2147483647},' +
    '{"type":"opaque","kind":"unknownTag","offsetUTF16":0,' +
    '"raw":"' + LargeRaw + '","order":40001},' +
    '{"type":"opaque","kind":"unknownTag","offsetUTF16":0,' +
    '"raw":"<y>","order":40001}]}';
  ExpectedRaw := LargeRaw + '<y>a|b';
  Fragment := nil;
  Target := TTheWordWysiwygSessionV2.Create('');
  try
    AssertTrue('historical v1 payload remains readable',
      TTheWordClipboardCodecV2.TryDecodePayload(UTF8Encode(Payload), Fragment));
    AssertEquals(3, Fragment.ObjectCount);
    AssertEquals(40001, Fragment.Objects[0].SourceOrder);
    AssertEquals(40001, Fragment.Objects[1].SourceOrder);
    AssertEquals(High(Integer), Fragment.Objects[2].SourceOrder);
    AssertTrue('historical v1 raw rebuild succeeds',
      TTheWordClipboardCodecV2.BuildFragmentRaw(Fragment, BuiltRaw));
    AssertEquals('legacy offset/order sort is deterministic',
      ExpectedRaw, BuiltRaw);
    AssertTrue('historical v1 payload pastes',
      Target.Paste(0, 0, UTF8Encode(Payload), '', Inserted, UsedPrivate));
    AssertTrue(UsedPrivate);
    AssertEquals(2, Inserted);
    AssertEquals(ExpectedRaw, Target.RawText);
  finally
    Target.Free;
    Fragment.Free;
  end;
end;

procedure TTheWordClipboardV2Test.SemanticFontLexicalOrderRoundTrips;
const
  CASES: array[0..1] of UnicodeString = (
    '<b><font size=+1>word<WG123></font></b>',
    '<b><font size=+1>word</font><WG123></b>'
  );
var
  Source, Target: TTheWordWysiwygSessionV2;
  Codec: ITheWordOntCodecV2;
  Decoded: TTheWordVerseDocument;
  Fragment: TTheWordClipboardFragmentV2;
  Parser: TONTParser;
  Parsed: TSyntagmList;
  ParsedWord: TSyntagm;
  Plain, BuiltRaw, ExpectedRaw: UnicodeString;
  Payload: string;
  Inserted, CaseIndex, Index: Integer;
  UsedPrivate: Boolean;
begin
  Parser := TONTParser.Create;
  Codec := TTheWordOntCodecV2.Create;
  Fragment := nil;
  try
    for CaseIndex := Low(CASES) to High(CASES) do
    begin
      Source := TTheWordWysiwygSessionV2.Create(CASES[CaseIndex]);
      Target := TTheWordWysiwygSessionV2.Create('');
      try
        AssertTrue(Source.CopySelection(0, 4, Plain, Payload));
        ExpectedRaw := '<font size=+1><b>word<WG123></b></font>';
        AssertTrue('private payload decodes',
          TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
        AssertTrue('fragment raw rebuild succeeds',
          TTheWordClipboardCodecV2.BuildFragmentRaw(Fragment, BuiltRaw));
        AssertEquals('same-offset lexical is normalized before font close',
          ExpectedRaw, BuiltRaw);
        Decoded := Codec.DecodeVerse(BuiltRaw);
        try
          AssertEquals('balanced font range', 1, Decoded.FontMarkCount);
          AssertEquals('font starts at word start', 0,
            Decoded.FontMarks[0].StartOffset);
          AssertEquals('font ends at word end', 4,
            Decoded.FontMarks[0].EndOffset);
          AssertEquals('one lexical postfix', 1, Decoded.LexicalItemCount);
          AssertEquals('lexical anchor stays at word end', 4,
            Decoded.LexicalItems[0].Offset);
          AssertEquals('WG kind survives normalization',
            Ord(twlkStrongGreek), Ord(Decoded.LexicalItems[0].Kind));
          AssertEquals(UnicodeString('<WG123>'),
            Decoded.LexicalItems[0].RawLexeme);
        finally
          Decoded.Free;
        end;
        FreeAndNil(Fragment);

        AssertTrue(Target.Paste(0, 0, Payload, '', Inserted, UsedPrivate));
        AssertTrue(UsedPrivate);
        AssertEquals(4, Inserted);
        AssertEquals('pasted raw is normalized', ExpectedRaw, Target.RawText);
        Parsed := Parser.ParseLine(UTF8Encode(Target.RawText), nil);
        try
          ParsedWord := nil;
          for Index := 0 to Parsed.Count - 1 do
            if (Parsed[Index].Kind = skSyntagm) and
               (Parsed[Index].Text = 'word') then
            begin
              ParsedWord := Parsed[Index];
              Break;
            end;
          AssertTrue('word syntagm remains parsed', Assigned(ParsedWord));
          AssertEquals('WG123 remains attached after paste', 1,
            ParsedWord.Strong.Count);
          AssertEquals('Strong index survives', 'G123',
            ParsedWord.Strong[0]);
        finally
          for Index := Parsed.Count - 1 downto 0 do Parsed[Index].Free;
          Parsed.Free;
        end;
      finally
        Fragment.Free;
        Fragment := nil;
        Target.Free;
        Source.Free;
      end;
    end;

    Source := TTheWordWysiwygSessionV2.Create(
      '<b><font size=+1>word</font><WG123><WTN-NSM></b>');
    Target := TTheWordWysiwygSessionV2.Create(
      '<font color=blue>XY</font><x keep><RF q=*><b>note</b><Rf>');
    Fragment := nil;
    try
      AssertTrue(Source.CopySelection(0, 4, Plain, Payload));
      AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
      AssertEquals('two lexical postfixes copied', 2, Fragment.ObjectCount);
      AssertEquals(UnicodeString('<WG123>'), Fragment.Objects[0].RawLexeme);
      AssertEquals(UnicodeString('<WTN-NSM>'), Fragment.Objects[1].RawLexeme);
      AssertTrue(TTheWordClipboardCodecV2.BuildFragmentRaw(Fragment, BuiltRaw));
      ExpectedRaw :=
        '<font size=+1><b>word<WG123><WTN-NSM></b></font>';
      AssertEquals('two lexical postfixes retain SourceOrder before close',
        ExpectedRaw, BuiltRaw);
      FreeAndNil(Fragment);

      AssertTrue('whole destination paste succeeds',
        Target.Paste(0, 2, Payload, '', Inserted, UsedPrivate));
      AssertTrue(UsedPrivate);
      AssertEquals(4, Inserted);
      AssertEquals(
        UnicodeString(
          '<font color=blue><font size=+1><b>word<WG123><WTN-NSM>' +
          '</b></font></font><x keep><RF q=*><b>note</b><Rf>'),
        Target.RawText);
      AssertTrue('opaque object remains byte-for-byte',
        Pos('<x keep>', Target.RawText) > 0);
      AssertTrue('translator note remains byte-for-byte',
        Pos('<RF q=*><b>note</b><Rf>', Target.RawText) > 0);

      Parsed := Parser.ParseLine(UTF8Encode(Target.RawText), nil);
      try
        ParsedWord := nil;
        for Index := 0 to Parsed.Count - 1 do
          if (Parsed[Index].Kind = skSyntagm) and
             (Parsed[Index].Text = 'word') then
          begin
            ParsedWord := Parsed[Index];
            Break;
          end;
        AssertTrue(Assigned(ParsedWord));
        AssertEquals(1, ParsedWord.Strong.Count);
        AssertEquals('G123', ParsedWord.Strong[0]);
        AssertEquals(1, ParsedWord.Morph.Count);
        AssertEquals('N-NSM', ParsedWord.Morph[0]);
      finally
        for Index := Parsed.Count - 1 downto 0 do Parsed[Index].Free;
        Parsed.Free;
      end;
    finally
      Fragment.Free;
      Target.Free;
      Source.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TTheWordClipboardV2Test.LargeLegacyObjectOrderIsDeterministic;
var
  Fragment: TTheWordClipboardFragmentV2;
  Payload: string;
  I: Integer;
begin
  Payload := '{"schema":"ibiblia.theword-semantic-fragment",' +
    '"version":1,"text":"a","marks":[],"objects":[';
  for I := 1999 downto 0 do
  begin
    if I < 1999 then Payload := Payload + ',';
    Payload := Payload + '{"type":"opaque","kind":"unknownTag",' +
      '"offsetUTF16":0,"raw":"<x' + IntToStr(I) + '>","order":' +
      IntToStr(I) + '}';
  end;
  Payload := Payload + ']}';
  Fragment := nil;
  try
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals(2000, Fragment.ObjectCount);
    AssertEquals(0, Fragment.Objects[0].SourceOrder);
    AssertEquals(1999, Fragment.Objects[1999].SourceOrder);
  finally
    Fragment.Free;
  end;
end;

procedure TTheWordClipboardV2Test.CorruptPayloadFallsBackToUnicodePlainText;
var
  Session: TTheWordWysiwygSessionV2;
  Emoji, Plain: UnicodeString;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  Emoji := UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00));
  Plain := UnicodeString('A') + #13#10 + Emoji;
  Session := TTheWordWysiwygSessionV2.Create('xy');
  try
    AssertTrue(Session.Paste(1, 0, '{corrupt', Plain, Inserted,
      UsedPrivate));
    AssertFalse(UsedPrivate);
    AssertEquals(4, Inserted);
    AssertEquals(UnicodeString('xA ') + Emoji + 'y', Session.VisibleText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordClipboardV2Test.EmojiRangesRemainUTF16Based;
var
  Source, Target: TTheWordWysiwygSessionV2;
  Emoji, Plain: UnicodeString;
  Payload: string;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  Emoji := UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00));
  Source := TTheWordWysiwygSessionV2.Create('<FU>' + Emoji + '<Fu>x');
  Target := TTheWordWysiwygSessionV2.Create('');
  try
    AssertTrue(Source.CopySelection(0, 2, Plain, Payload));
    AssertEquals(2, Length(Plain));
    AssertTrue(Target.Paste(0, 0, Payload, '', Inserted, UsedPrivate));
    AssertTrue(UsedPrivate);
    AssertEquals(2, Inserted);
    AssertEquals(UnicodeString('<FU>') + Emoji + '<Fu>', Target.RawText);
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TTheWordClipboardV2Test.FullyContainedObjectsRoundTrip;
var
  Source, Target: TTheWordWysiwygSessionV2;
  Fragment: TTheWordClipboardFragmentV2;
  Plain: UnicodeString;
  Payload: string;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  Source := TTheWordWysiwygSessionV2.Create(
    'word<WG123><WTN-NSM>|<x data="1">tail');
  Target := TTheWordWysiwygSessionV2.Create('');
  Fragment := nil;
  try
    AssertTrue(Source.CopySelection(0, 8, Plain, Payload));
    AssertTrue(Payload <> '');
    AssertTrue(TTheWordClipboardCodecV2.TryDecodePayload(Payload, Fragment));
    AssertEquals(4, Fragment.ObjectCount);
    FreeAndNil(Fragment);
    AssertTrue(Target.Paste(0, 0, Payload, '', Inserted, UsedPrivate));
    AssertTrue(UsedPrivate);
    AssertEquals(UnicodeString('wordtail'), Target.VisibleText);
    AssertTrue(Pos('<WG123>', Target.RawText) > 0);
    AssertTrue(Pos('<WTN-NSM>', Target.RawText) > 0);
    AssertTrue(Pos('|', Target.RawText) > 0);
    AssertTrue(Pos('<x data="1">', Target.RawText) > 0);
  finally
    Fragment.Free;
    Target.Free;
    Source.Free;
  end;
end;

procedure TTheWordClipboardV2Test.
  PartialLexicalCutIsRejectedAndFullCutIsAtomic;
var
  Session: TTheWordWysiwygSessionV2;
  Plain: UnicodeString;
  Payload: string;
begin
  Session := TTheWordWysiwygSessionV2.Create('word<WG123> tail');
  try
    AssertFalse(Session.CutSelection(1, 3, Plain, Payload));
    AssertFalse(Session.CanUndo);
    AssertEquals(UnicodeString('word<WG123> tail'), Session.RawText);
    AssertTrue(Session.CutSelection(0, 4, Plain, Payload));
    AssertEquals(UnicodeString('word'), Plain);
    AssertTrue(Pos('<WG123>', Payload) > 0);
    AssertEquals(UnicodeString(' tail'), Session.RawText);
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('word<WG123> tail'), Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordClipboardV2Test.
  FontMarksSurviveCutAndUndoAtomically;
var
  Codec: ITheWordOntCodecV2;
  Document: TTheWordVerseDocument;
  Session: TTheWordWysiwygSessionV2;
  Parser: TONTParser;
  Parsed: TSyntagmList;
  Word: TSyntagm;
  Emoji, OriginalRaw, ExpectedRaw, NoteRaw, CutRaw, Plain: UnicodeString;
  Payload: string;
  I, OuterFontIndex, InnerFontIndex: Integer;
begin
  Codec := TTheWordOntCodecV2.Create;
  Emoji := UnicodeString(WideChar($D83D)) +
    UnicodeString(WideChar($DE00));
  OriginalRaw := '<font color=red>A<font size=+1>' + Emoji +
    ' word<WG123><WTN-NSM></font>Z</font>';
  ExpectedRaw :=
    '<font color=red>A<font size=+1> word<WG123><WTN-NSM></font>Z</font>';

  Document := Codec.DecodeVerse(OriginalRaw);
  try
    AssertFalse('half of a surrogate pair cannot be cut',
      TTheWordClipboardCodecV2.BuildCutRaw(Document, 1, 1, CutRaw));
    AssertTrue('whole non-BMP character can be cut',
      TTheWordClipboardCodecV2.BuildCutRaw(Document, 1, 2, CutRaw));
    AssertEquals('nested font ranges trim in UTF-16 units',
      ExpectedRaw, CutRaw);
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(CutRaw);
  try
    AssertEquals('outer and inner font ranges remain nested',
      2, Document.FontMarkCount);
    if Document.FontMarks[0].HasColor then
    begin
      OuterFontIndex := 0;
      InnerFontIndex := 1;
    end
    else
    begin
      OuterFontIndex := 1;
      InnerFontIndex := 0;
    end;
    AssertTrue(Document.FontMarks[OuterFontIndex].HasColor);
    AssertTrue(Document.FontMarks[InnerFontIndex].HasSize);
    AssertEquals(0, Document.FontMarks[OuterFontIndex].StartOffset);
    AssertEquals(7, Document.FontMarks[OuterFontIndex].EndOffset);
    AssertEquals(1, Document.FontMarks[InnerFontIndex].StartOffset);
    AssertEquals(6, Document.FontMarks[InnerFontIndex].EndOffset);
    AssertEquals('WG and WT stay at the word end', 2,
      Document.LexicalItemCount);
    AssertEquals(6, Document.LexicalItems[0].Offset);
    AssertEquals(6, Document.LexicalItems[1].Offset);
  finally
    Document.Free;
  end;

  Parser := TONTParser.Create;
  Parsed := Parser.ParseLine(UTF8Encode(CutRaw), nil);
  try
    Word := nil;
    for I := 0 to Parsed.Count - 1 do
      if (Parsed[I].Kind = skSyntagm) and (Parsed[I].Text = 'word') then
      begin
        Word := Parsed[I];
        Break;
      end;
    AssertTrue('word syntagm remains parsed', Assigned(Word));
    AssertEquals(1, Word.Strong.Count);
    AssertEquals('G123', Word.Strong[0]);
    AssertEquals(1, Word.Morph.Count);
    AssertEquals('N-NSM', Word.Morph[0]);
  finally
    for I := Parsed.Count - 1 downto 0 do Parsed[I].Free;
    Parsed.Free;
    Parser.Free;
  end;

  Document := Codec.DecodeVerse(
    '<font color=red>ab</font>XY<font size=+1>cd</font>');
  try
    AssertTrue(TTheWordClipboardCodecV2.BuildCutRaw(
      Document, 2, 2, CutRaw));
    AssertEquals('before unchanged and after shifted left',
      UnicodeString(
        '<font color=red>ab</font><font size=+1>cd</font>'), CutRaw);
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(
    '<font color=red>word</font><font size=+1></font>tail');
  try
    AssertTrue(TTheWordClipboardCodecV2.BuildCutRaw(
      Document, 0, 4, CutRaw));
    AssertEquals('collapsed nonempty wrapper is removed but imported empty stays',
      UnicodeString('<font size=+1></font>tail'), CutRaw);
  finally
    Document.Free;
  end;

  NoteRaw :=
    'a <RF q=S><b>start</b><font color=blue></font><Rf>' +
    '<font size=+1>b<WG123><WTN-NSM></font>' +
    '<RF q=E>end<Rf>c<RF q=O>outside<Rf>';
  Document := Codec.DecodeVerse(NoteRaw);
  try
    AssertTrue('notes exactly at cut boundaries remain allowed',
      TTheWordClipboardCodecV2.BuildCutRaw(Document, 2, 1, CutRaw));
    AssertEquals('boundary/outside notes remain lossless and ordered',
      UnicodeString(
        'a <RF q=S><b>start</b><font color=blue></font><Rf>' +
        '<RF q=E>end<Rf>c' +
        '<RF q=O>outside<Rf>'), CutRaw);
  finally
    Document.Free;
  end;
  Document := Codec.DecodeVerse(CutRaw);
  try
    AssertEquals(3, Document.TranslatorNoteCount);
    AssertEquals(2, Document.TranslatorNotes[0].OffsetUTF16);
    AssertEquals(2, Document.TranslatorNotes[1].OffsetUTF16);
    AssertEquals(3, Document.TranslatorNotes[2].OffsetUTF16);
    AssertEquals(UnicodeString('S'), Document.TranslatorNotes[0].Q);
    AssertEquals(UnicodeString(
      '<b>start</b><font color=blue></font>'),
      Document.TranslatorNotes[0].BodyRaw);
    AssertEquals(UnicodeString('E'), Document.TranslatorNotes[1].Q);
    AssertEquals(UnicodeString('end'), Document.TranslatorNotes[1].BodyRaw);
    AssertEquals(UnicodeString('O'), Document.TranslatorNotes[2].Q);
    AssertEquals(UnicodeString('outside'), Document.TranslatorNotes[2].BodyRaw);
  finally
    Document.Free;
  end;

  OriginalRaw := 'ab<RF q=X>inside<Rf>cd';
  Document := Codec.DecodeVerse(OriginalRaw);
  try
    AssertFalse('a note strictly inside the cut blocks the cut',
      TTheWordClipboardCodecV2.BuildCutRaw(Document, 1, 2, CutRaw));
    AssertEquals(OriginalRaw, Codec.EncodeVerse(Document));
  finally
    Document.Free;
  end;

  Document := Codec.DecodeVerse(
    '<FI><font size=+1>word<WG123><WTN-NSM></font><Fi>!');
  try
    AssertTrue(TTheWordClipboardCodecV2.BuildCutRaw(
      Document, 4, 1, CutRaw));
    AssertTrue(Pos('<FI>', CutRaw) > 0);
    AssertTrue(Pos('<Fi>', CutRaw) > 0);
    AssertTrue(Pos('<font size=+1>', CutRaw) > 0);
    AssertTrue(Pos('word<WG123><WTN-NSM>', CutRaw) > 0);
  finally
    Document.Free;
  end;

  OriginalRaw := '<font color=red>A<font size=+1>' + Emoji +
    ' word<WG123><WTN-NSM></font>Z</font>';
  Session := TTheWordWysiwygSessionV2.Create(OriginalRaw);
  try
    AssertFalse(Session.CutSelection(1, 1, Plain, Payload));
    AssertFalse(Session.CanUndo);
    AssertTrue(Session.CutSelection(1, 2, Plain, Payload));
    AssertEquals(Emoji, Plain);
    AssertEquals(ExpectedRaw, Session.RawText);
    AssertTrue(Session.CanUndo);
    AssertTrue(Session.Undo);
    AssertEquals(OriginalRaw, Session.RawText);
    AssertFalse('cut creates exactly one undo entry', Session.CanUndo);
  finally
    Session.Free;
  end;

  NoteRaw :=
    'a <RF q=S>start<Rf><font size=+1>b<WG123><WTN-NSM></font>' +
    '<RF q=E>end<Rf>c<RF q=O>outside<Rf>';
  ExpectedRaw :=
    'a <RF q=S>start<Rf><RF q=E>end<Rf>c<RF q=O>outside<Rf>';
  Session := TTheWordWysiwygSessionV2.Create(NoteRaw);
  try
    AssertTrue(Session.CutSelection(2, 1, Plain, Payload));
    AssertEquals(UnicodeString('b'), Plain);
    AssertEquals(ExpectedRaw, Session.RawText);
    Document := Codec.DecodeVerse(Session.RawText);
    try
      AssertEquals(3, Document.TranslatorNoteCount);
      AssertEquals(2, Document.TranslatorNotes[0].OffsetUTF16);
      AssertEquals(2, Document.TranslatorNotes[1].OffsetUTF16);
      AssertEquals(3, Document.TranslatorNotes[2].OffsetUTF16);
    finally
      Document.Free;
    end;
    AssertTrue(Session.Undo);
    AssertEquals(NoteRaw, Session.RawText);
    AssertFalse('note-preserving cut creates one undo entry', Session.CanUndo);
    Document := Codec.DecodeVerse(Session.RawText);
    try
      AssertEquals(3, Document.TranslatorNoteCount);
      AssertEquals(UnicodeString('S'), Document.TranslatorNotes[0].Q);
      AssertEquals(UnicodeString('start'),
        Document.TranslatorNotes[0].BodyRaw);
      AssertEquals(UnicodeString('E'), Document.TranslatorNotes[1].Q);
      AssertEquals(UnicodeString('end'), Document.TranslatorNotes[1].BodyRaw);
      AssertEquals(UnicodeString('O'), Document.TranslatorNotes[2].Q);
      AssertEquals(UnicodeString('outside'),
        Document.TranslatorNotes[2].BodyRaw);
    finally
      Document.Free;
    end;
  finally
    Session.Free;
  end;
end;

procedure TTheWordClipboardV2Test.
  CanonicalProtectionIncludesStructuralAndLexicalRanges;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('a|word<WG123> z');
  try
    AssertFalse(Session.CanReplaceText(1, 0));
    AssertFalse(Session.ReplaceText(1, 0, 'x'));
    AssertFalse(Session.CanReplaceText(3, 0));
    AssertFalse(Session.CanReplaceText(2, 2));
    AssertFalse(Session.CanUndo);
  finally
    Session.Free;
  end;
end;

procedure TTheWordClipboardV2Test.ChangesAreSemanticOnlyIsStrict;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('word<WG123>|tail');
  try
    AssertTrue(Session.ChangesAreSemanticOnly);
    AssertTrue(Session.ApplySemantic(twsmItalic, 0, 4));
    AssertTrue(Session.ChangesAreSemanticOnly);
    AssertTrue(CanCommitLegacyWysiwygChangeV2(True,
      Session.ChangesAreSemanticOnly));
    AssertTrue(Session.ReplaceRawText('<FI>word<Fi><WG124>|tail'));
    AssertFalse(Session.ChangesAreSemanticOnly);
    AssertFalse(CanCommitLegacyWysiwygChangeV2(True,
      Session.ChangesAreSemanticOnly));
    AssertTrue(CanCommitLegacyWysiwygChangeV2(False,
      Session.ChangesAreSemanticOnly));
    Session.Cancel;
    AssertTrue(Session.ReplaceRawText('word<WG123>tail'));
    AssertFalse(Session.ChangesAreSemanticOnly);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('plain');
  try
    AssertTrue(Session.ReplaceText(5, 0, '!'));
    AssertFalse(Session.ChangesAreSemanticOnly);
  finally
    Session.Free;
  end;
end;

initialization
  RegisterTest(TTheWordClipboardV2Test);

end.
