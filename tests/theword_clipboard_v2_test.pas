unit theword_clipboard_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTheWordClipboardV2Test = class(TTestCase)
  published
    procedure SemanticPayloadIsDeterministicAndPreferred;
    procedure CorruptPayloadFallsBackToUnicodePlainText;
    procedure EmojiRangesRemainUTF16Based;
    procedure FullyContainedObjectsRoundTrip;
    procedure PartialLexicalCutIsRejectedAndFullCutIsAtomic;
    procedure CanonicalProtectionIncludesStructuralAndLexicalRanges;
    procedure ChangesAreSemanticOnlyIsStrict;
  end;

implementation

uses
  theword_ont_codec_v2, theword_clipboard_v2,
  theword_wysiwyg_session_v2;

procedure TTheWordClipboardV2Test.SemanticPayloadIsDeterministicAndPreferred;
var
  Source, Target: TTheWordWysiwygSessionV2;
  Plain: UnicodeString;
  Payload, SecondPayload: string;
  Inserted: Integer;
  UsedPrivate: Boolean;
begin
  Source := TTheWordWysiwygSessionV2.Create('<FI>ab<Fi><FR>cd<Fr>');
  Target := TTheWordWysiwygSessionV2.Create('XY');
  try
    AssertTrue(Source.CopySelection(0, 4, Plain, Payload));
    AssertTrue(Source.CopySelection(0, 4, Plain, SecondPayload));
    AssertEquals(Payload, SecondPayload);
    AssertTrue(Pos('ibiblia.theword-semantic-fragment', Payload) > 0);
    AssertTrue(Target.Paste(1, 0, Payload, 'wrong', Inserted, UsedPrivate));
    AssertTrue(UsedPrivate);
    AssertEquals(4, Inserted);
    AssertEquals(UnicodeString('XabcdY'), Target.VisibleText);
    AssertTrue(Pos('<FI>ab<Fi>', Target.RawText) > 0);
    AssertTrue(Pos('<FR>cd<Fr>', Target.RawText) > 0);
    AssertTrue(Target.Undo);
    AssertEquals(UnicodeString('XY'), Target.RawText);
    AssertTrue(Target.Redo);
    AssertEquals(UnicodeString('XabcdY'), Target.VisibleText);
  finally
    Target.Free;
    Source.Free;
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
