unit theword_wysiwyg_session_v2_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTheWordWysiwygSessionV2Test = class(TTestCase)
  published
    procedure AppliesFourSemanticsWithoutExposingTagsAsText;
    procedure UndoRedoRestoresOntSnapshots;
    procedure TextReplacementUsesUTF16Offsets;
    procedure RejectedReplacementDoesNotPolluteUndo;
    procedure OpaqueAndLexicalObjectsProtectTextEdits;
    procedure VisualAdapterCombinesOverlappingStyles;
    procedure VisualAdapterMapsSyntheticProtectedSeparators;
    procedure ProtectedAnchorsAllowAdjacentButNotCrossingEdits;
    procedure RawFallbackReentersTheSameCodecSession;
  end;

implementation

uses
  Graphics, text_direction, verse_visual_document, theword_ont_codec_v2,
  theword_verse_visual_adapter_v2, theword_wysiwyg_session_v2;

procedure TTheWordWysiwygSessionV2Test.
  AppliesFourSemanticsWithoutExposingTagsAsText;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('abcd');
  try
    AssertTrue(Session.ApplySemantic(twsmItalic, 0, 1));
    AssertTrue(Session.ApplySemantic(twsmWordsOfJesus, 1, 1));
    AssertTrue(Session.ApplySemantic(twsmOldTestamentQuote, 2, 1));
    AssertTrue(Session.ApplySemantic(twsmUnderline, 3, 1));
    AssertEquals(UnicodeString('abcd'), Session.VisibleText);
    AssertEquals(UnicodeString('<FI>a<Fi><FR>b<Fr><FO>c<Fo><FU>d<Fu>'),
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.UndoRedoRestoresOntSnapshots;
const
  ORIGINAL: UnicodeString = '<FI>ab<Fi><x data="1">cd';
var
  Session: TTheWordWysiwygSessionV2;
  Changed: UnicodeString;
begin
  Session := TTheWordWysiwygSessionV2.Create(ORIGINAL);
  try
    AssertTrue(Session.ApplySemantic(twsmUnderline, 0, 2));
    Changed := Session.RawText;
    AssertTrue(Pos('<x data="1">', Changed) > 0);
    AssertTrue(Session.Undo);
    AssertEquals(ORIGINAL, Session.RawText);
    AssertTrue(Session.Redo);
    AssertEquals(Changed, Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.TextReplacementUsesUTF16Offsets;
var
  Session: TTheWordWysiwygSessionV2;
  Emoji: UnicodeString;
begin
  Emoji := UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00));
  Session := TTheWordWysiwygSessionV2.Create('abcd');
  try
    AssertTrue(Session.ReplaceText(1, 2, Emoji));
    AssertEquals(UnicodeString('a') + Emoji + 'd', Session.VisibleText);
    AssertEquals(4, Length(Session.VisibleText));
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('abcd'), Session.VisibleText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  RejectedReplacementDoesNotPolluteUndo;
var
  Session: TTheWordWysiwygSessionV2;
  Malformed: UnicodeString;
  Rejected: Boolean;
begin
  Malformed := UnicodeString(WideChar($D800));
  Session := TTheWordWysiwygSessionV2.Create('abcd');
  try
    Rejected := False;
    try
      Session.ReplaceText(1, 0, Malformed);
    except
      on E: ETheWordOntCodec do
        Rejected := True;
    end;
    AssertTrue('malformed UTF-16 replacement must be rejected', Rejected);
    AssertEquals(UnicodeString('abcd'), Session.RawText);
    AssertFalse('a rejected edit must not create undo history', Session.CanUndo);
  finally
    Session.Free;
  end;
end;
procedure TTheWordWysiwygSessionV2Test.
  OpaqueAndLexicalObjectsProtectTextEdits;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('a<unknown x="1"><WG123>b');
  try
    AssertFalse(Session.CanReplaceText(1, 0));
    AssertFalse(Session.ReplaceText(0, 2, 'z'));
    AssertEquals(UnicodeString('ab'), Session.VisibleText);
    AssertEquals(UnicodeString('a<unknown x="1"><WG123>b'),
      Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterCombinesOverlappingStyles;
var
  Codec: ITheWordOntCodecV2;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
  Style: TVisualTextStyle;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse('<FI>ab<Fi>cd');
  try
    Source.AddSemanticMark(twsmWordsOfJesus, 1, 4);
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 7);
    AssertEquals(UnicodeString('abcd'), Visual.Text);
    AssertEquals(QWord(7), Visual.Revision);
    AssertTrue(Visual.SpanCount >= 3);
    Style := Visual.Spans[1].Style;
    AssertTrue(vtsfItalic in Style.Flags);
    AssertTrue(Style.HasForegroundColor);
    AssertEquals(LongInt(clRed), Style.ForegroundColor);
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  VisualAdapterMapsSyntheticProtectedSeparators;
var
  Codec: ITheWordOntCodecV2;
  DocumentLength, DocumentOffset, LayoutOffset: Integer;
  Hebrew: UnicodeString;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;

  Source := Codec.DecodeVerse('existe<x>NO');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 1);
    AssertEquals(UnicodeString('existe NO'), Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 6, twoaBefore,
      LayoutOffset));
    AssertEquals('document anchor before separator', 6, LayoutOffset);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, 6, twoaAfter,
      LayoutOffset));
    AssertEquals('document anchor after separator', 7, LayoutOffset);
    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, 6,
      DocumentOffset));
    AssertEquals('layout before separator maps to anchor', 6,
      DocumentOffset);
    AssertTrue(TryTheWordLayoutToDocumentOffset(Visual, 7,
      DocumentOffset));
    AssertEquals('layout after separator maps to anchor', 6,
      DocumentOffset);
    AssertTrue(TryTheWordLayoutRangeToDocument(Visual, 7, 2,
      DocumentOffset, DocumentLength));
    AssertEquals('right word starts at document anchor', 6,
      DocumentOffset);
    AssertEquals('right word document length', 2, DocumentLength);
    AssertTrue(TryTheWordLayoutRangeToDocument(Visual, 6, 1,
      DocumentOffset, DocumentLength));
    AssertEquals('synthetic separator has no document width', 0,
      DocumentLength);
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('a||b');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 2);
    AssertEquals('adjacent structural objects add one separator',
      UnicodeString('a b'), Visual.Text);
  finally
    Source.Free;
  end;

  Source := Codec.DecodeVerse('a <x>b');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 3);
    AssertEquals('existing whitespace is not duplicated',
      UnicodeString('a b'), Visual.Text);
  finally
    Source.Free;
  end;

  Hebrew := UnicodeString(WideChar($05D1)) +
    UnicodeString(WideChar($05B0)) + UnicodeString(WideChar($05BC));
  Source := Codec.DecodeVerse(Hebrew + '<x>gloss');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdRightToLeft, 4);
    AssertEquals('mixed Hebrew and gloss remain visibly separated',
      Hebrew + ' gloss', Visual.Text);
    AssertTrue(TryTheWordDocumentToLayoutOffset(Visual, Length(Hebrew),
      twoaAfter, LayoutOffset));
    AssertEquals('Hebrew anchor accounts for synthetic layout unit',
      Length(Hebrew) + 1, LayoutOffset);
  finally
    Source.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  ProtectedAnchorsAllowAdjacentButNotCrossingEdits;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('a|b');
  try
    AssertFalse('caret cannot insert on structural separator',
      Session.CanReplaceText(1, 0));
    AssertFalse('edit cannot cross structural separator',
      Session.CanReplaceText(0, 2));
    AssertTrue('right-hand text remains editable',
      Session.CanReplaceText(1, 1));
    AssertTrue(Session.ReplaceText(1, 1, 'B'));
    AssertEquals(UnicodeString('a|B'), Session.RawText);
  finally
    Session.Free;
  end;

  Session := TTheWordWysiwygSessionV2.Create('a<x>b');
  try
    AssertFalse('caret cannot insert on opaque separator',
      Session.CanReplaceText(1, 0));
    AssertFalse('edit cannot cross opaque separator',
      Session.CanReplaceText(0, 2));
    AssertTrue('text adjacent to opaque object remains editable',
      Session.CanReplaceText(1, 1));
    AssertTrue(Session.ReplaceText(1, 1, 'B'));
    AssertEquals(UnicodeString('a<x>B'), Session.RawText);
    AssertTrue('semantic formatting adjacent to opaque object is allowed',
      Session.CanApplySemantic(1, 1));
    AssertTrue(Session.ApplySemantic(twsmItalic, 1, 1));
    AssertEquals(UnicodeString('a<x><FI>B<Fi>'), Session.RawText);
  finally
    Session.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2Test.
  RawFallbackReentersTheSameCodecSession;
var
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('word');
  try
    AssertTrue(Session.ReplaceRawText('<FU>word<Fu><opaque>'));
    AssertEquals(UnicodeString('word'), Session.VisibleText);
    AssertEquals(Ord(twlcProtectedRegions), Ord(Session.Capability));
    AssertTrue(Session.Undo);
    AssertEquals(UnicodeString('word'), Session.RawText);
  finally
    Session.Free;
  end;
end;

initialization
  RegisterTest(TTheWordWysiwygSessionV2Test);

end.
