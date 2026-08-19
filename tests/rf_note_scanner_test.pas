unit rf_note_scanner_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, fpcunit, testregistry, ONTTokenizer, rf_note_scanner;

type
  TRFNoteScannerTests = class(TTestCase)
  published
    procedure OpeningTagParsingIsCaseSensitiveAndReadsQ;
    procedure AutomaticNumbersIgnoreCustomMarkers;
    procedure AttributesWithoutQDoNotConsumeAutomaticNumber;
    procedure EditorialCommentNormalizationUsesNoteBreaks;
    procedure EditorialCommentExportIsTemporaryAndKeepsInlineRF;
    procedure EditorialCommentProjectionRejectsStructuralRFDelimiters;
    procedure AutomaticCounterSkipsNoteBodiesAndCustomMarkers;
    procedure FlexibleReaderStopsAtFirstClosingAndPreservesTail;
  end;

implementation

procedure TRFNoteScannerTests.OpeningTagParsingIsCaseSensitiveAndReadsQ;
var
  Info: TRFOpeningTagInfo;
begin
  AssertTrue(TryParseRFOpeningTag('<RF>', Info));
  AssertFalse(Info.HasAttributes);
  AssertFalse(Info.HasQ);

  AssertTrue(TryParseRFOpeningTag('<RF q="*">', Info));
  AssertTrue(Info.HasAttributes);
  AssertTrue(Info.HasQ);
  AssertEquals('*', string(Info.Q));

  AssertTrue(TryParseRFOpeningTag('<RF q=nota>', Info));
  AssertEquals('nota', string(Info.Q));
  AssertTrue(TryParseRFOpeningTag('<RF q=" first " q=second>', Info));
  AssertEquals('first', string(Info.Q));
  AssertTrue(TryParseRFOpeningTag('<RF Q=upper>', Info));
  AssertTrue(Info.HasAttributes);
  AssertFalse(Info.HasQ);
  AssertEquals('', string(Info.Q));
  AssertFalse(IsRFOpeningTag('<rf>'));
  AssertFalse(IsRFOpeningTag('<RFX>'));
  AssertTrue(IsRFClosingTag('<Rf>'));
  AssertTrue(IsRFClosingTag('<  Rf >'));
  AssertFalse(IsRFClosingTag('<RF>'));
  AssertFalse(IsRFClosingTag('<  rf >'));
  AssertFalse(IsRFClosingTag('<Rf x>'));
end;

procedure TRFNoteScannerTests.AutomaticNumbersIgnoreCustomMarkers;
var
  Marker: UnicodeString;
  Numberer: TRFNoteNumberer;
begin
  Numberer := TRFNoteNumberer.Create;
  try
    Numberer.Reset;
    AssertTrue(Numberer.ResolveMarker('<RF>', Marker));
    AssertEquals('1', string(Marker));
    AssertTrue(Numberer.ResolveMarker('<RF q="a">', Marker));
    AssertEquals('a', string(Marker));
    AssertTrue(Numberer.ResolveMarker('<RF>', Marker));
    AssertEquals('2', string(Marker));
  finally
    Numberer.Free;
  end;
end;

procedure TRFNoteScannerTests.AttributesWithoutQDoNotConsumeAutomaticNumber;
var
  Marker: UnicodeString;
  Numberer: TRFNoteNumberer;
begin
  Numberer := TRFNoteNumberer.Create;
  try
    Numberer.Reset(3);
    AssertTrue(Numberer.ResolveMarker('<RF>', Marker));
    AssertEquals('4', string(Marker));
    AssertTrue(Numberer.ResolveMarker('<RF x="legacy">', Marker));
    AssertEquals('', string(Marker));
    AssertTrue(Numberer.ResolveMarker('<RF Q="upper">', Marker));
    AssertEquals('', string(Marker));
    AssertTrue(Numberer.ResolveMarker('<RF>', Marker));
    AssertEquals('5', string(Marker));
  finally
    Numberer.Free;
  end;
end;

procedure TRFNoteScannerTests.EditorialCommentNormalizationUsesNoteBreaks;
var
  Input: UnicodeString;
begin
  Input := 'a' + #13#10 + 'b<lf/>c<crlf/>d<br/>e<br>f' +
    '<p>g<CL>h<CM>i<CI>j';
  AssertEquals('a<p>b<p>c<p>d<CL>e<CL>f<p>g<CL>h<CM>i<CI>j',
    string(NormalizeEditorialCommentForRF(Input)));
  AssertEquals('  text  ',
    string(NormalizeEditorialCommentForRF('  text  ')));
end;

procedure TRFNoteScannerTests.
  EditorialCommentExportIsTemporaryAndKeepsInlineRF;
const
  VERSE: UnicodeString = 'text<RF q=*>native<Rf>';
begin
  AssertEquals(string(VERSE), string(AppendEditorialCommentAsRF(
    VERSE, 'editorial', False)));
  AssertEquals('text<RF q=*>native<Rf><RF>line<p>next<Rf>',
    string(AppendEditorialCommentAsRF(VERSE, 'line' + #13#10 + 'next',
      True)));
  AssertEquals(string(VERSE), string(AppendEditorialCommentAsRF(
    VERSE, '', True)));
end;

procedure TRFNoteScannerTests.
  EditorialCommentProjectionRejectsStructuralRFDelimiters;
begin
  AssertTrue(ContainsStructuralRFTag('before<  RF q="x" >after'));
  AssertTrue(ContainsStructuralRFTag('before<  Rf >after'));
  AssertFalse(ContainsStructuralRFTag('before<  rf >after'));
  AssertFalse(ContainsStructuralRFTag(
    '<a title="literal <RF> text">safe</a>'));

  try
    AppendEditorialCommentAsRF('verse', 'before<  RF >after', True);
    Fail('structural RF opening must abort editorial-comment projection');
  except
    on E: ERFEditorialCommentProjectionError do
      AssertTrue('projection error must identify RF', Pos('RF', E.Message) > 0);
  end;

  try
    AppendEditorialCommentAsRF('verse', 'before<  Rf >leaked', True);
    Fail('structural RF closing must abort editorial-comment projection');
  except
    on E: ERFEditorialCommentProjectionError do
      AssertTrue('projection error must identify RF', Pos('RF', E.Message) > 0);
  end;

  AssertEquals('scanner case sensitivity must remain unchanged',
    'verse<RF>before<  rf >after<Rf>',
    string(AppendEditorialCommentAsRF('verse',
      'before<  rf >after', True)));
end;

procedure TRFNoteScannerTests.
  AutomaticCounterSkipsNoteBodiesAndCustomMarkers;
begin
  AssertEquals(2, CountAutomaticRFNotes(
    'a<RF>one<RF>nested-looking<Rf>b' +
    '<RF q=x>custom<Rf><RF>two<Rf>'));
  AssertEquals(2, CountAutomaticRFNotes(
    'a<  RF>one<  Rf >b<RF>two< Rf >'));
  AssertEquals(0, CountAutomaticRFNotes(
    '<RF legacy=1>no marker<Rf><rf>wrong case<Rf>'));
end;

procedure TRFNoteScannerTests.
  FlexibleReaderStopsAtFirstClosingAndPreservesTail;
var
  Body: string;
  ClosingTag: string;
  Token: TSyntagmToken;
  Tokenizer: TONTTokenizer;
begin
  Tokenizer := TTokenizerFactory.CreatePreferredTokenizer(
    'body<RF>nested-looking<  Rf >tail<Rf>after');
  try
    AssertTrue(ReadRFNoteBody(Tokenizer, Body, ClosingTag));
    AssertEquals('body<RF>nested-looking', Body);
    AssertEquals('<  Rf >', ClosingTag);
    AssertEquals(Ord(skSyntagm),
      Ord(Tokenizer.ReadSyntagm(Token)));
    AssertEquals('tail', Token.Value);
    AssertEquals(Ord(skTag),
      Ord(Tokenizer.ReadSyntagm(Token)));
    AssertEquals('<Rf>', Token.Value);
    AssertEquals(Ord(skSyntagm),
      Ord(Tokenizer.ReadSyntagm(Token)));
    AssertEquals('after', Token.Value);
  finally
    Tokenizer.Free;
  end;

  Tokenizer := TTokenizerFactory.CreatePreferredTokenizer('unclosed body');
  try
    AssertFalse(ReadRFNoteBody(Tokenizer, Body, ClosingTag));
    AssertEquals('unclosed body', Body);
    AssertEquals('', ClosingTag);
  finally
    Tokenizer.Free;
  end;
end;

initialization
  RegisterTest(TRFNoteScannerTests);

end.
