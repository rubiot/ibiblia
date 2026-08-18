unit theword_syntax_highlighter_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, IniFiles, SynEdit, SynEditHighlighter,
  fpcunit, testregistry, theword_syntax_highlighter,
  theword_wysiwyg_editor_control;

type

  { TTheWordSyntaxHighlighterTests }

  TTheWordSyntaxHighlighterTests = class(TTestCase)
  private
    function CompletionItemsContainTag(Items: TStrings;
      const TagName: string): Boolean;
    function CreateTestIni: TMemIniFile;
    function KindForToken(const Line, Token: string): TTheWordTokenKind;
    procedure AssertAttributeStyle(const Message: string;
      Attr: TSynHighlighterAttributes; const Style: TTheWordSyntaxElementStyle);
    procedure AssertElementStyle(const Message: string;
      const Expected, Actual: TTheWordSyntaxElementStyle);
    procedure AssertTagPosition(const Message: string;
      const Position: TTheWordTagPairPosition; Row, StartCol, EndCol: Integer);
    procedure AssertTokenKind(const Line, Token: string; Kind: TTheWordTokenKind);
  published
    procedure TextTokensRemainPlain;
    procedure SemanticTagsAreRecognized;
    procedure StrongAndMorphTagsAreRecognized;
    procedure NoteAndReferenceTagsAreRecognized;
    procedure LinkHtmlAndRtfTagsAreRecognized;
    procedure SwitchableTagsAreRecognized;
    procedure MalformedTagsAreWarnings;
    procedure ColorSchemeDefaultsAreApplied;
    procedure ColorSchemeLoadsCustomValues;
    procedure ColorSchemeInvalidValuesFallBackToDefaults;
    procedure ColorSchemeResetRestoresDefaults;
    procedure ColorSchemeSavesCustomValues;
    procedure ColorSchemeAppliesToHighlighter;
    procedure EditorFontDefaultsAreApplied;
    procedure EditorFontLoadsCustomValues;
    procedure EditorFontInvalidValuesFallBackToDefaults;
    procedure EditorFontSavesCustomValues;
    procedure EditorFontAppliesToEditor;
    procedure WysiwygEditorKeepsRawSyntaxFont;
    procedure TagPairFindsClosingTheWordTag;
    procedure TagPairFindsOpeningTheWordTag;
    procedure TagPairHandlesNestedTheWordTags;
    procedure TagPairFindsAcrossLines;
    procedure TagPairFindsHtmlClosingTag;
    procedure TagPairIgnoresUnmatchedTags;
    procedure ConfigureEditorInstallsTagPairMarkup;
    procedure TagCompletionFiltersByPrefix;
    procedure TagCompletionSnippetsUseClosingTags;
    procedure TagCompletionIndentTagsAreNotPairs;
    procedure TagCompletionCompletesTagAtCaret;
    procedure TagCompletionAutoClosesTypedTag;
    procedure TagCompletionDoesNotAutoCloseSingleTag;
    procedure ConfigureEditorInstallsTagCompletionOnlyWhenEditable;
  end;

implementation

function TTheWordSyntaxHighlighterTests.CompletionItemsContainTag(
  Items: TStrings; const TagName: string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Items.Count - 1 do
    if Pos(TagName + ' ', Items[Index]) = 1 then
      Exit(True);
end;

function TTheWordSyntaxHighlighterTests.CreateTestIni: TMemIniFile;
var
  FileName: string;
begin
  FileName := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'ibiblia_theword_highlighter_test.ini';
  DeleteFile(FileName);
  Result := TMemIniFile.Create(FileName);
end;

function TTheWordSyntaxHighlighterTests.KindForToken(const Line, Token: string
  ): TTheWordTokenKind;
var
  Highlighter: TTheWordSyntaxHighlighter;
begin
  Highlighter := TTheWordSyntaxHighlighter.Create(nil);
  try
    Highlighter.SetLine(Line, 0);
    while not Highlighter.GetEol do
    begin
      if Highlighter.GetToken = Token then
        Exit(TTheWordTokenKind(Highlighter.GetTokenKind));
      Highlighter.Next;
    end;
  finally
    Highlighter.Free;
  end;

  Fail(Format('Token "%s" not found in "%s"', [Token, Line]));
  Result := twNull;
end;

procedure TTheWordSyntaxHighlighterTests.AssertElementStyle(
  const Message: string; const Expected, Actual: TTheWordSyntaxElementStyle);
begin
  AssertEquals(Message + '.foreground', LongInt(Expected.Foreground),
    LongInt(Actual.Foreground));
  AssertEquals(Message + '.background', LongInt(Expected.Background),
    LongInt(Actual.Background));
  AssertEquals(Message + '.has_background', Expected.HasBackground,
    Actual.HasBackground);
  AssertTrue(Message + '.style', Expected.FontStyle = Actual.FontStyle);
end;

procedure TTheWordSyntaxHighlighterTests.AssertAttributeStyle(
  const Message: string; Attr: TSynHighlighterAttributes;
  const Style: TTheWordSyntaxElementStyle);
var
  ExpectedBackground: TColor;
begin
  AssertNotNull(Message + '.attribute', Attr);
  AssertEquals(Message + '.foreground', LongInt(Style.Foreground),
    LongInt(Attr.Foreground));
  if Style.HasBackground then
    ExpectedBackground := Style.Background
  else
    ExpectedBackground := clNone;
  AssertEquals(Message + '.background', LongInt(ExpectedBackground),
    LongInt(Attr.Background));
  AssertTrue(Message + '.style', Attr.Style = Style.FontStyle);
end;

procedure TTheWordSyntaxHighlighterTests.AssertTagPosition(
  const Message: string; const Position: TTheWordTagPairPosition; Row,
  StartCol, EndCol: Integer);
begin
  AssertEquals(Message + '.row', Row, Position.Row);
  AssertEquals(Message + '.start_col', StartCol, Position.StartCol);
  AssertEquals(Message + '.end_col', EndCol, Position.EndCol);
end;

procedure TTheWordSyntaxHighlighterTests.AssertTokenKind(const Line,
  Token: string; Kind: TTheWordTokenKind);
begin
  AssertEquals(
    Format('Token "%s" in "%s"', [Token, Line]),
    TheWordSyntaxTokenKindName(Ord(Kind)),
    TheWordSyntaxTokenKindName(Ord(KindForToken(Line, Token))));
end;

procedure TTheWordSyntaxHighlighterTests.TextTokensRemainPlain;
begin
  AssertTokenKind('In the beginning', 'In', twText);
  AssertTokenKind('In the beginning', ' ', twSpace);
end;

procedure TTheWordSyntaxHighlighterTests.SemanticTagsAreRecognized;
begin
  AssertTokenKind('<FI>added<Fi> <CM><TS1>Heading<Ts>', 'FI', twSemanticTag);
  AssertTokenKind('<FI>added<Fi> <CM><TS1>Heading<Ts>', 'Fi', twSemanticTag);
  AssertTokenKind('<FI>added<Fi> <CM><TS1>Heading<Ts>', 'CM', twSemanticTag);
  AssertTokenKind('<FI>added<Fi> <CM><TS1>Heading<Ts>', 'TS1', twSemanticTag);
end;

procedure TTheWordSyntaxHighlighterTests.StrongAndMorphTagsAreRecognized;
begin
  AssertTokenKind('word<WG123><WH456s><WTV-AAI-3S l="lemma"><wt>', 'WG123', twStrongTag);
  AssertTokenKind('word<WG123><WH456s><WTV-AAI-3S l="lemma"><wt>', 'WH456s', twStrongTag);
  AssertTokenKind('word<WG123><WH456s><WTV-AAI-3S l="lemma"><wt>', 'WTV-AAI-3S', twMorphTag);
  AssertTokenKind('word<WG123><WH456s><WTV-AAI-3S l="lemma"><wt>', 'wt', twWordTokenTag);
  AssertTokenKind('word<WG123><WH456s><WTV-AAI-3S l="lemma"><wt>', 'l', twAttribute);
  AssertTokenKind('word<WG123><WH456s><WTV-AAI-3S l="lemma"><wt>', '"lemma"', twAttributeValue);
end;

procedure TTheWordSyntaxHighlighterTests.NoteAndReferenceTagsAreRecognized;
begin
  AssertTokenKind('<RF q=a>note<Rf><RX 1.1.1 q=b>', 'RF', twNoteRefTag);
  AssertTokenKind('<RF q=a>note<Rf><RX 1.1.1 q=b>', 'Rf', twNoteRefTag);
  AssertTokenKind('<RF q=a>note<Rf><RX 1.1.1 q=b>', 'RX', twNoteRefTag);
  AssertTokenKind('<RF q=a>note<Rf><RX 1.1.1 q=b>', 'q', twAttribute);
end;

procedure TTheWordSyntaxHighlighterTests.LinkHtmlAndRtfTagsAreRecognized;
begin
  AssertTokenKind('<a href=tw://strong?t=G1>link</a>', 'a', twLinkTag);
  AssertTokenKind('<a href=tw://strong?t=G1>link</a>', 'href', twAttribute);
  AssertTokenKind('<font color=red><sup>x</sup></font>', 'font', twHtmlTag);
  AssertTokenKind('<font color=red><sup>x</sup></font>', 'sup', twHtmlTag);
  AssertTokenKind('<rtf tag="{\b ">text</rtf>', 'rtf', twRtfTag);
end;

procedure TTheWordSyntaxHighlighterTests.SwitchableTagsAreRecognized;
begin
  AssertTokenKind('<NB toggle=footnotes>n<Nb><ST toggle=cmts>s<St>', 'NB', twSemanticTag);
  AssertTokenKind('<NB toggle=footnotes>n<Nb><ST toggle=cmts>s<St>', 'Nb', twSemanticTag);
  AssertTokenKind('<NB toggle=footnotes>n<Nb><ST toggle=cmts>s<St>', 'ST', twSemanticTag);
  AssertTokenKind('<NB toggle=footnotes>n<Nb><ST toggle=cmts>s<St>', 'toggle', twAttribute);
end;

procedure TTheWordSyntaxHighlighterTests.MalformedTagsAreWarnings;
begin
  AssertTokenKind('<Unknown>tag', '<Unknown>', twWarning);
  AssertTokenKind('<CM >', '<CM >', twWarning);
  AssertTokenKind('<WGabc>', '<WGabc>', twWarning);
  AssertTokenKind('partial <FI', '<FI', twWarning);
end;

procedure TTheWordSyntaxHighlighterTests.ColorSchemeDefaultsAreApplied;
var
  Defaults: TTheWordSyntaxColorScheme;
  Scheme: TTheWordSyntaxColorScheme;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    GetDefaultTheWordSyntaxColorScheme(Defaults);
    LoadTheWordSyntaxColorScheme(Ini);
    Scheme := GetTheWordSyntaxColorScheme;
    AssertElementStyle('strong default', Defaults[tweStrongTag],
      Scheme[tweStrongTag]);
    AssertElementStyle('morphology default', Defaults[tweMorphTag],
      Scheme[tweMorphTag]);
    AssertElementStyle('html default', Defaults[tweHtmlTag],
      Scheme[tweHtmlTag]);
  finally
    Ini.Free;
    ResetTheWordSyntaxColorScheme;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.ColorSchemeLoadsCustomValues;
var
  Scheme: TTheWordSyntaxColorScheme;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'strong.foreground',
      'clLime');
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'strong.background',
      'clYellow');
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'strong.style',
      'italic,underline');

    LoadTheWordSyntaxColorScheme(Ini);
    Scheme := GetTheWordSyntaxColorScheme;
    AssertEquals('foreground', LongInt(clLime),
      LongInt(Scheme[tweStrongTag].Foreground));
    AssertEquals('background', LongInt(clYellow),
      LongInt(Scheme[tweStrongTag].Background));
    AssertTrue('has background', Scheme[tweStrongTag].HasBackground);
    AssertTrue('style', Scheme[tweStrongTag].FontStyle =
      [fsItalic, fsUnderline]);
  finally
    Ini.Free;
    ResetTheWordSyntaxColorScheme;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.ColorSchemeInvalidValuesFallBackToDefaults;
var
  Defaults: TTheWordSyntaxColorScheme;
  Scheme: TTheWordSyntaxColorScheme;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    GetDefaultTheWordSyntaxColorScheme(Defaults);
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'html.foreground',
      'not-a-color');
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'html.background',
      'also-not-a-color');
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'html.style',
      'bold,blink');

    LoadTheWordSyntaxColorScheme(Ini);
    Scheme := GetTheWordSyntaxColorScheme;
    AssertElementStyle('html fallback', Defaults[tweHtmlTag],
      Scheme[tweHtmlTag]);
  finally
    Ini.Free;
    ResetTheWordSyntaxColorScheme;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.ColorSchemeResetRestoresDefaults;
var
  Defaults: TTheWordSyntaxColorScheme;
  Scheme: TTheWordSyntaxColorScheme;
begin
  GetDefaultTheWordSyntaxColorScheme(Defaults);
  Scheme := Defaults;
  Scheme[tweStrongTag].Foreground := clAqua;
  Scheme[tweStrongTag].FontStyle := [fsItalic];
  SetTheWordSyntaxColorScheme(Scheme);

  ResetTheWordSyntaxColorScheme;
  Scheme := GetTheWordSyntaxColorScheme;
  AssertElementStyle('strong reset', Defaults[tweStrongTag],
    Scheme[tweStrongTag]);
end;

procedure TTheWordSyntaxHighlighterTests.ColorSchemeSavesCustomValues;
var
  Defaults: TTheWordSyntaxColorScheme;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    GetDefaultTheWordSyntaxColorScheme(Defaults);
    Defaults[tweStrongTag].Foreground := clLime;
    Defaults[tweStrongTag].Background := clYellow;
    Defaults[tweStrongTag].HasBackground := True;
    Defaults[tweStrongTag].FontStyle := [fsBold, fsUnderline];
    SetTheWordSyntaxColorScheme(Defaults);

    SaveTheWordSyntaxColorScheme(Ini);
    AssertEquals('foreground', ColorToString(clLime),
      Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'strong.foreground', ''));
    AssertEquals('background', ColorToString(clYellow),
      Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'strong.background', ''));
    AssertEquals('style', 'bold,underline',
      Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'strong.style', ''));
  finally
    Ini.Free;
    ResetTheWordSyntaxColorScheme;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.ColorSchemeAppliesToHighlighter;
var
  Highlighter: TTheWordSyntaxHighlighter;
  Scheme: TTheWordSyntaxColorScheme;
begin
  Highlighter := TTheWordSyntaxHighlighter.Create(nil);
  try
    GetDefaultTheWordSyntaxColorScheme(Scheme);
    Scheme[tweStrongTag].Foreground := clPurple;
    Scheme[tweStrongTag].FontStyle := [fsItalic];
    Scheme[tweMorphTag].Foreground := clOlive;
    Scheme[tweMorphTag].Background := clYellow;
    Scheme[tweMorphTag].HasBackground := True;
    Scheme[tweHtmlTag].Foreground := clTeal;
    Scheme[tweHtmlTag].FontStyle := [fsUnderline];

    ApplyTheWordSyntaxColorScheme(Highlighter, Scheme);
    AssertAttributeStyle('strong', Highlighter.StrongTagAttri,
      Scheme[tweStrongTag]);
    AssertAttributeStyle('strong/morph legacy',
      Highlighter.StrongMorphTagAttri, Scheme[tweStrongTag]);
    AssertAttributeStyle('morphology', Highlighter.MorphTagAttri,
      Scheme[tweMorphTag]);
    AssertAttributeStyle('html', Highlighter.HtmlTagAttri,
      Scheme[tweHtmlTag]);
  finally
    Highlighter.Free;
    ResetTheWordSyntaxColorScheme;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.EditorFontDefaultsAreApplied;
var
  Defaults: TTheWordSyntaxEditorFont;
  EditorFont: TTheWordSyntaxEditorFont;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    GetDefaultTheWordSyntaxEditorFont(Defaults);
    LoadTheWordSyntaxEditorFont(Ini);
    EditorFont := GetTheWordSyntaxEditorFont;
    AssertEquals('font name', Defaults.Name, EditorFont.Name);
    AssertEquals('font size', Defaults.Size, EditorFont.Size);
  finally
    Ini.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.EditorFontLoadsCustomValues;
var
  EditorFont: TTheWordSyntaxEditorFont;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.name',
      'Consolas');
    Ini.WriteInteger(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.size',
      13);

    LoadTheWordSyntaxEditorFont(Ini);
    EditorFont := GetTheWordSyntaxEditorFont;
    AssertEquals('font name', 'Consolas', EditorFont.Name);
    AssertEquals('font size', 13, EditorFont.Size);
  finally
    Ini.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.EditorFontInvalidValuesFallBackToDefaults;
var
  Defaults: TTheWordSyntaxEditorFont;
  EditorFont: TTheWordSyntaxEditorFont;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    GetDefaultTheWordSyntaxEditorFont(Defaults);
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.name', '');
    Ini.WriteInteger(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.size', -1);

    LoadTheWordSyntaxEditorFont(Ini);
    EditorFont := GetTheWordSyntaxEditorFont;
    AssertEquals('font name', Defaults.Name, EditorFont.Name);
    AssertEquals('font size', Defaults.Size, EditorFont.Size);
  finally
    Ini.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.EditorFontSavesCustomValues;
var
  EditorFont: TTheWordSyntaxEditorFont;
  Ini: TMemIniFile;
begin
  Ini := CreateTestIni;
  try
    EditorFont.Name := 'Consolas';
    EditorFont.Size := 14;
    SetTheWordSyntaxEditorFont(EditorFont);

    SaveTheWordSyntaxEditorFont(Ini);
    AssertEquals('font name', 'Consolas',
      Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.name', ''));
    AssertEquals('font size', 14,
      Ini.ReadInteger(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.size', 0));
  finally
    Ini.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.EditorFontAppliesToEditor;
var
  Editor: TSynEdit;
  EditorFont: TTheWordSyntaxEditorFont;
begin
  Editor := TSynEdit.Create(nil);
  try
    EditorFont.Name := 'Consolas';
    EditorFont.Size := 15;
    ApplyTheWordSyntaxEditorFont(Editor, EditorFont);
    AssertEquals('font name', 'Consolas', Editor.Font.Name);
    AssertEquals('font size', 15, Editor.Font.Size);
    AssertEquals('font quality', Ord(fqAntialiased), Ord(Editor.Font.Quality));
  finally
    Editor.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.WysiwygEditorKeepsRawSyntaxFont;
var
  ComponentIndex: Integer;
  Editor: TTheWordWysiwygEditorControl;
  RawEditor: TSynEdit;
  SyntaxFont: TTheWordSyntaxEditorFont;
  VerseFont: TFont;
begin
  SyntaxFont.Name := 'Courier New';
  SyntaxFont.Size := 12;
  SetTheWordSyntaxEditorFont(SyntaxFont);
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  VerseFont := TFont.Create;
  try
    RawEditor := nil;
    for ComponentIndex := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[ComponentIndex] is TSynEdit then
      begin
        RawEditor := TSynEdit(Editor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('raw editor exists', Assigned(RawEditor));

    VerseFont.Name := 'Times New Roman';
    VerseFont.Size := 22;
    Editor.SetEditorFont(VerseFont);

    AssertEquals('raw editor keeps configured font name', SyntaxFont.Name,
      RawEditor.Font.Name);
    AssertEquals('raw editor keeps configured font size', SyntaxFont.Size,
      RawEditor.Font.Size);
  finally
    VerseFont.Free;
    Editor.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagPairFindsClosingTheWordTag;
var
  Lines: TStringList;
  CurrentTag: TTheWordTagPairPosition;
  MatchingTag: TTheWordTagPairPosition;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<FI>word<Fi>');
    AssertTrue('pair found', FindTheWordMatchingTagPairAt(Lines,
      Point(2, 1), CurrentTag, MatchingTag));
    AssertTagPosition('current', CurrentTag, 1, 1, 5);
    AssertTagPosition('matching', MatchingTag, 1, 9, 13);
  finally
    Lines.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagPairFindsOpeningTheWordTag;
var
  Lines: TStringList;
  CurrentTag: TTheWordTagPairPosition;
  MatchingTag: TTheWordTagPairPosition;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<FI>word<Fi>');
    AssertTrue('pair found', FindTheWordMatchingTagPairAt(Lines,
      Point(10, 1), CurrentTag, MatchingTag));
    AssertTagPosition('current', CurrentTag, 1, 9, 13);
    AssertTagPosition('matching', MatchingTag, 1, 1, 5);
  finally
    Lines.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagPairHandlesNestedTheWordTags;
var
  Lines: TStringList;
  CurrentTag: TTheWordTagPairPosition;
  MatchingTag: TTheWordTagPairPosition;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<FI>a<FI>b<Fi>c<Fi>');
    AssertTrue('pair found', FindTheWordMatchingTagPairAt(Lines,
      Point(2, 1), CurrentTag, MatchingTag));
    AssertTagPosition('current', CurrentTag, 1, 1, 5);
    AssertTagPosition('matching', MatchingTag, 1, 16, 20);
  finally
    Lines.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagPairFindsAcrossLines;
var
  Lines: TStringList;
  CurrentTag: TTheWordTagPairPosition;
  MatchingTag: TTheWordTagPairPosition;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<RF q=a>note');
    Lines.Add('more<Rf>');
    AssertTrue('pair found', FindTheWordMatchingTagPairAt(Lines,
      Point(2, 1), CurrentTag, MatchingTag));
    AssertTagPosition('current', CurrentTag, 1, 1, 9);
    AssertTagPosition('matching', MatchingTag, 2, 5, 9);
  finally
    Lines.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagPairFindsHtmlClosingTag;
var
  Lines: TStringList;
  CurrentTag: TTheWordTagPairPosition;
  MatchingTag: TTheWordTagPairPosition;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<i>treva</i>');
    AssertTrue('pair found', FindTheWordMatchingTagPairAt(Lines,
      Point(2, 1), CurrentTag, MatchingTag));
    AssertTagPosition('current', CurrentTag, 1, 1, 4);
    AssertTagPosition('matching', MatchingTag, 1, 9, 13);
  finally
    Lines.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagPairIgnoresUnmatchedTags;
var
  Lines: TStringList;
  CurrentTag: TTheWordTagPairPosition;
  MatchingTag: TTheWordTagPairPosition;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('<FI>word');
    AssertFalse('pair found', FindTheWordMatchingTagPairAt(Lines,
      Point(2, 1), CurrentTag, MatchingTag));
  finally
    Lines.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.ConfigureEditorInstallsTagPairMarkup;
var
  Editor: TSynEdit;
begin
  Editor := TSynEdit.Create(nil);
  try
    ConfigureTheWordSyntaxEditor(Editor, False);
    AssertNotNull('tag pair markup',
      Editor.MarkupByClass[TTheWordTagPairMarkup]);
  finally
    Editor.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagCompletionFiltersByPrefix;
var
  Items: TStringList;
begin
  Items := TStringList.Create;
  try
    GetTheWordTagCompletionItems('F', Items);
    AssertTrue('FI suggested', CompletionItemsContainTag(Items, 'FI'));
    AssertTrue('FO suggested', CompletionItemsContainTag(Items, 'FO'));
    AssertFalse('CM not suggested', CompletionItemsContainTag(Items, 'CM'));

    GetTheWordTagCompletionItems('fo', Items);
    AssertTrue('case-insensitive FO suggested',
      CompletionItemsContainTag(Items, 'FO'));
    AssertFalse('FI filtered out', CompletionItemsContainTag(Items, 'FI'));
  finally
    Items.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagCompletionSnippetsUseClosingTags;
var
  CaretOffset: Integer;
  Snippet: string;
begin
  AssertTrue('FI snippet', TryGetTheWordTagCompletionSnippet('FI',
    Snippet, CaretOffset));
  AssertEquals('FI snippet text', '<FI><Fi>', Snippet);
  AssertEquals('FI caret', Length('<FI>'), CaretOffset);

  AssertTrue('HTML snippet', TryGetTheWordTagCompletionSnippet('i',
    Snippet, CaretOffset));
  AssertEquals('HTML snippet text', '<i></i>', Snippet);
  AssertEquals('HTML caret', Length('<i>'), CaretOffset);

  AssertTrue('WT snippet', TryGetTheWordTagCompletionSnippet('WT',
    Snippet, CaretOffset));
  AssertEquals('WT snippet text', '<WT>', Snippet);
  AssertEquals('WT caret', Length('<WT'), CaretOffset);
end;

procedure TTheWordSyntaxHighlighterTests.TagCompletionIndentTagsAreNotPairs;
var
  CaretOffset: Integer;
  CurrentTag: TTheWordTagPairPosition;
  Lines: TStringList;
  MatchingTag: TTheWordTagPairPosition;
  Snippet: string;
begin
  AssertTrue('PI snippet', TryGetTheWordTagCompletionSnippet('PI',
    Snippet, CaretOffset));
  AssertEquals('PI text', '<PI>', Snippet);
  AssertEquals('PI caret', Length('<PI>'), CaretOffset);

  AssertTrue('PF snippet', TryGetTheWordTagCompletionSnippet('PF',
    Snippet, CaretOffset));
  AssertEquals('PF text', '<PF>', Snippet);
  AssertEquals('PF caret', Length('<PF>'), CaretOffset);

  Lines := TStringList.Create;
  try
    Lines.Add('<PI>indented<Pi>');
    AssertFalse('PI is not a pair', FindTheWordMatchingTagPairAt(Lines,
      Point(2, 1), CurrentTag, MatchingTag));
  finally
    Lines.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagCompletionCompletesTagAtCaret;
var
  Editor: TSynEdit;
begin
  Editor := TSynEdit.Create(nil);
  try
    ConfigureTheWordSyntaxEditor(Editor, False);
    Editor.Text := '<F';
    Editor.LogicalCaretXY := Point(3, 1);
    AssertTrue('completed', CompleteTheWordTagAtCaret(Editor, 'FI'));
    AssertEquals('text', '<FI><Fi>', Editor.Lines[0]);
    AssertEquals('caret x', Length('<FI>') + 1, Editor.LogicalCaretXY.X);
  finally
    Editor.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagCompletionAutoClosesTypedTag;
var
  Editor: TSynEdit;
begin
  Editor := TSynEdit.Create(nil);
  try
    ConfigureTheWordSyntaxEditor(Editor, False);
    Editor.Text := '<FI>';
    Editor.LogicalCaretXY := Point(5, 1);
    AssertTrue('closed', InsertTheWordAutoClosingTagAtCaret(Editor));
    AssertEquals('text', '<FI><Fi>', Editor.Lines[0]);
    AssertEquals('caret x', Length('<FI>') + 1, Editor.LogicalCaretXY.X);
  finally
    Editor.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.TagCompletionDoesNotAutoCloseSingleTag;
var
  Editor: TSynEdit;
begin
  Editor := TSynEdit.Create(nil);
  try
    ConfigureTheWordSyntaxEditor(Editor, False);
    Editor.Text := '<CM>';
    Editor.LogicalCaretXY := Point(5, 1);
    AssertFalse('not closed', InsertTheWordAutoClosingTagAtCaret(Editor));
    AssertEquals('text', '<CM>', Editor.Lines[0]);
    AssertEquals('caret x', 5, Editor.LogicalCaretXY.X);
  finally
    Editor.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.ConfigureEditorInstallsTagCompletionOnlyWhenEditable;
var
  Editor: TSynEdit;
begin
  Editor := TSynEdit.Create(nil);
  try
    ConfigureTheWordSyntaxEditor(Editor, False);
    AssertTrue('editable editor has completion',
      TheWordSyntaxEditorHasTagCompletion(Editor));

    ConfigureTheWordSyntaxEditor(Editor, True);
    AssertFalse('read-only editor has no completion',
      TheWordSyntaxEditorHasTagCompletion(Editor));
  finally
    Editor.Free;
    ResetTheWordSyntaxEditorFont;
  end;
end;

initialization
  RegisterTest(TTheWordSyntaxHighlighterTests);

end.
