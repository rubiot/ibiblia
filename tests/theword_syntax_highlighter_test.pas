unit theword_syntax_highlighter_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, IniFiles, StdCtrls, LCLType,
  SynEdit,
  SynEditHighlighter,
  fpcunit, testregistry, theword_syntax_highlighter,
  directwrite_verse_editor, theword_wysiwyg_editor_control,
  theword_ont_codec_v2, theword_verse_visual_adapter_v2,
  verse_visual_document;

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
    procedure WysiwygRawEditorCtrlWheelZoomsWithoutModifyingText;
    procedure WysiwygVisualCtrlWheelZoomsAndPersistsWithoutModifyingText;
    procedure WysiwygVisualKeyboardRoutesParagraphAndSemantics;
    procedure WysiwygEmptyTitleCommandsAndWholeTitleDeletion;
    procedure WysiwygTitleSelectionWrapsChangesAndUnwraps;
    procedure WysiwygTitleClipboardPreservesFormattingAndBoundaries;
    procedure WysiwygTitleCaretAndPendingFormatting;
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

procedure TTheWordSyntaxHighlighterTests.
  WysiwygRawEditorCtrlWheelZoomsWithoutModifyingText;
var
  ComponentIndex: Integer;
  Editor: TTheWordWysiwygEditorControl;
  Handled: Boolean;
  InitialSize: Integer;
  RawEditor: TSynEdit;
begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    RawEditor := nil;
    for ComponentIndex := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[ComponentIndex] is TSynEdit then
      begin
        RawEditor := TSynEdit(Editor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('raw editor exists', Assigned(RawEditor));

    Editor.Text := '<FI>word<Fi>';
    Editor.RawMode := True;
    AssertFalse('entering raw mode does not modify verse text',
      Editor.Modified);
    InitialSize := RawEditor.Font.Size;
    Handled := False;
    RawEditor.OnMouseWheel(RawEditor, [ssCtrl], 120, Point(0, 0), Handled);

    AssertTrue('Ctrl+wheel is handled', Handled);
    AssertEquals('raw editor font grows', InitialSize + 1,
      RawEditor.Font.Size);
    AssertFalse('font zoom does not modify verse text', Editor.Modified);
    AssertEquals('raw text remains unchanged', '<FI>word<Fi>', Editor.Text);
  finally
    Editor.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.
  WysiwygVisualCtrlWheelZoomsAndPersistsWithoutModifyingText;
var
  BaseFont: TFont;
  ComponentIndex: Integer;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
  Handled: Boolean;
  InitialSize: Integer;
begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  BaseFont := TFont.Create;
  try
    DirectEditor := nil;
    for ComponentIndex := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[ComponentIndex] is TDirectWriteVerseEditor then
      begin
        DirectEditor := TDirectWriteVerseEditor(
          Editor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('DirectWrite editor exists', Assigned(DirectEditor));

    BaseFont.Name := 'Times New Roman';
    BaseFont.Size := 18;
    Editor.SetEditorFont(BaseFont);
    Editor.Text := '<FO>word<Fo>';
    DirectEditor.SetSelection(1, 2);
    InitialSize := DirectEditor.Font.Size;

    Handled := False;
    DirectEditor.OnMouseWheel(DirectEditor, [], 120, Point(0, 0), Handled);
    AssertFalse('wheel without Ctrl remains available for scrolling', Handled);
    AssertEquals('ordinary wheel does not zoom', InitialSize,
      DirectEditor.Font.Size);

    Handled := False;
    DirectEditor.OnMouseWheel(DirectEditor, [ssCtrl], 120,
      Point(0, 0), Handled);
    AssertTrue('Ctrl+wheel is handled', Handled);
    AssertEquals('DirectWrite font grows', InitialSize + 1,
      DirectEditor.Font.Size);
    AssertFalse('visual zoom does not modify verse text', Editor.Modified);
    AssertEquals('raw text remains unchanged', '<FO>word<Fo>', Editor.Text);
    AssertEquals('visual zoom keeps selection start', 1,
      DirectEditor.SelectionStart);
    AssertEquals('visual zoom keeps selection length', 2,
      DirectEditor.SelectionLength);
    AssertEquals('visual zoom keeps active caret', 3,
      DirectEditor.CaretOffset);
    AssertFalse('visual zoom does not create an undo step', Editor.Undo);

    Editor.Text := 'next verse';
    AssertEquals('zoom survives reloading the visual document',
      InitialSize + 1, DirectEditor.Font.Size);
    BaseFont.Size := 20;
    Editor.SetEditorFont(BaseFont);
    AssertEquals('zoom size survives verse font reload', InitialSize + 1,
      DirectEditor.Font.Size);

    while DirectEditor.Font.Size > 1 do
    begin
      Handled := False;
      DirectEditor.OnMouseWheel(DirectEditor, [ssCtrl], -120,
        Point(0, 0), Handled);
      AssertTrue('Ctrl+wheel down is handled', Handled);
    end;
    Handled := False;
    DirectEditor.OnMouseWheel(DirectEditor, [ssCtrl], -120,
      Point(0, 0), Handled);
    AssertTrue('Ctrl+wheel at minimum is handled', Handled);
    AssertEquals('DirectWrite font has a one-point minimum', 1,
      DirectEditor.Font.Size);
    AssertFalse('minimum-size zoom does not modify verse text',
      Editor.Modified);
  finally
    BaseFont.Free;
    Editor.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.
  WysiwygVisualKeyboardRoutesParagraphAndSemantics;
var
  Broker: TEdit;
  ComponentIndex: Integer;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;

  function PressKey(AKey: Word; AShift: TShiftState): Word;
  begin
    Result := AKey;
    AssertTrue('input broker key handler exists', Assigned(Broker.OnKeyDown));
    Broker.OnKeyDown(Broker, Result, AShift);
  end;

begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    DirectEditor := nil;
    for ComponentIndex := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[ComponentIndex] is TDirectWriteVerseEditor then
      begin
        DirectEditor := TDirectWriteVerseEditor(
          Editor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('DirectWrite editor exists', Assigned(DirectEditor));

    Broker := nil;
    for ComponentIndex := 0 to DirectEditor.ComponentCount - 1 do
      if DirectEditor.Components[ComponentIndex] is TEdit then
      begin
        Broker := TEdit(DirectEditor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('native input broker exists', Assigned(Broker));

    Editor.Text := 'alpha beta';
    DirectEditor.SetSelection(5, 0);
    AssertEquals('plain Enter is consumed', 0, PressKey(VK_RETURN, []));
    AssertEquals('Enter inserts structural CM rather than visible text',
      'alpha<CM> beta', Editor.Text);
    AssertEquals('caret follows the synthetic pilcrow', 6,
      DirectEditor.CaretOffset);
    AssertEquals('Backspace next to pilcrow is consumed', 0,
      PressKey(VK_BACK, []));
    AssertEquals('Backspace removes the adjacent CM', 'alpha beta',
      Editor.Text);
    AssertEquals('caret returns to the document anchor', 5,
      DirectEditor.CaretOffset);

    Editor.Text := 'alpha<CM> beta';
    DirectEditor.SetSelection(0, 7);
    AssertEquals('Backspace consumes a selection containing a pilcrow', 0,
      PressKey(VK_BACK, []));
    AssertEquals('Backspace removes selected text and CM atomically',
      'beta', Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals('combined Backspace deletion is one undo step',
      'alpha<CM> beta', Editor.Text);
    DirectEditor.SetSelection(0, 7);
    AssertEquals('Delete consumes a selection containing a pilcrow', 0,
      PressKey(VK_DELETE, []));
    AssertEquals('Delete follows the same atomic structural route',
      'beta', Editor.Text);

    Editor.Text := 'a<CM><CM>b';
    DirectEditor.SetSelection(1, 2);
    AssertEquals('Delete consumes two selected consecutive pilcrows', 0,
      PressKey(VK_DELETE, []));
    AssertEquals('each selected pilcrow removes one corresponding CM',
      'ab', Editor.Text);

    Editor.Text := '<TS>Head<Ts><CM>body';
    DirectEditor.SetSelection(6, 0);
    AssertEquals('Backspace at body start reaches the CM after title break',
      0, PressKey(VK_BACK, []));
    AssertEquals('title remains protected while adjacent CM is removed',
      '<TS>Head<Ts>body', Editor.Text);
    AssertEquals('caret remains at the beginning of the body', 5,
      DirectEditor.CaretOffset);

    Editor.Text := 'alpha beta';
    DirectEditor.SetSelection(0, 5);
    AssertEquals(0, PressKey(VK_RETURN, []));
    AssertEquals('Enter replaces a visual selection with one CM',
      '<CM> beta', Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals('selection replacement is one undo step', 'alpha beta',
      Editor.Text);

    Editor.Text := 'alpha<CM><CM>beta';
    DirectEditor.SetSelection(7, 0);
    AssertEquals(0, PressKey(VK_BACK, []));
    AssertEquals('Backspace removes only one consecutive CM',
      'alpha<CM>beta', Editor.Text);
    DirectEditor.SetSelection(5, 0);
    AssertEquals(0, PressKey(VK_DELETE, []));
    AssertEquals('Delete removes the one forward CM', 'alphabeta',
      Editor.Text);

    Editor.Text := 'word';
    DirectEditor.SetSelection(0, 4);
    AssertEquals(0, PressKey(VK_I, [ssCtrl]));
    AssertEquals('Ctrl+I routes to FI', '<FI>word<Fi>', Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals(0, PressKey(VK_J, [ssCtrl]));
    AssertEquals('Ctrl+J routes to FR', '<FR>word<Fr>', Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals(0, PressKey(VK_O, [ssCtrl]));
    AssertEquals('Ctrl+O routes to FO', '<FO>word<Fo>', Editor.Text);
    AssertTrue(Editor.Undo);

    AssertEquals(0, PressKey(VK_S, [ssCtrl, ssShift]));
    AssertEquals('Ctrl+Shift+S routes to strike', '<s>word</s>',
      Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals(0, PressKey(VK_OEM_PLUS, [ssCtrl, ssShift]));
    AssertEquals('Ctrl+Shift+= routes to superscript',
      '<sup>word</sup>', Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals(0, PressKey(VK_OEM_PLUS, [ssCtrl]));
    AssertEquals('Ctrl+= routes to subscript', '<sub>word</sub>',
      Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals('AltGr-like Ctrl+Alt must not be captured', VK_I,
      PressKey(VK_I, [ssCtrl, ssAlt]));
    AssertEquals('ignored AltGr-like chord does not change text', 'word',
      Editor.Text);
    DirectEditor.SetSelection(0, 0);
    AssertEquals('Shift+Enter routes to a soft line break', 0,
      PressKey(VK_RETURN, [ssShift]));
    AssertEquals('Shift+Enter inserts CL', '<CL>word', Editor.Text);
    AssertTrue(Editor.Undo);
    AssertEquals('unsupported modified Enter must not be captured',
      VK_RETURN, PressKey(VK_RETURN, [ssCtrl]));
    AssertEquals('unsupported modified Enter does not alter text', 'word',
      Editor.Text);
  finally
    Editor.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.
  WysiwygEmptyTitleCommandsAndWholeTitleDeletion;
var
  Broker: TEdit;
  ComponentIndex: Integer;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
  Key: Word;
  Title: TTheWordTitleRange;
begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    DirectEditor := nil;
    for ComponentIndex := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[ComponentIndex] is TDirectWriteVerseEditor then
      begin
        DirectEditor := TDirectWriteVerseEditor(
          Editor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('DirectWrite editor exists', Assigned(DirectEditor));

    Broker := nil;
    for ComponentIndex := 0 to DirectEditor.ComponentCount - 1 do
      if DirectEditor.Components[ComponentIndex] is TEdit then
      begin
        Broker := TEdit(DirectEditor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('native input broker exists', Assigned(Broker));

    Editor.Text := '<TS><Ts>body';
    DirectEditor.SetSelection(0, 0);
    AssertEquals('title-side caret remains before the synthetic break', 0,
      DirectEditor.SelectionStart);

    DirectEditor.SetSelection(1, 0);
    AssertEquals('body-side caret remains after the synthetic break', 1,
      DirectEditor.SelectionStart);
    AssertFalse('body-side caret does not resolve the empty title',
      Editor.TryGetCurrentTitle(Title));
    DirectEditor.SetSelection(0, 0);
    AssertTrue('empty title is resolved at its title-side caret',
      Editor.TryGetCurrentTitle(Title));
    AssertEquals('empty title starts at document anchor', 0,
      Title.StartOffset);
    AssertEquals('empty title has zero length', 0, Title.EndOffset);
    AssertEquals('legacy TS is level one', 1, Title.Level);
    AssertTrue('empty title level can be changed without a placeholder',
      Editor.ChangeCurrentTitleLevel(2));
    AssertEquals('level change preserves empty title literally',
      '<TS2><Ts>body', Editor.Text);
    AssertTrue('empty title remains selected after refresh',
      Editor.TryGetCurrentTitle(Title));
    AssertTrue('empty title can be removed from its caret',
      Editor.RemoveCurrentTitle);
    AssertEquals('removal leaves body intact', 'body', Editor.Text);

    Editor.Text := '<TS><Ts><TS2><Ts>body';
    DirectEditor.SetSelection(0, 0);
    AssertFalse('two empty titles at one anchor are intentionally ambiguous',
      Editor.TryGetCurrentTitle(Title));
    AssertFalse('ambiguous empty title cannot change level',
      Editor.ChangeCurrentTitleLevel(3));
    AssertFalse('ambiguous empty title cannot be removed',
      Editor.RemoveCurrentTitle);
    AssertEquals('ambiguous titles remain untouched',
      '<TS><Ts><TS2><Ts>body', Editor.Text);

    Editor.Text := 'left<TS>Title<Ts>right';
    { The title is laid out after the synthetic break following "left". }
    DirectEditor.SetSelection(5, 5);
    Key := VK_DELETE;
    AssertTrue('native input broker key handler exists',
      Assigned(Broker.OnKeyDown));
    Broker.OnKeyDown(Broker, Key, []);
    AssertEquals('Delete is consumed for the selected whole title', 0, Key);
    AssertEquals('deleting all title text removes its wrapper and joins body',
      'left right', Editor.Text);
    AssertTrue('whole-title deletion is one undo step', Editor.Undo);
    AssertEquals('undo restores title text and wrapper',
      'left<TS>Title<Ts>right', Editor.Text);
  finally
    Editor.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.
  WysiwygTitleSelectionWrapsChangesAndUnwraps;
var
  ComponentIndex: Integer;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
  Title: TTheWordTitleRange;
begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    DirectEditor := nil;
    for ComponentIndex := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[ComponentIndex] is TDirectWriteVerseEditor then
      begin
        DirectEditor := TDirectWriteVerseEditor(
          Editor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('DirectWrite editor exists', Assigned(DirectEditor));

    Editor.Text := 'alphabeta body';
    DirectEditor.SetSelection(0, 0);
    AssertFalse('ordinary body caret cannot create an empty title',
      Editor.CanApplyTitle);
    DirectEditor.SetSelection(0, 5);
    AssertTrue('a body selection can become a title', Editor.CanApplyTitle);
    AssertTrue(Editor.ApplyTitleLevel(1));
    AssertEquals('<TS>alpha<Ts>beta body', Editor.Text);
    AssertEquals('wrapping preserves the selected title text', 5,
      DirectEditor.SelectionLength);

    DirectEditor.SetSelection(1, 3);
    AssertTrue('inline HTML remains editable inside a title',
      Editor.ApplySemantic(twsmHtmlBold));
    AssertEquals('<TS>a<b>lph</b>a<Ts>beta body', Editor.Text);
    DirectEditor.SetSelection(0, 5);
    AssertTrue('one title is recognized across differently styled spans',
      Editor.TryGetCurrentTitle(Title));
    AssertTrue('the unified command changes the current title level',
      Editor.ApplyTitleLevel(2));
    AssertEquals('<TS2>a<b>lph</b>a<Ts>beta body', Editor.Text);
    AssertEquals('changing level preserves a multi-span selection', 5,
      DirectEditor.SelectionLength);

    AssertTrue('font color remains editable inside a title',
      Editor.ApplyFontColor(True, LongInt(clRed)));
    AssertEquals('font color preserves the title selection', 5,
      DirectEditor.SelectionLength);
    AssertTrue('relative size remains editable inside a title',
      Editor.AdjustRelativeFontSize(1));
    AssertEquals('relative size preserves the title selection', 5,
      DirectEditor.SelectionLength);
    AssertTrue('removing title formatting keeps inline formatting',
      Editor.RemoveCurrentTitle);
    AssertEquals('unwrapping preserves the selection', 5,
      DirectEditor.SelectionLength);
    AssertEquals('title opening tag is removed', 0,
      Pos('<TS', Editor.Text));
    AssertEquals('title closing tag is removed', 0,
      Pos('<Ts>', Editor.Text));
    AssertTrue('inline bold opening survives title removal',
      Pos('<b>', Editor.Text) > 0);
    AssertTrue('inline bold closing survives title removal',
      Pos('</b>', Editor.Text) > 0);

    Editor.Text := 'alphabeta body';
    DirectEditor.SetSelection(0, 5);
    AssertTrue(Editor.ApplyTitleLevel(1));
    DirectEditor.SetSelection(6, 4);
    AssertTrue('the adjacent body range can become a second title',
      Editor.ApplyTitleLevel(2));
    AssertEquals('consecutive titles need no empty separator paragraph',
      '<TS>alpha<Ts><TS2>beta<Ts> body', Editor.Text);
    AssertEquals('consecutive title visual text has one line boundary', 1,
      Pos(UTF8Encode('alpha' + #10 + 'beta'),
        UTF8Encode(DirectEditor.Document.Text)));
  finally
    Editor.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.
  WysiwygTitleClipboardPreservesFormattingAndBoundaries;
var
  ComponentIndex: Integer;
  Handled: Boolean;
  SourceDirect, TargetDirect: TDirectWriteVerseEditor;
  Source, Target: TTheWordWysiwygEditorControl;
begin
  EnableTheWordClipboardFixture('');
  Source := TTheWordWysiwygEditorControl.Create(nil);
  Target := TTheWordWysiwygEditorControl.Create(nil);
  try
    SourceDirect := nil;
    for ComponentIndex := 0 to Source.ComponentCount - 1 do
      if Source.Components[ComponentIndex] is TDirectWriteVerseEditor then
      begin
        SourceDirect := TDirectWriteVerseEditor(
          Source.Components[ComponentIndex]);
        Break;
      end;
    TargetDirect := nil;
    for ComponentIndex := 0 to Target.ComponentCount - 1 do
      if Target.Components[ComponentIndex] is TDirectWriteVerseEditor then
      begin
        TargetDirect := TDirectWriteVerseEditor(
          Target.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('source DirectWrite editor exists', Assigned(SourceDirect));
    AssertTrue('target DirectWrite editor exists', Assigned(TargetDirect));

    Source.Text := '<TS><b>A</b><i>B</i><u>C</u>' +
      '<font color=red size=+1>D</font><Ts>body';
    Handled := False;
    SourceDirect.OnClipboardCopy(SourceDirect, 0, 4, False, Handled);
    AssertTrue('copy inside one title is handled semantically', Handled);
    AssertEquals('formatted title copy exposes its visible text', 'ABCD',
      UTF8Encode(TheWordClipboardPlainTextFixture));

    Handled := False;
    SourceDirect.OnClipboardCopy(SourceDirect, 3, 3, False, Handled);
    AssertTrue('copy crossing the title boundary is consumed', Handled);
    AssertEquals('cross-boundary copy preserves the prior valid clipboard',
      'ABCD', UTF8Encode(TheWordClipboardPlainTextFixture));

    Target.Text := '<TS2>XY<Ts>tail';
    Handled := False;
    TargetDirect.OnClipboardPaste(TargetDirect, 1, 1, Handled);
    AssertTrue('private title payload is accepted inside a title', Handled);
    AssertEquals('private payload keeps formatting and the target TS pair',
      '<TS2>X<b>A</b><i>B</i><u>C</u>' +
      '<font color=red size=+1>D</font><Ts>tail', Target.Text);

    Handled := False;
    TargetDirect.OnClipboardPaste(TargetDirect, 4, 2, Handled);
    AssertTrue('paste crossing the title boundary is consumed', Handled);
    AssertEquals('cross-boundary paste leaves the title unchanged',
      '<TS2>X<b>A</b><i>B</i><u>C</u>' +
      '<font color=red size=+1>D</font><Ts>tail', Target.Text);

    AssertTrue('formatted title paste is undoable', Target.Undo);
    AssertEquals('one undo restores the complete target title',
      '<TS2>XY<Ts>tail', Target.Text);
    AssertFalse('formatted title paste creates exactly one undo entry',
      Target.Undo);
  finally
    DisableTheWordClipboardFixture;
    Target.Free;
    Source.Free;
  end;
end;

procedure TTheWordSyntaxHighlighterTests.
  WysiwygTitleCaretAndPendingFormatting;
var
  Accepted: Boolean;
  ComponentIndex, NewCaret: Integer;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
  Span: TVisualSpan;
  Style: TVisualTextStyle;

  procedure AssertInsertionStyle(ALayoutOffset, AFontDelta: Integer;
    const AFlags: TVisualTextStyleFlags);
  begin
    AssertTrue('insertion style callback exists',
      Assigned(DirectEditor.OnResolveInsertionStyle));
    AssertTrue('insertion style resolves at layout offset ' +
      IntToStr(ALayoutOffset),
      DirectEditor.OnResolveInsertionStyle(DirectEditor,
        ALayoutOffset, Style));
    AssertEquals('insertion font delta at layout offset ' +
      IntToStr(ALayoutOffset), AFontDelta, Style.FontSizeDelta);
    AssertTrue('insertion flags at layout offset ' +
      IntToStr(ALayoutOffset), Style.Flags = AFlags);
  end;

  procedure TypeAt(ALayoutOffset: Integer;
    const AText: UnicodeString);
  begin
    Accepted := False;
    NewCaret := -1;
    AssertTrue('text replace callback exists',
      Assigned(DirectEditor.OnReplaceText));
    DirectEditor.OnReplaceText(DirectEditor, ALayoutOffset, 0,
      AText, Accepted, NewCaret);
    AssertTrue('title text input is accepted', Accepted);
    AssertTrue('new caret is returned', NewCaret >= 0);
    DirectEditor.SetSelection(NewCaret, 0);
  end;

begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    DirectEditor := nil;
    for ComponentIndex := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[ComponentIndex] is TDirectWriteVerseEditor then
      begin
        DirectEditor := TDirectWriteVerseEditor(
          Editor.Components[ComponentIndex]);
        Break;
      end;
    AssertTrue('DirectWrite editor exists', Assigned(DirectEditor));

    Editor.Text := '<TS>one<Ts><TS2>two<Ts>body';
    AssertEquals(UnicodeString('one' + #10 + 'two' + #10 + 'body'),
      DirectEditor.Document.Text);
    AssertInsertionStyle(3, 2, [vtsfBold]);
    AssertInsertionStyle(4, 1, [vtsfBold, vtsfItalic]);
    AssertInsertionStyle(7, 1, [vtsfBold, vtsfItalic]);
    AssertInsertionStyle(8, 0, []);

    Editor.Text := '<TS><Ts>body';
    DirectEditor.SetSelection(0, 0);
    AssertTrue('A+ can be armed in an empty title',
      Editor.AdjustRelativeFontSize(1));
    AssertTrue('font color can be armed in an empty title',
      Editor.ApplyFontColor(True, LongInt(clRed)));
    AssertInsertionStyle(0, 4, [vtsfBold]);
    AssertTrue(Style.HasForegroundColor);
    AssertEquals(LongInt(clRed), Style.ForegroundColor);
    TypeAt(0, 'X');
    TypeAt(NewCaret, 'Y');
    AssertEquals(UnicodeString('XY' + #10 + 'body'),
      DirectEditor.Document.Text);
    AssertTrue(DirectEditor.Document.TryGetSpanAt(0, Span));
    AssertEquals('first typed character keeps title + pending size',
      4, Span.Style.FontSizeDelta);
    AssertEquals(LongInt(clRed), Span.Style.ForegroundColor);
    AssertTrue(DirectEditor.Document.TryGetSpanAt(1, Span));
    AssertEquals('second contiguous character keeps pending size',
      4, Span.Style.FontSizeDelta);
    AssertEquals('second contiguous character keeps pending color',
      LongInt(clRed), Span.Style.ForegroundColor);
    AssertTrue('second character is one undo command', Editor.Undo);
    AssertEquals(UnicodeString('X' + #10 + 'body'),
      DirectEditor.Document.Text);
    AssertTrue('first character is one undo command', Editor.Undo);
    AssertEquals('<TS><Ts>body', Editor.Text);
    AssertFalse('no hidden extra undo is created', Editor.Undo);

    Editor.Text := '<TS1><font color=red>ab</font><Ts>body';
    DirectEditor.SetSelection(1, 0);
    AssertTrue('Default color can be armed inside TS1',
      Editor.ApplyFontColor(False, 0));
    AssertInsertionStyle(1, 2, [vtsfBold]);
    AssertTrue('TS1 structural gray remains visible in preview',
      Style.HasForegroundColor);
    AssertEquals(THEWORD_TITLE_LEVEL1_COLOR, Style.ForegroundColor);
    TypeAt(1, 'X');
    AssertTrue(DirectEditor.Document.TryGetSpanAt(1, Span));
    AssertTrue('typed Default color reveals structural gray',
      Span.Style.HasForegroundColor);
    AssertEquals(THEWORD_TITLE_LEVEL1_COLOR,
      Span.Style.ForegroundColor);
    AssertTrue(Editor.Undo);
    AssertEquals('<TS1><font color=red>ab</font><Ts>body', Editor.Text);

    Editor.Text := '<TS2><font color=red>ab</font><Ts>body';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.ApplyFontColor(False, 0));
    AssertInsertionStyle(1, 1, [vtsfBold, vtsfItalic]);
    AssertFalse('TS2 Default color inherits the editor default',
      Style.HasForegroundColor);

    Editor.Text := '<TS3><font color=red>ab</font><Ts>body';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.ApplyFontColor(False, 0));
    AssertInsertionStyle(1, 0, [vtsfItalic]);
    AssertFalse('TS3 Default color inherits the editor default',
      Style.HasForegroundColor);

    Editor.Text := '<TS1><FR>ab<Fr><Ts>body';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.ApplyFontColor(False, 0));
    AssertInsertionStyle(1, 2, [vtsfBold]);
    AssertEquals('FR keeps semantic priority over pending Default',
      LongInt(clRed), Style.ForegroundColor);

    Editor.Text := '<TS1><FO>ab<Fo><Ts>body';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.ApplyFontColor(False, 0));
    AssertInsertionStyle(1, 2, [vtsfBold]);
    AssertEquals('FO keeps semantic priority over pending Default',
      LongInt(clBlack), Style.ForegroundColor);
  finally
    Editor.Free;
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
