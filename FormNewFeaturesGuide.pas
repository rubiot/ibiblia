unit FormNewFeaturesGuide;

{ Help > New features guide (Shift+F1): the features that came from the
  1.0.1 mod, described for this build. A topic list on the left, the text of
  the chosen topic on the right. The guide text is documentation and is kept
  in English; the window and menu captions are translated. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls;

procedure ShowNewFeaturesGuide(AOwner: TComponent);

resourcestring
  SNewFeaturesGuideTitle = 'New features guide';
  SNewFeaturesGuideMenu = '&New features guide';
  SNewFeaturesGuideHint = 'What came with the merged features, and how to use them';
  SNewFeaturesGuideClose = 'Close';

implementation

uses
  ui_style;

const
  TOPIC_COUNT = 12;

  TOPIC_TITLES: array[0..TOPIC_COUNT - 1] of string = (
    '1. Choosing words with the mouse wheel',
    '2. Dictionary popup and default dictionaries',
    '3. The control strip on each verse panel',
    '4. Editing: paragraphs, italics and saving',
    '5. Text to speech',
    '6. Skins and dark mode',
    '7. Importing and exporting Bibles, Apocrypha',
    '8. Status filter (F8) and Find in project (Ctrl+F)',
    '9. Synchronization with theWord and Timotheos',
    '10. 1.1.0 Visual WYSIWYG Editor & Translator Notes',
    '11. 1.1.0 BiDi Rendering & Translation Memory',
    '12. 1.1.0 Guided Wizard, Custom Statuses & AI Flows');

  TOPIC_TEXTS: array[0..TOPIC_COUNT - 1] of string = (
    // 1
    'MOUSE WHEEL WORD SELECTION' + LineEnding + LineEnding +
    'Put the pointer over the Source or the Destination text and turn the mouse wheel: ' +
    'the selection moves one source word to the left or to the right, and its linked ' +
    'words are shown in the other text.' + LineEnding + LineEnding +
    'LINKING SEVERAL WORDS' + LineEnding +
    '  - Hold Ctrl+Shift while you click the words of one phrase; they are linked together.' + LineEnding +
    '  - When you let go of Ctrl and Shift, the next source word that has no link yet is ' +
    'selected for you, so you can carry on without touching the mouse wheel.' + LineEnding + LineEnding +
    'Words that are not linked are shown in italics after you save the verse.',
    // 2
    'POPUP NEXT TO THE POINTER' + LineEnding + LineEnding +
    'In the Destination panel the dictionary popup opens next to the mouse pointer, ' +
    'so you do not have to look away from the word you are on.' + LineEnding + LineEnding +
    'DEFAULT DICTIONARIES' + LineEnding +
    'Tools > Default dictionaries... lets you choose four dictionaries that are used ' +
    'when a word has no dictionary of its own: Strong''s Greek, Strong''s Hebrew, ' +
    'morphology, and a word dictionary. English words are looked up by their stem, so ' +
    '"walked" and "walking" find "walk".' + LineEnding + LineEnding +
    'Compressed theWord dictionaries (.twm made with compression) are read as they are.',
    // 3
    'THE CONTROL STRIP' + LineEnding + LineEnding +
    'A thin strip sits over the top edge of each verse panel. It stays in place when ' +
    'the verse text scrolls.' + LineEnding + LineEnding +
    '  - Status circles (Destination panel): one circle for each status of the project, ' +
    'in the colour of the status. The ringed circle is the status of the current verse. ' +
    'Click a circle to set that status. It does the same as the status list in the header ' +
    'and the keys 1, 2, 3...' + LineEnding +
    '  - < and > arrows: they appear while the pointer is over the panel. They go to the ' +
    'previous or the next verse.' + LineEnding +
    '  - Edit: opens the verse in the visual editor, as double-clicking the verse does.' + LineEnding +
    '  - Properties: opens the project settings on the tab of that text.' + LineEnding + LineEnding +
    'The name of the translation is shown at the bottom right of each panel.',
    // 4
    'PARAGRAPH KEYS IN THE EDITOR' + LineEnding + LineEnding +
    '  - Enter: a tight paragraph break (<CI>).' + LineEnding +
    '  - Enter twice: a spaced paragraph break (<CM>).' + LineEnding +
    '  - Shift+Enter: a new line in the same paragraph (<CL>).' + LineEnding + LineEnding +
    'ITALICS ON SAVE' + LineEnding +
    'After saving, the words that are not linked to the original are shown in italics ' +
    'in the preview.' + LineEnding + LineEnding +
    'SAVING WHEN A VERSE IS FINISHED' + LineEnding +
    'In Preferences you can ask for the project to be saved when you choose the last ' +
    'status in the list (for example "Associated"). Needs-review is kept smart: when the ' +
    'text of a verse is reloaded, a verse whose words are all linked goes back to the ' +
    'needs-review status.',
    // 5
    'READING WORDS ALOUD' + LineEnding + LineEnding +
    'Ctrl+T turns the reader on and off. While it is on, the word under the pointer is ' +
    'read aloud, in Greek, Hebrew or English depending on the text. Tools > Voice ' +
    'settings... chooses the voice, speed and volume for each language.',
    // 6
    'SKINS' + LineEnding + LineEnding +
    'View > Skins offers seven looks: Classic, theWord Dark, Night, Sepia, Slate, ' +
    'Solarized Dark and High Contrast Dark. Ctrl+Shift+D switches between theWord Dark ' +
    'and Classic.' + LineEnding + LineEnding +
    'A skin covers the main window, the menu bar, the verse panels, the context panel ' +
    'and every other window, which is skinned the first time it opens. Classic keeps ' +
    'the standard colours. Your choice is remembered.',
    // 7
    'IMPORT' + LineEnding + LineEnding +
    'New project and Project settings read theWord modules (.ont, .ot, .nt) and also ' +
    'e-Sword (.bblx, .bbli, .bbl), USFM (.usfm, .sfm) and USFX (.usfx, .xml) Bibles. ' +
    'For USFM, choose one book and the other books in the same folder are read too.' + LineEnding + LineEnding +
    'APOCRYPHA' + LineEnding +
    'Deuterocanonical books found in the file are added after the canonical books ' +
    '(book numbers 67 and up). They appear in the Bible tree, the context panel, Find ' +
    'in project, F8 stepping and the Timotheos sync. Projects made earlier from an ' +
    'e-Sword, USFM or USFX file get their Apocrypha the first time they are opened.' + LineEnding + LineEnding +
    'EXPORT' + LineEnding +
    '  - Right-click any Bible panel > Export as Bible module...: writes the whole Bible ' +
    'of that panel in the format it was imported from (the list starts with that format). ' +
    'The Destination panel is written with its links as tagged text.' + LineEnding +
    '  - Right-click > Save text to file...: plain text, one verse per line.' + LineEnding +
    '  - Project > Export: theWord, MySword, e-Sword, USFM and USFX, for the tagged ' +
    'Destination and for the interlinear.' + LineEnding +
    'Apocrypha books go to e-Sword, USFM and USFX files only: theWord and MySword ' +
    'Bible modules cannot hold them.',
    // 8
    'STATUS FILTER' + LineEnding + LineEnding +
    'In the status list under the Bible tree, untick the statuses you do not want. ' +
    'F8 goes to the next verse that has one of the ticked statuses, Shift+F8 to the ' +
    'previous one. The choice is remembered for the next session.' + LineEnding + LineEnding +
    'FIND IN PROJECT' + LineEnding +
    'Ctrl+F searches the Source, the Destination or the reference texts of the whole ' +
    'project, Apocrypha included, and goes to the verse you choose.',
    // 9
    'THEWORD' + LineEnding + LineEnding +
    'iBiblia and theWord follow each other''s verse, in both directions. This works in ' +
    'the 64-bit build too, and a verse that is sent is not echoed back.' + LineEnding + LineEnding +
    'TIMOTHEOS (TANGLAW)' + LineEnding +
    'Tools > Synchronization with Timotheos (Tanglaw) sends the verse you are on to ' +
    'Timotheos, and follows it when you move there. The books are matched by name, so ' +
    'the Apocrypha books work too.',
    // 10
    'VISUAL WYSIWYG & RAW ONT EDITOR (1.1.0.0)' + LineEnding + LineEnding +
    'Write and format verses visually with titles, added words (<FI>...<Fi>), Words of ' +
    'Jesus (<FR>...<Fr>), Old Testament quotations (<FO>...<Fo>), underline, strikethrough, ' +
    'superscript, subscript, custom font colors, and relative font sizes, or switch to ' +
    'Raw mode to edit theWord ONT markup directly.' + LineEnding + LineEnding +
    'RICH TRANSLATOR NOTES' + LineEnding +
    'Add and edit rich translator notes (<RF>...<Rf>) with automatic or custom markers ' +
    'and instant hover previews without leaving the current verse.' + LineEnding + LineEnding +
    'ASSOCIATION PRESERVATION' + LineEnding +
    'Unaffected word associations are preserved automatically when you edit verse text, ' +
    'with an impact review prompt before any associated words are removed.',
    // 11
    'BIDIRECTIONAL RENDERING & UNICODE SELECTION (1.1.0.0)' + LineEnding + LineEnding +
    'Work seamlessly with mixed Hebrew (RTL), Greek (LTR), and translated text using ' +
    'DirectWrite bidirectional rendering and accurate Unicode word selection.' + LineEnding + LineEnding +
    'TRANSLATION MEMORY' + LineEnding +
    'Compare past translation choices in the Context Panel''s Memory tab, sort by verse ' +
    'order or rarest-first frequency, open exact highlighted occurrences, and preview ' +
    'verses on hover without leaving the current verse.',
    // 12
    'GUIDED WIZARD, CUSTOM STATUSES & SAVE AS (1.1.0.0)' + LineEnding + LineEnding +
    '  - Create Association or Translation projects through a guided wizard, including ' +
    'an initially empty Destination text for new translations.' + LineEnding +
    '  - Customize verse statuses (names, colors, and display order), use the contextual ' +
    'guide banner, and save independent .bib copies with File > Save As.' + LineEnding +
    '  - Choose English or Portuguese interface language, 4 icon families (Connected, ' +
    'Clareza, Mosaico, Folio), and 3 toolbar icon sizes (Small 16px, Medium 22px, Large 28px).' + LineEnding + LineEnding +
    'EXPERIMENTAL AI TRANSLATION & ASSOCIATION' + LineEnding +
    'Configure machine-default or per-project AI settings, flow-specific prompts, and ' +
    'reusable verse examples, with batch review before accepted results are applied and ' +
    'Windows local secure storage for API credentials.');

var
  GuideForm: TForm = nil;
  GuideList: TListBox;
  GuideText: TMemo;

procedure ShowTopic(Index: Integer);
begin
  if (Index < 0) or (Index >= TOPIC_COUNT) then
    Exit;
  GuideText.Lines.Text := TOPIC_TEXTS[Index];
end;

type
  TGuideHandlers = class
    procedure TopicChanged(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure GuideClosed(Sender: TObject; var CloseAction: TCloseAction);
  end;

var
  Handlers: TGuideHandlers = nil;

procedure TGuideHandlers.TopicChanged(Sender: TObject);
begin
  ShowTopic(GuideList.ItemIndex);
end;

procedure TGuideHandlers.CloseClick(Sender: TObject);
begin
  GuideForm.Close;
end;

procedure TGuideHandlers.GuideClosed(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  GuideForm := nil;
end;

procedure ShowNewFeaturesGuide(AOwner: TComponent);
var
  Index: Integer;
  Buttons: TPanel;
  CloseButton: TButton;
  Splitter: TSplitter;
begin
  if Assigned(GuideForm) then
  begin
    GuideForm.Show;
    GuideForm.BringToFront;
    Exit;
  end;
  if Handlers = nil then
    Handlers := TGuideHandlers.Create;

  GuideForm := TForm.CreateNew(AOwner);
  GuideForm.Caption := SNewFeaturesGuideTitle;
  GuideForm.Position := poOwnerFormCenter;
  GuideForm.BorderIcons := [biSystemMenu, biMaximize];
  GuideForm.SetBounds(0, 0, UIScale(GuideForm, 900), UIScale(GuideForm, 560));
  GuideForm.OnClose := @Handlers.GuideClosed;

  Buttons := TPanel.Create(GuideForm);
  Buttons.Parent := GuideForm;
  Buttons.Align := alBottom;
  Buttons.Height := UIScale(GuideForm, 44);
  Buttons.BevelOuter := bvNone;

  CloseButton := TButton.Create(GuideForm);
  CloseButton.Parent := Buttons;
  CloseButton.Caption := SNewFeaturesGuideClose;
  CloseButton.Anchors := [akRight, akTop];
  CloseButton.SetBounds(Buttons.Width - UIScale(GuideForm, 100),
    UIScale(GuideForm, 8), UIScale(GuideForm, 88), UIScale(GuideForm, 28));
  CloseButton.OnClick := @Handlers.CloseClick;

  GuideList := TListBox.Create(GuideForm);
  GuideList.Parent := GuideForm;
  GuideList.Align := alLeft;
  GuideList.Width := UIScale(GuideForm, 290);
  GuideList.OnSelectionChange := nil;
  for Index := 0 to TOPIC_COUNT - 1 do
    GuideList.Items.Add(TOPIC_TITLES[Index]);

  Splitter := TSplitter.Create(GuideForm);
  Splitter.Parent := GuideForm;
  Splitter.Align := alLeft;
  Splitter.Left := GuideList.Width + 1;

  GuideText := TMemo.Create(GuideForm);
  GuideText.Parent := GuideForm;
  GuideText.Align := alClient;
  GuideText.ReadOnly := True;
  GuideText.ScrollBars := ssVertical;
  GuideText.WordWrap := True;
  GuideText.BorderSpacing.Around := UIScale(GuideForm, 6);
  GuideText.Font.Size := GuideText.Font.Size + 1;

  ApplyDialogStyle(GuideForm);

  GuideList.ItemIndex := 0;
  GuideList.OnClick := @Handlers.TopicChanged;
  ShowTopic(0);
  GuideForm.Show;
end;

finalization
  Handlers.Free;

end.
