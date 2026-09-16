unit theword_translator_note_editor;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  ui_style, Classes, SysUtils, Forms, Controls, Graphics, Types, KMemo,
  theword_ont_codec_v2;

type
  TTranslatorNoteEditorAction = (tneaCancel, tneaSave, tneaDelete);
{$IFDEF FPCUNIT}
  TTranslatorNoteDpiFontProbe = record
    PlainBody: UnicodeString;
    PositiveBody: UnicodeString;
    NegativeBody: UnicodeString;
    ButtonBody: UnicodeString;
    PartialBody: UnicodeString;
    MovedCaretBody: UnicodeString;
    CaretBody: UnicodeString;
    InheritedCaretBody: UnicodeString;
    ZoomBody: UnicodeString;
  end;
  TTranslatorNoteEditorBreakProbe = record
    VisualBody: UnicodeString;
    RawBody: UnicodeString;
    RawInteriorNewlineBody: UnicodeString;
    BodyOnlyRawBody: UnicodeString;
    TerminalSoftBreakBody: UnicodeString;
    BeforeLastCharacterBody: UnicodeString;
  end;
{$ENDIF}
  TTranslatorNoteApplyEvent = function(Sender: TObject;
    AAction: TTranslatorNoteEditorAction; const AQ,
    ABodyRaw: UnicodeString; out AErrorMessage: string): Boolean of object;

  TTranslatorNotePreviewWindow = class(TForm)
  private
    FBodyRaw: UnicodeString;
    FBodyRendered: Boolean;
    FMemo: TKMemo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure ShowNote(const ABodyRaw: UnicodeString;
      const AAnchorScreenRect: TRect);
    procedure HidePreview;
  end;

function ExecuteTranslatorNoteEditor(AOwner: TComponent;
  const AAnchorScreenRect: TRect; AExisting: Boolean;
  ACapability: TTheWordLayerCapability; const AInitialQ,
  AInitialBodyRaw: UnicodeString;
  AOnApply: TTranslatorNoteApplyEvent):
  TTranslatorNoteEditorAction;

function ExecuteTheWordBodyEditor(AOwner: TComponent;
  const AAnchorScreenRect: TRect; const ATitle: string;
  const AInitialBodyRaw: UnicodeString;
  out AEditedBodyRaw: UnicodeString): Boolean;

function IsTranslatorNoteVisualEditingEnabled(
  ACapability: TTheWordLayerCapability): Boolean;
function IsAllowedTranslatorNoteHref(const AValue: UnicodeString): Boolean;
function TranslatorNoteBodyHasEffectiveContent(
  const ABodyRaw: UnicodeString): Boolean;
function TryApplyTranslatorNoteEditorDraft(
  ASender: TObject; AOnApply: TTranslatorNoteApplyEvent;
  AAction: TTranslatorNoteEditorAction; AAutomaticMarker: Boolean;
  const AQ, ABodyRaw: UnicodeString; out AErrorMessage: string): Boolean;
function TryRoundTripTranslatorNoteVisualBody(
  const ABodyRaw: UnicodeString; out ACanonical: UnicodeString;
  out AErrorMessage: string): Boolean;
{$IFDEF FPCUNIT}
{ White-box UI drivers are available only to the FPCUnit targets. }
function TryExerciseTranslatorNoteEditorInitialFocus(
  out AErrorMessage: string): Boolean;
function TryExerciseTranslatorNoteEditorTyping(
  const AInput: UnicodeString; out ABodyRaw: UnicodeString;
  out ACaretPosition: Integer; out AErrorMessage: string): Boolean;
function TryExerciseTranslatorNoteEditorNamedColor(
  const ABodyRaw: UnicodeString; AEditStart, AEditLength: Integer;
  const AEditText: UnicodeString;
  out ANoOpBodyRaw, AEditedBodyRaw: UnicodeString;
  out AErrorMessage: string): Boolean;
function TryExerciseTranslatorNoteEditorDpiFontDrift(
  const APlainText: UnicodeString;
  out AProbe: TTranslatorNoteDpiFontProbe;
  out AErrorMessage: string): Boolean;
function TryExerciseTranslatorNoteEditorBreakEditing(
  out AProbe: TTranslatorNoteEditorBreakProbe;
  out AErrorMessage: string): Boolean;
function TryExerciseTheWordBodyEditorMode(
  const AInitialBodyRaw: UnicodeString;
  out ASerializedBody: UnicodeString;
  out AMarkerControlsVisible, ADeleteVisible, AVisualEditing: Boolean;
  out AErrorMessage: string): Boolean;
function TryExerciseTheWordBodyEditorSave(
  const AInitialBodyRaw: UnicodeString;
  out ASavedBody: UnicodeString; out AVisualEditing: Boolean;
  out AErrorMessage: string): Boolean;
{$ENDIF}

implementation

uses
  Buttons, ComCtrls, Dialogs, ExtCtrls, LCLType, Math, Spin, StdCtrls,
  ColorBox, LazUTF8, SynEdit, KEditCommon, theword_syntax_highlighter,
  theword_translator_note_body_v2,
  theword_translator_note_preview, performance_debug;

resourcestring
  STranslatorNoteTitle = 'Translator note';
  SAutomaticMarker = 'Automatic marker';
  SCustomMarker = 'Custom:';
  SVisualTab = 'Visual';
  SRawTab = 'Raw';
  SSaveNote = 'Save';
  SCancelNote = 'Cancel';
  SDeleteNote = 'Delete';
  SDeleteNoteConfirm = 'Delete this translator note?';
  SDeleteNoteConfirmTitle = 'Translator note';
  SProtectedNoteBody =
    'This note contains protected or unknown markup and can only be edited in Raw mode.';
  SLiteralLessThan =
    'A literal "<" cannot be saved safely in Visual mode. Use Raw mode.';
  SLinkPrompt = 'Link (http://, https:// or tw://):';
  SLinkTitle = 'Translator note link';
  SInvalidLink =
    'Links must use http://, https:// or tw:// and cannot contain spaces, quotes or angle brackets.';
  SEmptyNoteBody = 'The translator note cannot be empty.';
  SInvalidCustomMarker =
    'The custom marker cannot be empty or contain control characters, quotes or angle brackets.';
  SPhysicalLineBreaks =
    'Use paragraph or soft-break markup; physical line breaks cannot be stored in a verse.';
  SApplyNoteFailed = 'The translator note could not be applied.';
  SBoldLabel = 'B';
  SBoldHint = 'Bold';
  SItalicLabel = 'I';
  SItalicHint = 'Italic';
  SUnderlineLabel = 'U';
  SUnderlineHint = 'Underline';
  SStrikeLabel = 'S';
  SStrikeHint = 'Strikeout';
  SSuperscriptLabel = 'x²';
  SSuperscriptHint = 'Superscript';
  SSubscriptLabel = 'x₂';
  SSubscriptHint = 'Subscript';
  SLinkLabel = '🔗';
  SInlineLtrLabel = 'L→';
  SInlineLtrHint = 'Inline left-to-right';
  SInlineRtlLabel = 'R←';
  SInlineRtlHint = 'Inline right-to-left';
  SParagraphLtrLabel = 'L¶';
  SParagraphLtrHint = 'Paragraph left-to-right';
  SParagraphRtlLabel = 'R¶';
  SParagraphRtlHint = 'Paragraph right-to-left';
  SRelativeFontSizeHint = 'Relative font size';
  STextColorHint = 'Text color';
  SBackgroundColorHint = 'Background color';

type
  TNoteStyleState = record
    FontName: UnicodeString;
    FontSize: Integer;
    HasRelativeFontSize: Boolean;
    RelativeFontSize: Integer;
    ForegroundColor: TColor;
    BackgroundColor: TColor;
    HasBackground: Boolean;
    FontStyle: TFontStyles;
    ScriptPosition: TKMemoScriptPosition;
    Href: UnicodeString;
    InlineDirection: TNoteDirection;
    ParagraphDirection: TNoteDirection;
  end;
  TNoteStyleStateArray = array of TNoteStyleState;

  TTranslatorNoteEditorMemo = class(TKMemo)
  private
    FAdvanceAfterNextInput: Boolean;
    FHasInsertionRelativeFontSize: Boolean;
    FInsertionRelativeFontSize: Integer;
    function EnsureTranslatorRun(ABlock: TKMemoBlock): TTranslatorNoteRun;
    procedure SetRunRelativeFontSize(ARun: TTranslatorNoteRun;
      AValue: Integer);
  protected
    procedure SelectionInit(ASelStart: TKMemoSelectionIndex;
      ADoScroll: Boolean = True;
      APosition: TKMemoLinePosition = eolInside); overload; override;
    procedure SelectionInit(const APoint: TPoint;
      ADoScroll: Boolean = True); overload; override;
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
  public
    procedure ApplyRelativeFontSizeToSelection(AValue: Integer);
    procedure ExpectInputAfterParagraph;
    procedure MoveCaretToDocumentEnd;
    procedure ResetRelativeFontInsertion;
{$IFDEF FPCUNIT}
    procedure SimulatePhysicalReturn;
    procedure SimulateReturnKey(Shift: TShiftState);
    procedure SimulateTextInput(const AText: UnicodeString);
{$ENDIF}
  end;

  TTranslatorNoteRawEditor = class(TSynEdit)
  protected
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
  public
{$IFDEF FPCUNIT}
    procedure SimulatePhysicalReturn;
{$ENDIF}
  end;

  TTranslatorNoteEditorForm = class(TForm)
  private
    FAutoMarker: TCheckBox;
    FBackgroundColor: TColorBox;
    FBoldButton: TSpeedButton;
    FCancelButton: TButton;
    FCustomMarker: TEdit;
    FDeleteButton: TButton;
    FExisting: Boolean;
    FBodyOnly: Boolean;
    FHasVisualSource: Boolean;
    FVisualSourceRaw: UnicodeString;
    FOnApply: TTranslatorNoteApplyEvent;
    FFontColor: TColorBox;
    FFontName: TComboBox;
    FFontSize: TSpinEdit;
    FItalicButton: TSpeedButton;
    FLinkButton: TSpeedButton;
    FLtrButton: TSpeedButton;
    FLtrParagraphButton: TSpeedButton;
    FMemo: TKMemo;
    FPageControl: TPageControl;
    FParagraphLabel: TLabel;
    FRawEditor: TSynEdit;
    FRawPage: TTabSheet;
    FRtlButton: TSpeedButton;
    FRtlParagraphButton: TSpeedButton;
    FSaveButton: TButton;
    FStrikeButton: TSpeedButton;
    FSubscriptButton: TSpeedButton;
    FSuperscriptButton: TSpeedButton;
    FToolbar: TPanel;
    FUnderlineButton: TSpeedButton;
    FUpdatingPage: Boolean;
    FVisualPage: TTabSheet;
    procedure ApplyBackground(Sender: TObject);
    procedure ApplyFontColor(Sender: TObject);
    procedure ApplyFontName(Sender: TObject);
    procedure ApplyFontSize(Sender: TObject);
    procedure ApplyLink(Sender: TObject);
    procedure ApplyMetadata(AInline, AParagraph: TNoteDirection;
      const AHref: UnicodeString = '');
    procedure ApplyStyleFlag(AStyle: TFontStyle);
    procedure ApplyScript(AScript: TKMemoScriptPosition);
    procedure AutoMarkerChanged(Sender: TObject);
    procedure BoldClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FontButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FocusBodyEditor;
    procedure InlineLtrClick(Sender: TObject);
    procedure InlineRtlClick(Sender: TObject);
    procedure InsertParagraphBreak;
    procedure InsertSoftBreak;
    procedure ItalicClick(Sender: TObject);
    procedure LinkClick(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PageChanged(Sender: TObject);
    procedure ParagraphLtrClick(Sender: TObject);
    procedure ParagraphRtlClick(Sender: TObject);
    procedure RawKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SaveClick(Sender: TObject);
    procedure StrikeClick(Sender: TObject);
    procedure SubscriptClick(Sender: TObject);
    procedure SuperscriptClick(Sender: TObject);
    procedure UnderlineClick(Sender: TObject);
    procedure SetVisualFormattingEnabled(AEnabled: Boolean);
    function RawBodyText: UnicodeString;
    function ResolvedVisualBodyRaw: UnicodeString;
    function SerializeVisual: UnicodeString;
    function SyncRawToVisual(AShowError: Boolean): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure LoadNote(AExisting: Boolean;
      ACapability: TTheWordLayerCapability; const AQ,
      ABodyRaw: UnicodeString);
    procedure LoadBody(ACapability: TTheWordLayerCapability;
      const ABodyRaw: UnicodeString);
    function MarkerQ: UnicodeString;
    function NoteBodyRaw: UnicodeString;
    property OnApply: TTranslatorNoteApplyEvent read FOnApply write FOnApply;
  end;

function TryHtmlColor(const AValue: UnicodeString;
  out AColor: TColor): Boolean;
var
  Color: LongInt;
begin
  AColor := clNone;
  Result := TryTheWordTranslatorNoteHtmlColor(AValue, Color);
  if Result then
    AColor := TColor(Color);
end;

function HtmlColor(AColor: TColor): UnicodeString;
var
  RGB: LongInt;
begin
  RGB := ColorToRGB(AColor);
  Result := UnicodeString(Format('#%.2x%.2x%.2x',
    [RGB and $FF, (RGB shr 8) and $FF, (RGB shr 16) and $FF]));
end;

function IsAllowedTranslatorNoteHref(const AValue: UnicodeString): Boolean;
var
  Index, PrefixLength: Integer;
  LowerValue: string;
begin
  LowerValue := LowerCase(UTF8Encode(AValue));
  if Pos('http://', LowerValue) = 1 then
    PrefixLength := 7
  else if Pos('https://', LowerValue) = 1 then
    PrefixLength := 8
  else if Pos('tw://', LowerValue) = 1 then
    PrefixLength := 5
  else
    Exit(False);
  if Length(AValue) <= PrefixLength then Exit(False);
  for Index := 1 to Length(AValue) do
    if (Ord(AValue[Index]) <= 32) or (AValue[Index] = '"') or
       (AValue[Index] = '''') or (AValue[Index] = '<') or
       (AValue[Index] = '>') then Exit(False);
  Result := True;
end;

function IsValidCustomMarker(const AValue: UnicodeString): Boolean;
var
  Index: Integer;
begin
  Result := AValue <> '';
  if not Result then Exit;
  for Index := 1 to Length(AValue) do
    if (Ord(AValue[Index]) < 32) or (AValue[Index] = '"') or
       (AValue[Index] = '''') or (AValue[Index] = '<') or
       (AValue[Index] = '>') then Exit(False);
  Result := True;
end;

function TranslatorNoteBodyHasEffectiveContent(
  const ABodyRaw: UnicodeString): Boolean;
var
  Body: TTheWordTranslatorNoteBodyDocument;
  Index: Integer;
  OpaqueText: UnicodeString;
begin
  Result := False;
  if Trim(ABodyRaw) = '' then Exit;
  Body := TTheWordTranslatorNoteBodyCodec.Decode(ABodyRaw);
  try
    for Index := 0 to Body.TokenCount - 1 do
      if ((Body.Tokens[Index].Kind = twnbtkText) and
          (Trim(Body.Tokens[Index].Text) <> '')) or
         (Body.Tokens[Index].Kind = twnbtkOpaque) then
      begin
        if Body.Tokens[Index].Kind = twnbtkText then Exit(True);
        OpaqueText := Trim(Body.Tokens[Index].RawLexeme);
        { Protected formatting wrappers alone are not note content. }
        if (OpaqueText <> '') and not
           ((OpaqueText[1] = '<') and
            (OpaqueText[Length(OpaqueText)] = '>')) then
          Exit(True);
      end;
  finally
    Body.Free;
  end;
end;

function IsTranslatorNoteVisualEditingEnabled(
  ACapability: TTheWordLayerCapability): Boolean;
begin
  Result := ACapability = twlcFullWysiwyg;
end;

function TryApplyTranslatorNoteEditorDraft(
  ASender: TObject; AOnApply: TTranslatorNoteApplyEvent;
  AAction: TTranslatorNoteEditorAction; AAutomaticMarker: Boolean;
  const AQ, ABodyRaw: UnicodeString; out AErrorMessage: string): Boolean;
begin
  Result := False;
  AErrorMessage := '';
  if AAction = tneaCancel then Exit;
  try
    if AAction = tneaSave then
    begin
      if (Pos(#13, ABodyRaw) > 0) or (Pos(#10, ABodyRaw) > 0) or
         (Pos(#13, AQ) > 0) or (Pos(#10, AQ) > 0) then
        raise Exception.Create(SPhysicalLineBreaks);
      if not TranslatorNoteBodyHasEffectiveContent(ABodyRaw) then
        raise Exception.Create(SEmptyNoteBody);
      if (not AAutomaticMarker) and not IsValidCustomMarker(AQ) then
        raise Exception.Create(SInvalidCustomMarker);
    end;
    if not Assigned(AOnApply) then
    begin
      AErrorMessage := SApplyNoteFailed;
      Exit;
    end;
    Result := AOnApply(ASender, AAction, AQ, ABodyRaw, AErrorMessage);
    if not Result and (AErrorMessage = '') then
      AErrorMessage := SApplyNoteFailed;
  except
    on E: Exception do
    begin
      AErrorMessage := E.Message;
      Result := False;
    end;
  end;
end;

procedure ApplyStateToRun(ARun: TTranslatorNoteRun;
  const AState: TNoteStyleState);
begin
  ARun.TextStyle.Font.Name := UTF8Encode(AState.FontName);
  ARun.TextStyle.Font.Size := Max(1, AState.FontSize);
  ARun.HasRelativeFontSize := AState.HasRelativeFontSize;
  ARun.RelativeFontSize := AState.RelativeFontSize;
  ARun.TextStyle.Font.Color := AState.ForegroundColor;
  ARun.TextStyle.Font.Style := AState.FontStyle;
  ARun.TextStyle.ScriptPosition := AState.ScriptPosition;
  ARun.TextStyle.AllowBrush := AState.HasBackground;
  if AState.HasBackground then
  begin
    ARun.TextStyle.Brush.Style := bsSolid;
    ARun.TextStyle.Brush.Color := AState.BackgroundColor;
  end;
  ARun.Href := AState.Href;
  ARun.InlineDirection := AState.InlineDirection;
  ARun.ParagraphDirection := AState.ParagraphDirection;
end;

procedure LoadBodyIntoMemo(AMemo: TKMemo; const ABodyRaw: UnicodeString;
  out ACapability: TTheWordNoteBodyCapability); overload;
var
  Body: TTheWordTranslatorNoteBodyDocument;
  Current, Saved: TNoteStyleState;
  Index, SizeValue, StackTop: Integer;
  Run: TTranslatorNoteRun;
  Stack: TNoteStyleStateArray;
  Token: TTheWordNoteBodyToken;
  ParsedColor: TColor;
begin
  Body := TTheWordTranslatorNoteBodyCodec.Decode(ABodyRaw);
  try
    ACapability := Body.Capability;
    if ACapability <> twnbcFullWysiwyg then
      Exit;
    Current.FontName := UTF8Decode(AMemo.TextStyle.Font.Name);
    Current.FontSize := AMemo.TextStyle.Font.Size;
    Current.HasRelativeFontSize := False;
    Current.RelativeFontSize := 0;
    Current.ForegroundColor := AMemo.TextStyle.Font.Color;
    Current.BackgroundColor := clNone;
    Current.HasBackground := False;
    Current.FontStyle := [];
    Current.ScriptPosition := tpoNormal;
    Current.Href := '';
    Current.InlineDirection := ndNone;
    Current.ParagraphDirection := ndNone;
    SetLength(Stack, 0);
    AMemo.Blocks.Clear;
    for Index := 0 to Body.TokenCount - 1 do
    begin
      Token := Body.Tokens[Index];
      case Token.Kind of
        twnbtkText:
          begin
            Run := TTranslatorNoteRun.Create;
            Run.Text := UTF8Encode(Token.Text);
            ApplyStateToRun(Run, Current);
            AMemo.Blocks.AddAt(Run);
          end;
        twnbtkParagraphBreak:
          AMemo.Blocks.AddParagraph;
        twnbtkLineBreak:
          AMemo.Blocks.AddAt(TTranslatorNoteSoftBreak.Create);
        twnbtkOpaque:
          begin
            Run := TTranslatorNoteRun.Create;
            Run.Text := UTF8Encode(Token.RawLexeme);
            Saved := Current;
            Saved.FontName := 'Consolas';
            Saved.ForegroundColor := clGrayText;
            ApplyStateToRun(Run, Saved);
            AMemo.Blocks.AddAt(Run);
          end;
        twnbtkTagOpen:
          begin
            StackTop := Length(Stack);
            SetLength(Stack, StackTop + 1);
            Stack[StackTop] := Current;
            case Token.TagKind of
              twnbBold: Include(Current.FontStyle, fsBold);
              twnbItalic: Include(Current.FontStyle, fsItalic);
              twnbUnderline: Include(Current.FontStyle, fsUnderline);
              twnbStrike: Include(Current.FontStyle, fsStrikeOut);
              twnbSuperscript: Current.ScriptPosition := tpoSuperscript;
              twnbSubscript: Current.ScriptPosition := tpoSubscript;
              twnbFont:
                begin
                  if Token.FontName <> '' then
                    Current.FontName := Token.FontName;
                  if Token.FontSize <> '' then
                  begin
                    if TryStrToInt(UTF8Encode(Token.FontSize), SizeValue) then
                    begin
                      if Token.FontSize[1] in ['+', '-'] then
                      begin
                        Current.HasRelativeFontSize := True;
                        Current.RelativeFontSize := SizeValue;
                        Current.FontSize := Max(1,
                          AMemo.TextStyle.Font.Size + 2 * SizeValue);
                      end
                      else
                      begin
                        Current.HasRelativeFontSize := False;
                        Current.RelativeFontSize := 0;
                        Current.FontSize := Max(1, SizeValue);
                      end;
                    end;
                  end;
                  if TryHtmlColor(Token.ForegroundColor, ParsedColor) then
                    Current.ForegroundColor := ParsedColor;
                end;
              twnbSpanBackground:
                if TryHtmlColor(Token.BackgroundColor, ParsedColor) then
                begin
                  Current.HasBackground := True;
                  Current.BackgroundColor := ParsedColor;
                end;
              twnbLink: Current.Href := Token.Href;
              twnbLtr: Current.InlineDirection := ndLtr;
              twnbRtl: Current.InlineDirection := ndRtl;
              twnbLtrParagraph: Current.ParagraphDirection := ndLtr;
              twnbRtlParagraph: Current.ParagraphDirection := ndRtl;
            end;
          end;
        twnbtkTagClose:
          if Length(Stack) > 0 then
          begin
            Current := Stack[High(Stack)];
            SetLength(Stack, Length(Stack) - 1);
          end;
      end;
    end;
    ApplyTranslatorNoteParagraphDirections(AMemo);
    AMemo.Select(0, 0);
    AMemo.Modified := False;
  finally
    Body.Free;
  end;
end;

procedure LoadBodyIntoMemo(AMemo: TKMemo;
  const ABodyRaw: UnicodeString); overload;
var
  Capability: TTheWordNoteBodyCapability;
begin
  LoadBodyIntoMemo(AMemo, ABodyRaw, Capability);
end;

procedure AddStyledText(ADocument: TTheWordTranslatorNoteBodyDocument;
  ABlock: TKMemoTextBlock; ADefaultStyle: TKMemoTextStyle);
var
  CloseKinds: array of TTheWordNoteBodyTagKind;
  CloseCount, Index: Integer;
  HasRelativeFontSize: Boolean;
  Run: TTranslatorNoteRun;
  Token: TTheWordNoteBodyToken;
  Text: UnicodeString;

  procedure Open(AKind: TTheWordNoteBodyTagKind);
  begin
    ADocument.AddOpenTag(AKind);
    SetLength(CloseKinds, CloseCount + 1);
    CloseKinds[CloseCount] := AKind;
    Inc(CloseCount);
  end;

begin
  Text := UTF8Decode(ABlock.Text);
  if Pos('<', Text) > 0 then
    raise ETheWordOntCodec.Create(SLiteralLessThan);
  Run := nil;
  if ABlock is TTranslatorNoteRun then
    Run := TTranslatorNoteRun(ABlock);
  CloseCount := 0;
  if Assigned(Run) then
  begin
    if Run.ParagraphDirection = ndLtr then Open(twnbLtrParagraph)
    else if Run.ParagraphDirection = ndRtl then Open(twnbRtlParagraph);
    if Run.InlineDirection = ndLtr then Open(twnbLtr)
    else if Run.InlineDirection = ndRtl then Open(twnbRtl);
    if Run.Href <> '' then
    begin
      if not IsAllowedTranslatorNoteHref(Run.Href) then
        raise ETheWordOntCodec.Create(SInvalidLink);
      Open(twnbLink);
      ADocument.Tokens[ADocument.TokenCount - 1].Href := Run.Href;
    end;
  end;
  HasRelativeFontSize := Assigned(Run) and Run.HasRelativeFontSize;
  if (ABlock.TextStyle.Font.Name <> ADefaultStyle.Font.Name) or
     HasRelativeFontSize or
     (ColorToRGB(ABlock.TextStyle.Font.Color) <>
      ColorToRGB(ADefaultStyle.Font.Color)) then
  begin
    Open(twnbFont);
    Token := ADocument.Tokens[ADocument.TokenCount - 1];
    if ABlock.TextStyle.Font.Name <> ADefaultStyle.Font.Name then
      Token.FontName := UTF8Decode(ABlock.TextStyle.Font.Name);
    if HasRelativeFontSize then
    begin
      if Run.RelativeFontSize >= 0 then
        Token.FontSize := UnicodeString('+' + IntToStr(Run.RelativeFontSize))
      else
        Token.FontSize := UnicodeString(IntToStr(Run.RelativeFontSize));
    end;
    if ColorToRGB(ABlock.TextStyle.Font.Color) <>
       ColorToRGB(ADefaultStyle.Font.Color) then
      Token.ForegroundColor := HtmlColor(ABlock.TextStyle.Font.Color);
  end;
  if ABlock.TextStyle.AllowBrush and
     (ABlock.TextStyle.Brush.Style <> bsClear) and
     (ABlock.TextStyle.Brush.Color <> clNone) then
  begin
    Open(twnbSpanBackground);
    ADocument.Tokens[ADocument.TokenCount - 1].BackgroundColor :=
      HtmlColor(ABlock.TextStyle.Brush.Color);
  end;
  if fsBold in ABlock.TextStyle.Font.Style then Open(twnbBold);
  if fsItalic in ABlock.TextStyle.Font.Style then Open(twnbItalic);
  if fsUnderline in ABlock.TextStyle.Font.Style then Open(twnbUnderline);
  if fsStrikeOut in ABlock.TextStyle.Font.Style then Open(twnbStrike);
  case ABlock.TextStyle.ScriptPosition of
    tpoSuperscript: Open(twnbSuperscript);
    tpoSubscript: Open(twnbSubscript);
  end;
  ADocument.AddText(Text);
  for Index := CloseCount - 1 downto 0 do
    ADocument.AddCloseTag(CloseKinds[Index]);
end;

function SerializeMemo(AMemo: TKMemo): UnicodeString;
var
  Block: TKMemoBlock;
  Body: TTheWordTranslatorNoteBodyDocument;
  Index: Integer;
begin
  Body := TTheWordTranslatorNoteBodyDocument.Create;
  try
    Body.Clear;
    for Index := 0 to AMemo.Blocks.Count - 1 do
    begin
      Block := AMemo.Blocks[Index];
      if Block is TTranslatorNoteSoftBreak then
        Body.AddLineBreak
      else if Block is TKMemoParagraph then
        Body.AddParagraphBreak
      else if Block is TKMemoTextBlock then
        AddStyledText(Body, TKMemoTextBlock(Block), AMemo.TextStyle);
    end;
    Result := TTheWordTranslatorNoteBodyCodec.Encode(Body);
  finally
    Body.Free;
  end;
end;

function TryRoundTripTranslatorNoteVisualBody(
  const ABodyRaw: UnicodeString; out ACanonical: UnicodeString;
  out AErrorMessage: string): Boolean;
var
  Memo: TKMemo;
begin
  Result := False;
  ACanonical := '';
  AErrorMessage := '';
  Memo := TKMemo.Create(nil);
  try
    LoadBodyIntoMemo(Memo, ABodyRaw);
    ACanonical := SerializeMemo(Memo);
    Result := True;
  except
    on E: Exception do AErrorMessage := E.Message;
  end;
  Memo.Free;
end;

constructor TTranslatorNotePreviewWindow.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 0);
  ApplyFormStyle(Self);
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  ShowInTaskBar := stNever;
  Color := UISurfaceColor;
  ClientWidth := 440;
  ClientHeight := 160;
  FMemo := theword_translator_note_preview.TTranslatorNoteBodyView.Create(
    Self);
  FMemo.ParentFont := False;
  FMemo.Parent := Self;
  FMemo.BorderSpacing.Around := 8;
  FMemo.Align := alClient;
  FMemo.ReadOnly := True;
  FMemo.BorderStyle := bsSingle;
  FMemo.Background.Color := UIContentColor;
  FMemo.ScrollBars := ssAutoVertical;
end;

procedure TTranslatorNotePreviewWindow.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
end;

procedure TTranslatorNoteEditorForm.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
end;

procedure TTranslatorNotePreviewWindow.ShowNote(
  const ABodyRaw: UnicodeString; const AAnchorScreenRect: TRect);
var
  TargetMonitor: TMonitor;
  Work: TRect;
begin
  if not FBodyRendered or (FBodyRaw <> ABodyRaw) then
  begin
    IBibliaPerfIncrement(ipcNotePreviewRenders);
    theword_translator_note_preview.TTranslatorNoteBodyView(FMemo).
      SetNoteBody(ABodyRaw);
    FBodyRaw := ABodyRaw;
    FBodyRendered := True;
  end;
  TargetMonitor := Screen.MonitorFromRect(AAnchorScreenRect);
  if Assigned(TargetMonitor) then
    Work := TargetMonitor.WorkareaRect
  else
    Work := Screen.WorkAreaRect;
  Left := EnsureRange(AAnchorScreenRect.Left, Work.Left,
    Max(Work.Left, Work.Right - Width));
  Top := AAnchorScreenRect.Bottom + 4;
  if Top + Height > Work.Bottom then
    Top := Max(Work.Top, AAnchorScreenRect.Top - Height - 4);
  Show;
  BringToFront;
end;

procedure TTranslatorNotePreviewWindow.HidePreview;
begin
  Hide;
end;

constructor TTranslatorNoteEditorForm.Create(AOwner: TComponent);
var
  BottomPanel: TPanel;
  MarkerLabel: TLabel;
  X: Integer;

  function NewFormatButton(const ACaption, AHint: string;
    AHandler: TNotifyEvent): TSpeedButton;
  begin
    Result := TSpeedButton.Create(Self);
    Result.Parent := FToolbar;
    Result.Caption := ACaption;
    Result.Hint := AHint;
    Result.ShowHint := True;
    Result.Flat := True;
    Result.OnClick := AHandler;
    Result.SetBounds(X, 56, 32, 32);
    Inc(X, 40);
  end;

begin
  inherited CreateNew(AOwner, 0);
  ApplyFormStyle(Self);
  Caption := STranslatorNoteTitle;
  BorderStyle := bsSizeable;
  Position := poDesigned;
  ClientWidth := 920;
  ClientHeight := 560;
  Constraints.MinWidth := 920;
  Constraints.MinHeight := 440;
  KeyPreview := True;
  OnKeyDown := @FormKeyDown;
  OnShow := @FormShow;

  FToolbar := TPanel.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align := alTop;
  FToolbar.Height := 104;
  FToolbar.BevelOuter := bvNone;
  FToolbar.ParentColor := True;

  FAutoMarker := TCheckBox.Create(Self);
  FAutoMarker.Parent := FToolbar;
  FAutoMarker.Caption := SAutomaticMarker;
  FAutoMarker.SetBounds(16, 16, 200, 24);
  FAutoMarker.OnChange := @AutoMarkerChanged;
  MarkerLabel := TLabel.Create(Self);
  MarkerLabel.Parent := FToolbar;
  MarkerLabel.Caption := SCustomMarker;
  MarkerLabel.SetBounds(232, 18, 128, 24);
  FCustomMarker := TEdit.Create(Self);
  FCustomMarker.Parent := FToolbar;
  FCustomMarker.SetBounds(376, 16, 144, 28);

  X := 16;
  FBoldButton := NewFormatButton(SBoldLabel, SBoldHint, @BoldClick);
  FBoldButton.Font.Style := [fsBold];
  FItalicButton := NewFormatButton(SItalicLabel, SItalicHint, @ItalicClick);
  FItalicButton.Font.Style := [fsItalic];
  FUnderlineButton := NewFormatButton(SUnderlineLabel, SUnderlineHint,
    @UnderlineClick);
  FUnderlineButton.Font.Style := [fsUnderline];
  FStrikeButton := NewFormatButton(SStrikeLabel, SStrikeHint, @StrikeClick);
  FStrikeButton.Font.Style := [fsStrikeOut];
  FSuperscriptButton := NewFormatButton(SSuperscriptLabel,
    SSuperscriptHint,
    @SuperscriptClick);
  FSubscriptButton := NewFormatButton(SSubscriptLabel, SSubscriptHint,
    @SubscriptClick);
  FLinkButton := NewFormatButton(SLinkLabel, SLinkTitle, @LinkClick);
  FLtrButton := NewFormatButton(SInlineLtrLabel, SInlineLtrHint,
    @InlineLtrClick);
  FRtlButton := NewFormatButton(SInlineRtlLabel, SInlineRtlHint,
    @InlineRtlClick);
  FLtrParagraphButton := NewFormatButton(SParagraphLtrLabel,
    SParagraphLtrHint, @ParagraphLtrClick);
  FRtlParagraphButton := NewFormatButton(SParagraphRtlLabel,
    SParagraphRtlHint, @ParagraphRtlClick);

  FFontName := TComboBox.Create(Self);
  FFontName.Parent := FToolbar;
  FFontName.Items.Assign(Screen.Fonts);
  FFontName.Style := csDropDownList;
  FFontName.SetBounds(X, 58, 144, 28);
  FFontName.OnChange := @FontButtonClick;
  Inc(X, 152);
  FFontSize := TSpinEdit.Create(Self);
  FFontSize.Parent := FToolbar;
  FFontSize.MinValue := -4;
  FFontSize.MaxValue := 10;
  FFontSize.Value := 0;
  FFontSize.Hint := SRelativeFontSizeHint;
  FFontSize.ShowHint := True;
  FFontSize.SetBounds(X, 58, 56, 28);
  FFontSize.OnChange := @ApplyFontSize;
  Inc(X, 64);
  FFontColor := TColorBox.Create(Self);
  FFontColor.Parent := FToolbar;
  FFontColor.Style := [cbStandardColors, cbExtendedColors, cbCustomColor];
  FFontColor.Selected := clWindowText;
  FFontColor.Hint := STextColorHint;
  FFontColor.ShowHint := True;
  FFontColor.SetBounds(X, 58, 96, 28);
  FFontColor.OnChange := @ApplyFontColor;
  Inc(X, 104);
  FBackgroundColor := TColorBox.Create(Self);
  FBackgroundColor.Parent := FToolbar;
  FBackgroundColor.Style := [cbStandardColors, cbExtendedColors,
    cbCustomColor, cbIncludeNone];
  FBackgroundColor.Selected := clNone;
  FBackgroundColor.Hint := SBackgroundColorHint;
  FBackgroundColor.ShowHint := True;
  FBackgroundColor.SetBounds(X, 58, 96, 28);
  FBackgroundColor.OnChange := @ApplyBackground;

  BottomPanel := TPanel.Create(Self);
  BottomPanel.Parent := Self;
  BottomPanel.Align := alBottom;
  BottomPanel.Height := 64;
  BottomPanel.BevelOuter := bvNone;
  FDeleteButton := TButton.Create(Self);
  FDeleteButton.Parent := BottomPanel;
  FDeleteButton.Caption := SDeleteNote;
  FDeleteButton.SetBounds(16, 16, 104, 32);
  FDeleteButton.OnClick := @DeleteClick;
  FCancelButton := TButton.Create(Self);
  FCancelButton.Parent := BottomPanel;
  FCancelButton.Caption := SCancelNote;
  FCancelButton.Anchors := [akTop, akRight];
  FCancelButton.SetBounds(BottomPanel.ClientWidth - 232, 16, 104, 32);
  FCancelButton.Cancel := True;
  FCancelButton.OnClick := @CancelClick;
  FSaveButton := TButton.Create(Self);
  FSaveButton.Parent := BottomPanel;
  FSaveButton.Caption := SSaveNote;
  FSaveButton.Default := True;
  FSaveButton.Anchors := [akTop, akRight];
  FSaveButton.SetBounds(BottomPanel.ClientWidth - 120, 16, 104, 32);
  FSaveButton.OnClick := @SaveClick;

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align := alClient;
  FPageControl.BorderSpacing.Left := 16;
  FPageControl.BorderSpacing.Right := 16;
  FPageControl.OnChange := @PageChanged;
  FVisualPage := TTabSheet.Create(Self);
  FVisualPage.PageControl := FPageControl;
  FVisualPage.Caption := SVisualTab;
  FRawPage := TTabSheet.Create(Self);
  FRawPage.PageControl := FPageControl;
  FRawPage.Caption := SRawTab;

  FMemo := TTranslatorNoteEditorMemo.Create(Self);
  FMemo.TextStyle.AllowBrush := False;
  FMemo.TextStyle.Brush.Style := bsClear;
  FMemo.ParentFont := False;
  FMemo.Parent := FVisualPage;
  FMemo.Align := alClient;
  FMemo.ScrollBars := ssAutoVertical;
  FMemo.OnKeyDown := @MemoKeyDown;
  FMemo.OnMouseWheel := @EditorMouseWheel;
  FRawEditor := TTranslatorNoteRawEditor.Create(Self);
  FRawEditor.Parent := FRawPage;
  FRawEditor.Align := alClient;
  ConfigureTheWordSyntaxEditor(FRawEditor, False);
  FRawEditor.OnKeyDown := @RawKeyDown;
  FRawEditor.OnMouseWheel := @EditorMouseWheel;

  FParagraphLabel := MarkerLabel;
end;

procedure TTranslatorNoteEditorForm.LoadNote(AExisting: Boolean;
  ACapability: TTheWordLayerCapability; const AQ,
  ABodyRaw: UnicodeString);
begin
  FBodyOnly := False;
  FExisting := AExisting;
  FDeleteButton.Visible := FExisting;
  FAutoMarker.Visible := True;
  FParagraphLabel.Visible := True;
  FCustomMarker.Visible := True;
  FAutoMarker.Checked := AQ = '';
  FCustomMarker.Text := UTF8Encode(AQ);
  AutoMarkerChanged(nil);
  FVisualSourceRaw := ABodyRaw;
  FHasVisualSource := False;
  FRawEditor.Text := UTF8Encode(ABodyRaw);
  FRawEditor.Modified := False;
  TTranslatorNoteEditorMemo(FMemo).ResetRelativeFontInsertion;
  IBibliaPerfIncrement(ipcNoteEditorDecodes);
  LoadBodyIntoMemo(FMemo, ABodyRaw);
  FVisualPage.TabVisible := IsTranslatorNoteVisualEditingEnabled(ACapability);
  if FVisualPage.TabVisible then
    FHasVisualSource := True;
  SetVisualFormattingEnabled(FVisualPage.TabVisible);
  if FVisualPage.TabVisible then
  begin
    FPageControl.ActivePage := FVisualPage;
    ActiveControl := FMemo;
  end
  else
  begin
    FPageControl.ActivePage := FRawPage;
    ActiveControl := FRawEditor;
  end;
end;

procedure TTranslatorNoteEditorForm.LoadBody(
  ACapability: TTheWordLayerCapability; const ABodyRaw: UnicodeString);
var
  Index: Integer;
begin
  LoadNote(False, ACapability, '', ABodyRaw);
  FBodyOnly := True;
  FDeleteButton.Visible := False;
  FAutoMarker.Visible := False;
  FParagraphLabel.Visible := False;
  FCustomMarker.Visible := False;
  for Index := 0 to FToolbar.ControlCount - 1 do
    if (FToolbar.Controls[Index] <> FAutoMarker) and
       (FToolbar.Controls[Index] <> FParagraphLabel) and
       (FToolbar.Controls[Index] <> FCustomMarker) and
       (FToolbar.Controls[Index].Top >= UIScale(Self, 56)) then
      FToolbar.Controls[Index].Top := FToolbar.Controls[Index].Top - UIScale(Self, 48);
  FToolbar.Height := UIScale(Self, 56);
end;

procedure TTranslatorNoteEditorForm.FocusBodyEditor;
begin
  if (FPageControl.ActivePage = FVisualPage) and FVisualPage.TabVisible and
     FMemo.Enabled and FMemo.CanFocus then
    FMemo.SetFocus;
end;

procedure TTranslatorNoteEditorForm.FormShow(Sender: TObject);
begin
  if FPageControl.ActivePage = FVisualPage then
    FocusBodyEditor
  else if FRawEditor.Enabled and FRawEditor.CanFocus then
    FRawEditor.SetFocus;
end;

procedure TTranslatorNoteEditorForm.SetVisualFormattingEnabled(
  AEnabled: Boolean);
begin
  FMemo.ReadOnly := not AEnabled;
  FBoldButton.Enabled := AEnabled;
  FItalicButton.Enabled := AEnabled;
  FUnderlineButton.Enabled := AEnabled;
  FStrikeButton.Enabled := AEnabled;
  FSuperscriptButton.Enabled := AEnabled;
  FSubscriptButton.Enabled := AEnabled;
  FLinkButton.Enabled := AEnabled;
  FLtrButton.Enabled := AEnabled;
  FRtlButton.Enabled := AEnabled;
  FLtrParagraphButton.Enabled := AEnabled;
  FRtlParagraphButton.Enabled := AEnabled;
  FFontName.Enabled := AEnabled;
  FFontSize.Enabled := AEnabled;
  FFontColor.Enabled := AEnabled;
  FBackgroundColor.Enabled := AEnabled;
end;

function TTranslatorNoteEditorForm.MarkerQ: UnicodeString;
begin
  if FAutoMarker.Checked then
    Result := ''
  else
    Result := UTF8Decode(FCustomMarker.Text);
end;

function TTranslatorNoteEditorForm.SerializeVisual: UnicodeString;
begin
  IBibliaPerfIncrement(ipcNoteEditorEncodes);
  Result := SerializeMemo(FMemo);
end;

function TTranslatorNoteEditorForm.ResolvedVisualBodyRaw: UnicodeString;
begin
  if FHasVisualSource and not FMemo.Modified then
    Exit(FVisualSourceRaw);
  Result := SerializeVisual;
  FVisualSourceRaw := Result;
  FHasVisualSource := True;
  FMemo.Modified := False;
end;

function TTranslatorNoteEditorForm.NoteBodyRaw: UnicodeString;
begin
  if FPageControl.ActivePage = FVisualPage then
    Result := ResolvedVisualBodyRaw
  else
    Result := RawBodyText;
end;

function TTranslatorNoteEditorForm.RawBodyText: UnicodeString;
var
  Index: Integer;
  Normalized: UnicodeString;
begin
  Result := UTF8Decode(FRawEditor.Text);
  { TSynEdit exposes one final platform line ending that belongs to the
    control rather than to the note. Keep any earlier line ending so the
    validation gate can still reject physical line breaks pasted by a user. }
  if (Length(Result) >= 2) and
     (Copy(Result, Length(Result) - 1, 2) = #13#10) then
    SetLength(Result, Length(Result) - 2)
  else if (Length(Result) >= 1) and
          (Result[Length(Result)] in [#10, #13]) then
    SetLength(Result, Length(Result) - 1);
  if FBodyOnly then
  begin
    Normalized := '';
    Index := 1;
    while Index <= Length(Result) do
    begin
      if Result[Index] = #13 then
      begin
        if (Index < Length(Result)) and (Result[Index + 1] = #10) then
          Inc(Index);
        Normalized := Normalized + '<p>';
      end
      else if Result[Index] = #10 then
        Normalized := Normalized + '<p>'
      else
        Normalized := Normalized + Result[Index];
      Inc(Index);
    end;
    Result := Normalized;
  end;
end;

function TTranslatorNoteEditorForm.SyncRawToVisual(
  AShowError: Boolean): Boolean;
var
  Capability: TTheWordNoteBodyCapability;
  Raw: UnicodeString;
begin
  Result := False;
  Raw := RawBodyText;
  TTranslatorNoteEditorMemo(FMemo).ResetRelativeFontInsertion;
  IBibliaPerfIncrement(ipcNoteEditorDecodes);
  LoadBodyIntoMemo(FMemo, Raw, Capability);
  if Capability <> twnbcFullWysiwyg then
  begin
    if AShowError then
      MessageDlg(STranslatorNoteTitle, SProtectedNoteBody,
        mtInformation, [mbOK], 0);
    Exit;
  end;
  FVisualSourceRaw := Raw;
  FHasVisualSource := True;
  Result := True;
end;

procedure TTranslatorNoteEditorForm.PageChanged(Sender: TObject);
begin
  if FUpdatingPage then Exit;
  FUpdatingPage := True;
  try
    if FPageControl.ActivePage = FRawPage then
    begin
      FRawEditor.Text := UTF8Encode(ResolvedVisualBodyRaw);
      FRawEditor.Modified := False;
    end
    else if not SyncRawToVisual(True) then
      FPageControl.ActivePage := FRawPage;
  finally
    FUpdatingPage := False;
  end;
end;

procedure TTranslatorNoteEditorForm.AutoMarkerChanged(Sender: TObject);
begin
  FCustomMarker.Enabled := not FAutoMarker.Checked;
end;

procedure TTranslatorNoteEditorForm.ApplyStyleFlag(AStyle: TFontStyle);
var
  Style: TKMemoTextStyle;
begin
  Style := TKMemoTextStyle.Create;
  try
    Style.Assign(FMemo.SelectionTextStyle);
    if AStyle in Style.Font.Style then
      Style.Font.Style := Style.Font.Style - [AStyle]
    else
      Style.Font.Style := Style.Font.Style + [AStyle];
    FMemo.SelectionTextStyle := Style;
  finally
    Style.Free;
  end;
  FocusBodyEditor;
end;

procedure TTranslatorNoteEditorForm.ApplyScript(
  AScript: TKMemoScriptPosition);
var
  Style: TKMemoTextStyle;
begin
  Style := TKMemoTextStyle.Create;
  try
    Style.Assign(FMemo.SelectionTextStyle);
    if Style.ScriptPosition = AScript then
      Style.ScriptPosition := tpoNormal
    else
      Style.ScriptPosition := AScript;
    FMemo.SelectionTextStyle := Style;
  finally
    Style.Free;
  end;
  FocusBodyEditor;
end;

procedure TTranslatorNoteEditorForm.BoldClick(Sender: TObject);
begin ApplyStyleFlag(fsBold); end;
procedure TTranslatorNoteEditorForm.ItalicClick(Sender: TObject);
begin ApplyStyleFlag(fsItalic); end;
procedure TTranslatorNoteEditorForm.UnderlineClick(Sender: TObject);
begin ApplyStyleFlag(fsUnderline); end;
procedure TTranslatorNoteEditorForm.StrikeClick(Sender: TObject);
begin ApplyStyleFlag(fsStrikeOut); end;
procedure TTranslatorNoteEditorForm.SuperscriptClick(Sender: TObject);
begin ApplyScript(tpoSuperscript); end;
procedure TTranslatorNoteEditorForm.SubscriptClick(Sender: TObject);
begin ApplyScript(tpoSubscript); end;

procedure TTranslatorNoteEditorForm.ApplyFontName(Sender: TObject);
var
  Style: TKMemoTextStyle;
begin
  if FFontName.ItemIndex < 0 then Exit;
  Style := TKMemoTextStyle.Create;
  try
    Style.Assign(FMemo.SelectionTextStyle);
    Style.Font.Name := FFontName.Text;
    FMemo.SelectionTextStyle := Style;
  finally Style.Free; end;
end;

procedure TTranslatorNoteEditorForm.FontButtonClick(Sender: TObject);
begin ApplyFontName(Sender); end;

procedure TTranslatorNoteEditorForm.ApplyFontSize(Sender: TObject);
begin
  TTranslatorNoteEditorMemo(FMemo).
    ApplyRelativeFontSizeToSelection(FFontSize.Value);
  FocusBodyEditor;
end;

procedure TTranslatorNoteEditorForm.ApplyFontColor(Sender: TObject);
var
  Style: TKMemoTextStyle;
begin
  Style := TKMemoTextStyle.Create;
  try
    Style.Assign(FMemo.SelectionTextStyle);
    Style.Font.Color := FFontColor.Selected;
    FMemo.SelectionTextStyle := Style;
  finally Style.Free; end;
  FocusBodyEditor;
end;

procedure TTranslatorNoteEditorForm.ApplyBackground(Sender: TObject);
var
  Style: TKMemoTextStyle;
begin
  Style := TKMemoTextStyle.Create;
  try
    Style.Assign(FMemo.SelectionTextStyle);
    Style.AllowBrush := FBackgroundColor.Selected <> clNone;
    if Style.AllowBrush then
    begin
      Style.Brush.Style := bsSolid;
      Style.Brush.Color := FBackgroundColor.Selected;
    end;
    FMemo.SelectionTextStyle := Style;
  finally Style.Free; end;
  FocusBodyEditor;
end;

procedure TTranslatorNoteEditorForm.ApplyMetadata(AInline,
  AParagraph: TNoteDirection; const AHref: UnicodeString);
var
  Block: TKMemoBlock;
  EndBlock, Index, SelectionLength, Start, StartBlock: Integer;
  Replacement, Run: TTranslatorNoteRun;
begin
  SelectionLength := FMemo.RealSelLength;
  if SelectionLength <= 0 then Exit;
  Start := FMemo.RealSelStart;
  StartBlock := FMemo.SplitAt(Start);
  EndBlock := FMemo.SplitAt(Start + SelectionLength);
  for Index := StartBlock to EndBlock - 1 do
  begin
    Block := FMemo.Blocks[Index];
    if not (Block is TKMemoTextBlock) then Continue;
    if Block is TTranslatorNoteRun then
      Run := TTranslatorNoteRun(Block)
    else
    begin
      Replacement := TTranslatorNoteRun.Create;
      Replacement.Text := TKMemoTextBlock(Block).Text;
      Replacement.TextStyle.Assign(TKMemoTextBlock(Block).TextStyle);
      FMemo.Blocks.Delete(Index);
      FMemo.Blocks.AddAt(Replacement, Index);
      Run := Replacement;
    end;
    if AInline <> ndNone then
      Run.InlineDirection := AInline;
    if AParagraph <> ndNone then
      Run.ParagraphDirection := AParagraph;
    if AHref <> '' then
      Run.Href := AHref;
  end;
  ApplyTranslatorNoteParagraphDirections(FMemo);
  FMemo.Modified := True;
  FMemo.Select(Start, SelectionLength);
  FMemo.Invalidate;
  FocusBodyEditor;
end;

procedure TTranslatorNoteEditorForm.ApplyLink(Sender: TObject);
var
  Link: string;
begin
  if FMemo.RealSelLength <= 0 then Exit;
  Link := '';
  if not InputQuery(SLinkTitle, SLinkPrompt, Link) then Exit;
  if not IsAllowedTranslatorNoteHref(UTF8Decode(Link)) then
  begin
    MessageDlg(SLinkTitle, SInvalidLink, mtError, [mbOK], 0);
    Exit;
  end;
  ApplyMetadata(ndNone, ndNone, UTF8Decode(Link));
end;

procedure TTranslatorNoteEditorForm.LinkClick(Sender: TObject);
begin ApplyLink(Sender); end;
procedure TTranslatorNoteEditorForm.InlineLtrClick(Sender: TObject);
begin ApplyMetadata(ndLtr, ndNone); end;
procedure TTranslatorNoteEditorForm.InlineRtlClick(Sender: TObject);
begin ApplyMetadata(ndRtl, ndNone); end;
procedure TTranslatorNoteEditorForm.ParagraphLtrClick(Sender: TObject);
begin ApplyMetadata(ndNone, ndLtr); end;
procedure TTranslatorNoteEditorForm.ParagraphRtlClick(Sender: TObject);
begin ApplyMetadata(ndNone, ndRtl); end;

procedure TTranslatorNoteEditorForm.InsertSoftBreak;
var
  At, BlockLength, Offset, Start: Integer;
  AtDocumentEnd: Boolean;
begin
  Start := FMemo.RealSelStart;
  if FMemo.RealSelLength > 0 then
  begin
    FMemo.Blocks.ClearSelection(True);
    Start := FMemo.RealSelStart;
  end;
  AtDocumentEnd := Start >= FMemo.SelectableLength;
  At := 0;
  Offset := 0;
  while At < FMemo.Blocks.Count do
  begin
    BlockLength := FMemo.Blocks[At].SelectableLength;
    if Start <= Offset then Break;
    if Start < Offset + BlockLength then
    begin
      At := FMemo.SplitAt(Start);
      Break;
    end;
    Inc(Offset, BlockLength);
    Inc(At);
  end;
  FMemo.Blocks.AddAt(TTranslatorNoteSoftBreak.Create, At);
  FMemo.Blocks.UpdateAttributes;
  if AtDocumentEnd then
  begin
    FMemo.Blocks.AddTextBlock('');
    FMemo.Blocks.UpdateAttributes;
    TTranslatorNoteEditorMemo(FMemo).MoveCaretToDocumentEnd;
    TTranslatorNoteEditorMemo(FMemo).ExpectInputAfterParagraph;
  end
  else
    FMemo.Select(Min(Start + 1, FMemo.SelectableLength), 0);
  FMemo.Modified := True;
end;

procedure TTranslatorNoteEditorForm.InsertParagraphBreak;
var
  AtDocumentEnd: Boolean;
begin
  AtDocumentEnd := FMemo.RealSelStart + FMemo.RealSelLength >=
    FMemo.SelectableLength;
  FMemo.ExecuteCommand(ecInsertNewLine);
  { At the end of a document KMemo adds the paragraph block but deliberately
    keeps its line-position affinity before it. Move to the editor bottom so
    subsequent text belongs to the new paragraph. }
  if AtDocumentEnd then
  begin
    { KMemo does not expose a caret position after a trailing paragraph block.
      Keep an empty text block there until the user types the next character. }
    FMemo.Blocks.AddTextBlock('');
    FMemo.Blocks.UpdateAttributes;
    TTranslatorNoteEditorMemo(FMemo).MoveCaretToDocumentEnd;
    TTranslatorNoteEditorMemo(FMemo).ExpectInputAfterParagraph;
  end;
end;

procedure TTranslatorNoteEditorForm.MemoKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and
     (Shift * [ssCtrl, ssAlt] = []) then
  begin
    if ssShift in Shift then
      InsertSoftBreak
    else
      InsertParagraphBreak;
    Key := 0;
  end;
end;

procedure TTranslatorNoteEditorForm.RawKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and
     (Shift * [ssCtrl, ssAlt] = []) then
  begin
    if ssShift in Shift then
      FRawEditor.SelText := '<CL>'
    else
      FRawEditor.SelText := '<p>';
    Key := 0;
  end;
end;

procedure TTranslatorNoteEditorForm.EditorMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Block: TKMemoBlock;
  Delta, Index, NewSize, OldSize: Integer;
begin
  if not (ssCtrl in Shift) or (WheelDelta = 0) then Exit;
  if WheelDelta > 0 then Delta := 1 else Delta := -1;
  if Sender = FRawEditor then
    FRawEditor.Font.Size := EnsureRange(FRawEditor.Font.Size + Delta, 6, 72)
  else
  begin
    OldSize := FMemo.TextStyle.Font.Size;
    NewSize := EnsureRange(OldSize + Delta, 6, 72);
    Delta := NewSize - OldSize;
    if Delta <> 0 then
    begin
      FMemo.TextStyle.Font.Size := NewSize;
      for Index := 0 to FMemo.Blocks.Count - 1 do
      begin
        Block := FMemo.Blocks[Index];
        if Block is TKMemoTextBlock then
          TKMemoTextBlock(Block).TextStyle.Font.Size :=
            Max(1, TKMemoTextBlock(Block).TextStyle.Font.Size + Delta);
      end;
      FMemo.Invalidate;
    end;
  end;
  Handled := True;
end;

procedure TTranslatorNoteEditorForm.SaveClick(Sender: TObject);
var
  Body, Q: UnicodeString;
  ErrorMessage: string;
begin
  try
    if FBodyOnly then
    begin
      Body := NoteBodyRaw;
      ModalResult := mrOK;
      Exit;
    end;

    Body := NoteBodyRaw;
    Q := MarkerQ;
    ErrorMessage := '';
    if not TryApplyTranslatorNoteEditorDraft(Self, FOnApply,
      tneaSave, FAutoMarker.Checked, Q, Body, ErrorMessage) then
    begin
      MessageDlg(STranslatorNoteTitle, ErrorMessage, mtError, [mbOK], 0);
      Exit;
    end;
    ModalResult := mrOK;
  except
    on E: Exception do
      MessageDlg(STranslatorNoteTitle, E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TTranslatorNoteEditorForm.CancelClick(Sender: TObject);
begin ModalResult := mrCancel; end;

procedure TTranslatorNoteEditorForm.DeleteClick(Sender: TObject);
var
  ErrorMessage: string;
begin
  if MessageDlg(SDeleteNoteConfirmTitle, SDeleteNoteConfirm,
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ErrorMessage := '';
    if not TryApplyTranslatorNoteEditorDraft(Self, FOnApply,
      tneaDelete, True, '', '', ErrorMessage) then
    begin
      MessageDlg(STranslatorNoteTitle, ErrorMessage, mtError, [mbOK], 0);
      Exit;
    end;
    ModalResult := mrAbort;
  end;
end;

procedure TTranslatorNoteEditorForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_Z) then
  begin
    if FPageControl.ActivePage = FRawPage then
    begin
      if ssShift in Shift then FRawEditor.Redo else FRawEditor.Undo;
    end
    else if ssShift in Shift then
      FMemo.ExecuteCommand(ecRedo)
    else
      FMemo.ExecuteCommand(ecUndo);
    Key := 0;
  end
  else if (ssCtrl in Shift) and (Key = VK_Y) then
  begin
    if FPageControl.ActivePage = FRawPage then
      FRawEditor.Redo
    else
      FMemo.ExecuteCommand(ecRedo);
    Key := 0;
  end
  else if (Key = VK_ESCAPE) then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end
  else if ((Key = VK_S) and (ssCtrl in Shift)) or
          ((Key = VK_RETURN) and (ssCtrl in Shift)) then
  begin
    SaveClick(nil);
    Key := 0;
  end;
end;

{$IFDEF FPCUNIT}
function TryExerciseTranslatorNoteEditorInitialFocus(
  out AErrorMessage: string): Boolean;
var
  Form: TTranslatorNoteEditorForm;
begin
  Result := False;
  AErrorMessage := '';
  Form := TTranslatorNoteEditorForm.Create(nil);
  try
    try
      Form.LoadNote(False, twlcFullWysiwyg, '', '');
      if Form.ActiveControl <> Form.FMemo then
      begin
        AErrorMessage := 'The visual note body was not selected as the initial control.';
        Exit;
      end;
      { This deliberately runs before the form is shown. A formatting command
        must not force focus into a control whose native window is not ready. }
      Form.BoldClick(nil);
      Result := True;
    except
      on E: Exception do AErrorMessage := E.Message;
    end;
  finally
    Form.Free;
  end;
end;
{$ENDIF}

function TTranslatorNoteEditorMemo.EnsureTranslatorRun(
  ABlock: TKMemoBlock): TTranslatorNoteRun;
var
  BlockIndex: Integer;
  Replacement: TTranslatorNoteRun;
begin
  Result := nil;
  if not (ABlock is TKMemoTextBlock) then Exit;
  if ABlock is TTranslatorNoteRun then
  begin
    Result := TTranslatorNoteRun(ABlock);
    Exit;
  end;
  BlockIndex := 0;
  while (BlockIndex < Blocks.Count) and
        (Blocks[BlockIndex] <> ABlock) do
    Inc(BlockIndex);
  if BlockIndex >= Blocks.Count then Exit;
  Replacement := TTranslatorNoteRun.Create;
  Replacement.Text := TKMemoTextBlock(ABlock).Text;
  Replacement.TextStyle.Assign(TKMemoTextBlock(ABlock).TextStyle);
  Blocks.Delete(BlockIndex);
  Blocks.AddAt(Replacement, BlockIndex);
  Result := Replacement;
end;

procedure TTranslatorNoteEditorMemo.SetRunRelativeFontSize(
  ARun: TTranslatorNoteRun; AValue: Integer);
begin
  if not Assigned(ARun) then Exit;
  ARun.HasRelativeFontSize := AValue <> 0;
  ARun.RelativeFontSize := AValue;
  ARun.TextStyle.Font.Size := Max(1, TextStyle.Font.Size + 2 * AValue);
end;

procedure TTranslatorNoteEditorMemo.ApplyRelativeFontSizeToSelection(
  AValue: Integer);
var
  EndBlock, Index, SelectionLength, Start, StartBlock: Integer;
  Run: TTranslatorNoteRun;
  Style: TKMemoTextStyle;
begin
  FHasInsertionRelativeFontSize := True;
  FInsertionRelativeFontSize := AValue;
  Start := RealSelStart;
  SelectionLength := RealSelLength;
  if SelectionLength <= 0 then
  begin
    Style := TKMemoTextStyle.Create;
    try
      Style.Assign(SelectionTextStyle);
      Style.Font.Size := Max(1, TextStyle.Font.Size + 2 * AValue);
      SelectionTextStyle := Style;
    finally
      Style.Free;
    end;
    Exit;
  end;
  StartBlock := SplitAt(Start);
  if Start + SelectionLength >= Blocks.SelectableLength then
    EndBlock := Blocks.Count
  else
    EndBlock := SplitAt(Start + SelectionLength);
  for Index := StartBlock to EndBlock - 1 do
  begin
    Run := EnsureTranslatorRun(Blocks[Index]);
    SetRunRelativeFontSize(Run, AValue);
  end;
  Blocks.UpdateAttributes;
  Modified := True;
  Select(Start, SelectionLength);
  Invalidate;
end;

procedure TTranslatorNoteEditorMemo.ResetRelativeFontInsertion;
begin
  FHasInsertionRelativeFontSize := False;
  FInsertionRelativeFontSize := 0;
end;

procedure TTranslatorNoteEditorMemo.SelectionInit(
  ASelStart: TKMemoSelectionIndex; ADoScroll: Boolean;
  APosition: TKMemoLinePosition);
begin
  ResetRelativeFontInsertion;
  inherited SelectionInit(ASelStart, ADoScroll, APosition);
end;

procedure TTranslatorNoteEditorMemo.SelectionInit(const APoint: TPoint;
  ADoScroll: Boolean);
begin
  ResetRelativeFontInsertion;
  inherited SelectionInit(APoint, ADoScroll);
end;

procedure TTranslatorNoteEditorMemo.UTF8KeyPress(var Key: TUTF8Char);
var
  AdvanceAfterParagraph, ApplyRelativeFontSize, HadInput,
    WasEmpty: Boolean;
  Block: TKMemoBlock;
  CaretBefore, CaretAfter, LocalIndex: Integer;
  RelativeFontSize: Integer;
  Run: TTranslatorNoteRun;
begin
  HadInput := (Key <> '') and (Key <> #13) and (Key <> #10);
  ApplyRelativeFontSize := FHasInsertionRelativeFontSize;
  RelativeFontSize := FInsertionRelativeFontSize;
  if (Key = #13) or (Key = #10) then
  begin
    Key := '';
    Exit;
  end;
  WasEmpty := Blocks.SelectableLength = 0;
  AdvanceAfterParagraph := FAdvanceAfterNextInput;
  FAdvanceAfterNextInput := False;
  CaretBefore := RealSelStart;
  if HadInput and not ApplyRelativeFontSize and
     (Blocks.SelectableLength > 0) then
  begin
    if CaretBefore > 0 then
      Block := Blocks.IndexToBlock(CaretBefore - 1, LocalIndex)
    else
      Block := Blocks.IndexToBlock(0, LocalIndex);
    if (Block is TTranslatorNoteRun) and
       TTranslatorNoteRun(Block).HasRelativeFontSize then
    begin
      ApplyRelativeFontSize := True;
      RelativeFontSize := TTranslatorNoteRun(Block).RelativeFontSize;
    end;
  end;
  inherited UTF8KeyPress(Key);
  if WasEmpty or AdvanceAfterParagraph then
  begin
    { KMemo can defer its selectable-length refresh after inserting into an
      empty run (the initial document and the run after a trailing paragraph).
      Without the refresh its own caret move is clamped before the new glyph. }
    Blocks.UpdateAttributes;
    if AdvanceAfterParagraph then
      Select(Min(CaretBefore + 1, Blocks.SelectableLength), 0)
    else if Blocks.SelectableLength > 0 then
      Select(Blocks.SelectableLength, 0);
  end;
  if HadInput and ApplyRelativeFontSize then
  begin
    CaretAfter := RealSelStart;
    if CaretAfter > 0 then
    begin
      Block := Blocks.IndexToBlock(CaretAfter - 1, LocalIndex);
      Run := EnsureTranslatorRun(Block);
      SetRunRelativeFontSize(Run, RelativeFontSize);
      Blocks.UpdateAttributes;
      Select(Min(CaretAfter, Blocks.SelectableLength), 0);
    end;
  end;
end;

procedure TTranslatorNoteEditorMemo.ExpectInputAfterParagraph;
begin
  FAdvanceAfterNextInput := True;
end;

{$IFDEF FPCUNIT}
procedure TTranslatorNoteEditorMemo.SimulatePhysicalReturn;
var
  Key: TUTF8Char;
begin
  Key := #13;
  UTF8KeyPress(Key);
end;

procedure TTranslatorNoteEditorMemo.SimulateReturnKey(
  Shift: TShiftState);
var
  KeyCode: Word;
  KeyChar: TUTF8Char;
begin
  KeyCode := VK_RETURN;
  KeyDown(KeyCode, Shift);
  KeyChar := #13;
  UTF8KeyPress(KeyChar);
end;

{$ENDIF}

procedure TTranslatorNoteEditorMemo.MoveCaretToDocumentEnd;
begin
  Blocks.UpdateAttributes;
  SelectionInit(Blocks.SelectableLength, True, eolEnd);
end;

{$IFDEF FPCUNIT}
procedure TTranslatorNoteEditorMemo.SimulateTextInput(
  const AText: UnicodeString);
var
  Encoded: UTF8String;
  Key: TUTF8Char;
  Index: Integer;
begin
  Encoded := UTF8Encode(AText);
  for Index := 1 to UTF8Length(Encoded) do
  begin
    Key := UTF8Copy(Encoded, Index, 1);
    UTF8KeyPress(Key);
  end;
end;
{$ENDIF}

procedure TTranslatorNoteRawEditor.UTF8KeyPress(var Key: TUTF8Char);
begin
  if (Key = #13) or (Key = #10) then
  begin
    Key := '';
    Exit;
  end;
  inherited UTF8KeyPress(Key);
end;

{$IFDEF FPCUNIT}
procedure TTranslatorNoteRawEditor.SimulatePhysicalReturn;
var
  Key: TUTF8Char;
begin
  Key := #13;
  UTF8KeyPress(Key);
end;

function TryExerciseTranslatorNoteEditorTyping(
  const AInput: UnicodeString; out ABodyRaw: UnicodeString;
  out ACaretPosition: Integer; out AErrorMessage: string): Boolean;
var
  Form: TTranslatorNoteEditorForm;
begin
  Result := False;
  ABodyRaw := '';
  ACaretPosition := -1;
  AErrorMessage := '';
  Form := TTranslatorNoteEditorForm.Create(nil);
  try
    try
      Form.LoadNote(False, twlcFullWysiwyg, '', '');
      if Form.ActiveControl <> Form.FMemo then
      begin
        AErrorMessage :=
          'The visual note body was not selected as the initial control.';
        Exit;
      end;
      if (Form.FMemo.RealSelStart <> 0) or
         (Form.FMemo.RealSelLength <> 0) then
      begin
        AErrorMessage := 'The empty visual note did not start at caret zero.';
        Exit;
      end;
      Form.Show;
      Application.ProcessMessages;
      if Form.ActiveControl <> Form.FMemo then
      begin
        AErrorMessage :=
          'The visual note body lost focus when the modal was shown.';
        Exit;
      end;
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput(AInput);
      Application.ProcessMessages;
      ABodyRaw := Form.ResolvedVisualBodyRaw;
      ACaretPosition := Form.FMemo.RealSelStart;
      Result := True;
    except
      on E: Exception do AErrorMessage := E.Message;
    end;
  finally
    Form.Hide;
    Form.Free;
  end;
end;

function TryExerciseTranslatorNoteEditorNamedColor(
  const ABodyRaw: UnicodeString; AEditStart, AEditLength: Integer;
  const AEditText: UnicodeString;
  out ANoOpBodyRaw, AEditedBodyRaw: UnicodeString;
  out AErrorMessage: string): Boolean;
var
  Form: TTranslatorNoteEditorForm;
begin
  Result := False;
  ANoOpBodyRaw := '';
  AEditedBodyRaw := '';
  AErrorMessage := '';
  Form := TTranslatorNoteEditorForm.Create(nil);
  try
    try
      Form.LoadNote(True, twlcFullWysiwyg, '', ABodyRaw);
      ANoOpBodyRaw := Form.ResolvedVisualBodyRaw;
      Form.FMemo.Select(AEditStart, AEditLength);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput(
        AEditText);
      AEditedBodyRaw := Form.ResolvedVisualBodyRaw;
      Result := True;
    except
      on E: Exception do AErrorMessage := E.Message;
    end;
  finally
    Form.Free;
  end;
end;

function TryExerciseTranslatorNoteEditorDpiFontDrift(
  const APlainText: UnicodeString;
  out AProbe: TTranslatorNoteDpiFontProbe;
  out AErrorMessage: string): Boolean;
var
  Block: TKMemoBlock;
  Form: TTranslatorNoteEditorForm;
  Handled: Boolean;
begin
  Result := False;
  AErrorMessage := '';
  AProbe.PlainBody := '';
  AProbe.PositiveBody := '';
  AProbe.NegativeBody := '';
  AProbe.ButtonBody := '';
  AProbe.PartialBody := '';
  AProbe.MovedCaretBody := '';
  AProbe.CaretBody := '';
  AProbe.InheritedCaretBody := '';
  AProbe.ZoomBody := '';
  Form := TTranslatorNoteEditorForm.Create(nil);
  try
    try
      Form.LoadNote(False, twlcFullWysiwyg, '', '');
      Form.Show;
      Application.ProcessMessages;
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput(APlainText);
      Block := Form.FMemo.Blocks[0];
      if not (Block is TKMemoTextBlock) then
        raise Exception.Create('The typed note did not create a text run');
      TKMemoTextBlock(Block).TextStyle.Font.Size :=
        Form.FMemo.TextStyle.Font.Size + 1;
      AProbe.PlainBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '',
        '<font size="+2">grande</font>');
      Block := Form.FMemo.Blocks[0];
      TKMemoTextBlock(Block).TextStyle.Font.Size :=
        TKMemoTextBlock(Block).TextStyle.Font.Size + 1;
      AProbe.PositiveBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '',
        '<font size="-1">menor</font>');
      Block := Form.FMemo.Blocks[0];
      TKMemoTextBlock(Block).TextStyle.Font.Size :=
        TKMemoTextBlock(Block).TextStyle.Font.Size - 1;
      AProbe.NegativeBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '', 'botão');
      Form.FMemo.Select(0, Form.FMemo.SelectableLength);
      Form.FFontSize.Value := 1;
      Form.ApplyFontSize(nil);
      AProbe.ButtonBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '', 'abcde');
      Form.FMemo.Select(1, 3);
      Form.FFontSize.Value := 1;
      Form.ApplyFontSize(nil);
      AProbe.PartialBody := Form.ResolvedVisualBodyRaw;

      TTranslatorNoteEditorMemo(Form.FMemo).MoveCaretToDocumentEnd;
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('z');
      AProbe.MovedCaretBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '', '');
      Form.FMemo.Select(0, 0);
      Form.FFontSize.Value := 1;
      Form.ApplyFontSize(nil);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('novo');
      AProbe.CaretBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '',
        '<font size="+1">herdado</font>');
      Form.FMemo.Select(Form.FMemo.SelectableLength, 0);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('x');
      AProbe.InheritedCaretBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '',
        '<font size="+1">zoom</font>');
      Handled := False;
      Form.EditorMouseWheel(Form.FMemo, [ssCtrl], 120,
        Point(0, 0), Handled);
      if not Handled then
        raise Exception.Create('The visual zoom command was not handled');
      AProbe.ZoomBody := Form.ResolvedVisualBodyRaw;
      Result := True;
    except
      on E: Exception do AErrorMessage := E.Message;
    end;
  finally
    Form.Hide;
    Form.Free;
  end;
end;

function TryExerciseTranslatorNoteEditorBreakEditing(
  out AProbe: TTranslatorNoteEditorBreakProbe;
  out AErrorMessage: string): Boolean;
var
  Form: TTranslatorNoteEditorForm;
  Key: Word;
begin
  Result := False;
  AErrorMessage := '';
  AProbe.VisualBody := '';
  AProbe.RawBody := '';
  AProbe.RawInteriorNewlineBody := '';
  AProbe.BodyOnlyRawBody := '';
  AProbe.TerminalSoftBreakBody := '';
  AProbe.BeforeLastCharacterBody := '';
  Form := TTranslatorNoteEditorForm.Create(nil);
  try
    try
      Form.LoadNote(False, twlcFullWysiwyg, '', '');
      Form.Show;
      Application.ProcessMessages;

      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('one');
      Key := VK_RETURN;
      Form.MemoKeyDown(Form.FMemo, Key, []);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulatePhysicalReturn;
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('two');
      Key := VK_RETURN;
      Form.MemoKeyDown(Form.FMemo, Key, [ssShift]);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulatePhysicalReturn;
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('three');
      Key := VK_RETURN;
      Form.MemoKeyDown(Form.FMemo, Key, []);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulatePhysicalReturn;
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('four');
      AProbe.VisualBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '', 'one');
      Form.FMemo.Select(Form.FMemo.SelectableLength, 0);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateReturnKey([ssShift]);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('two');
      AProbe.TerminalSoftBreakBody := Form.ResolvedVisualBodyRaw;

      Form.LoadNote(False, twlcFullWysiwyg, '', 'oneX');
      Form.FMemo.Select(3, 0);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateReturnKey([]);
      TTranslatorNoteEditorMemo(Form.FMemo).SimulateTextInput('two');
      AProbe.BeforeLastCharacterBody := Form.ResolvedVisualBodyRaw;

      Form.FRawEditor.Text := 'raw';
      Form.FRawEditor.SelStart := Length(Form.FRawEditor.Text);
      Key := VK_RETURN;
      Form.RawKeyDown(Form.FRawEditor, Key, []);
      TTranslatorNoteRawEditor(Form.FRawEditor).SimulatePhysicalReturn;
      Key := VK_RETURN;
      Form.RawKeyDown(Form.FRawEditor, Key, [ssShift]);
      TTranslatorNoteRawEditor(Form.FRawEditor).SimulatePhysicalReturn;
      AProbe.RawBody := Form.RawBodyText;

      Form.FRawEditor.Text := 'alpha' + LineEnding + 'beta';
      AProbe.RawInteriorNewlineBody := Form.RawBodyText;

      Form.LoadBody(twlcRawOnly, '<CM>opaque<CI>');
      Form.FRawEditor.Text := 'raw-only<CM>opaque<CI>';
      Form.FRawEditor.SelStart := Length(Form.FRawEditor.Text);
      Key := VK_RETURN;
      Form.RawKeyDown(Form.FRawEditor, Key, []);
      TTranslatorNoteRawEditor(Form.FRawEditor).SimulatePhysicalReturn;
      Key := VK_RETURN;
      Form.RawKeyDown(Form.FRawEditor, Key, [ssShift]);
      TTranslatorNoteRawEditor(Form.FRawEditor).SimulatePhysicalReturn;
      AProbe.BodyOnlyRawBody := Form.RawBodyText;
      Result := True;
    except
      on E: Exception do AErrorMessage := E.Message;
    end;
  finally
    Form.Hide;
    Form.Free;
  end;
end;
{$ENDIF}

{$IFDEF FPCUNIT}
function TryExerciseTheWordBodyEditorMode(
  const AInitialBodyRaw: UnicodeString;
  out ASerializedBody: UnicodeString;
  out AMarkerControlsVisible, ADeleteVisible, AVisualEditing: Boolean;
  out AErrorMessage: string): Boolean;
var
  Form: TTranslatorNoteEditorForm;
begin
  Result := False;
  ASerializedBody := '';
  AMarkerControlsVisible := True;
  ADeleteVisible := True;
  AVisualEditing := False;
  AErrorMessage := '';
  Form := TTranslatorNoteEditorForm.Create(nil);
  try
    try
      Form.LoadBody(AnalyzeTheWordTranslatorNoteBody(AInitialBodyRaw),
        AInitialBodyRaw);
      AMarkerControlsVisible := Form.FAutoMarker.Visible or
        Form.FParagraphLabel.Visible or Form.FCustomMarker.Visible;
      ADeleteVisible := Form.FDeleteButton.Visible;
      AVisualEditing := Form.FPageControl.ActivePage = Form.FVisualPage;
      if AVisualEditing then
      begin
        TTranslatorNoteEditorMemo(Form.FMemo).MoveCaretToDocumentEnd;
        TTranslatorNoteEditorMemo(Form.FMemo).SimulateReturnKey([]);
        TTranslatorNoteEditorMemo(Form.FMemo).SimulateReturnKey([ssShift]);
        ASerializedBody := Form.ResolvedVisualBodyRaw;
      end
      else
        ASerializedBody := Form.RawBodyText;
      Result := True;
    except
      on E: Exception do AErrorMessage := E.Message;
    end;
  finally
    Form.Hide;
    Form.Free;
  end;
end;

function TryExerciseTheWordBodyEditorSave(
  const AInitialBodyRaw: UnicodeString;
  out ASavedBody: UnicodeString; out AVisualEditing: Boolean;
  out AErrorMessage: string): Boolean;
var
  Form: TTranslatorNoteEditorForm;
begin
  Result := False;
  ASavedBody := '';
  AVisualEditing := False;
  AErrorMessage := '';
  Form := TTranslatorNoteEditorForm.Create(nil);
  try
    try
      Form.LoadBody(AnalyzeTheWordTranslatorNoteBody(AInitialBodyRaw),
        AInitialBodyRaw);
      AVisualEditing := Form.FPageControl.ActivePage = Form.FVisualPage;
      Form.SaveClick(nil);
      Result := Form.ModalResult = mrOK;
      if Result then
        ASavedBody := Form.NoteBodyRaw;
    except
      on E: Exception do AErrorMessage := E.Message;
    end;
  finally
    Form.Hide;
    Form.Free;
  end;
end;
{$ENDIF}

function ExecuteTranslatorNoteEditor(AOwner: TComponent;
  const AAnchorScreenRect: TRect; AExisting: Boolean;
  ACapability: TTheWordLayerCapability; const AInitialQ,
  AInitialBodyRaw: UnicodeString;
  AOnApply: TTranslatorNoteApplyEvent):
  TTranslatorNoteEditorAction;
var
  Form: TTranslatorNoteEditorForm;
  Work: TRect;
  TargetMonitor: TMonitor;
begin
  Result := tneaCancel;
  Form := TTranslatorNoteEditorForm.Create(AOwner);
  try
    TargetMonitor := Screen.MonitorFromRect(AAnchorScreenRect);
    if Assigned(TargetMonitor) then
      Work := TargetMonitor.WorkareaRect
    else
      Work := Screen.WorkAreaRect;
    Form.Left := EnsureRange(AAnchorScreenRect.Left, Work.Left,
      Max(Work.Left, Work.Right - Form.Width));
    Form.Top := AAnchorScreenRect.Bottom + 6;
    if Form.Top + Form.Height > Work.Bottom then
      Form.Top := Max(Work.Top, AAnchorScreenRect.Top - Form.Height - 6);
    Form.OnApply := AOnApply;
    Form.LoadNote(AExisting, ACapability, AInitialQ, AInitialBodyRaw);
    case Form.ShowModal of
      mrOK: Result := tneaSave;
      mrAbort: Result := tneaDelete;
    end;
  finally
    Form.Free;
  end;
end;

function ExecuteTheWordBodyEditor(AOwner: TComponent;
  const AAnchorScreenRect: TRect; const ATitle: string;
  const AInitialBodyRaw: UnicodeString;
  out AEditedBodyRaw: UnicodeString): Boolean;
var
  Form: TTranslatorNoteEditorForm;
  Work: TRect;
  TargetMonitor: TMonitor;
begin
  Result := False;
  AEditedBodyRaw := AInitialBodyRaw;
  Form := TTranslatorNoteEditorForm.Create(AOwner);
  try
    TargetMonitor := Screen.MonitorFromRect(AAnchorScreenRect);
    if Assigned(TargetMonitor) then
      Work := TargetMonitor.WorkareaRect
    else
      Work := Screen.WorkAreaRect;
    Form.Left := EnsureRange(AAnchorScreenRect.Left, Work.Left,
      Max(Work.Left, Work.Right - Form.Width));
    Form.Top := AAnchorScreenRect.Bottom + 6;
    if Form.Top + Form.Height > Work.Bottom then
      Form.Top := Max(Work.Top, AAnchorScreenRect.Top - Form.Height - 6);
    Form.Caption := ATitle;
    Form.LoadBody(AnalyzeTheWordTranslatorNoteBody(AInitialBodyRaw),
      AInitialBodyRaw);
    if Form.ShowModal = mrOK then
    begin
      AEditedBodyRaw := Form.NoteBodyRaw;
      Result := True;
    end;
  finally
    Form.Free;
  end;
end;

end.
