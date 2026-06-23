unit theword_syntax_highlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Types, Controls, Graphics, IniFiles, StdCtrls,
  SynEdit, SynEditHighlighter, SynEditMarkup, SynEditMiscClasses,
  SynEditTypes, SynEditWrappedView;

type
  TTheWordTokenKind = (
    twNull,
    twText,
    twSpace,
    twSemanticTag,
    twStrongMorphTag,
    twStrongTag,
    twMorphTag,
    twWordTokenTag,
    twNoteRefTag,
    twLinkTag,
    twHtmlTag,
    twRtfTag,
    twAttribute,
    twAttributeValue,
    twSymbol,
    twWarning
  );

  TTheWordSyntaxElement = (
    tweText,
    tweSemanticTag,
    tweStrongTag,
    tweMorphTag,
    tweWordTokenTag,
    tweNoteRefTag,
    tweLinkTag,
    tweHtmlTag,
    tweRtfTag,
    tweAttribute,
    tweAttributeValue,
    tweSymbol,
    tweWarning
  );

  TTheWordSyntaxElementStyle = record
    Foreground: TColor;
    Background: TColor;
    HasBackground: Boolean;
    FontStyle: TFontStyles;
  end;

  TTheWordSyntaxColorScheme = array[TTheWordSyntaxElement] of TTheWordSyntaxElementStyle;

  TTheWordSyntaxEditorFont = record
    Name: string;
    Size: Integer;
  end;

  TTheWordTagPairPosition = record
    Row: Integer;
    StartCol: Integer;
    EndCol: Integer;
  end;

  { TTheWordSyntaxHighlighter }

  TTheWordSyntaxHighlighter = class(TSynCustomHighlighter)
  private
    FLine: string;
    FLineLength: Integer;
    FRun: Integer;
    FTokenPos: Integer;
    FTokenKind: TTheWordTokenKind;
    FInsideTag: Boolean;
    FTagEnd: Integer;
    FTagNameRead: Boolean;
    FExpectValue: Boolean;
    FActiveTagKind: TTheWordTokenKind;
    FTextAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSemanticTagAttri: TSynHighlighterAttributes;
    FStrongMorphTagAttri: TSynHighlighterAttributes;
    FStrongTagAttri: TSynHighlighterAttributes;
    FMorphTagAttri: TSynHighlighterAttributes;
    FWordTokenTagAttri: TSynHighlighterAttributes;
    FNoteRefTagAttri: TSynHighlighterAttributes;
    FLinkTagAttri: TSynHighlighterAttributes;
    FHtmlTagAttri: TSynHighlighterAttributes;
    FRtfTagAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FAttributeValueAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FWarningAttri: TSynHighlighterAttributes;
    function AnalyzeTagAt(StartPos: Integer; out EndPos: Integer;
      out TagKind: TTheWordTokenKind): Boolean;
    function CharAt(Index: Integer): Char;
    function FindTagEnd(StartPos: Integer): Integer;
    function GetTagName(const Inner: string): string;
    function IsHtmlTagName(const AName: string): Boolean;
    function IsNoteRefTagName(const AName: string): Boolean;
    function IsRtfTagName(const AName: string): Boolean;
    function IsSemanticTagName(const AName: string): Boolean;
    function IsStrongMorphTagName(const AName: string;
      out IsMalformed: Boolean; out AKind: TTheWordTokenKind): Boolean;
    procedure ReadAttributeOrValue;
    procedure ReadTagName;
    procedure ReadText;
    procedure ReadWhitespace;
    procedure SetToken(AKind: TTheWordTokenKind; NewRun: Integer);
  protected
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    function GetEol: Boolean; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function GetTokenLen: Integer; override;
    procedure Next; override;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
  published
    property TextAttri: TSynHighlighterAttributes read FTextAttri write FTextAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property SemanticTagAttri: TSynHighlighterAttributes read FSemanticTagAttri write FSemanticTagAttri;
    property StrongMorphTagAttri: TSynHighlighterAttributes read FStrongMorphTagAttri write FStrongMorphTagAttri;
    property StrongTagAttri: TSynHighlighterAttributes read FStrongTagAttri write FStrongTagAttri;
    property MorphTagAttri: TSynHighlighterAttributes read FMorphTagAttri write FMorphTagAttri;
    property WordTokenTagAttri: TSynHighlighterAttributes read FWordTokenTagAttri write FWordTokenTagAttri;
    property NoteRefTagAttri: TSynHighlighterAttributes read FNoteRefTagAttri write FNoteRefTagAttri;
    property LinkTagAttri: TSynHighlighterAttributes read FLinkTagAttri write FLinkTagAttri;
    property HtmlTagAttri: TSynHighlighterAttributes read FHtmlTagAttri write FHtmlTagAttri;
    property RtfTagAttri: TSynHighlighterAttributes read FRtfTagAttri write FRtfTagAttri;
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri write FAttributeAttri;
    property AttributeValueAttri: TSynHighlighterAttributes read FAttributeValueAttri write FAttributeValueAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property WarningAttri: TSynHighlighterAttributes read FWarningAttri write FWarningAttri;
  end;

  { TTheWordTagPairMarkup }

  TTheWordTagPairMarkup = class(TSynEditMarkup)
  private
    FCurrentTag: TTheWordTagPairPosition;
    FMatchingTag: TTheWordTagPairPosition;
    FHasHighlight: Boolean;
    function ContainsPosition(const Position: TTheWordTagPairPosition;
      ARow, ACol: Integer): Boolean;
    procedure InvalidatePosition(const Position: TTheWordTagPairPosition);
    procedure UpdateHighlight;
  protected
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override;
  public
    constructor Create(ASynEdit: TSynEditBase);
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound;
      const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound;
      const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
      ANextLog: Integer); override;
  end;

const
  THEWORD_SYNTAX_HIGHLIGHT_SECTION = 'theword.syntax.highlight';

procedure ConfigureTheWordSyntaxEditor(Editor: TSynEdit; ReadOnly: Boolean);
procedure GetDefaultTheWordSyntaxColorScheme(out Scheme: TTheWordSyntaxColorScheme);
function GetTheWordSyntaxColorScheme: TTheWordSyntaxColorScheme;
procedure SetTheWordSyntaxColorScheme(const Scheme: TTheWordSyntaxColorScheme);
procedure ResetTheWordSyntaxColorScheme;
procedure GetDefaultTheWordSyntaxEditorFont(out EditorFont: TTheWordSyntaxEditorFont);
function GetTheWordSyntaxEditorFont: TTheWordSyntaxEditorFont;
procedure SetTheWordSyntaxEditorFont(const EditorFont: TTheWordSyntaxEditorFont);
procedure ResetTheWordSyntaxEditorFont;
procedure LoadTheWordSyntaxEditorFont(Ini: TCustomIniFile);
procedure SaveTheWordSyntaxEditorFont(Ini: TCustomIniFile);
procedure ApplyTheWordSyntaxEditorFont(Editor: TSynEdit); overload;
procedure ApplyTheWordSyntaxEditorFont(Editor: TSynEdit;
  const EditorFont: TTheWordSyntaxEditorFont); overload;
procedure ApplyTheWordSyntaxEditorFontToEditors(Root: TWinControl);
procedure LoadTheWordSyntaxColorScheme(Ini: TCustomIniFile);
procedure SaveTheWordSyntaxColorScheme(Ini: TCustomIniFile);
procedure ApplyTheWordSyntaxColorScheme(Highlighter: TTheWordSyntaxHighlighter); overload;
procedure ApplyTheWordSyntaxColorScheme(Highlighter: TTheWordSyntaxHighlighter;
  const Scheme: TTheWordSyntaxColorScheme); overload;
procedure ApplyTheWordSyntaxColorSchemeToEditors(Root: TWinControl);
function TheWordSyntaxElementId(Element: TTheWordSyntaxElement): string;
function TheWordSyntaxElementCaption(Element: TTheWordSyntaxElement): string;
function TheWordSyntaxTokenKindName(Kind: Integer): string;
function FindTheWordMatchingTagPairAt(Lines: TStrings; const Caret: TPoint;
  out CurrentTag, MatchingTag: TTheWordTagPairPosition): Boolean;

implementation

const
  STheWordSyntax = 'theWord Bible';

var
  GTheWordSyntaxColorScheme: TTheWordSyntaxColorScheme;
  GTheWordSyntaxEditorFont: TTheWordSyntaxEditorFont;

type
  TTheWordTagPairFamily = string[31];

  TTheWordRawTag = record
    Position: TTheWordTagPairPosition;
    Family: TTheWordTagPairFamily;
    IsOpening: Boolean;
  end;

  TTheWordRawTagArray = array of TTheWordRawTag;

procedure ClearTagPairPosition(out Position: TTheWordTagPairPosition);
begin
  Position.Row := 0;
  Position.StartCol := 0;
  Position.EndCol := 0;
end;

procedure ClearRawTag(out Tag: TTheWordRawTag);
begin
  ClearTagPairPosition(Tag.Position);
  Tag.Family := '';
  Tag.IsOpening := False;
end;

function MakeElementStyle(Foreground: TColor; FontStyle: TFontStyles;
  Background: TColor = clNone; HasBackground: Boolean = False
  ): TTheWordSyntaxElementStyle;
begin
  Result.Foreground := Foreground;
  Result.Background := Background;
  Result.HasBackground := HasBackground;
  Result.FontStyle := FontStyle;
end;

procedure GetDefaultTheWordSyntaxColorScheme(out Scheme: TTheWordSyntaxColorScheme);
begin
  Scheme[tweText] := MakeElementStyle(clDefault, []);
  Scheme[tweSemanticTag] := MakeElementStyle(clNavy, [fsBold]);
  Scheme[tweStrongTag] := MakeElementStyle(clBlue, [fsBold]);
  Scheme[tweMorphTag] := MakeElementStyle(clFuchsia, [fsBold]);
  Scheme[tweWordTokenTag] := MakeElementStyle(clMaroon, [fsBold]);
  Scheme[tweNoteRefTag] := MakeElementStyle(clOlive, [fsBold]);
  Scheme[tweLinkTag] := MakeElementStyle(clBlue, [fsUnderline]);
  Scheme[tweHtmlTag] := MakeElementStyle(clRed, [fsBold]);
  Scheme[tweRtfTag] := MakeElementStyle(clPurple, []);
  Scheme[tweAttribute] := MakeElementStyle(clGray, []);
  Scheme[tweAttributeValue] := MakeElementStyle(clGreen, []);
  Scheme[tweSymbol] := MakeElementStyle(clGray, []);
  Scheme[tweWarning] := MakeElementStyle(clRed, [fsBold, fsUnderline]);
end;

function GetTheWordSyntaxColorScheme: TTheWordSyntaxColorScheme;
begin
  Result := GTheWordSyntaxColorScheme;
end;

procedure SetTheWordSyntaxColorScheme(const Scheme: TTheWordSyntaxColorScheme);
begin
  GTheWordSyntaxColorScheme := Scheme;
end;

procedure ResetTheWordSyntaxColorScheme;
begin
  GetDefaultTheWordSyntaxColorScheme(GTheWordSyntaxColorScheme);
end;

procedure GetDefaultTheWordSyntaxEditorFont(out EditorFont: TTheWordSyntaxEditorFont);
begin
  EditorFont.Name := 'Courier New';
  EditorFont.Size := 10;
end;

function GetTheWordSyntaxEditorFont: TTheWordSyntaxEditorFont;
begin
  Result := GTheWordSyntaxEditorFont;
end;

procedure SetTheWordSyntaxEditorFont(const EditorFont: TTheWordSyntaxEditorFont);
begin
  if Trim(EditorFont.Name) = '' then
    GTheWordSyntaxEditorFont.Name := 'Courier New'
  else
    GTheWordSyntaxEditorFont.Name := EditorFont.Name;

  if EditorFont.Size < 1 then
    GTheWordSyntaxEditorFont.Size := 10
  else
    GTheWordSyntaxEditorFont.Size := EditorFont.Size;
end;

procedure ResetTheWordSyntaxEditorFont;
begin
  GetDefaultTheWordSyntaxEditorFont(GTheWordSyntaxEditorFont);
end;

procedure LoadTheWordSyntaxEditorFont(Ini: TCustomIniFile);
var
  Defaults: TTheWordSyntaxEditorFont;
  EditorFont: TTheWordSyntaxEditorFont;
begin
  GetDefaultTheWordSyntaxEditorFont(Defaults);
  EditorFont := Defaults;

  if Assigned(Ini) then
  begin
    EditorFont.Name := Trim(Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION,
      'editor.font.name', Defaults.Name));
    if EditorFont.Name = '' then
      EditorFont.Name := Defaults.Name;

    try
      EditorFont.Size := Ini.ReadInteger(THEWORD_SYNTAX_HIGHLIGHT_SECTION,
        'editor.font.size', Defaults.Size);
    except
      EditorFont.Size := Defaults.Size;
    end;

    if EditorFont.Size < 1 then
      EditorFont.Size := Defaults.Size;
  end;

  SetTheWordSyntaxEditorFont(EditorFont);
end;

procedure SaveTheWordSyntaxEditorFont(Ini: TCustomIniFile);
begin
  if not Assigned(Ini) then
    Exit;

  Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.name',
    GTheWordSyntaxEditorFont.Name);
  Ini.WriteInteger(THEWORD_SYNTAX_HIGHLIGHT_SECTION, 'editor.font.size',
    GTheWordSyntaxEditorFont.Size);
end;

procedure ApplyTheWordSyntaxEditorFont(Editor: TSynEdit;
  const EditorFont: TTheWordSyntaxEditorFont);
begin
  if not Assigned(Editor) then
    Exit;

  Editor.ParentFont := False;
  Editor.Font.Name := EditorFont.Name;
  Editor.Font.Size := EditorFont.Size;
  Editor.Font.Quality := fqAntialiased;
end;

procedure ApplyTheWordSyntaxEditorFont(Editor: TSynEdit);
begin
  ApplyTheWordSyntaxEditorFont(Editor, GTheWordSyntaxEditorFont);
end;

procedure ApplyTheWordSyntaxEditorFontToEditors(Root: TWinControl);
var
  Index: Integer;
begin
  if not Assigned(Root) then
    Exit;

  if Root is TSynEdit then
    ApplyTheWordSyntaxEditorFont(TSynEdit(Root));

  for Index := 0 to Root.ControlCount - 1 do
    if Root.Controls[Index] is TWinControl then
      ApplyTheWordSyntaxEditorFontToEditors(TWinControl(Root.Controls[Index]));
end;

function TheWordSyntaxElementId(Element: TTheWordSyntaxElement): string;
begin
  case Element of
    tweText:
      Result := 'text';
    tweSemanticTag:
      Result := 'theword_tag';
    tweStrongTag:
      Result := 'strong';
    tweMorphTag:
      Result := 'morphology';
    tweWordTokenTag:
      Result := 'word_token';
    tweNoteRefTag:
      Result := 'note_reference';
    tweLinkTag:
      Result := 'link';
    tweHtmlTag:
      Result := 'html';
    tweRtfTag:
      Result := 'rtf';
    tweAttribute:
      Result := 'attribute';
    tweAttributeValue:
      Result := 'attribute_value';
    tweSymbol:
      Result := 'symbol';
    tweWarning:
      Result := 'warning';
  else
    Result := 'unknown';
  end;
end;

function TheWordSyntaxElementCaption(Element: TTheWordSyntaxElement): string;
begin
  case Element of
    tweText:
      Result := 'Default text';
    tweSemanticTag:
      Result := 'theWord tag';
    tweStrongTag:
      Result := 'Strong';
    tweMorphTag:
      Result := 'Morphology';
    tweWordTokenTag:
      Result := 'Word token <wt>';
    tweNoteRefTag:
      Result := 'Note/reference';
    tweLinkTag:
      Result := 'Link';
    tweHtmlTag:
      Result := 'HTML';
    tweRtfTag:
      Result := 'RTF';
    tweAttribute:
      Result := 'Attribute';
    tweAttributeValue:
      Result := 'Attribute value';
    tweSymbol:
      Result := 'Symbol';
    tweWarning:
      Result := 'Warning';
  else
    Result := 'Unknown';
  end;
end;

function StyleToString(AStyle: TFontStyles): string;

  procedure AddToken(const Token: string);
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + Token;
  end;

begin
  Result := '';
  if fsBold in AStyle then
    AddToken('bold');
  if fsItalic in AStyle then
    AddToken('italic');
  if fsUnderline in AStyle then
    AddToken('underline');
end;

function TryStyleFromString(const Value: string; DefaultStyle: TFontStyles;
  out AStyle: TFontStyles): Boolean;
var
  Tokens: TStringList;
  Index: Integer;
  Token: string;
begin
  Result := True;
  AStyle := [];
  if Trim(Value) = '' then
    Exit;

  Tokens := TStringList.Create;
  try
    Tokens.StrictDelimiter := True;
    Tokens.Delimiter := ',';
    Tokens.DelimitedText := Value;

    for Index := 0 to Tokens.Count - 1 do
    begin
      Token := LowerCase(Trim(Tokens[Index]));
      if Token = '' then
        Continue
      else if Token = 'bold' then
        Include(AStyle, fsBold)
      else if Token = 'italic' then
        Include(AStyle, fsItalic)
      else if Token = 'underline' then
        Include(AStyle, fsUnderline)
      else
      begin
        AStyle := DefaultStyle;
        Exit(False);
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

function TryColorFromString(const Value: string; DefaultColor: TColor;
  out Color: TColor): Boolean;
begin
  Result := True;
  try
    Color := StringToColor(Value);
  except
    Color := DefaultColor;
    Result := False;
  end;
end;

function BackgroundToString(const Style: TTheWordSyntaxElementStyle): string;
begin
  if Style.HasBackground then
    Result := ColorToString(Style.Background)
  else
    Result := '';
end;

procedure LoadTheWordSyntaxColorScheme(Ini: TCustomIniFile);
var
  Defaults: TTheWordSyntaxColorScheme;
  Scheme: TTheWordSyntaxColorScheme;
  Element: TTheWordSyntaxElement;
  Id: string;
  Value: string;
  Color: TColor;
  Style: TFontStyles;
begin
  GetDefaultTheWordSyntaxColorScheme(Defaults);
  Scheme := Defaults;

  if Assigned(Ini) then
    for Element := Low(TTheWordSyntaxElement) to High(TTheWordSyntaxElement) do
    begin
      Id := TheWordSyntaxElementId(Element);

      Value := Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION,
        Id + '.foreground', ColorToString(Defaults[Element].Foreground));
      if TryColorFromString(Value, Defaults[Element].Foreground, Color) then
        Scheme[Element].Foreground := Color
      else
        Scheme[Element].Foreground := Defaults[Element].Foreground;

      Value := Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION,
        Id + '.background', BackgroundToString(Defaults[Element]));
      if Trim(Value) = '' then
      begin
        Scheme[Element].HasBackground := False;
        Scheme[Element].Background := clNone;
      end
      else if TryColorFromString(Value, Defaults[Element].Background, Color) then
      begin
        Scheme[Element].HasBackground := True;
        Scheme[Element].Background := Color;
      end
      else
      begin
        Scheme[Element].HasBackground := Defaults[Element].HasBackground;
        Scheme[Element].Background := Defaults[Element].Background;
      end;

      Value := Ini.ReadString(THEWORD_SYNTAX_HIGHLIGHT_SECTION,
        Id + '.style', StyleToString(Defaults[Element].FontStyle));
      if TryStyleFromString(Value, Defaults[Element].FontStyle, Style) then
        Scheme[Element].FontStyle := Style
      else
        Scheme[Element].FontStyle := Defaults[Element].FontStyle;
    end;

  SetTheWordSyntaxColorScheme(Scheme);
end;

procedure SaveTheWordSyntaxColorScheme(Ini: TCustomIniFile);
var
  Element: TTheWordSyntaxElement;
  Id: string;
begin
  if not Assigned(Ini) then
    Exit;

  for Element := Low(TTheWordSyntaxElement) to High(TTheWordSyntaxElement) do
  begin
    Id := TheWordSyntaxElementId(Element);
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, Id + '.foreground',
      ColorToString(GTheWordSyntaxColorScheme[Element].Foreground));
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, Id + '.background',
      BackgroundToString(GTheWordSyntaxColorScheme[Element]));
    Ini.WriteString(THEWORD_SYNTAX_HIGHLIGHT_SECTION, Id + '.style',
      StyleToString(GTheWordSyntaxColorScheme[Element].FontStyle));
  end;
end;

procedure ApplyElementStyle(Attr: TSynHighlighterAttributes;
  const Style: TTheWordSyntaxElementStyle);
begin
  if not Assigned(Attr) then
    Exit;

  Attr.Foreground := Style.Foreground;
  if Style.HasBackground then
    Attr.Background := Style.Background
  else
    Attr.Background := clNone;
  Attr.Style := Style.FontStyle;
end;

procedure ApplyTheWordSyntaxColorScheme(Highlighter: TTheWordSyntaxHighlighter;
  const Scheme: TTheWordSyntaxColorScheme);
begin
  if not Assigned(Highlighter) then
    Exit;

  ApplyElementStyle(Highlighter.TextAttri, Scheme[tweText]);
  ApplyElementStyle(Highlighter.SpaceAttri, Scheme[tweText]);
  ApplyElementStyle(Highlighter.SemanticTagAttri, Scheme[tweSemanticTag]);
  ApplyElementStyle(Highlighter.StrongMorphTagAttri, Scheme[tweStrongTag]);
  ApplyElementStyle(Highlighter.StrongTagAttri, Scheme[tweStrongTag]);
  ApplyElementStyle(Highlighter.MorphTagAttri, Scheme[tweMorphTag]);
  ApplyElementStyle(Highlighter.WordTokenTagAttri, Scheme[tweWordTokenTag]);
  ApplyElementStyle(Highlighter.NoteRefTagAttri, Scheme[tweNoteRefTag]);
  ApplyElementStyle(Highlighter.LinkTagAttri, Scheme[tweLinkTag]);
  ApplyElementStyle(Highlighter.HtmlTagAttri, Scheme[tweHtmlTag]);
  ApplyElementStyle(Highlighter.RtfTagAttri, Scheme[tweRtfTag]);
  ApplyElementStyle(Highlighter.AttributeAttri, Scheme[tweAttribute]);
  ApplyElementStyle(Highlighter.AttributeValueAttri, Scheme[tweAttributeValue]);
  ApplyElementStyle(Highlighter.SymbolAttri, Scheme[tweSymbol]);
  ApplyElementStyle(Highlighter.WarningAttri, Scheme[tweWarning]);
end;

procedure ApplyTheWordSyntaxColorScheme(Highlighter: TTheWordSyntaxHighlighter);
begin
  ApplyTheWordSyntaxColorScheme(Highlighter, GTheWordSyntaxColorScheme);
end;

procedure ApplyTheWordSyntaxColorSchemeToEditors(Root: TWinControl);
var
  Index: Integer;
  Editor: TSynEdit;
begin
  if not Assigned(Root) then
    Exit;

  if Root is TSynEdit then
  begin
    Editor := TSynEdit(Root);
    if Editor.Highlighter is TTheWordSyntaxHighlighter then
    begin
      ApplyTheWordSyntaxColorScheme(TTheWordSyntaxHighlighter(Editor.Highlighter));
      Editor.Invalidate;
    end;
  end;

  for Index := 0 to Root.ControlCount - 1 do
    if Root.Controls[Index] is TWinControl then
      ApplyTheWordSyntaxColorSchemeToEditors(TWinControl(Root.Controls[Index]));
end;

function IsAsciiDigit(C: Char): Boolean;
begin
  Result := C in ['0'..'9'];
end;

function IsTagSpace(C: Char): Boolean;
begin
  Result := C in [' ', #9, #10, #13];
end;

function IsTagStop(C: Char): Boolean;
begin
  Result := IsTagSpace(C) or (C in ['/', '>']);
end;

function IsNumberedTheWordTag(const AName, Prefix: string; MinValue,
  MaxValue: Integer): Boolean;
var
  Value: Integer;
begin
  Result := False;
  if (Length(AName) <> Length(Prefix) + 1) or
    (Copy(AName, 1, Length(Prefix)) <> Prefix) or
    not (AName[Length(AName)] in ['0'..'9']) then
    Exit;

  Value := Ord(AName[Length(AName)]) - Ord('0');
  Result := (Value >= MinValue) and (Value <= MaxValue);
end;

function IsHtmlPairTagName(const AName: string): Boolean;
var
  LowerName: string;
begin
  LowerName := LowerCase(AName);
  Result :=
    (LowerName = 'b') or (LowerName = 'i') or (LowerName = 'u') or
    (LowerName = 's') or (LowerName = 'sup') or (LowerName = 'sub') or
    (LowerName = 'font') or (LowerName = 'span') or (LowerName = 'p') or
    (LowerName = 'a') or (LowerName = 'rtl') or (LowerName = 'ltr') or
    (LowerName = 'rtf');
end;

function TryGetTheWordTagPairInfo(const AName: string; SlashClosing,
  SelfClosing: Boolean; out Family: TTheWordTagPairFamily;
  out IsOpening: Boolean): Boolean;
begin
  Result := False;
  Family := '';
  IsOpening := False;

  if SelfClosing or (AName = '') then
    Exit;

  if SlashClosing then
  begin
    if IsHtmlPairTagName(AName) then
    begin
      Family := 'html:' + LowerCase(AName);
      IsOpening := False;
      Exit(True);
    end;
    Exit;
  end;

  if IsHtmlPairTagName(AName) then
  begin
    Family := 'html:' + LowerCase(AName);
    IsOpening := True;
    Exit(True);
  end;

  if (AName = 'FI') or (AName = 'FR') or (AName = 'FO') or
    (AName = 'FU') or (AName = 'RF') or (AName = 'RX') or
    (AName = 'NB') or (AName = 'ST') then
  begin
    Family := AName;
    IsOpening := True;
    Exit(True);
  end;

  if (AName = 'Fi') or (AName = 'Fr') or (AName = 'Fo') or
    (AName = 'Fu') or (AName = 'Rf') or (AName = 'Rx') or
    (AName = 'Nb') or (AName = 'St') then
  begin
    Family := UpperCase(AName);
    IsOpening := False;
    Exit(True);
  end;

  if (AName = 'TS') or IsNumberedTheWordTag(AName, 'TS', 0, 3) then
  begin
    Family := 'TS';
    IsOpening := True;
    Exit(True);
  end;

  if AName = 'Ts' then
  begin
    Family := 'TS';
    IsOpening := False;
    Exit(True);
  end;

  if (AName = 'PI') or IsNumberedTheWordTag(AName, 'PI', 0, 7) then
  begin
    Family := 'PI';
    IsOpening := True;
    Exit(True);
  end;

  if AName = 'Pi' then
  begin
    Family := 'PI';
    IsOpening := False;
    Exit(True);
  end;
end;

function TryParsePairableTagAt(const Line: string; Row, StartCol: Integer;
  out Tag: TTheWordRawTag; out EndCol: Integer): Boolean;
var
  CloseCol: Integer;
  Inner: string;
  NameStart: Integer;
  NameEnd: Integer;
  TagName: string;
  SlashClosing: Boolean;
  SelfClosing: Boolean;
  Family: TTheWordTagPairFamily;
  IsOpening: Boolean;
begin
  Result := False;
  ClearRawTag(Tag);
  EndCol := StartCol + 1;

  if (StartCol < 1) or (StartCol > Length(Line)) or (Line[StartCol] <> '<') then
    Exit;

  CloseCol := PosEx('>', Line, StartCol + 1);
  if CloseCol <= 0 then
    Exit;

  EndCol := CloseCol + 1;
  Inner := Copy(Line, StartCol + 1, CloseCol - StartCol - 1);
  if (Inner = '') or IsTagSpace(Inner[1]) or IsTagSpace(Inner[Length(Inner)]) then
    Exit;

  SlashClosing := Inner[1] = '/';
  NameStart := 1;
  if SlashClosing then
    Inc(NameStart);

  if (NameStart > Length(Inner)) or IsTagSpace(Inner[NameStart]) then
    Exit;

  NameEnd := NameStart;
  while (NameEnd <= Length(Inner)) and not IsTagStop(Inner[NameEnd]) do
    Inc(NameEnd);

  TagName := Copy(Inner, NameStart, NameEnd - NameStart);
  if TagName = '' then
    Exit;

  SelfClosing := (Length(Inner) > 0) and (Inner[Length(Inner)] = '/');
  if not TryGetTheWordTagPairInfo(TagName, SlashClosing, SelfClosing,
    Family, IsOpening) then
    Exit;

  Tag.Position.Row := Row;
  Tag.Position.StartCol := StartCol;
  Tag.Position.EndCol := EndCol;
  Tag.Family := Family;
  Tag.IsOpening := IsOpening;
  Result := True;
end;

function ExtractPairableTagsFromLine(const Line: string; Row: Integer
  ): TTheWordRawTagArray;
var
  SearchCol: Integer;
  StartCol: Integer;
  EndCol: Integer;
  Tag: TTheWordRawTag;
begin
  Result := nil;
  ClearRawTag(Tag);
  SearchCol := 1;
  while SearchCol <= Length(Line) do
  begin
    StartCol := PosEx('<', Line, SearchCol);
    if StartCol <= 0 then
      Break;

    if TryParsePairableTagAt(Line, Row, StartCol, Tag, EndCol) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Tag;
      SearchCol := EndCol;
    end
    else if EndCol > StartCol + 1 then
      SearchCol := EndCol
    else
      SearchCol := StartCol + 1;
  end;
end;

function FindTagUnderCaret(Lines: TStrings; const Caret: TPoint;
  out Tag: TTheWordRawTag): Boolean;
var
  Tags: TTheWordRawTagArray;
  Index: Integer;
begin
  Result := False;
  ClearRawTag(Tag);

  if not Assigned(Lines) or (Caret.Y < 1) or (Caret.Y > Lines.Count) or
    (Caret.X < 1) then
    Exit;

  Tags := ExtractPairableTagsFromLine(Lines[Caret.Y - 1], Caret.Y);
  for Index := 0 to High(Tags) do
    if (Caret.X >= Tags[Index].Position.StartCol) and
      (Caret.X <= Tags[Index].Position.EndCol) then
    begin
      Tag := Tags[Index];
      Exit(True);
    end;
end;

function FindMatchingTagForward(Lines: TStrings; const StartTag: TTheWordRawTag;
  out MatchingTag: TTheWordRawTag): Boolean;
var
  Row: Integer;
  Tags: TTheWordRawTagArray;
  Index: Integer;
  Depth: Integer;
begin
  Result := False;
  ClearRawTag(MatchingTag);
  Depth := 0;

  for Row := StartTag.Position.Row to Lines.Count do
  begin
    Tags := ExtractPairableTagsFromLine(Lines[Row - 1], Row);
    for Index := 0 to High(Tags) do
    begin
      if (Row = StartTag.Position.Row) and
        (Tags[Index].Position.StartCol < StartTag.Position.EndCol) then
        Continue;
      if Tags[Index].Family <> StartTag.Family then
        Continue;

      if Tags[Index].IsOpening then
        Inc(Depth)
      else if Depth > 0 then
        Dec(Depth)
      else
      begin
        MatchingTag := Tags[Index];
        Exit(True);
      end;
    end;
  end;
end;

function FindMatchingTagBackward(Lines: TStrings; const StartTag: TTheWordRawTag;
  out MatchingTag: TTheWordRawTag): Boolean;
var
  Row: Integer;
  Tags: TTheWordRawTagArray;
  Index: Integer;
  Depth: Integer;
begin
  Result := False;
  ClearRawTag(MatchingTag);
  Depth := 0;

  for Row := StartTag.Position.Row downto 1 do
  begin
    Tags := ExtractPairableTagsFromLine(Lines[Row - 1], Row);
    for Index := High(Tags) downto 0 do
    begin
      if (Row = StartTag.Position.Row) and
        (Tags[Index].Position.StartCol >= StartTag.Position.StartCol) then
        Continue;
      if Tags[Index].Family <> StartTag.Family then
        Continue;

      if not Tags[Index].IsOpening then
        Inc(Depth)
      else if Depth > 0 then
        Dec(Depth)
      else
      begin
        MatchingTag := Tags[Index];
        Exit(True);
      end;
    end;
  end;
end;

function FindTheWordMatchingTagPairAt(Lines: TStrings; const Caret: TPoint;
  out CurrentTag, MatchingTag: TTheWordTagPairPosition): Boolean;
var
  RawCurrent: TTheWordRawTag;
  RawMatching: TTheWordRawTag;
begin
  Result := False;
  ClearTagPairPosition(CurrentTag);
  ClearTagPairPosition(MatchingTag);

  if not FindTagUnderCaret(Lines, Caret, RawCurrent) then
    Exit;

  if RawCurrent.IsOpening then
    Result := FindMatchingTagForward(Lines, RawCurrent, RawMatching)
  else
    Result := FindMatchingTagBackward(Lines, RawCurrent, RawMatching);

  if Result then
  begin
    CurrentTag := RawCurrent.Position;
    MatchingTag := RawMatching.Position;
  end;
end;

function IsKnownAttribute(const AName: string): Boolean;
begin
  Result := (AName = 'href') or (AName = 'q') or (AName = 'color') or
    (AName = 'size') or (AName = 'name') or (AName = 'style') or
    (AName = 'toggle') or (AName = 'tag') or (AName = 'l') or (AName = 'lh') or
    (AName = 'onsearch') or (AName = 'regex_off') or (AName = 'ot_only') or
    (AName = 'nt_only');
end;

procedure EnsureLineWrapPlugin(Editor: TSynEdit);
var
  Index: Integer;
begin
  for Index := 0 to Editor.PluginCount - 1 do
    if Editor.Plugin[Index] is TLazSynEditLineWrapPlugin then
      Exit;

  TLazSynEditLineWrapPlugin.Create(Editor);
end;

procedure EnsureTagPairMarkup(Editor: TSynEdit);
begin
  if not Assigned(Editor) then
    Exit;

  if not Assigned(Editor.MarkupByClass[TTheWordTagPairMarkup]) then
    Editor.MarkupManager.AddMarkUp(TTheWordTagPairMarkup.Create(Editor));
end;

{ TTheWordTagPairMarkup }

constructor TTheWordTagPairMarkup.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);

  MarkupInfo.Clear;
  MarkupInfo.Background := clInfoBk;
  MarkupInfo.BackPriority := 1;
  MarkupInfo.FrameColor := clHighlight;
  MarkupInfo.FramePriority := 1;
  MarkupInfo.FrameEdges := sfeAround;
  MarkupInfo.FrameStyle := slsSolid;
end;

function TTheWordTagPairMarkup.ContainsPosition(
  const Position: TTheWordTagPairPosition; ARow, ACol: Integer): Boolean;
begin
  Result := (Position.Row = ARow) and (ACol >= Position.StartCol) and
    (ACol < Position.EndCol);
end;

procedure TTheWordTagPairMarkup.InvalidatePosition(
  const Position: TTheWordTagPairPosition);
begin
  if Position.Row > 0 then
    InvalidateSynLines(Position.Row, Position.Row);
end;

procedure TTheWordTagPairMarkup.UpdateHighlight;
var
  OldCurrent: TTheWordTagPairPosition;
  OldMatching: TTheWordTagPairPosition;
  OldHasHighlight: Boolean;
begin
  OldCurrent := FCurrentTag;
  OldMatching := FMatchingTag;
  OldHasHighlight := FHasHighlight;

  FHasHighlight := Assigned(Caret) and Assigned(Lines) and
    FindTheWordMatchingTagPairAt(Lines, Caret.LineBytePos, FCurrentTag,
      FMatchingTag);
  if not FHasHighlight then
  begin
    ClearTagPairPosition(FCurrentTag);
    ClearTagPairPosition(FMatchingTag);
  end;

  if OldHasHighlight then
  begin
    InvalidatePosition(OldCurrent);
    InvalidatePosition(OldMatching);
  end;
  if FHasHighlight then
  begin
    InvalidatePosition(FCurrentTag);
    InvalidatePosition(FMatchingTag);
  end;
end;

procedure TTheWordTagPairMarkup.DoCaretChanged(Sender: TObject);
begin
  UpdateHighlight;
end;

procedure TTheWordTagPairMarkup.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  UpdateHighlight;
  if (StartLine > 0) and (EndLine > 0) then
    InvalidateSynLines(StartLine, EndLine);
end;

function TTheWordTagPairMarkup.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  Result := nil;
  if not FHasHighlight then
    Exit;

  if ContainsPosition(FCurrentTag, aRow, aStartCol.Logical) then
  begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(FCurrentTag.StartCol, FCurrentTag.EndCol);
  end
  else if ContainsPosition(FMatchingTag, aRow, aStartCol.Logical) then
  begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(FMatchingTag.StartCol, FMatchingTag.EndCol);
  end;
end;

procedure TTheWordTagPairMarkup.GetNextMarkupColAfterRowCol(
  const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer);

  procedure CheckPosition(const Position: TTheWordTagPairPosition);
  begin
    if Position.Row <> aRow then
      Exit;

    if (Position.StartCol > aStartCol.Logical) and
      ((ANextLog < 0) or (Position.StartCol < ANextLog)) then
      ANextLog := Position.StartCol;
    if (Position.EndCol > aStartCol.Logical) and
      ((ANextLog < 0) or (Position.EndCol < ANextLog)) then
      ANextLog := Position.EndCol;
  end;

begin
  ANextPhys := -1;
  ANextLog := -1;
  if not FHasHighlight then
    Exit;

  CheckPosition(FCurrentTag);
  CheckPosition(FMatchingTag);
end;

function TTheWordSyntaxHighlighter.CharAt(Index: Integer): Char;
begin
  if (Index < 0) or (Index >= FLineLength) then
    Result := #0
  else
    Result := FLine[Index + 1];
end;

function TTheWordSyntaxHighlighter.FindTagEnd(StartPos: Integer): Integer;
begin
  Result := StartPos + 1;
  while Result < FLineLength do
  begin
    if CharAt(Result) = '>' then
      Exit;
    Inc(Result);
  end;
  Result := -1;
end;

function TTheWordSyntaxHighlighter.GetTagName(const Inner: string): string;
var
  StartIndex: Integer;
  EndIndex: Integer;
begin
  Result := '';
  if Inner = '' then
    Exit;

  StartIndex := 1;
  if Inner[StartIndex] = '/' then
    Inc(StartIndex);

  EndIndex := StartIndex;
  while (EndIndex <= Length(Inner)) and not IsTagStop(Inner[EndIndex]) do
    Inc(EndIndex);

  Result := Copy(Inner, StartIndex, EndIndex - StartIndex);
end;

function TTheWordSyntaxHighlighter.IsSemanticTagName(const AName: string): Boolean;
begin
  Result :=
    (AName = 'FI') or (AName = 'Fi') or
    (AName = 'FR') or (AName = 'Fr') or
    (AName = 'FO') or (AName = 'Fo') or
    (AName = 'FU') or (AName = 'Fu') or
    (AName = 'CM') or (AName = 'CL') or (AName = 'CI') or
    (AName = 'PI') or (AName = 'Pi') or (AName = 'PF') or
    (AName = 'TS') or (AName = 'Ts') or
    (AName = 'NB') or (AName = 'Nb') or
    (AName = 'ST') or (AName = 'St');

  if not Result and (Length(AName) = 3) then
  begin
    Result :=
      (((Copy(AName, 1, 2) = 'PI') or (Copy(AName, 1, 2) = 'PF')) and
       (AName[3] in ['0'..'7'])) or
      ((Copy(AName, 1, 2) = 'TS') and (AName[3] in ['0'..'3']));
  end;
end;

function TTheWordSyntaxHighlighter.IsStrongMorphTagName(const AName: string;
  out IsMalformed: Boolean; out AKind: TTheWordTokenKind): Boolean;
var
  Index: Integer;
  LastIndex: Integer;
begin
  Result := False;
  IsMalformed := False;
  AKind := twStrongMorphTag;

  if AName = 'wt' then
  begin
    AKind := twWordTokenTag;
    Exit(True);
  end;

  if (Length(AName) >= 2) and ((Copy(AName, 1, 2) = 'WG') or (Copy(AName, 1, 2) = 'WH')) then
  begin
    Result := True;
    AKind := twStrongTag;
    LastIndex := Length(AName);
    if (LastIndex >= 3) and (AName[LastIndex] in ['x', 's']) then
      Dec(LastIndex);

    if LastIndex < 3 then
    begin
      IsMalformed := True;
      Exit;
    end;

    for Index := 3 to LastIndex do
      if not IsAsciiDigit(AName[Index]) then
      begin
        IsMalformed := True;
        Exit;
      end;
  end
  else if Copy(AName, 1, 2) = 'WT' then
  begin
    Result := True;
    AKind := twMorphTag;
    IsMalformed := Length(AName) <= 2;
  end;
end;

function TTheWordSyntaxHighlighter.IsNoteRefTagName(const AName: string): Boolean;
begin
  Result := (AName = 'RF') or (AName = 'Rf') or (AName = 'RX') or (AName = 'Rx');
end;

function TTheWordSyntaxHighlighter.IsHtmlTagName(const AName: string): Boolean;
begin
  Result :=
    (AName = 'b') or (AName = 'i') or (AName = 'u') or (AName = 's') or
    (AName = 'sup') or (AName = 'sub') or (AName = 'font') or
    (AName = 'span') or (AName = 'br') or (AName = 'p') or
    (AName = 'a') or (AName = 'rtl') or (AName = 'ltr');
end;

function TTheWordSyntaxHighlighter.IsRtfTagName(const AName: string): Boolean;
begin
  Result := AName = 'rtf';
end;

function TTheWordSyntaxHighlighter.AnalyzeTagAt(StartPos: Integer;
  out EndPos: Integer; out TagKind: TTheWordTokenKind): Boolean;
var
  Inner: string;
  TagName: string;
  MalformedStrongMorph: Boolean;
  StrongMorphKind: TTheWordTokenKind;
begin
  Result := False;
  EndPos := FindTagEnd(StartPos);
  TagKind := twWarning;

  if EndPos < 0 then
    Exit;

  Inner := Copy(FLine, StartPos + 2, EndPos - StartPos - 1);
  if (Inner = '') or IsTagSpace(Inner[1]) or IsTagSpace(Inner[Length(Inner)]) then
    Exit;

  TagName := GetTagName(Inner);
  if TagName = '' then
    Exit;

  if IsStrongMorphTagName(TagName, MalformedStrongMorph, StrongMorphKind) then
  begin
    if MalformedStrongMorph then
      Exit;
    TagKind := StrongMorphKind;
  end
  else if IsSemanticTagName(TagName) then
    TagKind := twSemanticTag
  else if IsNoteRefTagName(TagName) then
    TagKind := twNoteRefTag
  else if IsHtmlTagName(TagName) then
  begin
    if TagName = 'a' then
      TagKind := twLinkTag
    else
      TagKind := twHtmlTag;
  end
  else if IsRtfTagName(TagName) then
    TagKind := twRtfTag
  else
    Exit;

  Result := True;
end;

procedure TTheWordSyntaxHighlighter.SetToken(AKind: TTheWordTokenKind;
  NewRun: Integer);
begin
  FTokenKind := AKind;
  FRun := NewRun;
end;

procedure TTheWordSyntaxHighlighter.ReadWhitespace;
begin
  while (FRun < FLineLength) and (CharAt(FRun) <= ' ') do
    Inc(FRun);
  FTokenKind := twSpace;
end;

procedure TTheWordSyntaxHighlighter.ReadText;
begin
  while (FRun < FLineLength) and (CharAt(FRun) > ' ') and (CharAt(FRun) <> '<') do
    Inc(FRun);
  FTokenKind := twText;
end;

procedure TTheWordSyntaxHighlighter.ReadTagName;
begin
  while (FRun < FLineLength) and (FRun <= FTagEnd) and not IsTagStop(CharAt(FRun)) do
    Inc(FRun);
  FTokenKind := FActiveTagKind;
  FTagNameRead := True;
end;

procedure TTheWordSyntaxHighlighter.ReadAttributeOrValue;
var
  Quote: Char;
  Start: Integer;
  AttrName: string;
  NextChar: Char;
begin
  if CharAt(FRun) in ['"', ''''] then
  begin
    Quote := CharAt(FRun);
    Inc(FRun);
    while (FRun < FLineLength) and (FRun <= FTagEnd) and (CharAt(FRun) <> Quote) do
      Inc(FRun);
    if CharAt(FRun) = Quote then
      Inc(FRun);
    FTokenKind := twAttributeValue;
    FExpectValue := False;
    Exit;
  end;

  Start := FRun;
  while (FRun < FLineLength) and (FRun <= FTagEnd) and
    not IsTagSpace(CharAt(FRun)) and not (CharAt(FRun) in ['=', '/', '>']) do
    Inc(FRun);

  AttrName := Copy(FLine, Start + 1, FRun - Start);
  NextChar := CharAt(FRun);

  if FExpectValue or (NextChar <> '=') then
    FTokenKind := twAttributeValue
  else if IsKnownAttribute(AttrName) then
    FTokenKind := twAttribute
  else
    FTokenKind := twAttribute;

  FExpectValue := False;
end;

constructor TTheWordSyntaxHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTextAttri := TSynHighlighterAttributes.Create('Text', 'text');
  AddAttribute(FTextAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create('Space', 'space');
  AddAttribute(FSpaceAttri);

  FSemanticTagAttri := TSynHighlighterAttributes.Create('theWord tag', 'theword_tag');
  FSemanticTagAttri.Foreground := clNavy;
  FSemanticTagAttri.Style := [fsBold];
  AddAttribute(FSemanticTagAttri);

  FStrongMorphTagAttri := TSynHighlighterAttributes.Create('Strong/morph tag', 'strong_morph_tag');
  FStrongMorphTagAttri.Foreground := clBlue;
  FStrongMorphTagAttri.Style := [fsBold];
  AddAttribute(FStrongMorphTagAttri);

  FStrongTagAttri := TSynHighlighterAttributes.Create('Strong tag', 'strong_tag');
  FStrongTagAttri.Foreground := clBlue;
  FStrongTagAttri.Style := [fsBold];
  AddAttribute(FStrongTagAttri);

  FMorphTagAttri := TSynHighlighterAttributes.Create('Morphology tag', 'morphology_tag');
  FMorphTagAttri.Foreground := clFuchsia;
  FMorphTagAttri.Style := [fsBold];
  AddAttribute(FMorphTagAttri);

  FWordTokenTagAttri := TSynHighlighterAttributes.Create('Word token tag', 'word_token_tag');
  FWordTokenTagAttri.Foreground := clMaroon;
  FWordTokenTagAttri.Style := [fsBold];
  AddAttribute(FWordTokenTagAttri);

  FNoteRefTagAttri := TSynHighlighterAttributes.Create('Note/reference tag', 'note_reference_tag');
  FNoteRefTagAttri.Foreground := clOlive;
  FNoteRefTagAttri.Style := [fsBold];
  AddAttribute(FNoteRefTagAttri);

  FLinkTagAttri := TSynHighlighterAttributes.Create('Link tag', 'link_tag');
  FLinkTagAttri.Foreground := clBlue;
  FLinkTagAttri.Style := [fsUnderline];
  AddAttribute(FLinkTagAttri);

  FHtmlTagAttri := TSynHighlighterAttributes.Create('HTML tag', 'html_tag');
  FHtmlTagAttri.Foreground := clRed;
  FHtmlTagAttri.Style := [fsBold];
  AddAttribute(FHtmlTagAttri);

  FRtfTagAttri := TSynHighlighterAttributes.Create('RTF tag', 'rtf_tag');
  FRtfTagAttri.Foreground := clPurple;
  AddAttribute(FRtfTagAttri);

  FAttributeAttri := TSynHighlighterAttributes.Create('Attribute', 'attribute');
  FAttributeAttri.Foreground := clGray;
  AddAttribute(FAttributeAttri);

  FAttributeValueAttri := TSynHighlighterAttributes.Create('Attribute value', 'attribute_value');
  FAttributeValueAttri.Foreground := clGreen;
  AddAttribute(FAttributeValueAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create('Symbol', 'symbol');
  FSymbolAttri.Foreground := clGray;
  AddAttribute(FSymbolAttri);

  FWarningAttri := TSynHighlighterAttributes.Create('Warning', 'warning');
  FWarningAttri.Foreground := clRed;
  FWarningAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FWarningAttri);

  SetAttributesOnChange(@DefHighlightChange);
  ApplyTheWordSyntaxColorScheme(Self);
  FDefaultFilter := STheWordSyntax + ' (*.*)|*.*';
  FDefaultFilterInitialValue := FDefaultFilter;
end;

class function TTheWordSyntaxHighlighter.GetLanguageName: string;
begin
  Result := STheWordSyntax;
end;

function TTheWordSyntaxHighlighter.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER:
      Result := FTextAttri;
    SYN_ATTR_KEYWORD:
      Result := FSemanticTagAttri;
    SYN_ATTR_COMMENT:
      Result := FWarningAttri;
    SYN_ATTR_STRING:
      Result := FAttributeValueAttri;
    SYN_ATTR_SYMBOL:
      Result := FSymbolAttri;
    SYN_ATTR_WHITESPACE:
      Result := FSpaceAttri;
  else
    Result := FTextAttri;
  end;
end;

function TTheWordSyntaxHighlighter.GetEol: Boolean;
begin
  Result := FTokenKind = twNull;
end;

function TTheWordSyntaxHighlighter.GetToken: string;
begin
  Result := Copy(FLine, FTokenPos + 1, FRun - FTokenPos);
end;

procedure TTheWordSyntaxHighlighter.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength := FRun - FTokenPos;
  if TokenLength > 0 then
    TokenStart := PChar(FLine) + FTokenPos
  else
    TokenStart := nil;
end;

function TTheWordSyntaxHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenKind of
    twText:
      Result := FTextAttri;
    twSpace:
      Result := FSpaceAttri;
    twSemanticTag:
      Result := FSemanticTagAttri;
    twStrongMorphTag:
      Result := FStrongMorphTagAttri;
    twStrongTag:
      Result := FStrongTagAttri;
    twMorphTag:
      Result := FMorphTagAttri;
    twWordTokenTag:
      Result := FWordTokenTagAttri;
    twNoteRefTag:
      Result := FNoteRefTagAttri;
    twLinkTag:
      Result := FLinkTagAttri;
    twHtmlTag:
      Result := FHtmlTagAttri;
    twRtfTag:
      Result := FRtfTagAttri;
    twAttribute:
      Result := FAttributeAttri;
    twAttributeValue:
      Result := FAttributeValueAttri;
    twSymbol:
      Result := FSymbolAttri;
    twWarning:
      Result := FWarningAttri;
  else
    Result := nil;
  end;
end;

function TTheWordSyntaxHighlighter.GetTokenKind: integer;
begin
  Result := Ord(FTokenKind);
end;

function TTheWordSyntaxHighlighter.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

function TTheWordSyntaxHighlighter.GetTokenLen: Integer;
begin
  Result := FRun - FTokenPos;
end;

procedure TTheWordSyntaxHighlighter.Next;
var
  EndPos: Integer;
  TagKind: TTheWordTokenKind;
begin
  FTokenPos := FRun;

  if FRun >= FLineLength then
  begin
    FTokenKind := twNull;
    Exit;
  end;

  if FInsideTag then
  begin
    case CharAt(FRun) of
      '<':
        SetToken(twSymbol, FRun + 1);
      '>':
        begin
          SetToken(twSymbol, FRun + 1);
          FInsideTag := False;
          FExpectValue := False;
        end;
      '/':
        SetToken(twSymbol, FRun + 1);
      '=':
        begin
          SetToken(twSymbol, FRun + 1);
          FExpectValue := True;
        end;
      ' ', #9, #10, #13:
        ReadWhitespace;
    else
      if not FTagNameRead then
        ReadTagName
      else
        ReadAttributeOrValue;
    end;
    Exit;
  end;

  if CharAt(FRun) <= ' ' then
    ReadWhitespace
  else if CharAt(FRun) = '<' then
  begin
    if not AnalyzeTagAt(FRun, EndPos, TagKind) then
    begin
      if EndPos < 0 then
        EndPos := FLineLength - 1;
      SetToken(twWarning, EndPos + 1);
      Exit;
    end;

    FInsideTag := True;
    FTagEnd := EndPos;
    FActiveTagKind := TagKind;
    FTagNameRead := False;
    FExpectValue := False;
    SetToken(twSymbol, FRun + 1);
  end
  else
    ReadText;
end;

procedure TTheWordSyntaxHighlighter.SetLine(const NewValue: string;
  LineNumber: Integer);
begin
  FLine := NewValue;
  FLineLength := Length(FLine);
  FRun := 0;
  FInsideTag := False;
  FTagEnd := -1;
  FTagNameRead := False;
  FExpectValue := False;
  FActiveTagKind := twText;
  inherited SetLine(NewValue, LineNumber);
  Next;
end;

procedure ConfigureTheWordSyntaxEditor(Editor: TSynEdit; ReadOnly: Boolean);
begin
  if not Assigned(Editor) then
    Exit;

  ApplyTheWordSyntaxEditorFont(Editor);
  Editor.Gutter.Visible := False;
  Editor.RightGutter.Visible := False;
  Editor.ScrollBars := ssAutoVertical;
  Editor.ReadOnly := ReadOnly;
  Editor.WantTabs := False;
  Editor.TabWidth := 2;
  Editor.RightEdge := 0;
  Editor.Options := Editor.Options + [eoHideRightMargin];
  Editor.Options := Editor.Options - [eoScrollPastEol];
  EnsureLineWrapPlugin(Editor);
  EnsureTagPairMarkup(Editor);

  if not (Editor.Highlighter is TTheWordSyntaxHighlighter) then
    Editor.Highlighter := TTheWordSyntaxHighlighter.Create(Editor);
  if Editor.Highlighter is TTheWordSyntaxHighlighter then
    ApplyTheWordSyntaxColorScheme(TTheWordSyntaxHighlighter(Editor.Highlighter));
end;

function TheWordSyntaxTokenKindName(Kind: Integer): string;
begin
  case TTheWordTokenKind(Kind) of
    twNull:
      Result := 'null';
    twText:
      Result := 'text';
    twSpace:
      Result := 'space';
    twSemanticTag:
      Result := 'semantic_tag';
    twStrongMorphTag:
      Result := 'strong_morph_tag';
    twStrongTag:
      Result := 'strong_tag';
    twMorphTag:
      Result := 'morphology_tag';
    twWordTokenTag:
      Result := 'word_token_tag';
    twNoteRefTag:
      Result := 'note_ref_tag';
    twLinkTag:
      Result := 'link_tag';
    twHtmlTag:
      Result := 'html_tag';
    twRtfTag:
      Result := 'rtf_tag';
    twAttribute:
      Result := 'attribute';
    twAttributeValue:
      Result := 'attribute_value';
    twSymbol:
      Result := 'symbol';
    twWarning:
      Result := 'warning';
  else
    Result := 'unknown';
  end;
end;

initialization
  ResetTheWordSyntaxColorScheme;
  ResetTheWordSyntaxEditorFont;

end.
