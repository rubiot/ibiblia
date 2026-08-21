unit theword_translator_note_preview;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, Types, KMemo, KFunctions,
  KGraphics, theword_translator_note_body_v2, verse_visual_document;

type
  TNoteDirection = (ndNone, ndLtr, ndRtl);

  TTranslatorNoteRun = class(TKMemoTextBlock)
  private
    FHasRelativeFontSize: Boolean;
    FHref: UnicodeString;
    FInlineDirection: TNoteDirection;
    FParagraphDirection: TNoteDirection;
    FRelativeFontSize: Integer;
    procedure SetInlineDirection(AValue: TNoteDirection);
    procedure SetParagraphDirection(AValue: TNoteDirection);
  protected
    function InternalTextExtent(ACanvas: TCanvas;
      const AText: TKString): TSize; override;
    procedure InternalTextOutput(ACanvas: TCanvas; ALeft, ATop: Integer;
      const AText: TKString); override;
  public
    procedure Assign(ASource: TKObject); override;
    function DirectionalDisplayText(const AText: TKString): TKString;
    property HasRelativeFontSize: Boolean read FHasRelativeFontSize
      write FHasRelativeFontSize;
    property Href: UnicodeString read FHref write FHref;
    property InlineDirection: TNoteDirection read FInlineDirection
      write SetInlineDirection;
    property ParagraphDirection: TNoteDirection read FParagraphDirection
      write SetParagraphDirection;
    property RelativeFontSize: Integer read FRelativeFontSize
      write FRelativeFontSize;
  end;

  TTranslatorNoteSoftBreak = class(TKMemoParagraph);

  TTranslatorNoteBodyView = class(TKMemo)
  private
    FBodyRaw: UnicodeString;
    FCapability: TTheWordNoteBodyCapability;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetNoteBody(const ABodyRaw: UnicodeString);
    property BodyRaw: UnicodeString read FBodyRaw;
    property Capability: TTheWordNoteBodyCapability read FCapability;
  end;

procedure LoadTranslatorNoteBodyIntoMemo(AMemo: TKMemo;
  const ABodyRaw: UnicodeString); overload;
procedure LoadTranslatorNoteBodyIntoMemo(AMemo: TKMemo;
  const ABodyRaw: UnicodeString;
  out ACapability: TTheWordNoteBodyCapability); overload;
procedure ApplyTranslatorNoteParagraphDirections(AMemo: TKMemo);

implementation

uses
  Math;

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

function WrapDirectionalText(const AText: TKString;
  ADirection: TNoteDirection): TKString;
var
  Isolate: UnicodeChar;
begin
  case ADirection of
    ndLtr: Isolate := LTR_ISOLATE;
    ndRtl: Isolate := RTL_ISOLATE;
  else
    Exit(AText);
  end;
  Result := UTF8Encode(UnicodeString(Isolate) + UTF8Decode(AText) +
    UnicodeString(POP_DIRECTIONAL_ISOLATE));
end;

procedure TTranslatorNoteRun.Assign(ASource: TKObject);
begin
  inherited Assign(ASource);
  if ASource is TTranslatorNoteRun then
  begin
    FHasRelativeFontSize :=
      TTranslatorNoteRun(ASource).FHasRelativeFontSize;
    FRelativeFontSize := TTranslatorNoteRun(ASource).FRelativeFontSize;
    FHref := TTranslatorNoteRun(ASource).FHref;
    FInlineDirection := TTranslatorNoteRun(ASource).FInlineDirection;
    FParagraphDirection :=
      TTranslatorNoteRun(ASource).FParagraphDirection;
  end;
  if not (ASource is TTranslatorNoteRun) then
  begin
    FHasRelativeFontSize := False;
    FRelativeFontSize := 0;
    FHref := '';
    FInlineDirection := ndNone;
    FParagraphDirection := ndNone;
  end;
  Update([muExtent]);
end;

function TTranslatorNoteRun.DirectionalDisplayText(
  const AText: TKString): TKString;
begin
  Result := WrapDirectionalText(AText, FInlineDirection);
  Result := WrapDirectionalText(Result, FParagraphDirection);
end;

function TTranslatorNoteRun.InternalTextExtent(ACanvas: TCanvas;
  const AText: TKString): TSize;
begin
  Result := inherited InternalTextExtent(ACanvas,
    DirectionalDisplayText(AText));
end;

procedure TTranslatorNoteRun.InternalTextOutput(ACanvas: TCanvas;
  ALeft, ATop: Integer; const AText: TKString);
begin
  inherited InternalTextOutput(ACanvas, ALeft, ATop,
    DirectionalDisplayText(AText));
end;

procedure TTranslatorNoteRun.SetInlineDirection(AValue: TNoteDirection);
begin
  if FInlineDirection = AValue then Exit;
  FInlineDirection := AValue;
  Update([muExtent]);
end;

procedure TTranslatorNoteRun.SetParagraphDirection(AValue: TNoteDirection);
begin
  if FParagraphDirection = AValue then Exit;
  FParagraphDirection := AValue;
  Update([muExtent]);
end;

procedure ApplyTranslatorNoteParagraphDirections(AMemo: TKMemo);
var
  Block: TKMemoBlock;
  Direction: TNoteDirection;
  Index: Integer;

  procedure ApplyAlignment(AStyle: TKMemoParaStyle);
  begin
    if Direction = ndRtl then
      AStyle.HAlign := halRight
    else
      AStyle.HAlign := halLeft;
  end;

begin
  if not Assigned(AMemo) then Exit;
  Direction := ndNone;
  for Index := 0 to AMemo.Blocks.Count - 1 do
  begin
    Block := AMemo.Blocks[Index];
    if (Block is TTranslatorNoteRun) and
       (TTranslatorNoteRun(Block).ParagraphDirection <> ndNone) then
      Direction := TTranslatorNoteRun(Block).ParagraphDirection;
    if Block is TKMemoParagraph then
    begin
      ApplyAlignment(TKMemoParagraph(Block).ParaStyle);
      if not (Block is TTranslatorNoteSoftBreak) then
        Direction := ndNone;
    end;
  end;
  ApplyAlignment(AMemo.Blocks.DefaultParaStyle);
  AMemo.Invalidate;
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

procedure LoadTranslatorNoteBodyIntoMemo(AMemo: TKMemo;
  const ABodyRaw: UnicodeString);
var
  Capability: TTheWordNoteBodyCapability;
begin
  LoadTranslatorNoteBodyIntoMemo(AMemo, ABodyRaw, Capability);
end;

procedure LoadTranslatorNoteBodyIntoMemo(AMemo: TKMemo;
  const ABodyRaw: UnicodeString;
  out ACapability: TTheWordNoteBodyCapability);
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
    if AMemo.Blocks.Count = 0 then
    begin
      Run := TTranslatorNoteRun.Create;
      Run.Text := '';
      ApplyStateToRun(Run, Current);
      AMemo.Blocks.AddAt(Run);
    end;
    ApplyTranslatorNoteParagraphDirections(AMemo);
    // Callers can hold Blocks.LockUpdate while the line cache still describes
    // the previous body. Selecting here would index that stale cache.
    AMemo.Modified := False;
  finally
    Body.Free;
  end;
end;

constructor TTranslatorNoteBodyView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := True;
  ScrollBars := ssAutoVertical;
end;

procedure TTranslatorNoteBodyView.SetNoteBody(
  const ABodyRaw: UnicodeString);
begin
  FBodyRaw := ABodyRaw;
  Blocks.LockUpdate;
  try
    LoadTranslatorNoteBodyIntoMemo(Self, ABodyRaw, FCapability);
  finally
    Blocks.UnlockUpdate;
  end;
end;

end.
