unit module_properties_preview;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, StdCtrls, KMemo, KEditCommon;

type
  TModulePropertiesPreview = class(TKMemo)
  private
    FRawText: string;
    FBaseFontSize: Integer;
    FLastSkinId: Integer;
    FHasRendered: Boolean;
    procedure HandleHyperlinkClick(Sender: TObject);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RenderContent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySkinColors;
    procedure SetPropertiesText(const ARawText: string;
      ForceRender: Boolean = False);
    property RawText: string read FRawText;
  end;

resourcestring
  SPropsPreviewEmpty = '(No module properties)';
  SPropsPreviewAboutHeading = 'About';
  SPropsPreviewRulesHeading = 'Verse rules & comments';

implementation

uses
  StrUtils, Math, LCLIntf, LazUTF8, HTMLColors, ui_style, unitskins;

type
  TPropHAlign = (phaLeft, phaCenter, phaRight);

  TPropTextStyleState = record
    FontName: string;
    FontSize: Integer;
    FontStyle: TFontStyles;
    Color: TColor;
    HasExplicitColor: Boolean;
    BackgroundColor: TColor;
    HasBackground: Boolean;
    ScriptPosition: TKMemoScriptPosition;
    Href: string;
    Align: TPropHAlign;
    LeftPadding: Integer;
    Preformatted: Boolean;
  end;

  TPropListKind = (plkUnordered, plkOrdered);

  TPropListLevel = record
    Kind: TPropListKind;
    Counter: Integer;
  end;

  TPropMetaEntry = record
    Key: string;
    Value: string;
  end;

  TPropMetaArray = array of TPropMetaEntry;

const
  CP1252_80_9F: array[$80..$9F] of Cardinal = (
    $20AC, $003F, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152, $003F, $017D, $003F,
    $003F, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153, $003F, $017E, $0178
  );

function PreviewDefaultTextColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := UITextColor
  else
    Result := CurrentSkin.WindowText;
end;

function PreviewMutedColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := UIMutedTextColor
  else
    Result := CurrentSkin.WindowTextDim;
end;

function PreviewLinkColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := MakeRGB(0, 92, 197)
  else if CurrentSkin.IsDark then
    Result := MakeRGB(115, 185, 255)
  else
    Result := CurrentSkin.VerseNumColor;
end;

function ColorLuminance(AColor: TColor): Integer;
var
  RgbVal, R, G, B: LongInt;
begin
  RgbVal := ColorToRGB(AColor);
  R := RgbVal and $FF;
  G := (RgbVal shr 8) and $FF;
  B := (RgbVal shr 16) and $FF;
  Result := (299 * R + 587 * G + 114 * B) div 1000;
end;

function AdaptHtmlColorForSkin(AColor: TColor): TColor;
var
  RgbVal: LongInt;
  R, G, B: Byte;
begin
  Result := AColor;
  if CurrentSkin.Id = skClassic then
    Exit;
  RgbVal := ColorToRGB(AColor);
  R := RgbVal and $FF;
  G := (RgbVal shr 8) and $FF;
  B := (RgbVal shr 16) and $FF;

  if CurrentSkin.IsDark then
  begin
    if (B >= 120) and (R <= 60) and (G <= 95) then
      Result := MakeRGB(115, 185, 255)
    else if (R >= 110) and (G <= 60) and (B <= 60) then
      Result := MakeRGB(244, 110, 110)
    else if (G >= 95) and (R <= 60) and (B <= 60) then
      Result := MakeRGB(130, 225, 150)
    else if (R + G + B) < 120 then
      Result := CurrentSkin.WindowText;
  end
  else
  begin
    if (R = 0) and (G = 0) and (B = 0) then
      Result := CurrentSkin.WindowText;
  end;
end;

function IsSafeWebHref(const AUrl: string): Boolean;
var
  Trimmed: string;
begin
  Trimmed := Trim(AUrl);
  Result := AnsiStartsText('http://', Trimmed) or
    AnsiStartsText('https://', Trimmed) or
    AnsiStartsText('mailto:', Trimmed);
end;

function TryParseHexByte(const S: string; out Value: Byte): Boolean;
var
  IntVal: Integer;
begin
  Result := (Length(S) = 2) and TryStrToInt('$' + S, IntVal) and
    (IntVal >= 0) and (IntVal <= 255);
  if Result then
    Value := Byte(IntVal);
end;

function TryParseCssColor(const AValue: string; out AColor: TColor): Boolean;
var
  S, Lower, Inner: string;
  R, G, B: Byte;
  Comma1, Comma2, RInt, GInt, BInt, I: Integer;
  Parts: TStringList;
begin
  Result := False;
  AColor := clNone;
  S := Trim(AValue);
  if S = '' then
    Exit;
  Lower := LowerCase(S);
  if (Lower = 'transparent') or (Lower = 'inherit') or
     (Lower = 'initial') or (Lower = 'none') then
    Exit;

  if S[1] = '#' then
  begin
    if Length(S) = 7 then
    begin
      if TryParseHexByte(Copy(S, 2, 2), R) and
         TryParseHexByte(Copy(S, 4, 2), G) and
         TryParseHexByte(Copy(S, 6, 2), B) then
      begin
        AColor := MakeRGB(R, G, B);
        Exit(True);
      end;
    end
    else if Length(S) = 4 then
    begin
      if TryParseHexByte(S[2] + S[2], R) and
         TryParseHexByte(S[3] + S[3], G) and
         TryParseHexByte(S[4] + S[4], B) then
      begin
        AColor := MakeRGB(R, G, B);
        Exit(True);
      end;
    end;
    Exit;
  end;

  if AnsiStartsStr('rgb(', Lower) and AnsiEndsStr(')', Lower) then
  begin
    Inner := Trim(Copy(Lower, 5, Length(Lower) - 5));
    Parts := TStringList.Create;
    try
      Parts.StrictDelimiter := True;
      Parts.Delimiter := ',';
      Parts.DelimitedText := Inner;
      if (Parts.Count = 3) and
         TryStrToInt(Trim(Parts[0]), RInt) and
         TryStrToInt(Trim(Parts[1]), GInt) and
         TryStrToInt(Trim(Parts[2]), BInt) then
      begin
        AColor := MakeRGB(
          Byte(EnsureRange(RInt, 0, 255)),
          Byte(EnsureRange(GInt, 0, 255)),
          Byte(EnsureRange(BInt, 0, 255)));
        Exit(True);
      end;
    finally
      Parts.Free;
    end;
    Exit;
  end;

  Comma1 := Pos('!', Lower);
  if Comma1 > 0 then
    Lower := Trim(Copy(Lower, 1, Comma1 - 1));
  Comma2 := Pos(' ', Lower);
  if Comma2 > 0 then
    Lower := Trim(Copy(Lower, 1, Comma2 - 1));

  for I := Low(ColorTable) to High(ColorTable) do
    if ColorTable[I].Name = Lower then
    begin
      AColor := MakeRGB(
        Byte(ColorTable[I].Value shr 16),
        Byte(ColorTable[I].Value shr 8),
        Byte(ColorTable[I].Value));
      Exit(True);
    end;
end;

function TryParseCssPixelLength(const AValue: string; ABaseFontSize: Integer;
  out APixels: Integer): Boolean;
var
  S, NumStr: string;
  FloatVal: Double;
  I: Integer;
  FS: TFormatSettings;
begin
  Result := False;
  APixels := 0;
  S := LowerCase(Trim(AValue));
  if S = '' then
    Exit;
  I := 1;
  while (I <= Length(S)) and (S[I] in ['0'..'9', '+', '-', '.']) do
    Inc(I);
  if I = 1 then
    Exit;
  NumStr := Copy(S, 1, I - 1);
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  if not TryStrToFloat(NumStr, FloatVal, FS) then
    Exit;
  S := Trim(Copy(S, I, MaxInt));
  if (S = '') or (S = 'px') then
    APixels := Round(FloatVal)
  else if S = 'pt' then
    APixels := Round(FloatVal * 96.0 / 72.0)
  else if (S = 'em') or (S = 'rem') then
    APixels := Round(FloatVal * Max(10, ABaseFontSize) * 1.33)
  else
    Exit;
  Result := True;
end;

function DecodeHtmlEntities(const AStr: string): string;
var
  I, Len, SemiPos, Cp: Integer;
  Ent, LowerEnt: string;
begin
  if Pos('&', AStr) = 0 then
    Exit(AStr);
  Result := '';
  I := 1;
  Len := Length(AStr);
  while I <= Len do
  begin
    if AStr[I] = '&' then
    begin
      SemiPos := I + 1;
      while (SemiPos <= Len) and (SemiPos - I <= 12) and
            not (AStr[SemiPos] in [';', ' ', '<', '&', #10, #13]) do
        Inc(SemiPos);
      if (SemiPos <= Len) and (AStr[SemiPos] = ';') then
      begin
        Ent := Copy(AStr, I + 1, SemiPos - I - 1);
        if (Length(Ent) >= 2) and (Ent[1] = '#') then
        begin
          if (Length(Ent) >= 3) and (Ent[2] in ['x', 'X']) then
          begin
            if not TryStrToInt('$' + Copy(Ent, 3, MaxInt), Cp) then
              Cp := -1;
          end
          else
          begin
            if not TryStrToInt(Copy(Ent, 2, MaxInt), Cp) then
              Cp := -1;
          end;
          if (Cp > 0) and (Cp <= $10FFFF) then
          begin
            Result := Result + UnicodeToUTF8(Cardinal(Cp));
            I := SemiPos + 1;
            Continue;
          end;
        end
        else
        begin
          LowerEnt := LowerCase(Ent);
          if LowerEnt = 'nbsp' then begin Result := Result + ' '; I := SemiPos + 1; Continue; end
          else if LowerEnt = 'amp' then begin Result := Result + '&'; I := SemiPos + 1; Continue; end
          else if LowerEnt = 'lt' then begin Result := Result + '<'; I := SemiPos + 1; Continue; end
          else if LowerEnt = 'gt' then begin Result := Result + '>'; I := SemiPos + 1; Continue; end
          else if LowerEnt = 'quot' then begin Result := Result + '"'; I := SemiPos + 1; Continue; end
          else if LowerEnt = 'apos' then begin Result := Result + ''''; I := SemiPos + 1; Continue; end
          else if LowerEnt = 'rsquo' then begin Result := Result + UnicodeToUTF8($2019); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'lsquo' then begin Result := Result + UnicodeToUTF8($2018); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'ldquo' then begin Result := Result + UnicodeToUTF8($201C); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'rdquo' then begin Result := Result + UnicodeToUTF8($201D); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'ndash' then begin Result := Result + UnicodeToUTF8($2013); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'mdash' then begin Result := Result + UnicodeToUTF8($2014); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'hellip' then begin Result := Result + UnicodeToUTF8($2026); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'bull' then begin Result := Result + UnicodeToUTF8($2022); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'middot' then begin Result := Result + UnicodeToUTF8($00B7); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'copy' then begin Result := Result + UnicodeToUTF8($00A9); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'reg' then begin Result := Result + UnicodeToUTF8($00AE); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'trade' then begin Result := Result + UnicodeToUTF8($2122); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'laquo' then begin Result := Result + UnicodeToUTF8($00AB); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'raquo' then begin Result := Result + UnicodeToUTF8($00BB); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'deg' then begin Result := Result + UnicodeToUTF8($00B0); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'times' then begin Result := Result + UnicodeToUTF8($00D7); I := SemiPos + 1; Continue; end
          else if LowerEnt = 'divide' then begin Result := Result + UnicodeToUTF8($00F7); I := SemiPos + 1; Continue; end;
        end;
      end;
    end;
    Result := Result + AStr[I];
    Inc(I);
  end;
end;

function Cp1252ByteToUtf8(AByte: Byte): string;
begin
  if AByte < $80 then
    Result := Char(AByte)
  else if AByte <= $9F then
    Result := UnicodeToUTF8(CP1252_80_9F[AByte])
  else
    Result := UnicodeToUTF8(Cardinal(AByte));
end;

function LooksLikeHtml(const AStr: string): Boolean;
var
  Lower: string;
begin
  Lower := LowerCase(AStr);
  Result :=
    (Pos('<b>', Lower) > 0) or (Pos('</b>', Lower) > 0) or
    (Pos('<i>', Lower) > 0) or (Pos('</i>', Lower) > 0) or
    (Pos('<u>', Lower) > 0) or (Pos('</u>', Lower) > 0) or
    (Pos('<em>', Lower) > 0) or (Pos('<strong>', Lower) > 0) or
    (Pos('<br', Lower) > 0) or (Pos('<p>', Lower) > 0) or
    (Pos('<p ', Lower) > 0) or (Pos('</p>', Lower) > 0) or
    (Pos('<div', Lower) > 0) or (Pos('<span', Lower) > 0) or
    (Pos('<font', Lower) > 0) or (Pos('<center', Lower) > 0) or
    (Pos('<h1', Lower) > 0) or (Pos('<h2', Lower) > 0) or
    (Pos('<h3', Lower) > 0) or (Pos('<h4', Lower) > 0) or
    (Pos('<h5', Lower) > 0) or (Pos('<h6', Lower) > 0) or
    (Pos('<ol>', Lower) > 0) or (Pos('<ol ', Lower) > 0) or
    (Pos('<ul>', Lower) > 0) or (Pos('<ul ', Lower) > 0) or
    (Pos('<li>', Lower) > 0) or (Pos('<hr', Lower) > 0) or
    (Pos('<a ', Lower) > 0) or (Pos('<sup>', Lower) > 0) or
    (Pos('<sub>', Lower) > 0) or (Pos('<blockquote', Lower) > 0) or
    (Pos('<pre', Lower) > 0) or (Pos('<code', Lower) > 0) or
    (Pos('<small', Lower) > 0) or (Pos('<big', Lower) > 0) or
    (Pos('<table', Lower) > 0);
end;

function IsValidPropertyKey(const AKey: string): Boolean;
var
  I: Integer;
begin
  if AKey = '' then
    Exit(False);
  if not (AKey[1] in ['a'..'z', 'A'..'Z']) then
    Exit(False);
  for I := 2 to Length(AKey) do
    if not (AKey[I] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '_', '-']) then
      Exit(False);
  Result := True;
end;

procedure ExtractTagAttributes(const AAttrStr: string; out AAttrNames,
  AAttrValues: TStringList);
var
  I, Len, StartPos: Integer;
  QuoteChar: Char;
  NameStr, ValStr: string;
begin
  AAttrNames := TStringList.Create;
  AAttrValues := TStringList.Create;
  I := 1;
  Len := Length(AAttrStr);
  while I <= Len do
  begin
    while (I <= Len) and (AAttrStr[I] in [' ', #9, #10, #13, '/']) do
      Inc(I);
    if I > Len then
      Break;
    StartPos := I;
    while (I <= Len) and not (AAttrStr[I] in ['=', ' ', #9, #10, #13, '/']) do
      Inc(I);
    NameStr := LowerCase(Copy(AAttrStr, StartPos, I - StartPos));
    while (I <= Len) and (AAttrStr[I] in [' ', #9, #10, #13]) do
      Inc(I);
    ValStr := '';
    if (I <= Len) and (AAttrStr[I] = '=') then
    begin
      Inc(I);
      while (I <= Len) and (AAttrStr[I] in [' ', #9, #10, #13]) do
        Inc(I);
      if (I <= Len) and (AAttrStr[I] in ['"', '''']) then
      begin
        QuoteChar := AAttrStr[I];
        Inc(I);
        StartPos := I;
        while (I <= Len) and (AAttrStr[I] <> QuoteChar) do
          Inc(I);
        ValStr := Copy(AAttrStr, StartPos, I - StartPos);
        if (I <= Len) and (AAttrStr[I] = QuoteChar) then
          Inc(I);
      end
      else
      begin
        StartPos := I;
        while (I <= Len) and not (AAttrStr[I] in [' ', #9, #10, #13]) do
          Inc(I);
        ValStr := Copy(AAttrStr, StartPos, I - StartPos);
      end;
    end;
    if NameStr <> '' then
    begin
      AAttrNames.Add(NameStr);
      AAttrValues.Add(DecodeHtmlEntities(ValStr));
    end;
  end;
end;

function GetTagAttribute(AAttrNames, AAttrValues: TStringList;
  const ATargetName: string): string;
var
  I: Integer;
begin
  Result := '';
  if not Assigned(AAttrNames) or not Assigned(AAttrValues) then
    Exit;
  for I := 0 to AAttrNames.Count - 1 do
    if AAttrNames[I] = ATargetName then
      Exit(AAttrValues[I]);
end;

type
  TPropsDocumentBuilder = class
  private
    FMemo: TModulePropertiesPreview;
    FBaseFontSize: Integer;
    FHasBlocksInPara: Boolean;
    FLastCharWasSpace: Boolean;
    FParaAlign: TPropHAlign;
    FParaFirstIndent: Integer;
    FParaLeftPadding: Integer;
    FParaTopPadding: Integer;
    FParaBottomPadding: Integer;
    FStyleStack: array of TPropTextStyleState;
    FListStack: array of TPropListLevel;
    function CurrentStyle: TPropTextStyleState;
    procedure PushStyle(const AState: TPropTextStyleState);
    procedure PushInheritedStyle;
    procedure PopStyle;
    procedure ResetParagraphMetrics;
    procedure BeginParagraphIfNeeded;
    procedure FlushParagraph(ABottomPadding: Integer = -1);
    procedure EmitStyledText(const AStr: string;
      const AStyle: TPropTextStyleState);
    procedure EmitCollapsedText(const AStr: string);
    procedure ApplyInlineCss(const ACssText: string;
      var AState: TPropTextStyleState; AIsBlockTag: Boolean);
    procedure ApplyFontTagAttributes(AAttrNames, AAttrValues: TStringList;
      var AState: TPropTextStyleState);
    procedure EmitHorizontalRule;
  public
    constructor Create(AMemo: TModulePropertiesPreview;
      ABaseFontSize: Integer);
    procedure RenderPlainLine(const AStr: string; AColor: TColor;
      ABold, AItalic, AMono: Boolean; ABottomPadding: Integer;
      ATopPadding: Integer = 0; AFontSizeDelta: Integer = 0);
    procedure RenderMetadataRow(const AKey, AValue: string);
    procedure RenderHtmlFragment(const AHtml: string;
      ATrailingBreak: Boolean);
    procedure RenderRtfFragment(const ARtf: string);
    procedure FinalizeDocument;
  end;

constructor TPropsDocumentBuilder.Create(AMemo: TModulePropertiesPreview;
  ABaseFontSize: Integer);
var
  RootStyle: TPropTextStyleState;
begin
  inherited Create;
  FMemo := AMemo;
  FBaseFontSize := ABaseFontSize;
  FHasBlocksInPara := False;
  FLastCharWasSpace := False;
  SetLength(FStyleStack, 0);
  SetLength(FListStack, 0);

  RootStyle.FontName := UIDefaultFontName;
  RootStyle.FontSize := FBaseFontSize;
  RootStyle.FontStyle := [];
  RootStyle.Color := PreviewDefaultTextColor;
  RootStyle.HasExplicitColor := False;
  RootStyle.BackgroundColor := clNone;
  RootStyle.HasBackground := False;
  RootStyle.ScriptPosition := tpoNormal;
  RootStyle.Href := '';
  RootStyle.Align := phaLeft;
  RootStyle.LeftPadding := 0;
  RootStyle.Preformatted := False;
  PushStyle(RootStyle);
  ResetParagraphMetrics;
end;

function TPropsDocumentBuilder.CurrentStyle: TPropTextStyleState;
begin
  if Length(FStyleStack) > 0 then
    Result := FStyleStack[High(FStyleStack)]
  else
  begin
    Result.FontName := UIDefaultFontName;
    Result.FontSize := FBaseFontSize;
    Result.FontStyle := [];
    Result.Color := PreviewDefaultTextColor;
    Result.HasExplicitColor := False;
    Result.BackgroundColor := clNone;
    Result.HasBackground := False;
    Result.ScriptPosition := tpoNormal;
    Result.Href := '';
    Result.Align := phaLeft;
    Result.LeftPadding := 0;
    Result.Preformatted := False;
  end;
end;

procedure TPropsDocumentBuilder.PushStyle(const AState: TPropTextStyleState);
var
  N: Integer;
begin
  N := Length(FStyleStack);
  SetLength(FStyleStack, N + 1);
  FStyleStack[N] := AState;
end;

procedure TPropsDocumentBuilder.PushInheritedStyle;
begin
  PushStyle(CurrentStyle);
end;

procedure TPropsDocumentBuilder.PopStyle;
begin
  if Length(FStyleStack) > 1 then
    SetLength(FStyleStack, Length(FStyleStack) - 1);
end;

procedure TPropsDocumentBuilder.ResetParagraphMetrics;
var
  Cur: TPropTextStyleState;
begin
  Cur := CurrentStyle;
  FParaAlign := Cur.Align;
  FParaFirstIndent := 0;
  FParaLeftPadding := Cur.LeftPadding;
  FParaTopPadding := 0;
  FParaBottomPadding := 0;
  FLastCharWasSpace := False;
end;

procedure TPropsDocumentBuilder.BeginParagraphIfNeeded;
var
  Cur: TPropTextStyleState;
begin
  if not FHasBlocksInPara then
  begin
    Cur := CurrentStyle;
    if FParaAlign = phaLeft then
      FParaAlign := Cur.Align;
    if FParaLeftPadding = 0 then
      FParaLeftPadding := Cur.LeftPadding;
  end;
end;

procedure TPropsDocumentBuilder.FlushParagraph(ABottomPadding: Integer);
var
  Para: TKMemoParagraph;
  BottomPad: Integer;
begin
  if ABottomPadding >= 0 then
    BottomPad := ABottomPadding
  else
    BottomPad := FParaBottomPadding;
  Para := FMemo.Blocks.AddParagraph;
  case FParaAlign of
    phaCenter: Para.ParaStyle.HAlign := halCenter;
    phaRight: Para.ParaStyle.HAlign := halRight;
  else
    Para.ParaStyle.HAlign := halLeft;
  end;
  Para.ParaStyle.FirstIndent := FParaFirstIndent;
  Para.ParaStyle.LeftPadding := FParaLeftPadding;
  Para.ParaStyle.TopPadding := FParaTopPadding;
  Para.ParaStyle.BottomPadding := BottomPad;
  FHasBlocksInPara := False;
  ResetParagraphMetrics;
end;

procedure TPropsDocumentBuilder.EmitStyledText(const AStr: string;
  const AStyle: TPropTextStyleState);
var
  TextBlock: TKMemoTextBlock;
  LinkBlock: TKMemoHyperlink;
  EffectiveColor: TColor;

  procedure ApplyCommonTextStyle(AOutStyle: TKMemoTextStyle; IsLink: Boolean);
  begin
    AOutStyle.Font.Name := AStyle.FontName;
    AOutStyle.Font.Size := Max(7, AStyle.FontSize);
    if IsLink then
      AOutStyle.Font.Style := AStyle.FontStyle + [fsUnderline]
    else
      AOutStyle.Font.Style := AStyle.FontStyle;
    EffectiveColor := AStyle.Color;
    if IsLink and not AStyle.HasExplicitColor then
      EffectiveColor := PreviewLinkColor;
    if AStyle.HasBackground then
    begin
      AOutStyle.AllowBrush := True;
      AOutStyle.Brush.Style := bsSolid;
      AOutStyle.Brush.Color := AStyle.BackgroundColor;
      if not AStyle.HasExplicitColor and not IsLink then
      begin
        if ColorLuminance(AStyle.BackgroundColor) > 140 then
          EffectiveColor := clBlack
        else
          EffectiveColor := clWhite;
      end;
    end
    else
    begin
      AOutStyle.AllowBrush := False;
      AOutStyle.Brush.Style := bsClear;
    end;
    AOutStyle.Font.Color := EffectiveColor;
    AOutStyle.ScriptPosition := AStyle.ScriptPosition;
  end;

begin
  if AStr = '' then
    Exit;
  BeginParagraphIfNeeded;
  if (AStyle.Href <> '') and IsSafeWebHref(AStyle.Href) then
  begin
    LinkBlock := FMemo.Blocks.AddHyperlink(AStr, Trim(AStyle.Href));
    LinkBlock.OnClick := @FMemo.HandleHyperlinkClick;
    ApplyCommonTextStyle(LinkBlock.TextStyle, True);
  end
  else
  begin
    TextBlock := FMemo.Blocks.AddTextBlock(AStr);
    ApplyCommonTextStyle(TextBlock.TextStyle, False);
  end;
  FHasBlocksInPara := True;
  FLastCharWasSpace := AStr[Length(AStr)] = ' ';
end;

procedure TPropsDocumentBuilder.EmitCollapsedText(const AStr: string);
var
  Cur: TPropTextStyleState;
  Decoded, OutBuf: string;
  I, Len, StartPos: Integer;
begin
  if AStr = '' then
    Exit;
  Decoded := DecodeHtmlEntities(AStr);
  if Decoded = '' then
    Exit;
  Cur := CurrentStyle;

  if Cur.Preformatted then
  begin
    I := 1;
    Len := Length(Decoded);
    while I <= Len do
    begin
      if Decoded[I] in [#13, #10] then
      begin
        if (Decoded[I] = #13) and (I + 1 <= Len) and (Decoded[I + 1] = #10) then
          Inc(I);
        FlushParagraph(0);
        Inc(I);
        Continue;
      end;
      StartPos := I;
      while (I <= Len) and not (Decoded[I] in [#13, #10]) do
        Inc(I);
      EmitStyledText(Copy(Decoded, StartPos, I - StartPos), Cur);
    end;
    Exit;
  end;

  OutBuf := '';
  I := 1;
  Len := Length(Decoded);
  while I <= Len do
  begin
    if Decoded[I] in [' ', #9, #10, #13] then
    begin
      while (I <= Len) and (Decoded[I] in [' ', #9, #10, #13]) do
        Inc(I);
      if (FHasBlocksInPara or (OutBuf <> '')) and not FLastCharWasSpace then
      begin
        OutBuf := OutBuf + ' ';
        FLastCharWasSpace := True;
      end;
    end
    else
    begin
      StartPos := I;
      while (I <= Len) and not (Decoded[I] in [' ', #9, #10, #13]) do
        Inc(I);
      OutBuf := OutBuf + Copy(Decoded, StartPos, I - StartPos);
      FLastCharWasSpace := False;
    end;
  end;
  if OutBuf <> '' then
    EmitStyledText(OutBuf, Cur);
end;

procedure TPropsDocumentBuilder.ApplyInlineCss(const ACssText: string;
  var AState: TPropTextStyleState; AIsBlockTag: Boolean);
var
  Decls: TStringList;
  I, ColonPos, BangPos, PxVal: Integer;
  Decl, PropName, PropVal, LowerVal: string;
  ParsedColor: TColor;
begin
  if Trim(ACssText) = '' then
    Exit;
  Decls := TStringList.Create;
  try
    Decls.StrictDelimiter := True;
    Decls.Delimiter := ';';
    Decls.DelimitedText := ACssText;
    for I := 0 to Decls.Count - 1 do
    begin
      Decl := Trim(Decls[I]);
      ColonPos := Pos(':', Decl);
      if ColonPos <= 1 then
        Continue;
      PropName := LowerCase(Trim(Copy(Decl, 1, ColonPos - 1)));
      PropVal := Trim(Copy(Decl, ColonPos + 1, MaxInt));
      BangPos := Pos('!important', LowerCase(PropVal));
      if BangPos > 0 then
        PropVal := Trim(Copy(PropVal, 1, BangPos - 1));
      LowerVal := LowerCase(PropVal);

      if PropName = 'color' then
      begin
        if TryParseCssColor(PropVal, ParsedColor) then
        begin
          AState.Color := AdaptHtmlColorForSkin(ParsedColor);
          AState.HasExplicitColor := True;
        end;
      end
      else if (PropName = 'background-color') or (PropName = 'background') then
      begin
        if TryParseCssColor(PropVal, ParsedColor) then
        begin
          AState.BackgroundColor := ParsedColor;
          AState.HasBackground := True;
        end;
      end
      else if PropName = 'font-weight' then
      begin
        if (LowerVal = 'bold') or (LowerVal = 'bolder') or
           (StrToIntDef(LowerVal, 0) >= 600) then
          Include(AState.FontStyle, fsBold)
        else if LowerVal = 'normal' then
          Exclude(AState.FontStyle, fsBold);
      end
      else if PropName = 'font-style' then
      begin
        if (LowerVal = 'italic') or (LowerVal = 'oblique') then
          Include(AState.FontStyle, fsItalic)
        else if LowerVal = 'normal' then
          Exclude(AState.FontStyle, fsItalic);
      end
      else if PropName = 'text-decoration' then
      begin
        if Pos('underline', LowerVal) > 0 then
          Include(AState.FontStyle, fsUnderline);
        if Pos('line-through', LowerVal) > 0 then
          Include(AState.FontStyle, fsStrikeOut);
        if LowerVal = 'none' then
          AState.FontStyle := AState.FontStyle - [fsUnderline, fsStrikeOut];
      end
      else if PropName = 'font-size' then
      begin
        if TryParseCssPixelLength(PropVal, FBaseFontSize, PxVal) and (PxVal > 0) then
          AState.FontSize := EnsureRange(Round(PxVal * 72.0 / 96.0), 7, 28);
      end
      else if PropName = 'font-family' then
      begin
        ColonPos := Pos(',', PropVal);
        if ColonPos > 0 then
          PropVal := Trim(Copy(PropVal, 1, ColonPos - 1));
        PropVal := Trim(StringReplace(StringReplace(PropVal, '"', '', [rfReplaceAll]),
          '''', '', [rfReplaceAll]));
        if (PropVal <> '') and (LowerCase(PropVal) <> 'inherit') then
          AState.FontName := PropVal;
      end
      else if AIsBlockTag and (PropName = 'text-align') then
      begin
        if LowerVal = 'center' then
        begin
          AState.Align := phaCenter;
          FParaAlign := phaCenter;
        end
        else if LowerVal = 'right' then
        begin
          AState.Align := phaRight;
          FParaAlign := phaRight;
        end
        else if (LowerVal = 'left') or (LowerVal = 'justify') then
        begin
          AState.Align := phaLeft;
          FParaAlign := phaLeft;
        end;
      end
      else if AIsBlockTag and (PropName = 'text-indent') then
      begin
        if TryParseCssPixelLength(PropVal, FBaseFontSize, PxVal) then
          FParaFirstIndent := Max(0, UIScale(FMemo, PxVal));
      end
      else if AIsBlockTag and ((PropName = 'margin-left') or (PropName = 'padding-left')) then
      begin
        if TryParseCssPixelLength(PropVal, FBaseFontSize, PxVal) then
        begin
          AState.LeftPadding := AState.LeftPadding + Max(0, UIScale(FMemo, PxVal));
          FParaLeftPadding := AState.LeftPadding;
        end;
      end;
    end;
  finally
    Decls.Free;
  end;
end;

procedure TPropsDocumentBuilder.ApplyFontTagAttributes(AAttrNames,
  AAttrValues: TStringList; var AState: TPropTextStyleState);
var
  ColorVal, SizeVal, FaceVal: string;
  ParsedColor: TColor;
  SizeNum: Integer;
begin
  ColorVal := GetTagAttribute(AAttrNames, AAttrValues, 'color');
  if (ColorVal <> '') and TryParseCssColor(ColorVal, ParsedColor) then
  begin
    AState.Color := AdaptHtmlColorForSkin(ParsedColor);
    AState.HasExplicitColor := True;
  end;

  SizeVal := Trim(GetTagAttribute(AAttrNames, AAttrValues, 'size'));
  if SizeVal <> '' then
  begin
    if TryStrToInt(SizeVal, SizeNum) then
    begin
      if SizeVal[1] in ['+', '-'] then
        AState.FontSize := EnsureRange(FBaseFontSize + SizeNum * 2, 7, 26)
      else
      begin
        case EnsureRange(SizeNum, 1, 7) of
          1: AState.FontSize := Max(7, FBaseFontSize - 3);
          2: AState.FontSize := Max(8, FBaseFontSize - 1);
          3: AState.FontSize := FBaseFontSize;
          4: AState.FontSize := FBaseFontSize + 2;
          5: AState.FontSize := FBaseFontSize + 4;
          6: AState.FontSize := FBaseFontSize + 7;
          7: AState.FontSize := FBaseFontSize + 10;
        end;
      end;
    end;
  end;

  FaceVal := Trim(GetTagAttribute(AAttrNames, AAttrValues, 'face'));
  if FaceVal <> '' then
  begin
    if Pos(',', FaceVal) > 0 then
      FaceVal := Trim(Copy(FaceVal, 1, Pos(',', FaceVal) - 1));
    if FaceVal <> '' then
      AState.FontName := FaceVal;
  end;
end;

procedure TPropsDocumentBuilder.EmitHorizontalRule;
var
  RuleStr: string;
  I: Integer;
  Style: TPropTextStyleState;
begin
  if FHasBlocksInPara then
    FlushParagraph;
  RuleStr := '';
  for I := 1 to 40 do
    RuleStr := RuleStr + UnicodeToUTF8($2500);
  Style := CurrentStyle;
  Style.FontName := UIDefaultFontName;
  Style.FontSize := Max(8, FBaseFontSize - 1);
  Style.FontStyle := [];
  Style.Color := PreviewMutedColor;
  Style.HasExplicitColor := True;
  Style.HasBackground := False;
  Style.Href := '';
  FParaTopPadding := UIScale(FMemo, 4);
  FParaBottomPadding := UIScale(FMemo, 4);
  FParaFirstIndent := 0;
  EmitStyledText(RuleStr, Style);
  FlushParagraph(UIScale(FMemo, 4));
end;

procedure TPropsDocumentBuilder.RenderPlainLine(const AStr: string;
  AColor: TColor; ABold, AItalic, AMono: Boolean; ABottomPadding: Integer;
  ATopPadding: Integer; AFontSizeDelta: Integer);
var
  Style: TPropTextStyleState;
begin
  if FHasBlocksInPara then
    FlushParagraph;
  Style := CurrentStyle;
  if AMono then
    Style.FontName := UIMonoFontName
  else
    Style.FontName := UIDefaultFontName;
  Style.FontSize := Max(7, FBaseFontSize + AFontSizeDelta);
  Style.FontStyle := [];
  if ABold then
    Include(Style.FontStyle, fsBold);
  if AItalic then
    Include(Style.FontStyle, fsItalic);
  Style.Color := AColor;
  Style.HasExplicitColor := True;
  Style.HasBackground := False;
  Style.Href := '';
  FParaFirstIndent := 0;
  FParaLeftPadding := 0;
  FParaTopPadding := UIScale(FMemo, ATopPadding);
  EmitStyledText(AStr, Style);
  FlushParagraph(UIScale(FMemo, ABottomPadding));
end;

procedure TPropsDocumentBuilder.RenderMetadataRow(const AKey, AValue: string);
var
  KeyStyle, ValStyle: TPropTextStyleState;
  CleanVal: string;
  SubLines: TStringList;
  I: Integer;
begin
  if FHasBlocksInPara then
    FlushParagraph;

  KeyStyle := CurrentStyle;
  KeyStyle.FontName := UIDefaultFontName;
  KeyStyle.FontSize := FBaseFontSize;
  KeyStyle.FontStyle := [fsBold];
  KeyStyle.Color := PreviewMutedColor;
  KeyStyle.HasExplicitColor := True;
  KeyStyle.HasBackground := False;
  KeyStyle.Href := '';
  EmitStyledText(AKey + ': ', KeyStyle);

  CleanVal := Trim(AValue);
  if (CleanVal <> '') and (Pos(#10, CleanVal) = 0) and (Pos(' ', CleanVal) = 0) and
     IsSafeWebHref(CleanVal) then
  begin
    ValStyle := CurrentStyle;
    ValStyle.Href := CleanVal;
    ValStyle.HasExplicitColor := False;
    EmitStyledText(CleanVal, ValStyle);
  end
  else if LooksLikeHtml(AValue) then
  begin
    RenderHtmlFragment(AValue, False);
  end
  else
  begin
    SubLines := TStringList.Create;
    try
      SubLines.Text := DecodeHtmlEntities(AValue);
      for I := 0 to SubLines.Count - 1 do
      begin
        if I > 0 then
          FlushParagraph(0);
        if Trim(SubLines[I]) <> '' then
          EmitStyledText(Trim(SubLines[I]), CurrentStyle);
      end;
    finally
      SubLines.Free;
    end;
  end;

  if FHasBlocksInPara then
    FlushParagraph(UIScale(FMemo, 2));
end;

procedure TPropsDocumentBuilder.RenderHtmlFragment(const AHtml: string;
  ATrailingBreak: Boolean);
var
  I, Len, TagEnd, ClosePos, N, SpacePos: Integer;
  Inner, TagName, AttrStr, AlignAttr, StyleAttr, HrefAttr, Prefix: string;
  IsClosing: Boolean;
  AttrNames, AttrValues: TStringList;
  NextState: TPropTextStyleState;
  NewLevel: TPropListLevel;

  function FindMatchingTagEnd(StartIndex: Integer): Integer;
  var
    J: Integer;
    Q: Char;
  begin
    J := StartIndex + 1;
    Q := #0;
    while J <= Len do
    begin
      if Q <> #0 then
      begin
        if AHtml[J] = Q then
          Q := #0;
      end
      else
      begin
        if AHtml[J] in ['"', ''''] then
          Q := AHtml[J]
        else if AHtml[J] = '>' then
          Exit(J)
        else if AHtml[J] = '<' then
          Exit(-1);
      end;
      Inc(J);
    end;
    Result := -1;
  end;

  procedure ApplyAlignAttribute(const AAlign: string;
    var AState: TPropTextStyleState);
  var
    L: string;
  begin
    L := LowerCase(Trim(AAlign));
    if L = 'center' then
    begin
      AState.Align := phaCenter;
      FParaAlign := phaCenter;
    end
    else if L = 'right' then
    begin
      AState.Align := phaRight;
      FParaAlign := phaRight;
    end
    else if (L = 'left') or (L = 'justify') then
    begin
      AState.Align := phaLeft;
      FParaAlign := phaLeft;
    end;
  end;

  procedure EnsureParagraphFlushBeforeBlock;
  begin
    if FHasBlocksInPara then
      FlushParagraph;
  end;

  procedure CloseBlockIfOpen(ABottomPadding: Integer = -1);
  begin
    if FHasBlocksInPara then
      FlushParagraph(ABottomPadding)
    else
      ResetParagraphMetrics;
  end;

begin
  I := 1;
  Len := Length(AHtml);
  while I <= Len do
  begin
    if AHtml[I] = '<' then
    begin
      if Copy(AHtml, I, 4) = '<!--' then
      begin
        ClosePos := PosEx('-->', AHtml, I + 4);
        if ClosePos > 0 then
          I := ClosePos + 3
        else
          I := Len + 1;
        Continue;
      end;

      if (I + 1 > Len) or not (AHtml[I + 1] in ['/', '!', 'a'..'z', 'A'..'Z']) then
      begin
        EmitCollapsedText('<');
        Inc(I);
        Continue;
      end;

      TagEnd := FindMatchingTagEnd(I);
      if TagEnd < 0 then
      begin
        EmitCollapsedText('<');
        Inc(I);
        Continue;
      end;

      Inner := Trim(Copy(AHtml, I + 1, TagEnd - I - 1));
      I := TagEnd + 1;
      if Inner = '' then
        Continue;
      if Inner[1] = '!' then
        Continue;

      IsClosing := Inner[1] = '/';
      if IsClosing then
        Delete(Inner, 1, 1);
      if (Inner <> '') and (Inner[Length(Inner)] = '/') then
        Delete(Inner, Length(Inner), 1);
      Inner := Trim(Inner);
      if Inner = '' then
        Continue;

      SpacePos := 1;
      while (SpacePos <= Length(Inner)) and
            not (Inner[SpacePos] in [' ', #9, #10, #13]) do
        Inc(SpacePos);
      TagName := LowerCase(Copy(Inner, 1, SpacePos - 1));
      AttrStr := Copy(Inner, SpacePos, MaxInt);

      if not IsClosing and ((TagName = 'script') or (TagName = 'style') or
         (TagName = 'head')) then
      begin
        ClosePos := PosEx('</' + TagName, LowerCase(AHtml), I);
        if ClosePos > 0 then
        begin
          ClosePos := PosEx('>', AHtml, ClosePos);
          if ClosePos > 0 then
            I := ClosePos + 1
          else
            I := Len + 1;
        end
        else
          I := Len + 1;
        Continue;
      end;

      AttrNames := nil;
      AttrValues := nil;
      try
        if not IsClosing and (Trim(AttrStr) <> '') then
          ExtractTagAttributes(AttrStr, AttrNames, AttrValues);

        if TagName = 'br' then
        begin
          FlushParagraph(0);
        end
        else if TagName = 'hr' then
        begin
          EmitHorizontalRule;
        end
        else if (TagName = 'h1') or (TagName = 'h2') or (TagName = 'h3') or
                (TagName = 'h4') or (TagName = 'h5') or (TagName = 'h6') then
        begin
          if IsClosing then
          begin
            CloseBlockIfOpen;
            PopStyle;
          end
          else
          begin
            EnsureParagraphFlushBeforeBlock;
            NextState := CurrentStyle;
            Include(NextState.FontStyle, fsBold);
            case TagName[2] of
              '1': NextState.FontSize := FBaseFontSize + 6;
              '2': NextState.FontSize := FBaseFontSize + 4;
              '3': NextState.FontSize := FBaseFontSize + 2;
              '4': NextState.FontSize := FBaseFontSize + 1;
              '5': NextState.FontSize := FBaseFontSize;
              '6': NextState.FontSize := Max(7, FBaseFontSize - 1);
            end;
            AlignAttr := GetTagAttribute(AttrNames, AttrValues, 'align');
            StyleAttr := GetTagAttribute(AttrNames, AttrValues, 'style');
            ApplyAlignAttribute(AlignAttr, NextState);
            ApplyInlineCss(StyleAttr, NextState, True);
            PushStyle(NextState);
            FParaTopPadding := UIScale(FMemo, 6);
            FParaBottomPadding := UIScale(FMemo, 4);
          end;
        end
        else if TagName = 'center' then
        begin
          if IsClosing then
          begin
            CloseBlockIfOpen;
            PopStyle;
            FParaAlign := CurrentStyle.Align;
          end
          else
          begin
            EnsureParagraphFlushBeforeBlock;
            NextState := CurrentStyle;
            NextState.Align := phaCenter;
            PushStyle(NextState);
            FParaAlign := phaCenter;
          end;
        end
        else if (TagName = 'p') or (TagName = 'div') then
        begin
          if IsClosing then
          begin
            CloseBlockIfOpen;
            PopStyle;
          end
          else
          begin
            EnsureParagraphFlushBeforeBlock;
            NextState := CurrentStyle;
            AlignAttr := GetTagAttribute(AttrNames, AttrValues, 'align');
            StyleAttr := GetTagAttribute(AttrNames, AttrValues, 'style');
            ApplyAlignAttribute(AlignAttr, NextState);
            ApplyInlineCss(StyleAttr, NextState, True);
            PushStyle(NextState);
            if TagName = 'p' then
              FParaBottomPadding := UIScale(FMemo, 6);
          end;
        end
        else if TagName = 'blockquote' then
        begin
          if IsClosing then
          begin
            CloseBlockIfOpen;
            PopStyle;
            FParaLeftPadding := CurrentStyle.LeftPadding;
          end
          else
          begin
            EnsureParagraphFlushBeforeBlock;
            NextState := CurrentStyle;
            NextState.LeftPadding := NextState.LeftPadding + UIScale(FMemo, 20);
            PushStyle(NextState);
            FParaLeftPadding := NextState.LeftPadding;
            FParaBottomPadding := UIScale(FMemo, 4);
          end;
        end
        else if (TagName = 'ol') or (TagName = 'ul') then
        begin
          if IsClosing then
          begin
            CloseBlockIfOpen;
            if Length(FListStack) > 0 then
              SetLength(FListStack, Length(FListStack) - 1);
            PopStyle;
            FParaLeftPadding := CurrentStyle.LeftPadding;
          end
          else
          begin
            EnsureParagraphFlushBeforeBlock;
            N := Length(FListStack);
            if TagName = 'ol' then
              NewLevel.Kind := plkOrdered
            else
              NewLevel.Kind := plkUnordered;
            NewLevel.Counter := 0;
            SetLength(FListStack, N + 1);
            FListStack[N] := NewLevel;
            NextState := CurrentStyle;
            NextState.LeftPadding := NextState.LeftPadding + UIScale(FMemo, 18);
            PushStyle(NextState);
            FParaLeftPadding := NextState.LeftPadding;
          end;
        end
        else if TagName = 'li' then
        begin
          if IsClosing then
          begin
            CloseBlockIfOpen(UIScale(FMemo, 2));
          end
          else
          begin
            EnsureParagraphFlushBeforeBlock;
            FParaFirstIndent := 0;
            FParaBottomPadding := UIScale(FMemo, 2);
            if Length(FListStack) > 0 then
            begin
              N := High(FListStack);
              if FListStack[N].Kind = plkOrdered then
              begin
                Inc(FListStack[N].Counter);
                Prefix := IntToStr(FListStack[N].Counter) + '. ';
              end
              else
                Prefix := UnicodeToUTF8($2022) + ' ';
            end
            else
              Prefix := UnicodeToUTF8($2022) + ' ';
            EmitStyledText(Prefix, CurrentStyle);
            FLastCharWasSpace := True;
          end;
        end
        else if (TagName = 'b') or (TagName = 'strong') then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            Include(NextState.FontStyle, fsBold);
            PushStyle(NextState);
          end;
        end
        else if (TagName = 'i') or (TagName = 'em') or (TagName = 'cite') then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            Include(NextState.FontStyle, fsItalic);
            PushStyle(NextState);
          end;
        end
        else if (TagName = 'u') or (TagName = 'ins') then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            Include(NextState.FontStyle, fsUnderline);
            PushStyle(NextState);
          end;
        end
        else if (TagName = 's') or (TagName = 'strike') or (TagName = 'del') then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            Include(NextState.FontStyle, fsStrikeOut);
            PushStyle(NextState);
          end;
        end
        else if TagName = 'sup' then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            NextState.ScriptPosition := tpoSuperscript;
            PushStyle(NextState);
          end;
        end
        else if TagName = 'sub' then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            NextState.ScriptPosition := tpoSubscript;
            PushStyle(NextState);
          end;
        end
        else if TagName = 'small' then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            NextState.FontSize := Max(7, NextState.FontSize - 2);
            PushStyle(NextState);
          end;
        end
        else if TagName = 'big' then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            NextState.FontSize := Min(28, NextState.FontSize + 2);
            PushStyle(NextState);
          end;
        end
        else if (TagName = 'code') or (TagName = 'tt') or (TagName = 'pre') then
        begin
          if IsClosing then
          begin
            if TagName = 'pre' then
              CloseBlockIfOpen;
            PopStyle;
          end
          else
          begin
            if TagName = 'pre' then
              EnsureParagraphFlushBeforeBlock;
            NextState := CurrentStyle;
            NextState.FontName := UIMonoFontName;
            NextState.Preformatted := NextState.Preformatted or (TagName = 'pre');
            PushStyle(NextState);
          end;
        end
        else if TagName = 'font' then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            ApplyFontTagAttributes(AttrNames, AttrValues, NextState);
            StyleAttr := GetTagAttribute(AttrNames, AttrValues, 'style');
            ApplyInlineCss(StyleAttr, NextState, False);
            PushStyle(NextState);
          end;
        end
        else if TagName = 'span' then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            StyleAttr := GetTagAttribute(AttrNames, AttrValues, 'style');
            ApplyInlineCss(StyleAttr, NextState, False);
            PushStyle(NextState);
          end;
        end
        else if TagName = 'a' then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            NextState := CurrentStyle;
            HrefAttr := Trim(GetTagAttribute(AttrNames, AttrValues, 'href'));
            if HrefAttr <> '' then
              NextState.Href := HrefAttr;
            StyleAttr := GetTagAttribute(AttrNames, AttrValues, 'style');
            ApplyInlineCss(StyleAttr, NextState, False);
            PushStyle(NextState);
          end;
        end
        else if TagName = 'tr' then
        begin
          if IsClosing then
            CloseBlockIfOpen(UIScale(FMemo, 2))
          else
            EnsureParagraphFlushBeforeBlock;
        end
        else if (TagName = 'td') or (TagName = 'th') then
        begin
          if IsClosing then
            PopStyle
          else
          begin
            if FHasBlocksInPara then
              EmitStyledText('  |  ', CurrentStyle);
            NextState := CurrentStyle;
            if TagName = 'th' then
              Include(NextState.FontStyle, fsBold);
            PushStyle(NextState);
          end;
        end
        else if (TagName = 'table') or (TagName = 'tbody') or
                (TagName = 'thead') then
        begin
          if IsClosing then
            CloseBlockIfOpen
          else
            EnsureParagraphFlushBeforeBlock;
        end;
      finally
        AttrNames.Free;
        AttrValues.Free;
      end;
      Continue;
    end;

    ClosePos := PosEx('<', AHtml, I);
    if ClosePos = 0 then
    begin
      EmitCollapsedText(Copy(AHtml, I, MaxInt));
      Break;
    end
    else
    begin
      EmitCollapsedText(Copy(AHtml, I, ClosePos - I));
      I := ClosePos;
    end;
  end;

  if ATrailingBreak and FHasBlocksInPara then
    FlushParagraph;
end;

procedure TPropsDocumentBuilder.RenderRtfFragment(const ARtf: string);
var
  I, Len, ParamInt, SkipGroupDepth, GroupDepth, HexVal, UcCount,
    SkipAfterUnicode: Integer;
  Cmd, ParamStr, PlainBuf: string;
  CurState: TPropTextStyleState;
  Stack: array of TPropTextStyleState;
  UcStack: array of Integer;

  procedure FlushBuf;
  begin
    if PlainBuf <> '' then
    begin
      EmitStyledText(PlainBuf, CurState);
      PlainBuf := '';
    end;
  end;

  procedure PushRtfGroup;
  var
    N: Integer;
  begin
    N := Length(Stack);
    SetLength(Stack, N + 1);
    Stack[N] := CurState;
    SetLength(UcStack, N + 1);
    UcStack[N] := UcCount;
    Inc(GroupDepth);
  end;

  procedure PopRtfGroup;
  var
    N: Integer;
  begin
    if GroupDepth > 0 then
      Dec(GroupDepth);
    if (SkipGroupDepth >= 0) and (GroupDepth < SkipGroupDepth) then
      SkipGroupDepth := -1;
    N := Length(Stack);
    if N > 0 then
    begin
      CurState := Stack[N - 1];
      UcCount := UcStack[N - 1];
      SetLength(Stack, N - 1);
      SetLength(UcStack, N - 1);
    end;
  end;

  function IsSkipDestination(const AWord: string): Boolean;
  begin
    Result := (AWord = 'fonttbl') or (AWord = 'colortbl') or
      (AWord = 'stylesheet') or (AWord = 'info') or (AWord = 'pict') or
      (AWord = 'object') or (AWord = 'header') or (AWord = 'footer') or
      (AWord = 'generator');
  end;

begin
  CurState := CurrentStyle;
  SetLength(Stack, 0);
  SetLength(UcStack, 0);
  GroupDepth := 0;
  SkipGroupDepth := -1;
  UcCount := 1;
  SkipAfterUnicode := 0;
  PlainBuf := '';

  I := 1;
  Len := Length(ARtf);
  while I <= Len do
  begin
    case ARtf[I] of
      '{':
        begin
          FlushBuf;
          PushRtfGroup;
          Inc(I);
        end;
      '}':
        begin
          FlushBuf;
          PopRtfGroup;
          Inc(I);
        end;
      #10, #13:
        Inc(I);
      '\':
        begin
          Inc(I);
          if I > Len then
            Break;
          if ARtf[I] = '*' then
          begin
            if SkipGroupDepth < 0 then
              SkipGroupDepth := GroupDepth;
            Inc(I);
          end
          else if ARtf[I] in ['\', '{', '}'] then
          begin
            if SkipGroupDepth < 0 then
            begin
              if SkipAfterUnicode > 0 then
                Dec(SkipAfterUnicode)
              else
                PlainBuf := PlainBuf + ARtf[I];
            end;
            Inc(I);
          end
          else if ARtf[I] = '''' then
          begin
            if (I + 2 <= Len) and TryStrToInt('$' + Copy(ARtf, I + 1, 2), HexVal) then
            begin
              if SkipGroupDepth < 0 then
              begin
                if SkipAfterUnicode > 0 then
                  Dec(SkipAfterUnicode)
                else
                  PlainBuf := PlainBuf + Cp1252ByteToUtf8(Byte(HexVal));
              end;
              Inc(I, 3);
            end
            else
              Inc(I);
          end
          else if ARtf[I] = '~' then
          begin
            if SkipGroupDepth < 0 then
              PlainBuf := PlainBuf + ' ';
            Inc(I);
          end
          else if (ARtf[I] = '-') or (ARtf[I] = '_') then
          begin
            if (ARtf[I] = '_') and (SkipGroupDepth < 0) then
              PlainBuf := PlainBuf + '-';
            Inc(I);
          end
          else if ARtf[I] in ['a'..'z', 'A'..'Z'] then
          begin
            Cmd := '';
            while (I <= Len) and (ARtf[I] in ['a'..'z', 'A'..'Z']) do
            begin
              Cmd := Cmd + LowerCase(ARtf[I]);
              Inc(I);
            end;
            ParamStr := '';
            if (I <= Len) and (ARtf[I] = '-') then
            begin
              ParamStr := '-';
              Inc(I);
            end;
            while (I <= Len) and (ARtf[I] in ['0'..'9']) do
            begin
              ParamStr := ParamStr + ARtf[I];
              Inc(I);
            end;
            if (I <= Len) and (ARtf[I] = ' ') then
              Inc(I);

            if SkipGroupDepth < 0 then
            begin
              if IsSkipDestination(Cmd) then
              begin
                SkipGroupDepth := GroupDepth;
              end
              else if (Cmd = 'par') or (Cmd = 'line') then
              begin
                FlushBuf;
                FlushParagraph(UIScale(FMemo, 4));
              end
              else if Cmd = 'tab' then
              begin
                PlainBuf := PlainBuf + '    ';
              end
              else if Cmd = 'b' then
              begin
                FlushBuf;
                if ParamStr = '0' then
                  Exclude(CurState.FontStyle, fsBold)
                else
                  Include(CurState.FontStyle, fsBold);
              end
              else if Cmd = 'i' then
              begin
                FlushBuf;
                if ParamStr = '0' then
                  Exclude(CurState.FontStyle, fsItalic)
                else
                  Include(CurState.FontStyle, fsItalic);
              end
              else if (Cmd = 'ul') or (Cmd = 'ulnone') then
              begin
                FlushBuf;
                if (Cmd = 'ulnone') or (ParamStr = '0') then
                  Exclude(CurState.FontStyle, fsUnderline)
                else
                  Include(CurState.FontStyle, fsUnderline);
              end
              else if Cmd = 'uc' then
              begin
                if TryStrToInt(ParamStr, ParamInt) and (ParamInt >= 0) then
                  UcCount := ParamInt;
              end
              else if Cmd = 'u' then
              begin
                if TryStrToInt(ParamStr, ParamInt) then
                begin
                  if ParamInt < 0 then
                    ParamInt := ParamInt + 65536;
                  if (ParamInt > 0) and (ParamInt <= $10FFFF) then
                    PlainBuf := PlainBuf + UnicodeToUTF8(Cardinal(ParamInt));
                  SkipAfterUnicode := UcCount;
                end;
              end;
            end;
          end
          else
            Inc(I);
        end;
    else
      if SkipGroupDepth < 0 then
      begin
        if SkipAfterUnicode > 0 then
          Dec(SkipAfterUnicode)
        else
          PlainBuf := PlainBuf + ARtf[I];
      end;
      Inc(I);
    end;
  end;
  FlushBuf;
  if FHasBlocksInPara then
    FlushParagraph;
end;

procedure TPropsDocumentBuilder.FinalizeDocument;
begin
  if FHasBlocksInPara then
    FlushParagraph;
end;

{ TModulePropertiesPreview }

constructor TModulePropertiesPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := True;
  BorderStyle := bsNone;
  ScrollBars := ssAutoVertical;
  ParentFont := False;
  FBaseFontSize := 10;
  FLastSkinId := -1;
  FHasRendered := False;
  TextStyle.Font.Name := UIDefaultFontName;
  TextStyle.Font.Size := FBaseFontSize;
  OnMouseWheel := @HandleMouseWheel;
  ApplySkinColors;
end;

procedure TModulePropertiesPreview.ApplySkinColors;
begin
  if CurrentSkin.Id = skClassic then
  begin
    Background.Color := clWindow;
    Colors.BkGnd := clWindow;
    Colors.SelBkGnd := clHighlight;
    Colors.SelText := clHighlightText;
    Colors.SelBkGndFocused := clHighlight;
    Colors.SelTextFocused := clHighlightText;
    TextStyle.Font.Color := UITextColor;
  end
  else
  begin
    Background.Color := CurrentSkin.WindowBg;
    Colors.BkGnd := CurrentSkin.WindowBg;
    Colors.SelBkGnd := CurrentSkin.SelectedBg;
    Colors.SelText := CurrentSkin.SelectedText;
    Colors.SelBkGndFocused := CurrentSkin.SelectedBg;
    Colors.SelTextFocused := CurrentSkin.SelectedText;
    TextStyle.Font.Color := CurrentSkin.WindowText;
  end;

  if HandleAllocated then
  begin
    ApplyDarkModeToControl(Self, CurrentSkin.IsDark);
    {$IFDEF WINDOWS}
    HookNCScrollBarControl(Self);
    {$ENDIF}
  end;
  if FLastSkinId <> Ord(CurrentSkin.Id) then
  begin
    FLastSkinId := Ord(CurrentSkin.Id);
    if FHasRendered then
      RenderContent;
  end;
end;

procedure TModulePropertiesPreview.HandleHyperlinkClick(Sender: TObject);
var
  Target: string;
begin
  if not (Sender is TKMemoHyperlink) then
    Exit;
  Target := Trim(string(TKMemoHyperlink(Sender).URL));
  if IsSafeWebHref(Target) then
    OpenURL(Target);
end;

procedure TModulePropertiesPreview.HandleMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  NewSize: Integer;
begin
  if ssCtrl in Shift then
  begin
    Handled := True;
    NewSize := EnsureRange(FBaseFontSize + Sign(WheelDelta), 7, 24);
    if NewSize <> FBaseFontSize then
    begin
      FBaseFontSize := NewSize;
      TextStyle.Font.Size := FBaseFontSize;
      RenderContent;
    end;
  end;
end;

procedure TModulePropertiesPreview.SetPropertiesText(const ARawText: string;
  ForceRender: Boolean);
begin
  if not ForceRender and FHasRendered and (FRawText = ARawText) and
     (FLastSkinId = Ord(CurrentSkin.Id)) then
    Exit;
  FRawText := ARawText;
  FLastSkinId := Ord(CurrentSkin.Id);
  RenderContent;
end;

procedure TModulePropertiesPreview.RenderContent;
var
  Normalized, Line, Key, Val, AboutText: string;
  RawLines, FreeLines, RuleLines, SubLines: TStringList;
  MetaEntries: TPropMetaArray;
  I, EqPos, MetaCount: Integer;
  InAbout: Boolean;
  Builder: TPropsDocumentBuilder;
begin
  Blocks.LockUpdate;
  try
    Blocks.Clear;
    Blocks.DefaultParaStyle.FirstIndent := 0;
    Blocks.DefaultParaStyle.LeftPadding := 0;
    Blocks.DefaultParaStyle.TopPadding := 0;
    Blocks.DefaultParaStyle.BottomPadding := 0;
    Blocks.DefaultParaStyle.HAlign := halLeft;

    Builder := TPropsDocumentBuilder.Create(Self, FBaseFontSize);
    RawLines := TStringList.Create;
    FreeLines := TStringList.Create;
    RuleLines := TStringList.Create;
    try
      if Trim(FRawText) = '' then
      begin
        Builder.RenderPlainLine(SPropsPreviewEmpty, PreviewMutedColor,
          False, True, False, 0);
        Exit;
      end;

      Normalized := StringReplace(FRawText, #13#10, #10, [rfReplaceAll]);
      Normalized := StringReplace(Normalized, #13, #10, [rfReplaceAll]);
      RawLines.Text := Normalized;

      // Join theWord trailing backslash continuations.
      I := 0;
      while I < RawLines.Count do
      begin
        Line := RawLines[I];
        while (Line <> '') and (Line[Length(Line)] = '\') and
              (I + 1 < RawLines.Count) do
        begin
          Delete(Line, Length(Line), 1);
          Line := Line + #10 + RawLines[I + 1];
          RawLines.Delete(I + 1);
        end;
        RawLines[I] := Line;
        Inc(I);
      end;

      SetLength(MetaEntries, 0);
      AboutText := '';
      InAbout := False;

      for I := 0 to RawLines.Count - 1 do
      begin
        Line := RawLines[I];
        if Trim(Line) = '' then
        begin
          if InAbout and (AboutText <> '') then
            AboutText := AboutText + #10;
          Continue;
        end;

        if Copy(TrimLeft(Line), 1, 1) = '#' then
        begin
          InAbout := False;
          RuleLines.Add(Line);
          Continue;
        end;

        EqPos := Pos('=', Line);
        if EqPos > 1 then
        begin
          Key := Trim(Copy(Line, 1, EqPos - 1));
          if IsValidPropertyKey(Key) then
          begin
            Val := TrimLeft(Copy(Line, EqPos + 1, MaxInt));
            if SameText(Key, 'about') then
            begin
              InAbout := True;
              if AboutText <> '' then
                AboutText := AboutText + #10 + Val
              else
                AboutText := Val;
            end
            else if SameText(Key, 'verse.rule') then
            begin
              InAbout := False;
              RuleLines.Add(Line);
            end
            else
            begin
              InAbout := False;
              MetaCount := Length(MetaEntries);
              SetLength(MetaEntries, MetaCount + 1);
              MetaEntries[MetaCount].Key := Key;
              MetaEntries[MetaCount].Value := Val;
            end;
            Continue;
          end;
        end;

        if InAbout then
          AboutText := AboutText + #10 + Line
        else
          FreeLines.Add(Line);
      end;

      for I := 0 to High(MetaEntries) do
        Builder.RenderMetadataRow(MetaEntries[I].Key, MetaEntries[I].Value);

      if Trim(AboutText) <> '' then
      begin
        if Length(MetaEntries) > 0 then
          Builder.RenderPlainLine(SPropsPreviewAboutHeading,
            UIAccentTextColor, True, False, False, 4, 8, 1);
        if AnsiStartsText('{\rtf', TrimLeft(AboutText)) then
          Builder.RenderRtfFragment(AboutText)
        else if LooksLikeHtml(AboutText) then
          Builder.RenderHtmlFragment(AboutText, True)
        else
        begin
          SubLines := TStringList.Create;
          try
            SubLines.Text := DecodeHtmlEntities(AboutText);
            for I := 0 to SubLines.Count - 1 do
              if Trim(SubLines[I]) <> '' then
                Builder.RenderPlainLine(Trim(SubLines[I]),
                  PreviewDefaultTextColor, False, False, False, 4)
              else if I > 0 then
                Builder.RenderPlainLine('', PreviewDefaultTextColor,
                  False, False, False, 2);
          finally
            SubLines.Free;
          end;
        end;
      end;

      if FreeLines.Count > 0 then
      begin
        Normalized := Trim(FreeLines.Text);
        if Normalized <> '' then
        begin
          if LooksLikeHtml(Normalized) then
            Builder.RenderHtmlFragment(Normalized, True)
          else
            for I := 0 to FreeLines.Count - 1 do
              if Trim(FreeLines[I]) <> '' then
                Builder.RenderPlainLine(DecodeHtmlEntities(FreeLines[I]),
                  PreviewDefaultTextColor, False, False, False, 4);
        end;
      end;

      if RuleLines.Count > 0 then
      begin
        if (Length(MetaEntries) > 0) or (Trim(AboutText) <> '') or
           (FreeLines.Count > 0) then
          Builder.RenderPlainLine(SPropsPreviewRulesHeading,
            PreviewMutedColor, True, False, False, 3, 8);
        for I := 0 to RuleLines.Count - 1 do
          Builder.RenderPlainLine(RuleLines[I], PreviewMutedColor,
            False, False, True, 1, 0, -1);
      end;

      Builder.FinalizeDocument;
      Modified := False;
    finally
      RuleLines.Free;
      FreeLines.Free;
      RawLines.Free;
      Builder.Free;
    end;
  finally
    Blocks.UnLockUpdate;
    FHasRendered := True;
    Invalidate;
  end;
end;

end.
