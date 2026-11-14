unit FormDictionaryPopup;

{$mode objfpc}{$H+}

interface

uses
  ui_style, Classes, SysUtils, StrUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs,
  {$IFDEF WINDOWS}Windows, Messages,{$ENDIF}
  ExtCtrls, ComCtrls, Menus, ActnList, StdCtrls, LCLType, LCLIntf, LMessages, RichMemo, LazUTF8, Types, Math,
  unitskins;

type
  { TFrmDictionaryPopup }

  TFrmDictionaryPopup = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    RichMemoStrong: TRichMemo;
    RichMemoMorphology: TRichMemo;
    Splitter1: TSplitter;
    TabControlStrongs: TTabControl;
    TabControlMorphologies: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RichMemoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TabControlMorphologiesChange(Sender: TObject);
    procedure TabControlStrongsChange(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure TabControlMorphologiesResize(Sender: TObject);
  private
    { private declarations }
    FStrongs: TStringList;
    FMorphologies: TStringList;
    FInSizeMove: Boolean;
    FPositioning: Boolean;
    FApplyingLayout: Boolean;
    FDockToMainRightEdge: Boolean;
    FDockStateExplicitlySet: Boolean;
    FLastAnchorPoint: TPoint;
    FShowCursorPos: TPoint;
    FAutoHideTimer: TTimer;
    FScrollbarPaintTimer: TTimer;
    FScrollbarPaintTicks: Integer;
    FHoveringSyntagm: Boolean;
    FCtrlEnteredPopup: Boolean;
    FMorphologyPaneHeight: Integer;
    FPopupWidth: Integer;
    FPopupHeight: Integer;
    FDockRightOffset: Integer;
    FDockTopOffset: Integer;
    FLayoutLoadedFromOpts: Boolean;
    function IsCtrlKeyHeld: Boolean;
    procedure AutoHideTimerTick(Sender: TObject);
    procedure ScrollbarPaintTimerTick(Sender: TObject);
    procedure ClampToMonitorWorkarea(const RefPoint: TPoint);
    procedure SyncDockStateFromMainForm;
    function ComputeMainRightEdgeBounds(out ABoundsRect: TRect): Boolean;
    procedure ApplyMorphologyPaneHeight;
  protected
    procedure DoOnResize; override;
    procedure WndProc(var TheMessage: TLMessage); override;
  public
    { public declarations }
    procedure ApplySkin;
    procedure LoadLayoutFromOpts;
    procedure SaveLayoutToOpts;
    procedure ShowRtf(Memo: TRichMemo; const Rtf: string);
    procedure AddStrong(const strong: string; const definition: string);
    procedure AddMorphology(const morfo: string; const definition: string);
    procedure ShowAt(x: Integer; y: Integer);
    procedure HidePopup;
    procedure NotifySyntagmHoverEnter;
    procedure RequestDeferredHide;
    procedure SetDockToMainRightEdge(AValue: Boolean);
    procedure UpdateRightEdgeDockPosition;
    function IsSizingOrMoving: Boolean;
    function IsPointerInteracting: Boolean;
    function IsCursorInsidePopupWindow: Boolean;
    function RouteMouseWheelAtCursor(Shift: TShiftState; WheelDelta: Integer): Boolean;
    property Strongs: TStringList read FStrongs;
    property Morphologies: TStringList read FMorphologies;
    property DockToMainRightEdge: Boolean read FDockToMainRightEdge write SetDockToMainRightEdge;
  public
    procedure AfterConstruction; override;
  end;

var
  FrmDictionaryPopup: TFrmDictionaryPopup;

function AdaptRtfForSkin(const ARtf: string; const ASkin: TSkinColors): string;

implementation

uses
  formprincipal;

{$IFNDEF FPCUNIT}
{$R *.lfm}
{$ENDIF}

const
  MSG_ENTERSIZEMOVE = $0231;
  MSG_EXITSIZEMOVE  = $0232;

{$IFDEF WINDOWS}
const
  EM_SETTYPOGRAPHYOPTIONS = WM_USER + 202;
  TO_ADVANCEDTYPOGRAPHY   = $0001;
  TO_SIMPLELINEBREAK      = $0002;
  EM_SETEDITSTYLE         = WM_USER + 204;
  SES_EMULATESYSEDIT      = $0001;
  SES_BIDI                = $0004;
  EM_LINESCROLL           = $00B6;
  PM_REMOVE_CONST         = $0001;

var
  FPopupWheelHook: HHOOK;

function PopupWheelGetMsgProc(nCode: LongInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  MsgPtr: PMSG;
  WheelDelta: SmallInt;
  ShiftState: TShiftState;
begin
  if (nCode >= 0) and (wParam = PM_REMOVE_CONST) and (lParam <> 0) then
  begin
    MsgPtr := PMSG(lParam);
    if (MsgPtr^.message = WM_MOUSEWHEEL) and
       Assigned(FrmDictionaryPopup) and FrmDictionaryPopup.Visible and
       FrmDictionaryPopup.IsCursorInsidePopupWindow then
    begin
      WheelDelta := SmallInt((MsgPtr^.wParam shr 16) and $FFFF);
      ShiftState := [];
      if (MsgPtr^.wParam and MK_CONTROL) <> 0 then
        Include(ShiftState, ssCtrl);
      if (MsgPtr^.wParam and MK_SHIFT) <> 0 then
        Include(ShiftState, ssShift);
      if FrmDictionaryPopup.RouteMouseWheelAtCursor(ShiftState, WheelDelta) then
        MsgPtr^.message := WM_NULL;
    end;
  end;
  Result := CallNextHookEx(FPopupWheelHook, nCode, wParam, lParam);
end;

procedure ConfigureRichMemoForHebrewAndTables(Memo: TRichMemo);
begin
  if (Memo = nil) or (not Memo.HandleAllocated) then Exit;
  SendMessage(Memo.Handle, EM_SETTYPOGRAPHYOPTIONS,
    TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY or TO_SIMPLELINEBREAK);
  SendMessage(Memo.Handle, EM_SETEDITSTYLE, SES_BIDI, SES_BIDI);
end;
{$ENDIF}

function IsHebrewCodePoint(ACode: Integer): Boolean;
begin
  Result := ((ACode >= $0591) and (ACode <= $05F4)) or
            ((ACode >= $FB1D) and (ACode <= $FB4F));
end;

function IsHebrewBaseLetter(ACode: Integer): Boolean;
begin
  Result := ((ACode >= $05D0) and (ACode <= $05EA)) or
            ((ACode >= $FB1D) and (ACode <= $FB4F));
end;

function IsHebrewFinalLetter(ACode: Integer): Boolean;
begin
  // Final Kaf (1498), Final Mem (1501), Final Nun (1503), Final Pe (1507), Final Tsadi (1509)
  Result := (ACode = 1498) or (ACode = 1501) or (ACode = 1503) or
            (ACode = 1507) or (ACode = 1509);
end;

function ParseHebrewEscapeAt(const S: string; AIndex: Integer;
  out ACodePoint: Integer; out ANextIndex: Integer): Boolean;
var
  Len, i: Integer;
  NumStr: string;
  HadSpace: Boolean;
begin
  Result := False;
  ACodePoint := 0;
  ANextIndex := AIndex;
  Len := Length(S);
  if (AIndex > Len - 2) or (S[AIndex] <> '\') or (S[AIndex + 1] <> 'u') then
    Exit;
  i := AIndex + 2;
  NumStr := '';
  if (i <= Len) and (S[i] = '-') then
  begin
    NumStr := '-';
    Inc(i);
  end;
  while (i <= Len) and (S[i] in ['0'..'9']) do
  begin
    NumStr := NumStr + S[i];
    Inc(i);
  end;
  if (NumStr = '') or (NumStr = '-') then
    Exit;
  ACodePoint := StrToIntDef(NumStr, 0);
  if ACodePoint < 0 then
    ACodePoint := ACodePoint + 65536;
  if not IsHebrewCodePoint(ACodePoint) then
    Exit;
  HadSpace := False;
  if (i <= Len) and (S[i] = ' ') then
  begin
    HadSpace := True;
    Inc(i);
  end;
  if (i <= Len) and (S[i] = '\') and (i + 3 <= Len) and (S[i + 1] = '''') then
    Inc(i, 4)
  else if (i <= Len) and (S[i] = '?') then
    Inc(i)
  else if (not HadSpace) and (i <= Len) and not (S[i] in ['\', '{', '}']) then
    Inc(i);
  ANextIndex := i;
  Result := True;
end;

function NormalizeRtfTablesAndHebrew(const ARtf: string): string;
var
  S: string;
  sb: TStringBuilder;
  i, Len, NextIdx, CodePt: Integer;
  Cmd, ParamStr: string;
  Clusters: array of string;
  ClusterBase: array of Integer;
  ClusterCount, k, FirstBase, LastBase: Integer;
  NeedsReverse: Boolean;
begin
  Clusters := nil;
  ClusterBase := nil;
  Result := ARtf;
  if Result = '' then Exit;

  // 1. Strip Unicode BiDi Left-to-Right Override (U+202D = \u8237) and PDF (U+202C = \u8236)
  //    that force Hebrew words in uBDB/theWord modules to lay out Left-to-Right
  S := Result;
  S := StringReplace(S, '\u8237?', '', [rfReplaceAll]);
  S := StringReplace(S, '\u8237 ', '', [rfReplaceAll]);
  S := StringReplace(S, '\u-57299?', '', [rfReplaceAll]);
  S := StringReplace(S, '\u8236?', '', [rfReplaceAll]);
  S := StringReplace(S, '\u8236 ', '', [rfReplaceAll]);
  S := StringReplace(S, '\u-57300?', '', [rfReplaceAll]);
  S := StringReplace(S, #$E2#$80#$AD, '', [rfReplaceAll]);
  S := StringReplace(S, #$E2#$80#$AC, '', [rfReplaceAll]);

  // 2. Unwrap RTF table definitions (\trowd...\cellxN, \intbl, \cell, \row) so narrow first
  //    columns (e.g. \cellx140 in uBDB H6019) never wrap definitions 1 character per line,
  //    and fix any visual-order Hebrew runs starting with a final letter (ך/ם/ן/ף/ץ).
  sb := TStringBuilder.Create;
  try
    Len := Length(S);
    i := 1;
    while i <= Len do
    begin
      // Check for a contiguous run of \uN Hebrew escapes
      if ParseHebrewEscapeAt(S, i, CodePt, NextIdx) then
      begin
        ClusterCount := 0;
        SetLength(Clusters, 16);
        SetLength(ClusterBase, 16);
        while ParseHebrewEscapeAt(S, i, CodePt, NextIdx) do
        begin
          if (ClusterCount = 0) or IsHebrewBaseLetter(CodePt) then
          begin
            if ClusterCount >= Length(Clusters) then
            begin
              SetLength(Clusters, Length(Clusters) * 2);
              SetLength(ClusterBase, Length(ClusterBase) * 2);
            end;
            Clusters[ClusterCount] := Copy(S, i, NextIdx - i);
            ClusterBase[ClusterCount] := CodePt;
            Inc(ClusterCount);
          end
          else
          begin
            // Combining vowel/cantillation point attaches to its preceding base consonant
            Clusters[ClusterCount - 1] := Clusters[ClusterCount - 1] + Copy(S, i, NextIdx - i);
          end;
          i := NextIdx;
        end;

        FirstBase := 0;
        LastBase := 0;
        for k := 0 to ClusterCount - 1 do
          if IsHebrewBaseLetter(ClusterBase[k]) then
          begin
            if FirstBase = 0 then FirstBase := ClusterBase[k];
            LastBase := ClusterBase[k];
          end;

        // If a multi-cluster Hebrew word begins with a final-form letter (ך/ם/ן/ף/ץ)
        // and does not end with one, it is stored in visual (reversed) order.
        NeedsReverse := (ClusterCount >= 2) and
                        IsHebrewFinalLetter(FirstBase) and
                        (not IsHebrewFinalLetter(LastBase));

        if NeedsReverse then
        begin
          for k := ClusterCount - 1 downto 0 do
            sb.Append(Clusters[k]);
        end
        else
        begin
          for k := 0 to ClusterCount - 1 do
            sb.Append(Clusters[k]);
        end;
        Continue;
      end;

      if S[i] = '\' then
      begin
        if (i + 1 <= Len) and (S[i + 1] in ['\', '{', '}', '''', '*', '~', '-', '_', #10, #13]) then
        begin
          sb.Append(S[i]);
          sb.Append(S[i + 1]);
          Inc(i, 2);
          Continue;
        end;

        NextIdx := i + 1;
        Cmd := '';
        while (NextIdx <= Len) and (S[NextIdx] in ['a'..'z', 'A'..'Z']) do
        begin
          Cmd := Cmd + LowerCase(S[NextIdx]);
          Inc(NextIdx);
        end;
        ParamStr := '';
        if (NextIdx <= Len) and (S[NextIdx] = '-') then
        begin
          ParamStr := '-';
          Inc(NextIdx);
        end;
        while (NextIdx <= Len) and (S[NextIdx] in ['0'..'9']) do
        begin
          ParamStr := ParamStr + S[NextIdx];
          Inc(NextIdx);
        end;

        if Cmd <> '' then
        begin
          // Skip table row/cell formatting commands that create narrow bordered cells
          if (Cmd = 'trowd') or (Cmd = 'intbl') or (Cmd = 'itap') or
             (Cmd = 'trgaph') or (Cmd = 'trleft') or (Cmd = 'trrh') or
             (Cmd = 'trkeep') or (Cmd = 'trql') or (Cmd = 'trqr') or (Cmd = 'trqc') or
             (Cmd = 'cellx') or (Cmd = 'clmgf') or (Cmd = 'clmrg') or
             (Cmd = 'clvmgf') or (Cmd = 'clvmrg') or
             AnsiStartsStr('trbrdr', Cmd) or AnsiStartsStr('clbrdr', Cmd) or
             AnsiStartsStr('clvertal', Cmd) or AnsiStartsStr('clshdng', Cmd) or
             AnsiStartsStr('clcbpat', Cmd) or AnsiStartsStr('clcfpat', Cmd) or
             AnsiStartsStr('clpad', Cmd) or AnsiStartsStr('clfts', Cmd) or
             AnsiStartsStr('clwWidth', Cmd) or AnsiStartsStr('trfts', Cmd) or
             AnsiStartsStr('trwWidth', Cmd) or AnsiStartsStr('trpadd', Cmd) or
             AnsiStartsStr('trspd', Cmd) then
          begin
            if (NextIdx <= Len) and (S[NextIdx] = ' ') then
              Inc(NextIdx);
            i := NextIdx;
            Continue;
          end
          else if (Cmd = 'cell') or (Cmd = 'nestcell') then
          begin
            if (NextIdx <= Len) and (S[NextIdx] = ' ') then
              Inc(NextIdx);
            sb.Append('  ');
            i := NextIdx;
            Continue;
          end
          else if (Cmd = 'row') or (Cmd = 'nestrow') then
          begin
            if (NextIdx <= Len) and (S[NextIdx] = ' ') then
              Inc(NextIdx);
            sb.Append('\par\pard ');
            i := NextIdx;
            Continue;
          end;
        end;
      end;

      sb.Append(S[i]);
      Inc(i);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function AdaptRtfForSkin(const ARtf: string; const ASkin: TSkinColors): string;
var
  lightColorStr: string;
  rgbCol: LongInt;
  sb: TStringBuilder;
  i, len: Integer;
begin
  Result := NormalizeRtfTablesAndHebrew(ARtf);
  if Result = '' then Exit;

  if not ASkin.IsDark then Exit;

  rgbCol := ColorToRGB(ASkin.NoteText);
  lightColorStr := Format('\red%d\green%d\blue%d;', [Red(rgbCol), Green(rgbCol), Blue(rgbCol)]);

  // 1. Set color table index 0 (\cf0, the default/auto entry before the first semicolon)
  //    to ASkin.NoteText without shifting the indices of \cf1, \cf2, \cf3, etc.
  if Pos('{\colortbl ;', Result) > 0 then
    Result := StringReplace(Result, '{\colortbl ;', '{\colortbl ' + lightColorStr, [rfReplaceAll])
  else if Pos('{\colortbl;', Result) > 0 then
    Result := StringReplace(Result, '{\colortbl;', '{\colortbl ' + lightColorStr, [rfReplaceAll]);

  Result := StringReplace(Result, '\red0\green0\blue0;', lightColorStr, [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green0\blue0;', lightColorStr, [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green0\blue255;', '\red115\green175\blue255;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green0\blue128;', '\red135\green195\blue255;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red150\green60\blue100;', lightColorStr, [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green128\blue0;', '\red125\green225\blue145;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green128\blue128;', '\red115\green215\blue215;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green128\blue0;', '\red235\green235\blue135;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red96\green96\blue96;', '\red170\green175\blue185;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green128\blue128;', '\red185\green195\blue205;', [rfReplaceAll]);

  // 2. Word-boundary scanner to ensure every \pard, \plain, \par defaults to \cf0 (ASkin.NoteText)
  sb := TStringBuilder.Create;
  try
    len := Length(Result);
    i := 1;
    while i <= len do
    begin
      if (i <= len - 4) and (Copy(Result, i, 5) = '\pard') and
         ((i + 5 > len) or not (Result[i + 5] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\pard\cf0 ');
        Inc(i, 5);
      end
      else if (i <= len - 5) and (Copy(Result, i, 6) = '\plain') and
         ((i + 6 > len) or not (Result[i + 6] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\plain\cf0 ');
        Inc(i, 6);
      end
      else if (i <= len - 3) and (Copy(Result, i, 4) = '\par') and
         ((i + 4 > len) or not (Result[i + 4] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\par\cf0 ');
        Inc(i, 4);
      end
      else
      begin
        sb.Append(Result[i]);
        Inc(i);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;

  Result := StringReplace(Result, '\cf0 \cf0 ', '\cf0 ', [rfReplaceAll]);
end;

{$IFNDEF WINDOWS}
type
  TRtfStyleState = record
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    ColorIdx: Integer;
    FontSizePt: Integer;
    UcSkip: Integer;
  end;

  TRtfTextSpan = record
    CharStart: Integer;
    CharLen: Integer;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    Color: TColor;
    SizePt: Integer;
  end;

function CodePointToUTF8Str(ACodePoint: Cardinal): string;
begin
  if ACodePoint <= $7F then
    Result := Chr(ACodePoint)
  else if ACodePoint <= $7FF then
    Result := Chr($C0 or ((ACodePoint shr 6) and $1F)) +
              Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $FFFF then
    Result := Chr($E0 or ((ACodePoint shr 12) and $0F)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $10FFFF then
    Result := Chr($F0 or ((ACodePoint shr 18) and $07)) +
              Chr($80 or ((ACodePoint shr 12) and $3F)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else
    Result := '?';
end;

function CP1252ByteToCodePoint(AByte: Byte): Cardinal;
begin
  case AByte of
    $80: Result := $20AC;
    $82: Result := $201A;
    $83: Result := $0192;
    $84: Result := $201E;
    $85: Result := $2026;
    $86: Result := $2020;
    $87: Result := $2021;
    $88: Result := $02C6;
    $89: Result := $2030;
    $8A: Result := $0160;
    $8B: Result := $2039;
    $8C: Result := $0152;
    $8E: Result := $017D;
    $91: Result := $2018;
    $92: Result := $2019;
    $93: Result := $201C;
    $94: Result := $201D;
    $95: Result := $2022;
    $96: Result := $2013;
    $97: Result := $2014;
    $98: Result := $02DC;
    $99: Result := $2122;
    $9A: Result := $0161;
    $9B: Result := $203A;
    $9C: Result := $0153;
    $9E: Result := $017E;
    $9F: Result := $0178;
  else
    Result := AByte;
  end;
end;

function EnsureReadableColorOnSkin(AColor: TColor; const ASkin: TSkinColors;
  ADefaultText: TColor): TColor;
var
  RgbVal: LongInt;
  R, G, B, Lum: Integer;
begin
  RgbVal := ColorToRGB(AColor);
  if not ASkin.IsDark then
  begin
    Result := RgbVal;
    Exit;
  end;
  R := Red(RgbVal);
  G := Green(RgbVal);
  B := Blue(RgbVal);
  Lum := (R * 299 + G * 587 + B * 114) div 1000;
  if Lum >= 105 then
  begin
    Result := RgbVal;
    Exit;
  end;
  if (Abs(R - G) <= 20) and (Abs(G - B) <= 20) then
  begin
    Result := ColorToRGB(ADefaultText);
    Exit;
  end;
  R := Min(255, R + 125);
  G := Min(255, G + 125);
  B := Min(255, B + 125);
  Result := RGBToColor(R, G, B);
end;

procedure RenderRtfToRichMemoLinux(Memo: TRichMemo; const ARtf: string; const ASkin: TSkinColors);
var
  DefaultTextColor: TColor;
  ColorTable: array of TColor;
  ColorCount: Integer;
  StateStack: array of TRtfStyleState;
  StackTop: Integer;
  CurState: TRtfStyleState;
  Depth: Integer;
  SkipGroupDepth: Integer;
  InColorTbl: Boolean;
  ColorTblDepth: Integer;
  CurR, CurG, CurB: Integer;
  HasRGB: Boolean;
  FullText: string;
  RunBuffer: string;
  CurCodePointPos: Integer;
  Spans: array of TRtfTextSpan;
  SpanCount: Integer;
  i, Len: Integer;

  function ResolveColor(AIdx: Integer): TColor;
  begin
    if (AIdx >= 0) and (AIdx < ColorCount) then
      Result := EnsureReadableColorOnSkin(ColorTable[AIdx], ASkin, DefaultTextColor)
    else
      Result := DefaultTextColor;
  end;

  procedure FlushRun;
  var
    RunCodePoints: Integer;
  begin
    if RunBuffer = '' then Exit;
    RunCodePoints := UTF8Length(RunBuffer);
    if RunCodePoints <= 0 then
    begin
      RunBuffer := '';
      Exit;
    end;
    FullText := FullText + RunBuffer;
    if (SpanCount > 0) and
       (Spans[SpanCount - 1].Bold = CurState.Bold) and
       (Spans[SpanCount - 1].Italic = CurState.Italic) and
       (Spans[SpanCount - 1].Underline = CurState.Underline) and
       (Spans[SpanCount - 1].Color = ResolveColor(CurState.ColorIdx)) and
       (Spans[SpanCount - 1].SizePt = CurState.FontSizePt) and
       (Spans[SpanCount - 1].CharStart + Spans[SpanCount - 1].CharLen = CurCodePointPos) then
    begin
      Inc(Spans[SpanCount - 1].CharLen, RunCodePoints);
    end
    else
    begin
      if SpanCount >= Length(Spans) then
        SetLength(Spans, Max(32, Length(Spans) * 2));
      Spans[SpanCount].CharStart := CurCodePointPos;
      Spans[SpanCount].CharLen := RunCodePoints;
      Spans[SpanCount].Bold := CurState.Bold;
      Spans[SpanCount].Italic := CurState.Italic;
      Spans[SpanCount].Underline := CurState.Underline;
      Spans[SpanCount].Color := ResolveColor(CurState.ColorIdx);
      Spans[SpanCount].SizePt := CurState.FontSizePt;
      Inc(SpanCount);
    end;
    Inc(CurCodePointPos, RunCodePoints);
    RunBuffer := '';
  end;

  procedure PushState;
  begin
    if StackTop >= Length(StateStack) then
      SetLength(StateStack, Max(32, Length(StateStack) * 2));
    StateStack[StackTop] := CurState;
    Inc(StackTop);
  end;

  procedure PopState;
  begin
    if StackTop > 0 then
    begin
      FlushRun;
      Dec(StackTop);
      CurState := StateStack[StackTop];
    end;
  end;

var
  Ch: Char;
  Cmd: string;
  ParamStr: string;
  HasParam: Boolean;
  ParamVal: Integer;
  HadDelimiterSpace: Boolean;
  HexStr: string;
  HexVal: Integer;
  CodePt: Integer;
  SkipIdx: Integer;
  SeqLen: Integer;
  SpanIdx: Integer;
  BaseParams, Params: TFontParams;
begin
  if Memo = nil then Exit;

  if Assigned(Memo.Owner) and (Memo.Owner is TWinControl) then
    TWinControl(Memo.Owner).HandleNeeded;
  if Assigned(Memo.Parent) then
    Memo.Parent.HandleNeeded;
  Memo.HandleNeeded;

  if Trim(ARtf) = '' then
  begin
    Memo.Lines.Text := '';
    Exit;
  end;

  if ASkin.Id = skClassic then
    DefaultTextColor := ColorToRGB(clWindowText)
  else
    DefaultTextColor := ColorToRGB(ASkin.NoteText);

  if (Pos('{\rtf', LowerCase(TrimLeft(ARtf))) <> 1) and (Pos('\', ARtf) = 0) then
  begin
    Memo.Lines.Text := ARtf;
    CurCodePointPos := UTF8Length(ARtf);
    if CurCodePointPos > 0 then
    begin
      InitFontParams(BaseParams);
      BaseParams.Name := 'Sans';
      BaseParams.Size := 11;
      BaseParams.Color := DefaultTextColor;
      BaseParams.Style := [];
      Memo.SetTextAttributes(0, CurCodePointPos, BaseParams);
    end;
    Memo.SelStart := 0;
    Memo.SelLength := 0;
    Exit;
  end;

  SetLength(ColorTable, 16);
  ColorTable[0] := DefaultTextColor;
  ColorTable[1] := DefaultTextColor;
  ColorTable[2] := RGBToColor(115, 175, 255);
  ColorTable[3] := DefaultTextColor;
  ColorTable[4] := RGBToColor(125, 225, 145);
  ColorTable[5] := RGBToColor(220, 140, 255);
  ColorTable[6] := RGBToColor(255, 120, 120);
  ColorTable[7] := RGBToColor(240, 220, 120);
  ColorTable[8] := DefaultTextColor;
  ColorTable[9] := RGBToColor(135, 195, 255);
  ColorTable[10] := RGBToColor(115, 215, 215);
  ColorTable[11] := RGBToColor(125, 225, 145);
  ColorTable[12] := RGBToColor(210, 150, 235);
  ColorTable[13] := DefaultTextColor;
  ColorTable[14] := RGBToColor(235, 235, 135);
  ColorTable[15] := RGBToColor(185, 195, 205);
  ColorCount := 16;

  CurState.Bold := False;
  CurState.Italic := False;
  CurState.Underline := False;
  CurState.ColorIdx := 0;
  CurState.FontSizePt := 11;
  CurState.UcSkip := 1;

  SetLength(StateStack, 32);
  StackTop := 0;
  Depth := 0;
  SkipGroupDepth := 0;
  InColorTbl := False;
  ColorTblDepth := 0;
  CurR := 0;
  CurG := 0;
  CurB := 0;
  HasRGB := False;

  FullText := '';
  RunBuffer := '';
  CurCodePointPos := 0;
  SetLength(Spans, 64);
  SpanCount := 0;

  Len := Length(ARtf);
  i := 1;
  while i <= Len do
  begin
    Ch := ARtf[i];

    if Ch = '{' then
    begin
      Inc(Depth);
      PushState;
      Inc(i);
      if (i <= Len) and (ARtf[i] = '\') and (i + 1 <= Len) and (ARtf[i + 1] = '*') then
      begin
        if (SkipGroupDepth = 0) and (Depth >= 2) then
          SkipGroupDepth := Depth;
        Inc(i, 2);
      end;
      Continue;
    end;

    if Ch = '}' then
    begin
      if InColorTbl and (Depth <= ColorTblDepth) then
      begin
        InColorTbl := False;
        ColorTblDepth := 0;
      end;
      if (SkipGroupDepth > 0) and (Depth <= SkipGroupDepth) then
        SkipGroupDepth := 0;
      if Depth > 0 then
        Dec(Depth);
      PopState;
      Inc(i);
      Continue;
    end;

    if SkipGroupDepth > 0 then
    begin
      Inc(i);
      Continue;
    end;

    if InColorTbl then
    begin
      if Ch = ';' then
      begin
        if ColorCount >= Length(ColorTable) then
          SetLength(ColorTable, Max(16, Length(ColorTable) * 2));
        if HasRGB then
          ColorTable[ColorCount] := RGBToColor(CurR, CurG, CurB)
        else
          ColorTable[ColorCount] := DefaultTextColor;
        Inc(ColorCount);
        CurR := 0;
        CurG := 0;
        CurB := 0;
        HasRGB := False;
        Inc(i);
        Continue;
      end
      else if Ch = '\' then
      begin
        Inc(i);
        Cmd := '';
        while (i <= Len) and (ARtf[i] in ['a'..'z', 'A'..'Z']) do
        begin
          Cmd := Cmd + LowerCase(ARtf[i]);
          Inc(i);
        end;
        ParamStr := '';
        if (i <= Len) and (ARtf[i] = '-') then
        begin
          ParamStr := '-';
          Inc(i);
        end;
        while (i <= Len) and (ARtf[i] in ['0'..'9']) do
        begin
          ParamStr := ParamStr + ARtf[i];
          Inc(i);
        end;
        if (i <= Len) and (ARtf[i] = ' ') then
          Inc(i);
        ParamVal := StrToIntDef(ParamStr, 0);
        if Cmd = 'red' then
        begin
          CurR := EnsureRange(ParamVal, 0, 255);
          HasRGB := True;
        end
        else if Cmd = 'green' then
        begin
          CurG := EnsureRange(ParamVal, 0, 255);
          HasRGB := True;
        end
        else if Cmd = 'blue' then
        begin
          CurB := EnsureRange(ParamVal, 0, 255);
          HasRGB := True;
        end;
        Continue;
      end
      else
      begin
        Inc(i);
        Continue;
      end;
    end;

    if Ch = '\' then
    begin
      Inc(i);
      if i > Len then Break;

      if ARtf[i] in ['\', '{', '}'] then
      begin
        RunBuffer := RunBuffer + ARtf[i];
        Inc(i);
        Continue;
      end
      else if ARtf[i] = '~' then
      begin
        RunBuffer := RunBuffer + #$C2#$A0;
        Inc(i);
        Continue;
      end
      else if ARtf[i] = '_' then
      begin
        RunBuffer := RunBuffer + '-';
        Inc(i);
        Continue;
      end
      else if ARtf[i] = '-' then
      begin
        Inc(i);
        Continue;
      end
      else if ARtf[i] = '*' then
      begin
        if (SkipGroupDepth = 0) and (Depth >= 2) then
          SkipGroupDepth := Depth;
        Inc(i);
        Continue;
      end
      else if ARtf[i] in [#10, #13] then
      begin
        RunBuffer := RunBuffer + #10;
        Inc(i);
        if (i <= Len) and (ARtf[i] in [#10, #13]) and (ARtf[i] <> ARtf[i - 1]) then
          Inc(i);
        Continue;
      end
      else if ARtf[i] = '''' then
      begin
        Inc(i);
        if i + 1 <= Len then
        begin
          HexStr := '$' + Copy(ARtf, i, 2);
          Inc(i, 2);
          HexVal := StrToIntDef(HexStr, -1);
          if (HexVal >= 0) and (HexVal <= 255) then
            RunBuffer := RunBuffer + CodePointToUTF8Str(CP1252ByteToCodePoint(Byte(HexVal)));
        end;
        Continue;
      end;

      Cmd := '';
      while (i <= Len) and (ARtf[i] in ['a'..'z', 'A'..'Z']) do
      begin
        Cmd := Cmd + LowerCase(ARtf[i]);
        Inc(i);
      end;

      ParamStr := '';
      HasParam := False;
      if (i <= Len) and (ARtf[i] = '-') then
      begin
        ParamStr := '-';
        Inc(i);
      end;
      while (i <= Len) and (ARtf[i] in ['0'..'9']) do
      begin
        ParamStr := ParamStr + ARtf[i];
        HasParam := True;
        Inc(i);
      end;

      HadDelimiterSpace := False;
      if (i <= Len) and (ARtf[i] = ' ') then
      begin
        HadDelimiterSpace := True;
        Inc(i);
      end;

      if Cmd = '' then
        Continue;

      if (Cmd = 'fonttbl') or (Cmd = 'stylesheet') or (Cmd = 'info') or
         (Cmd = 'pict') or (Cmd = 'object') or (Cmd = 'header') or
         (Cmd = 'footer') or (Cmd = 'revtbl') or (Cmd = 'listtable') or
         (Cmd = 'listoverridetable') or (Cmd = 'rsidtbl') or (Cmd = 'generator') then
      begin
        if (SkipGroupDepth = 0) and (Depth >= 2) then
          SkipGroupDepth := Depth;
        Continue;
      end;

      if Cmd = 'colortbl' then
      begin
        if Depth >= 2 then
        begin
          InColorTbl := True;
          ColorTblDepth := Depth;
          CurR := 0;
          CurG := 0;
          CurB := 0;
          HasRGB := False;
          ColorCount := 0;
        end;
        Continue;
      end;

      if (Cmd = 'par') or (Cmd = 'line') or (Cmd = 'row') or (Cmd = 'nestrow') then
      begin
        RunBuffer := RunBuffer + #10;
      end
      else if (Cmd = 'tab') or (Cmd = 'cell') or (Cmd = 'nestcell') then
      begin
        RunBuffer := RunBuffer + '  ';
      end
      else if Cmd = 'bullet' then
      begin
        RunBuffer := RunBuffer + #$E2#$80#$A2;
      end
      else if Cmd = 'endash' then
      begin
        RunBuffer := RunBuffer + #$E2#$80#$93;
      end
      else if Cmd = 'emdash' then
      begin
        RunBuffer := RunBuffer + #$E2#$80#$94;
      end
      else if Cmd = 'lquote' then
      begin
        RunBuffer := RunBuffer + #$E2#$80#$98;
      end
      else if Cmd = 'rquote' then
      begin
        RunBuffer := RunBuffer + #$E2#$80#$99;
      end
      else if Cmd = 'ldblquote' then
      begin
        RunBuffer := RunBuffer + #$E2#$80#$9C;
      end
      else if Cmd = 'rdblquote' then
      begin
        RunBuffer := RunBuffer + #$E2#$80#$9D;
      end
      else if Cmd = 'plain' then
      begin
        FlushRun;
        CurState.Bold := False;
        CurState.Italic := False;
        CurState.Underline := False;
        CurState.ColorIdx := 0;
        CurState.FontSizePt := 11;
      end
      else if Cmd = 'pard' then
      begin
      end
      else if Cmd = 'b' then
      begin
        FlushRun;
        if HasParam and (StrToIntDef(ParamStr, 1) = 0) then
          CurState.Bold := False
        else
          CurState.Bold := True;
      end
      else if Cmd = 'i' then
      begin
        FlushRun;
        if HasParam and (StrToIntDef(ParamStr, 1) = 0) then
          CurState.Italic := False
        else
          CurState.Italic := True;
      end
      else if Cmd = 'ul' then
      begin
        FlushRun;
        if HasParam and (StrToIntDef(ParamStr, 1) = 0) then
          CurState.Underline := False
        else
          CurState.Underline := True;
      end
      else if Cmd = 'ulnone' then
      begin
        FlushRun;
        CurState.Underline := False;
      end
      else if Cmd = 'cf' then
      begin
        FlushRun;
        CurState.ColorIdx := Max(0, StrToIntDef(ParamStr, 0));
      end
      else if Cmd = 'fs' then
      begin
        FlushRun;
        ParamVal := StrToIntDef(ParamStr, 22);
        if ParamVal > 0 then
          CurState.FontSizePt := EnsureRange(ParamVal div 2, 8, 28);
      end
      else if Cmd = 'uc' then
      begin
        CurState.UcSkip := Max(0, StrToIntDef(ParamStr, 1));
      end
      else if (Cmd = 'u') and HasParam then
      begin
        CodePt := StrToIntDef(ParamStr, 63);
        if CodePt < 0 then
          CodePt := CodePt + 65536;
        if CodePt > 0 then
          RunBuffer := RunBuffer + CodePointToUTF8Str(Cardinal(CodePt));
        for SkipIdx := 1 to CurState.UcSkip do
        begin
          if i > Len then Break;
          if (ARtf[i] = '\') and (i + 1 <= Len) and (ARtf[i + 1] = '''') then
            Inc(i, 4)
          else if ARtf[i] = '?' then
            Inc(i)
          else if (not HadDelimiterSpace) and not (ARtf[i] in ['\', '{', '}']) then
            Inc(i);
        end;
      end;
      Continue;
    end;

    if Ch in [#10, #13] then
    begin
      Inc(i);
      Continue;
    end;

    if Ord(Ch) < $80 then
    begin
      RunBuffer := RunBuffer + Ch;
      Inc(i);
    end
    else
    begin
      SeqLen := UTF8CharacterStrictLength(@ARtf[i]);
      if SeqLen < 1 then SeqLen := 1;
      RunBuffer := RunBuffer + Copy(ARtf, i, SeqLen);
      Inc(i, SeqLen);
    end;
  end;

  FlushRun;

  Memo.Lines.Text := FullText;
  if (Memo.Lines.Text = '') and (FullText <> '') then
    Memo.InDelText(FullText, 0, 0);

  if CurCodePointPos > 0 then
  begin
    InitFontParams(BaseParams);
    BaseParams.Name := 'Sans';
    BaseParams.Size := 11;
    BaseParams.Color := DefaultTextColor;
    BaseParams.Style := [];
    Memo.SetTextAttributes(0, CurCodePointPos, BaseParams);

    InitFontParams(Params);
    for SpanIdx := 0 to SpanCount - 1 do
    begin
      if Spans[SpanIdx].CharLen <= 0 then Continue;
      Params.Name := 'Sans';
      Params.Size := Spans[SpanIdx].SizePt;
      Params.Color := Spans[SpanIdx].Color;
      Params.Style := [];
      if Spans[SpanIdx].Bold then Include(Params.Style, fsBold);
      if Spans[SpanIdx].Italic then Include(Params.Style, fsItalic);
      if Spans[SpanIdx].Underline then Include(Params.Style, fsUnderline);
      Memo.SetTextAttributes(Spans[SpanIdx].CharStart, Spans[SpanIdx].CharLen, Params);
    end;
  end;

  Memo.SelStart := 0;
  Memo.SelLength := 0;
end;
{$ENDIF}

{ TFrmDictionaryPopup }

procedure TFrmDictionaryPopup.ShowRtf(Memo: TRichMemo; const Rtf: string);
var
  AdaptedRtf: string;
begin
  if Memo = nil then Exit;
  Memo.ParentColor := False;
  Memo.ParentFont := False;
  if CurrentSkin.Id = skClassic then
  begin
    Memo.Color := clWhite;
    Memo.Font.Color := clWindowText;
    Panel1.Color := clWhite;
    Panel2.Color := clWhite;
  end
  else
  begin
    Memo.Color := CurrentSkin.NoteBg;
    Memo.Font.Color := CurrentSkin.NoteText;
    Panel1.Color := CurrentSkin.NoteBg;
    Panel2.Color := CurrentSkin.NoteBg;
  end;
  ApplyDarkModeToControl(Memo, CurrentSkin.IsDark);
  AdaptedRtf := AdaptRtfForSkin(Rtf, CurrentSkin);
{$IFDEF WINDOWS}
  Memo.HandleNeeded;
  ConfigureRichMemoForHebrewAndTables(Memo);
  Memo.Rtf := AdaptedRtf;
  if Memo.HandleAllocated then
  begin
    HookEditControl(Memo);
    PaintThemedNCScrollBars(Memo.Handle);
  end;
  if Assigned(FScrollbarPaintTimer) then
  begin
    FScrollbarPaintTicks := 0;
    FScrollbarPaintTimer.Enabled := True;
  end;
{$ELSE}
  Self.HandleNeeded;
  if Assigned(Memo.Parent) then
    Memo.Parent.HandleNeeded;
  Memo.HandleNeeded;
  RenderRtfToRichMemoLinux(Memo, AdaptedRtf, CurrentSkin);
{$ENDIF}
end;

procedure TFrmDictionaryPopup.ApplySkin;
begin
  ApplyFormStyle(Self);
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  ApplySkinToForm(Self, CurrentSkin);
  if CurrentSkin.Id = skClassic then
  begin
    Panel1.Color := clWhite;
    Panel2.Color := clWhite;
    RichMemoStrong.Color := clWhite;
    RichMemoStrong.Font.Color := clWindowText;
    RichMemoMorphology.Color := clWhite;
    RichMemoMorphology.Font.Color := clWindowText;
  end
  else
  begin
    Panel1.Color := CurrentSkin.NoteBg;
    Panel2.Color := CurrentSkin.NoteBg;
    RichMemoStrong.Color := CurrentSkin.NoteBg;
    RichMemoStrong.Font.Color := CurrentSkin.NoteText;
    RichMemoMorphology.Color := CurrentSkin.NoteBg;
    RichMemoMorphology.Font.Color := CurrentSkin.NoteText;
  end;
  Splitter1.Invalidate;
{$IFDEF WINDOWS}
  if RichMemoStrong.HandleAllocated then
  begin
    HookEditControl(RichMemoStrong);
    PaintThemedNCScrollBars(RichMemoStrong.Handle);
  end;
  if RichMemoMorphology.HandleAllocated then
  begin
    HookEditControl(RichMemoMorphology);
    PaintThemedNCScrollBars(RichMemoMorphology.Handle);
  end;
{$ENDIF}
end;

procedure TFrmDictionaryPopup.LoadLayoutFromOpts;
var
  ZoomS, ZoomM, SavedW, SavedH, SavedMorfoH: Integer;
  PrevPositioning, PrevApplying: Boolean;
begin
  if not Assigned(opts) then Exit;
  FLayoutLoadedFromOpts := True;
  PrevPositioning := FPositioning;
  PrevApplying := FApplyingLayout;
  FPositioning := True;
  FApplyingLayout := True;
  try
    ZoomS := opts.ReadInteger('leiaute', 'popup.zoom.strong', 10);
    ZoomM := opts.ReadInteger('leiaute', 'popup.zoom.morfo', 10);
    if ZoomS > 30 then
      RichMemoStrong.ZoomFactor := EnsureRange(ZoomS / 100.0, 0.2, 5.0)
    else if ZoomS > 0 then
      RichMemoStrong.ZoomFactor := EnsureRange(ZoomS / 10.0, 0.2, 5.0);
    if ZoomM > 30 then
      RichMemoMorphology.ZoomFactor := EnsureRange(ZoomM / 100.0, 0.2, 5.0)
    else if ZoomM > 0 then
      RichMemoMorphology.ZoomFactor := EnsureRange(ZoomM / 10.0, 0.2, 5.0);

    SavedW := opts.ReadInteger('leiaute', 'popup.largura', 425);
    SavedH := opts.ReadInteger('leiaute', 'popup.altura', 418);
    if SavedW >= 120 then
      FPopupWidth := SavedW
    else
      FPopupWidth := 425;
    if SavedH >= 100 then
      FPopupHeight := SavedH
    else
      FPopupHeight := 418;

    SavedMorfoH := opts.ReadInteger('leiaute', 'popup.morfo.altura',
      opts.ReadInteger('leiaute', 'popup.panel2.altura', 145));
    if SavedMorfoH >= 32 then
      FMorphologyPaneHeight := SavedMorfoH
    else
      FMorphologyPaneHeight := 145;

    FDockRightOffset := opts.ReadInteger('leiaute', 'popup.dock.offset.x', 0);
    FDockTopOffset := opts.ReadInteger('leiaute', 'popup.dock.offset.y', 0);
    if not FDockStateExplicitlySet then
    begin
      if opts.ValueExists('opcoes', 'popup.dock.right') then
        FDockToMainRightEdge := opts.ReadBool('opcoes', 'popup.dock.right', FDockToMainRightEdge)
      else if opts.ValueExists('leiaute', 'popup.dock.right') then
        FDockToMainRightEdge := opts.ReadBool('leiaute', 'popup.dock.right', FDockToMainRightEdge);
    end;

    SetBounds(Left, Top, FPopupWidth, FPopupHeight);
    DisableAlign;
    try
      TabControlMorphologies.Height := EnsureRange(FMorphologyPaneHeight, 32, Max(48, FPopupHeight - 60));
      TabControlMorphologies.Top := ClientHeight - TabControlMorphologies.Height;
      Splitter1.Top := TabControlMorphologies.Top - Splitter1.Height;
    finally
      EnableAlign;
    end;
  finally
    FApplyingLayout := PrevApplying;
    FPositioning := PrevPositioning;
  end;
end;

procedure TFrmDictionaryPopup.SaveLayoutToOpts;
begin
  if not Assigned(opts) then Exit;
  if not FLayoutLoadedFromOpts then
    LoadLayoutFromOpts;
  try
    if Visible and (not FPositioning) and (not FApplyingLayout) then
    begin
      if Width >= 120 then
        FPopupWidth := Width;
      if Height >= 100 then
        FPopupHeight := Height;
      if Panel1.Visible and Panel2.Visible and TabControlMorphologies.Visible and
         (TabControlMorphologies.Height >= 32) then
        FMorphologyPaneHeight := TabControlMorphologies.Height;
    end;
    if FPopupWidth >= 120 then
      opts.WriteInteger('leiaute', 'popup.largura', FPopupWidth)
    else if Width >= 120 then
      opts.WriteInteger('leiaute', 'popup.largura', Width);
    if FPopupHeight >= 100 then
      opts.WriteInteger('leiaute', 'popup.altura', FPopupHeight)
    else if Height >= 100 then
      opts.WriteInteger('leiaute', 'popup.altura', Height);
    opts.WriteInteger('leiaute', 'popup.dock.offset.x', FDockRightOffset);
    opts.WriteInteger('leiaute', 'popup.dock.offset.y', FDockTopOffset);
    if FMorphologyPaneHeight >= 32 then
    begin
      opts.WriteInteger('leiaute', 'popup.morfo.altura', FMorphologyPaneHeight);
      opts.WriteInteger('leiaute', 'popup.panel2.altura', FMorphologyPaneHeight);
    end;
    opts.WriteInteger('leiaute', 'popup.zoom.strong', Round(RichMemoStrong.ZoomFactor * 10));
    opts.WriteInteger('leiaute', 'popup.zoom.morfo', Round(RichMemoMorphology.ZoomFactor * 10));
    opts.UpdateFile;
  except
  end;
end;

procedure TFrmDictionaryPopup.Splitter1Moved(Sender: TObject);
begin
  if TabControlMorphologies.Visible and (TabControlMorphologies.Height >= 32) then
    FMorphologyPaneHeight := TabControlMorphologies.Height;
  SaveLayoutToOpts;
end;

procedure TFrmDictionaryPopup.TabControlMorphologiesResize(Sender: TObject);
begin
  if FPositioning or FApplyingLayout or (not Visible) then Exit;
  if Panel1.Visible and Panel2.Visible and TabControlMorphologies.Visible and
     (TabControlMorphologies.Height >= 32) then
  begin
    FMorphologyPaneHeight := TabControlMorphologies.Height;
    SaveLayoutToOpts;
  end;
end;

procedure TFrmDictionaryPopup.FormCreate(Sender: TObject);
begin
  Scaled := False;
  ApplyFormStyle(Self);
  fCompStyle := csHintWindow;
  Canvas.Brush.Style := bsClear;
  Caption := '';
  FStrongs := TStringList.Create;
  FMorphologies := TStringList.Create;
  FInSizeMove := False;
  FApplyingLayout := False;
  FDockToMainRightEdge := False;
  FDockStateExplicitlySet := False;
  FLayoutLoadedFromOpts := False;
  FPositioning := True;
  try
    FHoveringSyntagm := False;
    FCtrlEnteredPopup := False;
    FLastAnchorPoint := Point(0, 0);
    FShowCursorPos := Point(-1, -1);
    FAutoHideTimer := TTimer.Create(Self);
    FAutoHideTimer.Enabled := False;
    FAutoHideTimer.Interval := 180;
    FAutoHideTimer.OnTimer := @AutoHideTimerTick;

    FScrollbarPaintTicks := 0;
    FScrollbarPaintTimer := TTimer.Create(Self);
    FScrollbarPaintTimer.Enabled := False;
    FScrollbarPaintTimer.Interval := 40;
    FScrollbarPaintTimer.OnTimer := @ScrollbarPaintTimerTick;

    FPopupWidth := 425;
    FPopupHeight := 418;
    FMorphologyPaneHeight := 145;
    FDockRightOffset := 0;
    FDockTopOffset := 0;

    if Assigned(opts) then
      LoadLayoutFromOpts
    else
      SetBounds(Left, Top, FPopupWidth, FPopupHeight);
  finally
    FPositioning := False;
  end;
  Splitter1.OnMoved := @Splitter1Moved;
  TabControlMorphologies.OnResize := @TabControlMorphologiesResize;
  OnMouseWheel := @RichMemoMouseWheel;
  Panel1.OnMouseWheel := @RichMemoMouseWheel;
  Panel2.OnMouseWheel := @RichMemoMouseWheel;
  TabControlStrongs.OnMouseWheel := @RichMemoMouseWheel;
  TabControlMorphologies.OnMouseWheel := @RichMemoMouseWheel;
  RichMemoStrong.OnMouseWheel := @RichMemoMouseWheel;
  RichMemoMorphology.OnMouseWheel := @RichMemoMouseWheel;
  {$IFDEF WINDOWS}
  if FPopupWheelHook = 0 then
    FPopupWheelHook := SetWindowsHookEx(WH_GETMESSAGE, @PopupWheelGetMsgProc, 0, GetCurrentThreadId());
  {$ENDIF}
  SyncDockStateFromMainForm;
end;

procedure TFrmDictionaryPopup.FormDestroy(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  if FPopupWheelHook <> 0 then
  begin
    UnhookWindowsHookEx(FPopupWheelHook);
    FPopupWheelHook := 0;
  end;
  {$ENDIF}
  SaveLayoutToOpts;
  FStrongs.Free;
  FMorphologies.Free;
end;

procedure TFrmDictionaryPopup.FormHide(Sender: TObject);
begin
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
  if Assigned(FScrollbarPaintTimer) then
    FScrollbarPaintTimer.Enabled := False;
  FHoveringSyntagm := False;
  FCtrlEnteredPopup := False;
  FLastAnchorPoint := Point(-1, -1);
  FShowCursorPos := Point(-1, -1);
  if Assigned(FStrongs) then
    FStrongs.Clear;
  if Assigned(FMorphologies) then
    FMorphologies.Clear;
  if Assigned(TabControlStrongs) then
    TabControlStrongs.Tabs.Clear;
  if Assigned(TabControlMorphologies) then
    TabControlMorphologies.Tabs.Clear;
end;

procedure TFrmDictionaryPopup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    HidePopup;
    if Assigned(Application) and Assigned(Application.MainForm) and Application.MainForm.CanFocus then
      Application.MainForm.SetFocus;
    Key := 0;
  end;
end;

function TFrmDictionaryPopup.RouteMouseWheelAtCursor(Shift: TShiftState; WheelDelta: Integer): Boolean;
var
  Memo: TRichMemo;
  CurPos: TPoint;
begin
  Result := False;
  if (not Visible) or (not IsCursorInsidePopupWindow) then
    Exit;

  CurPos := Point(0, 0);
  GetCursorPos(CurPos);
  if Panel2.Visible and TabControlMorphologies.Visible and
     PtInRect(TabControlMorphologies.ClientRect, TabControlMorphologies.ScreenToClient(CurPos)) then
    Memo := RichMemoMorphology
  else if Panel1.Visible then
    Memo := RichMemoStrong
  else
    Memo := RichMemoMorphology;

  if Memo = nil then
    Exit;

  if ssCtrl in Shift then
  begin
    Memo.ZoomFactor := Max(Memo.ZoomFactor + Sign(WheelDelta) * 0.1, 0.1);
    SaveLayoutToOpts;
    Result := True;
    Exit;
  end;

  {$IFDEF WINDOWS}
  if Memo.HandleAllocated then
  begin
    if WheelDelta > 0 then
      Windows.SendMessage(Memo.Handle, EM_LINESCROLL, 0, -3)
    else if WheelDelta < 0 then
      Windows.SendMessage(Memo.Handle, EM_LINESCROLL, 0, 3);
    PaintThemedNCScrollBars(Memo.Handle);
  end;
  if Assigned(FScrollbarPaintTimer) then
  begin
    FScrollbarPaintTicks := 0;
    FScrollbarPaintTimer.Enabled := True;
  end;
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TFrmDictionaryPopup.RichMemoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Memo: TRichMemo;
begin
  if RouteMouseWheelAtCursor(Shift, WheelDelta) then
  begin
    Handled := True;
    Exit;
  end;
  if Sender is TRichMemo then
  begin
    Memo := TRichMemo(Sender);
    if ssCtrl in Shift then
    begin
      Memo.ZoomFactor := Max(Memo.ZoomFactor + Sign(WheelDelta) * 0.1, 0.1);
      SaveLayoutToOpts;
      Handled := True;
      Exit;
    end;
    {$IFDEF WINDOWS}
    if Memo.HandleAllocated then
    begin
      if WheelDelta > 0 then
        Windows.SendMessage(Memo.Handle, EM_LINESCROLL, 0, -3)
      else if WheelDelta < 0 then
        Windows.SendMessage(Memo.Handle, EM_LINESCROLL, 0, 3);
      PaintThemedNCScrollBars(Memo.Handle);
    end;
    if Assigned(FScrollbarPaintTimer) then
    begin
      FScrollbarPaintTicks := 0;
      FScrollbarPaintTimer.Enabled := True;
    end;
    Handled := True;
    {$ELSE}
    Handled := False;
    {$ENDIF}
  end;
end;

procedure TFrmDictionaryPopup.TabControlMorphologiesChange(Sender: TObject);
begin
  if (TabControlMorphologies.TabIndex >= 0) and
     (TabControlMorphologies.TabIndex < FMorphologies.Count) then
    ShowRtf(RichMemoMorphology, FMorphologies[TabControlMorphologies.TabIndex]);
end;

procedure TFrmDictionaryPopup.TabControlStrongsChange(Sender: TObject);
begin
  if (TabControlStrongs.TabIndex >= 0) and
     (TabControlStrongs.TabIndex < FStrongs.Count) then
    ShowRtf(RichMemoStrong, FStrongs[TabControlStrongs.TabIndex]);
end;

procedure TFrmDictionaryPopup.AddStrong(const strong: string; const definition: string);
begin
  FStrongs.Add(definition);
  TabControlStrongs.Tabs.Add(strong);
end;

procedure TFrmDictionaryPopup.AddMorphology(const morfo: string; const definition: string);
begin
  FMorphologies.Add(definition);
  TabControlMorphologies.Tabs.Add(morfo);
end;

procedure TFrmDictionaryPopup.DoOnResize;
begin
  inherited DoOnResize;
  if Visible and (not FPositioning) and (not FApplyingLayout) then
  begin
    if Width >= 120 then
      FPopupWidth := Width;
    if Height >= 100 then
      FPopupHeight := Height;
    if Panel1.Visible and Panel2.Visible and TabControlMorphologies.Visible and
       (TabControlMorphologies.Height >= 32) then
      FMorphologyPaneHeight := TabControlMorphologies.Height;
    SaveLayoutToOpts;
  end;
end;

procedure TFrmDictionaryPopup.WndProc(var TheMessage: TLMessage);
var
  RefForm: TForm;
  TopTL, BotBR: TPoint;
begin
  if TheMessage.msg = MSG_ENTERSIZEMOVE then
  begin
    FInSizeMove := True;
    if Assigned(FAutoHideTimer) then
      FAutoHideTimer.Enabled := False;
  end;
  inherited WndProc(TheMessage);
  if (TheMessage.msg = MSG_EXITSIZEMOVE) or
     (((TheMessage.msg = LM_MOVE) or (TheMessage.msg = LM_SIZE) or (TheMessage.msg = LM_WINDOWPOSCHANGED)) and
      Visible and (not FPositioning) and (not FApplyingLayout)) then
  begin
    if TheMessage.msg = MSG_EXITSIZEMOVE then
    begin
      FInSizeMove := False;
      FPositioning := True;
      try
        ClampToMonitorWorkarea(Mouse.CursorPos);
      finally
        FPositioning := False;
      end;
    end;
    if Width >= 120 then
      FPopupWidth := Width;
    if Height >= 100 then
      FPopupHeight := Height;
    if Panel1.Visible and Panel2.Visible and TabControlMorphologies.Visible and
       (TabControlMorphologies.Height >= 32) then
      FMorphologyPaneHeight := TabControlMorphologies.Height;
    if FDockToMainRightEdge and IsPointerInteracting and IsCursorInsidePopupWindow then
    begin
      RefForm := nil;
      if Assigned(MainForm) then
        RefForm := MainForm
      else if Assigned(Application) then
        RefForm := Application.MainForm;
      if Assigned(RefForm) and (RefForm.ClientWidth > 100) and (RefForm.ClientHeight > 100) then
      begin
        TopTL := RefForm.ClientToScreen(Point(0, 0));
        BotBR := RefForm.ClientToScreen(Point(RefForm.ClientWidth, RefForm.ClientHeight));
        FDockRightOffset := Left - BotBR.X;
        FDockTopOffset := Top - TopTL.Y;
      end;
    end;
    SaveLayoutToOpts;
  end;
end;

function TFrmDictionaryPopup.IsSizingOrMoving: Boolean;
begin
  Result := FInSizeMove;
end;

function TFrmDictionaryPopup.IsPointerInteracting: Boolean;
{$IFDEF WINDOWS}
begin
  Result := FInSizeMove or (GetAsyncKeyState(VK_LBUTTON) < 0) or (GetAsyncKeyState(VK_RBUTTON) < 0);
end;
{$ELSE}
begin
  Result := FInSizeMove or (GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0);
end;
{$ENDIF}

function TFrmDictionaryPopup.IsCtrlKeyHeld: Boolean;
{$IFDEF WINDOWS}
begin
  Result := (GetAsyncKeyState(VK_CONTROL) < 0);
end;
{$ELSE}
begin
  Result := (GetKeyState(VK_CONTROL) < 0);
end;
{$ENDIF}

function TFrmDictionaryPopup.IsCursorInsidePopupWindow: Boolean;
var
  P: TPoint;
  R: TRect;
begin
  P := Point(0, 0);
  GetCursorPos(P);
  R := Rect(Left - 8, Top - 8, Left + Width + 8, Top + Height + 8);
  {$IFDEF WINDOWS}
  if HandleAllocated and Windows.GetWindowRect(Handle, R) then
    InflateRect(R, 8, 8);
  {$ENDIF}
  Result := PtInRect(R, P);
end;

procedure TFrmDictionaryPopup.AutoHideTimerTick(Sender: TObject);
var
  CurPos: TPoint;
begin
  if not Visible then
  begin
    FAutoHideTimer.Enabled := False;
    FCtrlEnteredPopup := False;
    Exit;
  end;
  if FHoveringSyntagm or IsSizingOrMoving or IsPointerInteracting then
    Exit;
  if FDockToMainRightEdge and (FShowCursorPos.X >= 0) and (FShowCursorPos.Y >= 0) then
  begin
    CurPos := Point(0, 0);
    GetCursorPos(CurPos);
    if (Abs(CurPos.X - FShowCursorPos.X) <= 24) and
       (Abs(CurPos.Y - FShowCursorPos.Y) <= 24) then
      Exit;
  end;
  if IsCursorInsidePopupWindow or IsCtrlKeyHeld then
  begin
    if IsCursorInsidePopupWindow then
      FCtrlEnteredPopup := True;
    Exit;
  end;
  FCtrlEnteredPopup := False;
  FAutoHideTimer.Enabled := False;
  HidePopup;
end;

procedure TFrmDictionaryPopup.ScrollbarPaintTimerTick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  if RichMemoStrong.HandleAllocated then
    PaintThemedNCScrollBars(RichMemoStrong.Handle);
  if RichMemoMorphology.HandleAllocated then
    PaintThemedNCScrollBars(RichMemoMorphology.Handle);
  {$ENDIF}
  Inc(FScrollbarPaintTicks);
  if FScrollbarPaintTicks >= 10 then
    FScrollbarPaintTimer.Enabled := False;
end;

procedure TFrmDictionaryPopup.SyncDockStateFromMainForm;
begin
  if not FLayoutLoadedFromOpts then
    LoadLayoutFromOpts;
  if Assigned(opts) then
  begin
    if opts.ValueExists('opcoes', 'popup.dock.right') then
    begin
      FDockToMainRightEdge := opts.ReadBool('opcoes', 'popup.dock.right', FDockToMainRightEdge);
      Exit;
    end;
    if opts.ValueExists('leiaute', 'popup.dock.right') then
    begin
      FDockToMainRightEdge := opts.ReadBool('leiaute', 'popup.dock.right', FDockToMainRightEdge);
      Exit;
    end;
  end;
end;

procedure TFrmDictionaryPopup.SetDockToMainRightEdge(AValue: Boolean);
begin
  if not FLayoutLoadedFromOpts then
    LoadLayoutFromOpts;
  FDockStateExplicitlySet := True;
  FDockToMainRightEdge := AValue;
  if FDockToMainRightEdge then
  begin
    if Visible then
      UpdateRightEdgeDockPosition;
  end
  else if Visible then
  begin
    FPositioning := True;
    try
      SetBounds(Left, Top, Max(120, FPopupWidth), Max(100, FPopupHeight));
      ClampToMonitorWorkarea(Mouse.CursorPos);
      ApplyMorphologyPaneHeight;
    finally
      FPositioning := False;
    end;
  end;
end;

function TFrmDictionaryPopup.ComputeMainRightEdgeBounds(out ABoundsRect: TRect): Boolean;
var
  RefForm: TForm;
  Mon: TMonitor;
  WorkRect, MainRect: TRect;
  PopupW, PopupH, TargetLeft, TargetTop: Integer;
  TopTL, BotBR: TPoint;
begin
  Result := False;
  ABoundsRect := Rect(0, 0, 0, 0);
  MainRect := Rect(0, 0, 0, 0);

  if not FLayoutLoadedFromOpts then
    LoadLayoutFromOpts;

  RefForm := nil;
  if Assigned(MainForm) then
    RefForm := MainForm
  else if Assigned(Application) then
    RefForm := Application.MainForm;
  if not Assigned(RefForm) then
    Exit;

  Mon := nil;
  if RefForm.HandleAllocated then
    Mon := Screen.MonitorFromWindow(RefForm.Handle, mdNearest);
  if (Mon = nil) and ((FLastAnchorPoint.X > 0) or (FLastAnchorPoint.Y > 0)) then
    Mon := Screen.MonitorFromPoint(FLastAnchorPoint, mdNearest);
  if Mon = nil then
    Mon := Screen.MonitorFromPoint(Mouse.CursorPos, mdNearest);
  if Mon <> nil then
    WorkRect := Mon.WorkareaRect
  else
    WorkRect := Rect(0, 0, Screen.Width, Screen.Height);

  if FPopupWidth >= 120 then
    PopupW := FPopupWidth
  else
    PopupW := Max(120, Width);
  if PopupW > (WorkRect.Right - WorkRect.Left) - 100 then
    PopupW := Max(120, (WorkRect.Right - WorkRect.Left) div 2);

  if FPopupHeight >= 100 then
    PopupH := FPopupHeight
  else
    PopupH := Max(100, Height);
  if PopupH > (WorkRect.Bottom - WorkRect.Top) then
    PopupH := Max(100, WorkRect.Bottom - WorkRect.Top);

  if (RefForm.ClientWidth > 100) and (RefForm.ClientHeight > 100) then
  begin
    TopTL := RefForm.ClientToScreen(Point(0, 0));
    BotBR := RefForm.ClientToScreen(Point(RefForm.ClientWidth, RefForm.ClientHeight));
    TargetLeft := BotBR.X + FDockRightOffset;
    TargetTop := TopTL.Y + FDockTopOffset;
    TargetLeft := EnsureRange(TargetLeft, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
    TargetTop := EnsureRange(TargetTop, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));
    ABoundsRect := Rect(TargetLeft, TargetTop, TargetLeft + PopupW, TargetTop + PopupH);
    Result := True;
    Exit;
  end;

  {$IFDEF WINDOWS}
  if RefForm.HandleAllocated and Windows.GetWindowRect(RefForm.Handle, MainRect) then
  begin
    TargetLeft := MainRect.Right + FDockRightOffset;
    TargetTop := MainRect.Top + 48 + FDockTopOffset;
    TargetLeft := EnsureRange(TargetLeft, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
    TargetTop := EnsureRange(TargetTop, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));
    ABoundsRect := Rect(TargetLeft, TargetTop, TargetLeft + PopupW, TargetTop + PopupH);
    Result := True;
    Exit;
  end;
  {$ENDIF}

  TargetLeft := RefForm.Left + RefForm.Width + FDockRightOffset;
  TargetTop := RefForm.Top + 48 + FDockTopOffset;
  TargetLeft := EnsureRange(TargetLeft, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
  TargetTop := EnsureRange(TargetTop, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));
  ABoundsRect := Rect(TargetLeft, TargetTop, TargetLeft + PopupW, TargetTop + PopupH);
  Result := True;
end;

procedure TFrmDictionaryPopup.ApplyMorphologyPaneHeight;
begin
  if not TabControlMorphologies.Visible then Exit;
  FApplyingLayout := True;
  try
    DisableAlign;
    try
      TabControlMorphologies.Height := EnsureRange(FMorphologyPaneHeight, 32, Max(48, ClientHeight - 60));
      TabControlMorphologies.Top := ClientHeight - TabControlMorphologies.Height;
      Splitter1.Top := TabControlMorphologies.Top - Splitter1.Height;
    finally
      EnableAlign;
    end;
  finally
    FApplyingLayout := False;
  end;
end;

procedure TFrmDictionaryPopup.UpdateRightEdgeDockPosition;
var
  R: TRect;
  PrevPositioning: Boolean;
begin
  if not FDockToMainRightEdge then Exit;
  if not ComputeMainRightEdgeBounds(R) then Exit;
  PrevPositioning := FPositioning;
  FPositioning := True;
  try
    SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
    ClampToMonitorWorkarea(Point(R.Left, R.Top));
    ApplyMorphologyPaneHeight;
  finally
    FPositioning := PrevPositioning;
  end;
end;

procedure TFrmDictionaryPopup.ClampToMonitorWorkarea(const RefPoint: TPoint);
var
  Mon: TMonitor;
  Wa: TRect;
begin
  Mon := Screen.MonitorFromPoint(RefPoint, mdNearest);
  if Mon <> nil then
    Wa := Mon.WorkareaRect
  else
    Wa := Rect(0, 0, Screen.Width, Screen.Height);

  if Left + Width > Wa.Right then
    Left := Wa.Right - Width;
  if Top + Height > Wa.Bottom then
    Top := Wa.Bottom - Height;
  if Left < Wa.Left then
    Left := Wa.Left;
  if Top < Wa.Top then
    Top := Wa.Top;
end;

procedure TFrmDictionaryPopup.ShowAt(x: Integer; y: Integer);
var
  PopupW, PopupH: Integer;
  px, py: Integer;
  Mon: TMonitor;
  WorkRect, DockRect: TRect;
  PointerGap: Integer;
  PointerSize: Integer;
  CandL, CandT: Integer;
  AvailW, AvailH: Integer;
  ClearsPointer: Boolean;
  IsDiagonal: Boolean;
  FitArea, TotalArea: Int64;
  BestClearsPointer: Boolean;
  BestIsDiagonal: Boolean;
  BestFitArea, BestTotalArea: Int64;
  BestL, BestT: Integer;
  NorthH, SouthH, EastW, WestW: Integer;
  PointerRect, OverlapRect, CandRect: TRect;
  di, dj: Integer;
  SizeChangedWhileVisible: Boolean;
begin
  if (FStrongs.Count = 0) and (FMorphologies.Count = 0) then
    Exit;

  if not FLayoutLoadedFromOpts then
    LoadLayoutFromOpts;

  if Visible and (not FPositioning) and (not FApplyingLayout) then
  begin
    SizeChangedWhileVisible := False;
    if (Width >= 120) and (Width <> FPopupWidth) then
    begin
      FPopupWidth := Width;
      SizeChangedWhileVisible := True;
    end;
    if (Height >= 100) and (Height <> FPopupHeight) then
    begin
      FPopupHeight := Height;
      SizeChangedWhileVisible := True;
    end;
    if Panel1.Visible and Panel2.Visible and TabControlMorphologies.Visible and
       (TabControlMorphologies.Height >= 32) and
       (TabControlMorphologies.Height <> FMorphologyPaneHeight) then
    begin
      FMorphologyPaneHeight := TabControlMorphologies.Height;
      SizeChangedWhileVisible := True;
    end;
    if SizeChangedWhileVisible then
      SaveLayoutToOpts;
  end;

  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;

  FPositioning := True;
  try
    if FStrongs.Count > 0 then
    begin
      TabControlStrongs.Visible := True;
      Panel1.Visible := True;
      TabControlStrongs.TabIndex := 0;
      ShowRtf(RichMemoStrong, FStrongs[0]);
      RichMemoStrong.ZoomFactor := RichMemoStrong.ZoomFactor;
    end
    else
    begin
      Panel1.Visible := False;
      TabControlStrongs.Visible := False;
    end;

    if FMorphologies.Count > 0 then
    begin
      TabControlMorphologies.Visible := True;
      Panel2.Visible := True;
      TabControlMorphologies.TabIndex := 0;
      ShowRtf(RichMemoMorphology, FMorphologies[0]);
      RichMemoMorphology.ZoomFactor := RichMemoMorphology.ZoomFactor;
      Splitter1.Visible := Panel1.Visible;
    end
    else
    begin
      Panel2.Visible := False;
      TabControlMorphologies.Visible := False;
      Splitter1.Visible := False;
    end;

    px := x;
    py := y;
    FShowCursorPos := Point(0, 0);
    GetCursorPos(FShowCursorPos);
    if Assigned(MainForm) and Assigned(MainForm.ScrollBoxDstVerse) and
       MainForm.ScrollBoxDstVerse.Showing and
       PtInRect(MainForm.ScrollBoxDstVerse.ClientRect, MainForm.ScrollBoxDstVerse.ScreenToClient(FShowCursorPos)) then
    begin
      px := FShowCursorPos.X;
      py := FShowCursorPos.Y + 18;
    end;
    FLastAnchorPoint := Point(px, py);

    if FPopupWidth >= 120 then
      PopupW := FPopupWidth
    else
      PopupW := Max(120, Width);
    if FPopupHeight >= 100 then
      PopupH := FPopupHeight
    else
      PopupH := Max(100, Height);

    Mon := Screen.MonitorFromPoint(Point(px, py), mdNearest);
    if Mon <> nil then
      WorkRect := Mon.WorkareaRect
    else
      WorkRect := Classes.Rect(0, 0, Screen.Width, Screen.Height);

    PointerGap := 10;
    PointerSize := 16;
    PointerRect := Classes.Rect(px - PointerSize, py - PointerSize, px + PointerSize, py + PointerSize);

    NorthH := Max(0, py - PointerGap - WorkRect.Top);
    SouthH := Max(0, WorkRect.Bottom - (py + PointerGap));
    EastW  := Max(0, WorkRect.Right - (px + PointerGap));
    WestW  := Max(0, (px - PointerGap) - WorkRect.Left);

    BestClearsPointer := False;
    BestIsDiagonal := True;
    BestFitArea := -1;
    BestTotalArea := -1;
    BestL := WorkRect.Left;
    BestT := WorkRect.Top;

    for di := -1 to 1 do
      for dj := -1 to 1 do
      begin
        if (di = 0) and (dj = 0) then Continue;

        case di of
          -1: CandL := px - PointerGap - PopupW;
           0: CandL := px - (PopupW div 2);
           1: CandL := px + PointerGap;
        end;

        case dj of
          -1: CandT := py - PointerGap - PopupH;
           0: CandT := py - (PopupH div 2);
           1: CandT := py + PointerGap;
        end;

        CandL := EnsureRange(CandL, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
        CandT := EnsureRange(CandT, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));

        CandRect := Classes.Rect(CandL, CandT, CandL + PopupW, CandT + PopupH);
        ClearsPointer := not IntersectRect(OverlapRect, CandRect, PointerRect);
        IsDiagonal := (di <> 0) and (dj <> 0);

        AvailW := Max(0, Min(WorkRect.Right, CandL + PopupW) - Max(WorkRect.Left, CandL));
        AvailH := Max(0, Min(WorkRect.Bottom, CandT + PopupH) - Max(WorkRect.Top, CandT));
        FitArea := Int64(Min(PopupW, AvailW)) * Int64(Min(PopupH, AvailH));
        TotalArea := Int64(AvailW) * Int64(AvailH);

        if (BestFitArea < 0) or
           (ClearsPointer and not BestClearsPointer) or
           ((ClearsPointer = BestClearsPointer) and (FitArea > BestFitArea)) or
           ((ClearsPointer = BestClearsPointer) and (FitArea = BestFitArea) and
            ((IsDiagonal and not BestIsDiagonal) or
             ((IsDiagonal = BestIsDiagonal) and (TotalArea > BestTotalArea)))) then
        begin
          BestClearsPointer := ClearsPointer;
          BestIsDiagonal := IsDiagonal;
          BestFitArea := FitArea;
          BestTotalArea := TotalArea;
          BestL := CandL;
          BestT := CandT;
        end;
      end;

    CandRect := Classes.Rect(BestL, BestT, BestL + PopupW, BestT + PopupH);
    if IntersectRect(OverlapRect, CandRect, PointerRect) then
    begin
      if Max(NorthH, SouthH) >= Max(EastW, WestW) then
      begin
        if SouthH >= NorthH then
          BestT := Min(py + PointerGap, Max(WorkRect.Top, WorkRect.Bottom - 80))
        else
          BestT := Max(WorkRect.Top, (py - PointerGap) - PopupH);
      end
      else
      begin
        if EastW >= WestW then
          BestL := Min(px + PointerGap, Max(WorkRect.Left, WorkRect.Right - 100))
        else
          BestL := Max(WorkRect.Left, (px - PointerGap) - PopupW);
      end;
      BestL := EnsureRange(BestL, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
      BestT := EnsureRange(BestT, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));
    end;

    SyncDockStateFromMainForm;
    if FDockToMainRightEdge and ComputeMainRightEdgeBounds(DockRect) then
    begin
      BestL := DockRect.Left;
      BestT := DockRect.Top;
      PopupW := DockRect.Right - DockRect.Left;
      PopupH := DockRect.Bottom - DockRect.Top;
    end;

    SetBounds(BestL, BestT, PopupW, PopupH);
    if FMorphologies.Count > 0 then
      ApplyMorphologyPaneHeight;

    Visible := True;
    ApplySkin;

    if FDockToMainRightEdge then
      UpdateRightEdgeDockPosition;
  finally
    FPositioning := False;
  end;

  {$IFNDEF WINDOWS}
  if Panel1.Visible and (FStrongs.Count > 0) then
  begin
    if (TabControlStrongs.TabIndex >= 0) and (TabControlStrongs.TabIndex < FStrongs.Count) then
      ShowRtf(RichMemoStrong, FStrongs[TabControlStrongs.TabIndex])
    else
      ShowRtf(RichMemoStrong, FStrongs[0]);
  end;
  if Panel2.Visible and (FMorphologies.Count > 0) then
  begin
    if (TabControlMorphologies.TabIndex >= 0) and (TabControlMorphologies.TabIndex < FMorphologies.Count) then
      ShowRtf(RichMemoMorphology, FMorphologies[TabControlMorphologies.TabIndex])
    else
      ShowRtf(RichMemoMorphology, FMorphologies[0]);
  end;
  {$ENDIF}

  if Assigned(FScrollbarPaintTimer) then
  begin
    FScrollbarPaintTicks := 0;
    FScrollbarPaintTimer.Enabled := True;
  end;
end;

procedure TFrmDictionaryPopup.HidePopup;
begin
  if not Visible then
    Exit;
  if (not FPositioning) and (not FApplyingLayout) then
  begin
    if Width >= 120 then
      FPopupWidth := Width;
    if Height >= 100 then
      FPopupHeight := Height;
    if Panel1.Visible and Panel2.Visible and TabControlMorphologies.Visible and
       (TabControlMorphologies.Height >= 32) then
      FMorphologyPaneHeight := TabControlMorphologies.Height;
    SaveLayoutToOpts;
  end;
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
  if Assigned(FScrollbarPaintTimer) then
    FScrollbarPaintTimer.Enabled := False;
  FHoveringSyntagm := False;
  FCtrlEnteredPopup := False;
  FPositioning := True;
  try
    Visible := False;
    FormHide(Self);
    TabControlStrongs.Tabs.Clear;
    TabControlMorphologies.Tabs.Clear;
    {$IFDEF WINDOWS}
    RichMemoStrong.Rtf := '';
    RichMemoMorphology.Rtf := '';
    {$ELSE}
    RichMemoStrong.Lines.Text := '';
    RichMemoMorphology.Lines.Text := '';
    {$ENDIF}
  finally
    FPositioning := False;
  end;
end;

procedure TFrmDictionaryPopup.NotifySyntagmHoverEnter;
begin
  FHoveringSyntagm := True;
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
end;

procedure TFrmDictionaryPopup.RequestDeferredHide;
begin
  FHoveringSyntagm := False;
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := True;
end;

procedure TFrmDictionaryPopup.AfterConstruction;
begin
  inherited AfterConstruction;
  {$IFDEF WINDOWS}
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_NOACTIVATE);
  {$ENDIF}
  if Assigned(opts) then
    LoadLayoutFromOpts;
end;

end.
