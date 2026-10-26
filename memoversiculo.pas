unit MemoVersiculo;

{$mode objfpc}{$H+}

interface

uses
  {$IFNDEF UNIX}
  Windows,
  {$ENDIF}
  Classes, SysUtils, StrUtils, StdCtrls, ExtCtrls, Controls, Forms, Graphics,
  Menus, LCLType, Types, Dialogs, Versiculo, Math, unitskins;

type
  TMemoVersiculo = class;
  TEventoTextoModificado = procedure (Sender: TMemoVersiculo) of object;
  TEventoTrocarSecaoEdit = procedure (SectionIndex: Integer) of object;

  TVerseFormatCmd = (
    fmtNone,
    fmtTargetBadge,
    fmtCI,
    fmtCM,
    fmtCL,
    fmtPIDec,
    fmtPIInc,
    fmtPFDec,
    fmtPFInc,
    fmtCenter,
    fmtTS1,
    fmtTS2,
    fmtTS3,
    fmtFallbackCM,
    fmtFI,
    fmtFR,
    fmtFO,
    fmtBold,
    fmtItalic,
    fmtUnderline,
    fmtSup,
    fmtSub,
    fmtRF,
    fmtClear
  );

  TFormatBtnItem = record
    Cmd: TVerseFormatCmd;
    Bounds: TRect;
    TopLabel: string;
    SubLabel: string;
    HintText: string;
    GroupEnd: Boolean;
  end;

  { TMemoVersiculo }

  TMemoVersiculo = class
  private
    FModificado: boolean;
    FInternalChanging: boolean;
    FMemo: TMemo;
    FVersiculo: TVersiculo;
    FActiveSectionIndex: Integer;
    FActiveSectionName: string;
    FEventoTextoModificado: TEventoTextoModificado;
    FEventoLivePreview: TEventoTextoModificado;
    FEventoTrocarSecaoEdit: TEventoTrocarSecaoEdit;
    FRibbonHost: TWinControl;
    FRibbonPanel: TPanel;
    FMiniBar: TPanel;
    FTargetPopupMenu: TPopupMenu;
    FSelTimer: TTimer;
    FRibbonButtons: array of TFormatBtnItem;
    FMiniButtons: array of TFormatBtnItem;
    FHotRibbonIdx: Integer;
    FPushedRibbonIdx: Integer;
    FHotMiniIdx: Integer;
    FPushedMiniIdx: Integer;
    function GetAtivo: boolean;
    function GetTexto: TCaption;
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnMemoMouseUp(Sender: TObject; Button: TMouseButton;
             Shift: TShiftState; X, Y: Integer);
    procedure OnMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnMemoKeyPress(Sender: TObject; var Key: char);
    procedure OnSelTimer(Sender: TObject);
    procedure BuildRibbonButtons;
    procedure BuildMiniButtons;
    procedure OnRibbonPaint(Sender: TObject);
    procedure OnRibbonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnRibbonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnRibbonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnRibbonMouseLeave(Sender: TObject);
    procedure OnRibbonResize(Sender: TObject);
    procedure OnMiniBarPaint(Sender: TObject);
    procedure OnMiniBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnMiniBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMiniBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMiniBarMouseLeave(Sender: TObject);
    procedure OnTargetMenuItemClick(Sender: TObject);
    procedure DrawFormatIcon(ACanvas: TCanvas; Cmd: TVerseFormatCmd; IconRect: TRect; IsHot, IsDark: Boolean);
    procedure UpdateMiniBarPosition;
    procedure NotifyLivePreview;
    procedure ApplyTagToSelection(const StartTag, EndTag: string);
    procedure ApplyHeadingToSelection(Level: Integer);
    procedure InsertParagraphTagAtCursor(const ATag: string);
    procedure HandleReturnKey(IsShift: Boolean);
    procedure StepParagraphIndent(IsPI: Boolean; Delta: Integer);
    procedure ToggleParagraphCenter;
    procedure ToggleFallbackCM;
    procedure ClearFormattingTags;
    procedure GetCurrentLineBounds(out LineStart, LineEnd: Integer; out LineText: string);
  protected
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnChange(Sender: TObject);
    procedure ConfirmarAlteracao;
  public
    constructor Criar;
    destructor Destruir;
    class function ExpandForVisualEdit(const RawXML: string): string;
    class function FlattenFromVisualEdit(const VisualText: string): string;
    procedure AttachRibbonHost(AHost: TWinControl);
    procedure SetActiveSectionInfo(ASectionIndex: Integer; const ASectionName: string);
    procedure ExecuteFormatCommand(Cmd: TVerseFormatCmd);
    procedure Ativar(conteiner: TScrollBox; versiculo: TVersiculo);
    procedure Desativar;
    procedure ApplySkin(const ASkin: TSkinColors);
    property Ativo: boolean read GetAtivo;
    property Texto: TCaption read GetTexto;
    property Versiculo: TVersiculo read FVersiculo;
    property RibbonPanel: TPanel read FRibbonPanel;
    property QuandoModificarVersiculo: TEventoTextoModificado read FEventoTextoModificado write FEventoTextoModificado;
    property QuandoLivePreview: TEventoTextoModificado read FEventoLivePreview write FEventoLivePreview;
    property QuandoTrocarSecaoEdit: TEventoTrocarSecaoEdit read FEventoTrocarSecaoEdit write FEventoTrocarSecaoEdit;
  end;

resourcestring
  SChangeVerse = 'Change verse text?';
  SChangeVerseConfirmation = 'Are you sure you want to change the verse text? Some associations may be lost.';

implementation

{ TMemoVersiculo }

class function TMemoVersiculo.ExpandForVisualEdit(const RawXML: string): string;
var
  s: string;
  i, len: Integer;
  sb: string;
  matchedTag: string;
begin
  s := StringReplace(RawXML, #13#10, '', [rfReplaceAll]);
  s := StringReplace(s, #13, '', [rfReplaceAll]);
  s := StringReplace(s, #10, '', [rfReplaceAll]);

  sb := '';
  i := 1;
  len := Length(s);
  while i <= len do
  begin
    matchedTag := '';
    if (i + 3 <= len) and (SameText(Copy(s, i, 4), '<CM>') or
                           SameText(Copy(s, i, 4), '<CI>') or
                           SameText(Copy(s, i, 4), '<CL>')) then
    begin
      matchedTag := UpperCase(Copy(s, i, 4));
    end;

    if matchedTag <> '' then
    begin
      while (Length(sb) > 0) and (sb[Length(sb)] = ' ') do
        Delete(sb, Length(sb), 1);

      if (Length(sb) > 0) and (sb[Length(sb)] <> #10) then
        sb := sb + #13#10;

      sb := sb + matchedTag;
      Inc(i, 4);
      while (i <= len) and (s[i] = ' ') do
        Inc(i);
    end
    else
    begin
      sb := sb + s[i];
      Inc(i);
    end;
  end;
  Result := sb;
end;

class function TMemoVersiculo.FlattenFromVisualEdit(const VisualText: string): string;
var
  lines: TStringList;
  i: Integer;
  ln, acc, prevEnd: string;
begin
  lines := TStringList.Create;
  try
    lines.Text := VisualText;
    acc := '';
    for i := 0 to lines.Count - 1 do
    begin
      ln := lines[i];
      if ln = '' then
        continue;

      if acc = '' then
        acc := ln
      else
      begin
        prevEnd := RightStr(TrimRight(acc), 4);
        if AnsiStartsText('<CM>', TrimLeft(ln)) or
           AnsiStartsText('<CI>', TrimLeft(ln)) or
           AnsiStartsText('<CL>', TrimLeft(ln)) or
           AnsiStartsText('<PI', TrimLeft(ln)) or
           AnsiStartsText('<PF', TrimLeft(ln)) or
           AnsiStartsText('<TS', TrimLeft(ln)) or
           AnsiStartsText('<_CM>', TrimLeft(ln)) or
           SameText(prevEnd, '<CM>') or
           SameText(prevEnd, '<CI>') or
           SameText(prevEnd, '<CL>') or
           SameText(prevEnd, '<Ts>') or
           AnsiEndsText('<_CM>', TrimRight(acc)) then
        begin
          if AnsiStartsText('<CM>', TrimLeft(ln)) or
             AnsiStartsText('<CI>', TrimLeft(ln)) or
             AnsiStartsText('<CL>', TrimLeft(ln)) then
          begin
            acc := TrimRight(acc) + TrimLeft(ln);
          end
          else
            acc := acc + ln;
        end
        else
        begin
          if (acc[Length(acc)] = ' ') or (ln[1] = ' ') then
            acc := acc + ln
          else
            acc := acc + ' ' + ln;
        end;
      end;
    end;

    acc := StringReplace(acc, #13, '', [rfReplaceAll]);
    acc := StringReplace(acc, #10, '', [rfReplaceAll]);

    while Pos(' <CM>', acc) > 0 do
      acc := StringReplace(acc, ' <CM>', '<CM>', [rfReplaceAll]);
    while Pos('<CM> ', acc) > 0 do
      acc := StringReplace(acc, '<CM> ', '<CM>', [rfReplaceAll]);

    while Pos(' <CI>', acc) > 0 do
      acc := StringReplace(acc, ' <CI>', '<CI>', [rfReplaceAll]);
    while Pos('<CI> ', acc) > 0 do
      acc := StringReplace(acc, '<CI> ', '<CI>', [rfReplaceAll]);

    while Pos(' <CL>', acc) > 0 do
      acc := StringReplace(acc, ' <CL>', '<CL>', [rfReplaceAll]);
    while Pos('<CL> ', acc) > 0 do
      acc := StringReplace(acc, '<CL> ', '<CL>', [rfReplaceAll]);

    Result := acc;
  finally
    lines.Free;
  end;
end;

function TMemoVersiculo.GetAtivo: boolean;
begin
  Result := Assigned(FMemo) and FMemo.Visible;
end;

function TMemoVersiculo.GetTexto: TCaption;
begin
  Result := FlattenFromVisualEdit(FMemo.Text);
end;

procedure TMemoVersiculo.NotifyLivePreview;
begin
  if Assigned(FEventoLivePreview) and Assigned(FMemo) and FMemo.Visible then
    FEventoLivePreview(Self);
end;

procedure TMemoVersiculo.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    FMemo.Font.Size := Max(6, FMemo.Font.Size + Sign(WheelDelta) * 1);
    UpdateMiniBarPosition;
  end;
end;

procedure TMemoVersiculo.OnMemoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateMiniBarPosition;
  if Assigned(FRibbonPanel) and FRibbonPanel.Visible then
    FRibbonPanel.Invalidate;
end;

procedure TMemoVersiculo.OnMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  UpdateMiniBarPosition;
  if Assigned(FRibbonPanel) and FRibbonPanel.Visible then
    FRibbonPanel.Invalidate;
end;

procedure TMemoVersiculo.OnMemoKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#13, #10] then
    Key := #0;
end;

procedure TMemoVersiculo.OnSelTimer(Sender: TObject);
begin
  if not Assigned(FMemo) or not FMemo.Visible then
  begin
    if Assigned(FMiniBar) and FMiniBar.Visible then
      FMiniBar.Visible := False;
    Exit;
  end;
  UpdateMiniBarPosition;
end;

procedure TMemoVersiculo.UpdateMiniBarPosition;
var
  selLen, selPos: Integer;
  {$IFNDEF UNIX}
  charPos: LRESULT;
  ptX, ptY: SmallInt;
  {$ELSE}
  ptX, ptY: Integer;
  {$ENDIF}
  targetX, targetY: Integer;
begin
  if not Assigned(FMiniBar) or not Assigned(FMemo) then Exit;

  if not FMemo.Visible or not Assigned(FMemo.Parent) then
  begin
    FMiniBar.Visible := False;
    Exit;
  end;

  selLen := FMemo.SelLength;
  if selLen <= 0 then
  begin
    if FMiniBar.Visible then
      FMiniBar.Visible := False;
    Exit;
  end;

  if FMiniBar.Parent <> FMemo.Parent then
    FMiniBar.Parent := FMemo.Parent;

  selPos := FMemo.SelStart;
  {$IFNDEF UNIX}
  if FMemo.HandleAllocated then
  begin
    charPos := SendMessage(FMemo.Handle, $00D6 {EM_POSFROMCHAR}, WPARAM(selPos), 0);
    if charPos >= 0 then
    begin
      ptX := SmallInt(Word(charPos and 65535));
      ptY := SmallInt(Word((charPos shr 16) and 65535));
    end
    else
    begin
      ptX := 20;
      ptY := 24;
    end;
  end
  else
  begin
    ptX := 20;
    ptY := 24;
  end;
  {$ELSE}
  ptX := 20;
  ptY := 24;
  {$ENDIF}

  targetX := FMemo.Left + Max(4, ptX);
  if targetX + FMiniBar.Width > FMemo.Parent.ClientWidth - 4 then
    targetX := Max(4, FMemo.Parent.ClientWidth - FMiniBar.Width - 4);

  if ptY >= FMiniBar.Height + 6 then
    targetY := FMemo.Top + ptY - FMiniBar.Height - 4
  else
    targetY := Min(Max(4, FMemo.Parent.ClientHeight - FMiniBar.Height - 28),
                   FMemo.Top + Max(0, ptY) + Abs(FMemo.Font.Height) + 8);

  FMiniBar.Left := targetX;
  FMiniBar.Top := targetY;
  FMiniBar.BringToFront;
  if not FMiniBar.Visible then
    FMiniBar.Visible := True;
  FMiniBar.Invalidate;
end;

procedure TMemoVersiculo.ApplyTagToSelection(const StartTag, EndTag: string);
var
  fullText, selText, beforeSel, afterSel: string;
  selStart, selLen, startLen, endLen: Integer;
begin
  if not Assigned(FMemo) then Exit;
  fullText := FMemo.Text;
  selStart := FMemo.SelStart;
  selLen := FMemo.SelLength;
  selText := FMemo.SelText;
  startLen := Length(StartTag);
  endLen := Length(EndTag);

  FInternalChanging := True;
  try
    if selLen > 0 then
    begin
      if (Length(selText) >= startLen + endLen) and
         AnsiStartsText(StartTag, selText) and
         AnsiEndsText(EndTag, selText) then
      begin
        selText := Copy(selText, startLen + 1, Length(selText) - startLen - endLen);
        FMemo.SelText := selText;
        FMemo.SelStart := selStart;
        FMemo.SelLength := Length(selText);
      end
      else
      begin
        beforeSel := Copy(fullText, selStart - startLen + 1, startLen);
        afterSel := Copy(fullText, selStart + selLen + 1, endLen);
        if (selStart >= startLen) and SameText(beforeSel, StartTag) and SameText(afterSel, EndTag) then
        begin
          Delete(fullText, selStart + selLen + 1, endLen);
          Delete(fullText, selStart - startLen + 1, startLen);
          FMemo.Text := fullText;
          FMemo.SelStart := selStart - startLen;
          FMemo.SelLength := selLen;
        end
        else
        begin
          FMemo.SelText := StartTag + selText + EndTag;
          FMemo.SelStart := selStart + startLen;
          FMemo.SelLength := selLen;
        end;
      end;
    end
    else
    begin
      FMemo.SelText := StartTag + EndTag;
      FMemo.SelStart := selStart + startLen;
      FMemo.SelLength := 0;
    end;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.ApplyHeadingToSelection(Level: Integer);
var
  openTag, closeTag, selText, innerText: string;
  selStart, selLen: Integer;
begin
  if not Assigned(FMemo) then Exit;
  openTag := Format('<TS%d>', [EnsureRange(Level, 1, 3)]);
  closeTag := '<Ts><_CM>';

  selStart := FMemo.SelStart;
  selLen := FMemo.SelLength;
  selText := FMemo.SelText;

  FInternalChanging := True;
  try
    if selLen > 0 then
    begin
      if AnsiStartsText('<TS', selText) and
         (AnsiEndsText('<Ts><_CM>', selText) or AnsiEndsText('<Ts>', selText)) then
      begin
        innerText := selText;
        if AnsiStartsText('<TS1>', innerText) or AnsiStartsText('<TS2>', innerText) or AnsiStartsText('<TS3>', innerText) then
          Delete(innerText, 1, 5)
        else if AnsiStartsText('<TS>', innerText) then
          Delete(innerText, 1, 4);

        if AnsiEndsText('<Ts><_CM>', innerText) then
          Delete(innerText, Length(innerText) - 8, 9)
        else if AnsiEndsText('<Ts>', innerText) then
          Delete(innerText, Length(innerText) - 3, 4);

        if AnsiStartsText(openTag, selText) then
        begin
          FMemo.SelText := innerText;
          FMemo.SelStart := selStart;
          FMemo.SelLength := Length(innerText);
        end
        else
        begin
          FMemo.SelText := openTag + innerText + closeTag;
          FMemo.SelStart := selStart;
          FMemo.SelLength := Length(openTag + innerText + closeTag);
        end;
      end
      else
      begin
        FMemo.SelText := openTag + selText + closeTag;
        FMemo.SelStart := selStart;
        FMemo.SelLength := Length(openTag + selText + closeTag);
      end;
    end
    else
    begin
      FMemo.SelText := openTag + closeTag;
      FMemo.SelStart := selStart + Length(openTag);
      FMemo.SelLength := 0;
    end;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.HandleReturnKey(IsShift: Boolean);
var
  txt, beforePart, afterPart, insertStr: string;
  posIdx: Integer;
begin
  if not Assigned(FMemo) then Exit;
  txt := FMemo.Text;
  posIdx := FMemo.SelStart;

  if FMemo.SelLength > 0 then
  begin
    Delete(txt, posIdx + 1, FMemo.SelLength);
  end;

  beforePart := Copy(txt, 1, posIdx);
  afterPart := Copy(txt, posIdx + 1, MaxInt);

  FInternalChanging := True;
  try
    if IsShift then
    begin
      while (Length(beforePart) > 0) and (beforePart[Length(beforePart)] = ' ') do
        Delete(beforePart, Length(beforePart), 1);
      while (Length(afterPart) > 0) and (afterPart[1] = ' ') do
        Delete(afterPart, 1, 1);

      if (Length(beforePart) > 0) and not AnsiEndsStr(#10, beforePart) then
        insertStr := #13#10 + '<CL>'
      else
        insertStr := '<CL>';

      FMemo.Text := beforePart + insertStr + afterPart;
      FMemo.SelStart := Length(beforePart) + Length(insertStr);
      FMemo.SelLength := 0;
    end
    else
    begin
      // Double Return check: if immediately preceded by <CI>, upgrade <CI> to <CM>
      if AnsiEndsText('<CI>', beforePart) then
      begin
        Delete(beforePart, Length(beforePart) - 3, 4);
        beforePart := beforePart + '<CM>';
        FMemo.Text := beforePart + afterPart;
        FMemo.SelStart := Length(beforePart);
        FMemo.SelLength := 0;
      end
      else if AnsiEndsText('<CM>', beforePart) then
      begin
        // Already <CM>, keep cursor right after <CM>
        FMemo.SelStart := Length(beforePart);
        FMemo.SelLength := 0;
      end
      else
      begin
        // Single Return -> <CI> on a new visual line
        while (Length(beforePart) > 0) and (beforePart[Length(beforePart)] = ' ') do
          Delete(beforePart, Length(beforePart), 1);
        while (Length(afterPart) > 0) and (afterPart[1] = ' ') do
          Delete(afterPart, 1, 1);

        if (Length(beforePart) > 0) and not AnsiEndsStr(#10, beforePart) then
          insertStr := #13#10 + '<CI>'
        else
          insertStr := '<CI>';

        FMemo.Text := beforePart + insertStr + afterPart;
        FMemo.SelStart := Length(beforePart) + Length(insertStr);
        FMemo.SelLength := 0;
      end;
    end;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.InsertParagraphTagAtCursor(const ATag: string);
var
  txt, beforePart, afterPart, insertStr: string;
  posIdx: Integer;
begin
  if not Assigned(FMemo) then Exit;
  txt := FMemo.Text;
  posIdx := FMemo.SelStart;

  beforePart := Copy(txt, 1, posIdx);
  afterPart := Copy(txt, posIdx + 1, MaxInt);

  FInternalChanging := True;
  try
    if AnsiEndsText('<CI>', beforePart) or AnsiEndsText('<CM>', beforePart) or AnsiEndsText('<CL>', beforePart) then
    begin
      Delete(beforePart, Length(beforePart) - 3, 4);
      beforePart := beforePart + ATag;
      FMemo.Text := beforePart + afterPart;
      FMemo.SelStart := Length(beforePart);
      FMemo.SelLength := 0;
    end
    else if AnsiStartsText('<CI>', afterPart) or AnsiStartsText('<CM>', afterPart) or AnsiStartsText('<CL>', afterPart) then
    begin
      Delete(afterPart, 1, 4);
      afterPart := ATag + afterPart;
      FMemo.Text := beforePart + afterPart;
      FMemo.SelStart := Length(beforePart) + Length(ATag);
      FMemo.SelLength := 0;
    end
    else
    begin
      while (Length(beforePart) > 0) and (beforePart[Length(beforePart)] = ' ') do
        Delete(beforePart, Length(beforePart), 1);
      while (Length(afterPart) > 0) and (afterPart[1] = ' ') do
        Delete(afterPart, 1, 1);

      if (Length(beforePart) > 0) and not AnsiEndsStr(#10, beforePart) then
        insertStr := #13#10 + ATag
      else
        insertStr := ATag;

      FMemo.Text := beforePart + insertStr + afterPart;
      FMemo.SelStart := Length(beforePart) + Length(insertStr);
      FMemo.SelLength := 0;
    end;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.GetCurrentLineBounds(out LineStart, LineEnd: Integer; out LineText: string);
var
  txt: string;
  posIdx, len: Integer;
begin
  txt := FMemo.Text;
  len := Length(txt);
  posIdx := EnsureRange(FMemo.SelStart + 1, 1, Max(1, len + 1));

  LineStart := posIdx;
  while (LineStart > 1) and not (txt[LineStart - 1] in [#13, #10]) do
    Dec(LineStart);

  LineEnd := posIdx;
  while (LineEnd <= len) and not (txt[LineEnd] in [#13, #10]) do
    Inc(LineEnd);
  Dec(LineEnd);

  if LineEnd >= LineStart then
    LineText := Copy(txt, LineStart, LineEnd - LineStart + 1)
  else
    LineText := '';
end;

procedure TMemoVersiculo.StepParagraphIndent(IsPI: Boolean; Delta: Integer);
var
  txt, lineText, rest, boundaryPart, piPart, pfPart, centerPart, newLine: string;
  lineStart, lineEnd, curPI, curPF, newVal, pEnd: Integer;
  hasEarlierPI, hasEarlierPF: Boolean;
begin
  if not Assigned(FMemo) then Exit;
  txt := FMemo.Text;
  GetCurrentLineBounds(lineStart, lineEnd, lineText);

  hasEarlierPI := Pos('<PI', UpperCase(Copy(txt, 1, lineStart - 1))) > 0;
  hasEarlierPF := Pos('<PF', UpperCase(Copy(txt, 1, lineStart - 1))) > 0;

  rest := lineText;
  boundaryPart := '';
  piPart := '';
  pfPart := '';
  centerPart := '';
  curPI := 0;
  curPF := 0;

  // 1. Extract leading Headings (<TS..>..<Ts><_CM>) and Paragraph Boundary (<CM>/<CI>/<CL>)
  while True do
  begin
    if AnsiStartsText('<TS', rest) then
    begin
      pEnd := Pos('<Ts>', rest);
      if pEnd = 0 then pEnd := Pos('<TS>', UpperCase(rest));
      if pEnd > 0 then
      begin
        boundaryPart := boundaryPart + Copy(rest, 1, pEnd + 3);
        Delete(rest, 1, pEnd + 3);
        if AnsiStartsText('<_CM>', rest) then
        begin
          boundaryPart := boundaryPart + Copy(rest, 1, 5);
          Delete(rest, 1, 5);
        end;
        continue;
      end;
    end;

    if AnsiStartsText('<CM>', rest) or AnsiStartsText('<CI>', rest) or AnsiStartsText('<CL>', rest) then
    begin
      boundaryPart := boundaryPart + UpperCase(Copy(rest, 1, 4));
      Delete(rest, 1, 4);
      continue;
    end;

    if AnsiStartsText('<_CM>', rest) then
    begin
      boundaryPart := boundaryPart + '<_CM>';
      Delete(rest, 1, 5);
      continue;
    end;

    break;
  end;

  // 2. Extract any existing <PI..>, <PF..>, and <center> tags at the start of the paragraph
  while True do
  begin
    if (Length(rest) >= 5) and AnsiStartsText('<PI', rest) and (rest[4] in ['0'..'7']) and (rest[5] = '>') then
    begin
      curPI := Ord(rest[4]) - Ord('0');
      Delete(rest, 1, 5);
      continue;
    end
    else if AnsiStartsText('<PI>', rest) then
    begin
      if Copy(rest, 1, 4) = '<Pi>' then
        curPI := 0
      else
        curPI := 1;
      Delete(rest, 1, 4);
      continue;
    end
    else if (Length(rest) >= 5) and AnsiStartsText('<PF', rest) and (rest[4] in ['0'..'7']) and (rest[5] = '>') then
    begin
      curPF := Ord(rest[4]) - Ord('0');
      Delete(rest, 1, 5);
      continue;
    end
    else if AnsiStartsText('<center>', rest) then
    begin
      centerPart := '<center>';
      Delete(rest, 1, 8);
      continue;
    end;
    break;
  end;

  if IsPI then
  begin
    newVal := EnsureRange(curPI + Delta, 0, 7);
    if newVal > 0 then
      piPart := Format('<PI%d>', [newVal])
    else if hasEarlierPI and (curPI > 0) then
      piPart := '<PI0>'
    else
      piPart := '';

    if curPF > 0 then
      pfPart := Format('<PF%d>', [curPF]);
  end
  else
  begin
    newVal := EnsureRange(curPF + Delta, 0, 7);
    if curPI > 0 then
      piPart := Format('<PI%d>', [curPI]);

    if newVal > 0 then
      pfPart := Format('<PF%d>', [newVal])
    else if hasEarlierPF and (curPF > 0) then
      pfPart := '<PF0>'
    else
      pfPart := '';
  end;

  // Canonical theWord order: Boundary -> <PI#> -> <PF#> -> <center> -> Text
  newLine := boundaryPart + piPart + pfPart + centerPart + rest;

  FInternalChanging := True;
  try
    Delete(txt, lineStart, lineEnd - lineStart + 1);
    Insert(newLine, txt, lineStart);
    FMemo.Text := txt;
    FMemo.SelStart := lineStart + Length(boundaryPart + piPart + pfPart + centerPart) - 1;
    FMemo.SelLength := 0;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  if Assigned(FRibbonPanel) then
    FRibbonPanel.Invalidate;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.ToggleParagraphCenter;
var
  txt, lineText, newLine: string;
  lineStart, lineEnd, posCenter: Integer;
begin
  if not Assigned(FMemo) then Exit;
  txt := FMemo.Text;
  GetCurrentLineBounds(lineStart, lineEnd, lineText);

  posCenter := Pos('<center>', LowerCase(lineText));
  if posCenter > 0 then
  begin
    newLine := lineText;
    Delete(newLine, posCenter, 8);
  end
  else
  begin
    if AnsiStartsText('<CM>', lineText) or AnsiStartsText('<CI>', lineText) or AnsiStartsText('<CL>', lineText) then
      newLine := Copy(lineText, 1, 4) + '<center>' + Copy(lineText, 5, MaxInt)
    else
      newLine := '<center>' + lineText;
  end;

  FInternalChanging := True;
  try
    Delete(txt, lineStart, lineEnd - lineStart + 1);
    Insert(newLine, txt, lineStart);
    FMemo.Text := txt;
    FMemo.SelStart := lineStart + Length(newLine) - 1;
    FMemo.SelLength := 0;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.ToggleFallbackCM;
var
  txt: string;
  pTs: Integer;
begin
  if not Assigned(FMemo) then Exit;
  txt := FMemo.Text;
  pTs := Pos('<Ts>', txt);
  FInternalChanging := True;
  try
    if pTs > 0 then
    begin
      if SameText(Copy(txt, pTs + 4, 5), '<_CM>') then
        Delete(txt, pTs + 4, 5)
      else
        Insert('<_CM>', txt, pTs + 4);
      FMemo.Text := txt;
    end
    else
    begin
      FMemo.SelText := '<_CM>';
    end;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.ClearFormattingTags;
const
  TagsToStrip: array[0..38] of string = (
    '<CM>', '<CI>', '<CL>', '<_CM>',
    '<PI0>', '<PI1>', '<PI2>', '<PI3>', '<PI4>', '<PI5>', '<PI6>', '<PI7>', '<PI>', '<Pi>',
    '<PF0>', '<PF1>', '<PF2>', '<PF3>', '<PF4>', '<PF5>', '<PF6>', '<PF7>',
    '<center>',
    '<TS>', '<TS0>', '<TS1>', '<TS2>', '<TS3>', '<Ts>',
    '<FI>', '<Fi>', '<FR>', '<Fr>', '<FO>', '<Fo>', '<FU>', '<Fu>',
    '<b>', '</b>'
  );
  MoreTagsToStrip: array[0..9] of string = (
    '<i>', '</i>', '<u>', '</u>', '<s>', '</s>', '<sup>', '</sup>', '<sub>', '</sub>'
  );
var
  targetText: string;
  i, selStart: Integer;
  hasSelection: Boolean;
begin
  if not Assigned(FMemo) then Exit;
  hasSelection := FMemo.SelLength > 0;
  selStart := FMemo.SelStart;
  if hasSelection then
    targetText := FMemo.SelText
  else
    targetText := FMemo.Text;

  for i := Low(TagsToStrip) to High(TagsToStrip) do
    targetText := StringReplace(targetText, TagsToStrip[i], '', [rfReplaceAll, rfIgnoreCase]);
  for i := Low(MoreTagsToStrip) to High(MoreTagsToStrip) do
    targetText := StringReplace(targetText, MoreTagsToStrip[i], '', [rfReplaceAll, rfIgnoreCase]);

  FInternalChanging := True;
  try
    if hasSelection then
    begin
      FMemo.SelText := targetText;
      FMemo.SelStart := selStart;
      FMemo.SelLength := Length(targetText);
    end
    else
    begin
      targetText := StringReplace(targetText, #13#10, ' ', [rfReplaceAll]);
      targetText := StringReplace(targetText, #13, ' ', [rfReplaceAll]);
      targetText := StringReplace(targetText, #10, ' ', [rfReplaceAll]);
      while Pos('  ', targetText) > 0 do
        targetText := StringReplace(targetText, '  ', ' ', [rfReplaceAll]);
      FMemo.Text := Trim(targetText);
    end;
    FModificado := True;
  finally
    FInternalChanging := False;
  end;
  if FMemo.CanFocus then
    FMemo.SetFocus;
  UpdateMiniBarPosition;
  NotifyLivePreview;
end;

procedure TMemoVersiculo.ExecuteFormatCommand(Cmd: TVerseFormatCmd);
var
  pt: TPoint;
begin
  case Cmd of
    fmtTargetBadge:
    begin
      if Assigned(FTargetPopupMenu) and Assigned(FRibbonPanel) then
      begin
        pt := FRibbonPanel.ClientToScreen(Classes.Point(6, FRibbonPanel.Height));
        FTargetPopupMenu.PopUp(pt.X, pt.Y);
      end;
    end;
    fmtCI:          InsertParagraphTagAtCursor('<CI>');
    fmtCM:          InsertParagraphTagAtCursor('<CM>');
    fmtCL:          InsertParagraphTagAtCursor('<CL>');
    fmtPIDec:       StepParagraphIndent(True, -1);
    fmtPIInc:       StepParagraphIndent(True, +1);
    fmtPFDec:       StepParagraphIndent(False, -1);
    fmtPFInc:       StepParagraphIndent(False, +1);
    fmtCenter:      ToggleParagraphCenter;
    fmtTS1:         ApplyHeadingToSelection(1);
    fmtTS2:         ApplyHeadingToSelection(2);
    fmtTS3:         ApplyHeadingToSelection(3);
    fmtFallbackCM:  ToggleFallbackCM;
    fmtFI:          ApplyTagToSelection('<FI>', '<Fi>');
    fmtFR:          ApplyTagToSelection('<FR>', '<Fr>');
    fmtFO:          ApplyTagToSelection('<FO>', '<Fo>');
    fmtBold:        ApplyTagToSelection('<b>', '</b>');
    fmtItalic:      ApplyTagToSelection('<i>', '</i>');
    fmtUnderline:   ApplyTagToSelection('<FU>', '<Fu>');
    fmtSup:         ApplyTagToSelection('<sup>', '</sup>');
    fmtSub:         ApplyTagToSelection('<sub>', '</sub>');
    fmtRF:          ApplyTagToSelection('<RF>', '<Rf>');
    fmtClear:       ClearFormattingTags;
  end;
end;

procedure TMemoVersiculo.OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (ssShift in Shift) then
  begin
    case Key of
      Ord('I'): begin
        ApplyTagToSelection('<FI>', '<Fi>');
        Key := 0;
      end;
      Ord('E'): begin
        ApplyTagToSelection('<FR>', '<Fr>');
        Key := 0;
      end;
      Ord('O'): begin
        ApplyTagToSelection('<FO>', '<Fo>');
        Key := 0;
      end;
      Ord('1'): begin
        ApplyHeadingToSelection(1);
        Key := 0;
      end;
      Ord('2'): begin
        ApplyHeadingToSelection(2);
        Key := 0;
      end;
      Ord('3'): begin
        ApplyHeadingToSelection(3);
        Key := 0;
      end;
    end;
  end
  else if (ssCtrl in Shift) and not (ssShift in Shift) then
  begin
    case Key of
      Ord('B'): begin
        ApplyTagToSelection('<b>', '</b>');
        Key := 0;
      end;
      Ord('I'): begin
        ApplyTagToSelection('<i>', '</i>');
        Key := 0;
      end;
      Ord('U'): begin
        ApplyTagToSelection('<FU>', '<Fu>');
        Key := 0;
      end;
    end;
  end
  else if Key = VK_ESCAPE then
  begin
    ConfirmarAlteracao;
    if Assigned(FVersiculo) and Assigned(FVersiculo.Fonte) then
      FVersiculo.Fonte.Size := FMemo.Font.Size;
  end
  else if Key = VK_RETURN then
  begin
    HandleReturnKey(ssShift in Shift);
    Key := 0;
  end;
end;

procedure TMemoVersiculo.OnChange(Sender: TObject);
begin
  if FInternalChanging then Exit;
  if assigned(TMemo(Sender).Parent) and TMemo(Sender).Visible then
  begin
    FModificado := True;
    NotifyLivePreview;
  end;
end;

procedure TMemoVersiculo.ConfirmarAlteracao;
begin
  Desativar;
end;

procedure TMemoVersiculo.BuildRibbonButtons;
  procedure AddBtn(ACmd: TVerseFormatCmd; AWidth: Integer; const ATop, ASub, AHint: string; AGroupEnd: Boolean = False);
  var
    idx, curX: Integer;
  begin
    idx := Length(FRibbonButtons);
    if idx = 0 then
      curX := 6
    else
    begin
      curX := FRibbonButtons[idx - 1].Bounds.Right + 3;
      if FRibbonButtons[idx - 1].GroupEnd then
        Inc(curX, 8);
    end;
    SetLength(FRibbonButtons, idx + 1);
    FRibbonButtons[idx].Cmd := ACmd;
    FRibbonButtons[idx].Bounds := Classes.Rect(curX, 4, curX + AWidth, 42);
    FRibbonButtons[idx].TopLabel := ATop;
    FRibbonButtons[idx].SubLabel := ASub;
    FRibbonButtons[idx].HintText := AHint;
    FRibbonButtons[idx].GroupEnd := AGroupEnd;
  end;
begin
  SetLength(FRibbonButtons, 0);
  AddBtn(fmtTargetBadge, 134, 'Editing:', 'Destination', 'Click to switch which verse section (Source, Destination, Ref 1, Ref 2) is in Edit mode', True);

  // Paragraph Group
  AddBtn(fmtCI, 40, '¶', '<CI>', 'Tight Paragraph Break (<CI>) — Shortcut: Single Return (Enter)');
  AddBtn(fmtCM, 42, '¶+', '<CM>', 'Spaced Paragraph Break (<CM>) — Shortcut: Double Return (Enter x2)');
  AddBtn(fmtCL, 40, '↵', '<CL>', 'Line Break inside Paragraph (<CL>) — Shortcut: Shift+Return', True);

  // Indentation Group (Step Buttons)
  AddBtn(fmtPIDec, 38, '⇤', 'PI-', 'Decrease Whole-Paragraph Indent (<PI7>..<PI0>)');
  AddBtn(fmtPIInc, 38, '⇥', 'PI+', 'Increase Whole-Paragraph Indent (<PI1>..<PI7>)');
  AddBtn(fmtPFDec, 38, '⇤1', 'PF-', 'Decrease First-Line Indent (<PF7>..<PF0>)');
  AddBtn(fmtPFInc, 38, '⇥1', 'PF+', 'Increase First-Line Indent (<PF1>..<PF7>)');
  AddBtn(fmtCenter, 42, '≣', 'Center', 'Toggle Centered Paragraph (<center>)', True);

  // Headings Group
  AddBtn(fmtTS1, 36, 'H1', '<TS1>', 'Section Heading Level 1 (<TS1>..<Ts><_CM>) — Ctrl+Shift+1');
  AddBtn(fmtTS2, 36, 'H2', '<TS2>', 'Section Heading Level 2 (<TS2>..<Ts><_CM>) — Ctrl+Shift+2');
  AddBtn(fmtTS3, 36, 'H3', '<TS3>', 'Section Heading Level 3 (<TS3>..<Ts><_CM>) — Ctrl+Shift+3');
  AddBtn(fmtFallbackCM, 42, '¶↩', '<_CM>', 'Toggle Heading Paragraph Fallback (<_CM>)', True);

  // Semantic & Font Group
  AddBtn(fmtFI, 40, '[i]', '<FI>', 'Added / Translator''s Words (<FI>..<Fi>) — Ctrl+Shift+I');
  AddBtn(fmtFR, 42, '“J”', '<FR>', 'Words of Jesus in Red (<FR>..<Fr>) — Ctrl+Shift+E');
  AddBtn(fmtFO, 40, 'OT', '<FO>', 'Old Testament Quotation (<FO>..<Fo>) — Ctrl+Shift+O');
  AddBtn(fmtBold, 30, 'B', '<b>', 'Bold (<b>..</b>) — Ctrl+B');
  AddBtn(fmtItalic, 30, 'I', '<i>', 'Italic (<i>..</i>) — Ctrl+I');
  AddBtn(fmtUnderline, 32, 'U', '<FU>', 'Underline (<FU>..<Fu>) — Ctrl+U');
  AddBtn(fmtSup, 32, 'x²', '<sup>', 'Superscript (<sup>..</sup>)');
  AddBtn(fmtSub, 32, 'x₂', '<sub>', 'Subscript (<sub>..</sub>)');
  AddBtn(fmtRF, 36, 'Fn', '<RF>', 'Translator''s Footnote (<RF>..<Rf>)');
  AddBtn(fmtClear, 42, 'Tx', 'Clear', 'Clear Formatting Tags (preserves Strong''s & Morphology tags)');
end;

procedure TMemoVersiculo.BuildMiniButtons;
  procedure AddMini(ACmd: TVerseFormatCmd; AWidth: Integer; const ATop, AHint: string; AGroupEnd: Boolean = False);
  var
    idx, curX: Integer;
  begin
    idx := Length(FMiniButtons);
    if idx = 0 then
      curX := 4
    else
    begin
      curX := FMiniButtons[idx - 1].Bounds.Right + 2;
      if FMiniButtons[idx - 1].GroupEnd then
        Inc(curX, 5);
    end;
    SetLength(FMiniButtons, idx + 1);
    FMiniButtons[idx].Cmd := ACmd;
    FMiniButtons[idx].Bounds := Classes.Rect(curX, 3, curX + AWidth, 27);
    FMiniButtons[idx].TopLabel := ATop;
    FMiniButtons[idx].SubLabel := '';
    FMiniButtons[idx].HintText := AHint;
    FMiniButtons[idx].GroupEnd := AGroupEnd;
  end;
begin
  SetLength(FMiniButtons, 0);
  AddMini(fmtTS1, 26, 'H1', 'Heading 1 (<TS1>..<Ts><_CM>)');
  AddMini(fmtTS2, 26, 'H2', 'Heading 2 (<TS2>..<Ts><_CM>)');
  AddMini(fmtTS3, 26, 'H3', 'Heading 3 (<TS3>..<Ts><_CM>)', True);
  AddMini(fmtFI, 28, '[i]', 'Added Words (<FI>..<Fi>)');
  AddMini(fmtFR, 28, '“J”', 'Words of Jesus (<FR>..<Fr>)');
  AddMini(fmtFO, 28, 'OT', 'OT Quote (<FO>..<Fo>)', True);
  AddMini(fmtBold, 22, 'B', 'Bold (<b>..</b>)');
  AddMini(fmtItalic, 22, 'I', 'Italic (<i>..</i>)');
  AddMini(fmtUnderline, 22, 'U', 'Underline (<FU>..<Fu>)', True);
  AddMini(fmtCI, 28, '¶CI', 'Tight Paragraph (<CI>)');
  AddMini(fmtCM, 30, '¶CM', 'Spaced Paragraph (<CM>)');
  AddMini(fmtCL, 26, '↵CL', 'Line Break (<CL>)', True);
  AddMini(fmtPIDec, 26, 'PI-', 'Decrease Indent (<PI->)');
  AddMini(fmtPIInc, 26, 'PI+', 'Increase Indent (<PI+>)');
  AddMini(fmtClear, 26, 'Tx', 'Clear Formatting Tags');
end;

procedure TMemoVersiculo.DrawFormatIcon(ACanvas: TCanvas; Cmd: TVerseFormatCmd;
  IconRect: TRect; IsHot, IsDark: Boolean);
var
  cx, cy: Integer;
  cCyan, cGold, cBlue, cTeal, cCrimson, cText, cDim: TColor;
begin
  cx := (IconRect.Left + IconRect.Right) div 2;
  cy := (IconRect.Top + IconRect.Bottom) div 2;

  if IsDark then
  begin
    cCyan := MakeRGB(56, 205, 255);
    cGold := MakeRGB(242, 194, 68);
    cBlue := MakeRGB(96, 175, 255);
    cTeal := MakeRGB(78, 201, 176);
    cCrimson := MakeRGB(244, 92, 92);
    cText := CurrentSkin.PanelText;
    cDim := CurrentSkin.WindowTextDim;
  end
  else
  begin
    cCyan := MakeRGB(0, 120, 190);
    cGold := MakeRGB(185, 120, 0);
    cBlue := MakeRGB(20, 100, 200);
    cTeal := MakeRGB(0, 135, 115);
    cCrimson := MakeRGB(200, 30, 30);
    cText := CurrentSkin.PanelText;
    cDim := CurrentSkin.WindowTextDim;
  end;

  if IsHot and CurrentSkin.IsDark then
    cText := clWhite;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Name := 'Segoe UI';

  case Cmd of
    fmtCI:
    begin
      // Cyan Pilcrow ¶
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := cCyan;
      ACanvas.Pen.Color := cCyan;
      ACanvas.Ellipse(cx - 6, cy - 6, cx + 1, cy + 1);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Pen.Width := 2;
      ACanvas.MoveTo(cx - 1, cy - 6);
      ACanvas.LineTo(cx - 1, cy + 6);
      ACanvas.MoveTo(cx + 3, cy - 6);
      ACanvas.LineTo(cx + 3, cy + 6);
      ACanvas.MoveTo(cx - 3, cy - 6);
      ACanvas.LineTo(cx + 5, cy - 6);
      ACanvas.Pen.Width := 1;
    end;

    fmtCM:
    begin
      // Gold Double Pilcrow ¶+
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := cGold;
      ACanvas.Pen.Color := cGold;
      ACanvas.Ellipse(cx - 8, cy - 6, cx - 2, cy);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Pen.Width := 2;
      ACanvas.MoveTo(cx - 4, cy - 6);
      ACanvas.LineTo(cx - 4, cy + 6);
      ACanvas.MoveTo(cx - 1, cy - 6);
      ACanvas.LineTo(cx - 1, cy + 6);
      // Plus sign on right
      ACanvas.MoveTo(cx + 5, cy - 3);
      ACanvas.LineTo(cx + 5, cy + 4);
      ACanvas.MoveTo(cx + 2, cy);
      ACanvas.LineTo(cx + 9, cy);
      ACanvas.Pen.Width := 1;
    end;

    fmtCL:
    begin
      // Blue bent Return Arrow ↵
      ACanvas.Pen.Color := cBlue;
      ACanvas.Pen.Width := 2;
      ACanvas.MoveTo(cx + 5, cy - 5);
      ACanvas.LineTo(cx + 5, cy + 2);
      ACanvas.LineTo(cx - 5, cy + 2);
      ACanvas.MoveTo(cx - 2, cy - 1);
      ACanvas.LineTo(cx - 6, cy + 2);
      ACanvas.LineTo(cx - 2, cy + 5);
      ACanvas.Pen.Width := 1;
    end;

    fmtPIDec:
    begin
      // Left margin line + left arrow + text lines
      ACanvas.Pen.Color := cText;
      ACanvas.MoveTo(cx - 7, cy - 6);
      ACanvas.LineTo(cx - 7, cy + 6);
      // Text lines on right
      ACanvas.Pen.Color := cDim;
      ACanvas.MoveTo(cx, cy - 5); ACanvas.LineTo(cx + 7, cy - 5);
      ACanvas.MoveTo(cx, cy - 1); ACanvas.LineTo(cx + 7, cy - 1);
      ACanvas.MoveTo(cx, cy + 3); ACanvas.LineTo(cx + 7, cy + 3);
      // Left arrow
      ACanvas.Pen.Color := cCyan;
      ACanvas.MoveTo(cx - 1, cy); ACanvas.LineTo(cx - 6, cy);
      ACanvas.MoveTo(cx - 3, cy - 2); ACanvas.LineTo(cx - 5, cy);
      ACanvas.MoveTo(cx - 3, cy + 2); ACanvas.LineTo(cx - 5, cy);
    end;

    fmtPIInc:
    begin
      // Left margin line + right arrow + indented text lines
      ACanvas.Pen.Color := cText;
      ACanvas.MoveTo(cx - 7, cy - 6);
      ACanvas.LineTo(cx - 7, cy + 6);
      // Text lines on right
      ACanvas.Pen.Color := cDim;
      ACanvas.MoveTo(cx + 1, cy - 5); ACanvas.LineTo(cx + 7, cy - 5);
      ACanvas.MoveTo(cx + 1, cy - 1); ACanvas.LineTo(cx + 7, cy - 1);
      ACanvas.MoveTo(cx + 1, cy + 3); ACanvas.LineTo(cx + 7, cy + 3);
      // Right arrow
      ACanvas.Pen.Color := cCyan;
      ACanvas.MoveTo(cx - 5, cy); ACanvas.LineTo(cx, cy);
      ACanvas.MoveTo(cx - 2, cy - 2); ACanvas.LineTo(cx, cy);
      ACanvas.MoveTo(cx - 2, cy + 2); ACanvas.LineTo(cx, cy);
    end;

    fmtPFDec, fmtPFInc:
    begin
      // First-line indent icon: top arrow + 3 lines
      ACanvas.Pen.Color := cGold;
      if Cmd = fmtPFInc then
      begin
        ACanvas.MoveTo(cx - 7, cy - 4); ACanvas.LineTo(cx - 1, cy - 4);
        ACanvas.MoveTo(cx - 3, cy - 6); ACanvas.LineTo(cx - 1, cy - 4);
        ACanvas.MoveTo(cx - 3, cy - 2); ACanvas.LineTo(cx - 1, cy - 4);
      end
      else
      begin
        ACanvas.MoveTo(cx - 1, cy - 4); ACanvas.LineTo(cx - 7, cy - 4);
        ACanvas.MoveTo(cx - 5, cy - 6); ACanvas.LineTo(cx - 7, cy - 4);
        ACanvas.MoveTo(cx - 5, cy - 2); ACanvas.LineTo(cx - 7, cy - 4);
      end;
      ACanvas.Pen.Color := cText;
      ACanvas.MoveTo(cx + 1, cy - 4); ACanvas.LineTo(cx + 7, cy - 4);
      ACanvas.Pen.Color := cDim;
      ACanvas.MoveTo(cx - 7, cy + 1); ACanvas.LineTo(cx + 7, cy + 1);
      ACanvas.MoveTo(cx - 7, cy + 5); ACanvas.LineTo(cx + 5, cy + 5);
    end;

    fmtCenter:
    begin
      ACanvas.Pen.Color := cText;
      ACanvas.MoveTo(cx - 7, cy - 5); ACanvas.LineTo(cx + 7, cy - 5);
      ACanvas.MoveTo(cx - 4, cy - 1); ACanvas.LineTo(cx + 4, cy - 1);
      ACanvas.MoveTo(cx - 6, cy + 3); ACanvas.LineTo(cx + 6, cy + 3);
      ACanvas.MoveTo(cx - 3, cy + 6); ACanvas.LineTo(cx + 3, cy + 6);
    end;

    fmtTS1, fmtTS2, fmtTS3:
    begin
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 9;
      ACanvas.Font.Color := cTeal;
      if Cmd = fmtTS1 then
        ACanvas.TextOut(cx - ACanvas.TextWidth('H1') div 2, cy - ACanvas.TextHeight('H1') div 2, 'H1')
      else if Cmd = fmtTS2 then
        ACanvas.TextOut(cx - ACanvas.TextWidth('H2') div 2, cy - ACanvas.TextHeight('H2') div 2, 'H2')
      else
        ACanvas.TextOut(cx - ACanvas.TextWidth('H3') div 2, cy - ACanvas.TextHeight('H3') div 2, 'H3');
    end;

    fmtFallbackCM:
    begin
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 8;
      ACanvas.Font.Color := cGold;
      ACanvas.TextOut(cx - ACanvas.TextWidth('¶_') div 2, cy - ACanvas.TextHeight('¶_') div 2, '¶_');
    end;

    fmtFI:
    begin
      ACanvas.Font.Style := [fsItalic, fsBold];
      ACanvas.Font.Size := 9;
      ACanvas.Font.Color := cText;
      ACanvas.TextOut(cx - ACanvas.TextWidth('[i]') div 2, cy - ACanvas.TextHeight('[i]') div 2, '[i]');
    end;

    fmtFR:
    begin
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 9;
      ACanvas.Font.Color := cCrimson;
      ACanvas.TextOut(cx - ACanvas.TextWidth('“J”') div 2, cy - ACanvas.TextHeight('“J”') div 2, '“J”');
    end;

    fmtFO:
    begin
      // Scroll box outline + OT
      ACanvas.Pen.Color := cGold;
      ACanvas.Rectangle(cx - 8, cy - 6, cx + 8, cy + 6);
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 7;
      ACanvas.Font.Color := cGold;
      ACanvas.TextOut(cx - ACanvas.TextWidth('OT') div 2, cy - ACanvas.TextHeight('OT') div 2, 'OT');
    end;

    fmtBold:
    begin
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 9;
      ACanvas.Font.Color := cText;
      ACanvas.TextOut(cx - ACanvas.TextWidth('B') div 2, cy - ACanvas.TextHeight('B') div 2, 'B');
    end;

    fmtItalic:
    begin
      ACanvas.Font.Style := [fsItalic, fsBold];
      ACanvas.Font.Size := 9;
      ACanvas.Font.Color := cText;
      ACanvas.TextOut(cx - ACanvas.TextWidth('I') div 2, cy - ACanvas.TextHeight('I') div 2, 'I');
    end;

    fmtUnderline:
    begin
      ACanvas.Font.Style := [fsUnderline, fsBold];
      ACanvas.Font.Size := 9;
      ACanvas.Font.Color := cText;
      ACanvas.TextOut(cx - ACanvas.TextWidth('U') div 2, cy - ACanvas.TextHeight('U') div 2, 'U');
    end;

    fmtSup:
    begin
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 8;
      ACanvas.Font.Color := cText;
      ACanvas.TextOut(cx - ACanvas.TextWidth('x²') div 2, cy - ACanvas.TextHeight('x²') div 2, 'x²');
    end;

    fmtSub:
    begin
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 8;
      ACanvas.Font.Color := cText;
      ACanvas.TextOut(cx - ACanvas.TextWidth('x₂') div 2, cy - ACanvas.TextHeight('x₂') div 2, 'x₂');
    end;

    fmtRF:
    begin
      ACanvas.Pen.Color := cBlue;
      ACanvas.RoundRect(cx - 8, cy - 6, cx + 8, cy + 6, 3, 3);
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 7;
      ACanvas.Font.Color := cBlue;
      ACanvas.TextOut(cx - ACanvas.TextWidth('Fn') div 2, cy - ACanvas.TextHeight('Fn') div 2, 'Fn');
    end;

    fmtClear:
    begin
      ACanvas.Font.Style := [fsBold];
      ACanvas.Font.Size := 8;
      ACanvas.Font.Color := cCrimson;
      ACanvas.TextOut(cx - ACanvas.TextWidth('Tx') div 2, cy - ACanvas.TextHeight('Tx') div 2, 'Tx');
    end;
  end;
end;

procedure TMemoVersiculo.OnRibbonPaint(Sender: TObject);
var
  bmp: Graphics.TBitmap;
  i, sepX: Integer;
  rc, iconRc: TRect;
  isHot, isPushed: Boolean;
  badgeText, subTxt: string;
begin
  if not Assigned(FRibbonPanel) then Exit;
  bmp := Graphics.TBitmap.Create;
  try
    bmp.SetSize(Max(1, FRibbonPanel.ClientWidth), Max(1, FRibbonPanel.ClientHeight));
    bmp.Canvas.Brush.Color := CurrentSkin.PanelBg;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

    // Bottom border line of ribbon
    bmp.Canvas.Pen.Color := CurrentSkin.PanelBorder;
    bmp.Canvas.MoveTo(0, bmp.Height - 1);
    bmp.Canvas.LineTo(bmp.Width, bmp.Height - 1);

    for i := 0 to High(FRibbonButtons) do
    begin
      rc := FRibbonButtons[i].Bounds;
      isHot := (i = FHotRibbonIdx);
      isPushed := (i = FPushedRibbonIdx);

      if FRibbonButtons[i].Cmd = fmtTargetBadge then
      begin
        // Glowing blue target badge: "Editing: [Section] ▾"
        bmp.Canvas.Brush.Style := bsSolid;
        if isPushed then
          bmp.Canvas.Brush.Color := CurrentSkin.HighlightBg
        else
          bmp.Canvas.Brush.Color := CurrentSkin.PointedBg;
        bmp.Canvas.Pen.Color := CurrentSkin.HighlightBg;
        bmp.Canvas.RoundRect(rc.Left, rc.Top + 2, rc.Right, rc.Bottom - 2, 8, 8);

        // Glowing indicator dot
        bmp.Canvas.Brush.Color := MakeRGB(80, 250, 123);
        bmp.Canvas.Pen.Color := MakeRGB(80, 250, 123);
        bmp.Canvas.Ellipse(rc.Left + 8, rc.Top + 10, rc.Left + 16, rc.Top + 18);

        bmp.Canvas.Brush.Style := bsClear;
        bmp.Canvas.Font.Name := 'Segoe UI';
        bmp.Canvas.Font.Size := 7;
        bmp.Canvas.Font.Style := [];
        bmp.Canvas.Font.Color := MakeRGB(210, 230, 255);
        bmp.Canvas.TextOut(rc.Left + 22, rc.Top + 5, 'Editing Section:');

        badgeText := FActiveSectionName + ' ▾';
        bmp.Canvas.Font.Size := 8;
        bmp.Canvas.Font.Style := [fsBold];
        bmp.Canvas.Font.Color := clWhite;
        bmp.Canvas.TextOut(rc.Left + 22, rc.Top + 18, badgeText);
      end
      else
      begin
        // Standard Ribbon Button
        bmp.Canvas.Brush.Style := bsSolid;
        if isPushed then
        begin
          bmp.Canvas.Brush.Color := CurrentSkin.HighlightBg;
          bmp.Canvas.Pen.Color := CurrentSkin.HighlightBg;
          bmp.Canvas.RoundRect(rc.Left, rc.Top, rc.Right, rc.Bottom, 4, 4);
        end
        else if isHot then
        begin
          bmp.Canvas.Brush.Color := CurrentSkin.PointedBg;
          bmp.Canvas.Pen.Color := CurrentSkin.PanelBorder;
          bmp.Canvas.RoundRect(rc.Left, rc.Top, rc.Right, rc.Bottom, 4, 4);
        end
        else
        begin
          bmp.Canvas.Brush.Color := CurrentSkin.WindowBg;
          bmp.Canvas.Pen.Color := CurrentSkin.PanelBorder;
          bmp.Canvas.RoundRect(rc.Left, rc.Top, rc.Right, rc.Bottom, 4, 4);
        end;

        iconRc := Classes.Rect(rc.Left + 2, rc.Top + 2, rc.Right - 2, rc.Top + 22);
        DrawFormatIcon(bmp.Canvas, FRibbonButtons[i].Cmd, iconRc, isHot or isPushed, CurrentSkin.IsDark);

        // SubLabel (<CI>, <CM>, PI+, etc.)
        subTxt := FRibbonButtons[i].SubLabel;
        bmp.Canvas.Brush.Style := bsClear;
        bmp.Canvas.Font.Name := 'Segoe UI';
        bmp.Canvas.Font.Size := 7;
        bmp.Canvas.Font.Style := [];
        if isHot or isPushed then
          bmp.Canvas.Font.Color := CurrentSkin.PointedText
        else
          bmp.Canvas.Font.Color := CurrentSkin.WindowTextDim;
        bmp.Canvas.TextOut(
          (rc.Left + rc.Right - bmp.Canvas.TextWidth(subTxt)) div 2,
          rc.Bottom - 14,
          subTxt
        );
      end;

      if FRibbonButtons[i].GroupEnd then
      begin
        sepX := rc.Right + 5;
        bmp.Canvas.Pen.Color := CurrentSkin.PanelBorder;
        bmp.Canvas.MoveTo(sepX, 6);
        bmp.Canvas.LineTo(sepX, bmp.Height - 6);
      end;
    end;

    FRibbonPanel.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TMemoVersiculo.OnRibbonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i, newHot: Integer;
begin
  newHot := -1;
  for i := 0 to High(FRibbonButtons) do
    if ((X >= FRibbonButtons[i].Bounds.Left) and (X < FRibbonButtons[i].Bounds.Right) and (Y >= FRibbonButtons[i].Bounds.Top) and (Y < FRibbonButtons[i].Bounds.Bottom)) then
    begin
      newHot := i;
      break;
    end;

  if newHot <> FHotRibbonIdx then
  begin
    FHotRibbonIdx := newHot;
    if newHot >= 0 then
    begin
      FRibbonPanel.Hint := FRibbonButtons[newHot].HintText;
      FRibbonPanel.ShowHint := True;
    end
    else
      FRibbonPanel.Hint := '';
    FRibbonPanel.Invalidate;
  end;
end;

procedure TMemoVersiculo.OnRibbonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if Button <> mbLeft then Exit;
  for i := 0 to High(FRibbonButtons) do
    if ((X >= FRibbonButtons[i].Bounds.Left) and (X < FRibbonButtons[i].Bounds.Right) and (Y >= FRibbonButtons[i].Bounds.Top) and (Y < FRibbonButtons[i].Bounds.Bottom)) then
    begin
      FPushedRibbonIdx := i;
      FRibbonPanel.Invalidate;
      break;
    end;
end;

procedure TMemoVersiculo.OnRibbonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  clickedIdx: Integer;
begin
  if Button <> mbLeft then Exit;
  clickedIdx := FPushedRibbonIdx;
  FPushedRibbonIdx := -1;
  FRibbonPanel.Invalidate;

  if (clickedIdx >= 0) and (clickedIdx <= High(FRibbonButtons)) and
     ((X >= FRibbonButtons[clickedIdx].Bounds.Left) and (X < FRibbonButtons[clickedIdx].Bounds.Right) and (Y >= FRibbonButtons[clickedIdx].Bounds.Top) and (Y < FRibbonButtons[clickedIdx].Bounds.Bottom)) then
  begin
    ExecuteFormatCommand(FRibbonButtons[clickedIdx].Cmd);
  end;
end;

procedure TMemoVersiculo.OnRibbonMouseLeave(Sender: TObject);
begin
  if (FHotRibbonIdx <> -1) or (FPushedRibbonIdx <> -1) then
  begin
    FHotRibbonIdx := -1;
    FPushedRibbonIdx := -1;
    FRibbonPanel.Invalidate;
  end;
end;

procedure TMemoVersiculo.OnRibbonResize(Sender: TObject);
begin
  if Assigned(FRibbonPanel) then
    FRibbonPanel.Invalidate;
end;

procedure TMemoVersiculo.OnMiniBarPaint(Sender: TObject);
var
  bmp: Graphics.TBitmap;
  i, sepX: Integer;
  rc: TRect;
  isHot, isPushed: Boolean;
begin
  if not Assigned(FMiniBar) then Exit;
  bmp := Graphics.TBitmap.Create;
  try
    bmp.SetSize(Max(1, FMiniBar.ClientWidth), Max(1, FMiniBar.ClientHeight));
    bmp.Canvas.Brush.Color := CurrentSkin.PanelBg;
    bmp.Canvas.Pen.Color := CurrentSkin.PointedBg;
    bmp.Canvas.Rectangle(0, 0, bmp.Width, bmp.Height);

    for i := 0 to High(FMiniButtons) do
    begin
      rc := FMiniButtons[i].Bounds;
      isHot := (i = FHotMiniIdx);
      isPushed := (i = FPushedMiniIdx);

      bmp.Canvas.Brush.Style := bsSolid;
      if isPushed then
      begin
        bmp.Canvas.Brush.Color := CurrentSkin.HighlightBg;
        bmp.Canvas.Pen.Color := CurrentSkin.HighlightBg;
        bmp.Canvas.RoundRect(rc.Left, rc.Top, rc.Right, rc.Bottom, 3, 3);
      end
      else if isHot then
      begin
        bmp.Canvas.Brush.Color := CurrentSkin.PointedBg;
        bmp.Canvas.Pen.Color := CurrentSkin.PanelBorder;
        bmp.Canvas.RoundRect(rc.Left, rc.Top, rc.Right, rc.Bottom, 3, 3);
      end
      else
      begin
        bmp.Canvas.Brush.Color := CurrentSkin.WindowBg;
        bmp.Canvas.Pen.Color := CurrentSkin.PanelBorder;
        bmp.Canvas.RoundRect(rc.Left, rc.Top, rc.Right, rc.Bottom, 3, 3);
      end;

      DrawFormatIcon(bmp.Canvas, FMiniButtons[i].Cmd, rc, isHot or isPushed, CurrentSkin.IsDark);

      if FMiniButtons[i].GroupEnd then
      begin
        sepX := rc.Right + 3;
        bmp.Canvas.Pen.Color := CurrentSkin.PanelBorder;
        bmp.Canvas.MoveTo(sepX, 5);
        bmp.Canvas.LineTo(sepX, bmp.Height - 5);
      end;
    end;

    FMiniBar.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TMemoVersiculo.OnMiniBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i, newHot: Integer;
begin
  newHot := -1;
  for i := 0 to High(FMiniButtons) do
    if ((X >= FMiniButtons[i].Bounds.Left) and (X < FMiniButtons[i].Bounds.Right) and (Y >= FMiniButtons[i].Bounds.Top) and (Y < FMiniButtons[i].Bounds.Bottom)) then
    begin
      newHot := i;
      break;
    end;

  if newHot <> FHotMiniIdx then
  begin
    FHotMiniIdx := newHot;
    if newHot >= 0 then
    begin
      FMiniBar.Hint := FMiniButtons[newHot].HintText;
      FMiniBar.ShowHint := True;
    end
    else
      FMiniBar.Hint := '';
    FMiniBar.Invalidate;
  end;
end;

procedure TMemoVersiculo.OnMiniBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if Button <> mbLeft then Exit;
  for i := 0 to High(FMiniButtons) do
    if ((X >= FMiniButtons[i].Bounds.Left) and (X < FMiniButtons[i].Bounds.Right) and (Y >= FMiniButtons[i].Bounds.Top) and (Y < FMiniButtons[i].Bounds.Bottom)) then
    begin
      FPushedMiniIdx := i;
      FMiniBar.Invalidate;
      break;
    end;
end;

procedure TMemoVersiculo.OnMiniBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  clickedIdx: Integer;
begin
  if Button <> mbLeft then Exit;
  clickedIdx := FPushedMiniIdx;
  FPushedMiniIdx := -1;
  FMiniBar.Invalidate;

  if (clickedIdx >= 0) and (clickedIdx <= High(FMiniButtons)) and
     ((X >= FMiniButtons[clickedIdx].Bounds.Left) and (X < FMiniButtons[clickedIdx].Bounds.Right) and (Y >= FMiniButtons[clickedIdx].Bounds.Top) and (Y < FMiniButtons[clickedIdx].Bounds.Bottom)) then
  begin
    ExecuteFormatCommand(FMiniButtons[clickedIdx].Cmd);
  end;
end;

procedure TMemoVersiculo.OnMiniBarMouseLeave(Sender: TObject);
begin
  if (FHotMiniIdx <> -1) or (FPushedMiniIdx <> -1) then
  begin
    FHotMiniIdx := -1;
    FPushedMiniIdx := -1;
    FMiniBar.Invalidate;
  end;
end;

procedure TMemoVersiculo.OnTargetMenuItemClick(Sender: TObject);
begin
  if (Sender is TMenuItem) and Assigned(FEventoTrocarSecaoEdit) then
    FEventoTrocarSecaoEdit(TMenuItem(Sender).Tag);
end;

procedure TMemoVersiculo.AttachRibbonHost(AHost: TWinControl);
begin
  FRibbonHost := AHost;
  if Assigned(FRibbonPanel) and Assigned(FRibbonHost) then
  begin
    FRibbonPanel.Parent := FRibbonHost;
    FRibbonPanel.Top := 0;
    FRibbonPanel.Align := alTop;
  end;
end;

procedure TMemoVersiculo.SetActiveSectionInfo(ASectionIndex: Integer; const ASectionName: string);
var
  i: Integer;
begin
  FActiveSectionIndex := ASectionIndex;
  FActiveSectionName := ASectionName;
  if Assigned(FTargetPopupMenu) then
    for i := 0 to FTargetPopupMenu.Items.Count - 1 do
      FTargetPopupMenu.Items[i].Checked := (FTargetPopupMenu.Items[i].Tag = ASectionIndex);
  if Assigned(FRibbonPanel) then
    FRibbonPanel.Invalidate;
end;

constructor TMemoVersiculo.Criar;
const
  SecNames: array[0..3] of string = ('Source (1)', 'Destination (2)', 'Reference 1 (3)', 'Reference 2 (4)');
var
  i: Integer;
  mi: TMenuItem;
begin
  FModificado := False;
  FInternalChanging := False;
  FVersiculo := nil;
  FActiveSectionIndex := 1;
  FActiveSectionName := 'Destination';
  FEventoTextoModificado := nil;
  FEventoLivePreview := nil;
  FEventoTrocarSecaoEdit := nil;
  FRibbonHost := nil;
  FHotRibbonIdx := -1;
  FPushedRibbonIdx := -1;
  FHotMiniIdx := -1;
  FPushedMiniIdx := -1;

  FMemo := TMemo.Create(nil);
  FMemo.Visible      := False;
  FMemo.Align        := alClient;
  FMemo.ScrollBars   := ssAutoVertical;
  FMemo.WantReturns  := False;
  FMemo.OnKeyDown    := @OnKeyDown;
  FMemo.OnKeyUp      := @OnMemoKeyUp;
  FMemo.OnKeyPress   := @OnMemoKeyPress;
  FMemo.OnMouseUp    := @OnMemoMouseUp;
  FMemo.OnChange     := @OnChange;
  FMemo.OnMouseWheel := @OnMouseWheel;
  FMemo.ParentFont   := False;
  FMemo.Font.Name    := 'Courier New';
  FMemo.Color        := CurrentSkin.WindowBg;
  FMemo.Font.Color   := CurrentSkin.WindowText;

  BuildRibbonButtons;
  BuildMiniButtons;

  FRibbonPanel := TPanel.Create(nil);
  FRibbonPanel.Visible := False;
  FRibbonPanel.Height := 46;
  FRibbonPanel.BevelOuter := bvNone;
  FRibbonPanel.Caption := '';
  FRibbonPanel.Color := CurrentSkin.PanelBg;
  FRibbonPanel.ShowHint := True;
  FRibbonPanel.OnPaint := @OnRibbonPaint;
  FRibbonPanel.OnMouseMove := @OnRibbonMouseMove;
  FRibbonPanel.OnMouseDown := @OnRibbonMouseDown;
  FRibbonPanel.OnMouseUp := @OnRibbonMouseUp;
  FRibbonPanel.OnMouseLeave := @OnRibbonMouseLeave;
  FRibbonPanel.OnResize := @OnRibbonResize;

  FMiniBar := TPanel.Create(nil);
  FMiniBar.Visible := False;
  FMiniBar.Height := 30;
  if Length(FMiniButtons) > 0 then
    FMiniBar.Width := FMiniButtons[High(FMiniButtons)].Bounds.Right + 6
  else
    FMiniBar.Width := 410;
  FMiniBar.BevelOuter := bvNone;
  FMiniBar.Caption := '';
  FMiniBar.Color := CurrentSkin.PanelBg;
  FMiniBar.ShowHint := True;
  FMiniBar.OnPaint := @OnMiniBarPaint;
  FMiniBar.OnMouseMove := @OnMiniBarMouseMove;
  FMiniBar.OnMouseDown := @OnMiniBarMouseDown;
  FMiniBar.OnMouseUp := @OnMiniBarMouseUp;
  FMiniBar.OnMouseLeave := @OnMiniBarMouseLeave;

  FTargetPopupMenu := TPopupMenu.Create(nil);
  for i := 0 to 3 do
  begin
    mi := TMenuItem.Create(FTargetPopupMenu);
    mi.Caption := 'Edit ' + SecNames[i];
    mi.Tag := i;
    mi.RadioItem := True;
    mi.Checked := (i = 1);
    mi.OnClick := @OnTargetMenuItemClick;
    FTargetPopupMenu.Items.Add(mi);
  end;

  FSelTimer := TTimer.Create(nil);
  FSelTimer.Interval := 120;
  FSelTimer.Enabled := False;
  FSelTimer.OnTimer := @OnSelTimer;
end;

destructor TMemoVersiculo.Destruir;
begin
  if Assigned(FSelTimer) then
  begin
    FSelTimer.Enabled := False;
    FreeAndNil(FSelTimer);
  end;
  if Assigned(FTargetPopupMenu) then
    FreeAndNil(FTargetPopupMenu);
  if Assigned(FMiniBar) then
  begin
    FMiniBar.Parent := nil;
    FreeAndNil(FMiniBar);
  end;
  if Assigned(FRibbonPanel) then
  begin
    FRibbonPanel.Parent := nil;
    FreeAndNil(FRibbonPanel);
  end;
  FMemo.Free;
end;

procedure TMemoVersiculo.Ativar(conteiner: TScrollBox; versiculo: TVersiculo);
begin
  if FMemo.Visible and (FMemo.Parent = conteiner) and (FVersiculo = versiculo) then
  begin
    if Assigned(FRibbonPanel) and Assigned(FRibbonHost) then
    begin
      FRibbonPanel.Visible := True;
      FRibbonPanel.Invalidate;
    end;
    FMemo.BringToFront;
    if assigned(versiculo) then
      versiculo.RepositionTabPanel;
    if FMemo.CanFocus then
      FMemo.SetFocus;
    Exit;
  end;

  if FMemo.Visible and (FVersiculo <> versiculo) then
  begin
    Desativar;
  end;

  if assigned(versiculo) and assigned(versiculo.Fonte) then
    FMemo.Font.Size := versiculo.Fonte.Size;
  FMemo.Color      := CurrentSkin.WindowBg;
  FMemo.Font.Color := CurrentSkin.WindowText;
  FModificado      := False;
  FVersiculo       := versiculo;

  FInternalChanging := True;
  try
    FMemo.Parent := nil;
    if assigned(versiculo) then
      FMemo.Text := ExpandForVisualEdit(versiculo.XML)
    else
      FMemo.Text := '';
  finally
    FInternalChanging := False;
  end;

  FMemo.Parent := conteiner;
  FMemo.BringToFront;
  FMemo.Visible := True;
  ApplyDarkModeToControl(FMemo, CurrentSkin.IsDark);

  if Assigned(FRibbonPanel) and Assigned(FRibbonHost) then
  begin
    if FRibbonPanel.Parent <> FRibbonHost then
    begin
      FRibbonPanel.Parent := FRibbonHost;
      FRibbonPanel.Top := 0;
      FRibbonPanel.Align := alTop;
    end;
    FRibbonPanel.Visible := True;
    FRibbonPanel.Invalidate;
  end;

  if Assigned(FSelTimer) then
    FSelTimer.Enabled := True;

  if assigned(versiculo) then
  begin
    versiculo.RepositionTabPanel;
    if versiculo.ViewMode <> vvmEdit then
      versiculo.ViewMode := vvmEdit;
  end;

  if FMemo.CanFocus then
    FMemo.SetFocus;
end;

procedure TMemoVersiculo.Desativar;
var
  oldVersiculo: TVersiculo;
  wasMod: Boolean;
begin
  if not FMemo.Visible then
    exit;

  if Assigned(FSelTimer) then
    FSelTimer.Enabled := False;
  if Assigned(FMiniBar) then
  begin
    FMiniBar.Visible := False;
    FMiniBar.Parent := nil;
  end;
  if Assigned(FRibbonPanel) then
    FRibbonPanel.Visible := False;

  FMemo.Visible := False;
  oldVersiculo := FVersiculo;
  wasMod := FModificado;
  FModificado := False;

  if assigned(FEventoTextoModificado) and wasMod then
    FEventoTextoModificado(self);

  if assigned(oldVersiculo) and (oldVersiculo.ViewMode = vvmEdit) then
    oldVersiculo.ViewMode := vvmAssoc;
end;

procedure TMemoVersiculo.ApplySkin(const ASkin: TSkinColors);
begin
  if assigned(FMemo) then
  begin
    FMemo.Color := ASkin.WindowBg;
    FMemo.Font.Color := ASkin.WindowText;
    ApplyDarkModeToControl(FMemo, ASkin.IsDark);
  end;
  if assigned(FRibbonPanel) then
  begin
    FRibbonPanel.Color := ASkin.PanelBg;
    FRibbonPanel.Invalidate;
  end;
  if assigned(FMiniBar) then
  begin
    FMiniBar.Color := ASkin.PanelBg;
    FMiniBar.Invalidate;
  end;
end;

end.
