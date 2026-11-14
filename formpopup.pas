unit FormDictionaryPopup;

{$mode objfpc}{$H+}

interface

uses
  ui_style, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  ExtCtrls, ComCtrls, LCLType, RichMemo, Types, Math;

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
  private
    { private declarations }
    FStrongs: TStringList;
    FMorphologies: TStringList;
  public
    { public declarations }
    procedure ShowRtf(Memo: TRichMemo; const Rtf: string);
    procedure AddStrong(const strong: string; const definition: string);
    procedure AddMorphology(const morfo: string; const definition: string);
    procedure ShowAt(x: Integer; y: Integer);
    procedure HidePopup;
    property Strongs: TStringList read FStrongs;
    property Morphologies: TStringList read FMorphologies;
  public
    procedure AfterConstruction; override;
  end;

var
  FrmDictionaryPopup: TFrmDictionaryPopup;

implementation

uses formPrincipal, unitskins;

{ Dictionary entries are RTF with their own colour table (black text, maroon
  headwords, dark blue links) and an "auto" colour that RichEdit shows as
  black. On a dark skin the auto entry becomes the skin's note text, the dark
  colours are swapped for light ones, and each paragraph is reset to it.
  Taken from the 1.0.1 mod (formpopup.pas). }
function AdaptRtfForSkin(const ARtf: string; const ASkin: TSkinColors): string;
var
  lightColorStr: string;
  rgbCol: LongInt;
  sb: TStringBuilder;
  i, len: Integer;
begin
  Result := ARtf;
  if (Result = '') or not ASkin.IsDark then exit;

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

  // 2. Word-boundary scanner to ensure every \pard, \plain, \par defaults to \cf0 (ASkin.NoteText),
  //    not \cf1 (which in many dictionaries is maroon/red for headwords!).
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

procedure TFrmDictionaryPopup.ShowRtf(Memo: TRichMemo; const Rtf: string);
begin
  Memo.ParentColor := False;
  Memo.ParentFont := False;
  if CurrentSkin.Id = skClassic then
  begin
    Memo.Color := clWhite;
    Memo.Font.Color := clWindowText;
  end
  else
  begin
    Memo.Color := CurrentSkin.NoteBg;
    Memo.Font.Color := CurrentSkin.NoteText;
    Panel1.Color := CurrentSkin.NoteBg;
    Panel2.Color := CurrentSkin.NoteBg;
  end;
  ApplyDarkModeToControl(Memo, CurrentSkin.IsDark);
  Memo.Rtf := AdaptRtfForSkin(Rtf, CurrentSkin);
end;

{ TFrmDictionaryPopup }

procedure TFrmDictionaryPopup.FormCreate(Sender: TObject);
begin
  ApplyFormStyle(Self);
  fCompStyle := csHintWindow;
  Canvas.Brush.Style := bsClear;
  //BorderStyle := bsNone; // bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin; // bsNone;
  Caption := '';
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FStrongs := TStringList.Create;
  FMorphologies := TStringList.Create;

  RichMemoStrong.ZoomFactor := opts.ReadInteger('leiaute', 'popup.zoom.strong', 10) / 10.0;
  RichMemoMorphology.ZoomFactor := opts.ReadInteger('leiaute', 'popup.zoom.morfo',  10) / 10.0;
  Width  := opts.ReadInteger('leiaute', 'popup.largura', Width);
  Height := opts.ReadInteger('leiaute', 'popup.altura', Height);
  Panel2.Height := opts.ReadInteger('leiaute', 'popup.panel2.altura', Panel2.Height);
end;

procedure TFrmDictionaryPopup.FormDestroy(Sender: TObject);
begin
  opts.WriteInteger('leiaute', 'popup.largura', Width);
  opts.WriteInteger('leiaute', 'popup.altura', Height);
  opts.WriteInteger('leiaute', 'popup.panel2.altura', Panel2.Height);
  opts.WriteInteger('leiaute', 'popup.zoom.strong', Trunc(RichMemoStrong.ZoomFactor * 10));
  opts.WriteInteger('leiaute', 'popup.zoom.morfo',  Trunc(RichMemoMorphology.ZoomFactor * 10));

  FStrongs.Destroy;
  FMorphologies.Destroy;
end;

procedure TFrmDictionaryPopup.FormHide(Sender: TObject);
begin
  FStrongs.Clear;
  FMorphologies.Clear;
  TabControlStrongs.Tabs.Clear;
  TabControlMorphologies.Tabs.Clear;
end;

procedure TFrmDictionaryPopup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Visible := false;
    MainForm.SetFocus;
  end;
end;

procedure TFrmDictionaryPopup.RichMemoMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    TRichMemo(Sender).ZoomFactor := Max(TRichMemo(Sender).ZoomFactor + Sign(WheelDelta)*0.1, 0.1);
    Handled := true;
  end;
end;

procedure TFrmDictionaryPopup.TabControlMorphologiesChange(Sender: TObject);
begin
  ShowRtf(RichMemoMorphology, FMorphologies[TabControlMorphologies.TabIndex]);
end;

procedure TFrmDictionaryPopup.TabControlStrongsChange(Sender: TObject);
begin
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

procedure TFrmDictionaryPopup.ShowAt(x: Integer; y: Integer);
begin
  if FStrongs.Count > 0 then
  begin
    TabControlStrongs.TabIndex := 0;
    ShowRtf(RichMemoStrong, FStrongs[0]);
    RichMemoStrong.ZoomFactor  := RichMemoStrong.ZoomFactor; { circunventing bug... }
  end;
  if FMorphologies.Count > 0 then
  begin
    TabControlMorphologies.TabIndex := 0;
    ShowRtf(RichMemoMorphology, FMorphologies[0]);
    RichMemoMorphology.ZoomFactor := RichMemoMorphology.ZoomFactor; { circunventing bug... }
    TabControlMorphologies.Visible := true;
  end else
  begin
    TabControlMorphologies.Visible := false;
  end;

  if x + Width > Screen.Width then
  begin
    Left := x - Width;
    if Left < 0 then Left := 0;
  end
  else
    Left := x;

  if y + Height > Screen.Height then
  begin
    Top := y - Height;
    if Top < 0 then Top := 0;
  end
  else
    Top := y;

  Visible := True;
end;

procedure TFrmDictionaryPopup.HidePopup;
begin
  if not Visible then
    exit;
  Visible := false;
  FormHide(self);
  TabControlStrongs.Tabs.Clear;
  TabControlMorphologies.Tabs.Clear;
  RichMemoStrong.Rtf := '';
  RichMemoMorphology.Rtf := '';
end;

procedure TFrmDictionaryPopup.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
end;


initialization
{$IFNDEF FPCUNIT}
  {$R FormDictionaryPopup.lfm}
{$ENDIF}

end.

