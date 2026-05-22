unit FormDictionaryPopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
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
    procedure AddStrong(const strong: string; const definition: string);
    procedure AddMorphology(const morfo: string; const definition: string);
    procedure ShowAt(x: Integer; y: Integer);
    procedure HidePopup;
    property Strongs: TStringList read FStrongs;
    property Morphologies: TStringList read FMorphologies;
  end;

var
  FrmDictionaryPopup: TFrmDictionaryPopup;

implementation

uses formPrincipal;

{ TFrmDictionaryPopup }

procedure TFrmDictionaryPopup.FormCreate(Sender: TObject);
begin
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
    FrmPrincipal.SetFocus;
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
  RichMemoMorphology.Rtf := FMorphologies[TabControlMorphologies.TabIndex];
end;

procedure TFrmDictionaryPopup.TabControlStrongsChange(Sender: TObject);
begin
  RichMemoStrong.Rtf := FStrongs[TabControlStrongs.TabIndex];
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
    RichMemoStrong.Rtf         := FStrongs[0];
    RichMemoStrong.ZoomFactor  := RichMemoStrong.ZoomFactor; { circunventing bug... }
  end;
  if FMorphologies.Count > 0 then
  begin
    TabControlMorphologies.TabIndex := 0;
    RichMemoMorphology.Rtf := FMorphologies[0];
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

initialization
{$IFNDEF FPCUNIT}
  {$I FormDictionaryPopup.lrs}
{$ENDIF}

end.

