unit formpopup;

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
    RichMemoMorpho: TRichMemo;
    Splitter1: TSplitter;
    TabControlStrongs: TTabControl;
    TabControlMorfos: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RichMemoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TabControlMorfosChange(Sender: TObject);
    procedure TabControlStrongsChange(Sender: TObject);
  private
    { private declarations }
    FStrongs: TStringList;
    FMorfos: TStringList;
  public
    { public declarations }
    procedure AdicionarStrong(const strong: string; const definition: string);
    procedure AdicionarMorfo(const morfo: string; const definition: string);
    procedure MostrarEm(x: Integer; y: Integer);
    procedure Ocultar;
    property Strongs: TStringList read FStrongs;
    property Morfos: TStringList read FMorfos;
  end;

var
  FrmDictionaryPopup: TFrmDictionaryPopup;

implementation

uses formPrincipal;

{ TFrmDictionaryPopup }

procedure TFrmDictionaryPopup.FormCreate(Sender: TObject);
begin
  fCompStyle := csHintWindow;
  //Color := clInfoBk;
  Canvas.Brush.Style := bsClear;
  BorderStyle := bsNone;
  Caption := '';
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FStrongs := TStringList.Create;
  FMorfos  := TStringList.Create;

  RichMemoStrong.ZoomFactor := opts.ReadInteger('leiaute', 'popup.zoom.strong', 10) / 10.0;
  RichMemoMorpho.ZoomFactor := opts.ReadInteger('leiaute', 'popup.zoom.morfo',  10) / 10.0;
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
  opts.WriteInteger('leiaute', 'popup.zoom.morfo',  Trunc(RichMemoMorpho.ZoomFactor * 10));

  FStrongs.Destroy;
  FMorfos.Destroy;
end;

procedure TFrmDictionaryPopup.FormHide(Sender: TObject);
begin
  FStrongs.Clear;
  FMorfos.Clear;
  TabControlStrongs.Tabs.Clear;
  TabControlMorfos.Tabs.Clear;
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

procedure TFrmDictionaryPopup.TabControlMorfosChange(Sender: TObject);
begin
  RichMemoMorpho.Rtf := FMorfos[TabControlMorfos.TabIndex];
end;

procedure TFrmDictionaryPopup.TabControlStrongsChange(Sender: TObject);
begin
  RichMemoStrong.Rtf := FStrongs[TabControlStrongs.TabIndex];
end;

procedure TFrmDictionaryPopup.AdicionarStrong(const strong: string; const definition: string);
begin
  FStrongs.Add(definition);
  TabControlStrongs.Tabs.Add(strong);
end;

procedure TFrmDictionaryPopup.AdicionarMorfo(const morfo: string; const definition: string);
begin
  FMorfos.Add(definition);
  TabControlMorfos.Tabs.Add(morfo);
end;

procedure TFrmDictionaryPopup.MostrarEm(x: Integer; y: Integer);
begin
  if FStrongs.Count > 0 then
  begin
    TabControlStrongs.TabIndex := 0;
    RichMemoStrong.Rtf         := FStrongs[0];
    RichMemoStrong.ZoomFactor  := RichMemoStrong.ZoomFactor; { circunventing bug... }
  end;
  if FMorfos.Count > 0 then
  begin
    TabControlMorfos.TabIndex  := 0;
    RichMemoMorpho.Rtf         := FMorfos[0];
    RichMemoMorpho.ZoomFactor  := RichMemoMorpho.ZoomFactor; { circunventing bug... }
    TabControlMorfos.Visible   := true;
  end else
  begin
    TabControlMorfos.Visible   := false;
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

procedure TFrmDictionaryPopup.Ocultar;
begin
  if not Visible then
    exit;
  Visible := false;
  FormHide(self);
  TabControlStrongs.Tabs.Clear;
  TabControlMorfos.Tabs.Clear;
  RichMemoStrong.Rtf := '';
  RichMemoMorpho.Rtf := '';
end;

initialization
  {$I formpopup.lrs}

end.

