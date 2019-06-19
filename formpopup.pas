unit formpopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  {$IFDEF UNIX}
  LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  ExtCtrls, ComCtrls, RichMemo;

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
    procedure TabControlMorfosChange(Sender: TObject);
    procedure TabControlStrongsChange(Sender: TObject);
  private
    { private declarations }
    FZoomStrong: smallint;
    FZoomMorfo: smallint;
    FStrongs: TStringList;
    FMorfos: TStringList;
  public
    { public declarations }
    procedure AdicionarStrong(const strong: string; const definition: string);
    procedure AdicionarMorfo(const morfo: string; const definition: string);
    procedure MostrarEm(x: Integer; y: Integer);
    procedure Ocultar;
    property ZoomStrong: smallint read FZoomStrong write FZoomStrong;
    property ZoomMorfo: smallint read FZoomMorfo write FZoomMorfo;
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
  FStrongs := TStringList.Create;
  FMorfos := TStringList.Create;

  ZoomStrong := opts.ReadInteger('leiaute', 'popup.zoom.strong', 100);
  ZoomMorfo  := opts.ReadInteger('leiaute', 'popup.zoom.morfo', 100);
  Width  := opts.ReadInteger('leiaute', 'popup.largura', Width);
  Height := opts.ReadInteger('leiaute', 'popup.altura', Height);
  Panel2.Height := opts.ReadInteger('leiaute', 'popup.panel2.altura', Panel2.Height);
end;

procedure TFrmDictionaryPopup.FormDestroy(Sender: TObject);
begin
  opts.WriteInteger('leiaute', 'popup.largura', Width);
  opts.WriteInteger('leiaute', 'popup.altura', Height);
  opts.WriteInteger('leiaute', 'popup.panel2.altura', Panel2.Height);
  opts.WriteInteger('leiaute', 'popup.zoom.strong', ZoomStrong);
  opts.WriteInteger('leiaute', 'popup.zoom.morfo', ZoomMorfo);

  FStrongs.Destroy;
  FMorfos.Destroy;
end;

procedure TFrmDictionaryPopup.FormHide(Sender: TObject);
begin
  ZoomStrong := Trunc(RichMemoStrong.ZoomFactor * 10);
  ZoomMorfo := Trunc(RichMemoMorpho.ZoomFactor * 10);
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
    frmPrincipal.SetFocus;
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
    RichMemoStrong.ZoomFactor  := ZoomStrong / 10.0;
  end;
  if FMorfos.Count > 0 then
  begin
    TabControlMorfos.TabIndex  := 0;
    RichMemoMorpho.Rtf         := FMorfos[0];
    RichMemoMorpho.ZoomFactor  := ZoomMorfo / 10.0;
    TabControlMorfos.Visible   :=true;
  end else
  begin
    TabControlMorfos.Visible   := false;
  end;

  if x + self.Width > Screen.Width then
  begin
    self.left := x - self.Width;
    if self.Left < 0 then self.Left := 0;
  end
  else
    self.left := x;

  if y + self.Height > Screen.Height then
  begin
    self.top := y - self.Height;
    if self.top < 0 then self.top := 0;
  end
  else
    self.top := y;

  self.visible := true;
end;

procedure TFrmDictionaryPopup.Ocultar;
begin
  if not self.Visible then
    exit;
  self.Visible := false;
  self.FormHide(self);
  TabControlStrongs.Tabs.Clear;
  TabControlMorfos.Tabs.Clear;
  RichMemoStrong.Rtf := '';
  RichMemoMorpho.Rtf := '';
end;

initialization
  {$I formpopup.lrs}

end.

