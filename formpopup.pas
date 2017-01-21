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
  ExtCtrls, RTFEdit;

type

  { TFrmDictionaryPopup }

  TFrmDictionaryPopup = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FZoomStrong: smallint;
    FZoomMorfo: smallint;
    FRTFEditStrong1: TRTFEdit;
    FRTFEditMorfo1: TRTFEdit;
    procedure SetStrong1(const AValue: string);
    procedure SetMorfo1(const AValue: string);
  public
    { public declarations }
    procedure MostrarEm(x: Integer; y: Integer);
    property Strong1: string write SetStrong1;
    property Morfo1: string write SetMorfo1;
    property ZoomStrong: smallint read FZoomStrong write FZoomStrong;
    property ZoomMorfo: smallint read FZoomMorfo write FZoomMorfo;
  end;

var
  FrmDictionaryPopup: TFrmDictionaryPopup;

implementation

uses formPrincipal;

{ TFrmDictionaryPopup }

procedure TFrmDictionaryPopup.FormCreate(Sender: TObject);
begin
  FRTFEditStrong1 := TRTFEdit.Criar(Panel1);
  FRTFEditMorfo1 := TRTFEdit.Criar(Panel2);

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

  FRTFEditStrong1.Destruir;
  FRTFEditMorfo1.Destruir;
end;

procedure TFrmDictionaryPopup.FormHide(Sender: TObject);
begin
  ZoomStrong := FRTFEditStrong1.Zoom;
  ZoomMorfo := FRTFEditMorfo1.Zoom;
end;

procedure TFrmDictionaryPopup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_ESCAPE then
  begin
    Visible := false;
    frmPrincipal.SetFocus;
  end;
end;

procedure TFrmDictionaryPopup.FormShow(Sender: TObject);
begin

end;

procedure TFrmDictionaryPopup.SetStrong1(const AValue: string);
begin
  if length(AValue) > 0 then
  begin
    FRTFEditStrong1.RTF := AValue;
    FRTFEditStrong1.Zoom := ZoomStrong;
  end
  else
    FRTFEditStrong1.Texto := ' ';
end;

procedure TFrmDictionaryPopup.SetMorfo1(const AValue: string);
begin
  if length(AValue) > 0 then
  begin
    FRTFEditMorfo1.RTF := AValue;
    FRTFEditMorfo1.Zoom := ZoomMorfo;
  end
  else
    FRTFEditMorfo1.Texto := ' ';
end;

procedure TFrmDictionaryPopup.MostrarEm(x: Integer; y: Integer);
begin
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

initialization
  {$I formpopup.lrs}

end.

