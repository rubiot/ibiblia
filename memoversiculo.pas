unit MemoVersiculo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Forms, LCLType,
  {$IFNDEF UNIX}
  Windows,
  {$ENDIF}
  Dialogs, Versiculo, Math;

type

  TMemoVersiculo = class;
  TEventoTextoModificado = procedure (Sender: TMemoVersiculo) of object;

  { TMemoVersiculo }

  TMemoVersiculo = class
  private
    FModificado: boolean;
    FMemo: TMemo;
    FVersiculo: TVersiculo;
    FEventoTextoModificado: TEventoTextoModificado;
    function GetTexto: TCaption;
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnChange(Sender: TObject);
    procedure ConfirmarAlteracao;
  public
    constructor Criar;
    destructor Destruir;
    procedure Ativar(conteiner: TScrollBox; versiculo: TVersiculo);
    procedure Desativar;
    property Texto: TCaption read GetTexto;
    property Versiculo: TVersiculo read FVersiculo;
    property QuandoModificarVersiculo: TEventoTextoModificado read FEventoTextoModificado write FEventoTextoModificado;
  end;

resourcestring
  SChangeVerse = 'Change verse text?';
  SChangeVerseConfirmation = 'Are you sure you want to change the verse text? Some associations may be lost.';

implementation

{ TMemoVersiculo }

function TMemoVersiculo.GetTexto: TCaption;
begin
  result := FMemo.Text;
end;

procedure TMemoVersiculo.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    FMemo.Font.Size := FMemo.Font.Size + Sign(WheelDelta) * 1;
  end;
end;

procedure TMemoVersiculo.OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ConfirmarAlteracao;
    FVersiculo.Fonte.Size := FMemo.Font.Size;
  end
  else if Key = VK_RETURN then
    Key := 0;
end;

procedure TMemoVersiculo.OnChange(Sender: TObject);
begin
  if assigned(TMemo(Sender).Parent) and TMemo(Sender).Visible then
    FModificado := True;
end;

procedure TMemoVersiculo.ConfirmarAlteracao;
begin
  {
  if FModificado then
  begin
    case MessageDlg(SChangeVerse, SChangeVerseConfirmation, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if assigned(FEventoTextoModificado) then
          FEventoTextoModificado(self);
      //mrNo: salvar := false;
      mrCancel: exit;
    end;
  end;
  }
  Desativar;
end;

constructor TMemoVersiculo.Criar;
begin
  FMemo := TMemo.Create(nil);
  FMemo.Visible      := False;
  FMemo.Align        := alClient;
  FMemo.ScrollBars   := ssAutoVertical;
  FMemo.OnKeyDown    := @OnKeyDown;
  FMemo.OnChange     := @OnChange;
  FMemo.OnMouseWheel := @OnMouseWheel;
  FMemo.ParentFont   := False;
  FMemo.Font.Name    := 'Courier New';
  FModificado        := False;
  FVersiculo         := nil;
  FEventoTextoModificado := nil;
end;

destructor TMemoVersiculo.Destruir;
begin
  FMemo.Free;
end;

procedure TMemoVersiculo.Ativar(conteiner: TScrollBox; versiculo: TVersiculo);
begin
  if FMemo.Visible and FModificado and (FMemo.Parent <> conteiner) then
  begin
    ConfirmarAlteracao;
    exit;
  end;

  FMemo.Font.Size := versiculo.Fonte.Size;
  FModificado := False;
  FVersiculo := versiculo;
  FMemo.Parent := nil;  // flag sinalizando que não se deve disparar evento OnChange
  FMemo.Text := versiculo.XML;
  FMemo.Parent := conteiner;
  FMemo.BringToFront;
  FMemo.Visible := True;
  FMemo.SetFocus;
end;

procedure TMemoVersiculo.Desativar;
begin
  if not FMemo.Visible then
    exit;

  FMemo.Visible := False;
  if assigned(FEventoTextoModificado) and FModificado then
    FEventoTextoModificado(self);
end;

end.

