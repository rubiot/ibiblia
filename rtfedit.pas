unit RTFEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFNDEF UNIX}
  Windows, RichEdit,
  {$ENDIF}
  ExtCtrls, Forms;

type

  { TRTFEdit }

  TRTFEdit = class
  private
    {$IFNDEF UNIX}
    FHandle: HWND;
    {$ENDIF}
    FConteiner: TPanel;
    FSoLeitura: boolean;
    FZoom: smallint;
    function GetZoom: smallint;
    procedure SetRTF(const AValue: TStringStream);
    procedure SetRTF(const AValue: string);
    procedure SetSoLeitura(const AValue: boolean);
    procedure SetTexto(const AValue: string);
    procedure SetZoom(const AValue: smallint);
    {$IFNDEF UNIX}
    procedure SetOpcao(const Opcao: LRESULT; const AValue: boolean);
    {$ENDIF}
  public
    constructor Criar(Conteiner: TPanel);
    destructor Destruir;
    procedure OnRedimensionar(Sender: TObject);
    property RTF: TStringStream write SetRTF;
    property Texto: string write SetTexto;
    property Zoom: smallint read GetZoom write SetZoom;
    property SoLeitura: boolean read FSoLeitura write SetSoLeitura;
  end;

{$IFNDEF UNIX}
function SetRTFCallback(dwCookie: PDWORD; pbBuff: LPBYTE;
  cb: LONG; var pcb: longint): DWORD; stdcall;
{$ENDIF}

const
  MSFTEDIT_CLASS = 'RICHEDIT50W';

implementation

{ TRTFEdit }

{$IFNDEF UNIX}
function SetRTFCallback(dwCookie: PDWORD;
  pbBuff: LPBYTE; cb: LONG; var pcb: longint): DWORD; stdcall;
var
  stream: TStringStream;
  remain: integer;
begin
  stream := TStringStream(dwCookie);
  remain := stream.Size - stream.Position;

  Result := ERROR_SUCCESS;

  if stream.Size > stream.Position then
  begin
    if cb < remain then
      pcb := cb
    else
      pcb := remain;
    try
      CopyMemory(pbBuff, PByte(stream.DataString) + stream.Position, pcb);
      stream.Seek(pcb, soFromCurrent);
      Result := ERROR_SUCCESS;
    except
      Result := ERROR_CANNOT_COPY;
    end;
  end else
    pcb := 0;
end;
{$ENDIF}

function TRTFEdit.GetZoom: smallint;
{$IFNDEF UNIX}
var
  numerador: WPARAM;
  denominador: LPARAM;
{$ENDIF}
begin
  result := 0;
  {$IFNDEF UNIX}
  if BOOL(SendMessage(FHandle, EM_GETZOOM, WPARAM(@numerador), LPARAM(@denominador))) and
     (denominador > 0) then
    result := trunc(integer(numerador)/integer(denominador)*100);
  {$ENDIF}
end;

procedure TRTFEdit.SetRTF(const AValue: TStringStream);
var
  fSuccess: boolean;
  {$IFNDEF UNIX}
  es: EDITSTREAM;
  {$ENDIF}
begin
  {if AValue.Size <= 0 then
    exit;}
  {$IFNDEF UNIX}
  fSuccess := False;
  AValue.Seek(0, soBeginning);

  es.pfnCallback := @SetRTFCallback;
  es.dwCookie    := PtrInt(AValue);//DWORD(AValue);

  if BOOL(SendMessage(FHandle, EM_STREAMIN, SF_RTF, LPARAM(@es))) and
    (es.dwError = 0) then
  begin
    fSuccess := True;
  end;

  if fSuccess = false then
    SetTexto(AValue.DataString);
    //MessageBox(TForm(FConteiner.Owner).Handle, PChar('Falha ao atribuir RTF a controle TRTFEdit!!'), PChar('Erro'), MB_OK);

  {$ENDIF}
  SetZoom(FZoom);
end;

procedure TRTFEdit.SetRTF(const AValue: string);
var
  s: TStringStream;
begin
  s := TStringStream.Create(AValue);
  SetRTF(s);
  s.Free;
end;

procedure TRTFEdit.SetSoLeitura(const AValue: boolean);
begin
  if FSoLeitura = AValue then exit;
  FSoLeitura := AValue;
  {$IFNDEF UNIX}
  SetOpcao(ECO_READONLY, AValue);
  {$ENDIF}
end;

procedure TRTFEdit.SetTexto(const AValue: string);
var
  fSuccess: boolean;
  {$IFNDEF UNIX}
  es: EDITSTREAM;
  {$ENDIF}
  s: TStringStream;
begin
  {$IFNDEF UNIX}
  if length(AValue) = 0 then
    exit;
  fSuccess := False;

  s := TStringStream.Create(AValue);
  s.Seek(0, soBeginning);

  es.pfnCallback := @SetRTFCallback;
  es.dwCookie    := PtrInt(s);//DWORD(s);

  if BOOL(SendMessage(FHandle, EM_STREAMIN, SF_TEXT {or SF_UNICODE}, LPARAM(@es))) and
    (es.dwError = 0) then
  begin
    fSuccess := True;
  end;

  if fSuccess = false then
    MessageBox(TForm(FConteiner.Owner).Handle, PChar('Falha ao atribuir texto a controle TRTFEdit!!'), PChar('Error'), MB_OK);

  s.Destroy;
  {$ENDIF}

  SetZoom(FZoom);
end;

procedure TRTFEdit.SetZoom(const AValue: smallint);
var
  numerador: Longint;
  denominador: Longint;
begin
  FZoom := AValue;

  numerador := trunc((longint(AValue)/100.0)*64.0);
  denominador := 64;
  if numerador = 64 then
  begin
    numerador := 0;
    denominador := 0;
  end;
  {$IFNDEF UNIX}
  SendMessage(FHandle, EM_SETZOOM, WPARAM(numerador), LPARAM(denominador));
  {$ENDIF}
end;

{$IFNDEF UNIX}
procedure TRTFEdit.SetOpcao(const Opcao: LRESULT; const AValue: boolean);
var
  o: LRESULT;
begin
  o := Opcao;
  if not AValue then
    o := not o;
  SendMessage(FHandle, EM_SETOPTIONS, ECOOP_XOR, o);
end;
{$ENDIF}

constructor TRTFEdit.Criar(Conteiner: TPanel);
begin
  {$IFNDEF UNIX}
  if LoadLibrary(PChar('Msftedit.dll')) = 0 then
    MessageBox(TForm(Conteiner.Owner).Handle, PChar('RichEdit 4.1 not found!!'), PChar('Error'),
      MB_OK);

  FHandle := CreateWindowEx(0, MSFTEDIT_CLASS, PChar(''),
    ES_MULTILINE or WS_VISIBLE or WS_CHILD {or WS_BORDER} or WS_TABSTOP or
    WS_VSCROLL {or WS_HSCROLL}, 0, 0, Conteiner.Width, Conteiner.Height,
    Conteiner.Handle, 0, GetModuleHandle(PChar('')), PChar(''));
  {$ENDIF}

  Conteiner.OnResize := @OnRedimensionar;
  FConteiner := Conteiner;
  FZoom := 100;
  SetSoLeitura(true);
end;

destructor TRTFEdit.Destruir;
begin

end;

procedure TRTFEdit.OnRedimensionar(Sender: TObject);
begin
  {$IFNDEF UNIX}
  SetWindowPos(FHandle, HWND_TOP, 0, 0, FConteiner.Width, FConteiner.Height, SWP_NOZORDER);
  {$ENDIF}
end;

end.

