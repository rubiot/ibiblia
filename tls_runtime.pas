unit tls_runtime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, ssockets;

const
  TLS_SSL_FILE_NAME = 'libssl-3-x64.dll';
  TLS_CRYPTO_FILE_NAME = 'libcrypto-3-x64.dll';
  TLS_CA_FILE_NAME = 'cacert.pem';

type
  ETLSRuntimeError = class(Exception);
  ETLSCertificateError = class(ETLSRuntimeError)
  private
    FVerificationCode: Integer;
  public
    constructor Create(AVerificationCode: Integer);
    property VerificationCode: Integer read FVerificationCode;
  end;

  TIBibliaHTTPSClient = class(TFPHTTPClient)
  private
    FRuntimeDirectory: string;
  protected
    function GetSocketHandler(const UseSSL: Boolean): TSocketHandler; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateForRuntime(AOwner: TComponent;
      const ARuntimeDirectory: string);
  end;

procedure EnsureBundledTLSInitialized(const RuntimeDirectory: string = '');
function BundledTLSVersion: string;

implementation

uses
  syncobjs, openssl, opensslsockets;

type
  TVerifiedSocketHandler = class(TOpenSSLSocketHandler)
  public
    function Connect: Boolean; override;
  end;

var
  InitializationLock: TCriticalSection;
  LoadedRuntimeDirectory: string;

function NormalizeRuntimeDirectory(const Value: string): string;
begin
  if Value = '' then
    Result := ExtractFilePath(ParamStr(0))
  else
    Result := Value;
  Result := IncludeTrailingPathDelimiter(ExpandFileName(Result));
end;

constructor ETLSCertificateError.Create(AVerificationCode: Integer);
begin
  FVerificationCode := AVerificationCode;
  inherited CreateFmt(
    'The HTTPS certificate or server name could not be verified (code %d).',
    [AVerificationCode]);
end;

procedure EnsureBundledTLSInitialized(const RuntimeDirectory: string);
var
  DirectoryName, SSLFileName, CryptoFileName: string;
  RequiredFile: string;
begin
  DirectoryName := NormalizeRuntimeDirectory(RuntimeDirectory);
  InitializationLock.Enter;
  try
    if LoadedRuntimeDirectory <> '' then
    begin
      if not SameFileName(DirectoryName, LoadedRuntimeDirectory) then
        raise ETLSRuntimeError.Create(
          'TLS is already initialized from another runtime directory.');
      Exit;
    end;
    SSLFileName := DirectoryName + TLS_SSL_FILE_NAME;
    CryptoFileName := DirectoryName + TLS_CRYPTO_FILE_NAME;
    for RequiredFile in [SSLFileName, CryptoFileName,
      DirectoryName + TLS_CA_FILE_NAME] do
      if not FileExists(RequiredFile) then
        raise ETLSRuntimeError.CreateFmt(
          'The HTTPS runtime file %s is missing. Extract the complete iBiblia package.',
          [ExtractFileName(RequiredFile)]);

    if IsSSLLoaded then
      raise ETLSRuntimeError.Create(
        'TLS was initialized outside the bundled iBiblia runtime.');
    if not InitSSLInterface(SSLFileName, CryptoFileName) then
      raise ETLSRuntimeError.Create(
        'The bundled HTTPS libraries could not be loaded. Extract the complete Windows x64 package.');
    if (Pos('OpenSSL 3.5.', OpenSSLGetVersion(0)) <> 1) or
       (SslLibraryInit <> 1) then
    begin
      DestroySSLInterface;
      raise ETLSRuntimeError.Create(
        'The bundled OpenSSL 3.5 runtime could not be initialized.');
    end;
    LoadedRuntimeDirectory := DirectoryName;
  finally
    InitializationLock.Leave;
  end;
end;

function BundledTLSVersion: string;
begin
  if LoadedRuntimeDirectory = '' then
    Result := ''
  else
    Result := OpenSSLGetVersion(0);
end;

function TVerifiedSocketHandler.Connect: Boolean;
var
  Verification: Integer;
begin
  Result := inherited Connect;
  if not Result then
  begin
    Verification := 0;
    if Assigned(SSL) then
      Verification := SSL.VerifyResult;
    if Verification <> 0 then
      raise ETLSCertificateError.Create(Verification);
    raise ETLSRuntimeError.Create('The HTTPS connection failed: ' +
      Trim(SSLLastErrorString));
  end;
end;

constructor TIBibliaHTTPSClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRuntimeDirectory := NormalizeRuntimeDirectory('');
end;

constructor TIBibliaHTTPSClient.CreateForRuntime(AOwner: TComponent;
  const ARuntimeDirectory: string);
begin
  inherited Create(AOwner);
  FRuntimeDirectory := NormalizeRuntimeDirectory(ARuntimeDirectory);
end;

function TIBibliaHTTPSClient.GetSocketHandler(
  const UseSSL: Boolean): TSocketHandler;
var
  Handler: TVerifiedSocketHandler;
begin
  if not UseSSL then
    raise ETLSRuntimeError.Create('This connection requires HTTPS.');
  EnsureBundledTLSInitialized(FRuntimeDirectory);
  Handler := TVerifiedSocketHandler.Create;
  try
    Handler.VerifyPeerCert := True;
    Handler.SendHostAsSNI := True;
    Handler.CertificateData.CertCA.FileName :=
      FRuntimeDirectory + TLS_CA_FILE_NAME;
    Result := Handler;
  except
    Handler.Free;
    raise;
  end;
end;

initialization
  InitializationLock := TCriticalSection.Create;

finalization
  InitializationLock.Free;

end.
