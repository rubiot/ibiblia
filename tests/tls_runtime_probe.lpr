program tls_runtime_probe;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, tls_runtime;

var
  RuntimeDirectory, URL, Body: string;
  ExpectCertificateError, CheckRuntime: Boolean;
  Client: TIBibliaHTTPSClient;
  Index: Integer;

begin
  RuntimeDirectory := '';
  URL := '';
  ExpectCertificateError := False;
  CheckRuntime := False;
  Client := nil;
  try
    Index := 1;
    while Index <= ParamCount do
    begin
      case ParamStr(Index) of
        '--runtime-dir':
          begin
            Inc(Index);
            if Index > ParamCount then raise Exception.Create('Missing runtime directory');
            RuntimeDirectory := ParamStr(Index);
          end;
        '--url':
          begin
            Inc(Index);
            if Index > ParamCount then raise Exception.Create('Missing URL');
            URL := ParamStr(Index);
          end;
        '--expect-certificate-error': ExpectCertificateError := True;
        '--check-runtime': CheckRuntime := True;
      else
        raise Exception.Create('Unknown argument: ' + ParamStr(Index));
      end;
      Inc(Index);
    end;
    EnsureBundledTLSInitialized(RuntimeDirectory);
    if CheckRuntime then
      WriteLn('TLS_RUNTIME_OK ', BundledTLSVersion)
    else
    begin
      if URL = '' then raise Exception.Create('Use --url or --check-runtime');
      Client := TIBibliaHTTPSClient.CreateForRuntime(nil, RuntimeDirectory);
      Client.ConnectTimeout := 10000;
      Client.IOTimeout := 10000;
      try
        Body := Client.Get(URL);
        if ExpectCertificateError then
          raise Exception.Create('The invalid certificate was accepted');
        WriteLn('TLS_OK status=', Client.ResponseStatusCode,
          ' bytes=', Length(Body), ' version=', BundledTLSVersion);
      except
        on E: ETLSCertificateError do
          if ExpectCertificateError then
            WriteLn('TLS_REJECTED verification=', E.VerificationCode)
          else raise;
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  Client.Free;
end.
