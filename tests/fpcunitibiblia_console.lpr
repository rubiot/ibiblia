program fpcunitibiblia_console;

{$mode objfpc}{$H+}

uses
  consoletestrunner,
  ibibliatest,
  versiculotest,
  syntagmselectiontest;

var
  Application: TTestRunner;

begin
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'iBiblia console test runner';
  Application.Run;
  Application.Free;
end.