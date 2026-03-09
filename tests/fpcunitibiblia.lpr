program fpcunitibiblia;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ibibliatest, Syntagm, versiculotest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

