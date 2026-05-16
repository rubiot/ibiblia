program fpcunitibiblia;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ibibliatest, versiculotest, syntagmselectiontest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

