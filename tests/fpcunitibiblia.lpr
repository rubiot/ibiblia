program fpcunitibiblia;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ibibliatest, Sintagma;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

