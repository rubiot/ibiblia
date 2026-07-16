program fpcunitibiblia;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ibibliatest, versiculotest,
  syntagmselectiontest, statuscatalogtest, project_setup_validation_test,
  ai_processing_batch_test, ai_flow_config_test, suggestionmanagertest,
  associationmemoryservicetest, project_persistence_test,
  navigation_history_test;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

