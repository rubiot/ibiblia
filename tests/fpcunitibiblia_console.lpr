program fpcunitibiblia_console;

{$mode objfpc}{$H+}

uses
  Interfaces,
  consoletestrunner,
  ibibliatest,
  versiculotest,
  syntagmselectiontest,
  statuscatalogtest,
  project_setup_validation_test,
  theword_syntax_highlighter_test,
  ai_processing_batch_test,
  ai_flow_config_test,
  suggestionmanagertest,
  associationmemoryservicetest,
  concordanceexporttest,
  project_persistence_test,
  navigation_history_test,
  verse_visual_adapter_test,
  verse_visual_document_test,
  text_direction_test,
  text_edit_provenance_test,
  versememotest,
  theword_ont_codec_v2_test,
  theword_wysiwyg_session_v2_test,
  theword_clipboard_v2_test;

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
