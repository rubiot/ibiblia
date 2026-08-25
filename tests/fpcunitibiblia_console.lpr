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
  association_highlight_projection_test,
  project_persistence_test,
  navigation_history_test,
  verse_visual_adapter_test,
  verse_inline_style_projection_test,
  verse_visual_document_test,
  text_direction_test,
  text_edit_provenance_test,
  versememotest,
  theword_ont_codec_v2_test,
  theword_wysiwyg_session_v2_test,
  theword_clipboard_v2_test,
  rf_note_scanner_test,
  theword_translator_note_visual_test,
  project_exporter_integration_test,
  theword_module_encoding_test,
  theword_module_import_test,
  theword_effective_inline_style_v2_test,
  theword_effective_inline_style_v2_review_test,
  theword_font_color_mutation_v2_test,
  theword_pending_formatting_lifecycle_test,
  chapter_inline_style_projection_test,
  theword_font_size_mutation_v2_test,
  association_note_visual_projection_test,
  association_note_verse_interaction_test,
  translator_note_hover_controller_test,
  translation_memory_focus_controller_test,
  translation_memory_preview_test,
  project_creation_staging_test;

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
