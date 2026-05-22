program iBiblia;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formPrincipal, Verse, ONTTokenizer, Projeto, SuggestionManager,
  VerseMemo, PCRE, ConcordanceExport, FormVerseRules, FormMergeProjects,
  FormDictionaryPopup, FormNewProject, FormProjectProperties, FormProjectExport,
  {$IFDEF WINDOWS}
  twAutomate, twSyncThread,
  {$ENDIF}
  ONTParser, Syntagm, unitabout, MySwordModule, TheWordDictionary, lazrichview,
  HTMLColors, formexportpatch, FormApplyPatch, PatchFile,
  formInterlinearVerseRules, ChapterView, UnitPleaseWait,
  llmclient, FormLlmOptions, FormAiProcessing;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.CreateForm(TFrmDictionaryPopup, FrmDictionaryPopup);
  Application.CreateForm(TFrmProjectProperties, FrmProjectProperties);
  Application.CreateForm(TFrmNewProject, FrmNewProject);
  Application.CreateForm(TFrmVerseRules, FrmVerseRules);
  Application.CreateForm(TFrmProjectExport, FrmProjectExport);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormExportPatch, FrmExportPatch);
  Application.CreateForm(TFrmApplyPatch, FrmApplyPatch);
  Application.CreateForm(TFormInterlinearVerseRules, frmInterlinearVerseRules);
  Application.CreateForm(TFormPleaseWait, FormPleaseWait);
  Application.CreateForm(TFrmLLMOptions, FrmLLMOptions);
  Application.CreateForm(TFrmAIProcessing, FrmAIProcessing);
  Application.Run;
end.

