program iBiblia;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formPrincipal, Verse, ONTTokenizer, Projeto, SuggestionManager,
  memoversiculo, PCRE, exportarprojeto, formverserules, formmesclarprojetos,
  formpopup, formnovoprojeto, formpropprojeto, formexportar,
  {$IFDEF WINDOWS}
  twAutomate, twSyncThread,
  {$ENDIF}
  ONTParser, Syntagm, unitabout, MySwordModule, TheWordDictionary, lazrichview,
  HTMLColors, formexportpatch, formapplypatch, PatchFile,
  formInterlinearVerseRules, ChapterView, UnitPleaseWait,
  llmclient, FormLlmOptions, FormAiProcessing;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.CreateForm(TfrmDictionaryPopup, frmDictionaryPopup);
  Application.CreateForm(TFormPropProjeto, FormPropProjeto1);
  Application.CreateForm(TFormNovoProjeto, FormNovoProjeto1);
  Application.CreateForm(TFrmEscolherVerseRules, FrmEscolherVerseRules);
  Application.CreateForm(TfrmExportarProjeto, frmExportarProjeto);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormExportPatch, FrmExportPatch);
  Application.CreateForm(TFrmApplyPatch, FrmApplyPatch);
  Application.CreateForm(TFormInterlinearVerseRules, frmInterlinearVerseRules);
  Application.CreateForm(TFormPleaseWait, FormPleaseWait);
  Application.CreateForm(TFrmLLMOptions, FrmLLMOptions);
  Application.CreateForm(TFrmAIProcessing, FrmAIProcessing);
  Application.Run;
end.

