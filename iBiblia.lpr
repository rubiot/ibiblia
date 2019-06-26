program iBiblia;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formPrincipal, Versiculo, ONTTokenizer, Projeto, Sugestao,
  memoversiculo, PCRE, exportarprojeto, formverserules, formmesclarprojetos,
  formpopup, formnovoprojeto, formpropprojeto, formexportar,
  {$IFDEF WINDOWS}
  twAutomate, twSyncThread,
  {$ENDIF}
  ONTParser, Sintagma, unitabout, MySwordModule, TheWordDictionary, ChapterView,
  RTFChapterView, lazrichview;

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
  Application.Run;
end.

