program iBiblia;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formPrincipal, Versiculo, iBibliaXML, Projeto, Sugestao, RTFEdit,
  memoversiculo, PCRE, exportarprojeto, formverserules, formmesclarprojetos,
  formpopup, formnovoprojeto, formpropprojeto, formexportar;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.CreateForm(TFormNovoProjeto, FormNovoProjeto1);
  Application.CreateForm(TFormPropProjeto, FormPropProjeto1);
  Application.CreateForm(TfrmDictionaryPopup, frmDictionaryPopup);
  Application.CreateForm(TFrmEscolherVerseRules, FrmEscolherVerseRules);
  Application.CreateForm(TfrmExportarProjeto, frmExportarProjeto);
  Application.Run;
end.

