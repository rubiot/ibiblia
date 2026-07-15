program iBiblia;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formPrincipal, Verse, ONTTokenizer, Project, SuggestionManager,
  VerseMemo, PCRE, ConcordanceExport, FormVerseRules, FormMergeProjects,
  FormDictionaryPopup, FormNewProject, FormProjectProperties, FormProjectExport,
  {$IFDEF WINDOWS}
  Windows, fileassociation, twAutomate, twSyncThread,
  {$ENDIF}
  ONTParser, Syntagm, unitabout, MySwordModule, TheWordDictionary, lazrichview,
  HTMLColors, FormExportPatch, FormApplyPatch, PatchFile,
  FormInterlinearVerseRules, ChapterView, UnitPleaseWait,
  llmclient, FormLlmOptions, FormAiProcessing, FormTheWordHighlighterColors;

{$R *.res}

{$IFDEF WINDOWS}
function HandleFileAssociationCommand(out AExitCode: Integer): Boolean;
const
  SAppTitle = 'iBiblia';
  SProjectFileTypeName = 'iBiblia Project File';
  SRegisterFileAssociationSuccess = 'Successfully registered file association for .bib files';
  SRegisterFileAssociationFailure = 'Failed to register file association for .bib files';
  SUnregisterFileAssociationSuccess = 'Successfully removed file association for .bib files';
  SUnregisterFileAssociationFailure = 'Failed to remove file association for .bib files';
var
  MessageText: string;
  Success: Boolean;
begin
  Result := False;
  AExitCode := 0;

  if ParamCount = 0 then
    Exit;

  if ParamStr(1) = '--register-file-assoc' then
  begin
    Result := True;
    Success := AssociateFileExtension(
      '.bib',
      'iBiblia.Project',
      SProjectFileTypeName,
      ParamStr(0),
      ParamStr(0)
    );
    if Success then
      MessageText := SRegisterFileAssociationSuccess
    else
      MessageText := SRegisterFileAssociationFailure;
  end
  else if ParamStr(1) = '--unregister-file-assoc' then
  begin
    Result := True;
    Success := RemoveFileAssociation('.bib', 'iBiblia.Project');
    if Success then
      MessageText := SUnregisterFileAssociationSuccess
    else
      MessageText := SUnregisterFileAssociationFailure;
  end
  else
    Exit;

  if Success then
    MessageBox(0, PChar(MessageText), PChar(SAppTitle), MB_OK or MB_ICONINFORMATION)
  else
  begin
    AExitCode := 1;
    MessageBox(0, PChar(MessageText), PChar(SAppTitle), MB_OK or MB_ICONERROR);
  end;
end;
{$ENDIF}

var
  StartupExitCode: Integer;

begin
  {$IFDEF WINDOWS}
  if HandleFileAssociationCommand(StartupExitCode) then
  begin
    ExitCode := StartupExitCode;
    Exit;
  end;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.CreateForm(TFrmDictionaryPopup, FrmDictionaryPopup);
  Application.CreateForm(TFrmProjectProperties, FrmProjectProperties);
  Application.CreateForm(TFrmNewProject, FrmNewProject);
  Application.CreateForm(TFrmVerseRules, FrmVerseRules);
  Application.CreateForm(TFrmProjectExport, FrmProjectExport);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFrmExportPatch, FrmExportPatch);
  Application.CreateForm(TFrmApplyPatch, FrmApplyPatch);
  Application.CreateForm(TFrmInterlinearVerseRules, FrmInterlinearVerseRules);
  Application.CreateForm(TFormPleaseWait, FormPleaseWait);
  Application.CreateForm(TFrmLLMOptions, FrmLLMOptions);
  Application.CreateForm(TFrmAIProcessing, FrmAIProcessing);
  Application.Run;
end.

