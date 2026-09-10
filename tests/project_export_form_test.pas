unit project_export_form_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Controls, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Spin, fpcunit, testregistry, BibleScope, Project,
  FormProjectExport, project_exporter, ConcordanceExport,
  export_destination_history;

type
  TScriptedExportSaveDialog = class(TSaveDialog)
  public
    Accepted: Boolean;
    SelectedFileName, ConfiguredFileName, ConfiguredInitialDir: string;
    ConfiguredFilterIndex, ExecuteCount: Integer;
    Events: TStrings;
    function Execute: Boolean; override;
  end;

  TExportFormTestIni = class(TIniFile)
  public
    FailWrite: Boolean;
    WriteCount: Integer;
    Events: TStrings;
    procedure WriteString(const Section, Ident, Value: string); override;
  end;

  TExportFormHarness = class(TProjectExportForm)
  private
    function RecordExport(Kind: TExportDestinationKind;
      const FileName: string; Options: TOpcoesExportacao): TExportResult;
  protected
    function PerformTaggedDestinationExport(const FileName: string;
      Options: TOpcoesExportacao): TExportResult; override;
    function PerformInterlinearSourceExport(const FileName: string;
      Options: TOpcoesExportacao): TExportResult; override;
    function PerformConcordanceExport(const FileName: string;
      Options: TOpcoesExportacao; const Abbreviation: string;
      ContextMode: TConcordanceContextMode;
      ContextMaxCharacters: Integer): TExportResult; override;
  public
    Settings: TIniFile;
    ProjectFileName: string;
    Events: TStrings;
    ResultToReturn, ShownResult: TExportResult;
    ExportCount, ResultCount: Integer;
    LastKind: TExportDestinationKind;
    LastOptions: TOpcoesExportacao;
    LastExportFileName, RememberedBeforeExport, LastAbbreviation: string;
    LastContextMode: TConcordanceContextMode;
    LastContextMaxCharacters: Integer;
    ShownPreferenceWarning, ShownMessage, ShownTitle: string;
    ShownDialogType: TMsgDlgType;
    procedure ShowExportResult(const ExportResult: TExportResult;
      const PreferenceWarning: string = ''); override;
    procedure ResetProbe;
  end;

  TProjectExportFormTests = class(TTestCase)
  private
    FTempDir: string;
    FPreviousSettings: TIniFile;
    FSettings: TExportFormTestIni;
    FEvents: TStringList;
    FHost: TForm;
    FSourcePanel, FDestinationPanel: TScrollBox;
    FStatusCombo: TComboBox;
    FProject: TProject;
    FForm: TExportFormHarness;
    function TempPath(const Name: string): string;
    function ReadSettingsBytes: string;
    function SelectedDialog(Kind: TExportDestinationKind): TScriptedExportSaveDialog;
    procedure CreateFormControls;
    procedure OpenTemporaryProject(const Name: string);
    procedure SelectExportKind(Kind: TExportDestinationKind);
    procedure Remember(Kind: TExportDestinationKind; const FileName: string);
    procedure ResetProbe;
    procedure SetResult(Status: TExportStatus);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DialogUsesCurrentProjectAndExportKindHistory;
    procedure CancelDoesNotRememberExportOrPersistOptions;
    procedure ConfirmationRemembersBeforeEveryFailedExport;
    procedure MySwordPreservesBothTheWordDestinations;
    procedure PreferenceFailureDoesNotStopSuccessfulExport;
    procedure PreferenceAndExportFailuresKeepDistinctMessages;
    procedure ModuleWarningsAndPreferenceWarningShareOneResult;
    procedure NormalSuccessKeepsExistingTranslatedPrompt;
    procedure ExportOptionsKeepExistingPersistenceTiming;
  end;

implementation

uses
  formPrincipal, project_database_bootstrap, sqlite3conn, sqldb;

function TScriptedExportSaveDialog.Execute: Boolean;
begin
  Inc(ExecuteCount);
  if Assigned(Events) then Events.Add('dialog');
  ConfiguredFileName := FileName;
  ConfiguredInitialDir := InitialDir;
  ConfiguredFilterIndex := FilterIndex;
  if SelectedFileName <> '' then FileName := SelectedFileName;
  Result := Accepted;
end;

procedure TExportFormTestIni.WriteString(const Section, Ident, Value: string);
begin
  Inc(WriteCount);
  if Assigned(Events) then Events.Add('remember');
  if FailWrite then raise Exception.Create('Injected preference write failure');
  inherited WriteString(Section, Ident, Value);
end;

function TExportFormHarness.RecordExport(Kind: TExportDestinationKind;
  const FileName: string; Options: TOpcoesExportacao): TExportResult;
var
  ReopenedSettings: TIniFile;
begin
  Inc(ExportCount);
  Events.Add('export');
  LastKind := Kind;
  LastOptions := Options;
  LastExportFileName := FileName;
  { Reopen the INI to prove the preference reached disk before export starts. }
  ReopenedSettings := TIniFile.Create(Settings.FileName);
  try
    RememberedBeforeExport := ReadExportDestination(ReopenedSettings,
      ProjectFileName, Kind, 'missing-history');
  finally
    ReopenedSettings.Free;
  end;
  Result := ResultToReturn;
  Result.OutputFileName := FileName;
end;

function TExportFormHarness.PerformTaggedDestinationExport(
  const FileName: string; Options: TOpcoesExportacao): TExportResult;
begin
  Result := RecordExport(edTaggedTheWord, FileName, Options);
end;

function TExportFormHarness.PerformInterlinearSourceExport(
  const FileName: string; Options: TOpcoesExportacao): TExportResult;
begin
  Result := RecordExport(edInterlinearTheWord, FileName, Options);
end;

function TExportFormHarness.PerformConcordanceExport(const FileName: string;
  Options: TOpcoesExportacao; const Abbreviation: string;
  ContextMode: TConcordanceContextMode;
  ContextMaxCharacters: Integer): TExportResult;
var
  Kind: TExportDestinationKind;
begin
  if oeConcordDetalhada in Options then Kind := edAnalyticalConcordance
  else Kind := edSyntheticConcordance;
  LastAbbreviation := Abbreviation;
  LastContextMode := ContextMode;
  LastContextMaxCharacters := ContextMaxCharacters;
  Result := RecordExport(Kind, FileName, Options);
end;

procedure TExportFormHarness.ShowExportResult(const ExportResult: TExportResult;
  const PreferenceWarning: string);
begin
  Inc(ResultCount);
  Events.Add('result');
  ShownResult := ExportResult;
  ShownPreferenceWarning := PreferenceWarning;
  ShownMessage := BuildExportResultMessage(ExportResult, PreferenceWarning,
    ShownTitle, ShownDialogType);
end;

procedure TExportFormHarness.ResetProbe;
begin
  ExportCount := 0;
  ResultCount := 0;
  LastExportFileName := '';
  RememberedBeforeExport := '';
  ShownPreferenceWarning := '';
  ShownMessage := '';
  ShownTitle := '';
  ShownResult := Default(TExportResult);
end;

function TProjectExportFormTests.TempPath(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + Name;
end;

function TProjectExportFormTests.ReadSettingsBytes: string;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FSettings.FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure TProjectExportFormTests.CreateFormControls;
var
  BibleDialog, ConcordanceDialog: TScriptedExportSaveDialog;
begin
  FForm := TExportFormHarness.CreateNew(nil);
  FForm.Events := FEvents;
  FForm.Settings := FSettings;
  FForm.ExportPageControl := TPageControl.Create(FForm);
  FForm.ExportPageControl.Parent := FForm;
  FForm.TaggedDestinationTab := TTabSheet.Create(FForm);
  FForm.TaggedDestinationTab.PageControl := FForm.ExportPageControl;
  FForm.InterlinearSourceTab := TTabSheet.Create(FForm);
  FForm.InterlinearSourceTab.PageControl := FForm.ExportPageControl;
  FForm.ConcordanceTab := TTabSheet.Create(FForm);
  FForm.ConcordanceTab.PageControl := FForm.ExportPageControl;
  FForm.CbExportUnassociatedAsItalics := TCheckBox.Create(FForm);
  FForm.CbExportUnassociatedAsItalics.Parent := FForm;
  FForm.CbExportMorphology := TCheckBox.Create(FForm);
  FForm.CbExportMorphology.Parent := FForm;
  FForm.CbExportComments := TCheckBox.Create(FForm);
  FForm.CbExportComments.Parent := FForm;
  FForm.CbMarkReusedStrongs := TCheckBox.Create(FForm);
  FForm.CbMarkReusedStrongs.Parent := FForm;
  FForm.CbMarkUntranslatedStrongs := TCheckBox.Create(FForm);
  FForm.CbMarkUntranslatedStrongs.Parent := FForm;
  FForm.MemoSourceFooter := TMemo.Create(FForm);
  FForm.MemoSourceFooter.Parent := FForm;
  FForm.MemoDestinationFooter := TMemo.Create(FForm);
  FForm.MemoDestinationFooter.Parent := FForm;
  FForm.rgConcordanceContext := TRadioGroup.Create(FForm);
  FForm.rgConcordanceContext.Parent := FForm;
  FForm.rgConcordanceType := TRadioGroup.Create(FForm);
  FForm.rgConcordanceType.Parent := FForm;
  FForm.lblConcordanceDescription := TLabel.Create(FForm);
  FForm.lblConcordanceDescription.Parent := FForm;
  FForm.lblConcordanceMaxCharacters := TLabel.Create(FForm);
  FForm.lblConcordanceMaxCharacters.Parent := FForm;
  FForm.lblConcordanceOutput := TLabel.Create(FForm);
  FForm.lblConcordanceOutput.Parent := FForm;
  FForm.EditAbbreviation := TLabeledEdit.Create(FForm);
  FForm.EditAbbreviation.Parent := FForm;
  FForm.seConcordanceMaxCharacters := TSpinEdit.Create(FForm);
  FForm.seConcordanceMaxCharacters.Parent := FForm;
  FForm.seConcordanceMaxCharacters.MaxValue := CONCORDANCE_CONTEXT_MAX_CHARACTERS;
  BibleDialog := TScriptedExportSaveDialog.Create(FForm);
  BibleDialog.Events := FEvents;
  FForm.SaveBibleModuleDialog := BibleDialog;
  ConcordanceDialog := TScriptedExportSaveDialog.Create(FForm);
  ConcordanceDialog.Events := FEvents;
  FForm.SaveConcordanceDialog := ConcordanceDialog;
end;

procedure TProjectExportFormTests.OpenTemporaryProject(const Name: string);
var
  DatabaseFile: string;
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  if Assigned(FProject) then
  begin
    FProject.CloseProject(False);
    FreeAndNil(FProject);
  end;
  DatabaseFile := TempPath(Name);
  if not FileExists(DatabaseFile) then
  begin
    TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT, 1);
    Connection := TSQLite3Connection.Create(nil);
    Transaction := TSQLTransaction.Create(nil);
    try
      Connection.Transaction := Transaction;
      Connection.DatabaseName := DatabaseFile;
      Connection.Open;
      Transaction.StartTransaction;
      Connection.ExecuteDirect('update pares set pare_texto_origem = ''Source'', ' +
        'pare_texto_destino = ''Destination''');
      Transaction.Commit;
    finally
      Transaction.Free;
      Connection.Free;
    end;
  end;
  FProject := TProject.Create([FSourcePanel, FDestinationPanel], nil,
    FStatusCombo, nil);
  FProject.Escopo := etNT;
  FProject.OpenProject(DatabaseFile);
  FForm.ProjectFileName := FProject.FileName;
  FForm.SetProject(FProject);
end;

procedure TProjectExportFormTests.SetUp;
begin
  inherited SetUp;
  Forms.Application.Initialize;
  FPreviousSettings := opts;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_export_form_%x_%x', [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);
  FEvents := TStringList.Create;
  FSettings := TExportFormTestIni.Create(TempPath('settings.ini'));
  FSettings.Events := FEvents;
  opts := FSettings;
  FHost := TForm.CreateNew(nil);
  FSourcePanel := TScrollBox.Create(FHost);
  FSourcePanel.Parent := FHost;
  FDestinationPanel := TScrollBox.Create(FHost);
  FDestinationPanel.Parent := FHost;
  FStatusCombo := TComboBox.Create(FHost);
  FStatusCombo.Parent := FHost;
  CreateFormControls;
  OpenTemporaryProject('project-a.bib');
  FSettings.WriteString('fixture', 'sentinel', 'unchanged');
  FSettings.UpdateFile;
  SetResult(esFailed);
  ResetProbe;
end;

procedure TProjectExportFormTests.TearDown;
var
  Search: TSearchRec;
begin
  if Assigned(FProject) then
  begin
    FProject.CloseProject(False);
    FreeAndNil(FProject);
  end;
  FForm.Free;
  FHost.Free;
  opts := FPreviousSettings;
  FSettings.Free;
  FEvents.Free;
  if FindFirst(TempPath('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
        DeleteFile(TempPath(Search.Name));
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(FTempDir);
  inherited TearDown;
end;

function TProjectExportFormTests.SelectedDialog(
  Kind: TExportDestinationKind): TScriptedExportSaveDialog;
begin
  if Kind in [edTaggedTheWord, edInterlinearTheWord] then
    Result := TScriptedExportSaveDialog(FForm.SaveBibleModuleDialog)
  else
    Result := TScriptedExportSaveDialog(FForm.SaveConcordanceDialog);
end;

procedure TProjectExportFormTests.SelectExportKind(Kind: TExportDestinationKind);
begin
  case Kind of
    edTaggedTheWord: FForm.ExportPageControl.PageIndex := 0;
    edInterlinearTheWord: FForm.ExportPageControl.PageIndex := 1;
    edAnalyticalConcordance, edSyntheticConcordance:
    begin
      FForm.ExportPageControl.PageIndex := 2;
      if Kind = edAnalyticalConcordance then FForm.rgConcordanceType.ItemIndex := 0
      else FForm.rgConcordanceType.ItemIndex := 1;
      FForm.ConcordanceTypeClick(nil);
    end;
  end;
end;

procedure TProjectExportFormTests.Remember(Kind: TExportDestinationKind;
  const FileName: string);
var
  ErrorMessage: string;
begin
  AssertTrue('Seed destination: ' + FileName,
    TryRememberExportDestination(FSettings, FProject.FileName, Kind,
      FileName, ErrorMessage));
  AssertEquals('Seed preference error', '', ErrorMessage);
end;

procedure TProjectExportFormTests.ResetProbe;
var
  Dialog: TScriptedExportSaveDialog;
begin
  FForm.ResetProbe;
  FEvents.Clear;
  FSettings.WriteCount := 0;
  Dialog := TScriptedExportSaveDialog(FForm.SaveBibleModuleDialog);
  Dialog.ExecuteCount := 0;
  Dialog.Accepted := False;
  Dialog.SelectedFileName := '';
  Dialog := TScriptedExportSaveDialog(FForm.SaveConcordanceDialog);
  Dialog.ExecuteCount := 0;
  Dialog.Accepted := False;
  Dialog.SelectedFileName := '';
end;

procedure TProjectExportFormTests.SetResult(Status: TExportStatus);
begin
  FForm.ResultToReturn := Default(TExportResult);
  FForm.ResultToReturn.Status := Status;
  if Status = esFailed then
    FForm.ResultToReturn.ErrorMessage := 'Injected module export failure';
end;

procedure TProjectExportFormTests.DialogUsesCurrentProjectAndExportKindHistory;
var
  Kind: TExportDestinationKind;
  ProjectIndex: Integer;
  Expected: string;
  Dialog: TScriptedExportSaveDialog;
begin
  for ProjectIndex := 0 to 1 do
  begin
    OpenTemporaryProject(Format('history-project-%d.bib', [ProjectIndex]));
    for Kind := Low(TExportDestinationKind) to High(TExportDestinationKind) do
      Remember(Kind, TempPath(Format('project-%d-kind-%d.nt',
        [ProjectIndex, Ord(Kind)])));
  end;
  for ProjectIndex := 0 to 1 do
  begin
    OpenTemporaryProject(Format('history-project-%d.bib', [ProjectIndex]));
    for Kind := Low(TExportDestinationKind) to High(TExportDestinationKind) do
    begin
      ResetProbe;
      SelectExportKind(Kind);
      Expected := TempPath(Format('project-%d-kind-%d.nt',
        [ProjectIndex, Ord(Kind)]));
      Dialog := SelectedDialog(Kind);
      FForm.ExportButtonClick(nil);
      AssertEquals('History reaches the selected project/type dialog',
        Expected, Dialog.ConfiguredFileName);
      AssertEquals('Initial directory follows the same destination',
        ExtractFilePath(Expected), Dialog.ConfiguredInitialDir);
      AssertEquals('theWord remains the initial format', 1, Dialog.ConfiguredFilterIndex);
      AssertEquals('One simulated dialog', 1, Dialog.ExecuteCount);
      AssertEquals('Cancelled lookup does not write', 0, FSettings.WriteCount);
    end;
  end;
end;

procedure TProjectExportFormTests.CancelDoesNotRememberExportOrPersistOptions;
var
  Kind: TExportDestinationKind;
  BeforeSettings: string;
begin
  FProject.SetInfo('propriedades.origem', 'original source footer');
  FProject.SetInfo('propriedades.destino', 'original destination footer');
  FProject.SetInfo('propriedades.abbrev', 'original abbreviation');
  FProject.SetInfo('opcoes.exportar.comentarios', '0');
  FForm.MemoSourceFooter.Text := 'changed source footer';
  FForm.MemoDestinationFooter.Text := 'changed destination footer';
  FForm.CbExportComments.Checked := True;
  for Kind := Low(TExportDestinationKind) to High(TExportDestinationKind) do
  begin
    Remember(Kind, TempPath(Format('old-%d.nt', [Ord(Kind)])));
    BeforeSettings := ReadSettingsBytes;
    ResetProbe;
    SelectExportKind(Kind);
    FForm.EditAbbreviation.Text := 'changed abbreviation';
    SelectedDialog(Kind).SelectedFileName := TempPath('cancelled.nt');
    FForm.ModalResult := mrOK;
    FForm.ExportButtonClick(nil);
    AssertEquals('Cancel preserves local settings bytes', BeforeSettings, ReadSettingsBytes);
    AssertEquals('Cancel does not attempt to write', 0, FSettings.WriteCount);
    AssertEquals('Cancel does not export', 0, FForm.ExportCount);
    AssertEquals('Cancel does not show an export result', 0, FForm.ResultCount);
    AssertEquals('Export form stays open', mrNone, FForm.ModalResult);
    AssertEquals('Source options stay unchanged', 'original source footer',
      FProject.GetInfo('propriedades.origem'));
    AssertEquals('Destination options stay unchanged', 'original destination footer',
      FProject.GetInfo('propriedades.destino'));
    AssertEquals('Concordance options stay unchanged', 'original abbreviation',
      FProject.GetInfo('propriedades.abbrev'));
    AssertEquals('Checkbox options stay unchanged', '0',
      FProject.GetInfo('opcoes.exportar.comentarios'));
  end;
end;

procedure TProjectExportFormTests.ConfirmationRemembersBeforeEveryFailedExport;
var
  Kind: TExportDestinationKind;
  Destination: string;
begin
  for Kind := Low(TExportDestinationKind) to High(TExportDestinationKind) do
  begin
    ResetProbe;
    SelectExportKind(Kind);
    Destination := TempPath(Format('confirmed-%d.nt', [Ord(Kind)]));
    SelectedDialog(Kind).Accepted := True;
    SelectedDialog(Kind).SelectedFileName := Destination;
    FForm.ExportButtonClick(nil);
    AssertEquals('Confirmed path is durable before export begins',
      Destination, FForm.RememberedBeforeExport);
    AssertEquals('Correct export method receives the path', Destination,
      FForm.LastExportFileName);
    AssertEquals('Correct export kind is dispatched', Ord(Kind), Ord(FForm.LastKind));
    AssertEquals('Dialog, preference, export and result order',
      'dialog,remember,export,result', FEvents.CommaText);
    AssertEquals('Single export attempt', 1, FForm.ExportCount);
    AssertEquals('Single result notification', 1, FForm.ResultCount);
    AssertEquals('Module failure is preserved', Ord(esFailed), Ord(FForm.ShownResult.Status));
    AssertEquals('Successful preference write has no warning', '', FForm.ShownPreferenceWarning);
    AssertEquals('Failure does not discard the chosen destination', Destination,
      ReadExportDestination(FSettings, FProject.FileName, Kind, 'missing'));
    AssertFalse('Export stub creates no module', FileExists(Destination));
  end;
end;

procedure TProjectExportFormTests.MySwordPreservesBothTheWordDestinations;
var
  Kind: TExportDestinationKind;
  PreviousDestination, MySwordDestination, BeforeSettings: string;
begin
  for Kind := edTaggedTheWord to edInterlinearTheWord do
  begin
    PreviousDestination := TempPath(Format('theword-%d.nt', [Ord(Kind)]));
    Remember(Kind, PreviousDestination);
    BeforeSettings := ReadSettingsBytes;
    ResetProbe;
    SelectExportKind(Kind);
    MySwordDestination := TempPath(Format('mysword-%d.bbl.mybible', [Ord(Kind)]));
    SelectedDialog(Kind).Accepted := True;
    SelectedDialog(Kind).SelectedFileName := MySwordDestination;
    FForm.ExportButtonClick(nil);
    AssertEquals('Dialog initially suggests remembered theWord destination',
      PreviousDestination, SelectedDialog(Kind).ConfiguredFileName);
    AssertEquals('MySword still reaches export', MySwordDestination, FForm.LastExportFileName);
    AssertEquals('MySword does not replace theWord history',
      PreviousDestination, FForm.RememberedBeforeExport);
    AssertEquals('MySword does not write preferences', 0, FSettings.WriteCount);
    AssertEquals('MySword preserves local settings bytes', BeforeSettings, ReadSettingsBytes);
    AssertEquals('MySword no-op is not a warning', '', FForm.ShownPreferenceWarning);
    AssertEquals('MySword export still has one result', 1, FForm.ResultCount);
  end;
end;

procedure TProjectExportFormTests.PreferenceFailureDoesNotStopSuccessfulExport;
begin
  SetResult(esSuccess);
  FSettings.FailWrite := True;
  SelectExportKind(edTaggedTheWord);
  SelectedDialog(edTaggedTheWord).Accepted := True;
  SelectedDialog(edTaggedTheWord).SelectedFileName := TempPath('success.nt');
  FForm.ExportButtonClick(nil);
  AssertEquals('Preference failure does not stop export', 1, FForm.ExportCount);
  AssertEquals('One combined result notification', 1, FForm.ResultCount);
  AssertEquals('Export remains successful', Ord(esSuccess), Ord(FForm.ShownResult.Status));
  AssertEquals('Preference failure uses warning title', SExportWarningsTitle, FForm.ShownTitle);
  AssertEquals('Preference failure uses warning icon', Ord(mtWarning), Ord(FForm.ShownDialogType));
  AssertTrue('Successful module is explicit', Pos(SExportCompletedMessage, FForm.ShownMessage) > 0);
  AssertTrue('Preference error remains specific',
    Pos('Injected preference write failure', FForm.ShownMessage) > 0);
  AssertTrue('Successful export still offers file location',
    Pos(SExportOpenLocation, FForm.ShownMessage) > 0);
  AssertEquals('Preference warning does not invent module warnings', 0, FForm.ShownResult.WarningCount);
end;

procedure TProjectExportFormTests.PreferenceAndExportFailuresKeepDistinctMessages;
begin
  FSettings.FailWrite := True;
  SelectExportKind(edInterlinearTheWord);
  SelectedDialog(edInterlinearTheWord).Accepted := True;
  SelectedDialog(edInterlinearTheWord).SelectedFileName := TempPath('failed.nt');
  FForm.ExportButtonClick(nil);
  AssertEquals('Export is attempted despite preference error', 1, FForm.ExportCount);
  AssertEquals('Both failures have one notification', 1, FForm.ResultCount);
  AssertEquals('Module result remains failed', Ord(esFailed), Ord(FForm.ShownResult.Status));
  AssertEquals('Module error remains the title', SExportFailedTitle, FForm.ShownTitle);
  AssertEquals('Module error remains the icon', Ord(mtError), Ord(FForm.ShownDialogType));
  AssertEquals('Module failure stays first', 1,
    Pos('Injected module export failure', FForm.ShownMessage));
  AssertTrue('Preference warning follows as a separate paragraph',
    Pos(LineEnding + LineEnding + FForm.ShownPreferenceWarning, FForm.ShownMessage) > 0);
  AssertTrue('Preference warning explains the separate failure',
    Pos('Injected preference write failure', FForm.ShownPreferenceWarning) > 0);
  AssertEquals('Failed module does not offer to open file location', 0,
    Pos(SExportOpenLocation, FForm.ShownMessage));
end;

procedure TProjectExportFormTests.ModuleWarningsAndPreferenceWarningShareOneResult;
begin
  SetResult(esWarnings);
  FForm.ResultToReturn.WarningCount := 3;
  FForm.ResultToReturn.ErrorReportFileName := TempPath('module.errors.txt');
  FForm.ResultToReturn.ErrorMessage := 'Module warning details';
  FSettings.FailWrite := True;
  SelectExportKind(edSyntheticConcordance);
  SelectedDialog(edSyntheticConcordance).Accepted := True;
  SelectedDialog(edSyntheticConcordance).SelectedFileName := TempPath('warnings.dct.twm');
  FForm.ExportButtonClick(nil);
  AssertEquals('Only one combined notification', 1, FForm.ResultCount);
  AssertEquals('Existing module warning count is unchanged', 3, FForm.ShownResult.WarningCount);
  AssertEquals('Module status remains warnings', Ord(esWarnings), Ord(FForm.ShownResult.Status));
  AssertTrue('Module warnings are described',
    Pos(Format(SExportWarningsMessage, [3]), FForm.ShownMessage) > 0);
  AssertTrue('Report location is preserved',
    Pos(FForm.ResultToReturn.ErrorReportFileName, FForm.ShownMessage) > 0);
  AssertTrue('Module warning details are preserved',
    Pos('Module warning details', FForm.ShownMessage) > 0);
  AssertTrue('Preference warning is included separately',
    Pos(FForm.ShownPreferenceWarning, FForm.ShownMessage) > 0);
end;

procedure TProjectExportFormTests.NormalSuccessKeepsExistingTranslatedPrompt;
var
  Destination: string;
begin
  SetResult(esSuccess);
  SelectExportKind(edTaggedTheWord);
  Destination := TempPath('normal-success.nt');
  SelectedDialog(edTaggedTheWord).Accepted := True;
  SelectedDialog(edTaggedTheWord).SelectedFileName := Destination;
  FForm.ExportButtonClick(nil);
  AssertEquals('Success uses the existing complete translated message',
    Format(SExportCompletedPrompt, [Destination]), FForm.ShownMessage);
  AssertEquals('Success title remains unchanged', SExportCompletedTitle, FForm.ShownTitle);
  AssertEquals('Success keeps confirmation icon', Ord(mtConfirmation), Ord(FForm.ShownDialogType));
  AssertEquals('Success has one notification', 1, FForm.ResultCount);
end;

procedure TProjectExportFormTests.ExportOptionsKeepExistingPersistenceTiming;
begin
  FForm.MemoDestinationFooter.Text := 'confirmed destination footer';
  FForm.CbExportComments.Checked := True;
  SelectExportKind(edTaggedTheWord);
  SelectedDialog(edTaggedTheWord).Accepted := True;
  SelectedDialog(edTaggedTheWord).SelectedFileName := TempPath('tagged-options.nt');
  FForm.ExportButtonClick(nil);
  AssertEquals('Confirmed destination footer persists even on failure',
    FForm.MemoDestinationFooter.Text, FProject.GetInfo('propriedades.destino'));
  AssertEquals('Confirmed checkbox persists even on failure', '1',
    FProject.GetInfo('opcoes.exportar.comentarios'));
  AssertTrue('Selected options reach tagged exporter', oeExportarComentarios in FForm.LastOptions);
  ResetProbe;
  FForm.MemoSourceFooter.Text := 'confirmed source footer';
  SelectExportKind(edInterlinearTheWord);
  SelectedDialog(edInterlinearTheWord).Accepted := True;
  SelectedDialog(edInterlinearTheWord).SelectedFileName := TempPath('interlinear-options.nt');
  FForm.ExportButtonClick(nil);
  AssertEquals('Confirmed source footer persists even on failure',
    FForm.MemoSourceFooter.Text, FProject.GetInfo('propriedades.origem'));
  FProject.SetInfo('propriedades.abbrev', 'old abbreviation');
  ResetProbe;
  SelectExportKind(edAnalyticalConcordance);
  FForm.EditAbbreviation.Text := 'new abbreviation';
  FForm.rgConcordanceContext.ItemIndex := Ord(ccmRelevantExcerpt);
  FForm.seConcordanceMaxCharacters.Value := CONCORDANCE_CONTEXT_MIN_CHARACTERS;
  SelectedDialog(edAnalyticalConcordance).Accepted := True;
  SelectedDialog(edAnalyticalConcordance).SelectedFileName := TempPath('options.dct.twm');
  FForm.ExportButtonClick(nil);
  AssertEquals('Failed concordance preserves previous options', 'old abbreviation',
    FProject.GetInfo('propriedades.abbrev'));
  AssertEquals('Current abbreviation reaches exporter', 'new abbreviation', FForm.LastAbbreviation);
  AssertEquals('Current context reaches exporter', Ord(ccmRelevantExcerpt), Ord(FForm.LastContextMode));
  AssertEquals('Current excerpt size reaches exporter', CONCORDANCE_CONTEXT_MIN_CHARACTERS,
    FForm.LastContextMaxCharacters);
  SetResult(esSuccess);
  FForm.ExportButtonClick(nil);
  AssertEquals('Successful concordance persists current options', 'new abbreviation',
    FProject.GetInfo('propriedades.abbrev'));
end;

initialization
  RegisterTest(TProjectExportFormTests);

end.
