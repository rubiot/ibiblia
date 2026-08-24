unit theword_module_import_test;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Forms, ComCtrls, fpcunit, testregistry, sqlite3conn,
  sqldb, BibleScope, Project, project_database_bootstrap,
  bible_versification, theword_module_import_layout,
  theword_module_encoding, UnitPleaseWait, LMessages;

type
  TImportProgressSpy = class(TProgressBar)
  private
    FLastObservedPosition: Integer;
    FSawIntermediatePosition: Boolean;
    FUpdateCount: Integer;
  public
    procedure Update; override;
    property LastObservedPosition: Integer read FLastObservedPosition;
    property SawIntermediatePosition: Boolean read FSawIntermediatePosition;
    property UpdateCount: Integer read FUpdateCount;
  end;

  TDelayedImportProgressSpy = class(TImportProgressSpy)
  private
    FDelayApplied: Boolean;
    FSawTimeTriggeredUpdate: Boolean;
  public
    procedure Update; override;
    property SawTimeTriggeredUpdate: Boolean read FSawTimeTriggeredUpdate;
  end;

  TFailAfterStagedCommitProgressSpy = class(TImportProgressSpy)
  private
    FConsecutiveBatchUpdates: Integer;
    FRaisedAfterCommit: Boolean;
  public
    procedure Update; override;
    property RaisedAfterCommit: Boolean read FRaisedAfterCommit;
  end;

  TTheWordModuleImportTests = class(TTestCase)
  private
    FTempDir: string;
    FAsyncProject: TProject;
    FAsyncCallbackRan: Boolean;
    function TempPath(const AFileName: string): string;
    function CreateONTModule(const AFileName, ATextPrefix: string;
      AVerseCount: Integer; AIncludeCharsetZero, AWithUtf8Bom: Boolean;
      const AFirstVerseText: string = ''): string;
    function CountImportedVerses(const ADatabaseFile, AFieldName: string): Integer;
    function CountVersesWithTextPrefix(const ADatabaseFile, AFieldName,
      APrefix: string): Integer;
    function ReadVerseText(const ADatabaseFile, AFieldName,
      AReference: string): string;
    procedure SeedExistingDestinationText(const ADatabaseFile,
      AText: string);
    procedure SeedFirstAssociatedPair(const ADatabaseFile: string);
    procedure SeedAssociatedPairAtRow(const ADatabaseFile: string;
      ARowNumber: Integer);
    procedure MoveProjectCursorAsync(Data: PtrInt);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AssociationParticipationIsLimitedToSourceAndDestination;
    procedure Utf8BomWithCharsetZeroDoesNotRequireConfirmation;
    procedure Reference2ImportPreservesAssociationsAndStatus;
    procedure CanonicalVerseCountsAndFinalReferenceAreStable;
    procedure ONTSubsetLayoutsStopBeforeModuleProperties;
    procedure ProjectExhaustionFailsWithCountsAndLeavesNoPartialText;
    procedure RejectedAssociatedReplacementFailsAndRollsBack;
    procedure ProgressUpdatesDoNotDispatchQueuedNavigation;
    {$IFDEF MSWINDOWS}
    procedure ProgressWindowPumpDispatchesOnlyTargetMessages;
    {$ENDIF}
    procedure StagedBatchFailureRestoresCommittedPrefix;
    procedure ExistingDestinationStagedFailureRestoresOriginalText;
    procedure SequentialStagedSourceAndDestinationSurviveReopen;
  end;

implementation

const
  UTF8_BOM: array[0..2] of Byte = ($EF, $BB, $BF);

{$IFDEF MSWINDOWS}
const
  WM_PROGRESS_PUMP_PROBE = WM_APP + 731;

type
  TProgressPumpProbeForm = class(TForm)
  private
    FProbeMessageCount: Integer;
  protected
    procedure WndProc(var TheMessage: TLMessage); override;
  public
    property ProbeMessageCount: Integer read FProbeMessageCount;
  end;
{$ENDIF}

procedure TImportProgressSpy.Update;
begin
  Inc(FUpdateCount);
  FLastObservedPosition := Position;
  if (Position > Min) and (Position < Max) then
    FSawIntermediatePosition := True;
  inherited Update;
end;

procedure TDelayedImportProgressSpy.Update;
begin
  if not FDelayApplied then
  begin
    FDelayApplied := True;
    Sleep(225);
  end
  else if (Position > Min) and (Position < Max) and
    ((Position mod 75) <> 0) then
    FSawTimeTriggeredUpdate := True;
  inherited Update;
end;

procedure TFailAfterStagedCommitProgressSpy.Update;
begin
  if Position = 200 then
    Inc(FConsecutiveBatchUpdates)
  else
    FConsecutiveBatchUpdates := 0;

  if (FConsecutiveBatchUpdates = 2) and not FRaisedAfterCommit then
  begin
    FRaisedAfterCommit := True;
    raise Exception.Create('Injected failure after staged batch commit');
  end;
  inherited Update;
end;

function TTheWordModuleImportTests.TempPath(const AFileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + AFileName;
end;

function TTheWordModuleImportTests.CreateONTModule(const AFileName,
  ATextPrefix: string; AVerseCount: Integer; AIncludeCharsetZero,
  AWithUtf8Bom: Boolean; const AFirstVerseText: string): string;
var
  Stream: TFileStream;
  I: Integer;
  Line: RawByteString;

  procedure WriteRaw(const AValue: RawByteString);
  begin
    if AValue <> '' then
      Stream.WriteBuffer(AValue[1], Length(AValue));
  end;

begin
  Result := TempPath(AFileName);
  Stream := TFileStream.Create(Result, fmCreate);
  try
    if AWithUtf8Bom then
      Stream.WriteBuffer(UTF8_BOM[0], SizeOf(UTF8_BOM));

    for I := 1 to AVerseCount do
    begin
      if (I = 1) and (AFirstVerseText <> '') then
        Line := UTF8Encode(AFirstVerseText) + #13#10
      else
        Line := UTF8Encode(Format('%s %d', [ATextPrefix, I])) + #13#10;
      WriteRaw(Line);
    end;

    if AIncludeCharsetZero then
      WriteRaw('charset=0' + #13#10);
  finally
    Stream.Free;
  end;
end;

function TTheWordModuleImportTests.CountImportedVerses(
  const ADatabaseFile, AFieldName: string): Integer;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Result := 0;
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := ADatabaseFile;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'select count(*) from pares where ' + AFieldName +
      ' <> ''''';
    Connection.Open;
    Transaction.StartTransaction;
    Query.Open;
    if not Query.EOF then
      Result := Query.Fields[0].AsInteger;
    Query.Close;
    Transaction.Rollback;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

function TTheWordModuleImportTests.CountVersesWithTextPrefix(
  const ADatabaseFile, AFieldName, APrefix: string): Integer;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Result := 0;
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := ADatabaseFile;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'select count(*) from pares where ' + AFieldName +
      ' like :prefix';
    Connection.Open;
    Transaction.StartTransaction;
    Query.ParamByName('prefix').AsString := APrefix + '%';
    Query.Open;
    if not Query.EOF then
      Result := Query.Fields[0].AsInteger;
    Query.Close;
    Transaction.Rollback;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

function TTheWordModuleImportTests.ReadVerseText(const ADatabaseFile,
  AFieldName, AReference: string): string;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Result := '';
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := ADatabaseFile;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'select ' + AFieldName +
      ' from pares where pare_id = :reference';
    Connection.Open;
    Transaction.StartTransaction;
    Query.ParamByName('reference').AsString := AReference;
    Query.Open;
    if not Query.EOF then
      Result := Query.Fields[0].AsString;
    Query.Close;
    Transaction.Rollback;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TTheWordModuleImportTests.SeedExistingDestinationText(
  const ADatabaseFile, AText: string);
var
  Connection: TSQLite3Connection;
  Query: TSQLQuery;
  Transaction: TSQLTransaction;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := ADatabaseFile;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'update pares set pare_texto_destino = :destination';
    Connection.Open;
    Transaction.StartTransaction;
    Query.ParamByName('destination').AsString := AText;
    Query.ExecSQL;
    Transaction.Commit;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TTheWordModuleImportTests.SeedFirstAssociatedPair(
  const ADatabaseFile: string);
begin
  SeedAssociatedPairAtRow(ADatabaseFile, 1);
end;

procedure TTheWordModuleImportTests.SeedAssociatedPairAtRow(
  const ADatabaseFile: string; ARowNumber: Integer);
var
  Connection: TSQLite3Connection;
  Query: TSQLQuery;
  Transaction: TSQLTransaction;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := ADatabaseFile;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'update pares set pare_texto_origem = :source, ' +
      'pare_texto_destino = :destination, pare_pares = :pairs, ' +
      'pare_situacao = :status ' +
      'where rowid = :row_number';
    Connection.Open;
    Transaction.StartTransaction;
    Query.ParamByName('source').AsString := 'Original associated text';
    Query.ParamByName('destination').AsString := 'Destination associated text';
    Query.ParamByName('pairs').AsString := '<par a="bad" b="0">';
    Query.ParamByName('status').AsInteger := 2;
    Query.ParamByName('row_number').AsInteger := ARowNumber;
    Query.ExecSQL;
    Transaction.Commit;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TTheWordModuleImportTests.MoveProjectCursorAsync(Data: PtrInt);
begin
  FAsyncCallbackRan := True;
  if Assigned(FAsyncProject) then
    FAsyncProject.LastVerse;
end;

{$IFDEF MSWINDOWS}
procedure TProgressPumpProbeForm.WndProc(var TheMessage: TLMessage);
begin
  if TheMessage.Msg = WM_PROGRESS_PUMP_PROBE then
  begin
    Inc(FProbeMessageCount);
    TheMessage.Result := 0;
    Exit;
  end;
  inherited WndProc(TheMessage);
end;

procedure TTheWordModuleImportTests.
  ProgressWindowPumpDispatchesOnlyTargetMessages;
var
  ProbeForm: TProgressPumpProbeForm;
begin
  ProbeForm := TProgressPumpProbeForm.CreateNew(nil);
  try
    FAsyncCallbackRan := False;
    Application.QueueAsyncCall(@MoveProjectCursorAsync, 0);
    AssertTrue('probe message must be queued for the progress window',
      Windows.PostMessage(HWND(ProbeForm.Handle), WM_PROGRESS_PUMP_PROBE,
        0, 0));

    TFormPleaseWait.PumpProgressPaintMessages(ProbeForm);

    AssertEquals(
      'the progress window must dispatch messages addressed to itself', 1,
      ProbeForm.ProbeMessageCount);
    AssertFalse(
      'the progress window pump must not dispatch application async calls',
      FAsyncCallbackRan);
  finally
    Application.RemoveAsyncCalls(Self);
    ProbeForm.Free;
  end;
end;
{$ENDIF}

procedure TTheWordModuleImportTests.SetUp;
begin
  inherited SetUp;
  FAsyncProject := nil;
  FAsyncCallbackRan := False;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_theword_module_import_%x_%x',
      [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);
end;

procedure TTheWordModuleImportTests.TearDown;
var
  Search: TSearchRec;
begin
  Application.RemoveAsyncCalls(Self);
  FAsyncProject := nil;
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

procedure TTheWordModuleImportTests.
  AssociationParticipationIsLimitedToSourceAndDestination;
begin
  AssertTrue(TextTypeParticipatesInAssociations(tbOrigem));
  AssertTrue(TextTypeParticipatesInAssociations(tbDestino));
  AssertFalse(TextTypeParticipatesInAssociations(tbConsulta1));
  AssertFalse(TextTypeParticipatesInAssociations(tbConsulta2));
  AssertFalse(TextTypeParticipatesInAssociations(tbInterlinear));
  AssertFalse(TextTypeParticipatesInAssociations(tbNone));
end;

procedure TTheWordModuleImportTests.
  Utf8BomWithCharsetZeroDoesNotRequireConfirmation;
var
  ModuleFile: string;
  EncodingInfo: TTheWordModuleEncodingInfo;
begin
  ModuleFile := CreateONTModule('utf8-bom.ont', 'Texto em português',
    3, True, True);

  EncodingInfo := InspectTheWordModuleEncodingFile(ModuleFile);

  AssertTrue('UTF-8 BOM must be detected', EncodingInfo.HasUtf8Bom);
  AssertTrue('the complete payload must be strict UTF-8',
    EncodingInfo.IsStrictUtf8);
  AssertEquals('encoding kind', Ord(twmekUtf8Bom), Ord(EncodingInfo.Kind));
  AssertFalse('charset=0 is metadata and must not override valid UTF-8 bytes',
    EncodingInfo.RequiresConfirmation);
end;

procedure TTheWordModuleImportTests.
  Reference2ImportPreservesAssociationsAndStatus;
const
  FIRST_NT_REFERENCE = '40,1,1';
  FIXTURE_VERSE_COUNT = 5;
var
  DatabaseFile: string;
  ImportResult: TTheWordModuleImportResult;
  ModuleFile: string;
  ProjectInstance: TProject;
begin
  DatabaseFile := TempPath('reference2-preserves-associations.bib');
  ModuleFile := CreateONTModule('reference2-preserves-associations.nt',
    'Reference two', FIXTURE_VERSE_COUNT, False, True);
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT,
    FIXTURE_VERSE_COUNT);
  SeedFirstAssociatedPair(DatabaseFile);

  ProjectInstance := TProject.Create;
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);
    AssertTrue('a reference import must complete even when replace is true',
      ProjectInstance.ImportTheWordModuleFixture(ModuleFile, tbConsulta2, nil,
        True, ImportResult, True, FIXTURE_VERSE_COUNT));
    AssertEquals('status', Ord(twmisSucceeded), Ord(ImportResult.Status));
    ProjectInstance.Commit;
  finally
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;

  AssertEquals('reference 2 receives the imported text', 'Reference two 1',
    ReadVerseText(DatabaseFile, 'pare_texto_consulta2',
      FIRST_NT_REFERENCE));
  AssertEquals('source text is untouched', 'Original associated text',
    ReadVerseText(DatabaseFile, 'pare_texto_origem', FIRST_NT_REFERENCE));
  AssertEquals('destination text is untouched',
    'Destination associated text', ReadVerseText(DatabaseFile,
      'pare_texto_destino', FIRST_NT_REFERENCE));
  AssertEquals('the Association is untouched', '<par a="bad" b="0">',
    ReadVerseText(DatabaseFile, 'pare_pares', FIRST_NT_REFERENCE));
  AssertEquals('the review status is untouched', '2',
    ReadVerseText(DatabaseFile, 'pare_situacao', FIRST_NT_REFERENCE));
end;

procedure TTheWordModuleImportTests.
  CanonicalVerseCountsAndFinalReferenceAreStable;
var
  ReferenceID: string;
begin
  AssertEquals('canonical ONT verse count', 31102,
    DeclaredBibleVerseCount(etONT));
  AssertEquals('enumerated ONT verse count',
    DeclaredBibleVerseCount(etONT), CalculateBibleVerseCount(etONT));
  AssertEquals('canonical OT verse count', 23145,
    DeclaredBibleVerseCount(etOT));
  AssertEquals('enumerated OT verse count',
    DeclaredBibleVerseCount(etOT), CalculateBibleVerseCount(etOT));
  AssertEquals('canonical NT verse count', 7957,
    DeclaredBibleVerseCount(etNT));
  AssertEquals('enumerated NT verse count',
    DeclaredBibleVerseCount(etNT), CalculateBibleVerseCount(etNT));

  AssertTrue('the final ONT index must resolve to a verse',
    TryBibleReferenceAt(etONT, DeclaredBibleVerseCount(etONT) - 1,
      ReferenceID));
  AssertEquals('the final canonical reference', '66,22,21', ReferenceID);
  AssertFalse('the first property index must not resolve to a verse',
    TryBibleReferenceAt(etONT, DeclaredBibleVerseCount(etONT), ReferenceID));
  AssertEquals('an out-of-range reference must be empty', '', ReferenceID);
end;

procedure TTheWordModuleImportTests.
  ONTSubsetLayoutsStopBeforeModuleProperties;
var
  Layout: TTheWordModuleImportLayout;
  Resolution: TTheWordModuleImportLayoutResolution;
begin
  Resolution := ResolveTheWordModuleImportLayout('.ont', etONT, Layout);
  AssertEquals('ONT layout resolution', Ord(twmilrResolved),
    Ord(Resolution));
  AssertEquals('first ONT verse line', 0,
    FirstImportedModuleLineIndex(Layout));
  AssertEquals('last ONT verse line', QLinhas[etONT] - 1,
    LastImportedModuleLineIndex(Layout));
  AssertEquals('first ONT property line', QLinhas[etONT],
    FirstModulePropertyLineIndex(Layout));
  AssertTrue('Revelation 22:21 remains an imported verse line',
    IsImportedVerseLine(Layout, QLinhas[etONT] - 1));
  AssertFalse('module properties are not imported as verses',
    IsImportedVerseLine(Layout, QLinhas[etONT]));

  Resolution := ResolveTheWordModuleImportLayout('.ont', etOT, Layout);
  AssertEquals('ONT-to-OT layout resolution', Ord(twmilrResolved),
    Ord(Resolution));
  AssertEquals('ONT-to-OT first verse line', 0,
    FirstImportedModuleLineIndex(Layout));
  AssertEquals('ONT-to-OT last verse line', QLinhas[etOT] - 1,
    LastImportedModuleLineIndex(Layout));
  AssertEquals('ONT-to-OT expected verses', QLinhas[etOT],
    Layout.ExpectedVerseCount);
  AssertEquals('ONT properties remain after all ONT verses', QLinhas[etONT],
    FirstModulePropertyLineIndex(Layout));

  Resolution := ResolveTheWordModuleImportLayout('.ont', etNT, Layout);
  AssertEquals('ONT-to-NT layout resolution', Ord(twmilrResolved),
    Ord(Resolution));
  AssertEquals('ONT-to-NT starts after the OT subset', QLinhas[etOT],
    FirstImportedModuleLineIndex(Layout));
  AssertEquals('ONT-to-NT ends at Revelation 22:21', QLinhas[etONT] - 1,
    LastImportedModuleLineIndex(Layout));
  AssertEquals('ONT-to-NT expected verses', QLinhas[etNT],
    Layout.ExpectedVerseCount);
  AssertFalse('the first property line remains outside the NT subset',
    IsImportedVerseLine(Layout, FirstModulePropertyLineIndex(Layout)));
end;

procedure TTheWordModuleImportTests.
  ProjectExhaustionFailsWithCountsAndLeavesNoPartialText;
const
  EXPECTED_VERSE_COUNT = 4;
  AVAILABLE_PAIR_COUNT = 3;
var
  DatabaseFile: string;
  ImportResult: TTheWordModuleImportResult;
  ModuleFile: string;
  ProjectInstance: TProject;
begin
  DatabaseFile := TempPath('truncated-project.bib');
  ModuleFile := CreateONTModule('complete-source.nt', 'Source',
    EXPECTED_VERSE_COUNT, False, True);

  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT,
    AVAILABLE_PAIR_COUNT);

  ProjectInstance := TProject.Create;
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);

    AssertFalse('running out of project rows must fail the import',
      ProjectInstance.ImportTheWordModuleFixture(ModuleFile, tbOrigem, nil,
        False, ImportResult, False, EXPECTED_VERSE_COUNT));
    AssertEquals('status', Ord(twmisFailed), Ord(ImportResult.Status));
    AssertEquals('expected count', EXPECTED_VERSE_COUNT,
      ImportResult.ExpectedVerseCount);
    AssertEquals('available count', AVAILABLE_PAIR_COUNT,
      ImportResult.AvailableVerseCount);
    AssertEquals('nothing imported after failed capacity precheck', 0,
      ImportResult.ImportedVerseCount);
    AssertTrue('diagnostic must report the available count',
      Pos(IntToStr(AVAILABLE_PAIR_COUNT), ImportResult.ErrorMessage) > 0);
    AssertTrue('diagnostic must report the expected count',
      Pos(IntToStr(EXPECTED_VERSE_COUNT), ImportResult.ErrorMessage) > 0);

    { Committing after a failed import must not make its staged prefix durable. }
    ProjectInstance.Commit;
  finally
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;

  AssertEquals('a failed import must roll back its complete partial prefix', 0,
    CountImportedVerses(DatabaseFile, 'pare_texto_origem'));
end;

procedure TTheWordModuleImportTests.
  RejectedAssociatedReplacementFailsAndRollsBack;
const
  FIXTURE_VERSE_COUNT = 5;
var
  DatabaseFile: string;
  ImportResult: TTheWordModuleImportResult;
  ModuleFile: string;
  ProjectInstance: TProject;
begin
  DatabaseFile := TempPath('associated-replace.bib');
  ModuleFile := CreateONTModule('associated-replace.nt', 'Replacement',
    FIXTURE_VERSE_COUNT, False, True);
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT,
    FIXTURE_VERSE_COUNT);
  SeedFirstAssociatedPair(DatabaseFile);

  ProjectInstance := TProject.Create;
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);

    AssertFalse('an irreconcilable associated replacement must fail',
      ProjectInstance.ImportTheWordModuleFixture(ModuleFile, tbOrigem, nil,
        True, ImportResult, False, FIXTURE_VERSE_COUNT));
    AssertEquals('status', Ord(twmisFailed), Ord(ImportResult.Status));
    AssertEquals('expected fixture count', FIXTURE_VERSE_COUNT,
      ImportResult.ExpectedVerseCount);
    AssertEquals('all fixture rows were available', FIXTURE_VERSE_COUNT,
      ImportResult.AvailableVerseCount);
    AssertEquals('the rejected first row must not count as imported', 0,
      ImportResult.ImportedVerseCount);
    AssertFalse('failure details must be reported',
      ImportResult.ErrorMessage.IsEmpty);

    ProjectInstance.Commit;
  finally
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;

  AssertEquals('the original associated text must survive rollback',
    'Original associated text', ReadVerseText(DatabaseFile,
      'pare_texto_origem', '40,1,1'));
  AssertEquals('the destination text must survive rollback',
    'Destination associated text', ReadVerseText(DatabaseFile,
      'pare_texto_destino', '40,1,1'));
  AssertEquals('the Association must survive rollback',
    '<par a="bad" b="0">', ReadVerseText(DatabaseFile,
      'pare_pares', '40,1,1'));
  AssertEquals('no later verse may receive partial replacement text', 1,
    CountImportedVerses(DatabaseFile, 'pare_texto_origem'));
end;

procedure TTheWordModuleImportTests.
  ProgressUpdatesDoNotDispatchQueuedNavigation;
const
  FIRST_PROGRESS_BATCH = 75;
  REJECTED_ROW = 226;
  IMPORTED_BEFORE_FAILURE = REJECTED_ROW - 1;
var
  DatabaseFile: string;
  ImportResult: TTheWordModuleImportResult;
  ModuleFile: string;
  ProgressBar: TDelayedImportProgressSpy;
  ProjectInstance: TProject;
begin
  DatabaseFile := TempPath('progress-feedback.bib');
  ModuleFile := CreateONTModule('progress-feedback.nt', 'Replacement',
    REJECTED_ROW, False, True);
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT,
    REJECTED_ROW);
  SeedAssociatedPairAtRow(DatabaseFile, REJECTED_ROW);

  ProjectInstance := TProject.Create;
  ProgressBar := TDelayedImportProgressSpy.Create(nil);
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);
    FAsyncProject := ProjectInstance;
    Application.QueueAsyncCall(@MoveProjectCursorAsync, 0);

    AssertFalse('the deliberately invalid Association must stop the import',
      ProjectInstance.ImportTheWordModuleFixture(ModuleFile, tbOrigem,
        ProgressBar, True, ImportResult, False, REJECTED_ROW));

    AssertTrue('the progress control must be repainted after the first batch',
      ProgressBar.UpdateCount > 0);
    AssertTrue('the user must observe progress before the import finishes',
      ProgressBar.SawIntermediatePosition);
    AssertTrue('elapsed-time heartbeat must repaint before the 75-row batch',
      ProgressBar.SawTimeTriggeredUpdate);
    AssertTrue('the observed position must include the completed first batch',
      ProgressBar.LastObservedPosition >= FIRST_PROGRESS_BATCH);
    AssertEquals('the progress maximum must describe the complete fixture',
      REJECTED_ROW, ProgressBar.Max);
    AssertFalse(
      'feedback repaint must not dispatch queued navigation or async callbacks',
      FAsyncCallbackRan);
    AssertEquals('only the rows before the rejected Association were applied',
      IMPORTED_BEFORE_FAILURE, ImportResult.ImportedVerseCount);
    AssertEquals('status', Ord(twmisFailed), Ord(ImportResult.Status));
  finally
    Application.RemoveAsyncCalls(Self);
    FAsyncProject := nil;
    ProgressBar.Free;
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;

  AssertEquals('rollback must leave only the pre-existing associated row', 1,
    CountImportedVerses(DatabaseFile, 'pare_texto_origem'));
end;

procedure TTheWordModuleImportTests.
  StagedBatchFailureRestoresCommittedPrefix;
const
  COMMITTED_BATCH_SIZE = 200;
  FIXTURE_VERSE_COUNT = COMMITTED_BATCH_SIZE + 1;
var
  DatabaseFile: string;
  ImportResult: TTheWordModuleImportResult;
  ModuleFile: string;
  ProgressBar: TFailAfterStagedCommitProgressSpy;
  ProjectInstance: TProject;
begin
  DatabaseFile := TempPath('staged-reference-batch-rollback.bib');
  ModuleFile := CreateONTModule('staged-reference-batch-rollback.nt',
    'Reference', FIXTURE_VERSE_COUNT, False, True);
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT,
    FIXTURE_VERSE_COUNT);

  ProjectInstance := TProject.Create;
  ProgressBar := TFailAfterStagedCommitProgressSpy.Create(nil);
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);
    FAsyncProject := ProjectInstance;
    Application.QueueAsyncCall(@MoveProjectCursorAsync, 0);

    AssertFalse('staged reference import must stop after its first commit',
      ProjectInstance.ImportTheWordModuleFixture(ModuleFile, tbConsulta1,
        ProgressBar, False, ImportResult, True, FIXTURE_VERSE_COUNT));
    AssertTrue('the injected failure must occur after CommitPairs',
      ProgressBar.RaisedAfterCommit);
    AssertEquals('status', Ord(twmisFailed), Ord(ImportResult.Status));
    AssertEquals('the first 200-row batch must have been imported',
      COMMITTED_BATCH_SIZE, ImportResult.ImportedVerseCount);
    AssertFalse('staged batch heartbeats must not dispatch queued navigation',
      FAsyncCallbackRan);
  finally
    Application.RemoveAsyncCalls(Self);
    FAsyncProject := nil;
    ProgressBar.Free;
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;

  AssertEquals('durable rollback must remove the committed reference prefix',
    0, CountImportedVerses(DatabaseFile, 'pare_texto_consulta1'));
  AssertEquals('the first reference row must be empty after reopening storage',
    '', ReadVerseText(DatabaseFile, 'pare_texto_consulta1', '40,1,1'));
end;

procedure TTheWordModuleImportTests.
  ExistingDestinationStagedFailureRestoresOriginalText;
const
  COMMITTED_BATCH_SIZE = 200;
  FIXTURE_VERSE_COUNT = COMMITTED_BATCH_SIZE + 1;
  ORIGINAL_DESTINATION = 'Existing destination text';
var
  DatabaseFile: string;
  ImportResult: TTheWordModuleImportResult;
  ModuleFile: string;
  ProgressBar: TFailAfterStagedCommitProgressSpy;
  ProjectInstance: TProject;
begin
  DatabaseFile := TempPath('existing-destination-batch-rollback.bib');
  ModuleFile := CreateONTModule('existing-destination-batch-rollback.nt',
    'Replacement destination', FIXTURE_VERSE_COUNT, False, True);
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT,
    FIXTURE_VERSE_COUNT);
  SeedExistingDestinationText(DatabaseFile, ORIGINAL_DESTINATION);
  AssertEquals('the existing project starts with a complete destination',
    FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
      'pare_texto_destino'));

  ProjectInstance := TProject.Create;
  ProgressBar := TFailAfterStagedCommitProgressSpy.Create(nil);
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);
    FAsyncProject := ProjectInstance;
    Application.QueueAsyncCall(@MoveProjectCursorAsync, 0);

    AssertFalse(
      'replacing an existing destination must report the injected batch failure',
      ProjectInstance.ImportTheWordModuleFixture(ModuleFile, tbDestino,
        ProgressBar, True, ImportResult, True, FIXTURE_VERSE_COUNT));
    AssertTrue('the failure must occur after the first 200-row commit',
      ProgressBar.RaisedAfterCommit);
    AssertEquals('status', Ord(twmisFailed), Ord(ImportResult.Status));
    AssertEquals('the replacement must cross one complete staged batch',
      COMMITTED_BATCH_SIZE, ImportResult.ImportedVerseCount);
    AssertFalse(
      'progress repaint must not dispatch queued navigation callbacks',
      FAsyncCallbackRan);
    AssertEquals('the in-memory first verse must be restored immediately',
      ORIGINAL_DESTINATION,
      ProjectInstance.GetVerseText('40,1,1', tbDestino));
  finally
    Application.RemoveAsyncCalls(Self);
    FAsyncProject := nil;
    ProgressBar.Free;
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;

  AssertEquals('rollback must preserve every existing destination verse',
    FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
      'pare_texto_destino'));
  AssertEquals('no replacement from the committed prefix may remain durable',
    0, CountVersesWithTextPrefix(DatabaseFile, 'pare_texto_destino',
      'Replacement destination'));
  AssertEquals('the original first destination verse must survive reopening',
    ORIGINAL_DESTINATION, ReadVerseText(DatabaseFile,
      'pare_texto_destino', '40,1,1'));
end;

procedure TTheWordModuleImportTests.
  SequentialStagedSourceAndDestinationSurviveReopen;
const
  FIXTURE_VERSE_COUNT = 401;
  FIRST_NT_REFERENCE = '40,1,1';
  LAST_FIXTURE_REFERENCE = '40,13,6';
  FIRST_DESTINATION_VERSE =
    'Destino 1<RF q="1">Nota do tradutor<p>Segunda linha<Rf>';
var
  DatabaseFile: string;
  DestinationModuleFile: string;
  DestinationResult: TTheWordModuleImportResult;
  ProjectInstance: TProject;
  SourceModuleFile: string;
  SourceResult: TTheWordModuleImportResult;
begin
  DatabaseFile := TempPath('sequential-staged-import.bib');
  SourceModuleFile := CreateONTModule('sequential-source.nt', 'Source',
    FIXTURE_VERSE_COUNT, True, True);
  DestinationModuleFile := CreateONTModule('sequential-destination.nt',
    'Destination', FIXTURE_VERSE_COUNT, True, True,
    FIRST_DESTINATION_VERSE);
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT,
    FIXTURE_VERSE_COUNT);

  ProjectInstance := TProject.Create;
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);

    AssertTrue('the complete source module must import first',
      ProjectInstance.ImportTheWordModuleFixture(SourceModuleFile, tbOrigem,
        nil, False, SourceResult, True, FIXTURE_VERSE_COUNT));
    AssertEquals('source status', Ord(twmisSucceeded),
      Ord(SourceResult.Status));
    AssertEquals('all source rows must already be durable after staged import',
      FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
        'pare_texto_origem'));

    AssertTrue('the complete destination module must import second',
      ProjectInstance.ImportTheWordModuleFixture(DestinationModuleFile,
        tbDestino, nil, False, DestinationResult, True,
        FIXTURE_VERSE_COUNT));
    AssertEquals('destination status', Ord(twmisSucceeded),
      Ord(DestinationResult.Status));
    AssertEquals('the destination exists in the project dataset after import',
      FIRST_DESTINATION_VERSE,
      ProjectInstance.GetVerseText(FIRST_NT_REFERENCE, tbDestino));
    AssertEquals(
      'all destination rows must already be durable after staged import',
      FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
        'pare_texto_destino'));
    AssertEquals('the RF body must survive the destination import',
      FIRST_DESTINATION_VERSE, ReadVerseText(DatabaseFile,
        'pare_texto_destino', FIRST_NT_REFERENCE));

    AssertEquals('source expected verses', FIXTURE_VERSE_COUNT,
      SourceResult.ExpectedVerseCount);
    AssertEquals('source available verses', FIXTURE_VERSE_COUNT,
      SourceResult.AvailableVerseCount);
    AssertEquals('source imported verses', FIXTURE_VERSE_COUNT,
      SourceResult.ImportedVerseCount);
    AssertEquals('source final reference', LAST_FIXTURE_REFERENCE,
      SourceResult.LastReference);
    AssertEquals('the source property line must not replace the last verse',
      'Source 401', ReadVerseText(DatabaseFile, 'pare_texto_origem',
        LAST_FIXTURE_REFERENCE));
    AssertEquals('destination expected verses', FIXTURE_VERSE_COUNT,
      DestinationResult.ExpectedVerseCount);
    AssertEquals('destination available verses', FIXTURE_VERSE_COUNT,
      DestinationResult.AvailableVerseCount);
    AssertEquals('destination imported verses', FIXTURE_VERSE_COUNT,
      DestinationResult.ImportedVerseCount);
    AssertEquals('destination final reference', LAST_FIXTURE_REFERENCE,
      DestinationResult.LastReference);
    AssertEquals(
      'the destination property line must not replace the last verse',
      'Destination 401', ReadVerseText(DatabaseFile, 'pare_texto_destino',
        LAST_FIXTURE_REFERENCE));

    ProjectInstance.Commit;
    AssertEquals('explicit commit must preserve all destination rows',
      FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
        'pare_texto_destino'));
  finally
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;

  AssertEquals('closing the project must preserve all source rows',
    FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
      'pare_texto_origem'));
  AssertEquals('closing the project must preserve all destination rows',
    FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
      'pare_texto_destino'));

  ProjectInstance := TProject.Create;
  try
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(DatabaseFile);
    AssertEquals('reopening the project must preserve all source rows',
      FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
        'pare_texto_origem'));
    AssertEquals('reopening the project must preserve all destination rows',
      FIXTURE_VERSE_COUNT, CountImportedVerses(DatabaseFile,
        'pare_texto_destino'));
    AssertEquals('the reopened first destination verse must retain RF markup',
      FIRST_DESTINATION_VERSE, ReadVerseText(DatabaseFile,
        'pare_texto_destino', FIRST_NT_REFERENCE));
    AssertEquals('the reopened final source verse must remain canonical',
      'Source 401', ReadVerseText(DatabaseFile, 'pare_texto_origem',
        LAST_FIXTURE_REFERENCE));
    AssertEquals('the reopened final destination verse must remain canonical',
      'Destination 401', ReadVerseText(DatabaseFile, 'pare_texto_destino',
        LAST_FIXTURE_REFERENCE));
  finally
    ProjectInstance.CloseProject(False);
    ProjectInstance.Free;
  end;
end;

initialization
  RegisterTest(TTheWordModuleImportTests);
end.
