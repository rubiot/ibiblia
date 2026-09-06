unit project_exporter_integration_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, DB, fpcunit, testregistry, sqlite3conn, sqldb,
  BibleScope, project_persistence, project_verse_repository,
  verse_render_buffer, project_exporter, text_direction, rf_note_scanner,
  ConcordanceExport;

type
  TProjectExporterIntegrationTests = class(TTestCase)
  private
    FDatabaseFileName: string;
    FExportWithoutCommentsFileName: string;
    FExportWithCommentsFileName: string;
    FDatabaseSnapshot: RawByteString;
    FPersistence: TProjectPersistence;
    FRepository: TProjectVerseRepository;
    FRenderBuffer: TVerseRenderBuffer;
    FExporter: TProjectExporter;
    procedure CreateDatabase(const AComments: string);
    procedure OpenExporter;
    procedure CloseExporter;
    function ReadBinaryFile(const AFileName: string): RawByteString;
    procedure WriteBinaryFile(const FileName, Contents: string);
    procedure CorruptAssociations;
    function ReadFirstExportedVerse(const AFileName: string): string;
    procedure AssertPersistedVerseUnchanged;
    procedure AssertInvalidEditorialCommentRejected(const AComments: string);
    function NewOptions(AExportComments: Boolean):
      TDestinationTextExportOptions;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RealExportKeepsNativeRFAndTemporarilyProjectsEditorialComment;
    procedure StructuralRFInEditorialCommentAbortsExportAtomically;
    procedure WarningExportReturnsReportAndKeepsFallbackText;
    procedure ReportFailureDoesNotUndoPublishedModule;
    procedure LockedTheWordTargetReturnsFailureAndPreservesBytes;
    procedure MySwordWriteFailurePreservesPreviousOrAbsentOutput;
    procedure ConcordancePublishesReadableDatabase;
    procedure ConcordanceReadFailurePreservesPreviousOutput;
  end;

implementation

const
  SOURCE_TEXT = 'Logos<WG3056>';
  NATIVE_RF = '<  RF q=* >native <b>note</b><  Rf >';
  DESTINATION_TEXT = 'Word' + NATIVE_RF + ' tail';
  ASSOCIATIONS = '<par a="0" b="0">';
  EDITORIAL_COMMENT = 'first' + #13#10 + 'second<lf/>third<crlf/>fourth' +
    '<br>fifth<br/>sixth<p>seventh<CL>eighth<CM>ninth<CI>tenth';
  EXPORTED_EDITORIAL_RF = '<RF>first<p>second<p>third<p>fourth' +
    '<CL>fifth<CL>sixth<p>seventh<CL>eighth<CM>ninth<CI>tenth<Rf>';

procedure TProjectExporterIntegrationTests.CreateDatabase(
  const AComments: string);
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
    Connection.DatabaseName := FDatabaseFileName;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Transaction.StartTransaction;
    Connection.ExecuteDirect(
      'create table info (id text primary key, valor text)');
    Connection.ExecuteDirect('create table pares (' +
      'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
      'pare_texto_destino text, pare_texto_consulta1 text, ' +
      'pare_texto_consulta2 text, pare_pares text, pare_situacao number, ' +
      'pare_comentarios text)');
    Query.SQL.Text := 'insert into pares (' +
      'pare_id, pare_ref, pare_texto_origem, pare_texto_destino, ' +
      'pare_texto_consulta1, pare_texto_consulta2, pare_pares, ' +
      'pare_situacao, pare_comentarios) values (' +
      ':id, :reference, :source, :destination, :reference1, :reference2, ' +
      ':pairs, :status, :comments)';
    Query.ParamByName('id').AsString := '1,1,1';
    Query.ParamByName('reference').AsString := 'Genesis 1:1';
    Query.ParamByName('source').AsString := SOURCE_TEXT;
    Query.ParamByName('destination').AsString := DESTINATION_TEXT;
    Query.ParamByName('reference1').AsString := '';
    Query.ParamByName('reference2').AsString := '';
    Query.ParamByName('pairs').AsString := ASSOCIATIONS;
    Query.ParamByName('status').AsInteger := 3;
    Query.ParamByName('comments').AsString := AComments;
    Query.ExecSQL;
    Transaction.Commit;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectExporterIntegrationTests.OpenExporter;
var
  DestinationTextIndex, OriginalTextIndex: Integer;
begin
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(FDatabaseFileName);
  OriginalTextIndex := FPersistence.PairsTable.FieldByName(
    'pare_texto_origem').Index;
  DestinationTextIndex := FPersistence.PairsTable.FieldByName(
    'pare_texto_destino').Index;
  FRepository := TProjectVerseRepository.Create(FPersistence,
    OriginalTextIndex, DestinationTextIndex);
  FRenderBuffer := TVerseRenderBuffer.Create;
  FRenderBuffer.SetTokenizerVersion(2);
  FExporter := TProjectExporter.Create(etOT, FRepository, FRenderBuffer);
end;

procedure TProjectExporterIntegrationTests.CloseExporter;
begin
  FreeAndNil(FExporter);
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FRepository);
  FreeAndNil(FPersistence);
end;

function TProjectExporterIntegrationTests.ReadBinaryFile(
  const AFileName: string): RawByteString;
var
  Stream: TFileStream;
begin
  Result := '';
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function TProjectExporterIntegrationTests.ReadFirstExportedVerse(
  const AFileName: string): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    AssertTrue('theWord export must contain the verse line', Lines.Count > 0);
    Result := Lines[0].Replace(#239#187#191, '');
  finally
    Lines.Free;
  end;
end;

procedure TProjectExporterIntegrationTests.AssertPersistedVerseUnchanged;
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
    Connection.DatabaseName := FDatabaseFileName;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Transaction.StartTransaction;
    Query.SQL.Text := 'select pare_texto_origem, pare_texto_destino, ' +
      'pare_pares, pare_situacao, pare_comentarios from pares ' +
      'where pare_id = :id';
    Query.ParamByName('id').AsString := '1,1,1';
    Query.Open;
    AssertFalse('fixture Verse Pair must remain present', Query.EOF);
    AssertEquals(SOURCE_TEXT, Query.FieldByName(
      'pare_texto_origem').AsString);
    AssertEquals(DESTINATION_TEXT, Query.FieldByName(
      'pare_texto_destino').AsString);
    AssertEquals(ASSOCIATIONS, Query.FieldByName('pare_pares').AsString);
    AssertEquals(3, Query.FieldByName('pare_situacao').AsInteger);
    AssertEquals(EDITORIAL_COMMENT, Query.FieldByName(
      'pare_comentarios').AsString);
    Query.Close;
    Transaction.Rollback;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectExporterIntegrationTests.
  AssertInvalidEditorialCommentRejected(const AComments: string);
var
  ErrorMessage: string;
  Options: TDestinationTextExportOptions;
  OutputFileName: string;
  ExportResult: TExportResult;
begin
  CloseExporter;
  if FileExists(FDatabaseFileName) then
    AssertTrue('fixture database must be replaceable',
      DeleteFile(FDatabaseFileName));
  CreateDatabase(AComments);
  FDatabaseSnapshot := ReadBinaryFile(FDatabaseFileName);
  OpenExporter;

  OutputFileName := GetTempFileName(GetTempDir, 'rfe');
  try
    if FileExists(OutputFileName) then
      AssertTrue('temporary invalid-export target must be removable',
        DeleteFile(OutputFileName));
    AssertFalse('invalid-export target must start absent',
      FileExists(OutputFileName));
    ErrorMessage := '';
    Options := NewOptions(True);
    ExportResult := FExporter.ExportDestinationTextWithStrongs(
      OutputFileName, nil, Options, nil, nil, nil);
    AssertEquals(Ord(esFailed), Ord(ExportResult.Status));
    ErrorMessage := ExportResult.ErrorMessage;

    AssertTrue('export error must contain the Verse Pair reference',
      Pos('1,1,1', ErrorMessage) > 0);
    AssertTrue('export error must explain the RF delimiter',
      Pos('RF', ErrorMessage) > 0);
    AssertFalse('failed export must not create a partial output',
      FileExists(OutputFileName));
    WriteBinaryFile(OutputFileName, 'previous export');
    ExportResult := FExporter.ExportDestinationTextWithStrongs(
      OutputFileName, nil, Options, nil, nil, nil);
    AssertEquals(Ord(esFailed), Ord(ExportResult.Status));
    AssertEquals('previous export', string(ReadBinaryFile(OutputFileName)));
    AssertEquals('failed export must not stage project changes', False,
      FPersistence.ContentUpdatesPending);

    CloseExporter;
    AssertEquals('failed export must not alter SQLite bytes',
      string(FDatabaseSnapshot), string(ReadBinaryFile(FDatabaseFileName)));
  finally
    if FileExists(OutputFileName) then
      DeleteFile(OutputFileName);
  end;
end;

function TProjectExporterIntegrationTests.NewOptions(
  AExportComments: Boolean): TDestinationTextExportOptions;
begin
  Result := Default(TDestinationTextExportOptions);
  Result.ExportMorphology := True;
  Result.ExportComments := AExportComments;
  Result.Properties := 'short.title=RF integration test';
  Result.Direction := tdLeftToRight;
  Result.ErrorReportFileName := '';
end;

procedure TProjectExporterIntegrationTests.SetUp;
begin
  inherited SetUp;
  FDatabaseFileName := GetTempFileName(GetTempDir, 'rfx');
  FExportWithoutCommentsFileName := GetTempFileName(GetTempDir, 'rfo');
  FExportWithCommentsFileName := GetTempFileName(GetTempDir, 'rfi');
  CreateDatabase(EDITORIAL_COMMENT);
  FDatabaseSnapshot := ReadBinaryFile(FDatabaseFileName);
  OpenExporter;
end;

procedure TProjectExporterIntegrationTests.TearDown;
begin
  CloseExporter;
  if FileExists(FExportWithCommentsFileName) then
    DeleteFile(FExportWithCommentsFileName);
  if FileExists(FExportWithoutCommentsFileName) then
    DeleteFile(FExportWithoutCommentsFileName);
  if FileExists(FDatabaseFileName) then
    DeleteFile(FDatabaseFileName);
  inherited TearDown;
end;

procedure TProjectExporterIntegrationTests.
  RealExportKeepsNativeRFAndTemporarilyProjectsEditorialComment;
var
  Options: TDestinationTextExportOptions;
  VerseLine: string;
begin
  Options := NewOptions(False);
  FExporter.ExportDestinationTextWithStrongs(
    FExportWithoutCommentsFileName, nil, Options, nil, nil, nil);
  VerseLine := ReadFirstExportedVerse(FExportWithoutCommentsFileName);
  AssertTrue('the valid Association must contribute its Strong number',
    Pos('<WG3056>', VerseLine) > 0);
  AssertTrue('native RF must always be exported',
    Pos(NATIVE_RF, VerseLine) > 0);
  AssertTrue('text after a flexible RF closing must remain visible',
    Pos(NATIVE_RF, VerseLine) < Pos(' tail', VerseLine));
  AssertEquals('the checkbox off must omit only the editorial comment', 0,
    Pos(EXPORTED_EDITORIAL_RF, VerseLine));
  AssertEquals('no editorial comment content may leak with checkbox off', 0,
    Pos('first', VerseLine));
  AssertEquals('export must not stage project changes', False,
    FPersistence.ContentUpdatesPending);
  AssertPersistedVerseUnchanged;

  Options := NewOptions(True);
  FExporter.ExportDestinationTextWithStrongs(
    FExportWithCommentsFileName, nil, Options, nil, nil, nil);
  VerseLine := ReadFirstExportedVerse(FExportWithCommentsFileName);
  AssertTrue('the valid Association must remain rendered with comments on',
    Pos('<WG3056>', VerseLine) > 0);
  AssertTrue('native RF must remain byte-for-byte in the rendered verse',
    Pos(NATIVE_RF, VerseLine) > 0);
  AssertTrue('native RF tail must precede the projected editorial note',
    Pos(' tail', VerseLine) < Pos(EXPORTED_EDITORIAL_RF, VerseLine));
  AssertTrue('editorial comment is appended as one temporary RF',
    Pos(EXPORTED_EDITORIAL_RF, VerseLine) > 0);
  AssertTrue('temporary editorial RF follows the native inline RF',
    Pos(NATIVE_RF, VerseLine) < Pos(EXPORTED_EDITORIAL_RF, VerseLine));
  AssertEquals('export must not stage project changes', False,
    FPersistence.ContentUpdatesPending);
  AssertPersistedVerseUnchanged;

  CloseExporter;
  AssertEquals('export must not alter even the SQLite file bytes',
    string(FDatabaseSnapshot), string(ReadBinaryFile(FDatabaseFileName)));
end;

procedure TProjectExporterIntegrationTests.
  StructuralRFInEditorialCommentAbortsExportAtomically;
begin
  AssertInvalidEditorialCommentRejected(
    'before<  RF q="custom" >note body');
  AssertInvalidEditorialCommentRejected(
    'before<  Rf >LEAKED BIBLE TEXT');
end;

procedure TProjectExporterIntegrationTests.WriteBinaryFile(
  const FileName, Contents: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Contents <> '' then
      Stream.WriteBuffer(Contents[1], Length(Contents));
  finally
    Stream.Free;
  end;
end;

procedure TProjectExporterIntegrationTests.CorruptAssociations;
begin
  FPersistence.LocateReference('1,1,1');
  FPersistence.BeginCurrentEdit;
  FPersistence.SetCurrentFieldAsString('pare_pares', '<par a="999999" b="0">');
  FPersistence.PostCurrentEdit;
end;

procedure TProjectExporterIntegrationTests.WarningExportReturnsReportAndKeepsFallbackText;
var
  Options: TDestinationTextExportOptions;
  ExportResult: TExportResult;
  ReportName: string;
begin
  CorruptAssociations;
  Options := NewOptions(False);
  ReportName := FExportWithoutCommentsFileName + '.errors.txt';
  Options.ErrorReportFileName := ReportName;
  try
    ExportResult := FExporter.ExportDestinationTextWithStrongs(
      FExportWithoutCommentsFileName, nil, Options, nil, nil, nil);
    AssertEquals(ExportResult.ErrorMessage, Ord(esWarnings), Ord(ExportResult.Status));
    AssertEquals(1, ExportResult.WarningCount);
    AssertEquals(1, ExportResult.ProcessedVerseCount);
    AssertEquals(ReportName, ExportResult.ErrorReportFileName);
    AssertTrue(FileExists(ReportName));
    AssertTrue(Pos(DESTINATION_TEXT, ReadFirstExportedVerse(
      FExportWithoutCommentsFileName)) > 0);
  finally
    DeleteFile(ReportName);
  end;
end;

procedure TProjectExporterIntegrationTests.ReportFailureDoesNotUndoPublishedModule;
var
  Options: TDestinationTextExportOptions;
  ExportResult: TExportResult;
begin
  CorruptAssociations;
  Options := NewOptions(False);
  Options.ErrorReportFileName := FExportWithoutCommentsFileName +
    '.missing-folder' + PathDelim + 'errors.txt';
  ExportResult := FExporter.ExportDestinationTextWithStrongs(
    FExportWithoutCommentsFileName, nil, Options, nil, nil, nil);
  AssertEquals(Ord(esWarnings), Ord(ExportResult.Status));
  AssertTrue(FileExists(ExportResult.OutputFileName));
  AssertEquals('', ExportResult.ErrorReportFileName);
  AssertTrue(ExportResult.ErrorMessage <> '');
  AssertTrue(Pos(DESTINATION_TEXT, ReadFirstExportedVerse(
    FExportWithoutCommentsFileName)) > 0);
end;

procedure TProjectExporterIntegrationTests.LockedTheWordTargetReturnsFailureAndPreservesBytes;
{$IFDEF WINDOWS}
var
  Lock: TFileStream;
  ExportResult: TExportResult;
begin
  WriteBinaryFile(FExportWithoutCommentsFileName, 'previous export');
  Lock := TFileStream.Create(FExportWithoutCommentsFileName,
    fmOpenRead or fmShareExclusive);
  try
    ExportResult := FExporter.ExportDestinationTextWithStrongs(
      FExportWithoutCommentsFileName, nil, NewOptions(False), nil, nil, nil);
    AssertEquals(Ord(esFailed), Ord(ExportResult.Status));
    AssertEquals('', ExportResult.OutputFileName);
    AssertTrue(ExportResult.ErrorMessage <> '');
  finally
    Lock.Free;
  end;
  AssertEquals('previous export', string(ReadBinaryFile(FExportWithoutCommentsFileName)));
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TProjectExporterIntegrationTests.MySwordWriteFailurePreservesPreviousOrAbsentOutput;
var
  FileName: string;
  ExportResult: TExportResult;
  InterlinearOptions: TInterlinearTextExportOptions;
begin
  FileName := FExportWithoutCommentsFileName + '.mybible';
  try
    // The one-verse fixture fails after the SQLite module has been created.
    WriteBinaryFile(FileName, 'previous export');
    ExportResult := FExporter.ExportDestinationTextWithStrongs(
      FileName, nil, NewOptions(False), nil, nil, nil);
    AssertEquals(Ord(esFailed), Ord(ExportResult.Status));
    AssertEquals('previous export', string(ReadBinaryFile(FileName)));
    DeleteFile(FileName);
    InterlinearOptions := Default(TInterlinearTextExportOptions);
    ExportResult := FExporter.ExportInterlinearText(
      FileName, nil, nil, nil, InterlinearOptions, nil, nil, nil);
    AssertEquals(Ord(esFailed), Ord(ExportResult.Status));
    AssertFalse(FileExists(FileName));
  finally
    DeleteFile(FileName);
  end;
end;

procedure TProjectExporterIntegrationTests.ConcordancePublishesReadableDatabase;
var
  FileName: string;
  ExportResult: TExportResult;
  Options: TConcordanceExportOptions;
  Connection: TSQLite3Connection;
  Query: TSQLQuery;
begin
  FileName := FExportWithoutCommentsFileName + '.dct.twm';
  Connection := nil;
  try
    WriteBinaryFile(FileName, 'previous export');
    Options := Default(TConcordanceExportOptions);
    Options.Mode := cemSynthetic;
    Options.ContextMode := ccmReferencesOnly;
    Options.Abbreviation := 'test';
    FExporter.Attach(etNT, FRepository, FRenderBuffer);
    ExportResult := FExporter.ExportConcordance(FileName, nil, Options, nil, nil, nil);
    AssertEquals(ExportResult.ErrorMessage, Ord(esSuccess), Ord(ExportResult.Status));
    Connection := TSQLite3Connection.Create(nil);
    Connection.Transaction := TSQLTransaction.Create(Connection);
    Connection.DatabaseName := FileName;
    Query := TSQLQuery.Create(Connection);
    Query.Database := Connection;
    Query.Transaction := Connection.Transaction;
    Query.SQL.Text := 'select count(*) from topics where subject = ''G3056''';
    Query.Open;
    AssertEquals(1, Query.Fields[0].AsInteger);
    Query.Close;
    Connection.Transaction.Rollback;
    Connection.Close;
  finally
    Connection.Free;
    DeleteFile(FileName);
  end;
end;

procedure TProjectExporterIntegrationTests.ConcordanceReadFailurePreservesPreviousOutput;
var
  FileName, OriginalDirectory, EmptyDirectory: string;
  ExportResult: TExportResult;
  Options: TConcordanceExportOptions;
begin
  FileName := FExportWithoutCommentsFileName + '.dct.twm';
  EmptyDirectory := FExportWithCommentsFileName + '.directory';
  OriginalDirectory := GetCurrentDir;
  ForceDirectories(EmptyDirectory);
  try
    WriteBinaryFile(FileName, 'previous export');
    Options := Default(TConcordanceExportOptions);
    SetCurrentDir(EmptyDirectory);
    ExportResult := FExporter.ExportConcordance(FileName, nil, Options, nil, nil, nil);
    AssertEquals(Ord(esFailed), Ord(ExportResult.Status));
    AssertEquals('previous export', string(ReadBinaryFile(FileName)));
    AssertEquals('', ExportResult.OutputFileName);
  finally
    SetCurrentDir(OriginalDirectory);
    RemoveDir(EmptyDirectory);
    DeleteFile(FileName);
  end;
end;

initialization
  RegisterTest(TProjectExporterIntegrationTests);

end.
