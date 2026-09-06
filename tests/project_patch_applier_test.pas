unit project_patch_applier_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sqlite3conn, sqldb,
  project_persistence, project_patch_applier, PatchFile, Project, BibleTreeView;

type
  TProjectPatchApplierTests = class(TTestCase)
  private
    FFileName: string;
    FPersistence: TProjectPersistence;
    FApplier: TProjectPatchApplier;
    FPatch: TPatchFile;
    FMissingCount: Integer;
    function MissingReference(const Reference: string): Boolean;
    procedure AssertOriginalVerse(const Reference: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ExplicitEmptySelectionDoesNotMutateOrVisitReferences;
    procedure AllModeAppliesEveryVerseWithEmptyIndexes;
    procedure SelectedModeAppliesOnlyChosenVerse;
    procedure MissingReferencesDoNotCountAsApplied;
    procedure ProjectRejectsEmptySelectionBeforePersistence;
  end;

implementation

procedure TProjectPatchApplierTests.SetUp;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Index: Integer;
begin
  inherited SetUp;
  FFileName := GetTempFileName(GetTempDir(False), 'ppa');
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  try
    Connection.DatabaseName := FFileName;
    Connection.Transaction := Transaction;
    Connection.Open;
    Transaction.StartTransaction;
    Connection.ExecuteDirect('create table info (id text primary key, valor text)');
    Connection.ExecuteDirect('create table pares (' +
      'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
      'pare_texto_destino text, pare_texto_consulta1 text, ' +
      'pare_texto_consulta2 text, pare_pares text, pare_situacao integer, ' +
      'pare_comentarios text)');
    for Index := 1 to 2 do
      Connection.ExecuteDirect('insert into pares values (' +
        QuotedStr('1,1,' + IntToStr(Index)) + ', ' +
        QuotedStr('Genesis 1:' + IntToStr(Index)) +
        ', ''source'', ''destination'', '''', '''', ''pairs'', 2, ''comments'')');
    Transaction.Commit;
  finally
    Connection.Free;
  end;
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(FFileName);
  FApplier := TProjectPatchApplier.Create(FPersistence);
  FPatch := TPatchFile.Create;
  FPatch.Add('1,1,1', 'new source 1', 'new destination 1', '', '', 0);
  FPatch.Add('1,1,2', 'new source 2', 'new destination 2', '', '', 0);
  FMissingCount := 0;
end;

procedure TProjectPatchApplierTests.TearDown;
begin
  FPatch.Free;
  FApplier.Free;
  FPersistence.Free;
  DeleteFile(FFileName);
  inherited TearDown;
end;

function TProjectPatchApplierTests.MissingReference(
  const Reference: string): Boolean;
begin
  Inc(FMissingCount);
  Result := False;
end;

procedure TProjectPatchApplierTests.AssertOriginalVerse(const Reference: string);
begin
  AssertTrue(FPersistence.LocateReference(Reference));
  AssertEquals('source', FPersistence.CurrentFieldAsString('pare_texto_origem'));
  AssertEquals('destination', FPersistence.CurrentFieldAsString('pare_texto_destino'));
  AssertEquals('pairs', FPersistence.CurrentFieldAsString('pare_pares'));
  AssertEquals('comments', FPersistence.CurrentFieldAsString('pare_comentarios'));
  AssertEquals(2, FPersistence.CurrentFieldAsInteger('pare_situacao'));
end;

procedure TProjectPatchApplierTests.
  ExplicitEmptySelectionDoesNotMutateOrVisitReferences;
begin
  FPatch.Add('1,1,999', 'missing', 'missing', '', '', 0);
  AssertEquals(0, FApplier.ApplyPatch(FPatch, psmSelected, nil, @MissingReference));
  AssertOriginalVerse('1,1,1');
  AssertOriginalVerse('1,1,2');
  AssertFalse(FPersistence.ContentUpdatesPending);
  AssertEquals(0, FMissingCount);
end;

procedure TProjectPatchApplierTests.AllModeAppliesEveryVerseWithEmptyIndexes;
begin
  AssertEquals(2, FApplier.ApplyPatch(FPatch, psmAll, nil));
  AssertTrue(FPersistence.LocateReference('1,1,1'));
  AssertEquals('new source 1', FPersistence.CurrentFieldAsString('pare_texto_origem'));
  AssertTrue(FPersistence.LocateReference('1,1,2'));
  AssertEquals('new source 2', FPersistence.CurrentFieldAsString('pare_texto_origem'));
end;

procedure TProjectPatchApplierTests.SelectedModeAppliesOnlyChosenVerse;
var
  Indexes: TPatchIndexArray;
begin
  SetLength(Indexes, 1);
  Indexes[0] := 1;
  AssertEquals(1, FApplier.ApplyPatch(FPatch, psmSelected, Indexes));
  AssertOriginalVerse('1,1,1');
  AssertTrue(FPersistence.LocateReference('1,1,2'));
  AssertEquals('new source 2', FPersistence.CurrentFieldAsString('pare_texto_origem'));
end;

procedure TProjectPatchApplierTests.MissingReferencesDoNotCountAsApplied;
var
  Indexes: TPatchIndexArray;
begin
  FPatch.Add('1,1,999', 'missing', 'missing', '', '', 0);
  SetLength(Indexes, 1);
  Indexes[0] := 2;
  AssertEquals(0, FApplier.ApplyPatch(FPatch, psmSelected, Indexes, @MissingReference));
  AssertEquals(1, FMissingCount);
  AssertFalse(FPersistence.ContentUpdatesPending);
end;

procedure TProjectPatchApplierTests.ProjectRejectsEmptySelectionBeforePersistence;
var
  EmptyProject: TProject;
  Indexes: TIntegerList;
begin
  EmptyProject := TProject.Create;
  Indexes := TIntegerList.Create;
  try
    FPatch.Status[0] := '999';
    AssertEquals(0, EmptyProject.ApplyProjectPatch(FPatch, Indexes));
    AssertEquals(0, EmptyProject.ApplyProjectPatch(nil));
  finally
    Indexes.Free;
    EmptyProject.Free;
  end;
end;

initialization
  RegisterTest(TProjectPatchApplierTests);

end.
