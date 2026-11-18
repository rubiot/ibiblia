unit project_book_import;

{ "Add Bible Books" and the partial "Replace text" of the project
  properties (Texts tab).

  TBookImportRequest is what the user chose in the book dialog: the verses
  to set in one text of the project. Adding books may need new rows in the
  project file (apocrypha / deuterocanonical books 67+, or the other
  Testament when an OT-only or NT-only project becomes a whole-Bible
  project); PrepareProjectRows adds them while the project is closed and
  keeps the rows in Bible order (canonical books, then 67+). The verse texts
  themselves are set afterwards through TProject.ApplyVerseTexts, so
  associations and status follow the normal rules. This unit has no
  user-interface code. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BibleScope;

type
  TBookImportKind = (bikAdd, bikReplace);

  { TBookImportRequest }

  TBookImportRequest = class
  public
    Kind: TBookImportKind;
    TextIndex: Integer;          // Ord(TTipoTextoBiblico)
    FileName: string;
    Refs: TStringList;           // "b,c,v"
    Texts: TStringList;          // same order as Refs
    KeepAssociations: Boolean;
    NewScope: TEscopoTexto;      // etNone = unchanged
    ApocryphaInfo: string;       // "67=Tobit;68=Judith" for new books
    Summary: string;             // books, for the review page
    constructor Create;
    destructor Destroy; override;
    { True when the project file needs new rows first. }
    function NeedsNewRows(const ExistingRefs: TStrings;
      CurrentScope: TEscopoTexto): Boolean;
  end;

{ Adds the rows the requests need (and the other Testament's rows when
  NewScope is etONT) to a closed project file and stores the names of new
  apocrypha books. False + ErrorMessage on failure (the file is unchanged). }
function PrepareProjectRows(const ProjectFile: string; Requests: TList;
  CurrentScope: TEscopoTexto; out ErrorMessage: string): Boolean;

implementation

uses
  sqlite3conn, sqldb, db, bible_versification, apocrypha_books;

{ TBookImportRequest }

constructor TBookImportRequest.Create;
begin
  inherited Create;
  Refs := TStringList.Create;
  Texts := TStringList.Create;
  NewScope := etNone;
  KeepAssociations := True;
end;

destructor TBookImportRequest.Destroy;
begin
  Refs.Free;
  Texts.Free;
  inherited Destroy;
end;

function TBookImportRequest.NeedsNewRows(const ExistingRefs: TStrings;
  CurrentScope: TEscopoTexto): Boolean;
var
  Index: Integer;
begin
  if (NewScope <> etNone) and (NewScope <> CurrentScope) then
    Exit(True);
  for Index := 0 to Refs.Count - 1 do
    if ExistingRefs.IndexOf(Refs[Index]) < 0 then
      Exit(True);
  Result := False;
end;

function PrepareProjectRows(const ProjectFile: string; Requests: TList;
  CurrentScope: TEscopoTexto; out ErrorMessage: string): Boolean;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Qry, Insert: TSQLQuery;
  Existing: TStringList;
  TargetScope: TEscopoTexto;
  Enumerator: TBibleReferenceEnumerator;
  Index, RequestIndex, Added: Integer;
  Request: TBookImportRequest;
  CreateSql, Info, MergedInfo: string;

  procedure AddRow(const Ref: string);
  begin
    if Existing.IndexOf(Ref) >= 0 then
      Exit;
    Insert.Params.ParamByName('id').AsString := Ref;
    Insert.ExecSQL;
    Existing.Add(Ref);
    Inc(Added);
  end;

begin
  Result := False;
  ErrorMessage := '';
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Qry := TSQLQuery.Create(nil);
  Insert := TSQLQuery.Create(nil);
  Existing := TStringList.Create;
  try
    try
      Connection.DatabaseName := ProjectFile;
      Connection.Transaction := Transaction;
      Connection.Open;
      Qry.DataBase := Connection;
      Qry.Transaction := Transaction;
      Insert.DataBase := Connection;
      Insert.Transaction := Transaction;
      Transaction.StartTransaction;

      Qry.SQL.Text := 'SELECT pare_id FROM pares';
      Qry.Open;
      while not Qry.EOF do
      begin
        Existing.Add(Qry.Fields[0].AsString);
        Qry.Next;
      end;
      Qry.Close;
      Existing.Sorted := True;

      Insert.SQL.Text := 'INSERT INTO pares (pare_id, pare_ref, ' +
        'pare_texto_origem, pare_texto_destino, pare_texto_consulta1, ' +
        'pare_texto_consulta2, pare_pares, pare_situacao, pare_comentarios) ' +
        'VALUES (:id, :id, '''', '''', '''', '''', '''', 0, '''')';
      Insert.Prepare;

      Added := 0;
      TargetScope := CurrentScope;
      MergedInfo := '';
      for RequestIndex := 0 to Requests.Count - 1 do
      begin
        Request := TBookImportRequest(Requests[RequestIndex]);
        if Request.Kind <> bikAdd then
          Continue;
        if Request.NewScope = etONT then
          TargetScope := etONT;
        if Request.ApocryphaInfo <> '' then
          MergedInfo := MergeApocryphaInfo(MergedInfo, Request.ApocryphaInfo);
      end;

      { the other Testament }
      if TargetScope <> CurrentScope then
      begin
        Enumerator := TBibleReferenceEnumerator.Create(etONT);
        try
          while Enumerator.MoveNext do
            AddRow(Enumerator.ReferenceID);
        finally
          Enumerator.Free;
        end;
      end;

      { apocrypha rows and anything else that is missing }
      for RequestIndex := 0 to Requests.Count - 1 do
      begin
        Request := TBookImportRequest(Requests[RequestIndex]);
        if Request.Kind <> bikAdd then
          Continue;
        for Index := 0 to Request.Refs.Count - 1 do
          AddRow(Request.Refs[Index]);
      end;

      if Added > 0 then
      begin
        { keep the rows in Bible order: canonical books, then 67+ }
        Qry.SQL.Text := 'SELECT sql FROM sqlite_master WHERE type = ''table'' ' +
          'AND name = ''pares''';
        Qry.Open;
        CreateSql := Qry.Fields[0].AsString;
        Qry.Close;
        CreateSql := StringReplace(CreateSql, 'pares', 'pares_ordered',
          [rfIgnoreCase]);
        Connection.ExecuteDirect(CreateSql);
        Connection.ExecuteDirect(
          'INSERT INTO pares_ordered SELECT * FROM pares ORDER BY ' +
          'CASE WHEN CAST(pare_id AS INTEGER) >= 67 THEN 1 ELSE 0 END, ' +
          'CAST(pare_id AS INTEGER), ' +
          'CAST(substr(pare_id, instr(pare_id, '','') + 1) AS INTEGER), ' +
          'CAST(substr(substr(pare_id, instr(pare_id, '','') + 1), ' +
          'instr(substr(pare_id, instr(pare_id, '','') + 1), '','') + 1) AS INTEGER)');
        Connection.ExecuteDirect('DROP TABLE pares');
        Connection.ExecuteDirect('ALTER TABLE pares_ordered RENAME TO pares');
      end;

      if MergedInfo <> '' then
      begin
        Qry.SQL.Text := 'SELECT valor FROM info WHERE id = ''apocrypha.books''';
        Qry.Open;
        if Qry.EOF then
          Info := ''
        else
          Info := Qry.Fields[0].AsString;
        Qry.Close;
        MergedInfo := MergeApocryphaInfo(Info, MergedInfo);
        Connection.ExecuteDirect('DELETE FROM info WHERE id = ''apocrypha.books''');
        Qry.SQL.Text := 'INSERT INTO info (id, valor) VALUES (''apocrypha.books'', :v)';
        Qry.Params.ParamByName('v').AsString := MergedInfo;
        Qry.ExecSQL;
      end;

      Transaction.Commit;
      Result := True;
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        if Transaction.Active then
          Transaction.Rollback;
        Result := False;
      end;
    end;
  finally
    Existing.Free;
    Insert.Free;
    Qry.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

end.
