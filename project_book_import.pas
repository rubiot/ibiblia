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
    ApocryphaOrder: string;      // "69,67": the order in the imported file
    Summary: string;             // books, for the review page
    constructor Create;
    destructor Destroy; override;
    { True when the project file needs new rows first. }
    function NeedsNewRows(const ExistingRefs: TStrings;
      CurrentScope: TEscopoTexto): Boolean;
  end;

{ Where the apocrypha / deuterocanonical books go among the rows of a project
  (and in the Bible tree): after Malachi (book 39) and before Matthew. }
const
  LAST_OT_BOOK = 39;

{ Puts the rows of a closed project file in Bible order: Genesis-Malachi, the
  apocrypha books in the order of 'apocrypha.order' (the imported source
  text; books not listed by number), then Matthew-Revelation. Does nothing
  when the rows are already in that order. Returns True if rows were moved. }
function ArrangeProjectRows(const ProjectFile: string;
  out ErrorMessage: string): Boolean;

{ Adds the rows the requests need (and the other Testament's rows when
  NewScope is etONT) to a closed project file and stores the names of new
  apocrypha books. False + ErrorMessage on failure (the file is unchanged). }
function PrepareProjectRows(const ProjectFile: string; Requests: TList;
  CurrentScope: TEscopoTexto; out ErrorMessage: string): Boolean;

{ Deletes a book (Chapter = 0), a chapter (Verse = 0) or a verse from a
  closed project file: the rows leave 'pares'. When a whole apocrypha book
  (67+) goes, its name and place are dropped from the project too. Removed
  is the number of rows deleted. False + ErrorMessage on failure (the file
  is unchanged). }
function DeleteProjectRows(const ProjectFile: string; Book, Chapter,
  Verse: Integer; out Removed: Integer; out ErrorMessage: string): Boolean;

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

function SplitRef(const Ref: string; out B, C, V: Integer): Boolean;
var
  P1, P2: Integer;
begin
  P1 := Pos(',', Ref);
  Result := P1 > 1;
  if not Result then
    Exit;
  P2 := Pos(',', Copy(Ref, P1 + 1, MaxInt));
  Result := P2 > 0;
  if not Result then
    Exit;
  B := StrToIntDef(Copy(Ref, 1, P1 - 1), 0);
  C := StrToIntDef(Copy(Ref, P1 + 1, P2 - 1), 0);
  V := StrToIntDef(Copy(Ref, P1 + P2 + 1, MaxInt), 0);
  Result := B > 0;
end;

function ScopeName(Scope: TEscopoTexto): string;
begin
  case Scope of
    etNT: Result := 'NT';
    etONT: Result := 'ONT';
  else
    Result := 'OT';
  end;
end;

{ sort key of a row: Genesis-Malachi, apocrypha (source order), Matthew-Revelation }
function RowKey(const Ref: string; const OrderList: string): string;
var
  B, C, V, Group, Rank: Integer;
begin
  if not SplitRef(Ref, B, C, V) then
    Exit('9|000000|0000|0000|' + Ref);
  Rank := B;
  if B >= FIRST_APOCRYPHA_BOOK then
  begin
    Group := 1;
    Rank := ApocryphaRank(B);
  end
  else if B <= LAST_OT_BOOK then
    Group := 0
  else
    Group := 2;
  Result := Format('%d|%.6d|%.4d|%.4d|', [Group, Rank, C, V]) + Ref;
end;

function KeyRef(const Key: string): string;
var
  Index, Bars: Integer;
begin
  Bars := 0;
  for Index := 1 to Length(Key) do
    if Key[Index] = '|' then
    begin
      Inc(Bars);
      if Bars = 4 then
        Exit(Copy(Key, Index + 1, MaxInt));
    end;
  Result := Key;
end;

{ Rebuilds pares in Bible order (see ArrangeProjectRows). Returns True if the
  rows were moved. }
function ReorderRows(Connection: TSQLite3Connection; Qry: TSQLQuery): Boolean;
var
  Current, Sorted: TStringList;
  Index: Integer;
  CreateSql, Info: string;
  Same: Boolean;
begin
  Result := False;
  Current := TStringList.Create;
  Sorted := TStringList.Create;
  try
    Qry.SQL.Text := 'SELECT valor FROM info WHERE id = ''apocrypha.order''';
    Qry.Open;
    if Qry.EOF then
      Info := ''
    else
      Info := Qry.Fields[0].AsString;
    Qry.Close;
    SetApocryphaOrder(Info);
    Qry.SQL.Text := 'SELECT pare_id FROM pares ORDER BY rowid';
    Qry.Open;
    while not Qry.EOF do
    begin
      Current.Add(RowKey(Qry.Fields[0].AsString, Info));
      Qry.Next;
    end;
    Qry.Close;
    Sorted.CaseSensitive := True;
    Sorted.Duplicates := dupAccept;
    Sorted.Sorted := True;
    Sorted.AddStrings(Current);
    Same := Sorted.Count = Current.Count;
    for Index := 0 to Current.Count - 1 do
      if (Index >= Sorted.Count) or (Current[Index] <> Sorted[Index]) then
      begin
        Same := False;
        Break;
      end;
    if Same then
      Exit;

    Connection.ExecuteDirect('CREATE TEMP TABLE row_order (pare_id TEXT PRIMARY KEY, seq INTEGER)');
    Qry.SQL.Text := 'INSERT INTO row_order (pare_id, seq) VALUES (:id, :seq)';
    Qry.Prepare;
    for Index := 0 to Sorted.Count - 1 do
    begin
      Qry.Params.ParamByName('id').AsString := KeyRef(Sorted[Index]);
      Qry.Params.ParamByName('seq').AsInteger := Index;
      Qry.ExecSQL;
    end;
    Qry.UnPrepare;

    Qry.SQL.Text := 'SELECT sql FROM sqlite_master WHERE type = ''table'' ' +
      'AND name = ''pares''';
    Qry.Open;
    CreateSql := Qry.Fields[0].AsString;
    Qry.Close;
    CreateSql := StringReplace(CreateSql, 'pares', 'pares_ordered', [rfIgnoreCase]);
    Connection.ExecuteDirect(CreateSql);
    Connection.ExecuteDirect('INSERT INTO pares_ordered SELECT p.* FROM pares p ' +
      'JOIN row_order o ON o.pare_id = p.pare_id ORDER BY o.seq');
    Connection.ExecuteDirect('DROP TABLE pares');
    Connection.ExecuteDirect('ALTER TABLE pares_ordered RENAME TO pares');
    Connection.ExecuteDirect('DROP TABLE row_order');
    Result := True;
  finally
    Current.Free;
    Sorted.Free;
  end;
end;

function ArrangeProjectRows(const ProjectFile: string;
  out ErrorMessage: string): Boolean;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Qry: TSQLQuery;
begin
  Result := False;
  ErrorMessage := '';
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Qry := TSQLQuery.Create(nil);
  try
    try
      Connection.DatabaseName := ProjectFile;
      Connection.Transaction := Transaction;
      Connection.Open;
      Qry.DataBase := Connection;
      Qry.Transaction := Transaction;
      Transaction.StartTransaction;
      { only projects with apocrypha rows need it }
      Qry.SQL.Text := 'SELECT 1 FROM pares WHERE CAST(pare_id AS INTEGER) >= ' +
        IntToStr(FIRST_APOCRYPHA_BOOK) + ' LIMIT 1';
      Qry.Open;
      if Qry.EOF then
      begin
        Qry.Close;
        Transaction.Rollback;
        Exit;
      end;
      Qry.Close;
      Result := ReorderRows(Connection, Qry);
      Transaction.Commit;
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
    Qry.Free;
    Transaction.Free;
    Connection.Free;
  end;
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
  CreateSql, Info, MergedInfo, MergedOrder: string;

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
      MergedOrder := '';
      for RequestIndex := 0 to Requests.Count - 1 do
      begin
        Request := TBookImportRequest(Requests[RequestIndex]);
        if Request.Kind <> bikAdd then
          Continue;
        if Request.NewScope = etONT then
          TargetScope := etONT;
        if Request.ApocryphaInfo <> '' then
          MergedInfo := MergeApocryphaInfo(MergedInfo, Request.ApocryphaInfo);
        if Request.ApocryphaOrder <> '' then
          MergedOrder := MergeApocryphaOrder(MergedOrder, Request.ApocryphaOrder);
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

      if MergedOrder <> '' then
      begin
        Qry.SQL.Text := 'SELECT valor FROM info WHERE id = ''apocrypha.order''';
        Qry.Open;
        if Qry.EOF then
          Info := ''
        else
          Info := Qry.Fields[0].AsString;
        Qry.Close;
        MergedOrder := MergeApocryphaOrder(Info, MergedOrder);
        Connection.ExecuteDirect('DELETE FROM info WHERE id = ''apocrypha.order''');
        Qry.SQL.Text := 'INSERT INTO info (id, valor) VALUES (''apocrypha.order'', :v)';
        Qry.Params.ParamByName('v').AsString := MergedOrder;
        Qry.ExecSQL;
      end;

      if (Added > 0) or (MergedOrder <> '') then
        ReorderRows(Connection, Qry);

      { a project that remembers its scope (after a deletion) gets the new one }
      if TargetScope <> CurrentScope then
      begin
        Qry.SQL.Text := 'SELECT 1 FROM info WHERE id = ''project.scope''';
        Qry.Open;
        Info := '';
        if not Qry.EOF then
          Info := 'x';
        Qry.Close;
        if Info <> '' then
        begin
          Connection.ExecuteDirect('DELETE FROM info WHERE id = ''project.scope''');
          Qry.SQL.Text := 'INSERT INTO info (id, valor) VALUES (''project.scope'', :v)';
          Qry.Params.ParamByName('v').AsString := ScopeName(TargetScope);
          Qry.ExecSQL;
        end;
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

{ drops book BookID from "67=Tobit;68=Judith" }
function RemoveBookFromInfo(const Info: string; BookID: Integer): string;
var
  Items: TStringList;
  Index: Integer;
begin
  Items := TStringList.Create;
  try
    Items.Delimiter := ';';
    Items.StrictDelimiter := True;
    Items.DelimitedText := Info;
    for Index := Items.Count - 1 downto 0 do
      if StrToIntDef(Trim(Items.Names[Index]), 0) = BookID then
        Items.Delete(Index);
    Result := Items.DelimitedText;
  finally
    Items.Free;
  end;
end;

{ drops book BookID from "69,67,70" }
function RemoveBookFromOrder(const Order: string; BookID: Integer): string;
var
  Items: TStringList;
  Index: Integer;
begin
  Items := TStringList.Create;
  try
    Items.Delimiter := ',';
    Items.StrictDelimiter := True;
    Items.DelimitedText := Order;
    for Index := Items.Count - 1 downto 0 do
      if StrToIntDef(Trim(Items[Index]), 0) = BookID then
        Items.Delete(Index);
    Result := Items.DelimitedText;
  finally
    Items.Free;
  end;
end;

function DeleteProjectRows(const ProjectFile: string; Book, Chapter,
  Verse: Integer; out Removed: Integer; out ErrorMessage: string): Boolean;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Qry: TSQLQuery;
  Info: string;

  procedure RewriteInfo(const Key, Value: string);
  begin
    Connection.ExecuteDirect('DELETE FROM info WHERE id = ''' + Key + '''');
    if Value <> '' then
    begin
      Qry.SQL.Text := 'INSERT INTO info (id, valor) VALUES (' + QuotedStr(Key) + ', :v)';
      Qry.Params.ParamByName('v').AsString := Value;
      Qry.ExecSQL;
    end;
  end;

  function DetectScope: TEscopoTexto;
  var
    First: string;
  begin
    Qry.SQL.Text := 'SELECT pare_id FROM pares ORDER BY rowid LIMIT 1';
    Qry.Open;
    if Qry.EOF then
      First := ''
    else
      First := Qry.Fields[0].AsString;
    Qry.Close;
    if First = '40,1,1' then
      Exit(etNT);
    Qry.SQL.Text := 'SELECT 1 FROM pares WHERE pare_id = ''66,22,21''';
    Qry.Open;
    if Qry.EOF then
      Result := etOT
    else
      Result := etONT;
    Qry.Close;
  end;

  function ReadInfoValue(const Key: string): string;
  begin
    Qry.SQL.Text := 'SELECT valor FROM info WHERE id = ' + QuotedStr(Key);
    Qry.Open;
    if Qry.EOF then
      Result := ''
    else
      Result := Qry.Fields[0].AsString;
    Qry.Close;
  end;

begin
  Result := False;
  Removed := 0;
  ErrorMessage := '';
  if Book < 1 then
  begin
    ErrorMessage := 'No book chosen';
    Exit;
  end;
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Qry := TSQLQuery.Create(nil);
  try
    try
      Connection.DatabaseName := ProjectFile;
      Connection.Transaction := Transaction;
      Connection.Open;
      Qry.DataBase := Connection;
      Qry.Transaction := Transaction;
      Transaction.StartTransaction;

      { the scope (Old Testament, New Testament or both) was found from the
        first and last verse; remember it before those may be deleted }
      if ReadInfoValue('project.scope') = '' then
        RewriteInfo('project.scope', ScopeName(DetectScope));

      if Chapter < 1 then
        Qry.SQL.Text := 'SELECT COUNT(*) FROM pares WHERE pare_id LIKE ''' +
          IntToStr(Book) + ',%'''
      else if Verse < 1 then
        Qry.SQL.Text := 'SELECT COUNT(*) FROM pares WHERE pare_id LIKE ''' +
          IntToStr(Book) + ',' + IntToStr(Chapter) + ',%'''
      else
        Qry.SQL.Text := 'SELECT COUNT(*) FROM pares WHERE pare_id = ''' +
          IntToStr(Book) + ',' + IntToStr(Chapter) + ',' + IntToStr(Verse) + '''';
      Qry.Open;
      Removed := Qry.Fields[0].AsInteger;
      Qry.Close;

      if Removed > 0 then
      begin
        if Chapter < 1 then
          Connection.ExecuteDirect('DELETE FROM pares WHERE pare_id LIKE ''' +
            IntToStr(Book) + ',%''')
        else if Verse < 1 then
          Connection.ExecuteDirect('DELETE FROM pares WHERE pare_id LIKE ''' +
            IntToStr(Book) + ',' + IntToStr(Chapter) + ',%''')
        else
          Connection.ExecuteDirect('DELETE FROM pares WHERE pare_id = ''' +
            IntToStr(Book) + ',' + IntToStr(Chapter) + ',' + IntToStr(Verse) + '''');

        if (Chapter < 1) and (Book >= FIRST_APOCRYPHA_BOOK) then
        begin
          Info := ReadInfoValue('apocrypha.books');
          if Info <> '' then
            RewriteInfo('apocrypha.books', RemoveBookFromInfo(Info, Book));
          Info := ReadInfoValue('apocrypha.order');
          if Info <> '' then
            RewriteInfo('apocrypha.order', RemoveBookFromOrder(Info, Book));
        end;
      end;

      Transaction.Commit;
      Result := True;
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        Removed := 0;
        if Transaction.Active then
          Transaction.Rollback;
        Result := False;
      end;
    end;
  finally
    Qry.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

end.