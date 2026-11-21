unit project_change_log;

{ Stores and reads a per-project change history inside each .bib SQLite file.
  Each entry records the local date/time, a short category, a one-line summary,
  and optional multi-line details so the log travels with copies and backups. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TProjectChangeLogEntry = record
    Id: Int64;
    ChangedAt: string;
    Category: string;
    Summary: string;
    Details: string;
  end;
  TProjectChangeLogArray = array of TProjectChangeLogEntry;

function EnsureProjectChangeLogTable(const ProjectFile: string): Boolean;
function RecordProjectChange(const ProjectFile, Category, Summary: string;
  const Details: string = ''): Boolean; overload;
function RecordProjectChange(const ProjectFile, Category, Summary, Details: string;
  out ErrorMessage: string): Boolean; overload;
function LoadProjectChangeLog(const ProjectFile: string;
  MaxEntries: Integer = 5000): TProjectChangeLogArray;

function FormatProjectChangeLogText(const ProjectTitle, ProjectFile: string;
  const Entries: TProjectChangeLogArray): string;
function FormatProjectChangeLogCSV(const Entries: TProjectChangeLogArray): string;

function FormatReadableVerseRef(const PareId: string): string;
function FriendlyPairFieldName(const FieldName: string): string;
function FriendlyProjectInfoName(const InfoKey: string): string;

implementation

uses
  sqlite3conn, sqldb, db, bible_book_names;

function ChangeLogTableExists(Connection: TSQLite3Connection;
  Transaction: TSQLTransaction): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'SELECT name FROM sqlite_master WHERE type = ''table'' AND name = ''change_log''';
    Query.Open;
    Result := not Query.EOF;
    Query.Close;
  finally
    Query.Free;
  end;
end;

function EnsureProjectChangeLogTable(const ProjectFile: string): Boolean;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  Result := False;
  if (Trim(ProjectFile) = '') or (not FileExists(ProjectFile)) then
    Exit;

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  try
    try
      Connection.DatabaseName := ProjectFile;
      Connection.Transaction := Transaction;
      Connection.Open;
      Connection.ExecuteDirect('pragma busy_timeout = 2000');
      if not Transaction.Active then
        Transaction.StartTransaction;

      if not ChangeLogTableExists(Connection, Transaction) then
        Connection.ExecuteDirect(
          'CREATE TABLE IF NOT EXISTS change_log (' +
          '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
          '  changed_at TEXT NOT NULL,' +
          '  category TEXT NOT NULL,' +
          '  summary TEXT NOT NULL,' +
          '  details TEXT' +
          ')');

      Transaction.Commit;
      Result := True;
    except
      if Transaction.Active then
      try
        Transaction.Rollback;
      except
      end;
      Result := False;
    end;
  finally
    Connection.Free;
  end;
end;

function RecordProjectChange(const ProjectFile, Category, Summary: string;
  const Details: string): Boolean;
var
  DummyError: string;
begin
  Result := RecordProjectChange(ProjectFile, Category, Summary, Details, DummyError);
end;

function RecordProjectChange(const ProjectFile, Category, Summary, Details: string;
  out ErrorMessage: string): Boolean;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  CleanCategory: string;
  CleanSummary: string;
begin
  Result := False;
  ErrorMessage := '';
  CleanSummary := Trim(Summary);
  if (Trim(ProjectFile) = '') or (not FileExists(ProjectFile)) or (CleanSummary = '') then
  begin
    ErrorMessage := 'Project file or change summary is empty.';
    Exit;
  end;

  CleanCategory := Trim(Category);
  if CleanCategory = '' then
    CleanCategory := 'Change';

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  Query := TSQLQuery.Create(nil);
  try
    try
      Connection.DatabaseName := ProjectFile;
      Connection.Transaction := Transaction;
      Query.DataBase := Connection;
      Query.Transaction := Transaction;
      Connection.Open;
      Connection.ExecuteDirect('pragma busy_timeout = 2000');
      if not Transaction.Active then
        Transaction.StartTransaction;

      if not ChangeLogTableExists(Connection, Transaction) then
        Connection.ExecuteDirect(
          'CREATE TABLE IF NOT EXISTS change_log (' +
          '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
          '  changed_at TEXT NOT NULL,' +
          '  category TEXT NOT NULL,' +
          '  summary TEXT NOT NULL,' +
          '  details TEXT' +
          ')');

      Query.SQL.Text :=
        'INSERT INTO change_log (changed_at, category, summary, details) ' +
        'VALUES (:changed_at, :category, :summary, :details)';
      Query.ParamByName('changed_at').AsString :=
        FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
      Query.ParamByName('category').AsString := CleanCategory;
      Query.ParamByName('summary').AsString := CleanSummary;
      Query.ParamByName('details').AsString := Details;
      Query.ExecSQL;
      Transaction.Commit;
      Result := True;
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        if Transaction.Active then
        try
          Transaction.Rollback;
        except
        end;
        Result := False;
      end;
    end;
  finally
    Query.Free;
    Connection.Free;
  end;
end;

function LoadProjectChangeLog(const ProjectFile: string;
  MaxEntries: Integer): TProjectChangeLogArray;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  Count: Integer;
  Cap: Integer;
begin
  SetLength(Result, 0);
  if (Trim(ProjectFile) = '') or (not FileExists(ProjectFile)) then
    Exit;

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  Query := TSQLQuery.Create(nil);
  try
    try
      Connection.DatabaseName := ProjectFile;
      Connection.Transaction := Transaction;
      Query.DataBase := Connection;
      Query.Transaction := Transaction;
      Connection.Open;
      Connection.ExecuteDirect('pragma busy_timeout = 2000');
      if not Transaction.Active then
        Transaction.StartTransaction;

      if not ChangeLogTableExists(Connection, Transaction) then
      begin
        Transaction.Commit;
        Exit;
      end;

      if MaxEntries > 0 then
        Query.SQL.Text :=
          'SELECT id, changed_at, category, summary, coalesce(details, '''') AS details ' +
          'FROM change_log ORDER BY id DESC LIMIT ' + IntToStr(MaxEntries)
      else
        Query.SQL.Text :=
          'SELECT id, changed_at, category, summary, coalesce(details, '''') AS details ' +
          'FROM change_log ORDER BY id DESC';

      Query.Open;
      Count := 0;
      Cap := 64;
      SetLength(Result, Cap);
      while not Query.EOF do
      begin
        if Count >= Cap then
        begin
          Cap := Cap * 2;
          SetLength(Result, Cap);
        end;
        Result[Count].Id := Query.FieldByName('id').AsLargeInt;
        Result[Count].ChangedAt := Query.FieldByName('changed_at').AsString;
        Result[Count].Category := Query.FieldByName('category').AsString;
        Result[Count].Summary := Query.FieldByName('summary').AsString;
        Result[Count].Details := Query.FieldByName('details').AsString;
        Inc(Count);
        Query.Next;
      end;
      SetLength(Result, Count);
      Query.Close;
      Transaction.Commit;
    except
      SetLength(Result, 0);
      if Transaction.Active then
      try
        Transaction.Rollback;
      except
      end;
    end;
  finally
    Query.Free;
    Connection.Free;
  end;
end;

function FormatProjectChangeLogText(const ProjectTitle, ProjectFile: string;
  const Entries: TProjectChangeLogArray): string;
var
  Lines, DetailLines: TStringList;
  I, J: Integer;
begin
  Lines := TStringList.Create;
  DetailLines := TStringList.Create;
  try
    Lines.Add('iBiblia Project Change Log');
    if Trim(ProjectTitle) <> '' then
      Lines.Add('Project: ' + ProjectTitle);
    if Trim(ProjectFile) <> '' then
      Lines.Add('File: ' + ProjectFile);
    Lines.Add('Exported: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Lines.Add('Entries: ' + IntToStr(Length(Entries)));
    Lines.Add(StringOfChar('-', 72));

    for I := 0 to High(Entries) do
    begin
      Lines.Add(Format('[%s] [%s] %s', [
        Entries[I].ChangedAt,
        Entries[I].Category,
        Entries[I].Summary
      ]));
      if Trim(Entries[I].Details) <> '' then
      begin
        DetailLines.Text := Entries[I].Details;
        for J := 0 to DetailLines.Count - 1 do
          Lines.Add('    ' + DetailLines[J]);
      end;
      Lines.Add('');
    end;
    Result := Lines.Text;
  finally
    DetailLines.Free;
    Lines.Free;
  end;
end;

function EscapeCSVField(const Value: string): string;
var
  NeedQuotes: Boolean;
  I: Integer;
  C: Char;
begin
  NeedQuotes := False;
  for I := 1 to Length(Value) do
  begin
    C := Value[I];
    if (C = ',') or (C = '"') or (C = #10) or (C = #13) then
    begin
      NeedQuotes := True;
      Break;
    end;
  end;
  if NeedQuotes then
    Result := '"' + StringReplace(Value, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := Value;
end;

function FormatProjectChangeLogCSV(const Entries: TProjectChangeLogArray): string;
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('ID,Date and Time,Category,Summary,Details');
    for I := 0 to High(Entries) do
      Lines.Add(
        IntToStr(Entries[I].Id) + ',' +
        EscapeCSVField(Entries[I].ChangedAt) + ',' +
        EscapeCSVField(Entries[I].Category) + ',' +
        EscapeCSVField(Entries[I].Summary) + ',' +
        EscapeCSVField(Entries[I].Details)
      );
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function FormatReadableVerseRef(const PareId: string): string;
var
  P1, P2: Integer;
  BookID, ChapterNum, VerseNum: Integer;
  BookName: string;
begin
  Result := Trim(PareId);
  P1 := Pos(',', Result);
  if P1 <= 0 then
    Exit;
  P2 := Pos(',', Copy(Result, P1 + 1, MaxInt));
  if P2 <= 0 then
    Exit;
  P2 := P1 + P2;

  if (not TryStrToInt(Trim(Copy(Result, 1, P1 - 1)), BookID)) or
     (not TryStrToInt(Trim(Copy(Result, P1 + 1, P2 - P1 - 1)), ChapterNum)) or
     (not TryStrToInt(Trim(Copy(Result, P2 + 1, MaxInt)), VerseNum)) then
    Exit;

  BookName := LocalizedBookNameByID(BookID);
  if BookName <> '' then
    Result := Format('%s %d:%d', [BookName, ChapterNum, VerseNum]);
end;

function FriendlyPairFieldName(const FieldName: string): string;
begin
  if FieldName = 'pare_texto_origem' then
    Result := 'Source text'
  else if FieldName = 'pare_texto_destino' then
    Result := 'Destination text'
  else if FieldName = 'pare_texto_consulta1' then
    Result := 'Reference 1 text'
  else if FieldName = 'pare_texto_consulta2' then
    Result := 'Reference 2 text'
  else if FieldName = 'pare_pares' then
    Result := 'Associations'
  else if FieldName = 'pare_situacao' then
    Result := 'Status'
  else if FieldName = 'pare_comentarios' then
    Result := 'Comments'
  else
    Result := FieldName;
end;

function FriendlyTextSlotLabel(SlotChar: Char): string;
begin
  case SlotChar of
    '0': Result := 'Source';
    '1': Result := 'Destination';
    '2': Result := 'Reference 1';
    '3': Result := 'Reference 2';
  else
    Result := '';
  end;
end;

function FriendlyProjectInfoName(const InfoKey: string): string;
var
  SlotLabel: string;
begin
  Result := '';
  if (InfoKey = '') or
     (InfoKey = 'marcador') or
     (InfoKey = 'dicionarios.configured') or
     (InfoKey = 'apocrypha.order') or
     (Pos('association_memory.', InfoKey) = 1) or
     (Pos('association.memory.', InfoKey) = 1) then
    Exit;

  if InfoKey = 'descricao' then
    Result := 'Project name'
  else if InfoKey = 'escopo' then
    Result := 'Project scope'
  else if InfoKey = 'tokenizer.version' then
    Result := 'Tokenizer version'
  else if InfoKey = 'status.catalog' then
    Result := 'Status catalog'
  else if InfoKey = 'tree.order' then
    Result := 'Bible tree order'
  else if InfoKey = 'suggestion.mode' then
    Result := 'Suggestion mode'
  else if InfoKey = 'sugestoes.automaticas' then
    Result := 'Automatic association suggestion'
  else if InfoKey = 'verse.default.mode' then
    Result := 'Default verse mode'
  else if InfoKey = 'short.title.origem' then
    Result := 'Source short title'
  else if InfoKey = 'short.title.destino' then
    Result := 'Destination short title'
  else if InfoKey = 'short.title.referencia1' then
    Result := 'Reference 1 short title'
  else if InfoKey = 'short.title.referencia2' then
    Result := 'Reference 2 short title'
  else if InfoKey = 'propriedades.origem' then
    Result := 'Source module properties'
  else if InfoKey = 'propriedades.destino' then
    Result := 'Destination module properties'
  else if InfoKey = 'propriedades.referencia1' then
    Result := 'Reference 1 module properties'
  else if InfoKey = 'propriedades.referencia2' then
    Result := 'Reference 2 module properties'
  else if (Length(InfoKey) = 10) and (Pos('textodesc', InfoKey) = 1) then
  begin
    SlotLabel := FriendlyTextSlotLabel(InfoKey[10]);
    if SlotLabel <> '' then
      Result := SlotLabel + ' short title';
  end
  else if (Length(InfoKey) = 10) and (Pos('fontenome', InfoKey) = 1) then
  begin
    SlotLabel := FriendlyTextSlotLabel(InfoKey[10]);
    if SlotLabel <> '' then
      Result := SlotLabel + ' font';
  end
  else if (Length(InfoKey) = 13) and (Pos('fontetamanho', InfoKey) = 1) then
  begin
    SlotLabel := FriendlyTextSlotLabel(InfoKey[13]);
    if SlotLabel <> '' then
      Result := SlotLabel + ' font size';
  end
  else if (Pos('fonte', InfoKey) = 1) and (Length(InfoKey) >= 7) and
          (InfoKey[6] in ['0'..'3']) and (InfoKey[7] = '.') then
  begin
    SlotLabel := FriendlyTextSlotLabel(InfoKey[6]);
    if SlotLabel <> '' then
      Result := SlotLabel + ' font';
  end
  else if (Length(InfoKey) = 10) and (Pos('dicstrong', InfoKey) = 1) then
  begin
    SlotLabel := FriendlyTextSlotLabel(InfoKey[10]);
    if SlotLabel <> '' then
      Result := SlotLabel + ' Strong''s dictionary';
  end
  else if (Length(InfoKey) = 9) and (Pos('dicmorfo', InfoKey) = 1) then
  begin
    SlotLabel := FriendlyTextSlotLabel(InfoKey[9]);
    if SlotLabel <> '' then
      Result := SlotLabel + ' morphology dictionary';
  end
  else if Pos('dicionarios.', InfoKey) = 1 then
    Result := 'Dictionary settings'
  else
    Result := InfoKey;
end;

end.
