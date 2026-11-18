unit project_backup;

{ Zipped backups of an iBiblia project (.bib).

  A backup is a zip file named "<project>_backup_<date>_<time>.bib.zip"
  (made by the user) or "<project>_autobackup_<date>_<time>.bib.zip" (made
  before a change such as replacing or adding Bible books). It holds the
  project database, copied with SQLite's own backup command and checked, and
  a small "backup.ini" with the date, the note and some counts.
  Only the newest AUTO_BACKUPS_KEPT automatic backups of a project are kept;
  backups made by the user are never deleted. This unit has no user-interface
  code. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  AUTO_BACKUPS_KEPT = 2;
  BACKUP_EXTENSION = '.bib.zip';

type
  TProjectBackupInfo = record
    FileName: string;
    Created: TDateTime;
    Automatic: Boolean;
    Note: string;
    ProjectName: string;
    VerseCount: Integer;
    AssociatedVerses: Integer;
    Size: Int64;
  end;
  TProjectBackupInfoArray = array of TProjectBackupInfo;

{ Copies the project file into a new zipped backup in Folder. The project
  should be saved first. Returns the new file name; raises on failure (no
  file is left behind). Automatic backups beyond AUTO_BACKUPS_KEPT are
  removed afterwards. }
function CreateProjectBackup(const ProjectFile, Folder, Note: string;
  Automatic: Boolean): string;

{ The backups of the given project in Folder, newest first. }
function ListProjectBackups(const ProjectFile, Folder: string): TProjectBackupInfoArray;

{ Reads the description of one backup (False if it is not a project zip). }
function ReadBackupInfo(const ZipFile: string; out Info: TProjectBackupInfo): Boolean;

{ True if the zip file holds an iBiblia project (.bib). }
function IsZippedProject(const ZipFile: string): Boolean;

{ The name of the project file inside the zip ('' if none). }
function ZippedProjectName(const ZipFile: string): string;

{ Unpacks the project of a zipped backup to DestinationFile (which must not
  exist) and checks it. Raises on failure (nothing is left behind). }
procedure ExtractZippedProject(const ZipFile, DestinationFile: string);

{ Removes the oldest automatic backups of the project, keeping Keep. }
procedure PruneAutomaticBackups(const ProjectFile, Folder: string; Keep: Integer);

implementation

uses
  Zipper, IniFiles, DateUtils, sqlite3conn, sqldb, sqlite_database_copy;

const
  INFO_ENTRY = 'backup.ini';
  MANUAL_TAG = '_backup_';
  AUTO_TAG = '_autobackup_';

resourcestring
  SBackupNoProject = 'The project file does not exist: %s';
  SBackupNoFolder = 'The backup folder does not exist: %s';
  SBackupNotAProject = 'This zip file does not contain an iBiblia project: %s';
  SBackupExtractFailed = 'The project could not be unpacked from %s';
  SBackupDestinationExists = 'A file already exists at %s';

function FileSizeOf(const FileName: string): Int64;
var
  Search: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, Search) = 0 then
  begin
    Result := Search.Size;
    FindClose(Search);
  end;
end;

function ProjectBaseName(const ProjectFile: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(ProjectFile), '');
end;

function NewTempFolder(const Parent: string): string;
var
  Index: Integer;
begin
  Index := 0;
  repeat
    Result := IncludeTrailingPathDelimiter(Parent) + Format('.ibiblia-backup-%d-%d',
      [GetProcessID, Index]);
    Inc(Index);
  until not DirectoryExists(Result) and not FileExists(Result);
  if not ForceDirectories(Result) then
    raise Exception.CreateFmt(SBackupNoFolder, [Result]);
end;

procedure RemoveFolder(const Folder: string);
var
  Search: TSearchRec;
begin
  if Folder = '' then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(Folder) + '*', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
      begin
        if (Search.Attr and faDirectory) <> 0 then
          RemoveFolder(IncludeTrailingPathDelimiter(Folder) + Search.Name)
        else
          SysUtils.DeleteFile(IncludeTrailingPathDelimiter(Folder) + Search.Name);
      end;
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(Folder);
end;

procedure CountProject(const DatabaseFile: string; out Verses, Associated: Integer;
  out ProjectName: string);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Qry: TSQLQuery;
begin
  Verses := 0;
  Associated := 0;
  ProjectName := '';
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Qry := TSQLQuery.Create(nil);
  try
    try
      Connection.DatabaseName := DatabaseFile;
      Connection.Transaction := Transaction;
      Connection.Open;
      Qry.DataBase := Connection;
      Qry.Transaction := Transaction;
      Qry.SQL.Text := 'SELECT COUNT(*), SUM(CASE WHEN COALESCE(pare_pares, '''') ' +
        '<> '''' THEN 1 ELSE 0 END) FROM pares';
      Qry.Open;
      Verses := Qry.Fields[0].AsInteger;
      Associated := Qry.Fields[1].AsInteger;
      Qry.Close;
      Qry.SQL.Text := 'SELECT valor FROM info WHERE id = ''descricao''';
      Qry.Open;
      if not Qry.EOF then
        ProjectName := Qry.Fields[0].AsString;
      Qry.Close;
      Connection.Close;
    except
      { counts are only informative }
    end;
  finally
    Qry.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

function UniqueBackupName(const Folder, Base: string; Automatic: Boolean): string;
var
  Stem: string;
  Index: Integer;
begin
  if Automatic then
    Stem := Base + AUTO_TAG
  else
    Stem := Base + MANUAL_TAG;
  Stem := IncludeTrailingPathDelimiter(Folder) + Stem +
    FormatDateTime('yyyy-mm-dd_hhnn', Now);
  Result := Stem + BACKUP_EXTENSION;
  Index := 2;
  while FileExists(Result) do
  begin
    Result := Stem + '_' + IntToStr(Index) + BACKUP_EXTENSION;
    Inc(Index);
  end;
end;

function CreateProjectBackup(const ProjectFile, Folder, Note: string;
  Automatic: Boolean): string;
var
  TempFolder, TempDatabase, TempInfo, Base, ProjectName: string;
  Ini: TMemIniFile;
  Zip: TZipper;
  Verses, Associated: Integer;
begin
  if not FileExists(ProjectFile) then
    raise Exception.CreateFmt(SBackupNoProject, [ProjectFile]);
  if not DirectoryExists(Folder) and not ForceDirectories(Folder) then
    raise Exception.CreateFmt(SBackupNoFolder, [Folder]);
  Base := ProjectBaseName(ProjectFile);
  Result := UniqueBackupName(Folder, Base, Automatic);
  TempFolder := NewTempFolder(Folder);
  try
    TempDatabase := IncludeTrailingPathDelimiter(TempFolder) + Base + '.bib';
    CopySQLiteDatabase(ProjectFile, TempDatabase, False);
    ValidateSQLiteDatabase(TempDatabase);
    CountProject(TempDatabase, Verses, Associated, ProjectName);

    TempInfo := IncludeTrailingPathDelimiter(TempFolder) + INFO_ENTRY;
    Ini := TMemIniFile.Create(TempInfo);
    try
      Ini.WriteString('backup', 'created', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      if Automatic then
        Ini.WriteString('backup', 'kind', 'automatic')
      else
        Ini.WriteString('backup', 'kind', 'manual');
      Ini.WriteString('backup', 'note', StringReplace(Note, LineEnding, ' ',
        [rfReplaceAll]));
      Ini.WriteString('backup', 'project.file', ProjectFile);
      Ini.WriteString('backup', 'project.name', ProjectName);
      Ini.WriteInteger('backup', 'verses', Verses);
      Ini.WriteInteger('backup', 'verses.associated', Associated);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;

    Zip := TZipper.Create;
    try
      Zip.FileName := Result;
      Zip.Entries.AddFileEntry(TempDatabase, Base + '.bib');
      Zip.Entries.AddFileEntry(TempInfo, INFO_ENTRY);
      try
        Zip.ZipAllFiles;
      except
        SysUtils.DeleteFile(Result);
        raise;
      end;
    finally
      Zip.Free;
    end;
    if not IsZippedProject(Result) then
    begin
      SysUtils.DeleteFile(Result);
      raise Exception.CreateFmt(SBackupNotAProject, [Result]);
    end;
  finally
    RemoveFolder(TempFolder);
  end;
  if Automatic then
    PruneAutomaticBackups(ProjectFile, Folder, AUTO_BACKUPS_KEPT);
end;

function ZippedProjectName(const ZipFile: string): string;
var
  UnZip: TUnZipper;
  Index: Integer;
  Name: string;
begin
  Result := '';
  if not FileExists(ZipFile) then
    Exit;
  UnZip := TUnZipper.Create;
  try
    try
      UnZip.FileName := ZipFile;
      UnZip.Examine;
      for Index := 0 to UnZip.Entries.Count - 1 do
      begin
        Name := UnZip.Entries[Index].ArchiveFileName;
        if SameText(ExtractFileExt(Name), '.bib') then
          Exit(Name);
      end;
    except
      Result := '';
    end;
  finally
    UnZip.Free;
  end;
end;

function IsZippedProject(const ZipFile: string): Boolean;
begin
  Result := ZippedProjectName(ZipFile) <> '';
end;

function ParseCreated(const Value: string; out Created: TDateTime): Boolean;
var
  Year, Month, Day, Hour, Minute, Second: Integer;
begin
  Result := (Length(Value) >= 16) and
    TryStrToInt(Copy(Value, 1, 4), Year) and TryStrToInt(Copy(Value, 6, 2), Month) and
    TryStrToInt(Copy(Value, 9, 2), Day) and TryStrToInt(Copy(Value, 12, 2), Hour) and
    TryStrToInt(Copy(Value, 15, 2), Minute);
  if not Result then
    Exit;
  if not TryStrToInt(Copy(Value, 18, 2), Second) then
    Second := 0;
  Result := TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0, Created);
end;

function ReadBackupInfo(const ZipFile: string; out Info: TProjectBackupInfo): Boolean;
var
  UnZip: TUnZipper;
  TempFolder, InfoFile: string;
  Ini: TMemIniFile;
  Files: TStringList;
begin
  Info := Default(TProjectBackupInfo);
  Info.FileName := ZipFile;
  Info.Created := FileDateToDateTime(FileAge(ZipFile));
  Info.Automatic := Pos(AUTO_TAG, ExtractFileName(ZipFile)) > 0;
  Info.Size := FileSizeOf(ZipFile);
  Info.VerseCount := -1;
  Info.AssociatedVerses := -1;
  Result := IsZippedProject(ZipFile);
  if not Result then
    Exit;
  TempFolder := '';
  UnZip := TUnZipper.Create;
  Files := TStringList.Create;
  try
    try
      TempFolder := NewTempFolder(GetTempDir(False));
      UnZip.FileName := ZipFile;
      UnZip.OutputPath := TempFolder;
      Files.Add(INFO_ENTRY);
      UnZip.UnZipFiles(Files);
      InfoFile := IncludeTrailingPathDelimiter(TempFolder) + INFO_ENTRY;
      if FileExists(InfoFile) then
      begin
        Ini := TMemIniFile.Create(InfoFile);
        try
          ParseCreated(Ini.ReadString('backup', 'created', ''), Info.Created);
          Info.Automatic := Ini.ReadString('backup', 'kind', '') = 'automatic';
          Info.Note := Ini.ReadString('backup', 'note', '');
          Info.ProjectName := Ini.ReadString('backup', 'project.name', '');
          Info.VerseCount := Ini.ReadInteger('backup', 'verses', -1);
          Info.AssociatedVerses := Ini.ReadInteger('backup', 'verses.associated', -1);
        finally
          Ini.Free;
        end;
      end;
    except
      { a zip without backup.ini is still a valid project zip }
    end;
  finally
    Files.Free;
    UnZip.Free;
    RemoveFolder(TempFolder);
  end;
end;

procedure FindBackupFiles(const ProjectFile, Folder: string; AutomaticOnly: Boolean;
  Files: TStringList);
var
  Search: TSearchRec;
  Base, Name: string;
begin
  Files.Clear;
  if (Folder = '') or not DirectoryExists(Folder) then
    Exit;
  Base := ProjectBaseName(ProjectFile);
  if FindFirst(IncludeTrailingPathDelimiter(Folder) + '*' + BACKUP_EXTENSION,
    faAnyFile, Search) = 0 then
  try
    repeat
      Name := Search.Name;
      if (Search.Attr and faDirectory) <> 0 then
        Continue;
      if SameText(Copy(Name, 1, Length(Base + AUTO_TAG)), Base + AUTO_TAG) or
         (not AutomaticOnly and
          SameText(Copy(Name, 1, Length(Base + MANUAL_TAG)), Base + MANUAL_TAG)) then
        Files.Add(IncludeTrailingPathDelimiter(Folder) + Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
end;

function ListProjectBackups(const ProjectFile, Folder: string): TProjectBackupInfoArray;
var
  Files: TStringList;
  Index, J, Count: Integer;
  Info, Swap: TProjectBackupInfo;
begin
  Result := nil;
  Files := TStringList.Create;
  try
    FindBackupFiles(ProjectFile, Folder, False, Files);
    SetLength(Result, Files.Count);
    Count := 0;
    for Index := 0 to Files.Count - 1 do
      if ReadBackupInfo(Files[Index], Info) then
      begin
        Result[Count] := Info;
        Inc(Count);
      end;
    SetLength(Result, Count);
    { newest first }
    for Index := 1 to Count - 1 do
    begin
      Swap := Result[Index];
      J := Index - 1;
      while (J >= 0) and (Result[J].Created < Swap.Created) do
      begin
        Result[J + 1] := Result[J];
        Dec(J);
      end;
      Result[J + 1] := Swap;
    end;
  finally
    Files.Free;
  end;
end;

procedure PruneAutomaticBackups(const ProjectFile, Folder: string; Keep: Integer);
var
  Files: TStringList;
  Dates: array of TDateTime;
  Info: TProjectBackupInfo;
  Index, J, Oldest: Integer;
begin
  Files := TStringList.Create;
  try
    FindBackupFiles(ProjectFile, Folder, True, Files);
    SetLength(Dates, Files.Count);
    for Index := 0 to Files.Count - 1 do
    begin
      ReadBackupInfo(Files[Index], Info);
      Dates[Index] := Info.Created;
    end;
    while Files.Count > Keep do
    begin
      Oldest := 0;
      for J := 1 to Files.Count - 1 do
        if Dates[J] < Dates[Oldest] then
          Oldest := J;
      SysUtils.DeleteFile(Files[Oldest]);
      Files.Delete(Oldest);
      for J := Oldest to High(Dates) - 1 do
        Dates[J] := Dates[J + 1];
      SetLength(Dates, Length(Dates) - 1);
    end;
  finally
    Files.Free;
  end;
end;

procedure ExtractZippedProject(const ZipFile, DestinationFile: string);
var
  EntryName, TempFolder, Extracted: string;
  UnZip: TUnZipper;
  Files: TStringList;
begin
  EntryName := ZippedProjectName(ZipFile);
  if EntryName = '' then
    raise Exception.CreateFmt(SBackupNotAProject, [ZipFile]);
  if FileExists(DestinationFile) then
    raise Exception.CreateFmt(SBackupDestinationExists, [DestinationFile]);
  TempFolder := NewTempFolder(ExtractFileDir(ExpandFileName(DestinationFile)));
  UnZip := TUnZipper.Create;
  Files := TStringList.Create;
  try
    UnZip.FileName := ZipFile;
    UnZip.OutputPath := TempFolder;
    Files.Add(EntryName);
    UnZip.UnZipFiles(Files);
    Extracted := IncludeTrailingPathDelimiter(TempFolder) +
      StringReplace(EntryName, '/', PathDelim, [rfReplaceAll]);
    if not FileExists(Extracted) then
      raise Exception.CreateFmt(SBackupExtractFailed, [ZipFile]);
    ValidateSQLiteDatabase(Extracted);
    if not RenameFile(Extracted, DestinationFile) then
    begin
      { another drive: copy instead }
      CopySQLiteDatabase(Extracted, DestinationFile, False);
      ValidateSQLiteDatabase(DestinationFile);
    end;
  finally
    Files.Free;
    UnZip.Free;
    RemoveFolder(TempFolder);
  end;
end;

end.
