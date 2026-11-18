unit bible_archive;

{ Compressed files (zip, tar, tar.gz, tar.bz2, tar.xz, gz, 7z, rar ...) that
  hold Bible books, whole Bibles or dictionaries.

  Zip files are unpacked by iBiblia itself. The other kinds are unpacked with
  a tool of the computer (tar, 7-Zip, unrar), started without a shell and with
  a list of arguments. Unpacking is done into a new temporary folder; names
  that leave that folder are skipped, links are removed and a size limit
  protects against "zip bombs". No user-interface code here. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TArchiveFileFilter = function(const FileName: string): Boolean;

const
  MAX_ARCHIVE_BYTES = Int64(2) * 1024 * 1024 * 1024;

{ True when the file name looks like a compressed file we can open. }
function IsArchiveFile(const FileName: string): Boolean;

{ Masks for open dialogs: "*.zip;*.tar;..." }
function ArchiveFilterMask: string;

{ Unpacks ArchiveFile into DestFolder (created if missing). }
function ExtractArchive(const ArchiveFile, DestFolder: string;
  out ErrorMessage: string): Boolean;

{ All files below Folder (subfolders included) accepted by Accept, sorted by
  path. Hidden files and folders such as __MACOSX are skipped. }
procedure FindArchiveFiles(const Folder: string; Accept: TArchiveFileFilter;
  Files: TStrings);

{ A new empty folder in the temporary directory / removal of a folder tree. }
function NewArchiveTempFolder(const Prefix: string): string;
procedure RemoveArchiveFolder(const Folder: string);

{ Unpacks the archive and copies the first file accepted by Accept (in path
  order) into Folder, keeping its name; returns its full name. Used for
  dictionaries, which must stay on disk after the dialog closes. }
function ExtractOneFileTo(const ArchiveFile, Folder: string;
  Accept: TArchiveFileFilter; out ResultFile, ErrorMessage: string): Boolean;

{ For dictionary pickers: when Picked is a compressed file, unpacks the
  theWord dictionary (*.dct.twm, else *.twm) inside it into the iBiblia
  "dictionaries" folder of the user and returns that path; any other file is
  returned unchanged. False (with ErrorMessage) when nothing could be used. }
function ResolvePickedDictionary(const Picked: string; out DictionaryFile,
  ErrorMessage: string): Boolean;

{ Open-dialog filter for dictionaries: theWord dictionaries, compressed
  files, all files. }
function DictionaryDialogFilter: string;

resourcestring
  SArchiveNoTool = 'This kind of compressed file needs the program "%s", ' +
    'which was not found on this computer. Unpack the file yourself and ' +
    'choose the files inside.';
  SArchiveFailed = 'The compressed file could not be unpacked.';
  SArchiveTooBig = 'The compressed file is too large when unpacked.';
  SArchiveEmpty = 'The compressed file contains nothing that can be used here.';

implementation

uses
  Process, Zipper, zstream, FileUtil, LazFileUtils, sword_module;

const
  ARCHIVE_SUFFIXES: array[0..13] of string = (
    '.zip', '.tar', '.tar.gz', '.tgz', '.tar.bz2', '.tbz2', '.tbz',
    '.tar.xz', '.txz', '.gz', '.7z', '.rar', '.tar.zst', '.bz2');

function EndsWithText(const S, Suffix: string): Boolean;
begin
  Result := (Length(S) >= Length(Suffix)) and
    (LowerCase(Copy(S, Length(S) - Length(Suffix) + 1, MaxInt)) = Suffix);
end;

function IsArchiveFile(const FileName: string): Boolean;
var
  I: Integer;
begin
  for I := Low(ARCHIVE_SUFFIXES) to High(ARCHIVE_SUFFIXES) do
    if (Length(FileName) > Length(ARCHIVE_SUFFIXES[I])) and
       EndsWithText(FileName, ARCHIVE_SUFFIXES[I]) then
      Exit(True);
  Result := False;
end;

function ArchiveFilterMask: string;
begin
  Result := '*.zip;*.tar;*.tar.gz;*.tgz;*.tar.bz2;*.tbz2;*.tar.xz;*.txz;' +
    '*.tar.zst;*.gz;*.bz2;*.7z;*.rar';
end;

function NewArchiveTempFolder(const Prefix: string): string;
var
  Base: string;
  N: Integer;
begin
  Base := IncludeTrailingPathDelimiter(GetTempDir(False));
  Randomize;
  for N := 1 to 1000 do
  begin
    Result := Base + Prefix + IntToStr(Random(100000000));
    if not DirectoryExists(Result) and CreateDir(Result) then
      Exit;
  end;
  Result := '';
end;

procedure RemoveArchiveFolder(const Folder: string);
begin
  if (Folder <> '') and DirectoryExists(Folder) then
    DeleteDirectory(Folder, False);
end;

function SafeEntryName(const Name: string): Boolean;
var
  Parts: TStringArray;
  I: Integer;
  N: string;
begin
  N := StringReplace(Name, '\', '/', [rfReplaceAll]);
  if (N = '') or (N[1] = '/') or ((Length(N) > 1) and (N[2] = ':')) then
    Exit(False);
  Parts := N.Split('/');
  for I := 0 to High(Parts) do
    if Parts[I] = '..' then
      Exit(False);
  Result := True;
end;

{ ---------- size and link checks after unpacking ---------- }

function CheckTree(const Folder: string; var Total: Int64): Boolean;
var
  SR: TSearchRec;
  Path: string;
begin
  Result := True;
  Path := IncludeTrailingPathDelimiter(Folder);
  if FindFirst(Path + '*', faAnyFile, SR) <> 0 then
    Exit;
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      if (SR.Attr and faSymLink) <> 0 then
      begin
        DeleteFile(Path + SR.Name);
        Continue;
      end;
      if (SR.Attr and faDirectory) <> 0 then
      begin
        if not CheckTree(Path + SR.Name, Total) then
          Exit(False);
      end
      else
      begin
        Inc(Total, SR.Size);
        if Total > MAX_ARCHIVE_BYTES then
          Exit(False);
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

{ ---------- zip ---------- }

function ExtractZip(const ArchiveFile, Dest: string; out Err: string): Boolean;
var
  UnZ: TUnZipper;
  I: Integer;
  Names: TStringList;
  Total: Int64;
begin
  Result := False;
  Err := '';
  UnZ := TUnZipper.Create;
  Names := TStringList.Create;
  try
    try
      UnZ.FileName := ArchiveFile;
      UnZ.OutputPath := Dest;
      UnZ.Examine;
      Total := 0;
      for I := 0 to UnZ.Entries.Count - 1 do
      begin
        Total := Total + UnZ.Entries[I].Size;
        if SafeEntryName(UnZ.Entries[I].ArchiveFileName) then
          Names.Add(UnZ.Entries[I].ArchiveFileName);
      end;
      if Total > MAX_ARCHIVE_BYTES then
      begin
        Err := SArchiveTooBig;
        Exit;
      end;
      if Names.Count = 0 then
      begin
        Err := SArchiveEmpty;
        Exit;
      end;
      UnZ.UnZipFiles(Names);
      Result := True;
    except
      on E: Exception do
      begin
        Err := SArchiveFailed + ' ' + E.Message;
        Result := False;
      end;
    end;
  finally
    Names.Free;
    UnZ.Free;
  end;
end;

{ ---------- single gzip file ---------- }

function ExtractGzip(const ArchiveFile, Dest: string; out Err: string): Boolean;
var
  Src: TGZFileStream;
  Dst: TFileStream;
  Name: string;
  Buf: array[0..65535] of Byte;
  N: Integer;
  Total: Int64;
begin
  Result := False;
  Err := '';
  Name := ExtractFileName(ArchiveFile);
  Name := Copy(Name, 1, Length(Name) - 3);
  if (Name = '') or not SafeEntryName(Name) then
    Name := 'file';
  try
    Src := TGZFileStream.Create(ArchiveFile, gzopenread);
    try
      Dst := TFileStream.Create(IncludeTrailingPathDelimiter(Dest) + Name, fmCreate);
      try
        Total := 0;
        repeat
          N := Src.Read(Buf, SizeOf(Buf));
          if N > 0 then
          begin
            Dst.WriteBuffer(Buf, N);
            Inc(Total, N);
            if Total > MAX_ARCHIVE_BYTES then
            begin
              Err := SArchiveTooBig;
              Exit;
            end;
          end;
        until N <= 0;
        Result := True;
      finally
        Dst.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: Exception do
      Err := SArchiveFailed + ' ' + E.Message;
  end;
end;

{ ---------- external tools ---------- }

function RunTool(const Exe: string; const Args: array of string): Integer;
var
  P: TProcess;
  I, N: Integer;
  Chunk: array[0..4095] of Byte;
begin
  Result := -1;
  P := TProcess.Create(nil);
  try
    P.Executable := Exe;
    for I := 0 to High(Args) do
      P.Parameters.Add(Args[I]);
    P.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    try
      P.Execute;
      while P.Running or (P.Output.NumBytesAvailable > 0) do
      begin
        N := P.Output.Read(Chunk, SizeOf(Chunk));
        if N <= 0 then
          Sleep(20);
      end;
      Result := P.ExitCode;
    except
      Result := -1;
    end;
  finally
    P.Free;
  end;
end;

function FindTool(const Names: array of string): string;
var
  I: Integer;
  Dir: string;
begin
  for I := 0 to High(Names) do
  begin
    Result := FileSearch(Names[I] + GetExeExt, GetEnvironmentVariable('PATH'));
    if Result <> '' then
      Exit;
  end;
  {$IFDEF WINDOWS}
  for I := 0 to High(Names) do
    for Dir in ['C:\Program Files\7-Zip\', 'C:\Program Files (x86)\7-Zip\'] do
      if FileExists(Dir + Names[I] + '.exe') then
        Exit(Dir + Names[I] + '.exe');
  {$ENDIF}
  Result := '';
end;

function ExtractWithTool(const ArchiveFile, Dest: string; out Err: string): Boolean;
var
  Tool: string;
  IsTar, IsRar: Boolean;
begin
  Err := '';
  IsTar := (Pos('.tar', LowerCase(ArchiveFile)) > 0) or
    EndsWithText(ArchiveFile, '.tgz') or EndsWithText(ArchiveFile, '.tbz2') or
    EndsWithText(ArchiveFile, '.tbz') or EndsWithText(ArchiveFile, '.txz');
  IsRar := EndsWithText(ArchiveFile, '.rar');
  if IsTar then
  begin
    Tool := FindTool(['tar', 'bsdtar']);
    if Tool <> '' then
      Exit(RunTool(Tool, ['-x', '--no-same-owner', '-f', ArchiveFile, '-C', Dest]) = 0);
  end;
  if IsRar then
  begin
    Tool := FindTool(['unrar', 'rar']);
    if Tool <> '' then
      Exit(RunTool(Tool, ['x', '-y', ArchiveFile, IncludeTrailingPathDelimiter(Dest)]) = 0);
  end;
  Tool := FindTool(['7zz', '7z', '7za', '7zr']);
  if Tool <> '' then
    Exit(RunTool(Tool, ['x', '-y', '-o' + Dest, ArchiveFile]) = 0);
  if IsTar then
    Err := Format(SArchiveNoTool, ['tar'])
  else if IsRar then
    Err := Format(SArchiveNoTool, ['unrar / 7-Zip'])
  else
    Err := Format(SArchiveNoTool, ['7-Zip']);
  Result := False;
end;

function ExtractArchive(const ArchiveFile, DestFolder: string;
  out ErrorMessage: string): Boolean;
var
  Total: Int64;
  Tars: TStringList;
  I: Integer;
  Ignored: string;
begin
  ErrorMessage := '';
  if not FileExists(ArchiveFile) then
  begin
    ErrorMessage := SArchiveFailed;
    Exit(False);
  end;
  ForceDirectories(DestFolder);
  if EndsWithText(ArchiveFile, '.zip') then
    Result := ExtractZip(ArchiveFile, DestFolder, ErrorMessage)
  else if EndsWithText(ArchiveFile, '.gz') and (Pos('.tar', LowerCase(ArchiveFile)) = 0) then
    Result := ExtractGzip(ArchiveFile, DestFolder, ErrorMessage)
  else
    Result := ExtractWithTool(ArchiveFile, DestFolder, ErrorMessage);
  if not Result then
  begin
    if ErrorMessage = '' then
      ErrorMessage := SArchiveFailed;
    Exit;
  end;

  { 7-Zip unpacks "x.tar.gz" to "x.tar": open that one too }
  Tars := TStringList.Create;
  try
    FindAllFiles(Tars, DestFolder, '*.tar', False);
    for I := 0 to Tars.Count - 1 do
      if ExtractWithTool(Tars[I], DestFolder, Ignored) then
        DeleteFile(Tars[I]);
  finally
    Tars.Free;
  end;

  Total := 0;
  if not CheckTree(DestFolder, Total) then
  begin
    ErrorMessage := SArchiveTooBig;
    Result := False;
  end;
end;

procedure FindArchiveFiles(const Folder: string; Accept: TArchiveFileFilter;
  Files: TStrings);
var
  All, Hits: TStringList;
  I: Integer;
  Rel: string;
begin
  Files.Clear;
  All := TStringList.Create;
  Hits := TStringList.Create;
  try
    FindAllFiles(All, Folder, '', True);
    for I := 0 to All.Count - 1 do
    begin
      Rel := StringReplace(Copy(All[I], Length(Folder) + 1, MaxInt), '\', '/',
        [rfReplaceAll]);
      if (Pos('__MACOSX', Rel) > 0) or (Pos('/.', '/' + Rel) > 0) then
        Continue;
      if Accept(All[I]) then
        Hits.Add(All[I]);
    end;
    Hits.Sort;
    Files.AddStrings(Hits);
  finally
    Hits.Free;
    All.Free;
  end;
end;

function ExtractOneFileTo(const ArchiveFile, Folder: string;
  Accept: TArchiveFileFilter; out ResultFile, ErrorMessage: string): Boolean;
var
  Temp: string;
  Found: TStringList;
  Target: string;
begin
  Result := False;
  ResultFile := '';
  Temp := NewArchiveTempFolder('ibiblia_arc_');
  if Temp = '' then
  begin
    ErrorMessage := SArchiveFailed;
    Exit;
  end;
  Found := TStringList.Create;
  try
    if not ExtractArchive(ArchiveFile, Temp, ErrorMessage) then
      Exit;
    FindArchiveFiles(Temp, Accept, Found);
    if Found.Count = 0 then
    begin
      ErrorMessage := SArchiveEmpty;
      Exit;
    end;
    ForceDirectories(Folder);
    Target := IncludeTrailingPathDelimiter(Folder) + ExtractFileName(Found[0]);
    if FileExists(Target) then
      DeleteFile(Target);
    if not CopyFile(Found[0], Target) then
    begin
      ErrorMessage := SArchiveFailed;
      Exit;
    end;
    ResultFile := Target;
    Result := True;
  finally
    Found.Free;
    RemoveArchiveFolder(Temp);
  end;
end;

function DictionaryDialogFilter: string;
begin
  Result := 'theWord and SWORD dictionaries and compressed files|*.dct.twm;*.twm;*.conf;' +
    ArchiveFilterMask + '|theWord dictionaries (*.dct.twm;*.twm)|*.dct.twm;*.twm|' +
    'SWORD dictionaries (*.conf)|*.conf|' +
    'Compressed files|' + ArchiveFilterMask + '|All files (*.*)|*.*';
end;

function AcceptDictionaryFile(const FileName: string): Boolean;
begin
  Result := EndsWithText(FileName, '.dct.twm');
end;

function AcceptAnyTwm(const FileName: string): Boolean;
begin
  Result := EndsWithText(FileName, '.twm');
end;

function ResolvePickedDictionary(const Picked: string; out DictionaryFile,
  ErrorMessage: string): Boolean;
var
  Folder, Temp, Target, Err: string;
  Found: TStringList;
begin
  ErrorMessage := '';
  DictionaryFile := Picked;
  if SameText(ExtractFileExt(Picked), '.conf') then
  begin
    // a SWORD dictionary: converted to a theWord dictionary once
    Target := SwordDictionaryTarget(Picked);
    Result := ConvertSwordDictionary(Picked, Target, ErrorMessage);
    if Result then
      DictionaryFile := Target
    else
      DictionaryFile := '';
    Exit;
  end;
  if not IsArchiveFile(Picked) then
    Exit(True);
  Folder := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'dictionaries';
  Result := ExtractOneFileTo(Picked, Folder, @AcceptDictionaryFile,
    DictionaryFile, ErrorMessage);
  if not Result then
    Result := ExtractOneFileTo(Picked, Folder, @AcceptAnyTwm,
      DictionaryFile, ErrorMessage);
  if not Result then
  begin
    // no theWord dictionary inside: a SWORD dictionary module, if there is one
    Temp := NewArchiveTempFolder('ibiblia_sworddict_');
    if Temp <> '' then
    begin
      Found := TStringList.Create;
      try
        if ExtractArchive(Picked, Temp, Err) then
        begin
          FindArchiveFiles(Temp, @IsSwordDictionaryConf, Found);
          if Found.Count > 0 then
          begin
            Target := SwordDictionaryTarget(Found[0]);
            if ConvertSwordDictionary(Found[0], Target, Err) then
            begin
              DictionaryFile := Target;
              ErrorMessage := '';
              Result := True;
            end
            else
              ErrorMessage := Err;
          end;
        end;
      finally
        Found.Free;
        RemoveArchiveFolder(Temp);
      end;
    end;
  end;
  if not Result then
    DictionaryFile := '';
end;

end.
