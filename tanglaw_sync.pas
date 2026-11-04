unit tanglaw_sync;

{ Verse synchronization with Timotheos (both Tauri 0.6.5+ and Electron builds,
  formerly Tanglaw Bible; Windows). Both names and both windowing frameworks
  (Tauri/WebView2 TaoWindowClass and Electron Chrome_WidgetWin) are recognised.
  The app exchanges the current verse through two small JSON files in its
  user-data folder (%APPDATA%\org.timotheos.bible for Tauri 2, or
  %APPDATA%\Timotheos / %APPDATA%\Tanglaw Bible / portable Timotheos Data):
    ibiblia-sync-in.json   written here, read by Timotheos ("go to verse")
    ibiblia-sync-out.json  written by Timotheos, read here (its current verse)
  When that file is missing, the window title
  ("<Book> <c>:<v> - Timotheos" / "... - Tanglaw Bible") and settings.json
  are used. }

{$mode objfpc}{$H+}
{$WARN 4055 OFF}
{$WARN 4056 OFF}
{$WARN 4082 OFF}

interface

uses
  Classes, SysUtils;

{ HWND of the Timotheos (or Tanglaw) main window, or 0. }
function IsTanglawRunning: PtrUInt;
{ Asks Timotheos to show book/chapter/verse (iBiblia numbering; apocrypha
  books are matched by name). }
procedure SyncTanglawVerse(BookID, Chapter, Verse: Integer);
{ Timotheos' current verse as 'b,c,v' in iBiblia numbering, or ''. }
function GetTanglawCurrentRef: string;

implementation

{$IFDEF WINDOWS}
uses
  Windows, PCRE, twAutomate, bible_book_names, apocrypha_books;

const
  MOVEFILE_REPLACE_EXISTING = $00000001;
  MOVEFILE_WRITE_THROUGH    = $00000008;

var
  OutSeq: Integer = 0;
  LastWrittenRef: string = '';
  LastWrittenProjectRef: string = '';
  LastWriteTick: QWord = 0;
  LastWriteUnixMs: Int64 = 0;

function QueryFullProcessImageNameA(hProcess: HANDLE; dwFlags: DWORD;
  lpExeName: LPSTR; var lpdwSize: DWORD): BOOL; stdcall;
  external 'KERNEL32' name 'QueryFullProcessImageNameA';

function MoveFileExA(lpExistingFileName, lpNewFileName: LPCSTR;
  dwFlags: DWORD): BOOL; stdcall;
  external 'KERNEL32' name 'MoveFileExA';

function CurrentUnixMs: Int64;
var
  Ft: TFileTime;
  Ticks100ns: QWord;
begin
  FillChar(Ft, SizeOf(Ft), 0);
  GetSystemTimeAsFileTime(Ft);
  Ticks100ns := (QWord(Ft.dwHighDateTime) shl 32) or QWord(Ft.dwLowDateTime);
  if Ticks100ns > QWord(116444736000000000) then
    Result := Int64((Ticks100ns - QWord(116444736000000000)) div 10000)
  else
    Result := Int64(GetTickCount64);
end;

function EscapeJsonString(const S: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    Ch := S[I];
    case Ch of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8:  Result := Result + '\b';
      #9:  Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if Ord(Ch) >= 32 then
        Result := Result + Ch;
    end;
  end;
end;

function ProcessExeName(Pid: DWORD): string;
var
  Proc: THandle;
  Buffer: array[0..MAX_PATH] of Char;
  Size: DWORD;
begin
  Result := '';
  if Pid = 0 then
    Exit;
  Proc := OpenProcess($1000 {PROCESS_QUERY_LIMITED_INFORMATION}, False, Pid);
  if Proc = 0 then
    Proc := OpenProcess(PROCESS_QUERY_INFORMATION, False, Pid);
  if Proc = 0 then
    Exit;
  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Size := Length(Buffer);
    if QueryFullProcessImageNameA(Proc, 0, Buffer, Size) then
      Result := StrPas(Buffer);
  finally
    CloseHandle(Proc);
  end;
end;

function IsBrowserOrNonTanglawExe(const ExeBaseLower: string): Boolean;
begin
  Result := (ExeBaseLower = 'chrome.exe') or
            (ExeBaseLower = 'msedge.exe') or
            (ExeBaseLower = 'msedgewebview2.exe') or
            (ExeBaseLower = 'brave.exe') or
            (ExeBaseLower = 'firefox.exe') or
            (ExeBaseLower = 'opera.exe') or
            (ExeBaseLower = 'vivaldi.exe') or
            (ExeBaseLower = 'explorer.exe') or
            (ExeBaseLower = 'code.exe') or
            (ExeBaseLower = 'lazarus.exe') or
            (ExeBaseLower = 'ibiblia.exe');
end;

type
  TTanglawFindCtx = record
    PrimaryWnd: THandle;
    FallbackWnd: THandle;
  end;
  PTanglawFindCtx = ^TTanglawFindCtx;

function FindTanglawWindowEnumProc(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
var
  Ctx: PTanglawFindCtx;
  ClassBuf, TitleBuf: array[0..255] of Char;
  ClassStr, TitleStr, ExeStr: string;
  Pid: DWORD;
  IsTanglawTitle, IsTanglawClass, IsTanglawExe: Boolean;
begin
  Result := True;
  Ctx := PTanglawFindCtx(Pointer(PtrUInt(Param)));
  if Ctx = nil then
    Exit(False);

  if not (IsWindowVisible(Wnd) or IsIconic(Wnd)) then
    Exit;
  Pid := 0;
  GetWindowThreadProcessId(Wnd, Pid);
  if (Pid = 0) or (Pid = GetCurrentProcessId) then
    Exit;

  FillChar(ClassBuf, SizeOf(ClassBuf), 0);
  FillChar(TitleBuf, SizeOf(TitleBuf), 0);
  GetClassName(Wnd, ClassBuf, Length(ClassBuf));
  GetWindowText(Wnd, TitleBuf, Length(TitleBuf));
  ClassStr := LowerCase(Trim(StrPas(ClassBuf)));
  TitleStr := LowerCase(Trim(StrPas(TitleBuf)));

  if (Pos('user guide', TitleStr) > 0) or
     (Pos('devtools', TitleStr) > 0) or
     (ClassStr = 'ime') or
     (ClassStr = 'msctfime ui') or
     (ClassStr = 'tooltips_class32') or
     (ClassStr = 'cabinetwclass') then
    Exit;

  ExeStr := LowerCase(ExtractFileName(ProcessExeName(Pid)));
  if (ExeStr <> '') and IsBrowserOrNonTanglawExe(ExeStr) then
    Exit;

  IsTanglawExe := (Pos('timotheos', ExeStr) > 0) or (Pos('tanglaw', ExeStr) > 0);
  IsTanglawTitle := (Pos(' - timotheos', TitleStr) > 0) or
                    (Pos(' – timotheos', TitleStr) > 0) or
                    (TitleStr = 'timotheos') or
                    (Pos('timotheos', TitleStr) > 0) or
                    (Pos('tanglaw bible', TitleStr) > 0);
  IsTanglawClass := (Pos('taowindowclass', ClassStr) > 0) or
                    (Pos('chrome_widgetwin', ClassStr) = 1) or
                    (Pos('wry', ClassStr) > 0) or
                    (Pos('timotheos', ClassStr) > 0) or
                    (Pos('tanglaw', ClassStr) > 0) or
                    (Pos('window', ClassStr) > 0);

  if IsTanglawExe and IsTanglawTitle then
  begin
    Ctx^.PrimaryWnd := Wnd;
    Exit(False);
  end;

  if IsTanglawTitle and IsTanglawClass and (ExeStr = '') then
  begin
    Ctx^.PrimaryWnd := Wnd;
    Exit(False);
  end;

  if IsTanglawTitle and IsTanglawClass and (Pos('electron', ExeStr) > 0) then
  begin
    Ctx^.PrimaryWnd := Wnd;
    Exit(False);
  end;

  if IsTanglawExe and IsTanglawClass and (TitleStr <> '') and (Ctx^.FallbackWnd = 0) then
    Ctx^.FallbackWnd := Wnd;
end;

function IsTanglawRunning: PtrUInt;
var
  Ctx: TTanglawFindCtx;
begin
  Ctx.PrimaryWnd := 0;
  Ctx.FallbackWnd := 0;
  EnumWindows(@FindTanglawWindowEnumProc, PtrInt(@Ctx));
  if IsWindow(Ctx.PrimaryWnd) then
    Result := Ctx.PrimaryWnd
  else if IsWindow(Ctx.FallbackWnd) then
    Result := Ctx.FallbackWnd
  else
    Result := 0;
end;

{ Timotheos/Tanglaw user-data folders: portable folder first, then Tauri 2
  (%APPDATA%\org.timotheos.bible) and Electron (%APPDATA%\Timotheos,
  %APPDATA%\Tanglaw Bible). }
procedure GetTanglawUserDataDirs(Dirs: TStrings);

  procedure Add(const Dir: string);
  var
    Clean: string;
  begin
    Clean := ExcludeTrailingPathDelimiter(Trim(Dir));
    if (Clean <> '') and (Dirs.IndexOf(Clean) < 0) then
      Dirs.Add(Clean);
  end;

var
  Wnd: THandle;
  Pid: DWORD;
  ExeDir, AppData, LocalAppData, EnvName: string;
begin
  Dirs.Clear;
  EnvName := SysUtils.GetEnvironmentVariable('TIMOTHEOS_PORTABLE_DIR');
  if (EnvName <> '') and DirectoryExists(EnvName) then
    Add(EnvName);
  EnvName := SysUtils.GetEnvironmentVariable('TANGLAW_PORTABLE_DIR');
  if (EnvName <> '') and DirectoryExists(EnvName) then
    Add(EnvName);

  Wnd := IsTanglawRunning;
  if Wnd <> 0 then
  begin
    Pid := 0;
    GetWindowThreadProcessId(Wnd, Pid);
    ExeDir := ExtractFilePath(ProcessExeName(Pid));
    if ExeDir <> '' then
    begin
      if FileExists(ExeDir + 'portable.json') or DirectoryExists(ExeDir + 'Timotheos Data') then
        Add(ExeDir + 'Timotheos Data');
      if DirectoryExists(ExeDir + 'Tanglaw Data') then
        Add(ExeDir + 'Tanglaw Data');
    end;
  end;

  AppData := SysUtils.GetEnvironmentVariable('APPDATA');
  LocalAppData := SysUtils.GetEnvironmentVariable('LOCALAPPDATA');

  if AppData <> '' then
  begin
    // Tauri 2 bundle identifier in timotheos-0.6.5+: org.timotheos.bible
    Add(IncludeTrailingPathDelimiter(AppData) + 'org.timotheos.bible');
    if (LocalAppData <> '') and
       DirectoryExists(IncludeTrailingPathDelimiter(LocalAppData) + 'org.timotheos.bible') then
      Add(IncludeTrailingPathDelimiter(LocalAppData) + 'org.timotheos.bible');

    // Electron / standalone Timotheos & Tanglaw Bible folders
    Add(IncludeTrailingPathDelimiter(AppData) + 'Timotheos');
    if DirectoryExists(IncludeTrailingPathDelimiter(AppData) + 'timotheos') then
      Add(IncludeTrailingPathDelimiter(AppData) + 'timotheos');
    if DirectoryExists(IncludeTrailingPathDelimiter(AppData) + 'Timotheos Bible') then
      Add(IncludeTrailingPathDelimiter(AppData) + 'Timotheos Bible');
    if DirectoryExists(IncludeTrailingPathDelimiter(AppData) + 'timotheos-bible') then
      Add(IncludeTrailingPathDelimiter(AppData) + 'timotheos-bible');
    if DirectoryExists(IncludeTrailingPathDelimiter(AppData) + 'Tanglaw Bible') then
      Add(IncludeTrailingPathDelimiter(AppData) + 'Tanglaw Bible');
    if DirectoryExists(IncludeTrailingPathDelimiter(AppData) + 'tanglaw-bible') then
      Add(IncludeTrailingPathDelimiter(AppData) + 'tanglaw-bible');
  end;
end;

{ Writes through a temporary file with atomic replacement so Timotheos never
  reads half a file or sees the file disappear between delete and rename. }
procedure WriteFileShared(const FileName, Content: string);
var
  Stream: TFileStream;
  TmpFile: string;
begin
  try
    ForceDirectories(ExtractFilePath(FileName));
    TmpFile := FileName + '.tmp';
    Stream := TFileStream.Create(TmpFile, fmCreate or fmShareDenyNone);
    try
      if Content <> '' then
        Stream.WriteBuffer(Content[1], Length(Content));
    finally
      Stream.Free;
    end;
    if not MoveFileExA(PChar(TmpFile), PChar(FileName),
       MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
    begin
      if FileExists(FileName) then
        SysUtils.DeleteFile(FileName);
      if not RenameFile(TmpFile, FileName) then
      begin
        Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
        try
          if Content <> '' then
            Stream.WriteBuffer(Content[1], Length(Content));
        finally
          Stream.Free;
        end;
        SysUtils.DeleteFile(TmpFile);
      end;
    end;
  except
    // Timotheos not installed or folder not writable: nothing to sync
  end;
end;

{ Reads file text with fmShareDenyNone so Timotheos is never blocked from
  concurrently renaming or writing ibiblia-sync-out.json or settings.json. }
function ReadFileText(const FileName: string): string;
var
  Stream: TFileStream;
  Len: Int64;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Len := Stream.Size;
      if (Len > 0) and (Len < 4 * 1024 * 1024) then
      begin
        SetLength(Result, Len);
        Stream.ReadBuffer(Result[1], Len);
      end;
    finally
      Stream.Free;
    end;
  except
    Result := '';
  end;
end;

{ Timotheos' number for an apocrypha book name (its canon.js numbering,
  which iBiblia's e-Sword numbering may not match), or 0. }
function TimotheosApocryphaID(const BookName: string): Integer;
var
  N: string;
begin
  N := LowerCase(Trim(BookName));
  Result := 0;
  if (Pos('tobit', N) > 0) or (Pos('tobias', N) > 0) then Result := 67
  else if Pos('judith', N) > 0 then Result := 68
  else if (Pos('addition', N) > 0) and (Pos('esther', N) > 0) then Result := 69
  else if (Pos('rest of esther', N) > 0) or (Pos('greek esther', N) > 0) or (Pos('esther (greek)', N) > 0) then Result := 69
  else if Pos('wisdom', N) > 0 then Result := 70
  else if (Pos('sirach', N) > 0) or (Pos('ecclesiasticus', N) > 0) then Result := 71
  else if (Pos('letter', N) > 0) and ((Pos('jeremiah', N) > 0) or (Pos('jeremy', N) > 0)) then Result := 73
  else if (Pos('epistle', N) > 0) and ((Pos('jeremiah', N) > 0) or (Pos('jeremy', N) > 0)) then Result := 73
  else if Pos('baruch', N) > 0 then Result := 72
  else if (Pos('jeremiah', N) > 0) or (Pos('jeremy', N) > 0) then Result := 73
  else if (Pos('azariah', N) > 0) or (Pos('three children', N) > 0) or (Pos('three holy children', N) > 0) then Result := 74
  else if Pos('susanna', N) > 0 then Result := 75
  else if Pos('bel', N) > 0 then Result := 76
  else if (Pos('1 macc', N) > 0) or (Pos('1macc', N) > 0) or (Pos('i macc', N) > 0) then Result := 77
  else if (Pos('2 macc', N) > 0) or (Pos('2macc', N) > 0) or (Pos('ii macc', N) > 0) then Result := 78
  else if (Pos('1 esdr', N) > 0) or (Pos('1esdr', N) > 0) or (Pos('3 esdr', N) > 0) then Result := 79
  else if Pos('manasse', N) > 0 then Result := 80
  else if Pos('151', N) > 0 then Result := 81
  else if (Pos('3 macc', N) > 0) or (Pos('3macc', N) > 0) or (Pos('iii macc', N) > 0) then Result := 82
  else if (Pos('2 esdr', N) > 0) or (Pos('2esdr', N) > 0) or (Pos('4 esdr', N) > 0) or (Pos('4 ezra', N) > 0) then Result := 83
  else if (Pos('4 macc', N) > 0) or (Pos('4macc', N) > 0) or (Pos('iv macc', N) > 0) then Result := 84
  else if Pos('ode', N) > 0 then Result := 85
  else if Pos('solomon', N) > 0 then Result := 86
  else if Pos('laodicean', N) > 0 then Result := 87
  else if Pos('esther', N) > 0 then Result := 69;
end;

function TimotheosDefaultBookName(TimotheosBookID: Integer): string;
begin
  if (TimotheosBookID >= 1) and (TimotheosBookID <= 66) then
    Exit(LocalizedBookNameByID(TimotheosBookID));
  case TimotheosBookID of
    67: Result := 'Tobit';
    68: Result := 'Judith';
    69: Result := 'Additions to Esther';
    70: Result := 'Wisdom';
    71: Result := 'Sirach';
    72: Result := 'Baruch';
    73: Result := 'Letter of Jeremiah';
    74: Result := 'Prayer of Azariah';
    75: Result := 'Susanna';
    76: Result := 'Bel and the Dragon';
    77: Result := '1 Maccabees';
    78: Result := '2 Maccabees';
    79: Result := '1 Esdras';
    80: Result := 'Prayer of Manasseh';
    81: Result := 'Psalm 151';
    82: Result := '3 Maccabees';
    83: Result := '2 Esdras';
    84: Result := '4 Maccabees';
    85: Result := 'Odes';
    86: Result := 'Psalms of Solomon';
    87: Result := 'Laodiceans';
  else
    Result := '';
  end;
end;

{ iBiblia book number -> Timotheos book number (same for 1-66). }
function ToTimotheosBook(BookID: Integer): Integer;
begin
  if BookID <= 66 then
    Result := BookID
  else
  begin
    Result := TimotheosApocryphaID(ApocryphaBookName(BookID));
    if Result = 0 then
      Result := BookID;
  end;
end;

{ Timotheos book number -> iBiblia book number & chapter of the open project, or 0. }
procedure FromTimotheosBookAndChapter(TimotheosBookID, TimotheosChapter: Integer;
  const TimotheosBookName: string; out IBibliaBookID, IBibliaChapter: Integer);
var
  Index, TargetApocID: Integer;
  ProjBookName: string;
begin
  IBibliaBookID := 0;
  IBibliaChapter := TimotheosChapter;

  if (TimotheosBookID >= 1) and (TimotheosBookID <= 66) then
  begin
    // Check if Daniel 13/14 or Psalm 151 is stored as a separate Apocrypha book in the open project
    if (TimotheosBookID = 27) and (TimotheosChapter = 13) then
    begin
      for Index := 0 to ApocryphaBookCount - 1 do
        if TimotheosApocryphaID(ApocryphaBookAt(Index).Name) = 75 then
        begin
          IBibliaBookID := ApocryphaBookAt(Index).BookID;
          IBibliaChapter := 1;
          Exit;
        end;
    end
    else if (TimotheosBookID = 27) and (TimotheosChapter = 14) then
    begin
      for Index := 0 to ApocryphaBookCount - 1 do
        if TimotheosApocryphaID(ApocryphaBookAt(Index).Name) = 76 then
        begin
          IBibliaBookID := ApocryphaBookAt(Index).BookID;
          IBibliaChapter := 1;
          Exit;
        end;
    end
    else if (TimotheosBookID = 19) and (TimotheosChapter = 151) then
    begin
      for Index := 0 to ApocryphaBookCount - 1 do
        if TimotheosApocryphaID(ApocryphaBookAt(Index).Name) = 81 then
        begin
          IBibliaBookID := ApocryphaBookAt(Index).BookID;
          IBibliaChapter := 1;
          Exit;
        end;
    end;
    IBibliaBookID := TimotheosBookID;
    Exit;
  end;

  TargetApocID := TimotheosBookID;
  if (TargetApocID < 67) and (TimotheosBookName <> '') then
    TargetApocID := TimotheosApocryphaID(TimotheosBookName);

  for Index := 0 to ApocryphaBookCount - 1 do
  begin
    ProjBookName := ApocryphaBookAt(Index).Name;
    if (TargetApocID >= 67) and (TimotheosApocryphaID(ProjBookName) = TargetApocID) then
    begin
      IBibliaBookID := ApocryphaBookAt(Index).BookID;
      Exit;
    end;
    if (TimotheosBookName <> '') and SameText(Trim(ProjBookName), Trim(TimotheosBookName)) then
    begin
      IBibliaBookID := ApocryphaBookAt(Index).BookID;
      Exit;
    end;
  end;

  // Fallback for Susanna / Bel / Psalm 151 when stored inside Daniel (27) or Psalms (19)
  if TargetApocID = 75 then
  begin
    IBibliaBookID := 27;
    IBibliaChapter := 13;
  end
  else if TargetApocID = 76 then
  begin
    IBibliaBookID := 27;
    IBibliaChapter := 14;
  end
  else if TargetApocID = 81 then
  begin
    IBibliaBookID := 19;
    IBibliaChapter := 151;
  end;
end;

procedure SyncTanglawVerse(BookID, Chapter, Verse: Integer);
var
  Dirs: TStringList;
  Index, TargetBookID: Integer;
  NowMs: Int64;
  BookName, Payload, OutPayload, Settings, DirPath, SyncOutPath, SettingsPath: string;
  RefRegex: IRegex;
  WroteAny, Running: Boolean;
begin
  if (BookID <= 0) or (Chapter <= 0) or (Verse <= 0) then
    Exit;

  if BookID >= FIRST_APOCRYPHA_BOOK then
    BookName := ApocryphaBookName(BookID)
  else
    BookName := TimotheosDefaultBookName(BookID);

  TargetBookID := ToTimotheosBook(BookID);
  if TargetBookID <= 0 then
    Exit;

  Inc(OutSeq);
  NowMs := CurrentUnixMs;
  LastWrittenRef := Format('%d,%d,%d', [TargetBookID, Chapter, Verse]);
  LastWrittenProjectRef := Format('%d,%d,%d', [BookID, Chapter, Verse]);
  LastWriteTick := GetTickCount64;
  LastWriteUnixMs := NowMs;

  Payload := Format(
    '{"seq":%d,"ts":%d,"cmd":"verse","b":%d,"c":%d,"v":%d,"rawB":%d,"rawC":%d,"rawV":%d,"bookName":"%s"}',
    [OutSeq, NowMs, TargetBookID, Chapter, Verse, BookID, Chapter, Verse, EscapeJsonString(BookName)]
  );
  OutPayload := Format(
    '{"b":%d,"c":%d,"v":%d,"bookName":"%s","ts":%d}',
    [TargetBookID, Chapter, Verse, EscapeJsonString(BookName), NowMs]
  );

  Running := IsTanglawRunning <> 0;
  Dirs := TStringList.Create;
  try
    GetTanglawUserDataDirs(Dirs);
    WroteAny := False;
    for Index := 0 to Dirs.Count - 1 do
    begin
      DirPath := Dirs[Index];
      if not DirectoryExists(DirPath) then
        Continue;

      WriteFileShared(IncludeTrailingPathDelimiter(DirPath) + 'ibiblia-sync-in.json', Payload);
      WroteAny := True;

      // Keep ibiblia-sync-out.json aligned with the verse we just sent because
      // Timotheos suppresses its own writeSyncOut during handleExternalSync
      SyncOutPath := IncludeTrailingPathDelimiter(DirPath) + 'ibiblia-sync-out.json';
      if FileExists(SyncOutPath) then
        WriteFileShared(SyncOutPath, OutPayload);

      // Timotheos closed: update settings.json so it opens at this verse next time
      SettingsPath := IncludeTrailingPathDelimiter(DirPath) + 'settings.json';
      if (not Running) and FileExists(SettingsPath) then
      begin
        Settings := ReadFileText(SettingsPath);
        if Settings <> '' then
        begin
          RefRegex := RegexCreate('"ref"\s*:\s*\{\s*"b"\s*:\s*\d+\s*,\s*"c"\s*:' +
            '\s*\d+\s*,\s*"v"\s*:\s*\d+[^}]*\}', [rcoUTF8]);
          if RefRegex.IsMatch(Settings) then
            WriteFileShared(SettingsPath, RefRegex.Replace(Settings,
              Format('"ref": {"b": %d, "c": %d, "v": %d}', [TargetBookID, Chapter, Verse])));
        end;
      end;
    end;

    // If no Timotheos userData folder existed yet, create the primary one
    if (not WroteAny) and (Dirs.Count > 0) then
      WriteFileShared(IncludeTrailingPathDelimiter(Dirs[0]) + 'ibiblia-sync-in.json', Payload);
  finally
    Dirs.Free;
  end;
end;

{ Book number for a name shown in Timotheos's title (English, Portuguese, or
  Apocrypha), or ''. }
function BookNumberFromName(const Name: string): string;
var
  CleanName: string;
  Index, ApocID, MappedB, MappedC: Integer;
begin
  Result := '';
  CleanName := Trim(Name);
  if CleanName = '' then
    Exit;

  Result := TWAutomateUtils.BookNameToNumber(CleanName);
  if Result <> '' then
    Exit;

  for Index := 1 to 66 do
    if SameText(LocalizedBookNameByID(Index), CleanName) then
      Exit(IntToStr(Index));

  ApocID := TimotheosApocryphaID(CleanName);
  if ApocID >= 67 then
  begin
    FromTimotheosBookAndChapter(ApocID, 1, CleanName, MappedB, MappedC);
    if MappedB > 0 then
      Exit(IntToStr(MappedB));
  end;
end;

function GetTanglawCurrentRef: string;
var
  Wnd: THandle;
  TitleBuf: array[0..255] of Char;
  Dirs: TStringList;
  Index, b, c, v, BestB, BestC, BestV, MappedB, MappedC: Integer;
  BestScore, Score: Int64;
  Json, Title, BookNum, OutPath, BestBookName, CandBookName: string;
  Match, TsMatch, NameMatch: IMatch;
  RefRegex, TsRegex, NameRegex, TitleRegex: IRegex;
begin
  Result := '';
  Wnd := IsTanglawRunning;
  if Wnd = 0 then
    Exit;

  RefRegex := RegexCreate('"b"\s*:\s*(\d+)\s*,\s*"c"\s*:\s*(\d+)\s*,\s*"v"\s*:\s*(\d+)',
    [rcoUTF8]);
  TsRegex := RegexCreate('"ts"\s*:\s*(\d+)', [rcoUTF8]);
  NameRegex := RegexCreate('"bookName"\s*:\s*"([^"]*)"', [rcoUTF8]);

  Dirs := TStringList.Create;
  try
    GetTanglawUserDataDirs(Dirs);

    // 1. Pick the freshest ibiblia-sync-out.json across all candidate directories
    BestB := 0;
    BestC := 0;
    BestV := 0;
    BestBookName := '';
    BestScore := -1;

    for Index := 0 to Dirs.Count - 1 do
    begin
      OutPath := IncludeTrailingPathDelimiter(Dirs[Index]) + 'ibiblia-sync-out.json';
      if not FileExists(OutPath) then
        Continue;
      Json := ReadFileText(OutPath);
      if Json = '' then
        Continue;
      Match := RefRegex.Match(Json);
      if Match.Success then
      begin
        b := StrToIntDef(Match.Groups[1].Value, 0);
        c := StrToIntDef(Match.Groups[2].Value, 0);
        v := StrToIntDef(Match.Groups[3].Value, 0);
        if (b <= 0) or (c <= 0) or (v <= 0) then
          Continue;

        Score := 0;
        TsMatch := TsRegex.Match(Json);
        if TsMatch.Success then
          Score := StrToInt64Def(TsMatch.Groups[1].Value, 0);
        if Score <= 0 then
          Score := FileAge(OutPath);

        CandBookName := '';
        NameMatch := NameRegex.Match(Json);
        if NameMatch.Success then
          CandBookName := NameMatch.Groups[1].Value;

        if Score > BestScore then
        begin
          BestScore := Score;
          BestB := b;
          BestC := c;
          BestV := v;
          BestBookName := CandBookName;
        end;
      end;
    end;

    if (BestB > 0) and (BestC > 0) and (BestV > 0) then
    begin
      if (GetTickCount64 < LastWriteTick + 1200) and
         (Format('%d,%d,%d', [BestB, BestC, BestV]) = LastWrittenRef) then
        Exit(LastWrittenProjectRef);

      FromTimotheosBookAndChapter(BestB, BestC, BestBookName, MappedB, MappedC);
      if (MappedB > 0) and (MappedC > 0) then
        Exit(Format('%d,%d,%d', [MappedB, MappedC, BestV]));
    end;

    // 2. Window title: "<Book> <c>:<v> - Timotheos" (or "- Tanglaw Bible")
    FillChar(TitleBuf, SizeOf(TitleBuf), 0);
    GetWindowText(Wnd, TitleBuf, Length(TitleBuf));
    Title := Trim(StrPas(TitleBuf));
    if Title <> '' then
    begin
      TitleRegex := RegexCreate('^(.*?)\s+(\d+):(\d+)\s+(?:-|–|—)\s*(?:Timotheos|Tanglaw)',
        [rcoUTF8, rcoIgnoreCase]);
      Match := TitleRegex.Match(Title);
      if Match.Success then
      begin
        BookNum := BookNumberFromName(Trim(Match.Groups[1].Value));
        if BookNum <> '' then
        begin
          b := StrToIntDef(BookNum, 0);
          c := StrToIntDef(Match.Groups[2].Value, 0);
          v := StrToIntDef(Match.Groups[3].Value, 0);
          FromTimotheosBookAndChapter(b, c, Trim(Match.Groups[1].Value), MappedB, MappedC);
          if (MappedB > 0) and (MappedC > 0) and (v > 0) then
            Exit(Format('%d,%d,%d', [MappedB, MappedC, v]));
        end;
      end;
    end;

    // 3. settings.json fallback, unless it still holds the verse we just sent
    BestB := 0;
    BestC := 0;
    BestV := 0;
    BestScore := -1;
    for Index := 0 to Dirs.Count - 1 do
    begin
      OutPath := IncludeTrailingPathDelimiter(Dirs[Index]) + 'settings.json';
      if not FileExists(OutPath) then
        Continue;
      Json := ReadFileText(OutPath);
      if Json = '' then
        Continue;
      Match := RegexCreate('"ref"\s*:\s*\{\s*"b"\s*:\s*(\d+)\s*,\s*"c"\s*:\s*(\d+)' +
        '\s*,\s*"v"\s*:\s*(\d+)', [rcoUTF8]).Match(Json);
      if Match.Success then
      begin
        b := StrToIntDef(Match.Groups[1].Value, 0);
        c := StrToIntDef(Match.Groups[2].Value, 0);
        v := StrToIntDef(Match.Groups[3].Value, 0);
        Score := FileAge(OutPath);
        if (b > 0) and (c > 0) and (v > 0) and (Score > BestScore) then
        begin
          BestScore := Score;
          BestB := b;
          BestC := c;
          BestV := v;
        end;
      end;
    end;

    if (BestB > 0) and (BestC > 0) and (BestV > 0) then
    begin
      if (GetTickCount64 < LastWriteTick + 1200) and
         (Format('%d,%d,%d', [BestB, BestC, BestV]) = LastWrittenRef) then
        Exit(LastWrittenProjectRef);
      FromTimotheosBookAndChapter(BestB, BestC, '', MappedB, MappedC);
      if (MappedB > 0) and (MappedC > 0) then
        Exit(Format('%d,%d,%d', [MappedB, MappedC, BestV]));
    end;
  finally
    Dirs.Free;
  end;
end;

{$ELSE}

function IsTanglawRunning: PtrUInt;
begin
  Result := 0;
end;

procedure SyncTanglawVerse(BookID, Chapter, Verse: Integer);
begin
end;

function GetTanglawCurrentRef: string;
begin
  Result := '';
end;

{$ENDIF}

end.
