unit crash_tracker;

{ Automated crash, unhandled exception, and hard-crash recovery logger.
  Captures stack traces, runtime environment, open project/verse context, and
  recent UI breadcrumbs into timestamped log files under <ConfigDir>/crash_logs,
  maintains a .session_active watchdog for hard crashes, and notifies the UI. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCrashLogSummary = record
    FileName: string;
    FilePath: string;
    TimestampText: string;
    Kind: string;
    Headline: string;
    FileSize: Int64;
    ModifiedAt: TDateTime;
  end;
  TCrashLogSummaryArray = array of TCrashLogSummary;

  TCrashDialogCallback = procedure(const LogFile, LogContent: string;
    IsRecoveredHardCrash: Boolean; out ShouldTerminate: Boolean);

procedure SetCrashLogBaseDir(const BaseDir: string);
function GetCrashLogsDir: string;

procedure InstallCrashTracker;
procedure FinalizeCrashTracker;
procedure MarkCrashSessionCleanShutdown;
function TakeRecoveredHardCrashLog(out LogFile, LogContent: string): Boolean;

procedure RegisterCrashDialogCallback(ACallback: TCrashDialogCallback);

procedure RecordCrashBreadcrumb(const EventText: string);
procedure SetCrashContextProject(const ProjectFile: string);
procedure SetCrashContextVerse(const VerseRef: string);

function WriteDiagnosticReport(const UserNote: string;
  out LogFile, LogContent: string): Boolean;
function ListCrashLogs: TCrashLogSummaryArray;
function ReadCrashLogFile(const FilePath: string): string;

implementation

uses
  Forms, Controls, syncobjs, app_updater;

const
  MAX_BREADCRUMBS = 60;
  CRASH_LOGS_SUBDIR = 'crash_logs';
  CRASH_HISTORY_FILE = 'crash_history.log';
  SESSION_MARKER_FILE = '.session_active';

type
  TCrashTrackerAppHandler = class
  public
    procedure HandleApplicationException(Sender: TObject; E: Exception);
  end;

var
  GLock: syncobjs.TCriticalSection;
  GInstalled: Boolean;
  FCleanShutdownMarked: Boolean;
  FHandlingCrash: Boolean;
  FBaseDir: string;
  FActiveMarkerFile: string;
  FRecoveredHardCrashFile: string;
  FRecoveredHardCrashPending: Boolean;
  FCurrentProject: string;
  FCurrentVerse: string;
  FSessionStartedText: string;
  FBreadcrumbs: array[0..MAX_BREADCRUMBS - 1] of string;
  FBreadcrumbCount: Integer;
  FBreadcrumbHead: Integer;
  FOldExceptProc: TExceptProc;
  GAppHandler: TCrashTrackerAppHandler;
  FCrashDialogCallback: TCrashDialogCallback;

function NormalizeDirPath(const Dir: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(ExpandFileName(Dir));
  {$IFDEF WINDOWS}
  Result := AnsiLowerCase(Result);
  {$ENDIF}
end;

function SafeWriteTextFile(const FileName, Content: string): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;
  try
    ForceDirectories(ExtractFileDir(FileName));
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      if Length(Content) > 0 then
        Stream.WriteBuffer(Content[1], Length(Content));
    finally
      Stream.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function SafeAppendLine(const FileName, LineText: string): Boolean;
var
  Stream: TFileStream;
  FullLine: string;
begin
  Result := False;
  try
    ForceDirectories(ExtractFileDir(FileName));
    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
    else
      Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.Seek(0, soFromEnd);
      FullLine := LineText + LineEnding;
      if Length(FullLine) > 0 then
        Stream.WriteBuffer(FullLine[1], Length(FullLine));
    finally
      Stream.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function SafeReadTextFile(const FilePath: string): string;
var
  Stream: TFileStream;
  Len: Int64;
begin
  Result := '';
  if (Trim(FilePath) = '') or (not FileExists(FilePath)) then
    Exit;
  try
    Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    try
      Len := Stream.Size;
      if Len > 0 then
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

function SafeAppVersion: string;
var
  RawVersion: string;
begin
  Result := 'unknown';
  try
    RawVersion := Trim(CurrentAppVersion);
    if RawVersion <> '' then
      Result := NormalizeVersion(RawVersion);
  except
    Result := 'unknown';
  end;
end;

function SafeLastBreadcrumbLocked: string;
var
  Idx: Integer;
begin
  if FBreadcrumbCount <= 0 then
    Exit('');
  Idx := (FBreadcrumbHead - 1 + MAX_BREADCRUMBS) mod MAX_BREADCRUMBS;
  Result := FBreadcrumbs[Idx];
end;

function ResolveDefaultBaseDir: string;
{$IFDEF WINDOWS}
var
  AppDir, ProbeFile: string;
  ProbeStream: TFileStream;
begin
  AppDir := ExtractFileDir(ParamStr(0));
  if AppDir <> '' then
  begin
    ProbeFile := IncludeTrailingPathDelimiter(AppDir) + '.ibiblia_write_probe';
    try
      ProbeStream := TFileStream.Create(ProbeFile, fmCreate);
      ProbeStream.Free;
      SysUtils.DeleteFile(ProbeFile);
      Result := AppDir;
      Exit;
    except
    end;
  end;
  Result := ExcludeTrailingPathDelimiter(GetAppConfigDir(False));
  ForceDirectories(Result);
end;
{$ELSE}
var
  ConfigRoot: string;
begin
  ConfigRoot := SysUtils.GetEnvironmentVariable('XDG_CONFIG_HOME');
  if ConfigRoot = '' then
    ConfigRoot := IncludeTrailingPathDelimiter(GetUserDir) + '.config';
  Result := IncludeTrailingPathDelimiter(ConfigRoot) + 'iBiblia';
  ForceDirectories(Result);
end;
{$ENDIF}

function EnsureBaseDir: string;
begin
  if Trim(FBaseDir) = '' then
    FBaseDir := ResolveDefaultBaseDir;
  Result := ExcludeTrailingPathDelimiter(FBaseDir);
  if Result <> '' then
    ForceDirectories(Result);
end;

function GetCrashLogsDir: string;
begin
  Result := IncludeTrailingPathDelimiter(EnsureBaseDir) + CRASH_LOGS_SUBDIR;
  ForceDirectories(Result);
end;

procedure WriteActiveSessionMarkerLocked;
var
  LogsDir, MarkerPath: string;
  Lines: TStringList;
begin
  if FCleanShutdownMarked then
    Exit;
  LogsDir := GetCrashLogsDir;
  MarkerPath := IncludeTrailingPathDelimiter(LogsDir) + SESSION_MARKER_FILE;
  if FSessionStartedText = '' then
    FSessionStartedText := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Lines := TStringList.Create;
  try
    Lines.Add('started=' + FSessionStartedText);
    Lines.Add('version=' + SafeAppVersion);
    Lines.Add('os=' + {$I %FPCTARGETOS%} + '/' + {$I %FPCTARGETCPU%});
    Lines.Add('project=' + FCurrentProject);
    Lines.Add('verse=' + FCurrentVerse);
    Lines.Add('last_event=' + SafeLastBreadcrumbLocked);
    Lines.Add('updated=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    SafeWriteTextFile(MarkerPath, Lines.Text);
    FActiveMarkerFile := MarkerPath;
  finally
    Lines.Free;
  end;
end;

function MakeUniqueLogFileName(const LogsDir, Prefix, Suffix: string): string;
var
  Stamp, Candidate: string;
  N: Integer;
begin
  Stamp := FormatDateTime('yyyy-mm-dd_hhnnss', Now);
  Candidate := IncludeTrailingPathDelimiter(LogsDir) +
    Prefix + Stamp + Suffix + '.log';
  N := 1;
  while FileExists(Candidate) and (N < 1000) do
  begin
    Candidate := IncludeTrailingPathDelimiter(LogsDir) +
      Prefix + Stamp + '_' + IntToStr(N) + Suffix + '.log';
    Inc(N);
  end;
  Result := Candidate;
end;

procedure CheckStaleSessionMarkerLocked(const LogsDir: string);
var
  MarkerPath, MarkerText, RecoveredPath, SummaryLine: string;
  Lines: TStringList;
begin
  MarkerPath := IncludeTrailingPathDelimiter(LogsDir) + SESSION_MARKER_FILE;
  if (FActiveMarkerFile <> '') and
     (NormalizeDirPath(ExtractFileDir(FActiveMarkerFile)) = NormalizeDirPath(LogsDir)) then
    Exit;
  if not FileExists(MarkerPath) then
    Exit;

  MarkerText := SafeReadTextFile(MarkerPath);
  SysUtils.DeleteFile(MarkerPath);

  Lines := TStringList.Create;
  try
    Lines.Add('iBiblia Crash Report (Recovered Hard Crash)');
    Lines.Add(StringOfChar('=', 60));
    Lines.Add('Detected at startup: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Lines.Add('Kind:                Recovered crash');
    Lines.Add('Summary:             Previous iBiblia session did not close cleanly');
    Lines.Add('App version:         ' + SafeAppVersion);
    Lines.Add('Target OS / CPU:     ' + {$I %FPCTARGETOS%} + ' / ' + {$I %FPCTARGETCPU%});
    Lines.Add('');
    Lines.Add('Previous session status file:');
    if Trim(MarkerText) <> '' then
      Lines.Add(TrimRight(MarkerText))
    else
      Lines.Add('(empty session marker)');
    Lines.Add('');
    Lines.Add('Explanation:');
    Lines.Add('  iBiblia found an active session marker left behind by a previous run.');
    Lines.Add('  This usually means iBiblia crashed immediately, lost power, or was');
    Lines.Add('  terminated by the operating system before saving shutdown state.');

    RecoveredPath := MakeUniqueLogFileName(LogsDir, 'ibiblia_crash_', '_recovered');
    if SafeWriteTextFile(RecoveredPath, Lines.Text) then
    begin
      FRecoveredHardCrashFile := RecoveredPath;
      FRecoveredHardCrashPending := True;
      SummaryLine := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
        ' | Recovered crash | Previous session terminated unexpectedly | ' +
        ExtractFileName(RecoveredPath);
      SafeAppendLine(IncludeTrailingPathDelimiter(LogsDir) + CRASH_HISTORY_FILE,
        SummaryLine);
    end;
  finally
    Lines.Free;
  end;
end;

procedure SetCrashLogBaseDir(const BaseDir: string);
var
  CleanNewDir, OldLogsDir, NewLogsDir: string;
begin
  if Trim(BaseDir) = '' then
    Exit;
  CleanNewDir := ExcludeTrailingPathDelimiter(ExpandFileName(BaseDir));
  if CleanNewDir = '' then
    Exit;

  if Assigned(GLock) then
    GLock.Acquire;
  try
    if (FBaseDir <> '') and (NormalizeDirPath(FBaseDir) = NormalizeDirPath(CleanNewDir)) then
      Exit;

    OldLogsDir := '';
    if FActiveMarkerFile <> '' then
    begin
      OldLogsDir := ExtractFileDir(FActiveMarkerFile);
      SysUtils.DeleteFile(FActiveMarkerFile);
      FActiveMarkerFile := '';
    end;

    FBaseDir := CleanNewDir;
    ForceDirectories(FBaseDir);
    NewLogsDir := GetCrashLogsDir;
    if GInstalled and (not FCleanShutdownMarked) then
    begin
      if (OldLogsDir = '') or (NormalizeDirPath(OldLogsDir) <> NormalizeDirPath(NewLogsDir)) then
        CheckStaleSessionMarkerLocked(NewLogsDir);
      WriteActiveSessionMarkerLocked;
    end;
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

function BuildStackTraceText(Addr: CodePointer; FrameCount: Longint;
  Frames: PCodePointer): string;
var
  Lines: TStringList;
  I, MaxFrames: Integer;
  LineStr: string;
  FramePtr: PCodePointer;
begin
  Lines := TStringList.Create;
  try
    try
      if Addr <> nil then
      begin
        if Assigned(BackTraceStrFunc) then
          LineStr := Trim(BackTraceStrFunc(Addr))
        else
          LineStr := '';
        if LineStr <> '' then
          Lines.Add('  [0] ' + LineStr)
        else
          Lines.Add('  [0] $' + IntToHex(PtrUInt(Addr), SizeOf(CodePointer) * 2));
      end;

      MaxFrames := FrameCount;
      if MaxFrames > 64 then
        MaxFrames := 64;
      if (Frames <> nil) and (MaxFrames > 0) then
      begin
        FramePtr := Frames;
        for I := 0 to MaxFrames - 1 do
        begin
          if Assigned(BackTraceStrFunc) then
            LineStr := Trim(BackTraceStrFunc(FramePtr^))
          else
            LineStr := '';
          if LineStr <> '' then
            Lines.Add('  [' + IntToStr(I + 1) + '] ' + LineStr)
          else
            Lines.Add('  [' + IntToStr(I + 1) + '] $' +
              IntToHex(PtrUInt(FramePtr^), SizeOf(CodePointer) * 2));
          Inc(FramePtr);
        end;
      end;

      if Lines.Count = 0 then
        Lines.Add('  (stack frames unavailable in release binary)');
    except
      on E: Exception do
        Lines.Add('  (error resolving stack trace: ' + E.Message + ')');
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function SafeActiveFormDescription: string;
var
  FormObj: TCustomForm;
begin
  Result := '(none)';
  try
    if Assigned(Screen) and Assigned(Screen.ActiveCustomForm) then
    begin
      FormObj := Screen.ActiveCustomForm;
      Result := FormObj.ClassName + ' ("' + FormObj.Caption + '")';
    end
    else if Assigned(Application) and Assigned(Application.MainForm) then
      Result := Application.MainForm.ClassName;
  except
    Result := '(unavailable)';
  end;
end;

function SafeActiveControlDescription: string;
var
  CtrlObj: TWinControl;
begin
  Result := '(none)';
  try
    if Assigned(Screen) and Assigned(Screen.ActiveControl) then
    begin
      CtrlObj := Screen.ActiveControl;
      Result := CtrlObj.Name + ' (' + CtrlObj.ClassName + ')';
    end;
  except
    Result := '(unavailable)';
  end;
end;

function BuildFullReportText(const Kind, Headline, ExceptionClass, ExceptionMsg,
  StackText, UserNote: string): string;
var
  Lines: TStringList;
  StartIdx, Offset, Idx: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('iBiblia Error & Diagnostic Report');
    Lines.Add(StringOfChar('=', 60));
    Lines.Add('Timestamp:           ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Lines.Add('Kind:                ' + Kind);
    Lines.Add('Summary:             ' + Headline);
    Lines.Add('App version:         ' + SafeAppVersion);
    Lines.Add('FPC / Target:        ' + {$I %FPCVERSION%} + ' / ' +
      {$I %FPCTARGETOS%} + '-' + {$I %FPCTARGETCPU%});
    Lines.Add('Executable:          ' + ParamStr(0));
    Lines.Add('Logs folder:         ' + GetCrashLogsDir);
    Lines.Add('Session started:     ' + FSessionStartedText);
    Lines.Add('Open project:        ' + FCurrentProject);
    Lines.Add('Current verse:       ' + FCurrentVerse);
    Lines.Add('Active window:       ' + SafeActiveFormDescription);
    Lines.Add('Active control:      ' + SafeActiveControlDescription);

    if Trim(UserNote) <> '' then
    begin
      Lines.Add('');
      Lines.Add('User note:');
      Lines.Add('  ' + Trim(UserNote));
    end;

    if (ExceptionClass <> '') or (ExceptionMsg <> '') then
    begin
      Lines.Add('');
      Lines.Add('Exception:');
      Lines.Add('  Class:   ' + ExceptionClass);
      Lines.Add('  Message: ' + ExceptionMsg);
    end;

    if Trim(StackText) <> '' then
    begin
      Lines.Add('');
      Lines.Add('Stack trace:');
      Lines.Add(TrimRight(StackText));
    end;

    Lines.Add('');
    Lines.Add('Recent activity (oldest to newest):');
    if FBreadcrumbCount = 0 then
      Lines.Add('  (no breadcrumbs recorded)')
    else
    begin
      StartIdx := (FBreadcrumbHead - FBreadcrumbCount + MAX_BREADCRUMBS) mod MAX_BREADCRUMBS;
      for Offset := 0 to FBreadcrumbCount - 1 do
      begin
        Idx := (StartIdx + Offset) mod MAX_BREADCRUMBS;
        Lines.Add('  ' + FBreadcrumbs[Idx]);
      end;
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function CaptureExceptionReport(const Kind, ExClass, ExMessage, StackText: string;
  out LogFile, LogContent: string): Boolean;
var
  LogsDir, Headline, SummaryLine: string;
begin
  Result := False;
  LogFile := '';
  LogContent := '';

  if Assigned(GLock) then
    GLock.Acquire;
  try
    LogsDir := GetCrashLogsDir;
    Headline := ExClass;
    if Trim(ExMessage) <> '' then
      Headline := Headline + ': ' + StringReplace(
        StringReplace(Trim(ExMessage), #13, ' ', [rfReplaceAll]),
        #10, ' ', [rfReplaceAll]);
    if Length(Headline) > 180 then
      Headline := Copy(Headline, 1, 177) + '...';

    LogContent := BuildFullReportText(Kind, Headline, ExClass, ExMessage, StackText, '');
    LogFile := MakeUniqueLogFileName(LogsDir, 'ibiblia_crash_', '');
    if SafeWriteTextFile(LogFile, LogContent) then
    begin
      Result := True;
      SummaryLine := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
        ' | ' + Kind + ' | ' + Headline + ' | ' + ExtractFileName(LogFile);
      SafeAppendLine(IncludeTrailingPathDelimiter(LogsDir) + CRASH_HISTORY_FILE,
        SummaryLine);
    end;
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

procedure TCrashTrackerAppHandler.HandleApplicationException(Sender: TObject;
  E: Exception);
var
  LogFile, LogContent, StackText, ExClass, ExMsg: string;
  ShouldTerminate: Boolean;
begin
  if (E is EAbort) then
    Exit;

  if FHandlingCrash then
    Exit;

  FHandlingCrash := True;
  try
    if Assigned(E) then
    begin
      ExClass := E.ClassName;
      ExMsg := E.Message;
    end
    else
    begin
      ExClass := 'Exception';
      ExMsg := 'Unknown unhandled exception';
    end;

    RecordCrashBreadcrumb('Unhandled exception: ' + ExClass + ' - ' + ExMsg);
    StackText := BuildStackTraceText(ExceptAddr, ExceptFrameCount, ExceptFrames);
    CaptureExceptionReport('Crash', ExClass, ExMsg, StackText, LogFile, LogContent);

    ShouldTerminate := False;
    if Assigned(FCrashDialogCallback) and Assigned(Application) and
       (not Application.Terminated) then
    begin
      try
        FCrashDialogCallback(LogFile, LogContent, False, ShouldTerminate);
      except
      end;
      if ShouldTerminate then
      begin
        MarkCrashSessionCleanShutdown;
        Application.Terminate;
      end;
    end;
  finally
    FHandlingCrash := False;
  end;
end;

procedure CrashTrackerExceptProc(Obj: TObject; Addr: CodePointer;
  FrameCount: Longint; Frames: PCodePointer);
var
  ExClass, ExMsg, StackText, LogFile, LogContent: string;
  ShouldTerminate: Boolean;
begin
  if not (Assigned(Obj) and (Obj is EAbort)) then
  begin
    if not FHandlingCrash then
    begin
      FHandlingCrash := True;
      try
        if Assigned(Obj) then
        begin
          if Obj is Exception then
          begin
            ExClass := Exception(Obj).ClassName;
            ExMsg := Exception(Obj).Message;
          end
          else
          begin
            ExClass := Obj.ClassName;
            ExMsg := 'Unhandled RTL object exception';
          end;
        end
        else
        begin
          ExClass := 'RuntimeError';
          ExMsg := 'Unhandled RTL exception with nil exception object';
        end;

        RecordCrashBreadcrumb('Fatal exception: ' + ExClass + ' - ' + ExMsg);
        StackText := BuildStackTraceText(Addr, FrameCount, Frames);
        CaptureExceptionReport('Crash', ExClass, ExMsg, StackText, LogFile, LogContent);

        if (GetCurrentThreadId = MainThreadID) and
           Assigned(FCrashDialogCallback) and
           Assigned(Application) and (not Application.Terminated) then
        begin
          try
            FCrashDialogCallback(LogFile, LogContent, False, ShouldTerminate);
          except
          end;
        end;
      finally
        FHandlingCrash := False;
      end;
    end;
  end;

  if Assigned(FOldExceptProc) then
    FOldExceptProc(Obj, Addr, FrameCount, Frames);
end;

procedure InstallCrashTracker;
var
  LogsDir: string;
begin
  if GInstalled then
    Exit;

  if Assigned(GLock) then
    GLock.Acquire;
  try
    GInstalled := True;
    FCleanShutdownMarked := False;
    FSessionStartedText := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    LogsDir := GetCrashLogsDir;
    CheckStaleSessionMarkerLocked(LogsDir);
    WriteActiveSessionMarkerLocked;

    if not Assigned(GAppHandler) then
      GAppHandler := TCrashTrackerAppHandler.Create;
    if Assigned(Application) then
      Application.OnException := @GAppHandler.HandleApplicationException;

    FOldExceptProc := ExceptProc;
    ExceptProc := @CrashTrackerExceptProc;
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
  RecordCrashBreadcrumb('Crash tracker initialized');
end;

procedure FinalizeCrashTracker;
begin
  if not GInstalled then
    Exit;
  MarkCrashSessionCleanShutdown;
  if Assigned(Application) then
    Application.OnException := nil;
  if Pointer(ExceptProc) = Pointer(@CrashTrackerExceptProc) then
    ExceptProc := FOldExceptProc;
  GInstalled := False;
end;

procedure MarkCrashSessionCleanShutdown;
begin
  if Assigned(GLock) then
    GLock.Acquire;
  try
    FCleanShutdownMarked := True;
    if (FActiveMarkerFile <> '') and FileExists(FActiveMarkerFile) then
      SysUtils.DeleteFile(FActiveMarkerFile);
    FActiveMarkerFile := '';
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

function TakeRecoveredHardCrashLog(out LogFile, LogContent: string): Boolean;
begin
  Result := False;
  LogFile := '';
  LogContent := '';
  if Assigned(GLock) then
    GLock.Acquire;
  try
    if FRecoveredHardCrashPending and (FRecoveredHardCrashFile <> '') and
       FileExists(FRecoveredHardCrashFile) then
    begin
      LogFile := FRecoveredHardCrashFile;
      LogContent := SafeReadTextFile(LogFile);
      FRecoveredHardCrashPending := False;
      Result := True;
    end;
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

procedure RegisterCrashDialogCallback(ACallback: TCrashDialogCallback);
begin
  FCrashDialogCallback := ACallback;
end;

procedure RecordCrashBreadcrumb(const EventText: string);
var
  Entry: string;
begin
  if Trim(EventText) = '' then
    Exit;
  if Assigned(GLock) then
    GLock.Acquire;
  try
    Entry := FormatDateTime('hh:nn:ss', Now) + '  ' + Trim(EventText);
    FBreadcrumbs[FBreadcrumbHead] := Entry;
    FBreadcrumbHead := (FBreadcrumbHead + 1) mod MAX_BREADCRUMBS;
    if FBreadcrumbCount < MAX_BREADCRUMBS then
      Inc(FBreadcrumbCount);
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

procedure SetCrashContextProject(const ProjectFile: string);
begin
  if Assigned(GLock) then
    GLock.Acquire;
  try
    FCurrentProject := Trim(ProjectFile);
    if FCurrentProject = '' then
      FCurrentVerse := '';
    if GInstalled and (not FCleanShutdownMarked) then
      WriteActiveSessionMarkerLocked;
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

procedure SetCrashContextVerse(const VerseRef: string);
begin
  if Assigned(GLock) then
    GLock.Acquire;
  try
    FCurrentVerse := Trim(VerseRef);
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

function WriteDiagnosticReport(const UserNote: string;
  out LogFile, LogContent: string): Boolean;
var
  LogsDir, Headline, SummaryLine: string;
begin
  Result := False;
  LogFile := '';
  LogContent := '';
  RecordCrashBreadcrumb('Created manual diagnostic report');

  if Assigned(GLock) then
    GLock.Acquire;
  try
    LogsDir := GetCrashLogsDir;
    if Trim(UserNote) <> '' then
      Headline := 'Manual diagnostic report: ' + Trim(UserNote)
    else
      Headline := 'Manual diagnostic report';

    LogContent := BuildFullReportText('Diagnostic', Headline, '', '', '', UserNote);
    LogFile := MakeUniqueLogFileName(LogsDir, 'ibiblia_diag_', '');
    if SafeWriteTextFile(LogFile, LogContent) then
    begin
      Result := True;
      SummaryLine := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
        ' | Diagnostic | ' + Headline + ' | ' + ExtractFileName(LogFile);
      SafeAppendLine(IncludeTrailingPathDelimiter(LogsDir) + CRASH_HISTORY_FILE,
        SummaryLine);
    end;
  finally
    if Assigned(GLock) then
      GLock.Release;
  end;
end;

procedure ParseLogMetadata(const FilePath, FileName: string; ModifiedAt: TDateTime;
  out TimestampText, Kind, Headline: string);
var
  Lines: TStringList;
  I: Integer;
  Line, LowerLine: string;
begin
  TimestampText := FormatDateTime('yyyy-mm-dd hh:nn:ss', ModifiedAt);
  if Pos('ibiblia_diag_', LowerCase(FileName)) = 1 then
    Kind := 'Diagnostic'
  else if Pos('_recovered', LowerCase(FileName)) > 0 then
    Kind := 'Recovered crash'
  else
    Kind := 'Crash';
  Headline := FileName;

  Lines := TStringList.Create;
  try
    Lines.Text := SafeReadTextFile(FilePath);
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      LowerLine := LowerCase(Line);
      if Pos('timestamp:', LowerLine) = 1 then
        TimestampText := Trim(Copy(Line, Length('Timestamp:') + 1, MaxInt))
      else if Pos('detected at startup:', LowerLine) = 1 then
        TimestampText := Trim(Copy(Line, Length('Detected at startup:') + 1, MaxInt))
      else if Pos('kind:', LowerLine) = 1 then
        Kind := Trim(Copy(Line, Length('Kind:') + 1, MaxInt))
      else if Pos('summary:', LowerLine) = 1 then
      begin
        Headline := Trim(Copy(Line, Length('Summary:') + 1, MaxInt));
        Break;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function ListCrashLogs: TCrashLogSummaryArray;
var
  LogsDir: string;
  Search: TSearchRec;
  Count, Cap, I, J: Integer;
  ModTime: TDateTime;
  TempItem: TCrashLogSummary;

  procedure ScanPattern(const Pattern: string);
  var
    FullPath: string;
  begin
    if FindFirst(IncludeTrailingPathDelimiter(LogsDir) + Pattern,
       faAnyFile and not faDirectory, Search) = 0 then
    begin
      try
        repeat
          if (Search.Attr and faDirectory) <> 0 then
            Continue;
          FullPath := IncludeTrailingPathDelimiter(LogsDir) + Search.Name;
          if Count >= Cap then
          begin
            Cap := Cap * 2;
            SetLength(Result, Cap);
          end;
          ModTime := FileDateToDateTime(Search.Time);
          Result[Count].FileName := Search.Name;
          Result[Count].FilePath := FullPath;
          Result[Count].FileSize := Search.Size;
          Result[Count].ModifiedAt := ModTime;
          ParseLogMetadata(FullPath, Search.Name, ModTime,
            Result[Count].TimestampText,
            Result[Count].Kind,
            Result[Count].Headline);
          Inc(Count);
        until FindNext(Search) <> 0;
      finally
        FindClose(Search);
      end;
    end;
  end;

begin
  SetLength(Result, 0);
  LogsDir := GetCrashLogsDir;
  Count := 0;
  Cap := 32;
  SetLength(Result, Cap);
  ScanPattern('ibiblia_crash_*.log');
  ScanPattern('ibiblia_diag_*.log');
  SetLength(Result, Count);

  for I := 0 to Count - 2 do
    for J := I + 1 to Count - 1 do
      if (Result[J].ModifiedAt > Result[I].ModifiedAt) or
         ((Result[J].ModifiedAt = Result[I].ModifiedAt) and
          (Result[J].FileName > Result[I].FileName)) then
      begin
        TempItem := Result[I];
        Result[I] := Result[J];
        Result[J] := TempItem;
      end;
end;

function ReadCrashLogFile(const FilePath: string): string;
begin
  Result := SafeReadTextFile(FilePath);
end;

initialization
  GLock := syncobjs.TCriticalSection.Create;
  GInstalled := False;
  FCleanShutdownMarked := False;
  FHandlingCrash := False;
  FBaseDir := '';
  FActiveMarkerFile := '';
  FRecoveredHardCrashFile := '';
  FRecoveredHardCrashPending := False;
  FCurrentProject := '';
  FCurrentVerse := '';
  FSessionStartedText := '';
  FBreadcrumbCount := 0;
  FBreadcrumbHead := 0;
  FOldExceptProc := nil;
  GAppHandler := nil;
  FCrashDialogCallback := nil;

finalization
  FinalizeCrashTracker;
  FreeAndNil(GAppHandler);
  FreeAndNil(GLock);

end.
