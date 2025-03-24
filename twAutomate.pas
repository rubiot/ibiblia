unit twAutomate;

interface

uses PCRE, sysutils, classes, gmap, gutil, Dialogs, Windows, LazUTF8, Controls;

type
  TBCV_A = packed record
    case Integer of
      0: (span: byte; vi: byte; ci: byte; bi: byte);
      1: (val: Cardinal);
  end;

  TCopyData_Op_GotoVerse = packed record
    bcv: TBCV_A;
  end;

  //the phrase array should be zero-filled: when theWord receives theWord
  //it treates it as a PWideChar so it needs the terminating WideChar(0) to
  //know where the string ends
  TCopyData_Op_DctWordLookup = packed record
    phrase: array[1..255] of WideChar;
  end;

  { TWAutomateUtils }

  TWAutomateUtils = class
    class procedure Reset;
    class function IsTwRunning: THandle;
    class function GetTwCurrentRef: string;
    class procedure LoadLangFile(langFile: string);
    class function GetConfigFile: string;
    class function GetCurrentLangFile(configFile: string): string;
    class procedure FindTheWordWindows(outputFile: string);
  end;

const
  //the classname of the main theWord window.
  TW6_MAINFORM_CLASSNAME = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a667.UnicodeClass';
  TW7_MAINFORM_CLASSNAME = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a66';
  //the value of COPYDATASTRUCT.dwData
  COPYDATA_OP_FIRST = $ffff0000;
  //Operation go-to-verse
  COPYDATA_OP_GOTOVERSE = COPYDATA_OP_FIRST + 1;
  COPYDATA_OP_DCTWORDLOOKUP = COPYDATA_OP_FIRST + 2;

  // File encoding constants
  FILE_ENCODING_UTF8 = 0;
  FILE_ENCODING_UTF16LE = 1;
  FILE_ENCODING_UTF16BE = 2;
  FILE_ENCODING_UNKNOWN = -1;

type
  TStrLess = specialize TLess<string>;
  TBooksMap = specialize TMap<string, string, TStrLess>;

var
  BooksMap: TBooksMap;
  IgnoreErrors: boolean;

resourcestring
  STWSynchronizationErrorTitle = 'Synchronization error';
  STWSynchronizationError = 'Failed to detect theWord''s current verse.'#13#10'Please restart theWord if you have switched its current language.';

implementation

class procedure TWAutomateUtils.Reset;
begin
  IgnoreErrors := false;
end;

// Detects the encoding of a file by checking for BOM (Byte Order Mark)
function DetectFileEncoding(const fileName: string): Integer;
var
  fs: TFileStream;
  header: array[0..3] of Byte;
  bytesRead: Integer;
begin
  Result := FILE_ENCODING_UNKNOWN;

  if not FileExists(fileName) then
    Exit;

  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
  try
    bytesRead := fs.Read(header, SizeOf(header));

    if bytesRead >= 3 then
    begin
      // UTF-8 BOM: EF BB BF
      if (header[0] = $EF) and (header[1] = $BB) and (header[2] = $BF) then
        Result := FILE_ENCODING_UTF8
      // UTF-16 LE BOM: FF FE
      else if (header[0] = $FF) and (header[1] = $FE) then
        Result := FILE_ENCODING_UTF16LE
      // UTF-16 BE BOM: FE FF
      else if (header[0] = $FE) and (header[1] = $FF) then
        Result := FILE_ENCODING_UTF16BE;
    end;
  finally
    fs.Free;
  end;
end;

{------------------------------------------------------------------------------}
{ Return 0 if tw is not running, else the HWND of the running instance }
class function TWAutomateUtils.IsTwRunning: THandle;
var
  PrevWindow: THandle;
begin
  // Try TW7 window class first
  PrevWindow := FindWindow(TW7_MAINFORM_CLASSNAME, nil);
  if IsWindow(PrevWindow) then
  begin
    Result := PrevWindow;
    Exit;
  end;

  // Fallback to TW6 window class
  PrevWindow := FindWindow(TW6_MAINFORM_CLASSNAME, nil);
  if IsWindow(PrevWindow) then
  begin
    Result := PrevWindow;
    Exit;
  end;

  // Neither window class was found
  FindTheWordWindows('theWordWindows.txt');
  Result := 0;
end;

function QueryFullProcessImageName(hProcess: HANDLE; dwFlags: DWORD; lpExeName: LPTSTR; var lpdwSize: DWORD): BOOL; stdcall; external 'KERNEL32' name 'QueryFullProcessImageNameA';

class function TWAutomateUtils.GetConfigFile: string;
var
  hwnd, hProcess: THandle;
  module: array[0 .. 255] of char;
  pid, size: DWORD;
begin
  result := '';
  pid := 0;
  hwnd := IsTwRunning;

  if hwnd = 0 then
    Exit;

  size := sizeof(module);
  if GetWindowThreadProcessId(hwnd, pid) <> 0 then
  begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, false, pid);
    if hProcess <> 0 then
    begin
      try
        if QueryFullProcessImageName(hProcess, 0, module, size) then
        begin
          result := module;
          result := ExtractFilePath(result) + 'config.ini';
        end;
      finally
        CloseHandle(hProcess);
      end;
    end;
  end;
end;

class function TWAutomateUtils.GetTwCurrentRef: string;

  function ExtractRefStr(caption: string): string;
  var
    reRef: IRegex;
    mtRef: IMatch;
  begin
    result := '';
    reRef := RegexCreate('^(.*?) (\d+):(\d+)', [rcoUTF8]);
    mtRef := reRef.Match(caption);

    if not mtRef.Success then exit;

    if not assigned(BooksMap) or (BooksMap.Find(mtRef.Groups[1].Value) = nil) then
    begin
      FreeAndNil(BooksMap);
      LoadLangFile(GetCurrentLangFile(GetConfigFile));
    end;

    if BooksMap.Find(mtRef.Groups[1].Value) = nil then
    begin
      if not IgnoreErrors and (MessageDlg(STWSynchronizationErrorTitle, STWSynchronizationError, mtError, [mbOK, mbIgnore], 0) = mrIgnore) then
        IgnoreErrors := true;
      exit;
    end;

    result := format('%s,%s,%s', [BooksMap[mtRef.Groups[1].Value], mtRef.Groups[2].Value, mtRef.Groups[3].Value]);
  end;

var
  hwnd: THandle;
  caption: array[0 .. 255] of char;
  refStr: string;
begin
  result := '';
  hwnd := IsTwRunning;
  if hwnd <> 0 then
  begin
    GetWindowText(hwnd, caption, SizeOf(caption));
    refStr := caption;
    result := ExtractRefStr(refStr);
  end;
end;

class procedure TWAutomateUtils.LoadLangFile(langFile: string);
var
  lines: TStringList;
  i: integer;
  reBook: IRegex;
  mtBook: IMatch;
begin
  if langFile = '' then
    exit;

  reBook := RegexCreate('^\s*rc_bk_alias3_(\d+)=''([^'']+)''', [rcoUTF8]);

  lines := TStringList.Create;
  lines.LoadFromFile(langFile);

  if BooksMap <> nil then
    BooksMap.Free;

  BooksMap := TBooksMap.Create;
  for i:=0 to lines.Count - 1 do
  begin
    mtBook := reBook.Match(lines[i]);
    if mtBook.Success then
      BooksMap[mtBook.Groups[2].Value] := mtBook.Groups[1].Value;
  end;

  lines.Free;
end;

class function TWAutomateUtils.GetCurrentLangFile(configFile: string): string;
var
  lines: TStringList;
  i: integer;
  reLang: IRegex;
  matches: IMatch;
  line: string;
  stream: TMemoryStream;
  encoding: Integer;
begin
  result := '';
  if configFile = '' then
    exit;

  encoding := DetectFileEncoding(configFile);
  lines := TStringList.Create;
  try
    stream := TMemoryStream.Create;
    try
      stream.LoadFromFile(configFile);
      stream.Position := 0;

      // Handle according to detected encoding
      case encoding of
        FILE_ENCODING_UTF16LE, FILE_ENCODING_UTF16BE:
          begin
            // Skip BOM if present (position is already set after LoadFromFile)
            lines.AddText(UTF16ToUTF8(PWideChar(stream.Memory), stream.Size div SizeOf(WideChar)));
          end;
        FILE_ENCODING_UTF8:
          begin
            // Standard UTF-8 with BOM
            lines.LoadFromFile(configFile);
          end;
        else
          begin
            // Unknown encoding or no BOM, try to load it directly
            lines.LoadFromFile(configFile);
          end;
      end;

      reLang := RegexCreate('^lang=(\w+)', [rcoUTF8]);

      for i:=0 to lines.Count - 1 do
      begin
        line := lines[i];
        matches := reLang.Match(line);
        if matches.Success then
        begin
          result := format('%s%s.lng', [ExtractFilePath(configFile), matches.Groups[1].Value]);
          exit;
        end;
      end;
    finally
      stream.Free;
    end;
  finally
    lines.Free;
  end;
end;

function EnumWindowsProc(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  className, windowTitle: array[0..255] of Char;
  classStr, titleStr: string;
  fPtr: ^TextFile;
begin
  Result := True; // Continue enumeration

  // Get window class name
  GetClassName(hwnd, className, 256);
  classStr := className;

  // Get window title
  GetWindowText(hwnd, windowTitle, 256);
  titleStr := windowTitle;

  // Check if either class name or title contains 'theWord'
  if (Pos('theWord', LowerCase(classStr)) > 0) or
     (Pos('theword', LowerCase(titleStr)) > 0) then
  begin
    // Write to file
    fPtr := Pointer(lParam);
    WriteLn(fPtr^, 'Window Handle: ', hwnd);
    WriteLn(fPtr^, 'Class Name: ', classStr);
    WriteLn(fPtr^, 'Window Title: ', titleStr);
    WriteLn(fPtr^, StringOfChar('-', 50));
  end;
end;

class procedure TWAutomateUtils.FindTheWordWindows(outputFile: string);
var
  f: TextFile;
begin
  AssignFile(f, outputFile);
  try
    Rewrite(f);
    WriteLn(f, 'List of Windows with "theWord" in class name or title:');
    WriteLn(f, StringOfChar('=', 50));
    WriteLn(f, 'Current MAINFORM_CLASSNAME constant: ', TW6_MAINFORM_CLASSNAME);
    WriteLn(f, StringOfChar('=', 50));
    WriteLn(f);

    // Enumerate all windows
    EnumWindows(@EnumWindowsProc, LPARAM(@f));

    WriteLn(f);
    WriteLn(f, 'End of Window List');
  finally
    CloseFile(f);
  end;

  ShowMessage('theWord window not found. Debug information has been saved to: ' + outputFile);
end;

end.
