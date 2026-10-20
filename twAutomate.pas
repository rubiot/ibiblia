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
    class procedure InitDefaultBooksMap;
    class function GetConfigFile: string;
    class function GetCurrentLangFile(configFile: string): string;
    class procedure FindTheWordWindows(outputFile: string);
  end;

const
  // The standard classname of the main theWord window (theWord 6 and 7).
  TW_MAINFORM_CLASSNAME = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a66.UnicodeClass';
  TW_MAINFORM_CLASSNAME_NOUNICODE = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a66';

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

class procedure TWAutomateUtils.InitDefaultBooksMap;
const
  EngAliases: array[1..66] of string = (
    'Gen','Exo','Lev','Num','Deu','Jos','Jdg','Rut','1Sa','2Sa','1Ki','2Ki','1Ch','2Ch','Ezr','Neh','Est','Job','Psa','Pro','Ecc','Sng','Isa','Jer','Lam','Ezk','Dan','Hos','Joe','Amo','Oba','Jon','Mic','Nah','Hab','Zep','Hag','Zec','Mal','Mat','Mar','Luk','Joh','Act','Rom','1Co','2Co','Gal','Eph','Php','Col','1Th','2Th','1Ti','2Ti','Tit','Phm','Heb','Jas','1Pe','2Pe','1Jo','2Jo','3Jo','Jud','Rev'
  );
  PtAliases: array[1..66] of string = (
    'Gn','Ex','Lv','Nm','Dt','Js','Jz','Rt','1Sm','2Sm','1Rs','2Rs','1Cr','2Cr','Ed','Ne','Et','Jó','Sl','Pv','Ec','Ct','Is','Jr','Lm','Ez','Dn','Os','Jl','Am','Ob','Jn','Mq','Na','Hc','Sf','Ag','Zc','Ml','Mt','Mc','Lc','Jo','At','Rm','1Co','2Co','Gl','Ef','Fp','Cl','1Ts','2Ts','1Tm','2Tm','Tt','Fm','Hb','Tg','1Pe','2Pe','1Jo','2Jo','3Jo','Jd','Ap'
  );
var
  k: Integer;
begin
  if BooksMap = nil then
    BooksMap := TBooksMap.Create;

  for k := 1 to 66 do
  begin
    BooksMap[EngAliases[k]] := IntToStr(k);
    BooksMap[PtAliases[k]] := IntToStr(k);
  end;
  BooksMap['Ps'] := '19';
  BooksMap['Psalm'] := '19';
  BooksMap['Psalms'] := '19';
  BooksMap['Song'] := '22';
  BooksMap['Matt'] := '40';
  BooksMap['Mark'] := '41';
  BooksMap['Luke'] := '42';
  BooksMap['John'] := '43';
  BooksMap['Acts'] := '44';
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

function FindTheWordEnumProc(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  className, windowTitle: array[0..255] of Char;
  classStr, titleStr: string;
begin
  Result := True; // Continue enumeration

  if not IsWindowVisible(hwnd) then
    Exit;

  GetClassName(hwnd, className, 256);
  classStr := LowerCase(className);

  // theWord registers its window class with 'theWord.' prefix or contains 'theword'
  if (Pos('theword.', classStr) = 1) or (Pos('theword', classStr) > 0) then
  begin
    PHandle(Pointer(lParam))^ := hwnd;
    Result := False; // Stop enumeration, found!
    Exit;
  end;

  GetWindowText(hwnd, windowTitle, 256);
  titleStr := LowerCase(windowTitle);
  if (Pos(' - theword', titleStr) > 0) or (Pos('[theword]', titleStr) > 0) then
  begin
    PHandle(Pointer(lParam))^ := hwnd;
    Result := False; // Stop enumeration, found!
    Exit;
  end;
end;

{------------------------------------------------------------------------------}
{ Return 0 if tw is not running, else the HWND of the running instance }
class function TWAutomateUtils.IsTwRunning: THandle;
var
  PrevWindow: THandle;
begin
  // 1. Exact match on standard theWord Unicode class name
  PrevWindow := FindWindow(TW_MAINFORM_CLASSNAME, nil);
  if IsWindow(PrevWindow) and IsWindowVisible(PrevWindow) then
  begin
    Result := PrevWindow;
    Exit;
  end;

  // 2. Exact match without .UnicodeClass
  PrevWindow := FindWindow(TW_MAINFORM_CLASSNAME_NOUNICODE, nil);
  if IsWindow(PrevWindow) and IsWindowVisible(PrevWindow) then
  begin
    Result := PrevWindow;
    Exit;
  end;

  // 3. Dynamic enumeration fallback across running top-level windows
  PrevWindow := 0;
  EnumWindows(@FindTheWordEnumProc, LPARAM(@PrevWindow));
  if IsWindow(PrevWindow) then
  begin
    Result := PrevWindow;
    Exit;
  end;

  // Neither window class was found
  Result := 0;
end;

function QueryFullProcessImageName(hProcess: HANDLE; dwFlags: DWORD; lpExeName: LPTSTR; var lpdwSize: DWORD): BOOL; stdcall; external 'KERNEL32' name 'QueryFullProcessImageNameA';

class function TWAutomateUtils.GetConfigFile: string;
var
  hwnd, hProcess: THandle;
  module: array[0 .. 255] of char;
  pid, size: DWORD;
  appDataPath: string;
begin
  result := '';
  pid := 0;
  hwnd := IsTwRunning;

  if hwnd = 0 then
    Exit;

  size := sizeof(module);
  if GetWindowThreadProcessId(hwnd, pid) <> 0 then
  begin
    // Try PROCESS_QUERY_LIMITED_INFORMATION ($1000) first, then PROCESS_QUERY_INFORMATION ($0400)
    hProcess := OpenProcess($1000, false, pid);
    if hProcess = 0 then
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

  // Fallback to standard AppData theWord location if config.ini not in exe folder
  if (result = '') or not FileExists(result) then
  begin
    appDataPath := GetEnvironmentVariable('APPDATA') + '\theWord\config.ini';
    if FileExists(appDataPath) then
      result := appDataPath;
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
  if BooksMap = nil then
    InitDefaultBooksMap;

  if (langFile = '') or not FileExists(langFile) then
    exit;

  reBook := RegexCreate('^\s*rc_bk_alias3_(\d+)=''([^'']+)''', [rcoUTF8]);

  lines := TStringList.Create;
  try
    lines.LoadFromFile(langFile);
    for i:=0 to lines.Count - 1 do
    begin
      mtBook := reBook.Match(lines[i]);
      if mtBook.Success then
        BooksMap[mtBook.Groups[2].Value] := mtBook.Groups[1].Value;
    end;
  finally
    lines.Free;
  end;
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
            lines.AddText(UTF16ToUTF8(PWideChar(stream.Memory), stream.Size div SizeOf(WideChar)));
          end;
        FILE_ENCODING_UTF8:
          begin
            lines.LoadFromFile(configFile);
          end;
        else
          begin
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
          if not FileExists(result) then
            result := format('%slanguages\%s.lng', [ExtractFilePath(configFile), matches.Groups[1].Value]);
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
  if (Pos('theword', LowerCase(classStr)) > 0) or
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
    WriteLn(f, 'Current MAINFORM_CLASSNAME constant: ', TW_MAINFORM_CLASSNAME);
    WriteLn(f, StringOfChar('=', 50));
    WriteLn(f);

    // Enumerate all windows
    EnumWindows(@EnumWindowsProc, LPARAM(@f));

    WriteLn(f);
    WriteLn(f, 'End of Window List');
  finally
    CloseFile(f);
  end;
end;

end.
