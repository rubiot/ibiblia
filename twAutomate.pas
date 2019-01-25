unit twAutomate;

interface

uses PCRE, sysutils, classes, gmap, gutil, Dialogs, Windows, LazUTF8;

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
    class function IsTwRunning: THandle;
    class function GetTwCurrentRef: string;
    class procedure LoadLangFile(langFile: string);
    class function GetConfigFile: string;
    class function GetCurrentLangFile(configFile: string): string;
  end;

const
  //the classname of the main theWord window.
  MAINFORM_CLASSNAME = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a66';
  //the value of COPYDATASTRUCT.dwData
  COPYDATA_OP_FIRST = $ffff0000;
  //Operation go-to-verse
  COPYDATA_OP_GOTOVERSE = COPYDATA_OP_FIRST + 1;
  COPYDATA_OP_DCTWORDLOOKUP = COPYDATA_OP_FIRST + 2;

type
  TStrLess = specialize TLess<string>;
  TBooksMap = specialize TMap<string, string, TStrLess>;

var
  BooksMap: TBooksMap;

implementation

{------------------------------------------------------------------------------}
{ Return 0 if tw is not running, else the HWND of the running instance }
class function TWAutomateUtils.IsTwRunning: THandle;
var
  PrevWindow: THandle;
begin
  PrevWindow := FindWindow(MAINFORM_CLASSNAME + '.UnicodeClass', nil);
  if IsWindow(PrevWindow) then begin
    Result := PrevWindow
  end
  else begin
    Result := 0;
  end;
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
  size := sizeof(module);
  if GetWindowThreadProcessId(hwnd, pid) <> 0 then
  begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, false, pid);
    if QueryFullProcessImageName(hProcess, 0, module, size) then
    begin
      result := module;
      result := ExtractFilePath(result) + 'config.ini';
      exit;
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
    if mtRef.Success then
      result := format('%s,%s,%s', [BooksMap[mtRef.Groups[1].Value], mtRef.Groups[2].Value, mtRef.Groups[3].Value])
  end;

var
  hwnd: THandle;
  caption: array[0 .. 255] of char;
  refStr: string;
begin
  result := '';
  if BooksMap = nil then
  begin
    LoadLangFile(GetCurrentLangFile(GetConfigFile));
    exit;
  end;
  hwnd := IsTwRunning;
  GetWindowText(hwnd, caption, SizeOf(caption));
  refStr := caption;
  result := ExtractRefStr(refStr);
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
begin
  result := '';
  if configFile = '' then
    exit;

  { o config do TheWord é codificado em UTF-16... }
  stream := TMemoryStream.Create;
  try
    stream.LoadFromFile(configFile);
    stream.Position := 0;
    lines := TStringList.Create;
    lines.AddText(UTF16ToUTF8(PWideChar(stream.Memory), stream.Size div SizeOf(WideChar)));
  finally
    stream.Free;
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
  lines.Free;
end;

end.
