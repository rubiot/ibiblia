unit twAutomate;

interface

uses PCRE, sysutils, classes, gmap, gutil, Dialogs, Windows, LazUTF8, Controls, syncobjs;

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
    class function BookNameToNumber(const ABookName: string): string;
    class procedure LoadLangFile(langFile: string);
    class procedure InitDefaultBooksMap;
    class function GetConfigFile: string;
    class function GetCurrentLangFile(configFile: string): string;
    class procedure FindTheWordWindows(outputFile: string);
    class procedure LoadTheWordFonts;
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
  BooksMapLock: syncobjs.TCriticalSection;
  IgnoreErrors: boolean;

resourcestring
  STWSynchronizationErrorTitle = 'Synchronization error';
  STWSynchronizationError = 'Failed to detect theWord''s current verse.'#13#10'Please restart theWord if you have switched its current language.';

implementation

function AddFontResourceExW(name: LPCWSTR; fl: DWORD; res: PVOID): Integer; stdcall; external 'gdi32' name 'AddFontResourceExW';

class procedure TWAutomateUtils.Reset;
begin
  IgnoreErrors := false;
end;

class procedure TWAutomateUtils.InitDefaultBooksMap;
  procedure AddB(const AName, ANum: string);
  begin
    if (AName <> '') and (BooksMap.Find(AName) = nil) then
      BooksMap[AName] := ANum;
  end;
begin
  if BooksMap = nil then
    BooksMap := TBooksMap.Create;

  // Law (1..5)
  AddB('Genesis','1'); AddB('Gen','1'); AddB('Ge','1'); AddB('Gn','1'); AddB('Gênesis','1'); AddB('Genesis','1');
  AddB('Exodus','2'); AddB('Exo','2'); AddB('Ex','2'); AddB('Êxodo','2'); AddB('Exodo','2');
  AddB('Leviticus','3'); AddB('Lev','3'); AddB('Le','3'); AddB('Lv','3'); AddB('Levítico','3'); AddB('Levitico','3');
  AddB('Numbers','4'); AddB('Num','4'); AddB('Nu','4'); AddB('Nm','4'); AddB('Números','4'); AddB('Numeros','4');
  AddB('Deuteronomy','5'); AddB('Deu','5'); AddB('Dt','5'); AddB('Deuteronômio','5'); AddB('Deuteronomio','5');

  // History (6..17)
  AddB('Joshua','6'); AddB('Jos','6'); AddB('Js','6'); AddB('Josué','6'); AddB('Josue','6');
  AddB('Judges','7'); AddB('Jdg','7'); AddB('Jg','7'); AddB('Jz','7'); AddB('Juízes','7'); AddB('Juizes','7');
  AddB('Ruth','8'); AddB('Rut','8'); AddB('Ru','8'); AddB('Rt','8'); AddB('Rute','8');
  AddB('1 Samuel','9'); AddB('1Sa','9'); AddB('1 Sam','9'); AddB('1Sam','9'); AddB('1S','9'); AddB('1Sm','9');
  AddB('2 Samuel','10'); AddB('2Sa','10'); AddB('2 Sam','10'); AddB('2Sam','10'); AddB('2S','10'); AddB('2Sm','10');
  AddB('1 Kings','11'); AddB('1Ki','11'); AddB('1 King','11'); AddB('1Kgs','11'); AddB('1Rs','11'); AddB('1 Reis','11');
  AddB('2 Kings','12'); AddB('2Ki','12'); AddB('2 King','12'); AddB('2Kgs','12'); AddB('2Rs','12'); AddB('2 Reis','12');
  AddB('1 Chronicles','13'); AddB('1Ch','13'); AddB('1 Chron','13'); AddB('1Chr','13'); AddB('1Cr','13'); AddB('1 Crônicas','13'); AddB('1 Cronicas','13');
  AddB('2 Chronicles','14'); AddB('2Ch','14'); AddB('2 Chron','14'); AddB('2Chr','14'); AddB('2Cr','14'); AddB('2 Crônicas','14'); AddB('2 Cronicas','14');
  AddB('Ezra','15'); AddB('Ezr','15'); AddB('Ed','15'); AddB('Esdras','15');
  AddB('Nehemiah','16'); AddB('Neh','16'); AddB('Ne','16'); AddB('Neemias','16');
  AddB('Esther','17'); AddB('Est','17'); AddB('Es','17'); AddB('Et','17'); AddB('Ester','17');

  // Poetry (18..22)
  AddB('Job','18'); AddB('Jb','18'); AddB('Jó','18'); AddB('Jo','18');
  AddB('Psalms','19'); AddB('Psalm','19'); AddB('Psa','19'); AddB('Ps','19'); AddB('Sl','19'); AddB('Salmos','19'); AddB('Salmo','19');
  AddB('Proverbs','20'); AddB('Pro','20'); AddB('Pr','20'); AddB('Pv','20'); AddB('Provérbios','20'); AddB('Proverbios','20');
  AddB('Ecclesiastes','21'); AddB('Ecc','21'); AddB('Ec','21'); AddB('Qoh','21'); AddB('Eclesiastes','21');
  AddB('Song of Solomon','22'); AddB('Song of Songs','22'); AddB('Song','22'); AddB('Sng','22'); AddB('So','22'); AddB('Canticles','22'); AddB('Ct','22'); AddB('Cânticos','22'); AddB('Canticos','22'); AddB('Cantares','22');

  // Major Prophets (23..27)
  AddB('Isaiah','23'); AddB('Isa','23'); AddB('Is','23'); AddB('Isaías','23'); AddB('Isaias','23');
  AddB('Jeremiah','24'); AddB('Jer','24'); AddB('Jr','24'); AddB('Jeremias','24');
  AddB('Lamentations','25'); AddB('Lam','25'); AddB('La','25'); AddB('Lm','25'); AddB('Lamentações','25'); AddB('Lamentacoes','25');
  AddB('Ezekiel','26'); AddB('Ezk','26'); AddB('Eze','26'); AddB('Ez','26'); AddB('Ezequiel','26');
  AddB('Daniel','27'); AddB('Dan','27'); AddB('Da','27'); AddB('Dn','27');

  // Minor Prophets (28..39)
  AddB('Hosea','28'); AddB('Hos','28'); AddB('Ho','28'); AddB('Os','28'); AddB('Oséias','28'); AddB('Oseias','28');
  AddB('Joel','29'); AddB('Joe','29'); AddB('Jl','29');
  AddB('Amos','30'); AddB('Amo','30'); AddB('Am','30'); AddB('Amós','30');
  AddB('Obadiah','31'); AddB('Oba','31'); AddB('Ob','31'); AddB('Obadias','31');
  AddB('Jonah','32'); AddB('Jon','32'); AddB('Jnh','32'); AddB('Jn','32'); AddB('Jonas','32');
  AddB('Micah','33'); AddB('Mic','33'); AddB('Mc','33'); AddB('Mq','33'); AddB('Miquéias','33'); AddB('Miqueias','33');
  AddB('Nahum','34'); AddB('Nah','34'); AddB('Na','34'); AddB('Naum','34');
  AddB('Habakkuk','35'); AddB('Hab','35'); AddB('Hb','35'); AddB('Hc','35'); AddB('Habacuque','35');
  AddB('Zephaniah','36'); AddB('Zep','36'); AddB('Zp','36'); AddB('Sf','36'); AddB('Sofonias','36');
  AddB('Haggai','37'); AddB('Hag','37'); AddB('Hg','37'); AddB('Ag','37'); AddB('Ageu','37');
  AddB('Zechariah','38'); AddB('Zec','38'); AddB('Zc','38'); AddB('Zacarias','38');
  AddB('Malachi','39'); AddB('Mal','39'); AddB('Ml','39'); AddB('Malaquias','39');

  // Gospels & Acts (40..44)
  AddB('Matthew','40'); AddB('Mat','40'); AddB('Matt','40'); AddB('Mt','40'); AddB('Mateus','40');
  AddB('Mark','41'); AddB('Mar','41'); AddB('Mk','41'); AddB('Mrk','41'); AddB('Mc','41'); AddB('Marcos','41');
  AddB('Luke','42'); AddB('Luk','42'); AddB('Lk','42'); AddB('Lc','42'); AddB('Lucas','42');
  AddB('John','43'); AddB('Joh','43'); AddB('Jn','43'); AddB('Jhn','43'); AddB('Jo','43'); AddB('João','43'); AddB('Joao','43');
  AddB('Acts','44'); AddB('Act','44'); AddB('Ac','44'); AddB('At','44'); AddB('Atos','44');

  // Paul (45..58)
  AddB('Romans','45'); AddB('Rom','45'); AddB('Ro','45'); AddB('Rm','45'); AddB('Romanos','45');
  AddB('1 Corinthians','46'); AddB('1Co','46'); AddB('1 Cor','46'); AddB('1Cor','46'); AddB('1 Coríntios','46'); AddB('1 Corintios','46');
  AddB('2 Corinthians','47'); AddB('2Co','47'); AddB('2 Cor','47'); AddB('2Cor','47'); AddB('2 Coríntios','47'); AddB('2 Corintios','47');
  AddB('Galatians','48'); AddB('Gal','48'); AddB('Ga','48'); AddB('Gl','48'); AddB('Gálatas','48'); AddB('Galatas','48');
  AddB('Ephesians','49'); AddB('Eph','49'); AddB('Ep','49'); AddB('Ef','49'); AddB('Efésios','49'); AddB('Efesios','49');
  AddB('Philippians','50'); AddB('Php','50'); AddB('Phil','50'); AddB('Fp','50'); AddB('Filipenses','50');
  AddB('Colossians','51'); AddB('Col','51'); AddB('Cl','51'); AddB('Colossenses','51');
  AddB('1 Thessalonians','52'); AddB('1Th','52'); AddB('1 Thess','52'); AddB('1Thess','52'); AddB('1Ts','52'); AddB('1 Tessalonicenses','52');
  AddB('2 Thessalonians','53'); AddB('2Th','53'); AddB('2 Thess','53'); AddB('2Thess','53'); AddB('2Ts','53'); AddB('2 Tessalonicenses','53');
  AddB('1 Timothy','54'); AddB('1Ti','54'); AddB('1 Tim','54'); AddB('1Tim','54'); AddB('1Tm','54'); AddB('1 Timóteo','54'); AddB('1 Timoteo','54');
  AddB('2 Timothy','55'); AddB('2Ti','55'); AddB('2 Tim','55'); AddB('2Tim','55'); AddB('2Tm','55'); AddB('2 Timóteo','55'); AddB('2 Timoteo','55');
  AddB('Titus','56'); AddB('Tit','56'); AddB('Ti','56'); AddB('Tt','56'); AddB('Tito','56');
  AddB('Philemon','57'); AddB('Phm','57'); AddB('Philem','57'); AddB('Fm','57'); AddB('Filemom','57'); AddB('Filemon','57');
  AddB('Hebrews','58'); AddB('Heb','58'); AddB('He','58'); AddB('Hb','58'); AddB('Hebreus','58');

  // General Epistles & Revelation (59..66)
  AddB('James','59'); AddB('Jas','59'); AddB('Ja','59'); AddB('Tg','59'); AddB('Tiago','59');
  AddB('1 Peter','60'); AddB('1Pe','60'); AddB('1 Pet','60'); AddB('1Pet','60'); AddB('1P','60'); AddB('1 Pedro','60');
  AddB('2 Peter','61'); AddB('2Pe','61'); AddB('2 Pet','61'); AddB('2Pet','61'); AddB('2P','61'); AddB('2 Pedro','61');
  AddB('1 John','62'); AddB('1Jo','62'); AddB('1 Joh','62'); AddB('1Jn','62'); AddB('1Jhn','62'); AddB('1J','62'); AddB('1 João','62'); AddB('1 Joao','62');
  AddB('2 John','63'); AddB('2Jo','63'); AddB('2 Joh','63'); AddB('2Jn','63'); AddB('2Jhn','63'); AddB('2J','63'); AddB('2 João','63'); AddB('2 Joao','63');
  AddB('3 John','64'); AddB('3Jo','64'); AddB('3 Joh','64'); AddB('3Jn','64'); AddB('3Jhn','64'); AddB('3J','64'); AddB('3 João','64'); AddB('3 Joao','64');
  AddB('Jude','65'); AddB('Jud','65'); AddB('Jd','65'); AddB('Judas','65');
  AddB('Revelation','66'); AddB('Rev','66'); AddB('Re','66'); AddB('Ap','66'); AddB('Apocalipse','66'); AddB('Apocalypse','66');
end;

class function TWAutomateUtils.BookNameToNumber(const ABookName: string): string;
var
  cleanName: string;
begin
  Result := '';
  cleanName := Trim(ABookName);
  if cleanName = '' then Exit;

  if BooksMapLock = nil then
    BooksMapLock := syncobjs.TCriticalSection.Create;

  BooksMapLock.Enter;
  try
    if BooksMap = nil then
      InitDefaultBooksMap;

    if (BooksMap <> nil) and (BooksMap.Find(cleanName) <> nil) then
      Result := BooksMap[cleanName];
  finally
    BooksMapLock.Leave;
  end;
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

  FillChar(className, SizeOf(className), 0);
  FillChar(windowTitle, SizeOf(windowTitle), 0);

  GetClassName(hwnd, className, 256);
  classStr := LowerCase(className);

  // theWord registers its window class with 'theWord.' prefix or contains 'theword'
  if (Pos('theword.', classStr) = 1) or (Pos('theword', classStr) > 0) then
  begin
    PHandle(Pointer(PtrUInt(lParam)))^ := hwnd;
    Result := False; // Stop enumeration, found!
    Exit;
  end;

  GetWindowText(hwnd, windowTitle, 256);
  titleStr := LowerCase(windowTitle);
  if (Pos(' - theword', titleStr) > 0) or (Pos('[theword]', titleStr) > 0) then
  begin
    PHandle(Pointer(PtrUInt(lParam)))^ := hwnd;
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
  EnumWindows(@FindTheWordEnumProc, LPARAM(PtrUInt(@PrevWindow)));
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
    appDataPath := SysUtils.GetEnvironmentVariable('APPDATA') + '\theWord\config.ini';
    if FileExists(appDataPath) then
      result := appDataPath;
  end;
end;

class function TWAutomateUtils.GetTwCurrentRef: string;

  function ExtractRefStr(const caption: string): string;
  var
    reRef: IRegex;
    mtRef: IMatch;
    bName, bNum: string;
  begin
    result := '';
    try
      reRef := RegexCreate('^(.*?) (\d+):(\d+)', [rcoUTF8]);
      mtRef := reRef.Match(caption);

      if not mtRef.Success then exit;

      bName := Trim(mtRef.Groups[1].Value);
      bNum := BookNameToNumber(bName);

      if bNum = '' then
      begin
        // If not recognized, attempt quiet lang file load without throwing or showing dialogs
        LoadLangFile(GetCurrentLangFile(GetConfigFile));
        bNum := BookNameToNumber(bName);
      end;

      if bNum <> '' then
        result := format('%s,%s,%s', [bNum, mtRef.Groups[2].Value, mtRef.Groups[3].Value]);
    except
      result := '';
    end;
  end;

var
  hwnd: THandle;
  caption: array[0 .. 255] of char;
  refStr: string;
begin
  result := '';
  try
    hwnd := IsTwRunning;
    if hwnd <> 0 then
    begin
      FillChar(caption, SizeOf(caption), 0);
      GetWindowText(hwnd, caption, SizeOf(caption));
      refStr := caption;
      result := ExtractRefStr(refStr);
    end;
  except
    result := '';
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

  // Match book name, short name, and all aliases from theWord .lng file
  reBook := RegexCreate('^\s*rc_bk_(?:name|sname|alias\d*)_(\d+)=''([^'']+)''', [rcoUTF8]);

  lines := TStringList.Create;
  try
    lines.LoadFromFile(langFile);
    if BooksMapLock <> nil then BooksMapLock.Enter;
    try
      for i:=0 to lines.Count - 1 do
      begin
        mtBook := reBook.Match(lines[i]);
        if mtBook.Success and (BooksMap <> nil) then
          BooksMap[mtBook.Groups[2].Value] := mtBook.Groups[1].Value;
      end;
    finally
      if BooksMapLock <> nil then BooksMapLock.Leave;
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

  FillChar(className, SizeOf(className), 0);
  FillChar(windowTitle, SizeOf(windowTitle), 0);

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
    fPtr := Pointer(PtrUInt(lParam));
    WriteLn(fPtr^, 'Window Handle: ', hwnd);
    WriteLn(fPtr^, 'Class Name: ', classStr);
    WriteLn(fPtr^, 'Window Title: ', titleStr);
    WriteLn(fPtr^, StringOfChar('-', 50));
  end;
end;

class procedure TWAutomateUtils.LoadTheWordFonts;
var
  dirs: TStringList;
  d, ext, fName, appData, localAppData, progData, progFiles, progFilesX86: string;
  info: TSearchRec;
begin
  dirs := TStringList.Create;
  try
    try
      d := ExtractFilePath(GetConfigFile);
      if d <> '' then
      begin
        dirs.Add(d + 'fonts\');
        dirs.Add(d);
      end;
    except
    end;

    appData := SysUtils.GetEnvironmentVariable('APPDATA');
    if appData <> '' then
    begin
      dirs.Add(appData + '\theWord\fonts\');
      dirs.Add(appData + '\theWord\');
    end;

    localAppData := SysUtils.GetEnvironmentVariable('LOCALAPPDATA');
    if localAppData <> '' then
      dirs.Add(localAppData + '\theWord\fonts\');

    progData := SysUtils.GetEnvironmentVariable('PROGRAMDATA');
    if progData <> '' then
      dirs.Add(progData + '\theWord\fonts\');

    progFilesX86 := SysUtils.GetEnvironmentVariable('ProgramFiles(x86)');
    if progFilesX86 <> '' then
      dirs.Add(progFilesX86 + '\The Word\fonts\');

    progFiles := SysUtils.GetEnvironmentVariable('ProgramFiles');
    if progFiles <> '' then
      dirs.Add(progFiles + '\The Word\fonts\');

    dirs.Add('C:\theWord\fonts\');
    dirs.Add('C:\theWord\');
    dirs.Add(ExtractFilePath(ParamStr(0)) + 'fonts\');

    for d in dirs do
    begin
      if DirectoryExists(d) then
      begin
        if FindFirst(d + '*.*', faAnyFile, info) = 0 then
        begin
          repeat
            if (info.Attr and faDirectory) = 0 then
            begin
              ext := LowerCase(ExtractFileExt(info.Name));
              if (ext = '.ttf') or (ext = '.otf') or (ext = '.ttc') then
              begin
                fName := d + info.Name;
                try
                  AddFontResourceExW(PWideChar(UTF8Decode(fName)), $10, nil);
                except
                end;
              end;
            end;
          until FindNext(info) <> 0;
          SysUtils.FindClose(info);
        end;
      end;
    end;
  finally
    dirs.Free;
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
    EnumWindows(@EnumWindowsProc, LPARAM(PtrUInt(@f)));

    WriteLn(f);
    WriteLn(f, 'End of Window List');
  finally
    CloseFile(f);
  end;
end;

initialization
  BooksMapLock := syncobjs.TCriticalSection.Create;

finalization
  if BooksMapLock <> nil then
  begin
    BooksMapLock.Free;
    BooksMapLock := nil;
  end;
  if BooksMap <> nil then
  begin
    BooksMap.Free;
    BooksMap := nil;
  end;

end.
