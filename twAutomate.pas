unit twAutomate;

interface

uses
  PCRE, sysutils, classes, gmap, gutil, Dialogs,
  {$IFDEF WINDOWS}Windows,{$ELSE}BaseUnix, dynlibs,{$ENDIF}
  LCLType, LazUTF8, Controls, syncobjs;

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

  // Explicit Win32/Win64 C-ABI COPYDATASTRUCT layout.
  // LCLType.TCopyDataStruct is a 'packed record' (20 bytes on Win64, lpData at
  // offset 12), but WM_COPYDATA on 64-bit Windows expects natural C alignment
  // (24 bytes, 4 bytes of padding after cbData, lpData at offset 16).
  TTWCopyDataStruct = packed record
    dwData: PtrUInt;
    cbData: DWORD;
    {$IFDEF CPU64}
    ReservedPadding: DWORD;
    {$ENDIF}
    lpData: Pointer;
  end;

  { TWAutomateUtils }

  TWAutomateUtils = class
    class procedure Reset;
    class function IsTwRunning: THandle;
    class procedure BroadcastCopyDataToTheWord(senderHwnd: THandle; var CData: TTWCopyDataStruct);
    class function GetTwCurrentRef: string;
    class function BookNameToNumber(const ABookName: string): string;
    class procedure InitDefaultBooksMap;
    class procedure LoadLangFile(langFile: string);
    class function GetConfigFile: string;
    class function GetCurrentLangFile(configFile: string): string;
    class procedure FindTheWordWindows(outputFile: string);
    class procedure LoadTheWordFonts;
    class function FindWindowTitleContaining(const ASubstring: string): string;
  end;

const
  //the classname of the main theWord window.
  TW6_MAINFORM_CLASSNAME = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a667.UnicodeClass';
  TW7_MAINFORM_CLASSNAME = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a66';
  //Unicode variants registered by some theWord 6/7 builds (32- and 64-bit)
  TW6_MAINFORM_CLASSNAME_NOUNICODE = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a667';
  TW7_MAINFORM_CLASSNAME_UNICODE = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a66.UnicodeClass';
  //common prefix of every theWord main window class
  TW_MAINFORM_CLASS_PREFIX = 'theword.0f2ba8a0';
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
  BooksMapLock: TCriticalSection;
  IgnoreErrors: boolean;

resourcestring
  STWSynchronizationErrorTitle = 'Synchronization error';
  STWSynchronizationError = 'Failed to detect theWord''s current verse.'#13#10'Please restart theWord if you have switched its current language.';
  SRegexErrorFmt = 'Error in regex: %s';
  STWWindowNotFoundDebugSavedFmt = 'theWord window not found. Debug information has been saved to: %s';

implementation

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
  AddB('Genesis','1'); AddB('Gen','1'); AddB('Ge','1'); AddB('Gn','1'); AddB('Gênesis','1');
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
  AddB('Job','18'); AddB('Jb','18'); AddB('Jó','18');
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
  AddB('Micah','33'); AddB('Mic','33'); AddB('Mq','33'); AddB('Miquéias','33'); AddB('Miqueias','33');
  AddB('Nahum','34'); AddB('Nah','34'); AddB('Na','34'); AddB('Naum','34');
  AddB('Habakkuk','35'); AddB('Hab','35'); AddB('Hc','35'); AddB('Habacuque','35');
  AddB('Zephaniah','36'); AddB('Zep','36'); AddB('Zp','36'); AddB('Sf','36'); AddB('Sofonias','36');
  AddB('Haggai','37'); AddB('Hag','37'); AddB('Hg','37'); AddB('Ag','37'); AddB('Ageu','37');
  AddB('Zechariah','38'); AddB('Zec','38'); AddB('Zc','38'); AddB('Zacarias','38');
  AddB('Malachi','39'); AddB('Mal','39'); AddB('Ml','39'); AddB('Malaquias','39');

  // Gospels & Acts (40..44)
  AddB('Matthew','40'); AddB('Mat','40'); AddB('Matt','40'); AddB('Mt','40'); AddB('Mateus','40');
  AddB('Mark','41'); AddB('Mar','41'); AddB('Mk','41'); AddB('Mrk','41'); AddB('Mc','41'); AddB('Marcos','41');
  AddB('Luke','42'); AddB('Luk','42'); AddB('Lk','42'); AddB('Lc','42'); AddB('Lucas','42');
  AddB('John','43'); AddB('Joh','43'); AddB('Jhn','43'); AddB('Jo','43'); AddB('João','43'); AddB('Joao','43');
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
    BooksMapLock := TCriticalSection.Create;

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

class procedure TWAutomateUtils.LoadLangFile(langFile: string);
var
  lines: TStringList;
  i: integer;
  reBook: IRegex;
  mtBook: IMatch;
begin
  if BooksMapLock = nil then
    BooksMapLock := TCriticalSection.Create;

  BooksMapLock.Enter;
  try
    if BooksMap = nil then
      InitDefaultBooksMap;
  finally
    BooksMapLock.Leave;
  end;

  if (langFile = '') or not FileExists(langFile) then
    exit;

  reBook := RegexCreate('^\s*rc_bk_(?:name|sname|alias\d*)_(\d+)=''([^'']+)''', [rcoUTF8]);

  lines := TStringList.Create;
  try
    lines.LoadFromFile(langFile);
    BooksMapLock.Enter;
    try
      for i:=0 to lines.Count - 1 do
      begin
        mtBook := reBook.Match(lines[i]);
        if mtBook.Success and (BooksMap <> nil) then
          BooksMap[mtBook.Groups[2].Value] := mtBook.Groups[1].Value;
      end;
    finally
      BooksMapLock.Leave;
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
          if not FileExists(result) then
            result := format('%slanguages' + PathDelim + '%s.lng', [ExtractFilePath(configFile), matches.Groups[1].Value]);
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

function ExtractTheWordRefFromCaption(const caption: string): string;
var
  reRef: IRegex;
  mtRef: IMatch;
  bName, bNum: string;
begin
  Result := '';
  if Trim(caption) = '' then
    Exit;
  try
    reRef := RegexCreate('^(.*?) (\d+):(\d+)', [rcoUTF8]);
    mtRef := reRef.Match(caption);
  except
    on E: Exception do
    begin
      if E.Message = 'PCRE_ERROR_BADUTF8' then
      begin // fallback for bad UTF-8 sequences
        reRef := RegexCreate('^(.*?) (\d+):(\d+)', []);
        mtRef := reRef.Match(caption);
      end
      else
        Exit;
    end;
  end;

  if not mtRef.Success then
    Exit;

  bName := Trim(mtRef.Groups[1].Value);
  bNum := TWAutomateUtils.BookNameToNumber(bName);

  if bNum = '' then
  begin
    TWAutomateUtils.LoadLangFile(TWAutomateUtils.GetCurrentLangFile(TWAutomateUtils.GetConfigFile));
    bNum := TWAutomateUtils.BookNameToNumber(bName);
  end;

  if bNum = '' then
  begin
    if not IgnoreErrors and (MessageDlg(STWSynchronizationErrorTitle, STWSynchronizationError, mtError, [mbOK, mbIgnore], 0) = mrIgnore) then
      IgnoreErrors := true;
    Exit;
  end;

  Result := Format('%s,%s,%s', [bNum, mtRef.Groups[2].Value, mtRef.Groups[3].Value]);
end;

{$IFDEF WINDOWS}
function FindTheWordByClassEnumProc(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  className: array[0..255] of Char;
  pid: DWORD;
begin
  Result := True; // continue enumeration
  pid := 0;
  GetWindowThreadProcessId(hwnd, pid);
  if pid = GetCurrentProcessId then
    Exit;
  FillChar(className, SizeOf(className), 0);
  GetClassName(hwnd, className, Length(className));
  if Pos(TW_MAINFORM_CLASS_PREFIX, LowerCase(StrPas(className))) = 1 then
  begin
    PHandle(Pointer(PtrUInt(lParam)))^ := hwnd;
    if IsWindowVisible(hwnd) then
      Result := False; // stop at the first visible main form
  end;
end;

{------------------------------------------------------------------------------}
{ Return 0 if tw is not running, else the HWND of the running instance }
class function TWAutomateUtils.IsTwRunning: THandle;
const
  KNOWN_CLASSES: array[0..3] of PChar = (
    TW7_MAINFORM_CLASSNAME,
    TW7_MAINFORM_CLASSNAME_UNICODE,
    TW6_MAINFORM_CLASSNAME,
    TW6_MAINFORM_CLASSNAME_NOUNICODE
  );
var
  PrevWindow, HiddenMatch: THandle;
  i: Integer;
begin
  HiddenMatch := 0;
  // 1. known class names; prefer a visible (or minimized) main window
  for i := Low(KNOWN_CLASSES) to High(KNOWN_CLASSES) do
  begin
    PrevWindow := FindWindow(KNOWN_CLASSES[i], nil);
    if IsWindow(PrevWindow) then
    begin
      if IsWindowVisible(PrevWindow) or IsIconic(PrevWindow) then
      begin
        Result := PrevWindow;
        Exit;
      end;
      if HiddenMatch = 0 then
        HiddenMatch := PrevWindow;
    end;
  end;

  // 2. any window whose class starts with the theWord prefix
  PrevWindow := 0;
  EnumWindows(@FindTheWordByClassEnumProc, PtrInt(@PrevWindow));
  if IsWindow(PrevWindow) then
  begin
    Result := PrevWindow;
    Exit;
  end;

  if IsWindow(HiddenMatch) then
  begin
    Result := HiddenMatch;
    Exit;
  end;

  // Neither window class was found
  Result := 0;
end;

type
  TTWBroadcastCtx = record
    TargetPid: DWORD;
    PrimaryHwnd: THandle;
    SenderHwnd: THandle;
    CopyDataAddr: PtrInt;
  end;
  PTWBroadcastCtx = ^TTWBroadcastCtx;

procedure SendCopyData(hwnd, senderHwnd: THandle; copyDataAddr: PtrInt);
var
  dwRes: DWORD_PTR;
begin
  dwRes := 0;
  if SendMessageTimeout(hwnd, WM_COPYDATA, WPARAM(senderHwnd), copyDataAddr,
     SMTO_ABORTIFHUNG or SMTO_NORMAL, 1000, @dwRes) = 0 then
    SendMessage(hwnd, WM_COPYDATA, WPARAM(senderHwnd), copyDataAddr);
end;

function BroadcastTheWordEnumProc(hwnd: HWND; ALParam: LPARAM): BOOL; stdcall;
var
  ctx: PTWBroadcastCtx;
  pid: DWORD;
  className: array[0..255] of Char;
begin
  Result := True;
  ctx := PTWBroadcastCtx(ALParam);
  if (ctx = nil) or (hwnd = 0) or (hwnd = ctx^.PrimaryHwnd) then
    Exit;
  pid := 0;
  GetWindowThreadProcessId(hwnd, pid);
  if (pid = 0) or (pid <> ctx^.TargetPid) then
    Exit;
  FillChar(className, SizeOf(className), 0);
  GetClassName(hwnd, className, Length(className));
  if Pos(TW_MAINFORM_CLASS_PREFIX, LowerCase(StrPas(className))) = 1 then
    SendCopyData(hwnd, ctx^.SenderHwnd, ctx^.CopyDataAddr);
end;

{ Sends CData to theWord's main window and to every other theWord main window
  of the same process (theWord may keep a hidden twin of its main form). }
class procedure TWAutomateUtils.BroadcastCopyDataToTheWord(senderHwnd: THandle; var CData: TTWCopyDataStruct);
var
  twHWND: THandle;
  pid: DWORD;
  ctx: TTWBroadcastCtx;
begin
  twHWND := IsTwRunning;
  if twHWND = 0 then
    Exit;

  SendCopyData(twHWND, senderHwnd, PtrInt(@CData));

  pid := 0;
  GetWindowThreadProcessId(twHWND, pid);
  if (pid <> 0) and (pid <> GetCurrentProcessId) then
  begin
    ctx.TargetPid := pid;
    ctx.PrimaryHwnd := twHWND;
    ctx.SenderHwnd := senderHwnd;
    ctx.CopyDataAddr := PtrInt(@CData);
    EnumWindows(@BroadcastTheWordEnumProc, PtrInt(@ctx));
  end;
end;

function AddFontResourceExW(name: LPCWSTR; fl: DWORD; res: PVOID): Integer; stdcall; external 'gdi32' name 'AddFontResourceExW';

const
  FR_PRIVATE = $10;

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

  if (result = '') or not FileExists(result) then
  begin
    appDataPath := SysUtils.GetEnvironmentVariable('APPDATA') + '\theWord\config.ini';
    if FileExists(appDataPath) then
      result := appDataPath;
  end;
end;

class function TWAutomateUtils.GetTwCurrentRef: string;
var
  hwnd: THandle;
  caption: array[0 .. 255] of char;
  refStr: string;
begin
  result := '';
  hwnd := IsTwRunning;
  if hwnd <> 0 then
  begin
    FillChar(caption, SizeOf(caption), 0);
    GetWindowText(hwnd, caption, SizeOf(caption));
    refStr := caption;
    result := ExtractTheWordRefFromCaption(refStr);
  end;
end;

function EnumWindowsProc(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  className, windowTitle: array[0..255] of Char;
  classStr, titleStr: string;
  fPtr: ^TextFile;
begin
  Result := True; // Continue enumeration

  GetClassName(hwnd, className, 256);
  classStr := className;

  GetWindowText(hwnd, windowTitle, 256);
  titleStr := windowTitle;

  if (Pos('theWord', LowerCase(classStr)) > 0) or
     (Pos('theword', LowerCase(titleStr)) > 0) then
  begin
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

    EnumWindows(@EnumWindowsProc, LPARAM(@f));

    WriteLn(f);
    WriteLn(f, 'End of Window List');
  finally
    CloseFile(f);
  end;

  ShowMessage(Format(STWWindowNotFoundDebugSavedFmt, [outputFile]));
end;

{ Makes the fonts that ship with theWord (e.g. Greek/Hebrew fonts used by
  modules and dictionaries) available to iBiblia for this session only.
  Nothing is installed system-wide. }
class procedure TWAutomateUtils.LoadTheWordFonts;
var
  Dirs: TStringList;
  Dir, Ext, EnvValue: string;
  Info: TSearchRec;

  procedure AddDir(const APath: string);
  begin
    if (APath <> '') and (Dirs.IndexOf(IncludeTrailingPathDelimiter(APath)) < 0) then
      Dirs.Add(IncludeTrailingPathDelimiter(APath));
  end;

begin
  Dirs := TStringList.Create;
  try
    try
      Dir := ExtractFilePath(GetConfigFile); // folder of the running theWord
      if Dir <> '' then
      begin
        AddDir(Dir + 'fonts');
        AddDir(Dir);
      end;
    except
    end;

    EnvValue := SysUtils.GetEnvironmentVariable('APPDATA');
    if EnvValue <> '' then
    begin
      AddDir(EnvValue + '\theWord\fonts');
      AddDir(EnvValue + '\theWord');
    end;
    EnvValue := SysUtils.GetEnvironmentVariable('LOCALAPPDATA');
    if EnvValue <> '' then
      AddDir(EnvValue + '\theWord\fonts');
    EnvValue := SysUtils.GetEnvironmentVariable('PROGRAMDATA');
    if EnvValue <> '' then
      AddDir(EnvValue + '\theWord\fonts');
    EnvValue := SysUtils.GetEnvironmentVariable('ProgramFiles(x86)');
    if EnvValue <> '' then
      AddDir(EnvValue + '\The Word\fonts');
    EnvValue := SysUtils.GetEnvironmentVariable('ProgramFiles');
    if EnvValue <> '' then
      AddDir(EnvValue + '\The Word\fonts');
    AddDir('C:\theWord\fonts');
    AddDir('C:\theWord');
    AddDir(ExtractFilePath(ParamStr(0)) + 'fonts');

    for Dir in Dirs do
    begin
      if not DirectoryExists(Dir) then
        Continue;
      if FindFirst(Dir + '*.*', faAnyFile, Info) = 0 then
      try
        repeat
          if (Info.Attr and faDirectory) = 0 then
          begin
            Ext := LowerCase(ExtractFileExt(Info.Name));
            if (Ext = '.ttf') or (Ext = '.otf') or (Ext = '.ttc') then
              AddFontResourceExW(PWideChar(UTF8Decode(Dir + Info.Name)), FR_PRIVATE, nil);
          end;
        until FindNext(Info) <> 0;
      finally
        SysUtils.FindClose(Info);
      end;
    end;
  finally
    Dirs.Free;
  end;
end;

class function TWAutomateUtils.FindWindowTitleContaining(const ASubstring: string): string;
begin
  Result := '';
end;

{$ELSE}

function ReadProcFileText(const FileName: string): string;
var
  Stream: TFileStream;
  Buf: array[0..4095] of Byte;
  Count, I: Integer;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Count := Stream.Read(Buf, SizeOf(Buf));
      if Count > 0 then
      begin
        for I := 0 to Count - 1 do
          if Buf[I] = 0 then
            Buf[I] := Ord(' ');
        SetLength(Result, Count);
        Move(Buf[0], Result[1], Count);
      end;
    finally
      Stream.Free;
    end;
  except
    Result := '';
  end;
end;

class function TWAutomateUtils.IsTwRunning: THandle;
var
  Info: TSearchRec;
  PidInt: Integer;
  MyPid: TPid;
  CommText, CmdText: string;
begin
  Result := 0;
  MyPid := FpGetpid;
  if FindFirst('/proc/*', faDirectory, Info) = 0 then
  try
    repeat
      if (Info.Attr and faDirectory) <> 0 then
      begin
        PidInt := StrToIntDef(Info.Name, 0);
        if (PidInt > 0) and (PidInt <> MyPid) then
        begin
          CommText := LowerCase(Trim(ReadProcFileText('/proc/' + Info.Name + '/comm')));
          if (Pos('theword', CommText) > 0) then
          begin
            Result := System.THandle(PidInt);
            Break;
          end;
          CmdText := LowerCase(Trim(ReadProcFileText('/proc/' + Info.Name + '/cmdline')));
          if (Pos('theword.exe', CmdText) > 0) then
          begin
            Result := System.THandle(PidInt);
            Break;
          end;
        end;
      end;
    until FindNext(Info) <> 0;
  finally
    SysUtils.FindClose(Info);
  end;
end;

class procedure TWAutomateUtils.BroadcastCopyDataToTheWord(senderHwnd: THandle; var CData: TTWCopyDataStruct);
begin
  // WM_COPYDATA is a Windows kernel message; on Linux no-op
end;

class function TWAutomateUtils.GetConfigFile: string;
var
  PidHandle: System.THandle;
  CwdLink, WinePrefix, UserHome, UserName, Cand: string;
  Candidates: TStringList;
  I: Integer;
begin
  Result := '';
  Candidates := TStringList.Create;
  try
    PidHandle := IsTwRunning;
    if PidHandle <> 0 then
    begin
      CwdLink := fpReadLink('/proc/' + IntToStr(PtrUInt(PidHandle)) + '/cwd');
      if CwdLink <> '' then
        Candidates.Add(IncludeTrailingPathDelimiter(CwdLink) + 'config.ini');
    end;

    UserHome := ExcludeTrailingPathDelimiter(GetUserDir);
    UserName := SysUtils.GetEnvironmentVariable('USER');
    if UserName = '' then
      UserName := ExtractFileName(UserHome);

    WinePrefix := Trim(SysUtils.GetEnvironmentVariable('WINEPREFIX'));
    if WinePrefix = '' then
      WinePrefix := UserHome + '/.wine';

    Candidates.Add(UserHome + '/.local/share/theWord/config.ini');
    Candidates.Add(UserHome + '/.config/theWord/config.ini');
    Candidates.Add(WinePrefix + '/drive_c/users/' + UserName + '/AppData/Roaming/theWord/config.ini');
    Candidates.Add(WinePrefix + '/drive_c/users/' + UserName + '/Application Data/theWord/config.ini');
    Candidates.Add(WinePrefix + '/drive_c/Program Files (x86)/The Word/config.ini');
    Candidates.Add(WinePrefix + '/drive_c/Program Files/The Word/config.ini');
    Candidates.Add(WinePrefix + '/drive_c/theWord/config.ini');

    for I := 0 to Candidates.Count - 1 do
    begin
      Cand := Candidates[I];
      if FileExists(Cand) then
        Exit(Cand);
    end;
  finally
    Candidates.Free;
  end;
end;

type
  PXDisplay = Pointer;
  TXWindow = PtrUInt;
  PXWindow = ^TXWindow;
  PPXWindow = ^PXWindow;
  TXAtom = PtrUInt;
  TXTextProperty = record
    value: PByte;
    encoding: TXAtom;
    format: Integer;
    nitems: PtrUInt;
  end;
  PXTextProperty = ^TXTextProperty;

  TXOpenDisplayFn = function(display_name: PChar): PXDisplay; cdecl;
  TXCloseDisplayFn = function(display: PXDisplay): Integer; cdecl;
  TXDefaultRootWindowFn = function(display: PXDisplay): TXWindow; cdecl;
  TXQueryTreeFn = function(display: PXDisplay; w: TXWindow; root_return, parent_return: PXWindow;
    children_return: PPXWindow; nchildren_return: PCardinal): Integer; cdecl;
  TXFetchNameFn = function(display: PXDisplay; w: TXWindow; window_name_return: PPChar): Integer; cdecl;
  TXGetWMNameFn = function(display: PXDisplay; w: TXWindow; text_prop_return: PXTextProperty): Integer; cdecl;
  TXFreeFn = function(data: Pointer): Integer; cdecl;

function ScanX11WindowTitlesForSubstring(const NeedleLower: string; AllMatches: TStrings): string;
var
  LibX11: TLibHandle;
  fnOpenDisplay: TXOpenDisplayFn;
  fnCloseDisplay: TXCloseDisplayFn;
  fnDefaultRootWindow: TXDefaultRootWindowFn;
  fnQueryTree: TXQueryTreeFn;
  fnFetchName: TXFetchNameFn;
  fnGetWMName: TXGetWMNameFn;
  fnFree: TXFreeFn;
  Dpy: PXDisplay;

  function GetWinTitle(W: TXWindow): string;
  var
    Prop: TXTextProperty;
    WinName: PChar;
  begin
    Result := '';
    if PPointer(@fnGetWMName)^ <> nil then
    begin
      FillChar(Prop, SizeOf(Prop), 0);
      if (fnGetWMName(Dpy, W, @Prop) <> 0) and (Prop.value <> nil) then
      begin
        if (Prop.format = 8) and (Prop.nitems > 0) then
          SetString(Result, PChar(Prop.value), Prop.nitems);
        fnFree(Prop.value);
      end;
    end;
    if (Result = '') and (PPointer(@fnFetchName)^ <> nil) then
    begin
      WinName := nil;
      if (fnFetchName(Dpy, W, @WinName) <> 0) and (WinName <> nil) then
      begin
        Result := StrPas(WinName);
        fnFree(WinName);
      end;
    end;
  end;

  procedure TraverseWindow(W: TXWindow; Depth: Integer);
  var
    RootRet, ParentRet: TXWindow;
    Children: PXWindow;
    NChildren, I: Cardinal;
    Title: string;
  begin
    if (W = 0) or (Depth > 4) then
      Exit;
    Title := Trim(GetWinTitle(W));
    if (Title <> '') and (Pos(NeedleLower, LowerCase(Title)) > 0) then
    begin
      if Assigned(AllMatches) then
        AllMatches.Add(Format('Window 0x%x: %s', [W, Title]));
      if Result = '' then
        Result := Title;
    end;
    if (Result <> '') and (not Assigned(AllMatches)) then
      Exit;
    Children := nil;
    NChildren := 0;
    if fnQueryTree(Dpy, W, @RootRet, @ParentRet, @Children, @NChildren) <> 0 then
    begin
      try
        if (Children <> nil) and (NChildren > 0) then
          for I := 0 to NChildren - 1 do
          begin
            TraverseWindow(PXWindow(PByte(Children) + PtrUInt(I) * SizeOf(TXWindow))^, Depth + 1);
            if (Result <> '') and (not Assigned(AllMatches)) then
              Break;
          end;
      finally
        if Children <> nil then
          fnFree(Children);
      end;
    end;
  end;

begin
  Result := '';
  if SysUtils.GetEnvironmentVariable('DISPLAY') = '' then
    Exit;
  LibX11 := LoadLibrary('libX11.so.6');
  if LibX11 = NilHandle then
    LibX11 := LoadLibrary('libX11.so');
  if LibX11 = NilHandle then
    Exit;
  try
    PPointer(@fnOpenDisplay)^ := GetProcedureAddress(LibX11, 'XOpenDisplay');
    PPointer(@fnCloseDisplay)^ := GetProcedureAddress(LibX11, 'XCloseDisplay');
    PPointer(@fnDefaultRootWindow)^ := GetProcedureAddress(LibX11, 'XDefaultRootWindow');
    PPointer(@fnQueryTree)^ := GetProcedureAddress(LibX11, 'XQueryTree');
    PPointer(@fnFetchName)^ := GetProcedureAddress(LibX11, 'XFetchName');
    PPointer(@fnGetWMName)^ := GetProcedureAddress(LibX11, 'XGetWMName');
    PPointer(@fnFree)^ := GetProcedureAddress(LibX11, 'XFree');
    if (PPointer(@fnOpenDisplay)^ = nil) or (PPointer(@fnCloseDisplay)^ = nil) or
       (PPointer(@fnDefaultRootWindow)^ = nil) or (PPointer(@fnQueryTree)^ = nil) or
       (PPointer(@fnFree)^ = nil) then
      Exit;
    Dpy := fnOpenDisplay(nil);
    if Dpy = nil then
      Exit;
    try
      TraverseWindow(fnDefaultRootWindow(Dpy), 0);
    finally
      fnCloseDisplay(Dpy);
    end;
  finally
    UnloadLibrary(LibX11);
  end;
end;

class function TWAutomateUtils.FindWindowTitleContaining(const ASubstring: string): string;
begin
  Result := ScanX11WindowTitlesForSubstring(LowerCase(ASubstring), nil);
end;

class function TWAutomateUtils.GetTwCurrentRef: string;
var
  Title: string;
begin
  Result := '';
  if IsTwRunning = 0 then
    Exit;
  Title := FindWindowTitleContaining('theword');
  if Title <> '' then
    Result := ExtractTheWordRefFromCaption(Title);
end;

class procedure TWAutomateUtils.FindTheWordWindows(outputFile: string);
var
  f: TextFile;
  Matches: TStringList;
  I: Integer;
begin
  Matches := TStringList.Create;
  try
    ScanX11WindowTitlesForSubstring('theword', Matches);
    AssignFile(f, outputFile);
    try
      Rewrite(f);
      WriteLn(f, 'List of X11 Windows with "theWord" in title:');
      WriteLn(f, StringOfChar('=', 50));
      for I := 0 to Matches.Count - 1 do
        WriteLn(f, Matches[I]);
      WriteLn(f);
      WriteLn(f, 'End of Window List');
    finally
      CloseFile(f);
    end;
  finally
    Matches.Free;
  end;
  ShowMessage(Format(STWWindowNotFoundDebugSavedFmt, [outputFile]));
end;

type
  TFcConfigGetCurrentFn = function: Pointer; cdecl;
  TFcConfigAppFontAddFileFn = function(config: Pointer; const fileName: PChar): Integer; cdecl;

class procedure TWAutomateUtils.LoadTheWordFonts;
var
  LibFc: TLibHandle;
  fnGetCurrent: TFcConfigGetCurrentFn;
  fnAddFile: TFcConfigAppFontAddFileFn;
  FcCfg: Pointer;
  Dirs: TStringList;
  Dir, Ext, UserHome, UserName, WinePrefix: string;
  Info: TSearchRec;

  procedure AddDir(const APath: string);
  var
    Clean: string;
  begin
    if APath = '' then
      Exit;
    Clean := IncludeTrailingPathDelimiter(APath);
    if Dirs.IndexOf(Clean) < 0 then
      Dirs.Add(Clean);
  end;

begin
  LibFc := LoadLibrary('libfontconfig.so.1');
  if LibFc = NilHandle then
    LibFc := LoadLibrary('libfontconfig.so');
  if LibFc = NilHandle then
    Exit;
  try
    PPointer(@fnGetCurrent)^ := GetProcedureAddress(LibFc, 'FcConfigGetCurrent');
    PPointer(@fnAddFile)^ := GetProcedureAddress(LibFc, 'FcConfigAppFontAddFile');
    if (PPointer(@fnGetCurrent)^ = nil) or (PPointer(@fnAddFile)^ = nil) then
      Exit;
    FcCfg := fnGetCurrent();
    if FcCfg = nil then
      Exit;

    Dirs := TStringList.Create;
    try
      try
        Dir := ExtractFilePath(GetConfigFile);
        if Dir <> '' then
        begin
          AddDir(Dir + 'fonts');
          AddDir(Dir);
        end;
      except
      end;

      UserHome := ExcludeTrailingPathDelimiter(GetUserDir);
      UserName := SysUtils.GetEnvironmentVariable('USER');
      if UserName = '' then
        UserName := ExtractFileName(UserHome);
      WinePrefix := Trim(SysUtils.GetEnvironmentVariable('WINEPREFIX'));
      if WinePrefix = '' then
        WinePrefix := UserHome + '/.wine';

      AddDir(ExtractFilePath(ParamStr(0)) + 'fonts');
      AddDir(UserHome + '/.local/share/iBiblia/fonts');
      AddDir(UserHome + '/.local/share/theWord/fonts');
      AddDir(WinePrefix + '/drive_c/users/' + UserName + '/AppData/Roaming/theWord/fonts');
      AddDir(WinePrefix + '/drive_c/Program Files (x86)/The Word/fonts');
      AddDir(WinePrefix + '/drive_c/Program Files/The Word/fonts');
      AddDir(WinePrefix + '/drive_c/theWord/fonts');

      for Dir in Dirs do
      begin
        if not DirectoryExists(Dir) then
          Continue;
        if FindFirst(Dir + '*', faAnyFile, Info) = 0 then
        try
          repeat
            if (Info.Attr and faDirectory) = 0 then
            begin
              Ext := LowerCase(ExtractFileExt(Info.Name));
              if (Ext = '.ttf') or (Ext = '.otf') or (Ext = '.ttc') then
                fnAddFile(FcCfg, PChar(Dir + Info.Name));
            end;
          until FindNext(Info) <> 0;
        finally
          SysUtils.FindClose(Info);
        end;
      end;
    finally
      Dirs.Free;
    end;
  finally
    UnloadLibrary(LibFc);
  end;
end;

{$ENDIF}

initialization
  BooksMapLock := TCriticalSection.Create;

finalization
  FreeAndNil(BooksMapLock);
  FreeAndNil(BooksMap);

end.
