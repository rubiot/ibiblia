unit esword_usfm;

{ e-Sword (.bblx/.bbli/.bbl), USFM (.usfm/.sfm) and USFX (.usfx/.xml) Bible
  import and export, including Apocrypha / deuterocanonical books (67+).
  Moved unchanged from the 1.0.1 mod's myswordmodule.pas (TESwordModule);
  1.1.0's own MySword exporter stays in myswordmodule.pas. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sqlite3conn, sqldb, db, LazUTF8, PCRE, Zipper;

var
  { When True, a USFM file with a single book is read alone; the default (False)
    also gathers the other .usfm/.sfm files of its folder (new projects). }
  USFMSingleFileOnly: Boolean;

type
  TESwordExtraVerse = record
    Book: integer;
    Chapter: integer;
    Verse: integer;
    PareID: string;
    ONTText: string;
  end;
  TESwordExtraVerseArray = array of TESwordExtraVerse;

  { TESwordModule }

  TESwordModule = class
  private
    FFinalFileName: string;
    FStagingFileName: string;
    FIsHTML: boolean;
    FCommitted: boolean;
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FInsertVerse: TSQLQuery;
    procedure CreateTables(ot, nt, strongs, apocrypha: boolean; const props: string);
  public
    constructor Create(const filename: string; ot, nt, strongs: boolean; const props: string; isHTML: boolean = False; apocrypha: boolean = False);
    destructor Destroy; override;
    procedure AddVerse(const ontText: string; book, chapter, verse: integer);
    procedure Commit;

    class function IsESwordFileName(const filename: string): boolean;
    class function IsUSFMFileName(const filename: string): boolean;
    class function IsUSFMZipFileName(const filename: string): boolean;
    class function IsUSFXFileName(const filename: string): boolean;
    class function IsUSFMOrUSFXFileName(const filename: string): boolean;
    class function USFMBookCodeToID(const code: string): integer;
    class function BookIDToUSFMCode(bookID: integer): string;
    class function DetectESwordScope(const filename: string; out hasOT, hasNT: boolean): boolean;
    class function GetCanonicalLineIndex(book, chapter, verse: integer; out maxVerseInChap: integer): integer;
    class function IdentifyApocryphaBookName(bookID, maxChap, chap1Verses, chap2Verses, maxChapVerses: integer; const sampleText: string): string;
    class function LoadUSFMBibleWithApocrypha(const filename: string; modulo: TStringList; out extraVerses: TESwordExtraVerseArray; out apocryphaBooksInfo: string; out hasOT, hasNT: boolean; out errMsg: string): boolean;
    class function LoadUSFXBibleWithApocrypha(const filename: string; modulo: TStringList; out extraVerses: TESwordExtraVerseArray; out apocryphaBooksInfo: string; out hasOT, hasNT: boolean; out errMsg: string): boolean;
    class function LoadESwordBibleWithApocrypha(const filename: string; modulo: TStringList; out extraVerses: TESwordExtraVerseArray; out apocryphaBooksInfo: string; out hasOT, hasNT: boolean; out errMsg: string): boolean;
    class function LoadESwordBibleToONT(const filename: string; modulo: TStringList; out hasOT, hasNT: boolean; out errMsg: string): boolean;
    class function ConvertESwordScriptureToONT(const rawScripture: string; preferHTML, isNTBook: boolean): string;
    class function ConvertONTToESwordRTF(const ontText: string): string;
    class function ConvertONTToESwordHTML(const ontText: string): string;
    class function ConvertONTToUSFX(const ontText: string; out headingBeforeVerse: string): string;
    class procedure ExportUSFMBible(verses, refIDs: TStringList; const filename, props: string; hasOT, hasNT: boolean);
    class procedure ExportUSFMBibleEx(verses, refIDs: TStringList; const filename, finalFileName, props: string; hasOT, hasNT, zipOnly: boolean);
    class procedure ExportUSFXBible(verses, refIDs: TStringList; const filename, props: string; hasOT, hasNT: boolean);
  end;

resourcestring
  SCreateMySwordBibleError = 'Unable to create new MySword module file';
  SEncryptedESwordModuleError = 'This e-Sword Bible module is encrypted or locked. iBiblia only supports non-encrypted e-Sword Bible files (.bblx, .bbli).';
  SLegacyJetESwordModuleError = 'This .bbl file uses the legacy e-Sword 7/8 MS Access (Jet) format. Please convert it to .bblx or .bbli using the e-Sword Module Converter first.';
  SInvalidESwordDatabaseError = 'The selected file is not a valid non-encrypted e-Sword SQLite Bible module.';

implementation

uses
  apocrypha_books;

const
  ESwordChaptersPerBook: array[1..66] of smallint = (
    50,40,27,36,34,24,21,4,31,24,22,25,29,36,10,13,10,42,150,31,12,8,66,
    52,5,48,12,14,3,9,1,4,7,3,3,3,2,14,4,28,16,24,21,28,16,16,13,6,6,4,
    4,5,3,6,4,3,1,13,5,5,3,5,1,1,1,22
  );

  ESwordBookChapterStart: array[1..66] of smallint = (
    1,51,91,118,154,188,212,233,237,268,292,314,339,368,404,414,427,437,479,629,
    660,672,680,746,798,803,851,863,877,880,889,890,894,901,904,907,910,912,926,
    930,958,974,998,1019,1047,1063,1079,1092,1098,1104,1108,1112,1117,1120,1126,
    1130,1133,1134,1147,1152,1157,1160,1165,1166,1167,1168
  );

  ESwordVersesPerChapter: array[1..1189] of smallint = (
    31,25,24,26,32,22,24,22,29,32,32,20,18,24,21,16,27,33,38,18,34,24,20,67,34,35,
    46,22,35,43,55,32,20,31,29,43,36,30,23,23,57,38,34,34,28,34,31,22,33,26,22,25,
    22,31,23,30,25,32,35,29,10,51,22,31,27,36,16,27,25,26,36,31,33,18,40,37,21,43,
    46,38,18,35,23,35,35,38,29,31,43,38,17,16,17,35,19,30,38,36,24,20,47,8,59,57,
    33,34,16,30,37,27,24,33,44,23,55,46,34,54,34,51,49,31,27,89,26,23,36,35,16,33,
    45,41,50,13,32,22,29,35,41,30,25,18,65,23,31,40,16,54,42,56,29,34,13,46,37,29,
    49,33,25,26,20,29,22,32,32,18,29,23,22,20,22,21,20,23,30,25,22,19,19,26,68,29,
    20,30,52,29,12,18,24,17,24,15,27,26,35,27,43,23,24,33,15,63,10,18,28,51,9,45,
    34,16,33,36,23,31,24,31,40,25,35,57,18,40,15,25,20,20,31,13,31,30,48,25,22,23,
    18,22,28,36,21,22,12,21,17,22,27,27,15,25,23,52,35,23,58,30,24,42,15,23,29,22,
    44,25,12,25,11,31,13,27,32,39,12,25,23,29,18,13,19,27,31,39,33,37,23,29,33,43,
    26,22,51,39,25,53,46,28,34,18,38,51,66,28,29,43,33,34,31,34,34,24,46,21,43,29,
    53,18,25,27,44,27,33,20,29,37,36,21,21,25,29,38,20,41,37,37,21,26,20,37,20,30,
    54,55,24,43,26,81,40,40,44,14,47,40,14,17,29,43,27,17,19,8,30,19,32,31,31,32,
    34,21,30,17,18,17,22,14,42,22,18,31,19,23,16,22,15,19,14,19,34,11,37,20,12,21,
    27,28,23,9,27,36,27,21,33,25,33,27,23,11,70,13,24,17,22,28,36,15,44,11,20,32,
    23,19,19,73,18,38,39,36,47,31,22,23,15,17,14,14,10,17,32,3,22,13,26,21,27,30,
    21,22,35,22,20,25,28,22,35,22,16,21,29,29,34,30,17,25,6,14,23,28,25,31,40,22,
    33,37,16,33,24,41,30,24,34,17,6,12,8,8,12,10,17,9,20,18,7,8,6,7,5,11,15,50,14,
    9,13,31,6,10,22,12,14,9,11,12,24,11,22,22,28,12,40,22,13,17,13,11,5,26,17,11,
    9,14,20,23,19,9,6,7,23,13,11,11,17,12,8,12,11,10,13,20,7,35,36,5,24,20,28,23,
    10,12,20,72,13,19,16,8,18,12,13,17,7,18,52,17,16,15,5,23,11,13,12,9,9,5,8,28,
    22,35,45,48,43,13,31,7,10,10,9,8,18,19,2,29,176,7,8,9,4,8,5,6,5,6,8,8,3,18,3,
    3,21,26,9,8,24,13,10,7,12,15,21,10,20,14,9,6,33,22,35,27,23,35,27,36,18,32,31,
    28,25,35,33,33,28,24,29,30,31,29,35,34,28,28,27,28,27,33,31,18,26,22,16,20,12,
    29,17,18,20,10,14,17,17,11,16,16,13,13,14,31,22,26,6,30,13,25,22,21,34,16,6,
    22,32,9,14,14,7,25,6,17,25,18,23,12,21,13,29,24,33,9,20,24,17,10,22,38,22,8,
    31,29,25,28,28,25,13,15,22,26,11,23,15,12,17,13,12,21,14,21,22,11,12,19,12,25,
    24,19,37,25,31,31,30,34,22,26,25,23,17,27,22,21,21,27,23,15,18,14,30,40,10,38,
    24,22,17,32,24,40,44,26,22,19,32,21,28,18,16,18,22,13,30,5,28,7,47,39,46,64,
    34,22,22,66,22,22,28,10,27,17,17,14,27,18,11,22,25,28,23,23,8,63,24,32,14,49,
    32,31,49,27,17,21,36,26,21,26,18,32,33,31,15,38,28,23,29,49,26,20,27,31,25,24,
    23,35,21,49,30,37,31,28,28,27,27,21,45,13,11,23,5,19,15,11,16,14,17,15,12,14,
    16,9,20,32,21,15,16,15,13,27,14,17,14,15,21,17,10,10,11,16,13,12,13,15,16,20,
    15,13,19,17,20,19,18,15,20,15,23,21,13,10,14,11,15,14,23,17,12,17,14,9,21,14,
    17,18,6,25,23,17,25,48,34,29,34,38,42,30,50,58,36,39,28,27,35,30,34,46,46,
    39,51,46,75,66,20,45,28,35,41,43,56,37,38,50,52,33,44,37,72,47,20,80,52,38,
    44,39,49,50,56,62,42,54,59,35,35,32,31,37,43,48,47,38,71,56,53,51,25,36,54,
    47,71,53,59,41,42,57,50,38,31,27,33,26,40,42,31,25,26,47,26,37,42,15,60,40,
    43,48,30,25,52,28,41,40,34,28,41,38,40,30,35,27,27,32,44,31,32,29,31,25,21,
    23,25,39,33,21,36,21,14,23,33,27,31,16,23,21,13,20,40,13,27,33,34,31,13,40,
    58,24,24,17,18,18,21,18,16,24,15,18,33,21,14,24,21,29,31,26,18,23,22,21,32,
    33,24,30,30,21,23,29,23,25,18,10,20,13,18,28,12,17,18,20,15,16,16,25,21,18,
    26,17,22,16,15,15,25,14,18,19,16,14,20,28,13,28,39,40,29,25,27,26,18,17,20,
    25,25,22,19,14,21,22,18,10,29,24,21,21,13,14,25,20,29,22,11,14,17,17,13,21,
    11,19,17,18,20,8,21,18,24,21,15,27,21
  );

  ESwordOTLines = 23145;
  ESwordNTLines = 7957;
  ESwordONTLines = 31102;

var
  ChapterLineOffsetsInitialized: boolean = False;
  ChapterLineOffsets: array[1..1189] of integer;

procedure EnsureChapterLineOffsets;
var
  i, acc: integer;
begin
  if ChapterLineOffsetsInitialized then
    Exit;
  acc := 0;
  for i := 1 to 1189 do
  begin
    ChapterLineOffsets[i] := acc;
    Inc(acc, ESwordVersesPerChapter[i]);
  end;
  ChapterLineOffsetsInitialized := True;
end;

function GetCanonicalONTLineIndex(book, chapter, verse: integer; out maxVerseInChap: integer): integer;
var
  chapIdx: integer;
begin
  EnsureChapterLineOffsets;
  Result := -1;
  maxVerseInChap := 0;
  if (book < 1) or (book > 66) then
    Exit;
  if (chapter < 1) or (chapter > ESwordChaptersPerBook[book]) then
    Exit;
  chapIdx := ESwordBookChapterStart[book] + chapter - 1;
  if (chapIdx < 1) or (chapIdx > 1189) then
    Exit;
  maxVerseInChap := ESwordVersesPerChapter[chapIdx];
  if verse < 1 then
    Result := ChapterLineOffsets[chapIdx]
  else if verse > maxVerseInChap then
    Result := ChapterLineOffsets[chapIdx] + maxVerseInChap - 1
  else
    Result := ChapterLineOffsets[chapIdx] + verse - 1;
end;


function GetStagingFileName(const targetFile: string): string;
begin
  Result := targetFile + '.tmp_esword';
end;

function CopyFileViaStream(const srcFile, dstFile: string): boolean;
var
  srcStream, dstStream: TFileStream;
begin
  Result := False;
  if not FileExists(srcFile) then
    Exit;
  try
    srcStream := TFileStream.Create(srcFile, fmOpenRead or fmShareDenyNone);
    try
      dstStream := TFileStream.Create(dstFile, fmCreate or fmOpenWrite or fmShareDenyWrite);
      try
        dstStream.Size := 0;
        if srcStream.Size > 0 then
          dstStream.CopyFrom(srcStream, srcStream.Size);
        Result := True;
      finally
        dstStream.Free;
      end;
    finally
      srcStream.Free;
    end;
  except
    Result := False;
  end;
end;

function PublishStagedFile(const stagedFile, finalFile: string): boolean;
var
  backupFile: string;
begin
  Result := False;
  if not FileExists(stagedFile) then
    Exit;

  backupFile := finalFile + '.bak_esword';
  if FileExists(backupFile) then
    SysUtils.DeleteFile(backupFile);

  if FileExists(finalFile) then
  begin
    if SysUtils.RenameFile(finalFile, backupFile) then
    begin
      if SysUtils.RenameFile(stagedFile, finalFile) then
      begin
        SysUtils.DeleteFile(backupFile);
        Result := True;
        Exit;
      end
      else
        SysUtils.RenameFile(backupFile, finalFile);
    end;

    if SysUtils.DeleteFile(finalFile) and SysUtils.RenameFile(stagedFile, finalFile) then
    begin
      Result := True;
      Exit;
    end;
  end
  else
  begin
    if SysUtils.RenameFile(stagedFile, finalFile) then
    begin
      Result := True;
      Exit;
    end;
  end;

  if CopyFileViaStream(stagedFile, finalFile) then
  begin
    SysUtils.DeleteFile(stagedFile);
    Result := True;
  end;
end;

function CodePointToUTF8Str(cp: Cardinal): string;
begin
  if cp <= $7F then
    Result := Chr(cp)
  else if cp <= $7FF then
    Result := Chr($C0 or (cp shr 6)) + Chr($80 or (cp and $3F))
  else if cp <= $FFFF then
    Result := Chr($E0 or (cp shr 12)) + Chr($80 or ((cp shr 6) and $3F)) + Chr($80 or (cp and $3F))
  else if cp <= $10FFFF then
    Result := Chr($F0 or (cp shr 18)) + Chr($80 or ((cp shr 12) and $3F)) + Chr($80 or ((cp shr 6) and $3F)) + Chr($80 or (cp and $3F))
  else
    Result := '';
end;

function CP1252ByteToUTF8(b: Byte): string;
const
  CP1252Table: array[$80..$9F] of Word = (
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152, $008D, $017D, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153, $009D, $017E, $0178
  );
begin
  if b < $80 then
    Result := Chr(b)
  else if (b >= $80) and (b <= $9F) then
    Result := CodePointToUTF8Str(CP1252Table[b])
  else
    Result := CodePointToUTF8Str(b);
end;

procedure AppendTagBeforeTrailingPunct(var buf: string; const tag: string);
var
  len, pStart: integer;
  trailingPunct: string;
begin
  if tag = '' then
    Exit;
  len := Length(buf);
  while (len > 0) and (buf[len] in [' ', #9]) do
    Dec(len);
  SetLength(buf, len);

  pStart := len;
  while (pStart > 0) and (buf[pStart] in ['.', ',', ';', ':', '!', '?', ')', ']', '"', '''']) do
    Dec(pStart);

  if (pStart > 0) and (pStart < len) and not (buf[pStart] in ['>', ' ', #9]) then
  begin
    trailingPunct := Copy(buf, pStart + 1, len - pStart);
    SetLength(buf, pStart);
    buf := buf + tag + trailingPunct;
  end
  else
    buf := buf + tag;
end;

function IsMorphologyCodeToken(const tok: string): boolean;
var
  u: string;
  i: integer;
begin
  Result := False;
  u := UpperCase(Trim(tok));
  if u = '' then
    Exit;
  if (u = 'PREP') or (u = 'CONJ') or (u = 'ADV') or (u = 'PRT') or (u = 'INJ') or
     (u = 'COND') or (u = 'ARAM') or (u = 'HEB') or (u = 'N-PRI') or (u = 'A-NUI') or
     (u = 'PRT-N') or (u = 'PRT-I') then
  begin
    Result := True;
    Exit;
  end;
  if (Length(u) >= 3) and (Pos('-', u) >= 2) and (u[1] in ['A'..'Z']) then
  begin
    Result := True;
    for i := 1 to Length(u) do
      if not (u[i] in ['A'..'Z', '0'..'9', '-']) then
      begin
        Result := False;
        Break;
      end;
  end;
end;

function ClassifyAndFormatSuperscriptTag(const rawContent: string; isExplicitMorph, isNTBook: boolean; var wordHasStrong: boolean): string;
var
  s, tok, cleanTok, prefixStr, numDigits, suffixStr: string;
  i, len, p, numVal: integer;
  inParens, tokenIsMorph: boolean;
begin
  Result := '';
  s := Trim(rawContent);
  if s = '' then
    Exit;

  i := 1;
  len := Length(s);
  while i <= len do
  begin
    while (i <= len) and (s[i] in [' ', #9, #13, #10, ',', ';', '/']) do
      Inc(i);
    if i > len then
      Break;

    p := i;
    while (i <= len) and not (s[i] in [' ', #9, #13, #10, ',', ';', '/']) do
      Inc(i);
    tok := Copy(s, p, i - p);

    inParens := (Length(tok) >= 2) and
                (((tok[1] = '(') and (tok[Length(tok)] = ')')) or
                 ((tok[1] = '[') and (tok[Length(tok)] = ']')));
    cleanTok := tok;
    while (Length(cleanTok) > 0) and (cleanTok[1] in ['(', '[', '{', ':', '.']) do
      Delete(cleanTok, 1, 1);
    while (Length(cleanTok) > 0) and (cleanTok[Length(cleanTok)] in [')', ']', '}', ':', '.', ',']) do
      SetLength(cleanTok, Length(cleanTok) - 1);

    if cleanTok = '' then
      Continue;

    if AnsiStartsText('<WG', cleanTok) or AnsiStartsText('<WH', cleanTok) or AnsiStartsText('<WT', cleanTok) then
    begin
      if AnsiStartsText('<WG', cleanTok) or AnsiStartsText('<WH', cleanTok) then
        wordHasStrong := True;
      Result := Result + cleanTok;
      Continue;
    end;

    if (Length(cleanTok) >= 2) and (cleanTok[1] in ['G', 'g', 'H', 'h']) and (cleanTok[2] in ['0'..'9']) then
    begin
      prefixStr := UpperCase(Copy(cleanTok, 1, 1));
      p := 2;
      while (p <= Length(cleanTok)) and (cleanTok[p] in ['0'..'9']) do
        Inc(p);
      numDigits := Copy(cleanTok, 2, p - 2);
      suffixStr := Copy(cleanTok, p, Length(cleanTok) - p + 1);
      numVal := StrToIntDef(numDigits, -1);

      if numVal >= 0 then
      begin
        tokenIsMorph := isExplicitMorph or inParens;
        if not tokenIsMorph then
        begin
          if (prefixStr = 'H') and (numVal >= 8675) and (numVal <= 8999) then
            tokenIsMorph := True
          else if (prefixStr = 'G') and isNTBook and wordHasStrong and (numVal >= 5625) and (numVal <= 5999) then
            tokenIsMorph := True;
        end;

        if tokenIsMorph then
          Result := Result + '<WT' + prefixStr + IntToStr(numVal) + suffixStr + '>'
        else
        begin
          wordHasStrong := True;
          Result := Result + '<W' + prefixStr + IntToStr(numVal) + suffixStr + '>';
        end;
        Continue;
      end;
    end;

    if isExplicitMorph or inParens or IsMorphologyCodeToken(cleanTok) then
      Result := Result + '<WT' + cleanTok + '>';
  end;
end;

function LooksLikeStandaloneTagRun(const s: string): boolean;
var
  trimmed: string;
begin
  trimmed := Trim(s);
  Result := False;
  if (trimmed = '') or (Length(trimmed) > 32) then
    Exit;
  if (Length(trimmed) >= 2) and (trimmed[1] in ['G', 'g', 'H', 'h']) and (trimmed[2] in ['0'..'9']) then
    Result := True
  else if (Length(trimmed) >= 4) and (trimmed[1] = '(') and (trimmed[2] in ['G', 'g', 'H', 'h']) then
    Result := True
  else if IsMorphologyCodeToken(trimmed) then
    Result := True;
end;

function ParseESwordRTFToONT(const rtf: string; isNTBook: boolean): string;
type
  TRTFState = record
    cf: integer;
    superRun: boolean;
    italic: boolean;
    bold: boolean;
    underline: boolean;
    red: boolean;
    ucCount: integer;
    ignoreGroup: boolean;
  end;
var
  stack: array[0..63] of TRTFState;
  sp: integer;
  cur: TRTFState;
  emittedItalic, emittedBold, emittedUnderline, emittedRed: boolean;
  inSuperBuf: boolean;
  superWasExplicit: boolean;
  superBuf: string;
  superCF: integer;
  wordHasStrong, hadSpaceBeforeSuper: boolean;
  i, len, cmdStart, paramStart, paramVal, k: integer;
  cmd, hexStr, formattedTags: string;
  hasParam, isNeg: boolean;
  cp, lowSurrogate: integer;

  procedure SyncFormattingTags(const target: TRTFState);
  begin
    if inSuperBuf then
      Exit;
    if emittedUnderline and not target.underline then
    begin
      Result := Result + '<Fu>';
      emittedUnderline := False;
    end;
    if emittedBold and not target.bold then
    begin
      Result := Result + '<Fo>';
      emittedBold := False;
    end;
    if emittedItalic and not target.italic then
    begin
      Result := Result + '<Fi>';
      emittedItalic := False;
    end;
    if emittedRed and not target.red then
    begin
      Result := Result + '<Fr>';
      emittedRed := False;
    end;

    if target.red and not emittedRed then
    begin
      Result := Result + '<FR>';
      emittedRed := True;
    end;
    if target.italic and not emittedItalic then
    begin
      Result := Result + '<FI>';
      emittedItalic := True;
    end;
    if target.bold and not emittedBold then
    begin
      Result := Result + '<FO>';
      emittedBold := True;
    end;
    if target.underline and not emittedUnderline then
    begin
      Result := Result + '<FU>';
      emittedUnderline := True;
    end;
  end;

  procedure FlushSuperBuf;
  begin
    if not inSuperBuf then
      Exit;
    inSuperBuf := False;
    formattedTags := ClassifyAndFormatSuperscriptTag(
      superBuf,
      (superCF = 11),
      isNTBook,
      wordHasStrong
    );
    if formattedTags <> '' then
      AppendTagBeforeTrailingPunct(Result, formattedTags)
    else if superWasExplicit and (Trim(superBuf) <> '') then
      Result := Result + '<RF>' + Trim(superBuf) + '<Rf>'
    else if superBuf <> '' then
    begin
      SyncFormattingTags(cur);
      Result := Result + superBuf;
    end;
    superWasExplicit := False;
    superBuf := '';
    superCF := 0;
  end;

  procedure AppendPlainTextChar(const ch: string);
  begin
    if cur.ignoreGroup then
      Exit;
    if cur.superRun or ((cur.cf in [11, 15]) and not cur.italic) then
    begin
      if not inSuperBuf then
      begin
        inSuperBuf := True;
        superWasExplicit := cur.superRun;
        superBuf := '';
        superCF := cur.cf;
        hadSpaceBeforeSuper := (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]);
      end
      else if cur.superRun then
        superWasExplicit := True;
      superBuf := superBuf + ch;
      Exit;
    end;

    if inSuperBuf then
      FlushSuperBuf;

    if (ch = ' ') or (ch = #9) then
      wordHasStrong := False
    else if hadSpaceBeforeSuper and (Length(Result) > 0) and (Result[Length(Result)] = '>') and
            not (ch[1] in ['.', ',', ';', ':', '!', '?', ')', ']', '"', '''', ' ', #9, '<']) then
    begin
      Result := Result + ' ';
      wordHasStrong := False;
    end;
    hadSpaceBeforeSuper := False;

    SyncFormattingTags(cur);
    Result := Result + ch;
  end;

begin
  Result := '';
  len := Length(rtf);
  if len = 0 then
    Exit;

  FillChar(stack, SizeOf(stack), 0);
  cur.cf := 0;
  cur.superRun := False;
  cur.italic := False;
  cur.bold := False;
  cur.underline := False;
  cur.red := False;
  cur.ucCount := 1;
  cur.ignoreGroup := False;
  sp := 0;

  emittedItalic := False;
  emittedBold := False;
  emittedUnderline := False;
  emittedRed := False;
  inSuperBuf := False;
  superWasExplicit := False;
  superBuf := '';
  superCF := 0;
  wordHasStrong := False;
  hadSpaceBeforeSuper := False;

  i := 1;
  while i <= len do
  begin
    case rtf[i] of
      '{':
      begin
        if sp < High(stack) then
        begin
          stack[sp] := cur;
          Inc(sp);
        end;
        Inc(i);
        if (i + 1 <= len) and (rtf[i] = '\') and (rtf[i + 1] = '*') then
        begin
          cur.ignoreGroup := True;
          Inc(i, 2);
        end;
      end;

      '}':
      begin
        if sp > 0 then
        begin
          Dec(sp);
          cur := stack[sp];
        end;
        if inSuperBuf and (not cur.superRun) and (not ((cur.cf in [11, 15]) and not cur.italic)) then
          FlushSuperBuf;
        if not cur.ignoreGroup then
          SyncFormattingTags(cur);
        Inc(i);
      end;

      '\':
      begin
        Inc(i);
        if i > len then
          Break;

        if rtf[i] in ['\', '{', '}'] then
        begin
          AppendPlainTextChar(rtf[i]);
          Inc(i);
        end
        else if rtf[i] = '~' then
        begin
          AppendPlainTextChar(' ');
          Inc(i);
        end
        else if rtf[i] = '-' then
        begin
          Inc(i);
        end
        else if rtf[i] = '_' then
        begin
          AppendPlainTextChar('-');
          Inc(i);
        end
        else if rtf[i] = '''' then
        begin
          Inc(i);
          if i + 1 <= len then
          begin
            hexStr := '$' + Copy(rtf, i, 2);
            paramVal := StrToIntDef(hexStr, -1);
            Inc(i, 2);
            if paramVal >= 0 then
              AppendPlainTextChar(CP1252ByteToUTF8(Byte(paramVal)));
          end;
        end
        else if rtf[i] in ['a'..'z', 'A'..'Z'] then
        begin
          cmdStart := i;
          while (i <= len) and (rtf[i] in ['a'..'z', 'A'..'Z']) do
            Inc(i);
          cmd := LowerCase(Copy(rtf, cmdStart, i - cmdStart));

          hasParam := False;
          isNeg := False;
          paramVal := 0;
          if (i <= len) and (rtf[i] = '-') then
          begin
            isNeg := True;
            Inc(i);
          end;
          paramStart := i;
          while (i <= len) and (rtf[i] in ['0'..'9']) do
            Inc(i);
          if i > paramStart then
          begin
            hasParam := True;
            paramVal := StrToIntDef(Copy(rtf, paramStart, i - paramStart), 0);
            if isNeg then
              paramVal := -paramVal;
          end;

          if (cmd = 'u') and hasParam then
          begin
            cp := paramVal;
            if cp < 0 then
              cp := cp + 65536;

            for k := 1 to cur.ucCount do
            begin
              if i <= len then
              begin
                if rtf[i] = '?' then
                  Inc(i)
                else if (i + 1 <= len) and (rtf[i] = ' ') and (rtf[i + 1] = '?') then
                  Inc(i, 2)
                else if (i + 3 <= len) and (rtf[i] = '\') and (rtf[i + 1] = '''') then
                  Inc(i, 4)
                else if rtf[i] = ' ' then
                  Inc(i);
              end;
            end;
            if (cur.ucCount = 0) and (i <= len) and (rtf[i] = ' ') then
              Inc(i);

            if (cp >= $D800) and (cp <= $DBFF) and (i + 2 <= len) and (rtf[i] = '\') and (rtf[i + 1] = 'u') then
            begin
              Inc(i, 2);
              isNeg := False;
              if (i <= len) and (rtf[i] = '-') then
              begin
                isNeg := True;
                Inc(i);
              end;
              paramStart := i;
              while (i <= len) and (rtf[i] in ['0'..'9']) do
                Inc(i);
              if i > paramStart then
              begin
                lowSurrogate := StrToIntDef(Copy(rtf, paramStart, i - paramStart), 0);
                if isNeg then
                  lowSurrogate := lowSurrogate + 65536;
                if (lowSurrogate >= $DC00) and (lowSurrogate <= $DFFF) then
                  cp := $10000 + ((cp - $D800) shl 10) + (lowSurrogate - $DC00);
                for k := 1 to cur.ucCount do
                  if (i <= len) and (rtf[i] in ['?', ' ']) then
                    Inc(i);
              end;
            end;

            AppendPlainTextChar(CodePointToUTF8Str(Cardinal(cp)));
            Continue;
          end;

          if (i <= len) and (rtf[i] = ' ') then
            Inc(i);

          if (cmd = 'fonttbl') or (cmd = 'colortbl') or (cmd = 'stylesheet') or
             (cmd = 'info') or (cmd = 'pict') or (cmd = 'header') or (cmd = 'footer') then
          begin
            cur.ignoreGroup := True;
          end
          else if not cur.ignoreGroup then
          begin
            if cmd = 'uc' then
            begin
              if hasParam and (paramVal >= 0) then
                cur.ucCount := paramVal
              else
                cur.ucCount := 1;
            end
            else if (cmd = 'par') or (cmd = 'line') then
            begin
              if inSuperBuf then
                FlushSuperBuf;
              Result := Result + '<CM>';
              wordHasStrong := False;
            end
            else if cmd = 'tab' then
              AppendPlainTextChar(' ')
            else if (cmd = 'super') or (cmd = 'up') then
            begin
              if not hasParam or (paramVal <> 0) then
              begin
                cur.superRun := True;
                if not inSuperBuf then
                begin
                  inSuperBuf := True;
                  superWasExplicit := True;
                  superBuf := '';
                  superCF := cur.cf;
                  hadSpaceBeforeSuper := (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]);
                end
                else
                begin
                  superWasExplicit := True;
                  if (cur.cf in [11, 15]) and (superCF = 0) then
                    superCF := cur.cf;
                end;
              end
              else
              begin
                cur.superRun := False;
                if inSuperBuf then
                  FlushSuperBuf;
              end;
            end
            else if cmd = 'nosupersub' then
            begin
              cur.superRun := False;
              if inSuperBuf then
                FlushSuperBuf;
            end
            else if cmd = 'cf' then
            begin
              if not hasParam then
                paramVal := 0;
              if inSuperBuf and (paramVal in [0, 6]) and not cur.superRun then
                FlushSuperBuf;
              cur.cf := paramVal;
              if inSuperBuf and (paramVal in [11, 15]) then
                superCF := paramVal;
              if paramVal = 6 then
                cur.red := True
              else if paramVal = 0 then
                cur.red := False;
              if not inSuperBuf then
                SyncFormattingTags(cur);
            end
            else if cmd = 'i' then
            begin
              cur.italic := (not hasParam) or (paramVal <> 0);
              if not inSuperBuf then
                SyncFormattingTags(cur);
            end
            else if cmd = 'b' then
            begin
              cur.bold := (not hasParam) or (paramVal <> 0);
              if not inSuperBuf then
                SyncFormattingTags(cur);
            end
            else if cmd = 'ul' then
            begin
              cur.underline := (not hasParam) or (paramVal <> 0);
              if not inSuperBuf then
                SyncFormattingTags(cur);
            end
            else if cmd = 'ulnone' then
            begin
              cur.underline := False;
              if not inSuperBuf then
                SyncFormattingTags(cur);
            end
            else if (cmd = 'plain') or (cmd = 'pard') then
            begin
              if cmd = 'plain' then
              begin
                cur.superRun := False;
                if inSuperBuf then
                  FlushSuperBuf;
                cur.cf := 0;
                cur.italic := False;
                cur.bold := False;
                cur.underline := False;
                cur.red := False;
                SyncFormattingTags(cur);
              end;
            end;
          end;
        end
        else
          Inc(i);
      end;

      #13, #10:
        Inc(i);

      else
      begin
        AppendPlainTextChar(rtf[i]);
        Inc(i);
      end;
    end;
  end;

  if inSuperBuf then
    FlushSuperBuf;

  cur.italic := False;
  cur.bold := False;
  cur.underline := False;
  cur.red := False;
  SyncFormattingTags(cur);
end;

function DecodeHTMLEntities(const s: string): string;
var
  i, len, semiPos, cp: integer;
  ent: string;
begin
  if Pos('&', s) = 0 then
  begin
    Result := s;
    Exit;
  end;
  Result := '';
  i := 1;
  len := Length(s);
  while i <= len do
  begin
    if s[i] = '&' then
    begin
      semiPos := i + 1;
      while (semiPos <= len) and (semiPos - i <= 12) and not (s[semiPos] in [';', ' ', '<', '&']) do
        Inc(semiPos);
      if (semiPos <= len) and (s[semiPos] = ';') then
      begin
        ent := Copy(s, i + 1, semiPos - i - 1);
        if (Length(ent) >= 2) and (ent[1] = '#') then
        begin
          if (Length(ent) >= 3) and (ent[2] in ['x', 'X']) then
            cp := StrToIntDef('$' + Copy(ent, 3, Length(ent) - 2), -1)
          else
            cp := StrToIntDef(Copy(ent, 2, Length(ent) - 1), -1);
          if cp > 0 then
          begin
            Result := Result + CodePointToUTF8Str(Cardinal(cp));
            i := semiPos + 1;
            Continue;
          end;
        end
        else
        begin
          ent := LowerCase(ent);
          if ent = 'nbsp' then begin Result := Result + ' '; i := semiPos + 1; Continue; end
          else if ent = 'amp' then begin Result := Result + '&'; i := semiPos + 1; Continue; end
          else if ent = 'lt' then begin Result := Result + '<'; i := semiPos + 1; Continue; end
          else if ent = 'gt' then begin Result := Result + '>'; i := semiPos + 1; Continue; end
          else if ent = 'quot' then begin Result := Result + '"'; i := semiPos + 1; Continue; end
          else if (ent = 'apos') or (ent = 'rsquo') then begin Result := Result + ''''; i := semiPos + 1; Continue; end
          else if ent = 'lsquo' then begin Result := Result + CodePointToUTF8Str($2018); i := semiPos + 1; Continue; end
          else if ent = 'ldquo' then begin Result := Result + CodePointToUTF8Str($201C); i := semiPos + 1; Continue; end
          else if ent = 'rdquo' then begin Result := Result + CodePointToUTF8Str($201D); i := semiPos + 1; Continue; end
          else if ent = 'ndash' then begin Result := Result + CodePointToUTF8Str($2013); i := semiPos + 1; Continue; end
          else if ent = 'mdash' then begin Result := Result + CodePointToUTF8Str($2014); i := semiPos + 1; Continue; end;
        end;
      end;
    end;
    Result := Result + s[i];
    Inc(i);
  end;
end;

function ParseESwordHTMLToONT(const html: string; isNTBook: boolean): string;
var
  i, len, tagEnd, closePos: integer;
  rawTag, tagLower, innerText, formattedTags: string;
  wordHasStrong, hadSpaceBeforeTag, isRedSpan, inRedSpan: boolean;

  function FindClosingTagCI(const needle: string; startIdx: integer): integer;
  var
    k, nLen: integer;
  begin
    Result := 0;
    nLen := Length(needle);
    for k := startIdx to len - nLen + 1 do
      if CompareText(Copy(html, k, nLen), needle) = 0 then
      begin
        Result := k;
        Exit;
      end;
  end;

begin
  Result := '';
  len := Length(html);
  if len = 0 then
    Exit;

  wordHasStrong := False;
  hadSpaceBeforeTag := False;
  inRedSpan := False;
  i := 1;
  while i <= len do
  begin
    if html[i] = '<' then
    begin
      tagEnd := i + 1;
      while (tagEnd <= len) and (html[tagEnd] <> '>') do
        Inc(tagEnd);
      if tagEnd > len then
      begin
        Result := Result + Copy(html, i, len - i + 1);
        Break;
      end;

      rawTag := Copy(html, i + 1, tagEnd - i - 1);
      tagLower := LowerCase(Trim(rawTag));

      if AnsiStartsText('WG', rawTag) or AnsiStartsText('WH', rawTag) or
         AnsiStartsText('WT', rawTag) or AnsiStartsText('FI', rawTag) or
         AnsiStartsText('FR', rawTag) or AnsiStartsText('FO', rawTag) or
         AnsiStartsText('FU', rawTag) or AnsiStartsText('TS', rawTag) or
         AnsiStartsText('RF', rawTag) or (UpperCase(rawTag) = 'CM') or
         (UpperCase(rawTag) = 'CL') or (UpperCase(rawTag) = 'CI') then
      begin
        if AnsiStartsText('WG', rawTag) or AnsiStartsText('WH', rawTag) or AnsiStartsText('WT', rawTag) then
        begin
          if AnsiStartsText('WG', rawTag) or AnsiStartsText('WH', rawTag) then
            wordHasStrong := True;
          AppendTagBeforeTrailingPunct(Result, '<' + rawTag + '>');
        end
        else
          Result := Result + '<' + rawTag + '>';
        i := tagEnd + 1;
        Continue;
      end;

      if (tagLower = 'num') or AnsiStartsStr('num ', tagLower) then
      begin
        closePos := FindClosingTagCI('</num>', tagEnd + 1);
        if closePos > 0 then
        begin
          hadSpaceBeforeTag := (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]);
          innerText := DecodeHTMLEntities(Copy(html, tagEnd + 1, closePos - tagEnd - 1));
          formattedTags := ClassifyAndFormatSuperscriptTag(innerText, False, isNTBook, wordHasStrong);
          if formattedTags <> '' then
            AppendTagBeforeTrailingPunct(Result, formattedTags);
          i := closePos + 6;
          Continue;
        end;
      end
      else if (tagLower = 'tvm') or AnsiStartsStr('tvm ', tagLower) or
              (tagLower = 'morph') or AnsiStartsStr('morph ', tagLower) then
      begin
        if AnsiStartsStr('morph', tagLower) then
          closePos := FindClosingTagCI('</morph>', tagEnd + 1)
        else
          closePos := FindClosingTagCI('</tvm>', tagEnd + 1);
        if closePos > 0 then
        begin
          hadSpaceBeforeTag := (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]);
          innerText := DecodeHTMLEntities(Copy(html, tagEnd + 1, closePos - tagEnd - 1));
          formattedTags := ClassifyAndFormatSuperscriptTag(innerText, True, isNTBook, wordHasStrong);
          if formattedTags <> '' then
            AppendTagBeforeTrailingPunct(Result, formattedTags);
          if AnsiStartsStr('morph', tagLower) then
            i := closePos + 8
          else
            i := closePos + 6;
          Continue;
        end;
      end
      else if (tagLower = 'sup') or AnsiStartsStr('sup ', tagLower) then
      begin
        closePos := FindClosingTagCI('</sup>', tagEnd + 1);
        if closePos > 0 then
        begin
          innerText := DecodeHTMLEntities(Copy(html, tagEnd + 1, closePos - tagEnd - 1));
          if LooksLikeStandaloneTagRun(innerText) or (Pos('morph', tagLower) > 0) or (Pos('tvm', tagLower) > 0) or (Pos('strong', tagLower) > 0) then
          begin
            hadSpaceBeforeTag := (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]);
            formattedTags := ClassifyAndFormatSuperscriptTag(
              innerText,
              (Pos('morph', tagLower) > 0) or (Pos('tvm', tagLower) > 0),
              isNTBook,
              wordHasStrong
            );
            if formattedTags <> '' then
              AppendTagBeforeTrailingPunct(Result, formattedTags);
          end
          else if Trim(innerText) <> '' then
            Result := Result + '<RF>' + Trim(innerText) + '<Rf>';
          i := closePos + 6;
          Continue;
        end;
      end
      else if AnsiStartsStr('a ', tagLower) then
      begin
        closePos := FindClosingTagCI('</a>', tagEnd + 1);
        if closePos > 0 then
        begin
          innerText := DecodeHTMLEntities(Copy(html, tagEnd + 1, closePos - tagEnd - 1));
          if (Pos('s://', tagLower) > 0) or (Pos('strong', tagLower) > 0) or
             (Pos('m://', tagLower) > 0) or (Pos('morph', tagLower) > 0) or
             LooksLikeStandaloneTagRun(innerText) then
          begin
            hadSpaceBeforeTag := (Length(Result) > 0) and (Result[Length(Result)] in [' ', #9]);
            formattedTags := ClassifyAndFormatSuperscriptTag(
              innerText,
              (Pos('m://', tagLower) > 0) or (Pos('morph', tagLower) > 0),
              isNTBook,
              wordHasStrong
            );
            if formattedTags <> '' then
              AppendTagBeforeTrailingPunct(Result, formattedTags);
          end
          else
            Result := Result + innerText;
          i := closePos + 4;
          Continue;
        end;
      end
      else if (tagLower = 'i') or (tagLower = 'em') then
        Result := Result + '<FI>'
      else if (tagLower = '/i') or (tagLower = '/em') then
        Result := Result + '<Fi>'
      else if (tagLower = 'b') or (tagLower = 'strong') then
        Result := Result + '<FO>'
      else if (tagLower = '/b') or (tagLower = '/strong') then
        Result := Result + '<Fo>'
      else if (tagLower = 'u') or (tagLower = 'ins') then
        Result := Result + '<FU>'
      else if (tagLower = '/u') or (tagLower = '/ins') then
        Result := Result + '<Fu>'
      else if (tagLower = 'red') then
        Result := Result + '<FR>'
      else if (tagLower = '/red') then
        Result := Result + '<Fr>'
      else if AnsiStartsStr('span', tagLower) or AnsiStartsStr('font', tagLower) then
      begin
        isRedSpan := (Pos('red', tagLower) > 0) or (Pos('woc', tagLower) > 0) or
                     (Pos('#ff0000', tagLower) > 0) or (Pos('#cc0000', tagLower) > 0);
        if isRedSpan then
        begin
          Result := Result + '<FR>';
          inRedSpan := True;
        end;
      end
      else if (tagLower = '/span') or (tagLower = '/font') then
      begin
        if inRedSpan then
        begin
          Result := Result + '<Fr>';
          inRedSpan := False;
        end;
      end
      else if (tagLower = 'h1') or (tagLower = 'h2') or (tagLower = 'h3') or (tagLower = 'h4') then
        Result := Result + '<TS1>'
      else if (tagLower = '/h1') or (tagLower = '/h2') or (tagLower = '/h3') or (tagLower = '/h4') then
        Result := Result + '<Ts>'
      else if (tagLower = 'note') or (tagLower = 'footnote') then
        Result := Result + '<RF>'
      else if (tagLower = '/note') or (tagLower = '/footnote') then
        Result := Result + '<Rf>'
      else if (tagLower = 'br') or (tagLower = 'br/') or (tagLower = 'br /') then
      begin
        Result := Result + '<CL>';
        wordHasStrong := False;
      end
      else if (tagLower = '/p') or (tagLower = '/div') then
      begin
        if (Length(Result) > 0) and not AnsiEndsStr('<CM>', Result) then
          Result := Result + '<CM>';
        wordHasStrong := False;
      end;

      i := tagEnd + 1;
      Continue;
    end;

    if html[i] in [#13, #10] then
    begin
      Inc(i);
      Continue;
    end;

    if html[i] in [' ', #9] then
      wordHasStrong := False
    else if hadSpaceBeforeTag and (Length(Result) > 0) and (Result[Length(Result)] = '>') and
            not (html[i] in ['.', ',', ';', ':', '!', '?', ')', ']', '"', '''', ' ', #9, '<']) then
    begin
      Result := Result + ' ';
      wordHasStrong := False;
    end;
    hadSpaceBeforeTag := False;

    Result := Result + html[i];
    Inc(i);
  end;

  if inRedSpan then
    Result := Result + '<Fr>';

  Result := DecodeHTMLEntities(Result);
end;

function CleanMorphTagContent(const rawTagContent: string): string;
var
  lemmaPos, q1, q2: integer;
begin
  Result := Trim(rawTagContent);
  lemmaPos := Pos(' l="', Result);
  if lemmaPos > 0 then
  begin
    q1 := lemmaPos + 4;
    q2 := PosEx('"', Result, q1);
    if q2 > 0 then
      Delete(Result, lemmaPos, q2 - lemmaPos + 1)
    else
      SetLength(Result, lemmaPos - 1);
    Result := Trim(Result);
  end;
end;

function EscapePlainTextToESwordRTF(const s: string): string;
var
  p: PChar;
  charLen: integer;
  cp: Cardinal;
  signed16: SmallInt;
  hiSurr, loSurr: SmallInt;
begin
  Result := '';
  if s = '' then
    Exit;
  p := PChar(s);
  while p^ <> #0 do
  begin
    cp := UTF8CodepointToUnicode(p, charLen);
    if charLen <= 0 then
      charLen := 1;
    if cp < $80 then
    begin
      case Chr(cp) of
        '\': Result := Result + '\\';
        '{': Result := Result + '\{';
        '}': Result := Result + '\}';
        #13, #10: ;
        else
          Result := Result + Chr(cp);
      end;
    end
    else if cp <= $FFFF then
    begin
      signed16 := SmallInt(Word(cp));
      Result := Result + '\u' + IntToStr(signed16) + '?';
    end
    else if cp <= $10FFFF then
    begin
      Dec(cp, $10000);
      hiSurr := SmallInt(Word($D800 + ((cp shr 10) and $3FF)));
      loSurr := SmallInt(Word($DC00 + (cp and $3FF)));
      Result := Result + '\u' + IntToStr(hiSurr) + '?\u' + IntToStr(loSurr) + '?';
    end;
    Inc(p, charLen);
  end;
end;

function EscapePlainTextToESwordHTML(const s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    case s[i] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      #13, #10: ;
      else
        Result := Result + s[i];
    end;
  end;
end;

function FormatInterlinearONTToESword(const ontText: string; isHTML: boolean): string;
var
  work: string;
  pStart, pEnd, tStart, tEnd: integer;
  block, seg, srcWord, strongTags, morphTags, destTrans, tagToken, morphCode: string;
begin
  Result := '';
  work := ontText;
  while Pos('<Q>', work) > 0 do
  begin
    pStart := Pos('<Q>', work);
    if pStart > 1 then
    begin
      seg := Copy(work, 1, pStart - 1);
      if isHTML then
        Result := Result + TESwordModule.ConvertONTToESwordHTML(seg)
      else
        Result := Result + TESwordModule.ConvertONTToESwordRTF(seg);
    end;
    Delete(work, 1, pStart + 2);
    pEnd := Pos('<q>', work);
    if pEnd > 0 then
    begin
      block := Copy(work, 1, pEnd - 1);
      Delete(work, 1, pEnd + 2);
    end
    else
    begin
      block := work;
      work := '';
    end;

    srcWord := '';
    strongTags := '';
    morphTags := '';
    destTrans := '';

    tStart := Pos('<H>', block);
    tEnd := Pos('<h>', block);
    if (tStart > 0) and (tEnd > tStart) then
    begin
      seg := Copy(block, tStart + 3, tEnd - (tStart + 3));
      while Pos('<W', seg) > 0 do
      begin
        pStart := Pos('<W', seg);
        pEnd := PosEx('>', seg, pStart);
        if pEnd > pStart then
        begin
          tagToken := Copy(seg, pStart + 2, pEnd - (pStart + 2));
          if isHTML then
            strongTags := strongTags + '<num>' + EscapePlainTextToESwordHTML(tagToken) + '</num>'
          else
            strongTags := strongTags + '{\cf15\super ' + EscapePlainTextToESwordRTF(tagToken) + '}';
          Delete(seg, pStart, pEnd - pStart + 1);
        end
        else
          Break;
      end;
      srcWord := Trim(seg);
    end;

    tStart := Pos('<T>', block);
    tEnd := Pos('<t>', block);
    if (tStart > 0) and (tEnd > tStart) then
    begin
      seg := Copy(block, tStart + 3, tEnd - (tStart + 3));
      while Pos('<WT', seg) > 0 do
      begin
        pStart := Pos('<WT', seg);
        pEnd := PosEx('>', seg, pStart);
        if pEnd > pStart then
        begin
          morphCode := CleanMorphTagContent(Copy(seg, pStart + 3, pEnd - (pStart + 3)));
          if morphCode <> '' then
          begin
            if isHTML then
              morphTags := morphTags + '<tvm>' + EscapePlainTextToESwordHTML(morphCode) + '</tvm>'
            else
              morphTags := morphTags + '{\cf11\super ' + EscapePlainTextToESwordRTF(morphCode) + '}';
          end;
          Delete(seg, pStart, pEnd - pStart + 1);
        end
        else
          Break;
      end;
      seg := Trim(StringReplace(seg, '<wt>', '', [rfReplaceAll, rfIgnoreCase]));
      if (morphTags = '') and (seg <> '') then
      begin
        if isHTML then
          morphTags := '<tvm>' + EscapePlainTextToESwordHTML(seg) + '</tvm>'
        else
          morphTags := '{\cf11\super ' + EscapePlainTextToESwordRTF(seg) + '}';
      end;
    end;

    tStart := Pos('<X>', block);
    tEnd := Pos('<x>', block);
    if (tStart > 0) and (tEnd > tStart) then
      destTrans := Trim(Copy(block, tStart + 3, tEnd - (tStart + 3)));

    if srcWord <> '' then
    begin
      if isHTML then
      begin
        Result := Result + EscapePlainTextToESwordHTML(srcWord) + strongTags + morphTags;
        if destTrans <> '' then
          Result := Result + ' <i>[' + EscapePlainTextToESwordHTML(destTrans) + ']</i>';
      end
      else
      begin
        Result := Result + EscapePlainTextToESwordRTF(srcWord) + strongTags + morphTags;
        if destTrans <> '' then
          Result := Result + ' {\i [' + EscapePlainTextToESwordRTF(destTrans) + ']}';
      end;
    end;
  end;

  if work <> '' then
  begin
    if isHTML then
      Result := Result + TESwordModule.ConvertONTToESwordHTML(work)
    else
      Result := Result + TESwordModule.ConvertONTToESwordRTF(work);
  end;
  Result := Trim(Result);
end;

function IsEncryptedScriptureSample(const s: string): boolean;
var
  i, len, ctrlCount: integer;
  b: Byte;
begin
  Result := False;
  len := Length(s);
  if len < 4 then
    Exit;
  ctrlCount := 0;
  for i := 1 to len do
  begin
    b := Byte(s[i]);
    if (b = 0) or (b in [1..8, 11..12, 14..31]) then
      Inc(ctrlCount);
  end;
  if (ctrlCount > 0) and ((ctrlCount * 20) >= len) then
    Result := True;
end;

function GetKJV1769FallbackVerse(book, chap, verse: integer): string;
begin
  Result := '';
  if (book = 13) and (chap = 4) then
  begin
    case verse of
      1: Result := 'The sons of Judah; Pharez, Hezron, and Carmi, and Hur, and Shobal.';
      2: Result := 'And Reaiah the son of Shobal begat Jahath; and Jahath begat Ahumai, and Lahad. These are the families of the Zorathites.';
      3: Result := 'And these were of the father of Etam; Jezreel, and Ishma, and Idbash: and the name of their sister was Hazelelponi:';
      4: Result := 'And Penuel the father of Gedor, and Ezer the father of Hushah. These are the sons of Hur, the firstborn of Ephratah, the father of Beth-lehem.';
      5: Result := 'And Ashur the father of Tekoa had two wives, Helah and Naarah.';
      6: Result := 'And Naarah bare him Ahuzzam, and Hepher, and Temeni, and Haahashtari. These were the sons of Naarah.';
      7: Result := 'And the sons of Helah were, Zereth, and Jezoar, and Ethnan.';
      8: Result := 'And Coz begat Anub, and Zobebah, and the families of Aharhel the son of Harum.';
      9: Result := 'And Jabez was more honourable than his brethren: and his mother called his name Jabez, saying, Because I bare him with sorrow.';
      10: Result := 'And Jabez called on the God of Israel, saying, Oh that thou wouldest bless me indeed, and enlarge my coast, and that thine hand might be with me, and that thou wouldest keep me from evil, that it may not grieve me! And God granted him that which he requested.';
      11: Result := 'And Chelub the brother of Shuah begat Mehir, which was the father of Eshton.';
      12: Result := 'And Eshton begat Beth-rapha, and Paseah, and Tehinnah the father of Ir-nahash. These are the men of Rechah.';
      13: Result := 'And the sons of Kenaz; Othniel, and Seraiah: and the sons of Othniel; Hathath.';
      14: Result := 'And Meonothai begat Ophrah: and Seraiah begat Joab, the father of the valley of Charashim; for they were craftsmen.';
      15: Result := 'And the sons of Caleb the son of Jephunneh; Iru, Elah, and Naam: and the sons of Elah, even Kenaz.';
      16: Result := 'And the sons of Jehaleleel; Ziph, and Ziphah, Tiria, and Asareel.';
      17: Result := 'And the sons of Ezra were, Jether, and Mered, and Epher, and Jalon: and she bare Miriam, and Shammai, and Ishbah the father of Eshtemoa.';
      18: Result := 'And his wife Jehudijah bare Jered the father of Gedor, and Heber the father of Socho, and Jekuthiel the father of Zanoah. And these are the sons of Bithiah the daughter of Pharaoh, which Mered took.';
      19: Result := 'And the sons of his wife Hodiah the sister of Naham, the father of Keilah the Garmite, and Eshtemoa the Maachathite.';
      20: Result := 'And the sons of Shimon were, Amnon, and Rinnah, Ben-hanan, and Tilon. And the sons of Ishi were, Zoheth, and Ben-zoheth.';
      21: Result := 'The sons of Shelah the son of Judah were, Er the father of Lecah, and Laadah the father of Mareshah, and the families of the house of them that wrought fine linen, of the house of Ashbea,';
      22: Result := 'And Jokim, and the men of Chozeba, and Joash, and Saraph, who had the dominion in Moab, and Jashubi-lehem. And these are ancient things.';
      23: Result := 'These were the potters, and those that dwelt among plants and hedges: there they dwelt with the king for his work.';
      24: Result := 'The sons of Simeon were, Nemuel, and Jamin, Jarib, Zerah, and Shaul:';
      25: Result := 'Shallum his son, Mibsam his son, Mishma his son.';
      26: Result := 'And the sons of Mishma; Hamuel his son, Zacchur his son, Shimei his son.';
      27: Result := 'And Shimei had sixteen sons and six daughters; but his brethren had not many children, neither did all their family multiply, like to the children of Judah.';
      28: Result := 'And they dwelt at Beer-sheba, and Moladah, and Hazar-shual,';
      29: Result := 'And at Bilhah, and at Ezem, and at Tolad,';
      30: Result := 'And at Bethuel, and at Hormah, and at Ziklag,';
      31: Result := 'And at Beth-marcaboth, and Hazar-susim, and at Beth-birei, and at Shaaraim. These were their cities unto the reign of David.';
      32: Result := 'And their villages were, Etam, and Ain, Rimmon, and Tochen, and Ashan, five cities:';
      33: Result := 'And all their villages that were round about the same cities, unto Baal. These were their habitations, and their genealogy.';
      34: Result := 'And Meshobab, and Jamlech, and Joshah the son of Amaziah,';
      35: Result := 'And Joel, and Jehu the son of Josibiah, the son of Seraiah, the son of Asiel,';
      36: Result := 'And Elioenai, and Jaakobah, and Jeshohaiah, and Asaiah, and Adiel, and Jesimiel, and Benaiah,';
      37: Result := 'And Ziza the son of Shiphi, the son of Allon, the son of Jedaiah, the son of Shimri, the son of Shemaiah;';
      38: Result := 'These mentioned by their names were princes in their families: and the house of their fathers increased greatly.';
      39: Result := 'And they went to the entrance of Gedor, even unto the east side of the valley, to seek pasture for their flocks.';
      40: Result := 'And they found fat pasture and good, and the land was wide, and quiet, and peaceable; for they of Ham had dwelt there of old.';
      41: Result := 'And these written by name came in the days of Hezekiah king of Judah, and smote their tents, and the habitations that were found there, and destroyed them utterly unto this day, and dwelt in their rooms: because there was pasture there for their flocks.';
      42: Result := 'And some of them, even of the sons of Simeon, five hundred men, went to mount Seir, having for their captains Pelatiah, and Neariah, and Rephaiah, and Uzziel, the sons of Ishi.';
      43: Result := 'And they smote the rest of the Amalekites that were escaped, and dwelt there unto this day.';
    end;
  end
  else if (book = 19) and (chap = 106) and (verse = 48) then
    Result := 'Blessed be the LORD God of Israel from everlasting to everlasting: and let all the people say, Amen. Praise ye the LORD.'
  else if (book = 28) and (chap = 14) then
  begin
    case verse of
      1: Result := 'O Israel, return unto the LORD thy God; for thou hast fallen by thine iniquity.';
      2: Result := 'Take with you words, and turn to the LORD: say unto him, Take away all iniquity, and receive us graciously: so will we render the calves of our lips.';
      3: Result := 'Asshur shall not save us; we will not ride upon horses: neither will we say any more to the work of our hands, Ye are our gods: for in thee the fatherless findeth mercy.';
      4: Result := 'I will heal their backsliding, I will love them freely: for mine anger is turned away from him.';
      5: Result := 'I will be as the dew unto Israel: he shall grow as the lily, and cast forth his roots as Lebanon.';
      6: Result := 'His branches shall spread, and his beauty shall be as the olive tree, and his smell as Lebanon.';
      7: Result := 'They that dwell under his shadow shall return; they shall revive as the corn, and grow as the vine: the scent thereof shall be as the wine of Lebanon.';
      8: Result := 'Ephraim shall say, What have I to do any more with idols? I have heard him, and observed him: I am like a green fir tree. From me is thy fruit found.';
      9: Result := 'Who is wise, and he shall understand these things? prudent, and he shall know them? for the ways of the LORD are right, and the just shall walk in them: but the transgressors shall fall therein.';
    end;
  end
  else if (book = 43) and (chap = 4) and (verse = 54) then
    Result := 'This is again the second miracle that Jesus did, when he was come out of Judaea into Galilee.'
  else if (book = 43) and (chap = 7) and (verse = 53) then
    Result := 'And every man went unto his own house.'
  else if (book = 47) and (chap = 13) and (verse = 14) then
    Result := 'The grace of the Lord Jesus Christ, and the love of God, and the communion of the Holy Ghost, be with you all. Amen.'
  else if (book = 66) and (chap = 22) and (verse = 21) then
    Result := 'The grace of our Lord Jesus Christ be with you all. Amen.';
end;

{ TESwordModule }

constructor TESwordModule.Create(const filename: string; ot, nt, strongs: boolean; const props: string; isHTML: boolean; apocrypha: boolean);
begin
  FFinalFileName := filename;
  FStagingFileName := GetStagingFileName(filename);
  FIsHTML := isHTML;
  FCommitted := False;

  if FileExists(FStagingFileName) then
    SysUtils.DeleteFile(FStagingFileName);

  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FConnection.Transaction := FTransaction;
  FConnection.DatabaseName := FStagingFileName;
  FConnection.Open;
  FTransaction.StartTransaction;

  CreateTables(ot, nt, strongs, apocrypha, props);

  FInsertVerse := TSQLQuery.Create(nil);
  FInsertVerse.Database := FConnection;
  FInsertVerse.Transaction := FTransaction;
  FInsertVerse.SQL.Text := 'INSERT INTO Bible (Book, Chapter, Verse, Scripture) VALUES (:b, :c, :v, :s)';
  FInsertVerse.Prepare;
end;

destructor TESwordModule.Destroy;
begin
  FreeAndNil(FInsertVerse);
  if Assigned(FTransaction) and FTransaction.Active then
  begin
    try
      FTransaction.Rollback;
    except
    end;
  end;
  if Assigned(FConnection) and FConnection.Connected then
  begin
    try
      FConnection.Close;
    except
    end;
  end;
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);

  if (not FCommitted) and FileExists(FStagingFileName) then
    SysUtils.DeleteFile(FStagingFileName);

  inherited Destroy;
end;

procedure TESwordModule.CreateTables(ot, nt, strongs, apocrypha: boolean; const props: string);
var
  propList: TStringList;
  qry: TSQLQuery;
  titleStr, abbrevStr, descStr, aboutStr, infoStr, fontStr, r2lStr: string;
  r2lBool: boolean;
begin
  FConnection.ExecuteDirect(
    'CREATE TABLE Details (' +
    'Description NVARCHAR(250), ' +
    'Abbreviation NVARCHAR(50), ' +
    'Information TEXT, ' +
    'Version INT, ' +
    'Font NVARCHAR(50), ' +
    'RightToLeft BOOL, ' +
    'OT BOOL, ' +
    'NT BOOL, ' +
    'Apocrypha BOOL, ' +
    'Strong BOOL)'
  );
  FConnection.ExecuteDirect('CREATE TABLE Bible (Book INT, Chapter INT, Verse INT, Scripture TEXT)');
  FConnection.ExecuteDirect('CREATE INDEX BookChapterVerseIndex ON Bible (Book, Chapter, Verse)');

  propList := TStringList.Create;
  qry := TSQLQuery.Create(nil);
  try
    propList.Text := props;
    titleStr := Trim(propList.Values['description']);
    if titleStr = '' then
      titleStr := Trim(propList.Values['title']);
    if titleStr = '' then
      titleStr := ChangeFileExt(ExtractFileName(FFinalFileName), '');

    abbrevStr := Trim(propList.Values['short.title']);
    if abbrevStr = '' then
      abbrevStr := Trim(propList.Values['id']);
    if abbrevStr = '' then
      abbrevStr := Copy(ChangeFileExt(ExtractFileName(FFinalFileName), ''), 1, 15);

    descStr := Trim(propList.Values['title']);
    aboutStr := Trim(propList.Values['about']);
    if (descStr <> '') and (aboutStr <> '') then
      infoStr := descStr + ' - ' + aboutStr
    else if aboutStr <> '' then
      infoStr := aboutStr
    else if descStr <> '' then
      infoStr := descStr
    else
      infoStr := titleStr;

    if FIsHTML then
      infoStr := StringReplace(infoStr, '\par', '<br/>', [rfReplaceAll, rfIgnoreCase]);

    fontStr := Trim(propList.Values['font']);
    if fontStr = '' then
      fontStr := 'DEFAULT';

    r2lStr := LowerCase(Trim(propList.Values['r2l']));
    r2lBool := (r2lStr = '1') or (r2lStr = 'true');

    qry.Database := FConnection;
    qry.Transaction := FTransaction;
    qry.SQL.Text :=
      'INSERT INTO Details (Description, Abbreviation, Information, Version, Font, RightToLeft, OT, NT, Apocrypha, Strong) ' +
      'VALUES (:desc, :abbr, :info, :ver, :font, :rtl, :ot, :nt, :apoc, :strong)';
    qry.Params.ParamByName('desc').AsString := titleStr;
    qry.Params.ParamByName('abbr').AsString := abbrevStr;
    qry.Params.ParamByName('info').AsString := infoStr;
    if FIsHTML then
      qry.Params.ParamByName('ver').AsInteger := 4
    else
      qry.Params.ParamByName('ver').AsInteger := 2;
    qry.Params.ParamByName('font').AsString := fontStr;
    qry.Params.ParamByName('rtl').AsBoolean := r2lBool;
    qry.Params.ParamByName('ot').AsBoolean := ot;
    qry.Params.ParamByName('nt').AsBoolean := nt;
    qry.Params.ParamByName('apoc').AsBoolean := apocrypha;
    qry.Params.ParamByName('strong').AsBoolean := strongs;
    qry.ExecSQL;
  finally
    qry.Free;
    propList.Free;
  end;
end;

procedure TESwordModule.AddVerse(const ontText: string; book, chapter, verse: integer);
var
  scripture: string;
begin
  if FIsHTML then
    scripture := ConvertONTToESwordHTML(ontText)
  else
    scripture := ConvertONTToESwordRTF(ontText);

  FInsertVerse.Params.ParamByName('b').AsInteger := book;
  FInsertVerse.Params.ParamByName('c').AsInteger := chapter;
  FInsertVerse.Params.ParamByName('v').AsInteger := verse;
  FInsertVerse.Params.ParamByName('s').AsString := scripture;
  FInsertVerse.ExecSQL;
end;

procedure TESwordModule.Commit;
begin
  FreeAndNil(FInsertVerse);
  if Assigned(FTransaction) and FTransaction.Active then
    FTransaction.Commit;
  if Assigned(FConnection) and FConnection.Connected then
    FConnection.Close;

  if not PublishStagedFile(FStagingFileName, FFinalFileName) then
    raise Exception.CreateFmt('Unable to publish exported e-Sword Bible module to "%s".', [FFinalFileName]);

  FCommitted := True;
end;

class function TESwordModule.IsUSFMFileName(const filename: string): boolean;
var
  lowName: string;
begin
  lowName := LowerCase(filename);
  Result := AnsiEndsStr('.usfm', lowName) or AnsiEndsStr('.sfm', lowName);
end;

class function TESwordModule.IsUSFXFileName(const filename: string): boolean;
var
  lowName: string;
begin
  lowName := LowerCase(filename);
  Result := AnsiEndsStr('.usfx', lowName) or AnsiEndsStr('.xml', lowName);
end;

class function TESwordModule.IsUSFMOrUSFXFileName(const filename: string): boolean;
begin
  Result := IsUSFMFileName(filename) or IsUSFXFileName(filename);
end;

class function TESwordModule.IsESwordFileName(const filename: string): boolean;
var
  lowName: string;
begin
  lowName := LowerCase(filename);
  Result := AnsiEndsStr('.bblx', lowName) or
            AnsiEndsStr('.bbli', lowName) or
            AnsiEndsStr('.bbl', lowName) or
            AnsiEndsStr('.mybible', lowName) or
            IsUSFMFileName(filename) or
            IsUSFXFileName(filename);
end;

class function TESwordModule.USFMBookCodeToID(const code: string): integer;
var
  u: string;
begin
  u := UpperCase(Trim(code));
  if Length(u) > 3 then
    u := Copy(u, 1, 3);
  Result := 0;
  case u of
    'GEN': Result := 1;
    'EXO': Result := 2;
    'LEV': Result := 3;
    'NUM': Result := 4;
    'DEU': Result := 5;
    'JOS': Result := 6;
    'JDG', 'JUDG': Result := 7;
    'RUT', 'RTH': Result := 8;
    '1SA': Result := 9;
    '2SA': Result := 10;
    '1KI': Result := 11;
    '2KI': Result := 12;
    '1CH': Result := 13;
    '2CH': Result := 14;
    'EZR': Result := 15;
    'NEH': Result := 16;
    'EST': Result := 17;
    'JOB': Result := 18;
    'PSA', 'PSM', 'PSS':
      if u = 'PSS' then Result := 85 else Result := 19;
    'PRO', 'PRV': Result := 20;
    'ECC', 'QOH': Result := 21;
    'SNG', 'SOL', 'SOS': Result := 22;
    'ISA': Result := 23;
    'JER': Result := 24;
    'LAM': Result := 25;
    'EZK', 'EZE': Result := 26;
    'DAN', 'DAG': Result := 27;
    'HOS': Result := 28;
    'JOL', 'JOE': Result := 29;
    'AMO': Result := 30;
    'OBA', 'OBD': Result := 31;
    'JON': Result := 32;
    'MIC': Result := 33;
    'NAM', 'NAH': Result := 34;
    'HAB': Result := 35;
    'ZEP': Result := 36;
    'HAG': Result := 37;
    'ZEC': Result := 38;
    'MAL': Result := 39;
    'MAT', 'MTT': Result := 40;
    'MRK', 'MAR', 'MK': Result := 41;
    'LUK': Result := 42;
    'JHN', 'JOH': Result := 43;
    'ACT': Result := 44;
    'ROM': Result := 45;
    '1CO': Result := 46;
    '2CO': Result := 47;
    'GAL': Result := 48;
    'EPH': Result := 49;
    'PHP', 'PHI': Result := 50;
    'COL': Result := 51;
    '1TH': Result := 52;
    '2TH': Result := 53;
    '1TI': Result := 54;
    '2TI': Result := 55;
    'TIT': Result := 56;
    'PHM': Result := 57;
    'HEB': Result := 58;
    'JAS', 'JAM': Result := 59;
    '1PE': Result := 60;
    '2PE': Result := 61;
    '1JN', '1JO': Result := 62;
    '2JN', '2JO': Result := 63;
    '3JN', '3JO': Result := 64;
    'JUD', 'JDE': Result := 65;
    'REV', 'APO': Result := 66;
    '1ES', 'ESD', '3EZ': Result := 67;
    '2ES', '4EZ', '5EZ', '6EZ': Result := 68;
    'TOB': Result := 69;
    'JDT': Result := 70;
    '1MA': Result := 71;
    '2MA': Result := 72;
    '3MA': Result := 73;
    '4MA': Result := 74;
    'MAN': Result := 75;
    'WIS': Result := 76;
    'SIR', 'ECN': Result := 77;
    'BAR': Result := 78;
    'LJE', 'epj': Result := 79;
    'S3Y', 'AZA': Result := 80;
    'SUS': Result := 81;
    'BEL': Result := 82;
    'ESG', 'ADE': Result := 83;
    'PS2': Result := 84;
    'PSS', 'PS3': Result := 85;
    'ODE', 'ODA': Result := 86;
    'LAO': Result := 87;
  end;
end;

class function TESwordModule.BookIDToUSFMCode(bookID: integer): string;
const
  CanonCodes: array[1..66] of string = (
    'GEN','EXO','LEV','NUM','DEU','JOS','JDG','RUT','1SA','2SA',
    '1KI','2KI','1CH','2CH','EZR','NEH','EST','JOB','PSA','PRO',
    'ECC','SNG','ISA','JER','LAM','EZK','DAN','HOS','JOL','AMO',
    'OBA','JON','MIC','NAM','HAB','ZEP','HAG','ZEC','MAL',
    'MAT','MRK','LUK','JHN','ACT','ROM','1CO','2CO','GAL','EPH',
    'PHP','COL','1TH','2TH','1TI','2TI','TIT','PHM','HEB','JAS',
    '1PE','2PE','1JN','2JN','3JN','JUD','REV'
  );
begin
  if (bookID >= 1) and (bookID <= 66) then
    Result := CanonCodes[bookID]
  else
  begin
    case bookID of
      67: Result := '1ES';
      68: Result := '2ES';
      69: Result := 'TOB';
      70: Result := 'JDT';
      71: Result := '1MA';
      72: Result := '2MA';
      73: Result := '3MA';
      74: Result := '4MA';
      75: Result := 'MAN';
      76: Result := 'WIS';
      77: Result := 'SIR';
      78: Result := 'BAR';
      79: Result := 'LJE';
      80: Result := 'S3Y';
      81: Result := 'SUS';
      82: Result := 'BEL';
      83: Result := 'ESG';
      84: Result := 'PS2';
      85: Result := 'PSS';
      86: Result := 'ODE';
      87: Result := 'LAO';
    else
      Result := 'XXA';
    end;
  end;
end;

class function TESwordModule.DetectESwordScope(const filename: string; out hasOT, hasNT: boolean): boolean;
var
  tempCopy, dummyApocInfo, dummyErr: string;
  dummyModulo: TStringList;
  dummyExtra: TESwordExtraVerseArray;
  conn: TSQLite3Connection;
  tran: TSQLTransaction;
  qry: TSQLQuery;
  fs: TFileStream;
  hdr: array[0..15] of AnsiChar;
begin
  Result := False;
  hasOT := False;
  hasNT := False;
  if not FileExists(filename) then
    Exit;

  if IsUSFMFileName(filename) then
  begin
    dummyModulo := TStringList.Create;
    try
      Result := LoadUSFMBibleWithApocrypha(filename, dummyModulo, dummyExtra, dummyApocInfo, hasOT, hasNT, dummyErr);
    finally
      dummyModulo.Free;
    end;
    Exit;
  end;

  if IsUSFXFileName(filename) then
  begin
    dummyModulo := TStringList.Create;
    try
      Result := LoadUSFXBibleWithApocrypha(filename, dummyModulo, dummyExtra, dummyApocInfo, hasOT, hasNT, dummyErr);
    finally
      dummyModulo.Free;
    end;
    Exit;
  end;

  FillChar(hdr, SizeOf(hdr), 0);
  try
    fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
    try
      if fs.Size < 16 then
        Exit;
      fs.ReadBuffer(hdr, 16);
    finally
      fs.Free;
    end;
  except
    Exit;
  end;

  if StrLComp(@hdr[0], 'SQLite format 3', 15) <> 0 then
    Exit;

  tempCopy := GetTempFileName(GetTempDir(False), 'ibib_esw_');
  if not CopyFileViaStream(filename, tempCopy) then
    tempCopy := filename;

  conn := TSQLite3Connection.Create(nil);
  tran := TSQLTransaction.Create(nil);
  qry := TSQLQuery.Create(nil);
  try
    try
      conn.Transaction := tran;
      conn.DatabaseName := tempCopy;
      conn.Open;
      qry.Database := conn;
      qry.Transaction := tran;

      qry.SQL.Text := 'SELECT MIN(Book) AS MinB, MAX(CASE WHEN Book <= 66 THEN Book ELSE 0 END) AS MaxCanonB, MAX(Book) AS MaxAllB FROM Bible WHERE Book >= 1 AND LENGTH(TRIM(Scripture)) > 0';
      qry.Open;
      if not qry.EOF then
      begin
        if ((not qry.FieldByName('MinB').IsNull) and (qry.FieldByName('MinB').AsInteger <= 39)) or
           ((not qry.FieldByName('MaxAllB').IsNull) and (qry.FieldByName('MaxAllB').AsInteger >= 67)) then
          hasOT := True;
        if (not qry.FieldByName('MaxCanonB').IsNull) and (qry.FieldByName('MaxCanonB').AsInteger >= 40) then
          hasNT := True;
      end;
      qry.Close;

      if (not hasOT) and (not hasNT) then
      begin
        qry.SQL.Text := 'SELECT * FROM Details LIMIT 1';
        qry.Open;
        if not qry.EOF then
        begin
          if Assigned(qry.FindField('OT')) then
            hasOT := qry.FieldByName('OT').AsBoolean or (qry.FieldByName('OT').AsInteger <> 0);
          if Assigned(qry.FindField('NT')) then
            hasNT := qry.FieldByName('NT').AsBoolean or (qry.FieldByName('NT').AsInteger <> 0);
        end;
        qry.Close;
      end;

      Result := hasOT or hasNT;
    except
      Result := False;
    end;
  finally
    qry.Free;
    tran.Free;
    conn.Free;
    if (tempCopy <> filename) and FileExists(tempCopy) then
      SysUtils.DeleteFile(tempCopy);
  end;
end;

class function TESwordModule.GetCanonicalLineIndex(book, chapter, verse: integer; out maxVerseInChap: integer): integer;
begin
  Result := GetCanonicalONTLineIndex(book, chapter, verse, maxVerseInChap);
end;

class function TESwordModule.IdentifyApocryphaBookName(bookID, maxChap, chap1Verses, chap2Verses, maxChapVerses: integer; const sampleText: string): string;
var
  lowSample: string;
begin
  lowSample := LowerCase(sampleText);

  if (Pos('josias held the feast', lowSample) > 0) or
     ((maxChap = 9) and (chap1Verses >= 50)) then
  begin
    Result := '1 Esdras';
    Exit;
  end;
  if (Pos('second book of the prophet esdras', lowSample) > 0) or
     (Pos('son of saraias', lowSample) > 0) or
     ((maxChap = 16) and ((chap1Verses = 40) or (maxChapVerses >= 70))) then
  begin
    Result := '2 Esdras';
    Exit;
  end;
  if (Pos('words of tobit', lowSample) > 0) or (Pos('tobiel', lowSample) > 0) or
     ((maxChap = 14) and (chap1Verses >= 20) and (chap1Verses <= 25)) then
  begin
    Result := 'Tobit';
    Exit;
  end;
  if ((Pos('twelfth year', lowSample) > 0) and ((Pos('nabuchodonosor', lowSample) > 0) or (Pos('arphaxad', lowSample) > 0))) or
     ((maxChap = 16) and (chap1Verses = 16) and (chap2Verses = 28)) then
  begin
    Result := 'Judith';
    Exit;
  end;
  if (Pos('love righteousness', lowSample) > 0) or (maxChap = 19) then
  begin
    Result := 'Wisdom';
    Exit;
  end;
  if (Pos('all wisdom cometh', lowSample) > 0) or (Pos('all wisdom is from the lord', lowSample) > 0) or (maxChap >= 50) then
  begin
    Result := 'Sirach';
    Exit;
  end;
  if (Pos('baruch the son of nerias', lowSample) > 0) or
     ((maxChap in [5, 6]) and (chap1Verses >= 20) and (chap1Verses <= 23)) then
  begin
    Result := 'Baruch';
    Exit;
  end;
  if ((Pos('alexander', lowSample) > 0) and (Pos('macedon', lowSample) > 0)) or
     ((maxChap = 16) and (chap1Verses >= 60)) then
  begin
    Result := '1 Maccabees';
    Exit;
  end;
  if ((Pos('brethren, the jews', lowSample) > 0) and (Pos('jerusalem', lowSample) > 0)) or
     (maxChap = 15) then
  begin
    Result := '2 Maccabees';
    Exit;
  end;
  if (Pos('philopator', lowSample) > 0) or
     ((maxChap = 7) and (chap1Verses >= 25)) then
  begin
    Result := '3 Maccabees';
    Exit;
  end;
  if (Pos('philosophical', lowSample) > 0) or
     ((maxChap = 18) and (chap1Verses >= 30)) then
  begin
    Result := '4 Maccabees';
    Exit;
  end;

  if maxChap = 1 then
  begin
    if ((Pos('epistle', lowSample) > 0) and (Pos('jeremy', lowSample) > 0)) or (maxChapVerses >= 70) then
    begin
      Result := 'Letter of Jeremiah';
      Exit;
    end;
    if (Pos('azarias', lowSample) > 0) or (Pos('midst of the fire', lowSample) > 0) or ((maxChapVerses >= 66) and (maxChapVerses <= 69)) then
    begin
      Result := 'Prayer of Azariah';
      Exit;
    end;
    if (Pos('joacim', lowSample) > 0) or (Pos('susanna', lowSample) > 0) or ((maxChapVerses >= 60) and (maxChapVerses <= 65)) then
    begin
      Result := 'Susanna';
      Exit;
    end;
    if (Pos('astyages', lowSample) > 0) or (Pos('bel ', lowSample) > 0) or ((maxChapVerses >= 40) and (maxChapVerses <= 45)) then
    begin
      Result := 'Bel and the Dragon';
      Exit;
    end;
    if (Pos('laodiceans', lowSample) > 0) or ((Pos('paul', lowSample) > 0) and (maxChapVerses >= 18) and (maxChapVerses <= 22)) then
    begin
      Result := 'Laodiceans';
      Exit;
    end;
    if (Pos('goliath', lowSample) > 0) or (Pos('small among my brethren', lowSample) > 0) or ((maxChapVerses >= 5) and (maxChapVerses <= 8)) then
    begin
      Result := 'Psalm 151';
      Exit;
    end;
    if (Pos('abraham, isaac, and jacob', lowSample) > 0) or (Pos('manasse', lowSample) > 0) or (maxChapVerses = 1) or ((maxChapVerses >= 12) and (maxChapVerses <= 17)) then
    begin
      Result := 'Prayer of Manasseh';
      Exit;
    end;
  end;

  if (Pos('mardocheus', lowSample) > 0) or (Pos('artexerxes', lowSample) > 0) then
  begin
    Result := 'Additions to Esther';
    Exit;
  end;
  if (maxChap = 18) and (chap1Verses <= 12) then
  begin
    Result := 'Psalms of Solomon';
    Exit;
  end;
  if maxChap = 14 then
  begin
    Result := 'Odes';
    Exit;
  end;

  if (bookID >= 67) and (bookID <= 87) then
    Result := DefaultApocryphaBookName(bookID)
  else
    Result := 'Apocrypha ' + IntToStr(bookID);
end;

class function TESwordModule.LoadESwordBibleToONT(const filename: string; modulo: TStringList; out hasOT, hasNT: boolean; out errMsg: string): boolean;
var
  dummyExtra: TESwordExtraVerseArray;
  dummyApocInfo: string;
begin
  SetLength(dummyExtra, 0);
  dummyApocInfo := '';
  Result := LoadESwordBibleWithApocrypha(filename, modulo, dummyExtra, dummyApocInfo, hasOT, hasNT, errMsg);
end;

class function TESwordModule.LoadESwordBibleWithApocrypha(const filename: string; modulo: TStringList; out extraVerses: TESwordExtraVerseArray; out apocryphaBooksInfo: string; out hasOT, hasNT: boolean; out errMsg: string): boolean;
var
  fs: TFileStream;
  hdr: array[0..15] of AnsiChar;
  tempCopy: string;
  conn: TSQLite3Connection;
  tran: TSQLTransaction;
  qry: TSQLQuery;
  titleStr, abbrevStr, infoStr, fontStr: string;
  versionNum: integer;
  r2lBool, preferHTML, isKJVLike: boolean;
  fldTitle, fldDesc, fldAbbr, fldInfo, fldComments, fldVer, fldFont, fldRTL, fldOT, fldNT: TField;
  fldBook, fldChap, fldVerse, fldScripture: TField;
  allVerses: array of string;
  i, book, chap, verse, vNum, lineIdx, maxVerse, nonEmptyCount, encryptedCount, extraCount: integer;
  rawScripture, ontLine, curPareID, bName: string;
  apocPresent: array[67..150] of boolean;
  apocMaxChap, apocChap1Verses, apocChap2Verses, apocMaxChapVerses: array[67..150] of integer;
  apocSample: array[67..150] of string;
begin
  Result := False;
  hasOT := False;
  hasNT := False;
  errMsg := '';
  SetLength(extraVerses, 0);
  apocryphaBooksInfo := '';
  extraCount := 0;
  for i := 67 to 150 do
  begin
    apocPresent[i] := False;
    apocMaxChap[i] := 0;
    apocChap1Verses[i] := 0;
    apocChap2Verses[i] := 0;
    apocMaxChapVerses[i] := 0;
    apocSample[i] := '';
  end;

  if not FileExists(filename) then
  begin
    errMsg := Format('File not found: "%s"', [filename]);
    Exit;
  end;

  if IsUSFMFileName(filename) then
  begin
    Result := LoadUSFMBibleWithApocrypha(filename, modulo, extraVerses, apocryphaBooksInfo, hasOT, hasNT, errMsg);
    Exit;
  end;

  if IsUSFXFileName(filename) then
  begin
    Result := LoadUSFXBibleWithApocrypha(filename, modulo, extraVerses, apocryphaBooksInfo, hasOT, hasNT, errMsg);
    Exit;
  end;

  FillChar(hdr, SizeOf(hdr), 0);
  try
    fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
    try
      if fs.Size < 16 then
      begin
        errMsg := SInvalidESwordDatabaseError;
        Exit;
      end;
      fs.ReadBuffer(hdr, 16);
    finally
      fs.Free;
    end;
  except
    on E: Exception do
    begin
      errMsg := E.Message;
      Exit;
    end;
  end;

  if StrLComp(@hdr[0], 'SQLite format 3', 15) <> 0 then
  begin
    if (Byte(hdr[0]) = 0) and (Byte(hdr[1]) = 1) and (Pos('Standard Jet DB', string(hdr)) > 0) then
      errMsg := SLegacyJetESwordModuleError
    else
      errMsg := SEncryptedESwordModuleError;
    Exit;
  end;

  tempCopy := GetTempFileName(GetTempDir(False), 'ibib_esw_');
  if not CopyFileViaStream(filename, tempCopy) then
    tempCopy := filename;

  titleStr := ChangeFileExt(ExtractFileName(filename), '');
  abbrevStr := titleStr;
  infoStr := '';
  fontStr := '';
  versionNum := 0;
  r2lBool := False;
  preferHTML := AnsiEndsText('.bbli', filename) or AnsiEndsText('.mybible', filename);

  conn := TSQLite3Connection.Create(nil);
  tran := TSQLTransaction.Create(nil);
  qry := TSQLQuery.Create(nil);
  try
    try
      conn.Transaction := tran;
      conn.DatabaseName := tempCopy;
      conn.Open;

      qry.Database := conn;
      qry.Transaction := tran;
      qry.PacketRecords := -1;

      try
        qry.SQL.Text := 'SELECT * FROM Details LIMIT 1';
        qry.Open;
        if not qry.EOF then
        begin
          fldTitle := qry.FindField('Title');
          fldDesc := qry.FindField('Description');
          fldAbbr := qry.FindField('Abbreviation');
          fldInfo := qry.FindField('Information');
          fldComments := qry.FindField('Comments');
          fldVer := qry.FindField('Version');
          fldFont := qry.FindField('Font');
          fldRTL := qry.FindField('RightToLeft');
          fldOT := qry.FindField('OT');
          fldNT := qry.FindField('NT');

          if Assigned(fldDesc) and (Trim(fldDesc.AsString) <> '') then
            titleStr := Trim(fldDesc.AsString);
          if Assigned(fldTitle) and (Trim(fldTitle.AsString) <> '') then
            titleStr := Trim(fldTitle.AsString);
          if Assigned(fldAbbr) and (Trim(fldAbbr.AsString) <> '') then
            abbrevStr := Trim(fldAbbr.AsString);
          if Assigned(fldInfo) and (Trim(fldInfo.AsString) <> '') then
            infoStr := Trim(fldInfo.AsString)
          else if Assigned(fldComments) and (Trim(fldComments.AsString) <> '') then
            infoStr := Trim(fldComments.AsString);

          if Assigned(fldVer) and (not fldVer.IsNull) then
          begin
            versionNum := fldVer.AsInteger;
            if versionNum >= 4 then
              preferHTML := True
            else if versionNum in [1, 2, 3] then
              preferHTML := False;
          end;

          if Assigned(fldFont) and (Trim(fldFont.AsString) <> '') and
             (not SameText(Trim(fldFont.AsString), 'DEFAULT')) then
            fontStr := Trim(fldFont.AsString);

          if Assigned(fldRTL) and (not fldRTL.IsNull) then
            r2lBool := fldRTL.AsBoolean or (fldRTL.AsInteger <> 0);

          if Assigned(fldOT) and (not fldOT.IsNull) then
            hasOT := fldOT.AsBoolean or (fldOT.AsInteger <> 0);
          if Assigned(fldNT) and (not fldNT.IsNull) then
            hasNT := fldNT.AsBoolean or (fldNT.AsInteger <> 0);
        end;
        qry.Close;
      except
        qry.Close;
      end;

      SetLength(allVerses, ESwordONTLines);
      for i := 0 to ESwordONTLines - 1 do
        allVerses[i] := '';

      hasOT := False;
      hasNT := False;
      nonEmptyCount := 0;
      encryptedCount := 0;

      qry.SQL.Text := 'SELECT * FROM Bible ORDER BY Book, Chapter, Verse';
      qry.Open;

      fldBook := qry.FindField('Book');
      if not Assigned(fldBook) then fldBook := qry.FindField('b');
      if (not Assigned(fldBook)) and (qry.Fields.Count >= 1) then fldBook := qry.Fields[0];

      fldChap := qry.FindField('Chapter');
      if not Assigned(fldChap) then fldChap := qry.FindField('c');
      if (not Assigned(fldChap)) and (qry.Fields.Count >= 2) then fldChap := qry.Fields[1];

      fldVerse := qry.FindField('Verse');
      if not Assigned(fldVerse) then fldVerse := qry.FindField('v');
      if (not Assigned(fldVerse)) and (qry.Fields.Count >= 3) then fldVerse := qry.Fields[2];

      fldScripture := qry.FindField('Scripture');
      if not Assigned(fldScripture) then fldScripture := qry.FindField('Text');
      if not Assigned(fldScripture) then fldScripture := qry.FindField('t');
      if (not Assigned(fldScripture)) and (qry.Fields.Count >= 4) then fldScripture := qry.Fields[3];

      if (not Assigned(fldBook)) or (not Assigned(fldChap)) or (not Assigned(fldVerse)) or (not Assigned(fldScripture)) then
      begin
        errMsg := SInvalidESwordDatabaseError;
        Exit;
      end;

      while not qry.EOF do
      begin
        book := fldBook.AsInteger;
        chap := fldChap.AsInteger;
        verse := fldVerse.AsInteger;
        rawScripture := fldScripture.AsString;

        if (book >= 1) and (Trim(rawScripture) <> '') then
        begin
          Inc(nonEmptyCount);
          if (nonEmptyCount <= 25) and IsEncryptedScriptureSample(rawScripture) then
          begin
            Inc(encryptedCount);
            if encryptedCount >= 5 then
            begin
              errMsg := SEncryptedESwordModuleError;
              Exit;
            end;
          end;

          if (book <= 66) and (chap >= 1) and (chap <= ESwordChaptersPerBook[book]) then
          begin
            lineIdx := GetCanonicalONTLineIndex(book, chap, verse, maxVerse);
            if lineIdx >= 0 then
            begin
              if book <= 39 then
                hasOT := True
              else
                hasNT := True;

              ontLine := ConvertESwordScriptureToONT(rawScripture, preferHTML, book >= 40);
              if ontLine <> '' then
              begin
                if verse = 0 then
                begin
                  if allVerses[lineIdx] <> '' then
                    allVerses[lineIdx] := '<TS1>' + ontLine + '<Ts>' + allVerses[lineIdx]
                  else
                    allVerses[lineIdx] := '<TS1>' + ontLine + '<Ts>';
                end
                else if (verse > maxVerse) and (allVerses[lineIdx] <> '') then
                  allVerses[lineIdx] := allVerses[lineIdx] + ' ' + ontLine
                else if (allVerses[lineIdx] <> '') and (verse <= maxVerse) then
                  allVerses[lineIdx] := allVerses[lineIdx] + ' ' + ontLine
                else
                  allVerses[lineIdx] := ontLine;
              end;
            end;
          end
          else if ((book <= 66) and (chap > ESwordChaptersPerBook[book])) or ((book >= 67) and (book <= 150)) then
          begin
            if (book <= 39) or (book >= 67) then
              hasOT := True
            else
              hasNT := True;

            ontLine := ConvertESwordScriptureToONT(rawScripture, preferHTML, False);
            if ontLine <> '' then
            begin
              if chap < 1 then
                chap := 1;
              if verse <= 0 then
              begin
                vNum := 1;
                ontLine := '<TS1>' + ontLine + '<Ts>';
              end
              else
                vNum := verse;

              curPareID := Format('%d,%d,%d', [book, chap, vNum]);
              if (extraCount > 0) and (extraVerses[extraCount - 1].PareID = curPareID) then
                extraVerses[extraCount - 1].ONTText := extraVerses[extraCount - 1].ONTText + ' ' + ontLine
              else
              begin
                if extraCount >= Length(extraVerses) then
                  SetLength(extraVerses, extraCount + 1024);
                extraVerses[extraCount].Book := book;
                extraVerses[extraCount].Chapter := chap;
                extraVerses[extraCount].Verse := vNum;
                extraVerses[extraCount].PareID := curPareID;
                extraVerses[extraCount].ONTText := ontLine;
                Inc(extraCount);
              end;

              if (book >= 67) and (book <= 150) then
              begin
                apocPresent[book] := True;
                if chap > apocMaxChap[book] then
                begin
                  apocMaxChap[book] := chap;
                  apocMaxChapVerses[book] := vNum;
                end
                else if (chap = apocMaxChap[book]) and (vNum > apocMaxChapVerses[book]) then
                  apocMaxChapVerses[book] := vNum;

                if (chap = 1) and (vNum > apocChap1Verses[book]) then
                  apocChap1Verses[book] := vNum;
                if (chap = 2) and (vNum > apocChap2Verses[book]) then
                  apocChap2Verses[book] := vNum;
                if (chap = 1) and (vNum <= 3) and (Length(apocSample[book]) < 300) then
                  apocSample[book] := apocSample[book] + ' ' + ontLine;
              end;
            end;
          end;
        end;

        qry.Next;
      end;
      qry.Close;
      SetLength(extraVerses, extraCount);

      isKJVLike := (Pos('king james', LowerCase(titleStr)) > 0) or
                   (Pos('authorized version', LowerCase(infoStr)) > 0) or
                   SameText(abbrevStr, 'KJV') or SameText(abbrevStr, 'KJVA');
      if isKJVLike then
      begin
        for verse := 1 to 43 do
        begin
          lineIdx := GetCanonicalONTLineIndex(13, 4, verse, maxVerse);
          if (lineIdx >= 0) and (allVerses[lineIdx] = '') then
            allVerses[lineIdx] := GetKJV1769FallbackVerse(13, 4, verse);
        end;
        lineIdx := GetCanonicalONTLineIndex(19, 106, 48, maxVerse);
        if (lineIdx >= 0) and (allVerses[lineIdx] = '') then
          allVerses[lineIdx] := GetKJV1769FallbackVerse(19, 106, 48);
        for verse := 1 to 9 do
        begin
          lineIdx := GetCanonicalONTLineIndex(28, 14, verse, maxVerse);
          if (lineIdx >= 0) and (allVerses[lineIdx] = '') then
            allVerses[lineIdx] := GetKJV1769FallbackVerse(28, 14, verse);
        end;
        lineIdx := GetCanonicalONTLineIndex(43, 4, 54, maxVerse);
        if (lineIdx >= 0) and (allVerses[lineIdx] = '') then
          allVerses[lineIdx] := GetKJV1769FallbackVerse(43, 4, 54);
        lineIdx := GetCanonicalONTLineIndex(43, 7, 53, maxVerse);
        if (lineIdx >= 0) and (allVerses[lineIdx] = '') then
          allVerses[lineIdx] := GetKJV1769FallbackVerse(43, 7, 53);
        lineIdx := GetCanonicalONTLineIndex(47, 13, 14, maxVerse);
        if (lineIdx >= 0) and (allVerses[lineIdx] = '') then
          allVerses[lineIdx] := GetKJV1769FallbackVerse(47, 13, 14);
        lineIdx := GetCanonicalONTLineIndex(66, 22, 21, maxVerse);
        if (lineIdx >= 0) and (allVerses[lineIdx] = '') then
          allVerses[lineIdx] := GetKJV1769FallbackVerse(66, 22, 21);
      end;

      for book := 67 to 150 do
      begin
        if apocPresent[book] then
        begin
          bName := IdentifyApocryphaBookName(
            book,
            apocMaxChap[book],
            apocChap1Verses[book],
            apocChap2Verses[book],
            apocMaxChapVerses[book],
            apocSample[book]
          );
          if apocryphaBooksInfo <> '' then
            apocryphaBooksInfo := apocryphaBooksInfo + ';';
          apocryphaBooksInfo := apocryphaBooksInfo + IntToStr(book) + '=' + bName;
        end;
      end;

      if (nonEmptyCount = 0) or ((not hasOT) and (not hasNT)) then
      begin
        errMsg := SInvalidESwordDatabaseError;
        Exit;
      end;

      modulo.BeginUpdate;
      try
        modulo.Clear;
        if hasOT and hasNT then
        begin
          for i := 0 to ESwordONTLines - 1 do
            modulo.Add(allVerses[i]);
        end
        else if hasOT then
        begin
          for i := 0 to ESwordOTLines - 1 do
            modulo.Add(allVerses[i]);
        end
        else
        begin
          for i := ESwordOTLines to ESwordONTLines - 1 do
            modulo.Add(allVerses[i]);
        end;

        titleStr := StringReplace(StringReplace(titleStr, #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]);
        abbrevStr := StringReplace(StringReplace(abbrevStr, #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]);

        modulo.Add('description=' + titleStr);
        modulo.Add('short.title=' + abbrevStr);
        modulo.Add('title=' + titleStr);
        if fontStr <> '' then
          modulo.Add('font=' + fontStr);
        if r2lBool then
          modulo.Add('r2l=1');
        if infoStr <> '' then
        begin
          infoStr := ConvertESwordScriptureToONT(infoStr, preferHTML, False);
          infoStr := StringReplace(StringReplace(infoStr, #13#10, '\par ', [rfReplaceAll]), #10, '\par ', [rfReplaceAll]);
          modulo.Add('about=' + infoStr);
        end;
      finally
        modulo.EndUpdate;
      end;

      Result := True;
    except
      on E: Exception do
      begin
        if (Pos('encrypted', LowerCase(E.Message)) > 0) or (Pos('not a database', LowerCase(E.Message)) > 0) then
          errMsg := SEncryptedESwordModuleError
        else
          errMsg := E.Message;
        Result := False;
      end;
    end;
  finally
    qry.Free;
    tran.Free;
    conn.Free;
    if (tempCopy <> filename) and FileExists(tempCopy) then
      SysUtils.DeleteFile(tempCopy);
  end;
end;

function HasESwordHTMLMarkup(const s: string): boolean;
var
  lowS: string;
begin
  Result := False;
  if (Pos('<', s) = 0) and (Pos('&', s) = 0) then
    Exit;
  lowS := LowerCase(s);
  Result := (Pos('<num', lowS) > 0) or (Pos('</num>', lowS) > 0) or
            (Pos('<tvm', lowS) > 0) or (Pos('</tvm>', lowS) > 0) or
            (Pos('<morph', lowS) > 0) or (Pos('<red', lowS) > 0) or
            (Pos('<i>', lowS) > 0) or (Pos('</i>', lowS) > 0) or
            (Pos('<em>', lowS) > 0) or (Pos('</em>', lowS) > 0) or
            (Pos('<b>', lowS) > 0) or (Pos('</b>', lowS) > 0) or
            (Pos('<strong>', lowS) > 0) or (Pos('<u>', lowS) > 0) or
            (Pos('<sup', lowS) > 0) or (Pos('</sup>', lowS) > 0) or
            (Pos('<a ', lowS) > 0) or (Pos('<span', lowS) > 0) or
            (Pos('<font', lowS) > 0) or (Pos('<br', lowS) > 0) or
            (Pos('<p>', lowS) > 0) or (Pos('</p>', lowS) > 0) or
            (Pos('<grk', lowS) > 0) or (Pos('<heb', lowS) > 0) or
            (Pos('<h1', lowS) > 0) or (Pos('<h2', lowS) > 0) or
            (Pos('<h3', lowS) > 0) or (Pos('<note', lowS) > 0) or
            (Pos('&nbsp;', lowS) > 0) or (Pos('&amp;', lowS) > 0) or
            (Pos('&lt;', lowS) > 0) or (Pos('&gt;', lowS) > 0) or
            (Pos('&quot;', lowS) > 0) or (Pos('&#', lowS) > 0);
end;

class function TESwordModule.ConvertESwordScriptureToONT(const rawScripture: string; preferHTML, isNTBook: boolean): string;
var
  s: string;
  hasRTF: boolean;
begin
  s := Trim(rawScripture);
  if s = '' then
  begin
    Result := '';
    Exit;
  end;

  if Pos(#239#187#191, s) = 1 then
    Delete(s, 1, 3);

  hasRTF := (Pos('{\', s) > 0) or (Pos('\cf', s) > 0) or (Pos('\super', s) > 0) or
            (Pos('\u', s) > 0) or (Pos('\par', s) > 0) or (Pos('\line', s) > 0) or
            (Pos('\i', s) > 0) or (Pos('\b', s) > 0) or (Pos('\''', s) > 0);

  if hasRTF and (not preferHTML or (Pos('{\', s) > 0) or (Pos('\cf', s) > 0) or (Pos('\super', s) > 0) or (Pos('\u', s) > 0)) then
    s := ParseESwordRTFToONT(s, isNTBook);

  if HasESwordHTMLMarkup(s) then
    s := ParseESwordHTMLToONT(s, isNTBook);

  s := StringReplace(s, #13, ' ', [rfReplaceAll]);
  s := StringReplace(s, #10, ' ', [rfReplaceAll]);
  Result := Trim(s);
end;

class function TESwordModule.ConvertONTToESwordRTF(const ontText: string): string;
var
  s: string;
  i, len, tagEnd: integer;
  rawTag, tagUpper, morphCode, plainRun: string;
begin
  s := ontText;
  if Pos(#239#187#191, s) = 1 then
    Delete(s, 1, 3);
  if Trim(s) = '' then
  begin
    Result := '';
    Exit;
  end;

  if Pos('<Q>', s) > 0 then
  begin
    Result := FormatInterlinearONTToESword(s, False);
    Exit;
  end;

  Result := '';
  plainRun := '';
  i := 1;
  len := Length(s);

  while i <= len do
  begin
    if s[i] = '<' then
    begin
      if plainRun <> '' then
      begin
        Result := Result + EscapePlainTextToESwordRTF(plainRun);
        plainRun := '';
      end;

      tagEnd := i + 1;
      while (tagEnd <= len) and (s[tagEnd] <> '>') do
        Inc(tagEnd);
      if tagEnd > len then
      begin
        Result := Result + EscapePlainTextToESwordRTF(Copy(s, i, len - i + 1));
        Break;
      end;

      rawTag := Copy(s, i + 1, tagEnd - i - 1);
      tagUpper := UpperCase(Trim(rawTag));

      if (Length(rawTag) >= 3) and ((AnsiStartsText('WG', rawTag)) or (AnsiStartsText('WH', rawTag))) and
         (rawTag[3] in ['0'..'9']) then
      begin
        Result := Result + '{\cf15\super ' + UpperCase(Copy(rawTag, 2, 1)) + Copy(rawTag, 3, Length(rawTag) - 2) + '}';
      end
      else if (Length(rawTag) > 2) and AnsiStartsText('WT', rawTag) then
      begin
        morphCode := CleanMorphTagContent(Copy(rawTag, 3, Length(rawTag) - 2));
        if morphCode <> '' then
          Result := Result + '{\cf11\super ' + EscapePlainTextToESwordRTF(morphCode) + '}';
      end
      else if rawTag = 'FI' then
        Result := Result + '{\i '
      else if rawTag = 'Fi' then
        Result := Result + '}'
      else if rawTag = 'FR' then
        Result := Result + '{\cf6 '
      else if rawTag = 'Fr' then
        Result := Result + '}'
      else if (rawTag = 'FO') or (tagUpper = 'B') then
        Result := Result + '{\b '
      else if (rawTag = 'Fo') or (tagUpper = '/B') then
        Result := Result + '}'
      else if (rawTag = 'FU') or (tagUpper = 'U') then
        Result := Result + '{\ul '
      else if (rawTag = 'Fu') or (tagUpper = '/U') then
        Result := Result + '}'
      else if AnsiStartsStr('TS', rawTag) then
        Result := Result + '{\b '
      else if rawTag = 'Ts' then
        Result := Result + '}\par '
      else if (rawTag = 'RF') or AnsiStartsStr('RF ', rawTag) then
        Result := Result + '{\cf10  [Note: '
      else if rawTag = 'Rf' then
        Result := Result + ']}'
      else if (tagUpper = 'CM') or (tagUpper = 'CI') then
        Result := Result + '\par '
      else if (tagUpper = 'CL') or (tagUpper = 'BR') or (tagUpper = 'BR/') then
        Result := Result + '\line ';

      i := tagEnd + 1;
      Continue;
    end;

    plainRun := plainRun + s[i];
    Inc(i);
  end;

  if plainRun <> '' then
    Result := Result + EscapePlainTextToESwordRTF(plainRun);

  Result := Trim(Result);
end;

class function TESwordModule.ConvertONTToESwordHTML(const ontText: string): string;
var
  s: string;
  i, len, tagEnd: integer;
  rawTag, tagUpper, morphCode, plainRun: string;
begin
  s := ontText;
  if Pos(#239#187#191, s) = 1 then
    Delete(s, 1, 3);
  if Trim(s) = '' then
  begin
    Result := '';
    Exit;
  end;

  if Pos('<Q>', s) > 0 then
  begin
    Result := FormatInterlinearONTToESword(s, True);
    Exit;
  end;

  Result := '';
  plainRun := '';
  i := 1;
  len := Length(s);

  while i <= len do
  begin
    if s[i] = '<' then
    begin
      if plainRun <> '' then
      begin
        Result := Result + EscapePlainTextToESwordHTML(plainRun);
        plainRun := '';
      end;

      tagEnd := i + 1;
      while (tagEnd <= len) and (s[tagEnd] <> '>') do
        Inc(tagEnd);
      if tagEnd > len then
      begin
        Result := Result + EscapePlainTextToESwordHTML(Copy(s, i, len - i + 1));
        Break;
      end;

      rawTag := Copy(s, i + 1, tagEnd - i - 1);
      tagUpper := UpperCase(Trim(rawTag));

      if (Length(rawTag) >= 3) and ((AnsiStartsText('WG', rawTag)) or (AnsiStartsText('WH', rawTag))) and
         (rawTag[3] in ['0'..'9']) then
      begin
        Result := Result + '<num>' + UpperCase(Copy(rawTag, 2, 1)) + Copy(rawTag, 3, Length(rawTag) - 2) + '</num>';
      end
      else if (Length(rawTag) > 2) and AnsiStartsText('WT', rawTag) then
      begin
        morphCode := CleanMorphTagContent(Copy(rawTag, 3, Length(rawTag) - 2));
        if morphCode <> '' then
          Result := Result + '<tvm>' + EscapePlainTextToESwordHTML(morphCode) + '</tvm>';
      end
      else if (rawTag = 'FI') or (tagUpper = 'I') then
        Result := Result + '<i>'
      else if (rawTag = 'Fi') or (tagUpper = '/I') then
        Result := Result + '</i>'
      else if rawTag = 'FR' then
        Result := Result + '<red>'
      else if rawTag = 'Fr' then
        Result := Result + '</red>'
      else if (rawTag = 'FO') or (tagUpper = 'B') then
        Result := Result + '<b>'
      else if (rawTag = 'Fo') or (tagUpper = '/B') then
        Result := Result + '</b>'
      else if (rawTag = 'FU') or (tagUpper = 'U') then
        Result := Result + '<u>'
      else if (rawTag = 'Fu') or (tagUpper = '/U') then
        Result := Result + '</u>'
      else if AnsiStartsStr('TS', rawTag) then
        Result := Result + '<h3>'
      else if rawTag = 'Ts' then
        Result := Result + '</h3>'
      else if (rawTag = 'RF') or AnsiStartsStr('RF ', rawTag) then
        Result := Result + ' <span class="note">[Note: '
      else if rawTag = 'Rf' then
        Result := Result + ']</span>'
      else if (tagUpper = 'CM') or (tagUpper = 'CI') or (tagUpper = 'CL') or (tagUpper = 'BR') or (tagUpper = 'BR/') then
        Result := Result + '<br/>';

      i := tagEnd + 1;
      Continue;
    end;

    plainRun := plainRun + s[i];
    Inc(i);
  end;

  if plainRun <> '' then
    Result := Result + EscapePlainTextToESwordHTML(plainRun);

  Result := Trim(Result);
end;


function ExtractLeadingInt(const s: string; defaultVal: integer): integer;
var
  i, startIdx: integer;
  numStr: string;
begin
  Result := defaultVal;
  numStr := '';
  startIdx := 1;
  while (startIdx <= Length(s)) and (s[startIdx] in [' ', #9, '"', '''']) do
    Inc(startIdx);
  for i := startIdx to Length(s) do
  begin
    if s[i] in ['0'..'9'] then
      numStr := numStr + s[i]
    else
      Break;
  end;
  if numStr <> '' then
    Result := StrToIntDef(numStr, defaultVal);
end;

function ExtractXMLAttrValue(const rawTag, attrName: string): string;
var
  lowTag, lowAttr: string;
  p, k, len: integer;
  quoteCh: Char;
begin
  Result := '';
  lowTag := LowerCase(rawTag);
  lowAttr := LowerCase(attrName);
  len := Length(rawTag);
  p := 1;
  while p <= len do
  begin
    p := PosEx(lowAttr, lowTag, p);
    if p = 0 then
      Exit;
    if (p = 1) or (lowTag[p - 1] in [' ', #9, #13, #10]) then
    begin
      k := p + Length(lowAttr);
      while (k <= len) and (rawTag[k] in [' ', #9]) do
        Inc(k);
      if (k <= len) and (rawTag[k] = '=') then
      begin
        Inc(k);
        while (k <= len) and (rawTag[k] in [' ', #9]) do
          Inc(k);
        if k <= len then
        begin
          if rawTag[k] in ['"', ''''] then
          begin
            quoteCh := rawTag[k];
            Inc(k);
            p := k;
            while (k <= len) and (rawTag[k] <> quoteCh) do
              Inc(k);
            Result := DecodeHTMLEntities(Copy(rawTag, p, k - p));
            Exit;
          end
          else
          begin
            p := k;
            while (k <= len) and not (rawTag[k] in [' ', #9, '/', '>']) do
              Inc(k);
            Result := DecodeHTMLEntities(Copy(rawTag, p, k - p));
            Exit;
          end;
        end;
      end;
    end;
    Inc(p, Length(lowAttr));
  end;
end;

function StripAllXMLTags(const s: string): string;
var
  i, len: integer;
  inTag: boolean;
begin
  Result := '';
  len := Length(s);
  inTag := False;
  for i := 1 to len do
  begin
    if s[i] = '<' then
      inTag := True
    else if s[i] = '>' then
      inTag := False
    else if not inTag then
      Result := Result + s[i];
  end;
  Result := DecodeHTMLEntities(Result);
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  Result := Trim(Result);
end;

function FormatStrongAndMorphAttrsToONT(const strongVal, morphVal, lemmaVal: string; isNTBook: boolean): string;
var
  sTags, lClean, mTok, lTok: string;
  dummyHasStrong: boolean;
  colonPos, k: integer;
  morphs, lemmas: TStringList;
begin
  Result := '';
  dummyHasStrong := False;
  if Trim(strongVal) <> '' then
  begin
    sTags := ClassifyAndFormatSuperscriptTag(Trim(strongVal), False, isNTBook, dummyHasStrong);
    Result := Result + sTags;
  end;
  lClean := StringReplace(Trim(lemmaVal), '"', '', [rfReplaceAll]);
  morphs := TStringList.Create;
  lemmas := TStringList.Create;
  try
    // several morphology codes (and lemmas) are separated by blanks
    morphs.Delimiter := ' ';
    morphs.StrictDelimiter := True;
    morphs.DelimitedText := Trim(morphVal);
    lemmas.Delimiter := ' ';
    lemmas.StrictDelimiter := True;
    lemmas.DelimitedText := lClean;
    for k := 0 to morphs.Count - 1 do
    begin
      mTok := Trim(morphs[k]);
      if mTok = '' then
        Continue;
      colonPos := Pos(':', mTok);
      if colonPos > 0 then
        Delete(mTok, 1, colonPos);
      mTok := Trim(mTok);
      if mTok = '' then
        Continue;
      lTok := '';
      if (lemmas.Count = morphs.Count) then
        lTok := lemmas[k]
      else if lemmas.Count > 0 then
        lTok := lemmas[0];
      if lTok <> '' then
        Result := Result + '<WT' + mTok + ' l="' + lTok + '">'
      else
        Result := Result + '<WT' + mTok + '>';
    end;
  finally
    lemmas.Free;
    morphs.Free;
  end;
end;

{ two verses on one line: a blank between them, except beside a paragraph boundary }
function JoinVerseTexts(const A, B: string): string;
  function IsBoundary(const T: string): Boolean;
  begin
    Result := (T = '<CM>') or (T = '<CI>') or (T = '<CL>');
  end;
begin
  if (A = '') or (B = '') then
    Result := A + B
  else if IsBoundary(Copy(A, Length(A) - 3, 4)) or IsBoundary(Copy(B, 1, 4)) then
    Result := A + B
  else
    Result := A + ' ' + B;
end;

{ ======================================================================
  USFM 3.0 reader
  The file is cut into logical lines (one block marker each, see
  SplitUSFMLogicalLines); the inline markers of a line are turned into theWord
  tags by TUSFMInlineReader; paragraph / poetry markers become <CM>/<CL> plus
  <PIn> (USFMParagraphTags) and are placed by the loader.
  ====================================================================== }

const
  USFMBookNames: array[1..66] of string = (
    'Genesis', 'Exodus', 'Leviticus', 'Numbers', 'Deuteronomy', 'Joshua', 'Judges', 'Ruth',
    '1 Samuel', '2 Samuel', '1 Kings', '2 Kings', '1 Chronicles', '2 Chronicles', 'Ezra',
    'Nehemiah', 'Esther', 'Job', 'Psalms', 'Proverbs', 'Ecclesiastes', 'Song of Solomon',
    'Isaiah', 'Jeremiah', 'Lamentations', 'Ezekiel', 'Daniel', 'Hosea', 'Joel', 'Amos',
    'Obadiah', 'Jonah', 'Micah', 'Nahum', 'Habakkuk', 'Zephaniah', 'Haggai', 'Zechariah',
    'Malachi', 'Matthew', 'Mark', 'Luke', 'John', 'Acts', 'Romans', '1 Corinthians',
    '2 Corinthians', 'Galatians', 'Ephesians', 'Philippians', 'Colossians', '1 Thessalonians',
    '2 Thessalonians', '1 Timothy', '2 Timothy', 'Titus', 'Philemon', 'Hebrews', 'James',
    '1 Peter', '2 Peter', '1 John', '2 John', '3 John', 'Jude', 'Revelation');

type
  TUSFMAlnEntry = record
    Strong, Lemma, Morph: string;
  end;

  { Turns the inline markers of USFM lines into theWord tags. The open
    character styles and the open alignment milestones are kept between calls. }
  TUSFMInlineReader = class
  private
    FOut: string;
    FNT: Boolean;
    FBook: Integer;
    FStyles: TStringList;
    FAln: array of TUSFMAlnEntry;      // milestones open now
    FAlnGroup: array of TUSFMAlnEntry; // every milestone of the open group, in order
    FAlnStart: Integer;                // where the group's text starts in FOut
    FJesus: Integer;                   // open \qt-s who="Jesus"
    procedure OpenStyle(const AName: string);
    procedure CloseStyle(const AName: string);
    procedure PushAln(const Attrs: string);
    procedure PopAln;
    procedure WordMarker(const Inner: string; Plain: Boolean);
    procedure Milestone(const AName, Attrs: string);
    function CrossRefs(const Raw: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    { Book is the book being read (for cross-references without a book). }
    function Convert(const S: string; IsNT: Boolean; Book: Integer): string;
    function CloseTags: string;   // closers for the open styles (they stay open)
    function ReopenTags: string;  // openers for the open styles
    procedure ClearStyles;
  end;

function USFMStyleOpenTag(const AName: string): string;
begin
  case AName of
    'wj': Result := '<FR>';
    'add': Result := '<FI>';
    'qt': Result := '<FO>';
    'it', 'em', 'tl': Result := '<i>';
    'bd': Result := '<b>';
    'bdit': Result := '<b><i>';
    'sup': Result := '<sup>';
  else
    Result := '';
  end;
end;

function USFMStyleCloseTag(const AName: string): string;
begin
  case AName of
    'wj': Result := '<Fr>';
    'add': Result := '<Fi>';
    'qt': Result := '<Fo>';
    'it', 'em', 'tl': Result := '</i>';
    'bd': Result := '</b>';
    'bdit': Result := '</i></b>';
    'sup': Result := '</sup>';
  else
    Result := '';
  end;
end;

function StripTrailingDigits(const AName: string): string;
var
  N: Integer;
begin
  N := Length(AName);
  while (N > 1) and (AName[N] in ['0'..'9']) do
    Dec(N);
  Result := Copy(AName, 1, N);
end;

function TrailingNumber(const AName: string; ADefault: Integer): Integer;
var
  N: Integer;
begin
  N := Length(AName);
  while (N > 0) and (AName[N] in ['0'..'9']) do
    Dec(N);
  Result := StrToIntDef(Copy(AName, N + 1, Length(AName)), ADefault);
end;

{ the markers that start a line of their own (everything else is inline) }
function IsUSFMBlockMarker(const AName: string): Boolean;
begin
  Result := False;
  case StripTrailingDigits(AName) of
    'id', 'usfm', 'ide', 'sts', 'rem', 'h', 'toc', 'mt', 'mte', 'ms', 'mr', 's', 'sr', 'r', 'd',
    'sp', 'cl', 'cd', 'cp', 'c', 'v', 'p', 'm', 'po', 'pr', 'cls', 'pmo', 'pm', 'pmc', 'pmr', 'pi',
    'mi', 'nb', 'pc', 'ph', 'b', 'q', 'qr', 'qc', 'qa', 'qm', 'qd', 'lh', 'li', 'lf', 'lim', 'tr',
    'esb', 'esbe', 'cat', 'periph', 'ie', 'im', 'imi', 'imq', 'ip', 'ipi', 'ipq', 'ipr', 'iq',
    'is', 'imt', 'imte', 'io', 'iot', 'ili', 'ib', 'iex', 'lit', 'pb', 'sd', 'pro':
      Result := True;
  end;
end;

{ Cuts USFM text into logical lines: a block marker (\p, \v, \c, \q1 ...) always
  starts a new line, even in the middle of a physical line; physical line
  breaks inside a paragraph are just spaces. }
procedure SplitUSFMLogicalLines(const Text: string; Lines: TStrings);
var
  I, J, Len, NmStart: Integer;
  Cur, Name: string;
  Plus: Boolean;
begin
  Lines.Clear;
  Cur := '';
  Len := Length(Text);
  I := 1;
  if (Len >= 3) and (Text[1] = #239) and (Text[2] = #187) and (Text[3] = #191) then
    I := 4;
  while I <= Len do
  begin
    if Text[I] = '\' then
    begin
      J := I + 1;
      Plus := (J <= Len) and (Text[J] = '+');
      if Plus then
        Inc(J);
      NmStart := J;
      while (J <= Len) and (Text[J] in ['a'..'z', 'A'..'Z', '0'..'9', '-']) do
        Inc(J);
      Name := LowerCase(Copy(Text, NmStart, J - NmStart));
      if (not Plus) and (Name <> '') and ((J > Len) or (Text[J] <> '*')) and IsUSFMBlockMarker(Name) then
      begin
        if Trim(Cur) <> '' then
          Lines.Add(Trim(Cur));
        Cur := '';
      end;
      Cur := Cur + Copy(Text, I, J - I);
      I := J;
      Continue;
    end;
    if Text[I] in [#13, #10] then
    begin
      if (Cur <> '') and (Cur[Length(Cur)] <> ' ') then
        Cur := Cur + ' ';
    end
    else
      Cur := Cur + Text[I];
    Inc(I);
  end;
  if Trim(Cur) <> '' then
    Lines.Add(Trim(Cur));
end;

{ Paragraph and poetry markers as theWord paragraph tags: ABound is the
  boundary (<CM>, <CL> or '') and API the indent (<PIn> or ''). }
function USFMParagraphTags(const AName: string; out ABound, API: string): Boolean;
var
  Base: string;
  N: Integer;

  function Indent(Level: Integer): string;
  begin
    if Level < 1 then
      Level := 1;
    if Level > 7 then
      Level := 7;
    Result := '<PI' + IntToStr(Level) + '>';
  end;

begin
  Result := True;
  ABound := '';
  API := '';
  Base := StripTrailingDigits(AName);
  N := TrailingNumber(AName, 0);
  case Base of
    'p', 'pc', 'po', 'b': ABound := '<CM>';
    'm', 'pr', 'cls', 'lh', 'lf', 'qd', 'tr': ABound := '<CL>';
    'pmo', 'pm', 'pmc', 'pmr', 'mi': begin ABound := '<CM>'; API := Indent(1); end;
    'pi', 'ph': begin ABound := '<CM>'; API := Indent(N); end;
    'q': begin ABound := '<CL>'; API := Indent(N); end;
    'qr': begin ABound := '<CL>'; API := Indent(3); end;
    'qc': begin ABound := '<CL>'; API := Indent(2); end;
    'qm': begin ABound := '<CL>'; API := Indent(N + 1); end;
    'li': begin ABound := '<CL>'; API := Indent(N); end;
    'lim': begin ABound := '<CL>'; API := Indent(N + 1); end;
    'nb': ; // no break: the paragraph goes on
  else
    Result := False;
  end;
end;

{ 1..3 for the heading markers (the level of the <TSn> tag), 0 for others }
function USFMHeadingLevel(const AName: string): Integer;
var
  Base: string;
  N: Integer;
begin
  Result := 0;
  Base := StripTrailingDigits(AName);
  N := TrailingNumber(AName, 1);
  if (Base = 's') or (Base = 'ms') then
  begin
    if N <= 1 then
      Result := 1
    else if N = 2 then
      Result := 2
    else
      Result := 3;
  end
  else if Base = 'd' then
    Result := 2
  else if Base = 'qa' then
    Result := 3;
end;

function LooksLikeStrongNumber(const S: string): Boolean;
var
  T: string;
  I: Integer;
begin
  T := Trim(S);
  Result := False;
  if T = '' then
    Exit;
  if T[1] in ['G', 'g', 'H', 'h'] then
    Delete(T, 1, 1);
  if T = '' then
    Exit;
  for I := 1 to Length(T) do
    if not (T[I] in ['0'..'9', ',', ' ', ';']) then
    begin
      if (I = Length(T)) and (T[I] in ['a'..'z', 'A'..'Z']) and (I > 1) then
        Continue;
      Exit;
    end;
  Result := True;
end;

{ "b:G1722 G3056" and "H1234,H5678" -> "G1722 G3056" / "H1234 H5678" }
function NormalizeStrongList(const S: string): string;
var
  I, P: Integer;
  Tok: string;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    while (I <= Length(S)) and (S[I] in [' ', ',', ';', #9]) do
      Inc(I);
    P := I;
    while (I <= Length(S)) and not (S[I] in [' ', ',', ';', #9]) do
      Inc(I);
    Tok := Copy(S, P, I - P);
    P := LastDelimiter(':', Tok);
    if P > 0 then
      Delete(Tok, 1, P);
    if Tok <> '' then
    begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + Tok;
    end;
  end;
end;

{ A \w attribute string: key="value" pairs; a lone value is the lemma (USFM 3.0),
  or a Strong's number (older files). }
function ParseUSFMAttributeString(const attrStr: string; out strongVal, morphVal, lemmaVal: string): boolean;
var
  p, len, k, eqPos: integer;
  key, val: string;
  quoteCh: Char;
begin
  Result := False;
  strongVal := '';
  morphVal := '';
  lemmaVal := '';
  len := Length(attrStr);
  if len = 0 then
    Exit;

  if Pos('=', attrStr) = 0 then
  begin
    if LooksLikeStrongNumber(attrStr) then
      strongVal := Trim(attrStr)
    else
      lemmaVal := Trim(attrStr);
    Result := (strongVal <> '') or (lemmaVal <> '');
    Exit;
  end;

  p := 1;
  while p <= len do
  begin
    while (p <= len) and (attrStr[p] in [' ', #9]) do
      Inc(p);
    if p > len then
      Break;
    eqPos := PosEx('=', attrStr, p);
    if eqPos = 0 then
      Break;
    key := LowerCase(Trim(Copy(attrStr, p, eqPos - p)));
    k := eqPos + 1;
    while (k <= len) and (attrStr[k] in [' ', #9]) do
      Inc(k);
    val := '';
    if (k <= len) and (attrStr[k] in ['"', '''']) then
    begin
      quoteCh := attrStr[k];
      Inc(k);
      p := k;
      while (k <= len) and (attrStr[k] <> quoteCh) do
        Inc(k);
      val := Copy(attrStr, p, k - p);
      if k <= len then
        Inc(k);
    end
    else
    begin
      p := k;
      while (k <= len) and not (attrStr[k] in [' ', #9]) do
        Inc(k);
      val := Copy(attrStr, p, k - p);
    end;
    p := k;

    if (key = 'strong') or (key = 's') or (key = 'str') or (key = 'x-strong') then
      strongVal := NormalizeStrongList(val)
    else if (key = 'x-morph') or (key = 'morph') or (key = 'm') or (key = 'oshm') then
      morphVal := val
    else if (key = 'lemma') or (key = 'l') or (key = 'x-lemma') then
      lemmaVal := val;
    Result := True;
  end;
end;

{ the text of a footnote / endnote without its markers, caller and attributes }
function CleanUSFMFootnoteToPlain(const rawNote: string): string;
var
  i, len, mStart: integer;
  mName: string;
begin
  Result := '';
  len := Length(rawNote);
  i := 1;
  while (i <= len) and (rawNote[i] in [' ', #9]) do
    Inc(i);
  if (i <= len) and not (rawNote[i] in ['\']) then
  begin
    while (i <= len) and not (rawNote[i] in [' ', #9, '\']) do
      Inc(i);
    while (i <= len) and (rawNote[i] in [' ', #9]) do
      Inc(i);
  end;

  while i <= len do
  begin
    if rawNote[i] = '\' then
    begin
      mStart := i + 1;
      if (mStart <= len) and (rawNote[mStart] = '+') then
        Inc(mStart);
      i := mStart;
      while (i <= len) and (rawNote[i] in ['a'..'z', 'A'..'Z', '0'..'9', '-']) do
        Inc(i);
      mName := LowerCase(Copy(rawNote, mStart, i - mStart));
      if (i <= len) and (rawNote[i] = '*') then
        Inc(i)
      else if (i <= len) and (rawNote[i] = ' ') then
        Inc(i);
      if (mName = 'fr') or (mName = 'xo') or (mName = 'fv') then
      begin
        while (i <= len) and (rawNote[i] <> '\') do
          Inc(i);
      end;
      Continue;
    end;
    if rawNote[i] = '|' then
    begin
      // attributes of a word or link inside the note
      while (i <= len) and (rawNote[i] <> '\') do
        Inc(i);
      Continue;
    end;
    Result := Result + rawNote[i];
    Inc(i);
  end;
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '~', ' ', [rfReplaceAll]);
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
  Result := Trim(Result);
end;

{ the book of a reference such as "Gen", "1 Cor", "Matthew" (0 if unknown) }
function USFMReferenceBookID(const AName: string): Integer;
var
  Norm, Cand: string;
  I: Integer;
begin
  Result := 0;
  Norm := '';
  for I := 1 to Length(AName) do
    if AName[I] in ['0'..'9', 'a'..'z', 'A'..'Z'] then
      Norm := Norm + LowerCase(AName[I]);
  if Length(Norm) < 2 then
    Exit;
  if Length(Norm) = 3 then
  begin
    Result := TESwordModule.USFMBookCodeToID(Norm);
    if (Result >= 1) and (Result <= 66) then
      Exit;
    Result := 0;
  end;
  case Norm of
    'mt': Exit(40);
    'mk': Exit(41);
    'lk': Exit(42);
    'jn': Exit(43);
    'rm': Exit(45);
    'ph': Exit(50);
  end;
  for I := 1 to 66 do
  begin
    Cand := LowerCase(StringReplace(USFMBookNames[I], ' ', '', [rfReplaceAll]));
    if Copy(Cand, 1, Length(Norm)) = Norm then
      Exit(I);
  end;
end;

{ ---- TUSFMInlineReader ---- }

constructor TUSFMInlineReader.Create;
begin
  inherited Create;
  FStyles := TStringList.Create;
  Reset;
end;

destructor TUSFMInlineReader.Destroy;
begin
  FStyles.Free;
  inherited Destroy;
end;

procedure TUSFMInlineReader.Reset;
begin
  FStyles.Clear;
  SetLength(FAln, 0);
  SetLength(FAlnGroup, 0);
  FAlnStart := 0;
  FJesus := 0;
end;

procedure TUSFMInlineReader.ClearStyles;
begin
  FStyles.Clear;
  FJesus := 0;
end;

function TUSFMInlineReader.CloseTags: string;
var
  I: Integer;
begin
  Result := '';
  for I := FStyles.Count - 1 downto 0 do
    Result := Result + USFMStyleCloseTag(FStyles[I]);
end;

function TUSFMInlineReader.ReopenTags: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FStyles.Count - 1 do
    Result := Result + USFMStyleOpenTag(FStyles[I]);
end;

procedure TUSFMInlineReader.OpenStyle(const AName: string);
begin
  if USFMStyleOpenTag(AName) = '' then
    Exit;
  FStyles.Add(AName);
  FOut := FOut + USFMStyleOpenTag(AName);
end;

procedure TUSFMInlineReader.CloseStyle(const AName: string);
var
  K, J: Integer;
begin
  if USFMStyleOpenTag(AName) = '' then
    Exit;
  K := -1;
  for J := FStyles.Count - 1 downto 0 do
    if FStyles[J] = AName then
    begin
      K := J;
      Break;
    end;
  if K < 0 then
    Exit;
  for J := FStyles.Count - 1 downto K + 1 do
    FOut := FOut + USFMStyleCloseTag(FStyles[J]);
  FOut := FOut + USFMStyleCloseTag(AName);
  for J := K + 1 to FStyles.Count - 1 do
    FOut := FOut + USFMStyleOpenTag(FStyles[J]);
  FStyles.Delete(K);
end;

procedure TUSFMInlineReader.PushAln(const Attrs: string);
var
  E: TUSFMAlnEntry;
  S, M, L: string;
begin
  ParseUSFMAttributeString(Trim(StringReplace(Attrs, '|', ' ', [])), S, M, L);
  E.Strong := S;
  E.Lemma := L;
  if Pos(',', M) > 0 then
    M := ''; // another morphology system than theWord's
  E.Morph := M;
  if Length(FAln) = 0 then
  begin
    FAlnStart := Length(FOut) + 1;
    SetLength(FAlnGroup, 0);
  end;
  SetLength(FAln, Length(FAln) + 1);
  FAln[High(FAln)] := E;
  SetLength(FAlnGroup, Length(FAlnGroup) + 1);
  FAlnGroup[High(FAlnGroup)] := E;
end;

procedure TUSFMInlineReader.PopAln;
var
  Phrase, Wg, Wt, Combined, Tag, Trail: string;
  I, P, Q: Integer;
  HasSpace: Boolean;
begin
  if Length(FAln) = 0 then
    Exit;
  SetLength(FAln, Length(FAln) - 1);
  if Length(FAln) > 0 then
    Exit;
  while (FAlnStart <= Length(FOut)) and (FOut[FAlnStart] = ' ') do
    Inc(FAlnStart);
  Phrase := Copy(FOut, FAlnStart, MaxInt);
  Wg := '';
  Wt := '';
  for I := 0 to High(FAlnGroup) do
  begin
    Combined := FormatStrongAndMorphAttrsToONT(FAlnGroup[I].Strong, FAlnGroup[I].Morph,
      FAlnGroup[I].Lemma, FNT);
    P := 1;
    while P <= Length(Combined) do
    begin
      if Combined[P] = '<' then
      begin
        Q := PosEx('>', Combined, P);
        if Q = 0 then
          Break;
        Tag := Copy(Combined, P, Q - P + 1);
        if AnsiStartsText('<WT', Tag) then
          Wt := Wt + Tag
        else
          Wg := Wg + Tag;
        P := Q + 1;
      end
      else
        Inc(P);
    end;
  end;
  SetLength(FAlnGroup, 0);
  if (Wg = '') or (Trim(StripAllXMLTags(Phrase)) = '') then
    Exit;
  HasSpace := Pos(' ', Trim(StripAllXMLTags(Phrase))) > 0;
  if HasSpace then
    Insert('<wt>', FOut, FAlnStart);
  Trail := '';
  while (FOut <> '') and (FOut[Length(FOut)] = ' ') do
  begin
    Trail := Trail + ' ';
    SetLength(FOut, Length(FOut) - 1);
  end;
  FOut := FOut + Wg + Wt + Trail;
end;

{ \w word|attributes\w*  (Plain: \wg \wh \wa, text only) }
procedure TUSFMInlineReader.WordMarker(const Inner: string; Plain: Boolean);
var
  PipePos: Integer;
  WordPart, AttrPart, S, M, L, Tags: string;
begin
  PipePos := Pos('|', Inner);
  if PipePos = 0 then
  begin
    FOut := FOut + Trim(Inner);
    Exit;
  end;
  WordPart := Trim(Copy(Inner, 1, PipePos - 1));
  AttrPart := Trim(Copy(Inner, PipePos + 1, MaxInt));
  FOut := FOut + WordPart;
  if Plain or (Length(FAln) > 0) then
    Exit; // inside an alignment the milestones carry the numbers
  ParseUSFMAttributeString(AttrPart, S, M, L);
  if Pos(',', M) > 0 then
    M := '';
  Tags := FormatStrongAndMorphAttrsToONT(S, M, L, FNT);
  if Pos('<W', Tags) > 0 then
    FOut := FOut + Tags;
end;

procedure TUSFMInlineReader.Milestone(const AName, Attrs: string);
var
  Who: string;
  P, Q: Integer;
begin
  if AName = 'zaln-s' then
    PushAln(Attrs)
  else if AName = 'zaln-e' then
    PopAln
  else if AName = 'qt-s' then
  begin
    P := Pos('who=', LowerCase(Attrs));
    Who := '';
    if P > 0 then
    begin
      Q := PosEx('"', Attrs, P + 5);
      if Q = 0 then
        Q := Length(Attrs) + 1;
      Who := LowerCase(Copy(Attrs, P + 5, Q - P - 5));
    end;
    if Pos('jesus', Who) > 0 then
    begin
      Inc(FJesus);
      OpenStyle('wj');
    end;
  end
  else if AName = 'qt-e' then
  begin
    if FJesus > 0 then
    begin
      Dec(FJesus);
      CloseStyle('wj');
    end;
  end;
end;

{ \x - \xo 2:3 \xt Gen 1:1; Exo 3:4-6\x*  ->  <RX 1.1.1><RX 2.3.4-6> }
function TUSFMInlineReader.CrossRefs(const Raw: string): string;
var
  I, Len, P: Integer;
  Name, Seg: string;
  Segs: TStringList;

  procedure Refs(const Text: string);
  var
    Part, Item, BookTok, Rest: string;
    Parts, Items: TStringList;
    B, C, V1, V2, K, Colon, Dash: Integer;
    N: Integer;
    HasBook: Boolean;
  begin
    Parts := TStringList.Create;
    Items := TStringList.Create;
    try
      Parts.Delimiter := ';';
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := Text;
      B := FBook;
      C := 0;
      for K := 0 to Parts.Count - 1 do
      begin
        Part := Trim(Parts[K]);
        if Part = '' then
          Continue;
        HasBook := False;
        N := 1;
        if Part[1] in ['1'..'3'] then
        begin
          N := 2;
          while (N <= Length(Part)) and (Part[N] in [' ', '.']) do
            Inc(N);
          if (N <= Length(Part)) and (Part[N] in ['a'..'z', 'A'..'Z']) then
            HasBook := True
          else
            N := 1;
        end
        else if Part[1] in ['a'..'z', 'A'..'Z'] then
          HasBook := True;
        if HasBook then
        begin
          while (N <= Length(Part)) and (Part[N] in ['a'..'z', 'A'..'Z', '.']) do
            Inc(N);
          BookTok := Trim(Copy(Part, 1, N - 1));
          Rest := Trim(Copy(Part, N, MaxInt));
          B := USFMReferenceBookID(BookTok);
          if B = 0 then
            Continue;
          C := 0;
        end
        else
          Rest := Part;
        if B = 0 then
          Continue;
        Items.Clear;
        Items.Delimiter := ',';
        Items.StrictDelimiter := True;
        Items.DelimitedText := Rest;
        for N := 0 to Items.Count - 1 do
        begin
          Item := Trim(Items[N]);
          if Item = '' then
            Continue;
          Item := StringReplace(Item, '.', ':', []);
          Colon := Pos(':', Item);
          if Colon > 0 then
          begin
            C := StrToIntDef(Trim(Copy(Item, 1, Colon - 1)), 0);
            Item := Copy(Item, Colon + 1, MaxInt);
          end;
          if C = 0 then
            C := 1;
          Dash := Pos('-', Item);
          V2 := 0;
          if Dash > 0 then
          begin
            V1 := ExtractLeadingInt(Copy(Item, 1, Dash - 1), 0);
            Item := Copy(Item, Dash + 1, MaxInt);
            if Pos(':', Item) = 0 then
              V2 := ExtractLeadingInt(Item, 0);
          end
          else
            V1 := ExtractLeadingInt(Item, 0);
          if V1 <= 0 then
            Continue;
          if (V2 > V1) then
            Result := Result + '<RX ' + IntToStr(B) + '.' + IntToStr(C) + '.' + IntToStr(V1) + '-' + IntToStr(V2) + '>'
          else
            Result := Result + '<RX ' + IntToStr(B) + '.' + IntToStr(C) + '.' + IntToStr(V1) + '>';
        end;
      end;
    finally
      Items.Free;
      Parts.Free;
    end;
  end;

begin
  Result := '';
  Segs := TStringList.Create;
  try
    Len := Length(Raw);
    I := 1;
    while I <= Len do
    begin
      if Raw[I] = '\' then
      begin
        P := I + 1;
        if (P <= Len) and (Raw[P] = '+') then
          Inc(P);
        I := P;
        while (I <= Len) and (Raw[I] in ['a'..'z', 'A'..'Z', '0'..'9', '-']) do
          Inc(I);
        Name := LowerCase(Copy(Raw, P, I - P));
        if (I <= Len) and (Raw[I] = '*') then
          Inc(I)
        else if (I <= Len) and (Raw[I] = ' ') then
          Inc(I);
        if Name = 'xt' then
        begin
          P := I;
          while (I <= Len) and (Raw[I] <> '\') do
            Inc(I);
          Seg := Trim(Copy(Raw, P, I - P));
          if (Pos('|', Seg) > 0) then
            Seg := Trim(Copy(Seg, 1, Pos('|', Seg) - 1));
          if Seg <> '' then
            Segs.Add(Seg);
        end;
        Continue;
      end;
      Inc(I);
    end;
    for I := 0 to Segs.Count - 1 do
      Refs(Segs[I]);
  finally
    Segs.Free;
  end;
end;

function TUSFMInlineReader.Convert(const S: string; IsNT: Boolean; Book: Integer): string;
var
  I, Len, J, NmStart, CEnd, NextIdx, K: Integer;
  Name, Base: string;
  Closing, Plus: Boolean;

  procedure EnsureSpace;
  begin
    if (FOut <> '') and not (FOut[Length(FOut)] in [' ', #9]) then
      FOut := FOut + ' ';
  end;

  { the end of \name ... \name* (or \+name*) }
  function FindEnd(const AName: string; StartAt: Integer; out ContentEnd, AfterMarker: Integer): Boolean;
  var
    P: Integer;
    T1, T2: string;
  begin
    Result := False;
    T1 := '\' + AName + '*';
    T2 := '\+' + AName + '*';
    for P := StartAt to Len do
      if S[P] = '\' then
      begin
        if CompareText(Copy(S, P, Length(T1)), T1) = 0 then
        begin
          ContentEnd := P - 1;
          AfterMarker := P + Length(T1);
          Exit(True);
        end;
        if CompareText(Copy(S, P, Length(T2)), T2) = 0 then
        begin
          ContentEnd := P - 1;
          AfterMarker := P + Length(T2);
          Exit(True);
        end;
      end;
  end;

begin
  FOut := '';
  FNT := IsNT;
  FBook := Book;
  if Length(FAln) > 0 then
    FAlnStart := 1; // a milestone that began on an earlier line
  Len := Length(S);
  I := 1;
  while I <= Len do
  begin
    if S[I] = '\' then
    begin
      J := I + 1;
      Plus := (J <= Len) and (S[J] = '+');
      if Plus then
        Inc(J);
      if (J <= Len) and (S[J] = '*') then
      begin
        I := J + 1; // a bare \* with no marker before it
        Continue;
      end;
      NmStart := J;
      while (J <= Len) and (S[J] in ['a'..'z', 'A'..'Z', '0'..'9', '-']) do
        Inc(J);
      Name := LowerCase(Copy(S, NmStart, J - NmStart));
      if Name = '' then
      begin
        FOut := FOut + '\';
        Inc(I);
        Continue;
      end;
      Closing := (J <= Len) and (S[J] = '*');
      if Closing then
        Inc(J)
      else if (J <= Len) and (S[J] = ' ') then
        Inc(J);
      I := J;

      if Closing then
      begin
        CloseStyle(Name);
        Continue;
      end;

      Base := StripTrailingDigits(Name);
      if (Name = 'w') or (Name = 'wg') or (Name = 'wh') or (Name = 'wa') then
      begin
        if FindEnd(Name, I, CEnd, NextIdx) then
        begin
          WordMarker(Copy(S, I, CEnd - I + 1), Name <> 'w');
          I := NextIdx;
        end;
        Continue;
      end;
      if (Name = 'f') or (Name = 'fe') or (Name = 'ef') then
      begin
        if FindEnd(Name, I, CEnd, NextIdx) then
        begin
          K := Length(CleanUSFMFootnoteToPlain(Copy(S, I, CEnd - I + 1)));
          if K > 0 then
            FOut := FOut + '<RF>' + CleanUSFMFootnoteToPlain(Copy(S, I, CEnd - I + 1)) + '<Rf>';
          I := NextIdx;
        end;
        Continue;
      end;
      if (Name = 'x') or (Name = 'ex') then
      begin
        if FindEnd(Name, I, CEnd, NextIdx) then
        begin
          FOut := FOut + CrossRefs(Copy(S, I, CEnd - I + 1));
          I := NextIdx;
        end;
        Continue;
      end;
      if (Name = 'fig') or (Name = 'va') or (Name = 'vp') or (Name = 'ca') or (Name = 'rq') then
      begin
        if FindEnd(Name, I, CEnd, NextIdx) then
          I := NextIdx;
        Continue;
      end;
      if (Length(Name) > 2) and ((Copy(Name, Length(Name) - 1, 2) = '-s') or (Copy(Name, Length(Name) - 1, 2) = '-e')) or
         (Name = 'ts') then
      begin
        // milestone: \name |attributes\*
        K := PosEx('\*', S, I);
        if K = 0 then
        begin
          Milestone(Name, Copy(S, I, MaxInt));
          I := Len + 1;
        end
        else
        begin
          Milestone(Name, Trim(Copy(S, I, K - I)));
          I := K + 2;
        end;
        Continue;
      end;
      if (Base = 'th') or (Base = 'tc') or (Base = 'thr') or (Base = 'tcr') then
      begin
        EnsureSpace;
        Continue;
      end;
      OpenStyle(Name); // the markers with a theWord tag; any other marker keeps its text
      Continue;
    end;

    if S[I] = '|' then
    begin
      K := I + 1;
      while (K <= Len) and not (S[K] in [' ', #9, '\']) do
        Inc(K);
      if Pos('=', Copy(S, I + 1, K - I - 1)) > 0 then
      begin
        while (K <= Len) and (S[K] <> '\') do
          Inc(K);
        I := K;
        Continue;
      end;
    end;

    if S[I] in [#13, #10] then
    begin
      EnsureSpace;
      Inc(I);
      Continue;
    end;
    if S[I] = '~' then
      FOut := FOut + ' '
    else
      FOut := FOut + S[I];
    Inc(I);
  end;

  while Pos('  ', FOut) > 0 do
    FOut := StringReplace(FOut, '  ', ' ', [rfReplaceAll]);
  Result := Trim(FOut);
end;

{ no space is wanted between the end of Buf and the start of Text }
function USFMNeedsSpaceBetween(const Buf, Text: string): Boolean;
var
  P: Integer;
  Tag: string;
begin
  Result := False;
  if (Buf = '') or (Text = '') then
    Exit;
  if (Buf[Length(Buf)] in [' ', #9]) or (Text[1] in [' ', #9]) then
    Exit;
  if Buf[Length(Buf)] <> '>' then
    Exit(True);
  P := Length(Buf);
  while (P > 0) and (Buf[P] <> '<') do
    Dec(P);
  if P = 0 then
    Exit(True);
  Tag := Copy(Buf, P, MaxInt);
  Result := not ((Tag = '<CM>') or (Tag = '<CI>') or (Tag = '<CL>') or (Tag = '<FR>') or
    (Tag = '<FI>') or (Tag = '<FO>') or (Tag = '<b>') or (Tag = '<i>') or (Tag = '<sup>') or
    (Tag = '<Ts>') or (Tag = '<wt>') or AnsiStartsStr('<PI', Tag) or AnsiStartsStr('<PF', Tag));
end;

{ a heading without footnotes or tags }
function USFMPlainHeading(const T: string): string;
var
  P, Q: Integer;
begin
  Result := T;
  repeat
    P := Pos('<RF>', Result);
    if P = 0 then
      Break;
    Q := PosEx('<Rf>', Result, P);
    if Q = 0 then
      Break;
    Delete(Result, P, Q + 3 - P);
  until False;
  Result := Trim(StripAllXMLTags(Result));
end;


class function TESwordModule.LoadUSFMBibleWithApocrypha(
  const filename: string;
  modulo: TStringList;
  out extraVerses: TESwordExtraVerseArray;
  out apocryphaBooksInfo: string;
  out hasOT, hasNT: boolean;
  out errMsg: string
): boolean;
var
  filesToParse, fileLines, sibFiles, lines: TStringList;
  sr: TSearchRec;
  dirPath, extLow, rawLine, trimmedLine, marker, rest, idCode, pendingHeading, curVerseBuf, bName: string;
  pendBound, pendPI, carryPI, paraBound, paraPI: string;
  fIdx, lIdx, spPos, idBookCount, curBook, curChap, curVerse, vNum, extraCount, i, book: integer;
  headingLevel, lastKind, lastIdx, lastBook, lastChap: integer;
  curHasText, pendActive, inSidebar, inPeriph: boolean;
  reader: TUSFMInlineReader;
  allVerses: array of string;
  apocPresent: array[67..150] of boolean;
  apocMaxChap, apocChap1Verses, apocChap2Verses, apocMaxChapVerses: array[67..150] of integer;
  apocSample: array[67..150] of string;
  titleStr, abbrevStr: string;
  nonEmptyCount: integer;

  procedure FlushCurrentUSFMVerse;
  var
    ontLine, curPareID: string;
    lineIdx, maxV: integer;
  begin
    if (curBook <= 0) or (curChap <= 0) or (curVerse <= 0) then
    begin
      curVerseBuf := '';
      curHasText := False;
      Exit;
    end;
    if curHasText then
      curVerseBuf := curVerseBuf + reader.CloseTags; // styles left open go on in the next verse
    if Trim(StripAllXMLTags(curVerseBuf)) = '' then
      curVerseBuf := '';
    ontLine := Trim(curVerseBuf);
    curVerseBuf := '';
    curHasText := False;
    if ontLine = '' then
      Exit;

    Inc(nonEmptyCount);
    if (curBook <= 66) and (curChap >= 1) and (curChap <= ESwordChaptersPerBook[curBook]) then
    begin
      lineIdx := GetCanonicalONTLineIndex(curBook, curChap, curVerse, maxV);
      if lineIdx >= 0 then
      begin
        if curBook <= 39 then
          hasOT := True
        else
          hasNT := True;
        if allVerses[lineIdx] <> '' then
          allVerses[lineIdx] := JoinVerseTexts(allVerses[lineIdx], ontLine)
        else
          allVerses[lineIdx] := ontLine;
        lastKind := 1;
        lastIdx := lineIdx;
        lastBook := curBook;
        lastChap := curChap;
      end;
    end
    else if ((curBook <= 66) and (curChap > ESwordChaptersPerBook[curBook])) or ((curBook >= 67) and (curBook <= 150)) then
    begin
      if (curBook <= 39) or (curBook >= 67) then
        hasOT := True
      else
        hasNT := True;

      curPareID := Format('%d,%d,%d', [curBook, curChap, curVerse]);
      if (extraCount > 0) and (extraVerses[extraCount - 1].PareID = curPareID) then
        extraVerses[extraCount - 1].ONTText := JoinVerseTexts(extraVerses[extraCount - 1].ONTText, ontLine)
      else
      begin
        if extraCount >= Length(extraVerses) then
          SetLength(extraVerses, extraCount + 1024);
        extraVerses[extraCount].Book := curBook;
        extraVerses[extraCount].Chapter := curChap;
        extraVerses[extraCount].Verse := curVerse;
        extraVerses[extraCount].PareID := curPareID;
        extraVerses[extraCount].ONTText := ontLine;
        Inc(extraCount);
      end;
      lastKind := 2;
      lastIdx := extraCount - 1;
      lastBook := curBook;
      lastChap := curChap;

      if (curBook >= 67) and (curBook <= 150) then
      begin
        apocPresent[curBook] := True;
        if curChap > apocMaxChap[curBook] then
        begin
          apocMaxChap[curBook] := curChap;
          apocMaxChapVerses[curBook] := curVerse;
        end
        else if (curChap = apocMaxChap[curBook]) and (curVerse > apocMaxChapVerses[curBook]) then
          apocMaxChapVerses[curBook] := curVerse;
        if (curChap = 1) and (curVerse > apocChap1Verses[curBook]) then
          apocChap1Verses[curBook] := curVerse;
        if (curChap = 2) and (curVerse > apocChap2Verses[curBook]) then
          apocChap2Verses[curBook] := curVerse;
        if (curChap = 1) and (curVerse <= 3) and (Length(apocSample[curBook]) < 300) then
          apocSample[curBook] := apocSample[curBook] + ' ' + ontLine;
      end;
    end;
  end;


  { a paragraph boundary at the end of the verse stored last (theWord puts the
    boundary there; the indent goes at the start of the next verse) }
  procedure AppendBoundaryToLastStored(const tag: string);
  var
    t: string;
  begin
    if (tag = '') or (lastKind = 0) or (lastBook <> curBook) or (lastChap <> curChap) then
      Exit;
    if lastKind = 1 then
      t := allVerses[lastIdx]
    else
      t := extraVerses[lastIdx].ONTText;
    if t = '' then
      Exit;
    if (Copy(t, Length(t) - 3, 4) = '<CM>') or (Copy(t, Length(t) - 3, 4) = '<CI>') then
      Exit;
    if Copy(t, Length(t) - 3, 4) = '<CL>' then
    begin
      if tag <> '<CM>' then
        Exit;
      SetLength(t, Length(t) - 4);
    end;
    t := t + tag;
    if lastKind = 1 then
      allVerses[lastIdx] := t
    else
      extraVerses[lastIdx].ONTText := t;
  end;

  { a paragraph marker that came after the text of a verse and was followed by
    the next verse (or a heading) belongs to what follows }
  procedure ApplyPendingLeading;
  begin
    if pendActive then
    begin
      AppendBoundaryToLastStored(pendBound);
      carryPI := pendPI;
      pendActive := False;
    end;
  end;

  procedure AddInlineText(const raw: string);
  var
    t: string;
  begin
    t := reader.Convert(raw, (curBook >= 40) and (curBook <= 66), curBook);
    if t = '' then
      Exit;
    if pendActive then
    begin
      while (curVerseBuf <> '') and (curVerseBuf[Length(curVerseBuf)] = ' ') do
        SetLength(curVerseBuf, Length(curVerseBuf) - 1);
      curVerseBuf := curVerseBuf + pendBound + pendPI;
      pendActive := False;
    end
    else if USFMNeedsSpaceBetween(curVerseBuf, t) then
      curVerseBuf := curVerseBuf + ' ';
    curVerseBuf := curVerseBuf + t;
    if Trim(StripAllXMLTags(t)) <> '' then
      curHasText := True;
  end;

begin
  Result := False;
  hasOT := False;
  hasNT := False;
  errMsg := '';
  SetLength(extraVerses, 0);
  apocryphaBooksInfo := '';
  extraCount := 0;
  nonEmptyCount := 0;

  if not FileExists(filename) then
  begin
    errMsg := Format('File not found: "%s"', [filename]);
    Exit;
  end;

  for i := 67 to 150 do
  begin
    apocPresent[i] := False;
    apocMaxChap[i] := 0;
    apocChap1Verses[i] := 0;
    apocChap2Verses[i] := 0;
    apocMaxChapVerses[i] := 0;
    apocSample[i] := '';
  end;

  SetLength(allVerses, ESwordONTLines);
  for i := 0 to ESwordONTLines - 1 do
    allVerses[i] := '';

  titleStr := ChangeFileExt(ExtractFileName(filename), '');
  abbrevStr := Copy(titleStr, 1, 15);

  filesToParse := TStringList.Create;
  fileLines := TStringList.Create;
  sibFiles := TStringList.Create;
  lines := TStringList.Create;
  try
    fileLines.LoadFromFile(filename);
    if Pos(#13#13, fileLines.Text) > 0 then
    begin
      rawLine := fileLines.Text;
      while Pos(#13#13, rawLine) > 0 do
        rawLine := StringReplace(rawLine, #13#13, #13, [rfReplaceAll]);
      fileLines.Text := rawLine;
    end;
    idBookCount := 0;
    for lIdx := 0 to fileLines.Count - 1 do
    begin
      trimmedLine := TrimLeft(fileLines[lIdx]);
      if Pos(#239#187#191, trimmedLine) = 1 then
        Delete(trimmedLine, 1, 3);
      if AnsiStartsText('\id ', trimmedLine) then
        Inc(idBookCount);
    end;

    if (idBookCount <= 1) and not USFMSingleFileOnly then
    begin
      dirPath := ExtractFilePath(ExpandFileName(filename));
      if FindFirst(dirPath + '*', faAnyFile, sr) = 0 then
      begin
        try
          repeat
            if (sr.Attr and faDirectory) = 0 then
            begin
              extLow := LowerCase(ExtractFileExt(sr.Name));
              if (extLow = '.usfm') or (extLow = '.sfm') then
                sibFiles.Add(dirPath + sr.Name);
            end;
          until FindNext(sr) <> 0;
        finally
          FindClose(sr);
        end;
      end;
      sibFiles.Sort;
      if sibFiles.Count > 1 then
        filesToParse.Assign(sibFiles)
      else
        filesToParse.Add(filename);
    end
    else
      filesToParse.Add(filename);

    curBook := 0;
    curChap := 0;
    curVerse := 0;
    pendingHeading := '';
    curVerseBuf := '';
    curHasText := False;
    pendActive := False;
    pendBound := '';
    pendPI := '';
    carryPI := '';
    lastKind := 0;
    lastIdx := 0;
    lastBook := 0;
    lastChap := 0;
    inSidebar := False;
    inPeriph := False;

    reader := TUSFMInlineReader.Create;
    try
    for fIdx := 0 to filesToParse.Count - 1 do
    begin
      fileLines.LoadFromFile(filesToParse[fIdx]);
      SplitUSFMLogicalLines(fileLines.Text, lines);
      for lIdx := 0 to lines.Count - 1 do
      begin
        trimmedLine := lines[lIdx];
        if trimmedLine = '' then
          Continue;

        marker := '';
        rest := trimmedLine;
        if trimmedLine[1] = '\' then
        begin
          spPos := 2;
          while (spPos <= Length(trimmedLine)) and not (trimmedLine[spPos] in [' ', #9]) do
            Inc(spPos);
          marker := LowerCase(Copy(trimmedLine, 2, spPos - 2));
          rest := Trim(Copy(trimmedLine, spPos + 1, Length(trimmedLine) - spPos));
        end;

        if inSidebar then
        begin
          if marker = 'esbe' then
            inSidebar := False;
          Continue;
        end;
        if inPeriph then
        begin
          if marker <> 'id' then
            Continue;
          inPeriph := False;
        end;

        if marker = 'id' then
        begin
          FlushCurrentUSFMVerse;
          spPos := 1;
          while (spPos <= Length(rest)) and not (rest[spPos] in [' ', #9]) do
            Inc(spPos);
          idCode := Copy(rest, 1, spPos - 1);
          curBook := USFMBookCodeToID(idCode);
          curChap := 0;
          curVerse := 0;
          pendingHeading := '';
          curVerseBuf := '';
          curHasText := False;
          pendActive := False;
          carryPI := '';
          lastKind := 0;
          reader.Reset;
        end
        else if marker = 'c' then
        begin
          FlushCurrentUSFMVerse;
          ApplyPendingLeading;
          reader.ClearStyles;
          curChap := ExtractLeadingInt(rest, curChap + 1);
          curVerse := 0;
          pendingHeading := '';
          curVerseBuf := '';
        end
        else if USFMHeadingLevel(marker) > 0 then
        begin
          headingLevel := USFMHeadingLevel(marker);
          FlushCurrentUSFMVerse;
          ApplyPendingLeading;
          reader.ClearStyles;
          curVerse := 0;
          rest := USFMPlainHeading(reader.Convert(rest, (curBook >= 40) and (curBook <= 66), curBook));
          if rest <> '' then
            pendingHeading := pendingHeading + '<TS' + IntToStr(headingLevel) + '>' + rest + '<Ts>';
        end
        else if marker = 'v' then
        begin
          FlushCurrentUSFMVerse;
          ApplyPendingLeading;
          if curChap <= 0 then
            curChap := 1;
          spPos := 1;
          while (spPos <= Length(rest)) and not (rest[spPos] in [' ', #9]) do
            Inc(spPos);
          vNum := ExtractLeadingInt(Copy(rest, 1, spPos - 1), curVerse + 1);
          if vNum < 1 then
            vNum := 1;
          curVerse := vNum;
          rest := Trim(Copy(rest, spPos + 1, Length(rest) - spPos));
          curVerseBuf := pendingHeading + carryPI + reader.ReopenTags;
          curHasText := False;
          pendingHeading := '';
          carryPI := '';
          AddInlineText(rest);
        end
        else if marker = 'esb' then
          inSidebar := True
        else if marker = 'periph' then
          inPeriph := True
        else if USFMParagraphTags(marker, paraBound, paraPI) then
        begin
          // paragraph / poetry / list / table row
          if (curVerse > 0) and curHasText then
          begin
            curVerseBuf := curVerseBuf + reader.CloseTags;
            reader.ClearStyles;
          end
          else if curVerse = 0 then
            reader.ClearStyles;
          if (curVerse = 0) or not curHasText then
          begin
            // before the text of a verse: the boundary ends the verse before
            AppendBoundaryToLastStored(paraBound);
            if curVerse = 0 then
              carryPI := paraPI
            else
              curVerseBuf := curVerseBuf + paraPI;
            pendActive := False;
          end
          else if (paraBound <> '') or (paraPI <> '') then
          begin
            // after the text: inside the verse, or before the next one
            if not (pendActive and (pendBound = '<CM>') and (paraBound = '<CL>')) then
              pendBound := paraBound;
            pendPI := paraPI;
            pendActive := True;
          end;
          if (curVerse > 0) and (rest <> '') then
            AddInlineText(rest);
        end
        else if (marker <> '') and IsUSFMBlockMarker(marker) then
        begin
          // header, title, introduction, reference and label lines: not verse text
        end
        else if curVerse > 0 then
          AddInlineText(trimmedLine);
      end;
      FlushCurrentUSFMVerse;
    end;
    finally
      reader.Free;
    end;

    SetLength(extraVerses, extraCount);
    if (nonEmptyCount = 0) or ((not hasOT) and (not hasNT)) then
    begin
      errMsg := 'No valid USFM Scripture verses found in "' + filename + '".';
      Exit;
    end;

    for book := 67 to 150 do
      if apocPresent[book] then
      begin
        if book <= 87 then
          bName := DefaultApocryphaBookName(book)
        else
          bName := IdentifyApocryphaBookName(
            book,
            apocMaxChap[book],
            apocChap1Verses[book],
            apocChap2Verses[book],
            apocMaxChapVerses[book],
            apocSample[book]
          );
        if apocryphaBooksInfo <> '' then
          apocryphaBooksInfo := apocryphaBooksInfo + ';';
        apocryphaBooksInfo := apocryphaBooksInfo + IntToStr(book) + '=' + bName;
      end;

    modulo.BeginUpdate;
    try
      modulo.Clear;
      if hasOT and hasNT then
      begin
        for i := 0 to ESwordONTLines - 1 do
          modulo.Add(allVerses[i]);
      end
      else if hasOT then
      begin
        for i := 0 to ESwordOTLines - 1 do
          modulo.Add(allVerses[i]);
      end
      else
      begin
        for i := ESwordOTLines to ESwordONTLines - 1 do
          modulo.Add(allVerses[i]);
      end;
      modulo.Add('description=' + titleStr);
      modulo.Add('short.title=' + abbrevStr);
      modulo.Add('title=' + titleStr);
    finally
      modulo.EndUpdate;
    end;

    Result := True;
  finally
    lines.Free;
    sibFiles.Free;
    fileLines.Free;
    filesToParse.Free;
  end;
end;

class function TESwordModule.LoadUSFXBibleWithApocrypha(
  const filename: string;
  modulo: TStringList;
  out extraVerses: TESwordExtraVerseArray;
  out apocryphaBooksInfo: string;
  out hasOT, hasNT: boolean;
  out errMsg: string
): boolean;
var
  sl: TStringList;
  xml: string;
  i, len, tagEnd, closePos, spPos, curBook, curChap, curVerse, vNum, extraCount, book, nonEmptyCount: integer;
  rawTag, tagLower, tagName, idCode, sAttr, mAttr, lAttr, sfmAttr, innerText, fnText, pendingHeading, curVerseBuf, bName, titleStr, abbrevStr: string;
  isSelfClosing, inVerse: boolean;
  allVerses: array of string;
  apocPresent: array[67..150] of boolean;
  apocMaxChap, apocChap1Verses, apocChap2Verses, apocMaxChapVerses: array[67..150] of integer;
  apocSample: array[67..150] of string;

  function FindTagCloseCI(const needle: string; startIdx: integer): integer;
  var
    k, nLen: integer;
  begin
    Result := 0;
    nLen := Length(needle);
    for k := startIdx to len - nLen + 1 do
      if CompareText(Copy(xml, k, nLen), needle) = 0 then
      begin
        Result := k;
        Exit;
      end;
  end;

  procedure FlushCurrentUSFXVerse;
  var
    ontLine, curPareID: string;
    lineIdx, maxV: integer;
  begin
    if (curBook <= 0) or (curChap <= 0) or (curVerse <= 0) then
    begin
      curVerseBuf := '';
      inVerse := False;
      Exit;
    end;
    ontLine := DecodeHTMLEntities(curVerseBuf);
    ontLine := StringReplace(ontLine, #13, ' ', [rfReplaceAll]);
    ontLine := StringReplace(ontLine, #10, ' ', [rfReplaceAll]);
    while Pos('  ', ontLine) > 0 do
      ontLine := StringReplace(ontLine, '  ', ' ', [rfReplaceAll]);
    ontLine := Trim(ontLine);
    curVerseBuf := '';
    inVerse := False;
    if ontLine = '' then
      Exit;

    Inc(nonEmptyCount);
    if (curBook <= 66) and (curChap >= 1) and (curChap <= ESwordChaptersPerBook[curBook]) then
    begin
      lineIdx := GetCanonicalONTLineIndex(curBook, curChap, curVerse, maxV);
      if lineIdx >= 0 then
      begin
        if curBook <= 39 then
          hasOT := True
        else
          hasNT := True;
        if allVerses[lineIdx] <> '' then
          allVerses[lineIdx] := allVerses[lineIdx] + ' ' + ontLine
        else
          allVerses[lineIdx] := ontLine;
      end;
    end
    else if ((curBook <= 66) and (curChap > ESwordChaptersPerBook[curBook])) or ((curBook >= 67) and (curBook <= 150)) then
    begin
      if (curBook <= 39) or (curBook >= 67) then
        hasOT := True
      else
        hasNT := True;

      curPareID := Format('%d,%d,%d', [curBook, curChap, curVerse]);
      if (extraCount > 0) and (extraVerses[extraCount - 1].PareID = curPareID) then
        extraVerses[extraCount - 1].ONTText := extraVerses[extraCount - 1].ONTText + ' ' + ontLine
      else
      begin
        if extraCount >= Length(extraVerses) then
          SetLength(extraVerses, extraCount + 1024);
        extraVerses[extraCount].Book := curBook;
        extraVerses[extraCount].Chapter := curChap;
        extraVerses[extraCount].Verse := curVerse;
        extraVerses[extraCount].PareID := curPareID;
        extraVerses[extraCount].ONTText := ontLine;
        Inc(extraCount);
      end;

      if (curBook >= 67) and (curBook <= 150) then
      begin
        apocPresent[curBook] := True;
        if curChap > apocMaxChap[curBook] then
        begin
          apocMaxChap[curBook] := curChap;
          apocMaxChapVerses[curBook] := curVerse;
        end
        else if (curChap = apocMaxChap[curBook]) and (curVerse > apocMaxChapVerses[curBook]) then
          apocMaxChapVerses[curBook] := curVerse;
        if (curChap = 1) and (curVerse > apocChap1Verses[curBook]) then
          apocChap1Verses[curBook] := curVerse;
        if (curChap = 2) and (curVerse > apocChap2Verses[curBook]) then
          apocChap2Verses[curBook] := curVerse;
        if (curChap = 1) and (curVerse <= 3) and (Length(apocSample[curBook]) < 300) then
          apocSample[curBook] := apocSample[curBook] + ' ' + ontLine;
      end;
    end;
  end;

begin
  Result := False;
  hasOT := False;
  hasNT := False;
  errMsg := '';
  SetLength(extraVerses, 0);
  apocryphaBooksInfo := '';
  extraCount := 0;
  nonEmptyCount := 0;

  if not FileExists(filename) then
  begin
    errMsg := Format('File not found: "%s"', [filename]);
    Exit;
  end;

  for i := 67 to 150 do
  begin
    apocPresent[i] := False;
    apocMaxChap[i] := 0;
    apocChap1Verses[i] := 0;
    apocChap2Verses[i] := 0;
    apocMaxChapVerses[i] := 0;
    apocSample[i] := '';
  end;

  SetLength(allVerses, ESwordONTLines);
  for i := 0 to ESwordONTLines - 1 do
    allVerses[i] := '';

  titleStr := ChangeFileExt(ExtractFileName(filename), '');
  abbrevStr := Copy(titleStr, 1, 15);

  sl := TStringList.Create;
  try
    sl.LoadFromFile(filename);
    xml := sl.Text;
  finally
    sl.Free;
  end;

  len := Length(xml);
  if len < 10 then
  begin
    errMsg := 'Empty or invalid USFX XML file.';
    Exit;
  end;

  curBook := 0;
  curChap := 0;
  curVerse := 0;
  inVerse := False;
  pendingHeading := '';
  curVerseBuf := '';

  i := 1;
  while i <= len do
  begin
    if xml[i] = '<' then
    begin
      if Copy(xml, i, 4) = '<!--' then
      begin
        closePos := PosEx('-->', xml, i + 4);
        if closePos > 0 then
          i := closePos + 3
        else
          Break;
        Continue;
      end;

      tagEnd := i + 1;
      while (tagEnd <= len) and (xml[tagEnd] <> '>') do
        Inc(tagEnd);
      if tagEnd > len then
        Break;

      rawTag := Trim(Copy(xml, i + 1, tagEnd - i - 1));
      isSelfClosing := (Length(rawTag) > 0) and (rawTag[Length(rawTag)] = '/');
      if isSelfClosing then
        rawTag := Trim(Copy(rawTag, 1, Length(rawTag) - 1));

      tagLower := LowerCase(rawTag);
      spPos := 1;
      while (spPos <= Length(tagLower)) and not (tagLower[spPos] in [' ', #9, '/']) do
        Inc(spPos);
      tagName := Copy(tagLower, 1, spPos - 1);

      if tagName = 'book' then
      begin
        FlushCurrentUSFXVerse;
        idCode := ExtractXMLAttrValue(rawTag, 'id');
        if idCode = '' then
          idCode := ExtractXMLAttrValue(rawTag, 'code');
        curBook := USFMBookCodeToID(idCode);
        curChap := 0;
        curVerse := 0;
        pendingHeading := '';
        curVerseBuf := '';
        i := tagEnd + 1;
        Continue;
      end
      else if tagName = '/book' then
      begin
        FlushCurrentUSFXVerse;
        curBook := 0;
        curChap := 0;
        curVerse := 0;
        pendingHeading := '';
        i := tagEnd + 1;
        Continue;
      end
      else if tagName = 'c' then
      begin
        FlushCurrentUSFXVerse;
        idCode := ExtractXMLAttrValue(rawTag, 'id');
        if idCode = '' then
          idCode := ExtractXMLAttrValue(rawTag, 'number');
        curChap := ExtractLeadingInt(idCode, curChap + 1);
        curVerse := 0;
        pendingHeading := '';
        if not isSelfClosing then
        begin
          closePos := FindTagCloseCI('</c>', tagEnd + 1);
          if (closePos > 0) and (closePos - tagEnd <= 16) then
          begin
            i := closePos + 4;
            Continue;
          end;
        end;
        i := tagEnd + 1;
        Continue;
      end
      else if tagName = 'v' then
      begin
        FlushCurrentUSFXVerse;
        if curChap <= 0 then
          curChap := 1;
        idCode := ExtractXMLAttrValue(rawTag, 'id');
        if idCode = '' then
          idCode := ExtractXMLAttrValue(rawTag, 'number');
        vNum := ExtractLeadingInt(idCode, curVerse + 1);
        if vNum < 1 then
          vNum := 1;
        curVerse := vNum;
        inVerse := True;
        curVerseBuf := pendingHeading;
        pendingHeading := '';
        if not isSelfClosing then
        begin
          closePos := FindTagCloseCI('</v>', tagEnd + 1);
          if (closePos > 0) and (closePos - tagEnd <= 16) and
             (Trim(Copy(xml, tagEnd + 1, closePos - tagEnd - 1)) = Trim(idCode)) then
          begin
            i := closePos + 4;
            Continue;
          end;
        end;
        i := tagEnd + 1;
        Continue;
      end
      else if (tagName = 've') or (tagName = '/ve') or (tagName = '/v') then
      begin
        FlushCurrentUSFXVerse;
        i := tagEnd + 1;
        Continue;
      end
      else if (curBook > 0) and ((tagName = 's') or (tagName = 's1') or (tagName = 's2') or (tagName = 'd')) and (not isSelfClosing) then
      begin
        closePos := FindTagCloseCI('</' + tagName + '>', tagEnd + 1);
        if closePos > 0 then
        begin
          innerText := StripAllXMLTags(Copy(xml, tagEnd + 1, closePos - tagEnd - 1));
          if innerText <> '' then
          begin
            if inVerse then
              curVerseBuf := '<TS1>' + innerText + '<Ts>' + curVerseBuf
            else
              pendingHeading := pendingHeading + '<TS1>' + innerText + '<Ts>';
          end;
          i := closePos + Length(tagName) + 3;
          Continue;
        end;
      end
      else if (curBook > 0) and (tagName = 'p') and (not isSelfClosing) then
      begin
        sfmAttr := LowerCase(ExtractXMLAttrValue(rawTag, 'sfm'));
        if (sfmAttr = 's') or (sfmAttr = 's1') or (sfmAttr = 's2') or (sfmAttr = 'd') then
        begin
          closePos := FindTagCloseCI('</p>', tagEnd + 1);
          if closePos > 0 then
          begin
            innerText := StripAllXMLTags(Copy(xml, tagEnd + 1, closePos - tagEnd - 1));
            if innerText <> '' then
              pendingHeading := pendingHeading + '<TS1>' + innerText + '<Ts>';
            i := closePos + 4;
            Continue;
          end;
        end
        else if (sfmAttr = 'mt') or (sfmAttr = 'mt1') or (sfmAttr = 'h') or (sfmAttr = 'toc1') or
                (sfmAttr = 'toc2') or (sfmAttr = 'toc3') or (sfmAttr = 'r') or (sfmAttr = 'ip') or
                (sfmAttr = 'is') or (sfmAttr = 'iot') or (sfmAttr = 'io') then
        begin
          closePos := FindTagCloseCI('</p>', tagEnd + 1);
          if closePos > 0 then
          begin
            i := closePos + 4;
            Continue;
          end;
        end;
      end
      else if (tagName = 'h') or (tagName = 'toc') or (tagName = 'mt') or (tagName = 'r') or
              (tagName = 'x') or (tagName = 'fig') or (tagName = 'periph') or (tagName = 'rem') then
      begin
        if not isSelfClosing then
        begin
          closePos := FindTagCloseCI('</' + tagName + '>', tagEnd + 1);
          if closePos > 0 then
          begin
            i := closePos + Length(tagName) + 3;
            Continue;
          end;
        end;
        i := tagEnd + 1;
        Continue;
      end
      else if inVerse then
      begin
        if (tagName = 'w') and (not isSelfClosing) then
        begin
          closePos := FindTagCloseCI('</w>', tagEnd + 1);
          if closePos > 0 then
          begin
            sAttr := ExtractXMLAttrValue(rawTag, 's');
            if sAttr = '' then sAttr := ExtractXMLAttrValue(rawTag, 'str');
            if sAttr = '' then sAttr := ExtractXMLAttrValue(rawTag, 'strong');
            mAttr := ExtractXMLAttrValue(rawTag, 'm');
            if mAttr = '' then mAttr := ExtractXMLAttrValue(rawTag, 'morph');
            if mAttr = '' then mAttr := ExtractXMLAttrValue(rawTag, 'oshm');
            lAttr := ExtractXMLAttrValue(rawTag, 'l');
            if lAttr = '' then lAttr := ExtractXMLAttrValue(rawTag, 'lemma');
            innerText := StripAllXMLTags(Copy(xml, tagEnd + 1, closePos - tagEnd - 1));
            curVerseBuf := curVerseBuf + innerText + FormatStrongAndMorphAttrsToONT(sAttr, mAttr, lAttr, (curBook >= 40) and (curBook <= 66));
            i := closePos + 4;
            Continue;
          end;
        end
        else if (tagName = 'f') or (tagName = 'fe') then
        begin
          if not isSelfClosing then
          begin
            closePos := FindTagCloseCI('</' + tagName + '>', tagEnd + 1);
            if closePos > 0 then
            begin
              fnText := StripAllXMLTags(Copy(xml, tagEnd + 1, closePos - tagEnd - 1));
              if fnText <> '' then
                curVerseBuf := curVerseBuf + '<RF>' + fnText + '<Rf>';
              i := closePos + Length(tagName) + 3;
              Continue;
            end;
          end;
        end
        else if tagName = 'wj' then
          curVerseBuf := curVerseBuf + '<FR>'
        else if tagName = '/wj' then
          curVerseBuf := curVerseBuf + '<Fr>'
        else if (tagName = 'add') or (tagName = 'it') or (tagName = 'i') then
          curVerseBuf := curVerseBuf + '<FI>'
        else if (tagName = '/add') or (tagName = '/it') or (tagName = '/i') then
          curVerseBuf := curVerseBuf + '<Fi>'
        else if (tagName = 'bd') or (tagName = 'bdit') or (tagName = 'b') then
        begin
          if not isSelfClosing then
            curVerseBuf := curVerseBuf + '<FO>'
          else if (Length(curVerseBuf) > 0) and not (curVerseBuf[Length(curVerseBuf)] in [' ', '>']) then
            curVerseBuf := curVerseBuf + ' ';
        end
        else if (tagName = '/bd') or (tagName = '/bdit') or (tagName = '/b') then
          curVerseBuf := curVerseBuf + '<Fo>'
        else if (tagName = 'q') or (tagName = 'q1') or (tagName = 'q2') or (tagName = '/q') or
                (tagName = '/q1') or (tagName = '/q2') or (tagName = 'p') or (tagName = '/p') or
                (tagName = 'm') or (tagName = '/m') or (tagName = 'br') then
        begin
          if (Length(curVerseBuf) > 0) and not (curVerseBuf[Length(curVerseBuf)] in [' ', '>']) then
            curVerseBuf := curVerseBuf + ' ';
        end;
      end;

      i := tagEnd + 1;
      Continue;
    end;

    if inVerse then
    begin
      if xml[i] in [#13, #10, #9] then
      begin
        if (Length(curVerseBuf) > 0) and not (curVerseBuf[Length(curVerseBuf)] in [' ', '>']) then
          curVerseBuf := curVerseBuf + ' ';
      end
      else
        curVerseBuf := curVerseBuf + xml[i];
    end;
    Inc(i);
  end;
  FlushCurrentUSFXVerse;

  SetLength(extraVerses, extraCount);
  if (nonEmptyCount = 0) or ((not hasOT) and (not hasNT)) then
  begin
    errMsg := 'No valid USFX Scripture verses found in "' + filename + '".';
    Exit;
  end;

  for book := 67 to 150 do
    if apocPresent[book] then
    begin
      if book <= 87 then
        bName := DefaultApocryphaBookName(book)
      else
        bName := IdentifyApocryphaBookName(
          book,
          apocMaxChap[book],
          apocChap1Verses[book],
          apocChap2Verses[book],
          apocMaxChapVerses[book],
          apocSample[book]
        );
      if apocryphaBooksInfo <> '' then
        apocryphaBooksInfo := apocryphaBooksInfo + ';';
      apocryphaBooksInfo := apocryphaBooksInfo + IntToStr(book) + '=' + bName;
    end;

  modulo.BeginUpdate;
  try
    modulo.Clear;
    if hasOT and hasNT then
    begin
      for i := 0 to ESwordONTLines - 1 do
        modulo.Add(allVerses[i]);
    end
    else if hasOT then
    begin
      for i := 0 to ESwordOTLines - 1 do
        modulo.Add(allVerses[i]);
    end
    else
    begin
      for i := ESwordOTLines to ESwordONTLines - 1 do
        modulo.Add(allVerses[i]);
    end;
    modulo.Add('description=' + titleStr);
    modulo.Add('short.title=' + abbrevStr);
    modulo.Add('title=' + titleStr);
  finally
    modulo.EndUpdate;
  end;

  Result := True;
end;

{ ======================================================================
  USFM 3.0 writer
  One theWord verse becomes the lines of a USFM 3.0 verse: headings (\s1..\s3),
  the paragraph / poetry marker chosen from the <CM>/<CL>/<PIn> tags,
  \v, character styles (nested ones with the + prefix), \w word|lemma strong
  x-morph \w*, footnotes (\f) and cross-references (\x).
  ====================================================================== }

function USFMBookDisplayName(BookID: Integer): string;
begin
  if (BookID >= 1) and (BookID <= 66) then
    Result := USFMBookNames[BookID]
  else if (BookID >= 67) and (BookID <= 87) then
    Result := DefaultApocryphaBookName(BookID)
  else
    Result := TESwordModule.BookIDToUSFMCode(BookID);
end;

function USFMParaMarkerFor(const Bound: string; Indent: Integer): string;
begin
  if Bound = '<CL>' then
  begin
    if Indent > 4 then
      Indent := 4;
    if Indent > 0 then
      Result := '\q' + IntToStr(Indent)
    else
      Result := '\m';
  end
  else
  begin
    if Indent > 3 then
      Indent := 3;
    if Indent > 0 then
      Result := '\pi' + IntToStr(Indent)
    else
      Result := '\p';
  end;
end;

function USFMSafeText(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if Result[I] in ['\', #9, #10, #13] then
      if Result[I] = '\' then
        Result[I] := '/'
      else
        Result[I] := ' ';
  Result := StringReplace(Result, '//', '/ /', [rfReplaceAll]);
end;

type
  TUSFMVerseWriter = class
  private
    FOut: TStringList;
    FLine: string;
    FStack: TStringList;
    FWtStart: Integer;
    procedure OpenStyle(const AName: string);
    procedure CloseStyle(const AName: string);
    procedure CloseAll;
    procedure ReopenAll(Saved: TStringList);
    procedure EndLine;
    procedure StartLine(const Marker: string);
    function LineIsBareMarker: Boolean;
    procedure BreakWith(const Marker: string; Saved: TStringList);
    procedure WrapWord(const Strongs, Morphs, Lemmas: string);
    function CrossRef(const Items: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    { Lines of the verse, joined by LF. PrevBound is the paragraph boundary at
      the end of the verse before; TrailBound returns this verse's own. }
    function Convert(const Ont: string; Chap, Verse: Integer; const PrevBound: string;
      out TrailBound: string): string;
  end;

constructor TUSFMVerseWriter.Create;
begin
  inherited Create;
  FOut := TStringList.Create;
  FStack := TStringList.Create;
end;

destructor TUSFMVerseWriter.Destroy;
begin
  FStack.Free;
  FOut.Free;
  inherited Destroy;
end;

procedure TUSFMVerseWriter.OpenStyle(const AName: string);
begin
  if FStack.Count = 0 then
    FLine := FLine + '\' + AName + ' '
  else
    FLine := FLine + '\+' + AName + ' ';
  FStack.Add(AName);
end;

procedure TUSFMVerseWriter.CloseStyle(const AName: string);
var
  Idx, J: Integer;
  Saved: TStringList;
  Mk: string;
begin
  Idx := -1;
  for J := FStack.Count - 1 downto 0 do
    if FStack[J] = AName then
    begin
      Idx := J;
      Break;
    end;
  if Idx < 0 then
    Exit;
  Saved := TStringList.Create;
  try
    for J := Idx + 1 to FStack.Count - 1 do
      Saved.Add(FStack[J]);
    for J := FStack.Count - 1 downto Idx do
    begin
      if J > 0 then
        Mk := '\+' + FStack[J]
      else
        Mk := '\' + FStack[J];
      if (Length(FLine) >= Length(Mk) + 1) and
         (Copy(FLine, Length(FLine) - Length(Mk), Length(Mk) + 1) = Mk + ' ') then
        SetLength(FLine, Length(FLine) - Length(Mk) - 1) // nothing inside: drop the pair
      else
        FLine := FLine + Mk + '*';
      FStack.Delete(J);
    end;
    ReopenAll(Saved);
  finally
    Saved.Free;
  end;
end;

procedure TUSFMVerseWriter.CloseAll;
var
  J: Integer;
  Mk: string;
begin
  for J := FStack.Count - 1 downto 0 do
  begin
    if J > 0 then
      Mk := '\+' + FStack[J]
    else
      Mk := '\' + FStack[J];
    if (Length(FLine) >= Length(Mk) + 1) and
       (Copy(FLine, Length(FLine) - Length(Mk), Length(Mk) + 1) = Mk + ' ') then
      SetLength(FLine, Length(FLine) - Length(Mk) - 1)
    else
      FLine := FLine + Mk + '*';
  end;
  FStack.Clear;
end;

procedure TUSFMVerseWriter.ReopenAll(Saved: TStringList);
var
  J: Integer;
begin
  for J := 0 to Saved.Count - 1 do
    OpenStyle(Saved[J]);
end;

procedure TUSFMVerseWriter.EndLine;
begin
  FLine := TrimRight(FLine);
  if FLine <> '' then
    FOut.Add(FLine);
  FLine := '';
  FWtStart := 0;
end;

procedure TUSFMVerseWriter.StartLine(const Marker: string);
begin
  FOut.Add(Marker);
end;

{ a paragraph marker on the line being built, with no text after it yet }
function TUSFMVerseWriter.LineIsBareMarker: Boolean;
var
  T: string;
  N: Integer;
begin
  T := Trim(FLine);
  Result := False;
  if (Length(T) < 2) or (T[1] <> '') then
    Exit;
  N := 2;
  while (N <= Length(T)) and (T[N] in ['a'..'z', '0'..'9']) do
    Inc(N);
  Result := N > Length(T);
end;

procedure TUSFMVerseWriter.BreakWith(const Marker: string; Saved: TStringList);
begin
  Saved.Clear;
  Saved.AddStrings(FStack);
  CloseAll;
  if not LineIsBareMarker then
    EndLine;
  FLine := Marker + ' ';
  FWtStart := 0;
  ReopenAll(Saved);
end;

procedure TUSFMVerseWriter.WrapWord(const Strongs, Morphs, Lemmas: string);
var
  StartPos, P: Integer;
  Phrase, Lead, Attrs, Pre: string;
begin
  if Strongs = '' then
    Exit;
  if (FWtStart > 0) and (FWtStart <= Length(FLine) + 1) then
    StartPos := FWtStart
  else
  begin
    P := Length(FLine);
    while (P >= 1) and (FLine[P] <> ' ') do
      Dec(P);
    StartPos := P + 1;
  end;
  FWtStart := 0;
  Phrase := Copy(FLine, StartPos, MaxInt);
  Lead := '';
  while (Phrase <> '') and (Phrase[1] in ['(', '[', '"', '''', ' ']) do
  begin
    Lead := Lead + Phrase[1];
    Delete(Phrase, 1, 1);
  end;
  if (Phrase = '') or (Pos('\', Phrase) > 0) or (Phrase[Length(Phrase)] = ' ') then
    Exit;
  Phrase := StringReplace(Phrase, '|', ' ', [rfReplaceAll]);
  Attrs := '';
  if Lemmas <> '' then
    Attrs := 'lemma="' + StringReplace(Lemmas, '"', '', [rfReplaceAll]) + '" ';
  Attrs := Attrs + 'strong="' + Strongs + '"';
  if Morphs <> '' then
    Attrs := Attrs + ' x-morph="' + StringReplace(Morphs, '"', '', [rfReplaceAll]) + '"';
  if FStack.Count > 0 then
    Pre := '\+w '
  else
    Pre := '\w ';
  FLine := Copy(FLine, 1, StartPos - 1) + Lead + Pre + Phrase + '|' + Attrs +
    Copy(Pre, 1, Length(Pre) - 1) + '*';
end;

{ <RX 43.5.26;1.1.1-3> items -> "JHN 5:26; GEN 1:1-3" }
function TUSFMVerseWriter.CrossRef(const Items: string): string;
var
  List: TStringList;
  K, Dot1, Dot2, B, C, V, V2, Sep: Integer;
  Item, Rest: string;
begin
  Result := '';
  List := TStringList.Create;
  try
    List.Delimiter := ';';
    List.StrictDelimiter := True;
    List.DelimitedText := Items;
    for K := 0 to List.Count - 1 do
    begin
      Item := Trim(List[K]);
      Dot1 := Pos('.', Item);
      if Dot1 = 0 then
        Continue;
      B := StrToIntDef(Copy(Item, 1, Dot1 - 1), 0);
      Rest := Copy(Item, Dot1 + 1, MaxInt);
      Dot2 := Pos('.', Rest);
      if (B < 1) or (Dot2 = 0) then
        Continue;
      C := StrToIntDef(Copy(Rest, 1, Dot2 - 1), 0);
      Rest := Copy(Rest, Dot2 + 1, MaxInt);
      V2 := 0;
      Sep := Pos('-', Rest);
      if Sep > 0 then
      begin
        V := ExtractLeadingInt(Copy(Rest, 1, Sep - 1), 0);
        V2 := ExtractLeadingInt(Copy(Rest, Sep + 1, MaxInt), 0);
      end
      else
      begin
        Sep := Pos('+', Rest);
        if Sep > 0 then
        begin
          V := ExtractLeadingInt(Copy(Rest, 1, Sep - 1), 0);
          V2 := V + ExtractLeadingInt(Copy(Rest, Sep + 1, MaxInt), 0);
        end
        else
          V := ExtractLeadingInt(Rest, 0);
      end;
      if (C < 1) or (V < 1) then
        Continue;
      if Result <> '' then
        Result := Result + '; ';
      Result := Result + TESwordModule.BookIDToUSFMCode(B) + ' ' + IntToStr(C) + ':' + IntToStr(V);
      if V2 > V then
        Result := Result + '-' + IntToStr(V2);
    end;
  finally
    List.Free;
  end;
end;

function TUSFMVerseWriter.Convert(const Ont: string; Chap, Verse: Integer;
  const PrevBound: string; out TrailBound: string): string;
var
  S, Raw, Up, Bound, Bnd, Note, Refs, Strongs, Morphs, Lemmas, Tmp, Tail: string;
  I, Len, E, K, J, Indent, PendIndent, Level, Q1, Q2: Integer;
  Heads: TStringList;
  Saved: TStringList;
  AllBlank: Boolean;
  Ch: Char;

  function TagAt(Pos0: Integer; out EndPos: Integer): string;
  begin
    Result := '';
    EndPos := PosEx('>', S, Pos0);
    if EndPos > 0 then
      Result := Copy(S, Pos0 + 1, EndPos - Pos0 - 1);
  end;

  function IsStrongTag(const R: string): Boolean;
  begin
    Result := (Length(R) >= 3) and (R[1] = 'W') and (R[2] in ['G', 'H']) and (R[3] in ['0'..'9']);
  end;

  function StrongOf(const R: string): string;
  var
    N: Integer;
  begin
    N := 3;
    while (N <= Length(R)) and (R[N] in ['0'..'9']) do
      Inc(N);
    Result := R[2] + Copy(R, 3, N - 3);
  end;

  function LemmaOf(const R: string): string;
  var
    A, B: Integer;
  begin
    Result := '';
    A := Pos(' l="', R);
    if A = 0 then
      Exit;
    B := PosEx('"', R, A + 4);
    if B > 0 then
      Result := Copy(R, A + 4, B - A - 4);
  end;

  function IsParaTag(const R: string): Boolean;
  begin
    Result := (R = 'CM') or (R = 'CI') or (R = 'CL');
  end;

  function IsIndentTag(const R: string): Boolean;
  begin
    Result := (R = 'PI') or (R = 'Pi') or ((Length(R) = 3) and (Copy(R, 1, 2) = 'PI') and (R[3] in ['0'..'9']));
  end;

  function IsFirstLineTag(const R: string): Boolean;
  begin
    Result := (R = 'PF') or ((Length(R) = 3) and (Copy(R, 1, 2) = 'PF') and (R[3] in ['0'..'9']));
  end;

  function IndentOf(const R: string): Integer;
  begin
    if R = 'PI' then
      Result := 1
    else if R = 'Pi' then
      Result := 0
    else
      Result := Ord(R[3]) - Ord('0');
  end;

  function HeadingLevel(const R: string): Integer;
  begin
    Result := 0;
    if R = 'TS' then
      Result := 1
    else if (Length(R) = 3) and (Copy(R, 1, 2) = 'TS') and (R[3] in ['1'..'3']) then
      Result := Ord(R[3]) - Ord('0');
  end;

begin
  FOut.Clear;
  FStack.Clear;
  FLine := '';
  FWtStart := 0;
  TrailBound := '';
  Result := '';
  S := Ont;
  if Pos(#239#187#191, S) = 1 then
    Delete(S, 1, 3);
  S := Trim(S);
  Len := Length(S);
  I := 1;
  Heads := TStringList.Create;
  Saved := TStringList.Create;
  try
    // ---- the tags in front of the text: headings, boundary, indent
    Bound := '';
    Indent := 0;
    while I <= Len do
    begin
      if S[I] = ' ' then
      begin
        Inc(I);
        Continue;
      end;
      if S[I] <> '<' then
        Break;
      Raw := TagAt(I, E);
      if E = 0 then
        Break;
      Level := HeadingLevel(Raw);
      if Level > 0 then
      begin
        K := PosEx('<Ts>', S, E + 1);
        if K = 0 then
          Break;
        Tmp := Trim(StripAllXMLTags(Copy(S, E + 1, K - E - 1)));
        if Tmp <> '' then
          Heads.Add('\s' + IntToStr(Level) + ' ' + USFMSafeText(Tmp));
        I := K + 4;
      end
      else if IsParaTag(Raw) then
      begin
        Bound := '<' + Raw + '>';
        I := E + 1;
      end
      else if IsIndentTag(Raw) then
      begin
        Indent := IndentOf(Raw);
        I := E + 1;
      end
      else if IsFirstLineTag(Raw) then
        I := E + 1
      else
        Break;
    end;
    if Bound = '' then
      Bound := PrevBound;
    for J := 0 to Heads.Count - 1 do
      FOut.Add(Heads[J]);
    if (Heads.Count > 0) or (Bound <> '') or (Indent > 0) then
      StartLine(USFMParaMarkerFor(Bound, Indent));
    FLine := '\v ' + IntToStr(Verse) + ' ';

    // ---- the verse text
    while I <= Len do
    begin
      Ch := S[I];
      if Ch <> '<' then
      begin
        if Ch in ['\', #9, #10, #13] then
        begin
          if Ch = '\' then
            Ch := '/'
          else
            Ch := ' ';
        end;
        FLine := FLine + Ch;
        Inc(I);
        Continue;
      end;
      Raw := TagAt(I, E);
      if E = 0 then
      begin
        FLine := FLine + '<';
        Inc(I);
        Continue;
      end;
      Up := UpperCase(Raw);

      if IsStrongTag(Raw) then
      begin
        Strongs := '';
        Morphs := '';
        Lemmas := '';
        while True do
        begin
          if IsStrongTag(Raw) then
          begin
            if Strongs <> '' then
              Strongs := Strongs + ',';
            Strongs := Strongs + StrongOf(Raw);
          end
          else if (Length(Raw) > 2) and (Copy(Raw, 1, 2) = 'WT') then
          begin
            Tmp := CleanMorphTagContent(Copy(Raw, 3, MaxInt));
            if Tmp <> '' then
            begin
              if Morphs <> '' then
                Morphs := Morphs + ' ';
              Morphs := Morphs + Tmp;
            end;
            Tmp := LemmaOf(Raw);
            if Tmp <> '' then
            begin
              if Lemmas <> '' then
                Lemmas := Lemmas + ' ';
              Lemmas := Lemmas + Tmp;
            end;
          end
          else if Raw <> 'wt' then
            Break;
          I := E + 1;
          if (I > Len) or (S[I] <> '<') then
            Break;
          Raw := TagAt(I, E);
          if E = 0 then
            Break;
        end;
        WrapWord(Strongs, Morphs, Lemmas);
        Continue;
      end;

      if Raw = 'wt' then
        FWtStart := Length(FLine) + 1
      else if HeadingLevel(Raw) > 0 then
      begin
        K := PosEx('<Ts>', S, E + 1);
        if K > 0 then
        begin
          Tmp := Trim(StripAllXMLTags(Copy(S, E + 1, K - E - 1)));
          Saved.Clear;
          Saved.AddStrings(FStack);
          CloseAll;
          EndLine;
          if Tmp <> '' then
          begin
            FOut.Add('\s' + IntToStr(HeadingLevel(Raw)) + ' ' + USFMSafeText(Tmp));
            StartLine('\p');
            ReopenAll(Saved);
          end;
          I := K + 4;
          Continue;
        end;
      end
      else if IsParaTag(Raw) then
      begin
        Bnd := '<' + Raw + '>';
        PendIndent := 0;
        J := E + 1;
        while J <= Len do
        begin
          if S[J] = ' ' then
          begin
            Inc(J);
            Continue;
          end;
          if S[J] <> '<' then
            Break;
          Tmp := TagAt(J, K);
          if K = 0 then
            Break;
          if IsParaTag(Tmp) then
          begin
            if (Tmp = 'CM') or (Bnd = '') then
              Bnd := '<' + Tmp + '>';
          end
          else if IsIndentTag(Tmp) then
            PendIndent := IndentOf(Tmp)
          else if not IsFirstLineTag(Tmp) then
            Break;
          J := K + 1;
        end;
        // anything but blanks and closing tags left in the verse?
        Tail := Copy(S, J, MaxInt);
        AllBlank := Trim(StripAllXMLTags(Tail)) = '';
        if AllBlank and (Pos('<WG', Tail) = 0) and (Pos('<WH', Tail) = 0) and
           (Pos('<RF', Tail) = 0) and (Pos('<RX', Tail) = 0) then
        begin
          TrailBound := Bnd;
          I := Len + 1;
          Break;
        end;
        BreakWith(USFMParaMarkerFor(Bnd, PendIndent), Saved);
        I := J;
        Continue;
      end
      else if IsIndentTag(Raw) or IsFirstLineTag(Raw) then
      begin
        // an indent without a boundary inside a verse: no USFM equivalent
      end
      else if Raw = 'FR' then
        OpenStyle('wj')
      else if Raw = 'Fr' then
        CloseStyle('wj')
      else if Raw = 'FI' then
        OpenStyle('add')
      else if Raw = 'Fi' then
        CloseStyle('add')
      else if Raw = 'FO' then
        OpenStyle('qt')
      else if Raw = 'Fo' then
        CloseStyle('qt')
      else if Up = 'B' then
        OpenStyle('bd')
      else if Up = '/B' then
        CloseStyle('bd')
      else if Up = 'I' then
        OpenStyle('it')
      else if Up = '/I' then
        CloseStyle('it')
      else if Up = 'SUP' then
        OpenStyle('sup')
      else if Up = '/SUP' then
        CloseStyle('sup')
      else if (Raw = 'RF') or (Copy(Raw, 1, 3) = 'RF ') then
      begin
        K := PosEx('<Rf>', S, E + 1);
        if K > 0 then
        begin
          Note := Trim(USFMSafeText(StripAllXMLTags(Copy(S, E + 1, K - E - 1))));
          if Note <> '' then
            FLine := FLine + '\f + \ft ' + Note + '\f*';
          I := K + 4;
          Continue;
        end;
      end
      else if (Copy(Raw, 1, 3) = 'RX ') then
      begin
        Refs := '';
        J := I;
        while (J <= Len) and (S[J] = '<') do
        begin
          Tmp := TagAt(J, K);
          if (K = 0) or (Copy(Tmp, 1, 3) <> 'RX ') then
            Break;
          if Refs <> '' then
            Refs := Refs + ';';
          Refs := Refs + Trim(Copy(Tmp, 4, MaxInt));
          J := K + 1;
          // blanks between two references are dropped; blanks after the last stay
          Q1 := J;
          while (Q1 <= Len) and (S[Q1] = ' ') do
            Inc(Q1);
          if (Q1 <= Len) and (Copy(S, Q1, 4) = '<RX ') then
            J := Q1;
        end;
        Refs := CrossRef(Refs);
        if Refs <> '' then
          FLine := FLine + '\x - \xo ' + IntToStr(Chap) + ':' + IntToStr(Verse) + ' \xt ' + Refs + '\x*';
        I := J;
        Continue;
      end
      else if (Raw = 'NB') or (Copy(Raw, 1, 3) = 'NB ') then
      begin
        K := PosEx('<Nb>', S, E + 1);
        if K > 0 then
        begin
          I := K + 4;
          Continue;
        end;
      end;
      I := E + 1;
    end;

    CloseAll;
    EndLine;
    Result := FOut.Text;
    while (Result <> '') and (Result[Length(Result)] in [#10, #13]) do
      SetLength(Result, Length(Result) - 1);
    Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
  finally
    Saved.Free;
    Heads.Free;
  end;
end;

class function TESwordModule.ConvertONTToUSFX(const ontText: string; out headingBeforeVerse: string): string;
var
  s, rawTag, tagUpper, plainRun, wordToken, prefixPunct, strongList, morphCode, attrStr: string;
  i, len, tagEnd, k, wordStart: integer;
begin
  Result := '';
  headingBeforeVerse := '';
  s := ontText;
  if Pos(#239#187#191, s) = 1 then
    Delete(s, 1, 3);
  s := Trim(s);
  if s = '' then
    Exit;

  plainRun := '';
  i := 1;
  len := Length(s);
  while i <= len do
  begin
    if s[i] = '<' then
    begin
      tagEnd := i + 1;
      while (tagEnd <= len) and (s[tagEnd] <> '>') do
        Inc(tagEnd);
      if tagEnd > len then
      begin
        plainRun := plainRun + Copy(s, i, len - i + 1);
        Break;
      end;

      rawTag := Copy(s, i + 1, tagEnd - i - 1);
      tagUpper := UpperCase(Trim(rawTag));

      if (Length(rawTag) >= 3) and (AnsiStartsText('WG', rawTag) or AnsiStartsText('WH', rawTag)) and (rawTag[3] in ['0'..'9']) then
      begin
        strongList := UpperCase(Copy(rawTag, 2, 1)) + Copy(rawTag, 3, Length(rawTag) - 2);
        morphCode := '';
        i := tagEnd + 1;
        while (i <= len) and (s[i] = '<') do
        begin
          tagEnd := i + 1;
          while (tagEnd <= len) and (s[tagEnd] <> '>') do
            Inc(tagEnd);
          if tagEnd > len then Break;
          rawTag := Copy(s, i + 1, tagEnd - i - 1);
          if (Length(rawTag) >= 3) and (AnsiStartsText('WG', rawTag) or AnsiStartsText('WH', rawTag)) and (rawTag[3] in ['0'..'9']) then
          begin
            strongList := strongList + ' ' + UpperCase(Copy(rawTag, 2, 1)) + Copy(rawTag, 3, Length(rawTag) - 2);
            i := tagEnd + 1;
          end
          else if (Length(rawTag) > 2) and AnsiStartsText('WT', rawTag) then
          begin
            morphCode := CleanMorphTagContent(Copy(rawTag, 3, Length(rawTag) - 2));
            i := tagEnd + 1;
          end
          else if SameText(rawTag, 'wt') then
            i := tagEnd + 1
          else
            Break;
        end;

        wordStart := Length(plainRun);
        while (wordStart >= 1) and not (plainRun[wordStart] in [' ', #9]) do
          Dec(wordStart);
        wordToken := Copy(plainRun, wordStart + 1, Length(plainRun) - wordStart);
        SetLength(plainRun, wordStart);
        Result := Result + EscapePlainTextToESwordHTML(plainRun);
        plainRun := '';

        prefixPunct := '';
        k := 1;
        while (k <= Length(wordToken)) and (wordToken[k] in ['(', '[', '"', '''']) do
        begin
          prefixPunct := prefixPunct + wordToken[k];
          Inc(k);
        end;
        Delete(wordToken, 1, k - 1);

        attrStr := 's="' + EscapePlainTextToESwordHTML(strongList) + '"';
        if morphCode <> '' then
          attrStr := attrStr + ' m="' + EscapePlainTextToESwordHTML(morphCode) + '"';

        if wordToken <> '' then
          Result := Result + EscapePlainTextToESwordHTML(prefixPunct) + '<w ' + attrStr + '>' + EscapePlainTextToESwordHTML(wordToken) + '</w>'
        else
          Result := Result + EscapePlainTextToESwordHTML(prefixPunct);
        Continue;
      end;

      if plainRun <> '' then
      begin
        Result := Result + EscapePlainTextToESwordHTML(plainRun);
        plainRun := '';
      end;

      if AnsiStartsStr('TS', rawTag) then
      begin
        k := PosEx('<Ts>', s, tagEnd + 1);
        if k > 0 then
        begin
          headingBeforeVerse := Trim(headingBeforeVerse + ' ' + StripAllXMLTags(Copy(s, tagEnd + 1, k - tagEnd - 1)));
          i := k + 4;
          Continue;
        end;
      end
      else if (rawTag = 'RF') or AnsiStartsStr('RF ', rawTag) then
      begin
        k := PosEx('<Rf>', s, tagEnd + 1);
        if k > 0 then
        begin
          Result := Result + '<f caller="+"><ft>' + EscapePlainTextToESwordHTML(StripAllXMLTags(Copy(s, tagEnd + 1, k - tagEnd - 1))) + '</ft></f>';
          i := k + 4;
          Continue;
        end;
      end
      else if rawTag = 'FR' then
        Result := Result + '<wj>'
      else if rawTag = 'Fr' then
        Result := Result + '</wj>'
      else if (rawTag = 'FI') or (tagUpper = 'I') then
        Result := Result + '<add>'
      else if (rawTag = 'Fi') or (tagUpper = '/I') then
        Result := Result + '</add>'
      else if (rawTag = 'FO') or (tagUpper = 'B') then
        Result := Result + '<bd>'
      else if (rawTag = 'Fo') or (tagUpper = '/B') then
        Result := Result + '</bd>'
      else if (tagUpper = 'CM') or (tagUpper = 'CL') or (tagUpper = 'CI') then
        Result := Result + ' ';

      i := tagEnd + 1;
      Continue;
    end;

    plainRun := plainRun + s[i];
    Inc(i);
  end;
  if plainRun <> '' then
    Result := Result + EscapePlainTextToESwordHTML(plainRun);
  Result := Trim(Result);
end;

class function TESwordModule.IsUSFMZipFileName(const filename: string): boolean;
begin
  Result := AnsiEndsStr('.zip', LowerCase(filename));
end;

function ResolveExportUSFMCode(BookID: Integer): string;
var
  CanonID: Integer;
begin
  if BookID >= FIRST_APOCRYPHA_BOOK then
  begin
    CanonID := CanonicalApocryphaIDByName(ApocryphaBookName(BookID));
    if CanonID > 0 then
      Exit(TESwordModule.BookIDToUSFMCode(CanonID));
  end;
  Result := TESwordModule.BookIDToUSFMCode(BookID);
end;

function ResolveExportBookDisplayName(BookID: Integer): string;
var
  ProjName: string;
begin
  if BookID >= FIRST_APOCRYPHA_BOOK then
  begin
    ProjName := Trim(ApocryphaBookName(BookID));
    if ProjName <> '' then
      Exit(ProjName);
  end;
  Result := USFMBookDisplayName(BookID);
end;

{ the USFM 3.0 books of a Bible, one text per book; FileNames gets the name of each
  book's file (Paratext style: 01-GEN.usfm ... 41-MAT.usfm ...) }
procedure BuildUSFMBookFiles(verses, refIDs: TStringList; const props, fallbackTitle: string;
  hasOT, hasNT: Boolean; FileNames, Texts: TStringList);
var
  propList, bookLines: TStringList;
  writer: TUSFMVerseWriter;
  titleStr, line, code, nameStr, verseText, trail, prevBound, bookText: string;
  i, curBook, curChap, extraCount, fileNo: Integer;
  b, c, v: smallint;

  procedure FlushBook;
  begin
    if curBook <= 0 then
      Exit;
    bookText := StringReplace(bookLines.Text, #13#10, #10, [rfReplaceAll]);
    bookText := StringReplace(bookText, #10, LineEnding, [rfReplaceAll]);
    if (curBook >= 1) and (curBook <= 39) then
      fileNo := curBook
    else if (curBook >= 40) and (curBook <= 66) then
      fileNo := curBook + 1
    else
      fileNo := 67 + extraCount;
    FileNames.Add(Format('%.2d-%s.usfm', [fileNo, ResolveExportUSFMCode(curBook)]));
    Texts.Add(bookText);
    bookLines.Clear;
  end;

begin
  propList := TStringList.Create;
  bookLines := TStringList.Create;
  writer := TUSFMVerseWriter.Create;
  try
    propList.Text := props;
    titleStr := Trim(propList.Values['description']);
    if titleStr = '' then
      titleStr := Trim(propList.Values['title']);
    if titleStr = '' then
      titleStr := fallbackTitle;
    titleStr := Trim(StringReplace(StringReplace(titleStr, #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]));

    curBook := -1;
    curChap := -1;
    extraCount := 0;
    prevBound := '<CM>';
    for i := 0 to verses.Count - 1 do
    begin
      line := Trim(verses[i]);
      if line = '' then
        Continue;
      if (not Assigned(refIDs)) or (i >= refIDs.Count) or (SScanf(refIDs[i], '%d,%d,%d', [@b, @c, @v]) <> 3) then
        Continue;
      if (((b <= 39) or (b >= 67)) and (not hasOT)) or ((b >= 40) and (b <= 66) and (not hasNT)) then
        Continue;

      if b <> curBook then
      begin
        FlushBook;
        if b >= 67 then
          Inc(extraCount);
        curBook := b;
        curChap := -1;
        code := ResolveExportUSFMCode(b);
        nameStr := ResolveExportBookDisplayName(b);
        bookLines.Add('\id ' + code + ' ' + titleStr);
        bookLines.Add('\usfm 3.0');
        bookLines.Add('\ide UTF-8');
        { tells the readers (Timotheos) that 5-digit numbers such as G13970 are
          the real Strong's number, not an unfoldingWord number with a 0 added }
        bookLines.Add('\rem iBiblia: Strong''s numbers are written exactly as tagged');
        bookLines.Add('\h ' + nameStr);
        bookLines.Add('\toc1 ' + nameStr);
        bookLines.Add('\toc2 ' + nameStr);
        bookLines.Add('\toc3 ' + code);
        bookLines.Add('\mt1 ' + nameStr);
      end;

      if c <> curChap then
      begin
        curChap := c;
        prevBound := '<CM>';
        bookLines.Add('\c ' + IntToStr(c));
      end;

      verseText := writer.Convert(line, c, v, prevBound, trail);
      prevBound := trail;
      bookLines.Add(verseText);
    end;
    FlushBook;
  finally
    writer.Free;
    bookLines.Free;
    propList.Free;
  end;
end;

procedure WriteUSFMTextFile(const fileName, text: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fileName, fmCreate or fmOpenWrite or fmShareDenyWrite);
  try
    fs.Size := 0;
    if text <> '' then
      fs.WriteBuffer(text[1], Length(text));
  finally
    fs.Free;
  end;
end;

class procedure TESwordModule.ExportUSFMBible(verses, refIDs: TStringList; const filename, props: string; hasOT, hasNT: boolean);
begin
  ExportUSFMBibleEx(verses, refIDs, filename, filename, props, hasOT, hasNT, False);
end;

{ zipOnly = False: one .usfm file with all the books, and a folder "<name>_usfm"
  beside it with one USFM 3.0 file per book. zipOnly = True: a .zip of the
  per-book files. filename is where to write (a staging name when the exporter
  stages); finalFileName is where it will end up, and names the folder. }
class procedure TESwordModule.ExportUSFMBibleEx(verses, refIDs: TStringList;
  const filename, finalFileName, props: string; hasOT, hasNT, zipOnly: boolean);
var
  names, texts: TStringList;
  stagedFile, folder, all: string;
  i: integer;
  zipper: TZipper;
  ms: TMemoryStream;
  streamList: TList;
begin
  names := TStringList.Create;
  texts := TStringList.Create;
  try
    BuildUSFMBookFiles(verses, refIDs, props, ChangeFileExt(ExtractFileName(finalFileName), ''),
      hasOT, hasNT, names, texts);
    if names.Count = 0 then
      raise Exception.Create('There is no Bible text to export as USFM.');
    stagedFile := GetStagingFileName(filename);
    if FileExists(stagedFile) then
      SysUtils.DeleteFile(stagedFile);

    if zipOnly then
    begin
      zipper := TZipper.Create;
      streamList := TList.Create;
      try
        zipper.FileName := stagedFile;
        for i := 0 to names.Count - 1 do
        begin
          ms := TMemoryStream.Create;
          streamList.Add(ms);
          if texts[i] <> '' then
            ms.WriteBuffer(texts[i][1], Length(texts[i]));
          ms.Position := 0;
          zipper.Entries.AddFileEntry(ms, names[i]);
        end;
        zipper.ZipAllFiles;
      finally
        zipper.Free;
        for i := 0 to streamList.Count - 1 do
          TObject(streamList[i]).Free;
        streamList.Free;
      end;
      if not PublishStagedFile(stagedFile, filename) then
        raise Exception.CreateFmt('Unable to publish the exported USFM 3.0 zip to "%s".', [filename]);
      Exit;
    end;

    all := '';
    for i := 0 to texts.Count - 1 do
      all := all + texts[i];
    WriteUSFMTextFile(stagedFile, all);
    if not PublishStagedFile(stagedFile, filename) then
      raise Exception.CreateFmt('Unable to publish exported USFM Bible module to "%s".', [filename]);

    // the same books, one file each, in a folder beside the combined file
    folder := ExtractFilePath(ExpandFileName(finalFileName)) +
      ChangeFileExt(ExtractFileName(finalFileName), '') + '_usfm';
    if not ForceDirectories(folder) then
      raise Exception.CreateFmt('The USFM file was written, but the folder "%s" for the per-book files could not be created.', [folder]);
    for i := 0 to names.Count - 1 do
      WriteUSFMTextFile(IncludeTrailingPathDelimiter(folder) + names[i], texts[i]);
  finally
    texts.Free;
    names.Free;
  end;
end;

class procedure TESwordModule.ExportUSFXBible(verses, refIDs: TStringList; const filename, props: string; hasOT, hasNT: boolean);
var
  outLines: TStringList;
  stagedFile, line, hdrText, usfxLine, bookCode: string;
  i, curBook, curChap: integer;
  b, c, v: smallint;
begin
  stagedFile := GetStagingFileName(filename);
  outLines := TStringList.Create;
  try
    outLines.Add('<?xml version="1.0" encoding="utf-8"?>');
    outLines.Add('<usfx xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://eBible.org/usfx/usfx-2009-07-12.xsd">');

    curBook := -1;
    curChap := -1;
    for i := 0 to verses.Count - 1 do
    begin
      line := Trim(verses[i]);
      if line = '' then
        Continue;
      if (not Assigned(refIDs)) or (i >= refIDs.Count) or (SScanf(refIDs[i], '%d,%d,%d', [@b, @c, @v]) <> 3) then
        Continue;
      if (((b <= 39) or (b >= 67)) and (not hasOT)) or ((b >= 40) and (b <= 66) and (not hasNT)) then
        Continue;

      if b <> curBook then
      begin
        if curBook > 0 then
          outLines.Add('  </book>');
        curBook := b;
        curChap := -1;
        bookCode := ResolveExportUSFMCode(b);
        outLines.Add('  <book id="' + bookCode + '">');
      end;

      if c <> curChap then
      begin
        curChap := c;
        outLines.Add('    <c id="' + IntToStr(c) + '" />');
      end;

      usfxLine := ConvertONTToUSFX(line, hdrText);
      if hdrText <> '' then
        outLines.Add('    <s>' + EscapePlainTextToESwordHTML(hdrText) + '</s>');
      outLines.Add('    <v id="' + IntToStr(v) + '" />' + usfxLine + '<ve />');
    end;

    if curBook > 0 then
      outLines.Add('  </book>');
    outLines.Add('</usfx>');

    outLines.SaveToFile(stagedFile);
    if not PublishStagedFile(stagedFile, filename) then
      raise Exception.CreateFmt('Unable to publish exported USFX XML Bible module to "%s".', [filename]);
  finally
    outLines.Free;
  end;
end;

end.

