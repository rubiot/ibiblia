unit verse_lookup;

{ Verse lookup: turns what the user types in the "Go to verse" box into a
  book, chapter and verse, the way theWord's lookup box does.

  Accepted input (case does not matter):
    jn 3 16      jn 3:16      jn 3.16      jn3.16      John 3 16
    1co 13 4     1 cor 13:4   i cor 13.4   ps 23       jude 5
    3 16         -> chapter 3 verse 16 of the current book
    3            -> chapter 3 of the current book
    :16  or .16  -> verse 16 of the current chapter
  A space, a period, a colon or a comma all separate chapter and verse.
  Books are found by a standard abbreviation, by their full name, or by the
  first letters of the name (English, the interface language, or Tagalog).
  When several books fit, a book that exists in the project wins, in Bible
  order. This unit has no user-interface code. }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BibleScope;

type
  TVerseLookupStatus = (
    vlsEmpty,            // nothing usable typed yet
    vlsOk,               // BookID/Chapter/Verse are valid
    vlsUnknownBook,      // the book text matches no book
    vlsBookNotInProject, // a real book, but outside this project's scope
    vlsNoSuchChapter,
    vlsNoSuchVerse
  );

{ Scope is the project's scope; CurrentBookID/CurrentChapter are where the
  project is now (used when no book, or only a verse, is typed).
  BookID is the absolute book number (1 = Genesis .. 66 = Revelation, 67+ =
  the project's extra books). On every status except vlsEmpty and
  vlsUnknownBook the out values hold what was understood so far. }
function ParseVerseLookup(const Input: string; Scope: TEscopoTexto;
  CurrentBookID, CurrentChapter: Integer;
  out BookID, Chapter, Verse: Integer): TVerseLookupStatus;

{ Display name of a book for the preview line. }
function VerseLookupBookName(BookID: Integer): string;

implementation

uses
  LazUTF8, bible_book_names, bible_versification, apocrypha_books;

type
  TBookAlias = record
    Key: string;
    BookID: Integer;
  end;

const
  MAX_LOOKUP_NUMBER = 99999;

  EnglishBookNames: array[1..66] of string = (
    'Genesis', 'Exodus', 'Leviticus', 'Numbers', 'Deuteronomy', 'Joshua',
    'Judges', 'Ruth', '1 Samuel', '2 Samuel', '1 Kings', '2 Kings',
    '1 Chronicles', '2 Chronicles', 'Ezra', 'Nehemiah', 'Esther', 'Job',
    'Psalms', 'Proverbs', 'Ecclesiastes', 'Song of Solomon', 'Isaiah',
    'Jeremiah', 'Lamentations', 'Ezekiel', 'Daniel', 'Hosea', 'Joel',
    'Amos', 'Obadiah', 'Jonah', 'Micah', 'Nahum', 'Habakkuk', 'Zephaniah',
    'Haggai', 'Zechariah', 'Malachi', 'Matthew', 'Mark', 'Luke', 'John',
    'Acts', 'Romans', '1 Corinthians', '2 Corinthians', 'Galatians',
    'Ephesians', 'Philippians', 'Colossians', '1 Thessalonians',
    '2 Thessalonians', '1 Timothy', '2 Timothy', 'Titus', 'Philemon',
    'Hebrews', 'James', '1 Peter', '2 Peter', '1 John', '2 John', '3 John',
    'Jude', 'Revelation'
  );

  { Tagalog names, so "mateo 5 3" or "1 corinto 13" also work. }
  TagalogBookNames: array[1..66] of string = (
    'Genesis', 'Exodo', 'Levitico', 'Bilang', 'Deuteronomio', 'Josue',
    'Hukom', 'Ruth', '1 Samuel', '2 Samuel', '1 Hari', '2 Hari',
    '1 Cronica', '2 Cronica', 'Ezra', 'Nehemias', 'Ester', 'Job', 'Awit',
    'Kawikaan', 'Mangangaral', 'Awit ni Solomon', 'Isaias', 'Jeremias',
    'Panaghoy', 'Ezekiel', 'Daniel', 'Oseas', 'Joel', 'Amos', 'Obadias',
    'Jonas', 'Mikas', 'Nahum', 'Habacuc', 'Zefanias', 'Hagai', 'Zacarias',
    'Malakias', 'Mateo', 'Marcos', 'Lucas', 'Juan', 'Gawa', 'Roma',
    '1 Corinto', '2 Corinto', 'Galacia', 'Efeso', 'Filipos', 'Colosas',
    '1 Tesalonica', '2 Tesalonica', '1 Timoteo', '2 Timoteo', 'Tito',
    'Filemon', 'Hebreo', 'Santiago', '1 Pedro', '2 Pedro', '1 Juan',
    '2 Juan', '3 Juan', 'Judas', 'Pahayag'
  );

  { Abbreviations that are not simply the first letters of the name. }
  BookAliases: array[0..103] of TBookAlias = (
    (Key: 'gn'; BookID: 1),
    (Key: 'lv'; BookID: 3),
    (Key: 'nm'; BookID: 4),
    (Key: 'nb'; BookID: 4),
    (Key: 'dt'; BookID: 5),
    (Key: 'jsh'; BookID: 6),
    (Key: 'jdg'; BookID: 7),
    (Key: 'jg'; BookID: 7),
    (Key: 'jgs'; BookID: 7),
    (Key: 'rth'; BookID: 8),
    (Key: '1sm'; BookID: 9),
    (Key: '2sm'; BookID: 10),
    (Key: '1kgs'; BookID: 11),
    (Key: '1kg'; BookID: 11),
    (Key: '2kgs'; BookID: 12),
    (Key: '2kg'; BookID: 12),
    (Key: '1chr'; BookID: 13),
    (Key: '2chr'; BookID: 14),
    (Key: 'ne'; BookID: 16),
    (Key: 'est'; BookID: 17),
    (Key: 'jb'; BookID: 18),
    (Key: 'psm'; BookID: 19),
    (Key: 'pss'; BookID: 19),
    (Key: 'pslm'; BookID: 19),
    (Key: 'psalm'; BookID: 19),
    (Key: 'prv'; BookID: 20),
    (Key: 'ec'; BookID: 21),
    (Key: 'qoh'; BookID: 21),
    (Key: 'sos'; BookID: 22),
    (Key: 'sng'; BookID: 22),
    (Key: 'ss'; BookID: 22),
    (Key: 'cant'; BookID: 22),
    (Key: 'canticles'; BookID: 22),
    (Key: 'songofsongs'; BookID: 22),
    (Key: 'isa'; BookID: 23),
    (Key: 'jr'; BookID: 24),
    (Key: 'la'; BookID: 25),
    (Key: 'ezk'; BookID: 26),
    (Key: 'dn'; BookID: 27),
    (Key: 'hs'; BookID: 28),
    (Key: 'jl'; BookID: 29),
    (Key: 'obd'; BookID: 31),
    (Key: 'jnh'; BookID: 32),
    (Key: 'mc'; BookID: 33),
    (Key: 'na'; BookID: 34),
    (Key: 'zph'; BookID: 36),
    (Key: 'zp'; BookID: 36),
    (Key: 'hg'; BookID: 37),
    (Key: 'zch'; BookID: 38),
    (Key: 'zc'; BookID: 38),
    (Key: 'ml'; BookID: 39),
    (Key: 'mt'; BookID: 40),
    (Key: 'mk'; BookID: 41),
    (Key: 'mr'; BookID: 41),
    (Key: 'mrk'; BookID: 41),
    (Key: 'lk'; BookID: 42),
    (Key: 'jn'; BookID: 43),
    (Key: 'jhn'; BookID: 43),
    (Key: 'ac'; BookID: 44),
    (Key: 'rm'; BookID: 45),
    (Key: '1co'; BookID: 46),
    (Key: '2co'; BookID: 47),
    (Key: 'ga'; BookID: 48),
    (Key: 'ep'; BookID: 49),
    (Key: 'php'; BookID: 50),
    (Key: 'pp'; BookID: 50),
    (Key: 'phl'; BookID: 50),
    (Key: 'phil'; BookID: 50),
    (Key: 'co'; BookID: 51),
    (Key: '1ts'; BookID: 52),
    (Key: '1th'; BookID: 52),
    (Key: '2ts'; BookID: 53),
    (Key: '2th'; BookID: 53),
    (Key: '1tm'; BookID: 54),
    (Key: '1ti'; BookID: 54),
    (Key: '2tm'; BookID: 55),
    (Key: '2ti'; BookID: 55),
    (Key: 'tts'; BookID: 56),
    (Key: 'tit'; BookID: 56),
    (Key: 'phm'; BookID: 57),
    (Key: 'phlm'; BookID: 57),
    (Key: 'pm'; BookID: 57),
    (Key: 'philem'; BookID: 57),
    (Key: 'hb'; BookID: 58),
    (Key: 'heb'; BookID: 58),
    (Key: 'jas'; BookID: 59),
    (Key: 'jm'; BookID: 59),
    (Key: '1pt'; BookID: 60),
    (Key: '1pe'; BookID: 60),
    (Key: '2pt'; BookID: 61),
    (Key: '2pe'; BookID: 61),
    (Key: '1jn'; BookID: 62),
    (Key: '1jhn'; BookID: 62),
    (Key: '2jn'; BookID: 63),
    (Key: '2jhn'; BookID: 63),
    (Key: '3jn'; BookID: 64),
    (Key: '3jhn'; BookID: 64),
    (Key: 'jud'; BookID: 65),
    (Key: 'jde'; BookID: 65),
    (Key: 'rv'; BookID: 66),
    (Key: 're'; BookID: 66),
    (Key: 'apoc'; BookID: 66),
    (Key: 'apocalypse'; BookID: 66),
    (Key: 'apocalipsis'; BookID: 66)
  );

function IsLetterChar(C: Char): Boolean;
begin
  Result := (C in ['a'..'z']) or (C >= #128);
end;

function IsBookSeparator(C: Char): Boolean;
begin
  Result := (C = ' ') or (C = '.') or (C = #9);
end;

{ Lower case, letters and digits only: "1 Cor." -> "1cor". }
function NormalizeBookKey(const S: string): string;
var
  Lowered: string;
  I: Integer;
begin
  Lowered := UTF8LowerCase(S);
  Result := '';
  for I := 1 to Length(Lowered) do
    if IsLetterChar(Lowered[I]) or (Lowered[I] in ['0'..'9']) then
      Result := Result + Lowered[I];
end;

function BookAvailable(Scope: TEscopoTexto; BookID: Integer): Boolean;
begin
  Result := BibleBookInScope(Scope, BookID) or IsApocryphaBook(BookID);
end;

function LookupChapterCount(BookID: Integer): Integer;
begin
  if IsApocryphaBook(BookID) then
    Result := ApocryphaChapterCount(BookID)
  else
    Result := BibleChapterCount(BookID);
end;

function LookupVerseCount(BookID, Chapter: Integer): Integer;
begin
  if IsApocryphaBook(BookID) then
    Result := ApocryphaVerseCount(BookID, Chapter)
  else
    Result := BibleVerseCount(BookID, Chapter);
end;

function NameMatches(const Name, Key: string; Exact: Boolean): Boolean;
var
  Normalized: string;
begin
  Normalized := NormalizeBookKey(Name);
  if Normalized = '' then
    Exit(False);
  if Exact then
    Result := Normalized = Key
  else
    Result := (Length(Key) <= Length(Normalized)) and
      (Copy(Normalized, 1, Length(Key)) = Key);
end;

{ Remembers the first match that is in the project (Best) and the first one
  that is not (Fallback). }
procedure ConsiderBook(Candidate: Integer; Scope: TEscopoTexto;
  var Best, Fallback: Integer);
begin
  if Candidate <= 0 then
    Exit;
  if BookAvailable(Scope, Candidate) then
  begin
    if Best = 0 then
      Best := Candidate;
  end
  else if Fallback = 0 then
    Fallback := Candidate;
end;

function FindBookByKey(const Key: string; Scope: TEscopoTexto): Integer;
var
  Best, Fallback, I, Pass, Book: Integer;
  Exact, EnglishPass: Boolean;
begin
  Result := 0;
  if Key = '' then
    Exit;
  Best := 0;
  Fallback := 0;

  for I := Low(BookAliases) to High(BookAliases) do
    if BookAliases[I].Key = Key then
      ConsiderBook(BookAliases[I].BookID, Scope, Best, Fallback);
  if Best > 0 then
    Exit(Best);

  { Pass 1: whole name, English / interface language / project's extra books.
    Pass 2: whole name, Tagalog.
    Pass 3: first letters, English / interface language / extra books.
    Pass 4: first letters, Tagalog. }
  for Pass := 1 to 4 do
  begin
    Exact := Pass <= 2;
    EnglishPass := Odd(Pass);
    for Book := 1 to 66 do
      if EnglishPass then
      begin
        if NameMatches(EnglishBookNames[Book], Key, Exact) or
           NameMatches(LocalizedBookNameByID(Book), Key, Exact) then
          ConsiderBook(Book, Scope, Best, Fallback);
      end
      else if NameMatches(TagalogBookNames[Book], Key, Exact) then
        ConsiderBook(Book, Scope, Best, Fallback);
    if EnglishPass then
      for I := 0 to ApocryphaBookCount - 1 do
        if NameMatches(ApocryphaBookAt(I).Name, Key, Exact) then
          ConsiderBook(ApocryphaBookAt(I).BookID, Scope, Best, Fallback);
    if Best > 0 then
      Exit(Best);
  end;

  Result := Fallback;
end;

function FindBook(const Key: string; Scope: TEscopoTexto): Integer;
begin
  Result := FindBookByKey(Key, Scope);
  { "Mga Gawa", "Mga Awit" }
  if (Result = 0) and (Length(Key) > 3) and (Copy(Key, 1, 3) = 'mga') then
    Result := FindBookByKey(Copy(Key, 4, MaxInt), Scope);
  { Roman numerals: "I Cor", "II Tim", "III Jn" }
  if (Result = 0) and (Length(Key) > 3) and (Copy(Key, 1, 3) = 'iii') then
    Result := FindBookByKey('3' + Copy(Key, 4, MaxInt), Scope);
  if (Result = 0) and (Length(Key) > 2) and (Copy(Key, 1, 2) = 'ii') then
    Result := FindBookByKey('2' + Copy(Key, 3, MaxInt), Scope);
  if (Result = 0) and (Length(Key) > 1) and (Key[1] = 'i') then
    Result := FindBookByKey('1' + Copy(Key, 2, MaxInt), Scope);
end;

function VerseLookupBookName(BookID: Integer): string;
begin
  Result := LocalizedBookNameByID(BookID);
  if (Result = '') and (BookID >= 1) and (BookID <= 66) then
    Result := EnglishBookNames[BookID];
end;

function ParseVerseLookup(const Input: string; Scope: TEscopoTexto;
  CurrentBookID, CurrentChapter: Integer;
  out BookID, Chapter, Verse: Integer): TVerseLookupStatus;
var
  S, BookKey: string;
  P, Q, L, NumberCount, Value, Digits: Integer;
  Numbers: array[0..1] of Integer;
  VerseOnly: Boolean;
begin
  BookID := 0;
  Chapter := 0;
  Verse := 0;
  Result := vlsEmpty;

  S := UTF8LowerCase(Trim(Input));
  L := Length(S);
  if L = 0 then
    Exit;
  P := 1;
  BookKey := '';

  { A leading 1, 2 or 3 belongs to the book name only when letters follow
    it: "1 jn 2 3", "1jn 2 3". Otherwise it is a chapter: "3 16". }
  if S[P] in ['1'..'3'] then
  begin
    Q := P + 1;
    while (Q <= L) and ((S[Q] = ' ') or (S[Q] = #9)) do
      Inc(Q);
    if (Q <= L) and IsLetterChar(S[Q]) then
    begin
      BookKey := S[P];
      P := Q;
    end;
  end;

  { Book name: letters, with spaces or periods between words. }
  while P <= L do
  begin
    if IsLetterChar(S[P]) then
    begin
      BookKey := BookKey + S[P];
      Inc(P);
    end
    else if IsBookSeparator(S[P]) and (BookKey <> '') then
    begin
      Q := P;
      while (Q <= L) and IsBookSeparator(S[Q]) do
        Inc(Q);
      if (Q <= L) and IsLetterChar(S[Q]) then
        P := Q
      else
        Break;
    end
    else
      Break;
  end;

  { ":16" or ".16" with no book = a verse of the current chapter. }
  Q := P;
  while (Q <= L) and ((S[Q] = ' ') or (S[Q] = #9)) do
    Inc(Q);
  VerseOnly := (BookKey = '') and (Q <= L) and ((S[Q] = ':') or (S[Q] = '.'));

  { Up to two numbers; anything that is not a digit separates them. }
  NumberCount := 0;
  Numbers[0] := 0;
  Numbers[1] := 0;
  while (P <= L) and (NumberCount < 2) do
  begin
    if S[P] in ['0'..'9'] then
    begin
      Value := 0;
      Digits := 0;
      while (P <= L) and (S[P] in ['0'..'9']) do
      begin
        if Digits < 5 then
          Value := Value * 10 + (Ord(S[P]) - Ord('0'))
        else
          Value := MAX_LOOKUP_NUMBER;
        Inc(Digits);
        Inc(P);
      end;
      Numbers[NumberCount] := Value;
      Inc(NumberCount);
    end
    else
      Inc(P);
  end;

  if BookKey <> '' then
  begin
    BookID := FindBook(BookKey, Scope);
    if BookID = 0 then
      Exit(vlsUnknownBook);
    case NumberCount of
      0:
        begin
          Chapter := 1;
          Verse := 1;
        end;
      1:
        if LookupChapterCount(BookID) = 1 then
        begin
          { One-chapter books: "jude 5" means verse 5. }
          Chapter := 1;
          Verse := Numbers[0];
        end
        else
        begin
          Chapter := Numbers[0];
          Verse := 1;
        end;
    else
      begin
        Chapter := Numbers[0];
        Verse := Numbers[1];
      end;
    end;
  end
  else
  begin
    if (NumberCount = 0) or (CurrentBookID <= 0) then
      Exit(vlsEmpty);
    BookID := CurrentBookID;
    if NumberCount = 1 then
    begin
      if VerseOnly then
      begin
        Chapter := CurrentChapter;
        Verse := Numbers[0];
      end
      else if LookupChapterCount(BookID) = 1 then
      begin
        Chapter := 1;
        Verse := Numbers[0];
      end
      else
      begin
        Chapter := Numbers[0];
        Verse := 1;
      end;
    end
    else
    begin
      Chapter := Numbers[0];
      Verse := Numbers[1];
    end;
  end;

  if not BookAvailable(Scope, BookID) then
    Exit(vlsBookNotInProject);
  if (Chapter < 1) or (Chapter > LookupChapterCount(BookID)) then
    Exit(vlsNoSuchChapter);
  if (Verse < 1) or (Verse > LookupVerseCount(BookID, Chapter)) then
    Exit(vlsNoSuchVerse);
  Result := vlsOk;
end;

end.
