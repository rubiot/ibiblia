unit bible_import_source;

{ Reads a Bible file into a list of verses (book, chapter, verse, text) for
  "Add Bible Books" and the partial "Replace text": theWord modules
  (.ont/.ot/.nt), every format module_conversion handles (e-Sword, USFM,
  USFX, MySword) and plain documents (.txt, .rtf, .odt, .docx).

  Plain documents have no book numbers, so the books found in them are
  listed with a guessed Bible book (DetectedBooks) that the user confirms
  before the verses are used (ApplyBookMapping). Verse numbers with a letter
  (Esther 1:1a, written as a raised "1" followed by a letter) are joined to
  their base verse. This unit has no user-interface code. }

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, BibleScope;

type
  TImportVerse = record
    Book: Integer;
    Chapter: Integer;
    Verse: Integer;
    Text: string;
  end;
  TImportVerseArray = array of TImportVerse;

  TDetectedBook = record
    Name: string;        // as written in the document
    BookID: Integer;     // guessed / chosen Bible book, 0 = skip
    Chapters: Integer;
    Verses: Integer;
  end;
  TDetectedBookArray = array of TDetectedBook;

  { TBibleImportSource }

  TBibleImportSource = class
  private
    FFileName: string;
    FVerses: TImportVerseArray;
    FVerseCount: Integer;
    FDetected: TDetectedBookArray;
    FDetectedVerses: TImportVerseArray;   // Book = index in FDetected
    FDetectedCount: Integer;
    FIsPlainDocument: Boolean;
    FApocryphaNames: TStringList;         // "67=Tobit"
    FIndex: TStringList;                  // "b,c,v" -> verse index
    FUnplacedVerses: Integer;
    FDuplicateVerses: Integer;
    procedure AddVerse(Book, Chapter, Verse: Integer; const Text: string);
    procedure AddDetectedVerse(BookIndex, Chapter, Verse: Integer;
      const Text: string);
    function LoadTheWordModule(const ModuleFile: string;
      out ErrorMessage: string): Boolean;
    function LoadPlainDocument(const FileName: string;
      out ErrorMessage: string): Boolean;
    procedure ParseParagraphs(Paragraphs: TStrings);
    procedure BuildIndex;
    function GetDetected(Index: Integer): TDetectedBook;
    function GetDetectedCount: Integer;
    function GetVerse(Index: Integer): TImportVerse;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(const FileName: string;
      out ErrorMessage: string): Boolean;
    { Plain documents: choose the Bible book of detected book Index
      (0 = do not import it), then call ApplyBookMapping. }
    procedure SetDetectedBookID(Index, BookID: Integer);
    procedure ApplyBookMapping;
    { Book numbers present, in Bible order (canonical, then 67+). }
    function BookIDs: TStringList;
    { Apocrypha book numbers in the order they appear in the file ("69,67"). }
    function ApocryphaOrder: string;
    function BookName(BookID: Integer): string;
    { Combining several files (a compressed file with many books): start an
      empty source, append the verses of chosen books of other sources in the
      order wanted, then call FinishCombined. }
    procedure BeginCombined(const AName: string);
    procedure AppendBook(Other: TBibleImportSource; BookID: Integer);
    procedure FinishCombined;
    function BookVerseCount(BookID: Integer): Integer;
    function TryGetText(const ReferenceID: string; out Text: string): Boolean;
    function ReferenceID(Index: Integer): string;
    property FileName: string read FFileName;
    property IsPlainDocument: Boolean read FIsPlainDocument;
    property DetectedCount: Integer read GetDetectedCount;
    property Detected[Index: Integer]: TDetectedBook read GetDetected;
    property VerseCount: Integer read FVerseCount;
    property Verses[Index: Integer]: TImportVerse read GetVerse;
    { Verses of a plain document that could not be given a chapter / verse
      (e.g. text before the first chapter of a book with chapters). }
    property UnplacedVerses: Integer read FUnplacedVerses;
    { Verse numbers that came twice in a plain document (texts joined). }
    property DuplicateVerses: Integer read FDuplicateVerses;
    property ApocryphaNames: TStringList read FApocryphaNames;
  end;

{ Bible book number for a book title in Greek, English, Tagalog or the
  interface language, including the deuterocanonical books (67+); 0 when
  nothing fits. }
function GuessBibleBookID(const Name: string): Integer;
{ Name of a book number (canonical or 67+). }
function AnyBibleBookName(BookID: Integer): string;
{ Chapter number of a chapter heading line (0 = not one, -1 = no number). }
function ChapterMarker(const Line: string): Integer;
{ Bible order key: canonical books first, then 67+. }
function BibleBookOrder(BookID: Integer): Integer;

const
  LAST_IMPORT_BOOK = 150;

resourcestring
  SNoVersesInFile = 'No Bible verses were found in this file.';
  SUnsupportedBibleFile = 'This kind of file cannot be read as a Bible.';

implementation

uses
  LazUTF8, StrUtils, Math, bible_versification, bible_book_names, apocrypha_books,
  verse_lookup, module_conversion, bible_document_reader, esword_usfm;

{ ---------- Greek helpers ---------- }

{ Lower case Greek / Latin letters without accents or breathings; other
  characters unchanged. Works on UTF-16. }
function BaseLetter(C: WideChar): WideChar;
const
  Map: array[0..1] of UnicodeString = (
    'άέήίόύώΐΰϊϋἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱἲἳἴἵἶἷὀὁὂὃὄὅὐὑὒὓὔὕὖὗὠὡὢὣὤὥὦὧὰάὲέὴήὶίὸόὺύὼώᾀᾁᾂᾃᾄᾅᾆᾇᾐᾑᾒᾓᾔᾕᾖᾗᾠᾡᾢᾣᾤᾥᾦᾧᾰᾱᾲᾳᾴᾶᾷῂῃῄῆῇῐῑῒΐῖῗῠῡῢΰῤῥῦῧῲῳῴῶῷ',
    'αεηιουωιυιυααααααααεεεεεεηηηηηηηηιιιιιιιιοοοοοουυυυυυυυωωωωωωωωααεεηηιιοουυωωααααααααηηηηηηηηωωωωωωωωααααααααηηηηηιιιιιιυυυυρρυυωωωωω');
var
  P: Integer;
  L: UnicodeString;
begin
  if (C >= 'A') and (C <= 'Z') then
    C := WideChar(Ord(C) + 32)
  else if (Ord(C) >= $0391) and (Ord(C) <= $03A9) then
    C := WideChar(Ord(C) + 32)
  else
    case Ord(C) of
      $0386: C := 'α'; $0388: C := 'ε'; $0389: C := 'η'; $038A: C := 'ι';
      $038C: C := 'ο'; $038E: C := 'υ'; $038F: C := 'ω'; $03AA: C := 'ι';
      $03AB: C := 'υ';
    end;
  if (Ord(C) >= $1F08) and (Ord(C) <= $1FFF) then
  begin
    { capital letters with breathings: same base as the small ones }
    L := UTF8Decode(UTF8LowerCase(UTF8Encode(UnicodeString(C))));
    if L <> '' then
      C := L[1];
  end;
  P := Pos(C, Map[0]);
  if P > 0 then
    Result := Map[1][P]
  else if C = 'ς' then
    Result := 'σ'
  else
    Result := C;
end;

{ Letters and digits only, lower case, no accents. Apostrophes and the Greek
  numeral sign are dropped. Words separated by single spaces. }
function NormalizeTitle(const S: string): UnicodeString;
var
  U: UnicodeString;
  I: Integer;
  C: WideChar;
  LastSpace: Boolean;
begin
  U := UTF8Decode(S);
  Result := '';
  LastSpace := True;
  for I := 1 to Length(U) do
  begin
    C := BaseLetter(U[I]);
    if (C >= 'a') and (C <= 'z') or (C >= '0') and (C <= '9') or
       (C >= 'α') and (C <= 'ω') then
    begin
      Result := Result + C;
      LastSpace := False;
    end
    else if (C = ' ') or (C = '-') or (C = '.') or (C = #9) then
    begin
      if not LastSpace then
        Result := Result + ' ';
      LastSpace := True;
    end;
  end;
  Result := Trim(Result);
  { Latin look-alike letters typed inside Greek words ("Αμβακοyμ") }
  if (Pos('α', Result) > 0) or (Pos('ο', Result) > 0) or (Pos('ι', Result) > 0) or
     (Pos('ε', Result) > 0) or (Pos('η', Result) > 0) or (Pos('ω', Result) > 0) then
  begin
    Result := StringReplace(Result, 'y', 'υ', [rfReplaceAll]);
    Result := StringReplace(Result, 'i', 'ι', [rfReplaceAll]);
    Result := StringReplace(Result, 'o', 'ο', [rfReplaceAll]);
    Result := StringReplace(Result, 'a', 'α', [rfReplaceAll]);
    Result := StringReplace(Result, 'e', 'ε', [rfReplaceAll]);
    Result := StringReplace(Result, 'k', 'κ', [rfReplaceAll]);
    Result := StringReplace(Result, 'v', 'ν', [rfReplaceAll]);
    Result := StringReplace(Result, 'u', 'υ', [rfReplaceAll]);
  end;
end;

type
  TGreekTitle = record
    Key: UnicodeString;   // normalized stem, no spaces
    Ordinal: Integer;     // 0 = none, 1..4
    BookID: Integer;
  end;

const
  GreekTitles: array[0..121] of TGreekTitle = (
    (Key: 'γενεσισ'; Ordinal: 0; BookID: 1),
    (Key: 'εξοδοσ'; Ordinal: 0; BookID: 2),
    (Key: 'λευιτικον'; Ordinal: 0; BookID: 3),
    (Key: 'αριθμοι'; Ordinal: 0; BookID: 4),
    (Key: 'δευτερονομιον'; Ordinal: 0; BookID: 5),
    (Key: 'ιησουσναυη'; Ordinal: 0; BookID: 6),
    (Key: 'ιησουσ'; Ordinal: 0; BookID: 6),
    (Key: 'κριται'; Ordinal: 0; BookID: 7),
    (Key: 'ρουθ'; Ordinal: 0; BookID: 8),
    (Key: 'βασιλειων'; Ordinal: 1; BookID: 9),
    (Key: 'βασιλειων'; Ordinal: 2; BookID: 10),
    (Key: 'βασιλειων'; Ordinal: 3; BookID: 11),
    (Key: 'βασιλειων'; Ordinal: 4; BookID: 12),
    (Key: 'σαμουηλ'; Ordinal: 1; BookID: 9),
    (Key: 'σαμουηλ'; Ordinal: 2; BookID: 10),
    (Key: 'παραλειπομενων'; Ordinal: 1; BookID: 13),
    (Key: 'παραλειπομενων'; Ordinal: 2; BookID: 14),
    (Key: 'παραλεοπομενων'; Ordinal: 1; BookID: 13),
    (Key: 'παραλεοπομενων'; Ordinal: 2; BookID: 14),
    (Key: 'χρονικων'; Ordinal: 1; BookID: 13),
    (Key: 'χρονικων'; Ordinal: 2; BookID: 14),
    (Key: 'εσδρασ'; Ordinal: 2; BookID: 15),
    (Key: 'εζρασ'; Ordinal: 0; BookID: 15),
    (Key: 'νεεμιασ'; Ordinal: 0; BookID: 16),
    (Key: 'εσθηρ'; Ordinal: 0; BookID: 17),
    (Key: 'ιωβ'; Ordinal: 0; BookID: 18),
    (Key: 'ψαλμοι'; Ordinal: 0; BookID: 19),
    (Key: 'ψαλτηριον'; Ordinal: 0; BookID: 19),
    (Key: 'παροιμιαι'; Ordinal: 0; BookID: 20),
    (Key: 'παροιμιαισολομωντοσ'; Ordinal: 0; BookID: 20),
    (Key: 'εκκλησιαστησ'; Ordinal: 0; BookID: 21),
    (Key: 'ασμαασματων'; Ordinal: 0; BookID: 22),
    (Key: 'ησαιασ'; Ordinal: 0; BookID: 23),
    (Key: 'ιερεμιασ'; Ordinal: 0; BookID: 24),
    (Key: 'θρηνοι'; Ordinal: 0; BookID: 25),
    (Key: 'θρηνοιιερεμιου'; Ordinal: 0; BookID: 25),
    (Key: 'ιεζεκιηλ'; Ordinal: 0; BookID: 26),
    (Key: 'δανιηλ'; Ordinal: 0; BookID: 27),
    (Key: 'ωσηε'; Ordinal: 0; BookID: 28),
    (Key: 'ιωηλ'; Ordinal: 0; BookID: 29),
    (Key: 'αμωσ'; Ordinal: 0; BookID: 30),
    (Key: 'οβδιου'; Ordinal: 0; BookID: 31),
    (Key: 'αβδιου'; Ordinal: 0; BookID: 31),
    (Key: 'ιωνασ'; Ordinal: 0; BookID: 32),
    (Key: 'μιχαιασ'; Ordinal: 0; BookID: 33),
    (Key: 'ναουμ'; Ordinal: 0; BookID: 34),
    (Key: 'αμβακουμ'; Ordinal: 0; BookID: 35),
    (Key: 'αμβακουμ'; Ordinal: 0; BookID: 35),
    (Key: 'σοφονιασ'; Ordinal: 0; BookID: 36),
    (Key: 'αγγαιοσ'; Ordinal: 0; BookID: 37),
    (Key: 'ζαχαριασ'; Ordinal: 0; BookID: 38),
    (Key: 'μαλαχιασ'; Ordinal: 0; BookID: 39),
    (Key: 'καταματθαιον'; Ordinal: 0; BookID: 40),
    (Key: 'ματθαιον'; Ordinal: 0; BookID: 40),
    (Key: 'καταμαρκον'; Ordinal: 0; BookID: 41),
    (Key: 'μαρκον'; Ordinal: 0; BookID: 41),
    (Key: 'καταλουκαν'; Ordinal: 0; BookID: 42),
    (Key: 'λουκαν'; Ordinal: 0; BookID: 42),
    (Key: 'καταιωαννην'; Ordinal: 0; BookID: 43),
    (Key: 'ιωαννην'; Ordinal: 0; BookID: 43),
    (Key: 'πραξεισαποστολων'; Ordinal: 0; BookID: 44),
    (Key: 'πραξεισ'; Ordinal: 0; BookID: 44),
    (Key: 'προσρωμαιουσ'; Ordinal: 0; BookID: 45),
    (Key: 'προσκορινθιουσ'; Ordinal: 1; BookID: 46),
    (Key: 'προσκορινθιουσ'; Ordinal: 2; BookID: 47),
    (Key: 'προσγαλατασ'; Ordinal: 0; BookID: 48),
    (Key: 'προσεφεσιουσ'; Ordinal: 0; BookID: 49),
    (Key: 'προσφιλιππησιουσ'; Ordinal: 0; BookID: 50),
    (Key: 'προσφιληππησιουσ'; Ordinal: 0; BookID: 50),
    (Key: 'προσκολοσσαεισ'; Ordinal: 0; BookID: 51),
    (Key: 'προσθεσσαλονικεισ'; Ordinal: 1; BookID: 52),
    (Key: 'προσθεσσαλονικεισ'; Ordinal: 2; BookID: 53),
    (Key: 'προστιμοθεον'; Ordinal: 1; BookID: 54),
    (Key: 'προστιμοθεον'; Ordinal: 2; BookID: 55),
    (Key: 'προστιτον'; Ordinal: 0; BookID: 56),
    (Key: 'προσφιλημονα'; Ordinal: 0; BookID: 57),
    (Key: 'προσεβραιουσ'; Ordinal: 0; BookID: 58),
    (Key: 'επιστοληιακωβου'; Ordinal: 0; BookID: 59),
    (Key: 'ιακωβου'; Ordinal: 0; BookID: 59),
    (Key: 'επιστοληπετρου'; Ordinal: 1; BookID: 60),
    (Key: 'επιστοληπετρου'; Ordinal: 2; BookID: 61),
    (Key: 'πετρου'; Ordinal: 1; BookID: 60),
    (Key: 'πετρου'; Ordinal: 2; BookID: 61),
    (Key: 'επιστοληιωαννου'; Ordinal: 1; BookID: 62),
    (Key: 'επιστοληιωαννου'; Ordinal: 2; BookID: 63),
    (Key: 'επιστοληιωαννου'; Ordinal: 3; BookID: 64),
    (Key: 'ιωαννου'; Ordinal: 1; BookID: 62),
    (Key: 'ιωαννου'; Ordinal: 2; BookID: 63),
    (Key: 'ιωαννου'; Ordinal: 3; BookID: 64),
    (Key: 'επιστοληιουδα'; Ordinal: 0; BookID: 65),
    (Key: 'ιουδα'; Ordinal: 0; BookID: 65),
    (Key: 'αποκαλυψισ'; Ordinal: 0; BookID: 66),
    (Key: 'αποκαλυψισιωαννου'; Ordinal: 0; BookID: 66),
    (Key: 'τωβιτ'; Ordinal: 0; BookID: 69),
    (Key: 'τωβιθ'; Ordinal: 0; BookID: 69),
    (Key: 'τωβιασ'; Ordinal: 0; BookID: 69),
    (Key: 'ιουδιθ'; Ordinal: 0; BookID: 70),
    (Key: 'σοφιασολομωντοσ'; Ordinal: 0; BookID: 76),
    (Key: 'σοφια'; Ordinal: 0; BookID: 76),
    (Key: 'σοφιασειραχ'; Ordinal: 0; BookID: 77),
    (Key: 'σιραχ'; Ordinal: 0; BookID: 77),
    (Key: 'σειραχ'; Ordinal: 0; BookID: 77),
    (Key: 'βαρουχ'; Ordinal: 0; BookID: 78),
    (Key: 'επιστοληιερεμιου'; Ordinal: 0; BookID: 79),
    (Key: 'προσευχηαζαριου'; Ordinal: 0; BookID: 80),
    (Key: 'τωντριωνπαιδωναινεσισ'; Ordinal: 0; BookID: 80),
    (Key: 'υμνοστωντριωνπαιδων'; Ordinal: 0; BookID: 80),
    (Key: 'σωσαννα'; Ordinal: 0; BookID: 81),
    (Key: 'βηλκαιδρακων'; Ordinal: 0; BookID: 82),
    (Key: 'βηλ'; Ordinal: 0; BookID: 82),
    (Key: 'μακκαβαιων'; Ordinal: 1; BookID: 71),
    (Key: 'μακκαβαιων'; Ordinal: 2; BookID: 72),
    (Key: 'μακκαβαιων'; Ordinal: 3; BookID: 73),
    (Key: 'μακκαβαιων'; Ordinal: 4; BookID: 74),
    (Key: 'εσδρασ'; Ordinal: 1; BookID: 67),
    (Key: 'εσδρασ'; Ordinal: 3; BookID: 68),
    (Key: 'προσευχημανασση'; Ordinal: 0; BookID: 75),
    (Key: 'προσευχημανασσηυιουεζεκιου'; Ordinal: 0; BookID: 75),
    (Key: 'ψαλμοσ151'; Ordinal: 0; BookID: 84),
    (Key: 'ωδαι'; Ordinal: 0; BookID: 86),
    (Key: 'ψαλμοισολομωντοσ'; Ordinal: 0; BookID: 85),
    (Key: 'ψαλμοισολομωνοσ'; Ordinal: 0; BookID: 85)
  );

type
  TEnglishTitle = record
    Key: string;
    Ordinal: Integer;
    BookID: Integer;
  end;

const
  EnglishApocrypha: array[0..27] of TEnglishTitle = (
    (Key: 'tobit'; Ordinal: 0; BookID: 69),
    (Key: 'tobias'; Ordinal: 0; BookID: 69),
    (Key: 'judith'; Ordinal: 0; BookID: 70),
    (Key: 'additionstoesther'; Ordinal: 0; BookID: 83),
    (Key: 'restofesther'; Ordinal: 0; BookID: 83),
    (Key: 'wisdom'; Ordinal: 0; BookID: 76),
    (Key: 'wisdomofsolomon'; Ordinal: 0; BookID: 76),
    (Key: 'sirach'; Ordinal: 0; BookID: 77),
    (Key: 'ecclesiasticus'; Ordinal: 0; BookID: 77),
    (Key: 'baruch'; Ordinal: 0; BookID: 78),
    (Key: 'letterofjeremiah'; Ordinal: 0; BookID: 79),
    (Key: 'epistleofjeremiah'; Ordinal: 0; BookID: 79),
    (Key: 'prayerofazariah'; Ordinal: 0; BookID: 80),
    (Key: 'songofthethreeyoungmen'; Ordinal: 0; BookID: 80),
    (Key: 'songofthethree'; Ordinal: 0; BookID: 80),
    (Key: 'susanna'; Ordinal: 0; BookID: 81),
    (Key: 'belandthedragon'; Ordinal: 0; BookID: 82),
    (Key: 'maccabees'; Ordinal: 1; BookID: 71),
    (Key: 'maccabees'; Ordinal: 2; BookID: 72),
    (Key: 'maccabees'; Ordinal: 3; BookID: 73),
    (Key: 'maccabees'; Ordinal: 4; BookID: 74),
    (Key: 'esdras'; Ordinal: 1; BookID: 67),
    (Key: 'esdras'; Ordinal: 2; BookID: 68),
    (Key: 'prayerofmanasseh'; Ordinal: 0; BookID: 75),
    (Key: 'prayerofmanasses'; Ordinal: 0; BookID: 75),
    (Key: 'psalm151'; Ordinal: 0; BookID: 84),
    (Key: 'odes'; Ordinal: 0; BookID: 86),
    (Key: 'psalmsofsolomon'; Ordinal: 0; BookID: 85)
  );

{ Splits a normalized title into an ordinal (1..4, from a leading/trailing
  "1", "I", "α" ... ) and the remaining words joined without spaces. }
procedure SplitOrdinal(const Title: UnicodeString; out Stem: UnicodeString;
  out Ordinal: Integer; AllowSuffix: Boolean);
var
  Words: TStringList;
  I, Value: Integer;
  W: UnicodeString;

  function OrdinalOf(const Token: UnicodeString): Integer;
  begin
    if (Token = '1') or (Token = 'i') or (Token = 'α') or (Token = 'πρωτη') or
       (Token = 'first') then
      Result := 1
    else if (Token = '2') or (Token = 'ii') or (Token = 'β') or
      (Token = 'δευτερα') or (Token = 'second') then
      Result := 2
    else if (Token = '3') or (Token = 'iii') or (Token = 'γ') or
      (Token = 'τριτη') or (Token = 'third') then
      Result := 3
    else if (Token = '4') or (Token = 'iv') or (Token = 'δ') or
      (Token = 'τεταρτη') or (Token = 'fourth') then
      Result := 4
    else
      Result := 0;
  end;

begin
  Stem := '';
  Ordinal := 0;
  Words := TStringList.Create;
  try
    Words.Delimiter := ' ';
    Words.StrictDelimiter := True;
    Words.DelimitedText := UTF8Encode(Title);
    for I := 0 to Words.Count - 1 do
    begin
      W := UTF8Decode(Words[I]);
      Value := OrdinalOf(W);
      if (Value > 0) and (Ordinal = 0) and ((I = 0) or (I = Words.Count - 1)) and
         (Words.Count > 1) then
        Ordinal := Value
      else
        Stem := Stem + W;
    end;
    { "βασιλειωνα" written without a space }
    if AllowSuffix and (Ordinal = 0) and (Length(Stem) > 4) then
    begin
      W := Copy(Stem, Length(Stem), 1);
      Value := OrdinalOf(W);
      if (Value > 0) and (W <> 'i') then
      begin
        Ordinal := Value;
        Stem := Copy(Stem, 1, Length(Stem) - 1);
      end;
    end;
  finally
    Words.Free;
  end;
end;

function GuessTitle(const Stem: UnicodeString; Ordinal: Integer): Integer;
var
  I: Integer;
  Plain: string;
begin
  Result := 0;
  for I := Low(GreekTitles) to High(GreekTitles) do
    if (GreekTitles[I].Key = Stem) and (GreekTitles[I].Ordinal = Ordinal) then
      Exit(GreekTitles[I].BookID);
  Plain := UTF8Encode(Stem);
  for I := Low(EnglishApocrypha) to High(EnglishApocrypha) do
    if (EnglishApocrypha[I].Key = Plain) and
       (EnglishApocrypha[I].Ordinal = Ordinal) then
      Exit(EnglishApocrypha[I].BookID);
end;

function GuessBibleBookID(const Name: string): Integer;
var
  Title, Stem: UnicodeString;
  Ordinal, I: Integer;
  Plain: string;
begin
  Result := 0;
  Title := NormalizeTitle(Name);
  if Length(Title) < 2 then
    Exit;
  SplitOrdinal(Title, Stem, Ordinal, False);
  Result := GuessTitle(Stem, Ordinal);
  if Result > 0 then
    Exit;
  SplitOrdinal(Title, Stem, Ordinal, True);
  Result := GuessTitle(Stem, Ordinal);
  if Result > 0 then
    Exit;
  { "Μακκαβαίων" or "Εσδρας" alone }
  for I := Low(GreekTitles) to High(GreekTitles) do
    if (GreekTitles[I].Key = Stem) and (Ordinal = 0) then
      Exit(GreekTitles[I].BookID);

  { English / Tagalog / interface language, canonical books }
  SplitOrdinal(Title, Stem, Ordinal, False);
  Plain := UTF8Encode(Stem);
  if Length(Stem) >= 3 then
  begin
    if Ordinal > 0 then
      Result := FindBookIDByName(IntToStr(Ordinal) + Plain)
    else
      Result := FindBookIDByName(Plain);
  end;
end;


function AnyBibleBookName(BookID: Integer): string;
begin
  if BookID >= FIRST_APOCRYPHA_BOOK then
    Result := ApocryphaBookName(BookID)
  else
    Result := LocalizedBookNameByID(BookID);
  if Result = '' then
    Result := Format('Book %d', [BookID]);
end;

function BibleBookOrder(BookID: Integer): Integer;
begin
  Result := BookID;
end;

{ ---------- plain document structure ---------- }

const
  SUPERS: UnicodeString = '⁰¹²³⁴⁵⁶⁷⁸⁹';
  SUBS: UnicodeString = '₀₁₂₃₄₅₆₇₈₉';

function SuperDigit(C: WideChar): Integer;
begin
  Result := Pos(C, SUPERS) - 1;
end;

function SubDigit(C: WideChar): Integer;
begin
  Result := Pos(C, SUBS) - 1;
end;

function IsSmallLetter(C: WideChar): Boolean;
var
  B: WideChar;
begin
  B := BaseLetter(C);
  Result := ((C >= 'a') and (C <= 'z')) or
    ((B >= 'α') and (B <= 'ω') and not ((Ord(C) >= $0386) and (Ord(C) <= $03AB)) and
     (UTF8LowerCase(UTF8Encode(UnicodeString(C))) = UTF8Encode(UnicodeString(C))));
end;

function HasVerseMarks(const U: UnicodeString): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(U) do
    if (SuperDigit(U[I]) >= 0) or (SubDigit(U[I]) >= 0) then
      Exit(True);
  Result := False;
end;

{ Value of a Greek numeral ("ΙΓ´" = 13); 0 when it is not one. }
function GreekNumeralValue(const S: UnicodeString): Integer;
const
  Letters: UnicodeString = 'αβγδεϛζηθικλμνξοπϙρστυφχψω';
  Values: array[1..26] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30,
    40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800);
var
  I, P: Integer;
  C: WideChar;
begin
  Result := 0;
  for I := 1 to Length(S) do
  begin
    C := BaseLetter(S[I]);
    if (C = '´') or (C = '''') or (C = 'ʹ') or (C = '΄') or (C = '’') or (C = WideChar($1FFD)) then
      Continue;
    if C = 'ς' then
      C := 'ϛ';
    P := Pos(C, Letters);
    if P = 0 then
      Exit(0);
    Inc(Result, Values[P]);
  end;
end;

{ Chapter number of a chapter heading line; 0 when the line is not one,
  -1 for a chapter heading that has no usable number. }
function ChapterMarker(const Line: string): Integer;
var
  N, FirstWord, Second: UnicodeString;
  Raw: string;
  P, I, J, Value: Integer;
begin
  Result := 0;
  Raw := Trim(Line);
  if Raw = '' then
    Exit;
  { a line holding only a number }
  if (Length(Raw) <= 3) then
  begin
    Value := StrToIntDef(Raw, -1);
    if (Value >= 0) and (IntToStr(Value) = Raw) then
    begin
      if Value = 0 then
        Exit(-1);
      Exit(Value);
    end;
  end;
  N := NormalizeTitle(Raw);
  P := Pos(' ', N);
  if P = 0 then
  begin
    { "Chapter12" is not supported; single words are not chapter lines }
    Exit;
  end;
  FirstWord := Copy(N, 1, P - 1);
  if not ((FirstWord = 'chapter') or (FirstWord = 'capitulo') or
     (FirstWord = 'kabanata') or (FirstWord = 'kapitulo') or
     (FirstWord = 'κεφαλαιο') or (FirstWord = 'κεφαλαιον') or
     (FirstWord = 'κεφ') or (FirstWord = 'psalm') or (FirstWord = 'salmo') or
     (FirstWord = 'awit') or (FirstWord = 'ψαλμοσ') or (FirstWord = 'chap')) then
    Exit;
  if Length(UTF8Decode(Raw)) > 40 then
    Exit;
  { first run of digits in the original line }
  I := 1;
  while (I <= Length(Raw)) and not (Raw[I] in ['0'..'9']) do
    Inc(I);
  if I <= Length(Raw) then
  begin
    { "Psalm (10)" continues the previous psalm }
    if Pos('(', Copy(Raw, 1, I - 1)) > 0 then
      Exit(0);
    J := I;
    while (J <= Length(Raw)) and (Raw[J] in ['0'..'9']) do
      Inc(J);
    Value := StrToIntDef(Copy(Raw, I, J - I), 0);
  end
  else
  begin
    Second := Trim(Copy(N, P + 1, MaxInt));
    P := Pos(' ', Second);
    if P > 0 then
      Second := Copy(Second, 1, P - 1);
    { Greek numerals only with a numeral sign: "ΚΕΦΑΛΑΙΟΝ ΙΓ´" }
    if (Pos('´', Raw) > 0) or (Pos('ʹ', Raw) > 0) or (Pos('΄', Raw) > 0) or
       (Pos(#$E1#$BF#$BD, Raw) > 0) or (Pos('''', Raw) > 0) or (Pos('’', Raw) > 0) then
      Value := GreekNumeralValue(Second)
    else
      Exit(0);
  end;
  if Value <= 0 then
    Value := -1;
  Result := Value;
end;

function LooksLikeTitle(const Line: string): Boolean;
var
  U: UnicodeString;
  Last: WideChar;
  Words, I: Integer;
  HasLetter: Boolean;
begin
  Result := False;
  U := UTF8Decode(Trim(Line));
  if (Length(U) = 0) or (Length(U) > 60) then
    Exit;
  if HasVerseMarks(U) then
    Exit;
  { a title starts with a letter (or "1 ", "I ", "Α'" ordinals) }
  if not ((BaseLetter(U[1]) <> U[1]) or ((U[1] >= 'A') and (U[1] <= 'Z')) or
     ((U[1] >= 'a') and (U[1] <= 'z')) or (U[1] >= #$0370) or
     ((U[1] >= '1') and (U[1] <= '4') and (Length(U) > 2) and (U[2] = ' '))) then
    Exit;
  { digits only as an ordinal: "from v.14" is not a title }
  for I := 2 to Length(U) do
    if (U[I] >= '0') and (U[I] <= '9') and (GuessBibleBookID(Line) = 0) then
      Exit;
  Last := U[Length(U)];
  if (Last = '.') or (Last = ',') or (Last = ';') or (Last = ':') or
     (Last = '·') or (Last = '·') then
    Exit;
  Words := 1;
  HasLetter := False;
  for I := 1 to Length(U) do
  begin
    if U[I] = ' ' then
      Inc(Words);
    if BaseLetter(U[I]) <> U[I] then
      HasLetter := True
    else if ((U[I] >= 'A') and (U[I] <= 'Z')) or ((U[I] >= 'a') and (U[I] <= 'z')) or
      (U[I] >= #$0370) then
      HasLetter := True;
  end;
  Result := HasLetter and (Words <= 7);
end;

{ ---------- TBibleImportSource ---------- }

constructor TBibleImportSource.Create;
begin
  inherited Create;
  FApocryphaNames := TStringList.Create;
  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  FIndex.Duplicates := dupIgnore;
end;

destructor TBibleImportSource.Destroy;
begin
  FIndex.Free;
  FApocryphaNames.Free;
  inherited Destroy;
end;

procedure TBibleImportSource.AddVerse(Book, Chapter, Verse: Integer;
  const Text: string);
begin
  if (Book < 1) or (Chapter < 1) or (Verse < 1) or (Trim(Text) = '') then
    Exit;
  if FVerseCount >= Length(FVerses) then
    SetLength(FVerses, FVerseCount * 2 + 1024);
  FVerses[FVerseCount].Book := Book;
  FVerses[FVerseCount].Chapter := Chapter;
  FVerses[FVerseCount].Verse := Verse;
  FVerses[FVerseCount].Text := Text;
  Inc(FVerseCount);
end;

procedure TBibleImportSource.AddDetectedVerse(BookIndex, Chapter,
  Verse: Integer; const Text: string);
var
  I: Integer;
begin
  if Trim(Text) = '' then
    Exit;
  { joined verses (sub-verse letters, continued paragraphs) }
  for I := FDetectedCount - 1 downto Max(0, FDetectedCount - 3) do
    if (FDetectedVerses[I].Book = BookIndex) and
       (FDetectedVerses[I].Chapter = Chapter) and
       (FDetectedVerses[I].Verse = Verse) then
    begin
      FDetectedVerses[I].Text := FDetectedVerses[I].Text + ' ' + Trim(Text);
      Exit;
    end;
  if FDetectedCount >= Length(FDetectedVerses) then
    SetLength(FDetectedVerses, FDetectedCount * 2 + 1024);
  FDetectedVerses[FDetectedCount].Book := BookIndex;
  FDetectedVerses[FDetectedCount].Chapter := Chapter;
  FDetectedVerses[FDetectedCount].Verse := Verse;
  FDetectedVerses[FDetectedCount].Text := Trim(Text);
  Inc(FDetectedCount);
end;

function TBibleImportSource.LoadTheWordModule(const ModuleFile: string;
  out ErrorMessage: string): Boolean;
var
  Lines: TStringList;
  Scope: TEscopoTexto;
  Ext: string;
  Enumerator: TBibleReferenceEnumerator;
  LineIndex, B, C, V: Integer;
  Payload: TApocryphaPayload;
  Names: TStringList;
  I: Integer;
begin
  Result := False;
  ErrorMessage := '';
  Ext := LowerCase(ExtractFileExt(ModuleFile));
  if Ext = '.ont' then
    Scope := etONT
  else if Ext = '.ot' then
    Scope := etOT
  else if Ext = '.nt' then
    Scope := etNT
  else
  begin
    ErrorMessage := SUnsupportedBibleFile;
    Exit;
  end;

  Lines := TStringList.Create;
  Enumerator := TBibleReferenceEnumerator.Create(Scope);
  try
    Lines.LoadFromFile(ModuleFile);
    if (Lines.Count > 0) and (Copy(Lines[0], 1, 3) = #$EF#$BB#$BF) then
      Lines[0] := Copy(Lines[0], 4, MaxInt);
    LineIndex := 0;
    while Enumerator.MoveNext and (LineIndex < Lines.Count) do
    begin
      SScanf(Enumerator.ReferenceID, '%d,%d,%d', [@B, @C, @V]);
      AddVerse(B, C, V, Lines[LineIndex]);
      Inc(LineIndex);
    end;
  finally
    Enumerator.Free;
    Lines.Free;
  end;

  if TryGetApocryphaPayload(ModuleFile, Payload) then
  begin
    for I := 0 to High(Payload.Verses) do
      AddVerse(Payload.Verses[I].Book, Payload.Verses[I].Chapter,
        Payload.Verses[I].Verse, Payload.Verses[I].ONTText);
    Names := TStringList.Create;
    try
      Names.Delimiter := ',';
      Names.StrictDelimiter := True;
      Names.DelimitedText := StringReplace(Payload.BooksInfo, ';', ',',
        [rfReplaceAll]);
      for I := 0 to Names.Count - 1 do
        if Trim(Names.Names[I]) <> '' then
          FApocryphaNames.Values[Trim(Names.Names[I])] :=
            Trim(Names.ValueFromIndex[I]);
    finally
      Names.Free;
    end;
  end;
  Result := FVerseCount > 0;
  if not Result then
    ErrorMessage := SNoVersesInFile;
end;

procedure TBibleImportSource.ParseParagraphs(Paragraphs: TStrings);
var
  P, NextP, K: Integer;
  Line: string;
  U: UnicodeString;
  BookIndex, Chapter, Verse, ChapterValue, Number, I, Start: Integer;
  BookHasChapters: Boolean;
  Pending: array of TImportVerse;   // text before chapter 1, per book
  Buffer: UnicodeString;
  BufferVerse: Integer;

  function NextNonEmpty(From: Integer): Integer;
  begin
    Result := From;
    while (Result < Paragraphs.Count) and (Trim(Paragraphs[Result]) = '') do
      Inc(Result);
  end;

  procedure FlushBuffer;
  var
    T: string;
  begin
    T := UTF8Trim(UTF8Encode(Buffer));
    while Pos('  ', T) > 0 do
      T := StringReplace(T, '  ', ' ', [rfReplaceAll]);
    Buffer := '';
    if (T = '') or (BookIndex < 0) then
      Exit;
    if Chapter = 0 then
    begin
      SetLength(Pending, Length(Pending) + 1);
      Pending[High(Pending)].Book := BookIndex;
      Pending[High(Pending)].Chapter := 0;
      Pending[High(Pending)].Verse := BufferVerse;
      Pending[High(Pending)].Text := T;
    end
    else
      AddDetectedVerse(BookIndex, Chapter, BufferVerse, T);
  end;

  procedure CloseBook;
  var
    J: Integer;
  begin
    FlushBuffer;
    if BookIndex < 0 then
      Exit;
    for J := 0 to High(Pending) do
      if Pending[J].Book = BookIndex then
      begin
        if not BookHasChapters then
          AddDetectedVerse(BookIndex, 1, Max(1, Pending[J].Verse), Pending[J].Text)
        else
          Inc(FUnplacedVerses);
      end;
    Pending := nil;
  end;

  procedure StartBook(const Title: string);
  begin
    CloseBook;
    SetLength(FDetected, Length(FDetected) + 1);
    BookIndex := High(FDetected);
    FDetected[BookIndex].Name := Trim(Title);
    FDetected[BookIndex].BookID := GuessBibleBookID(Title);
    Chapter := 0;
    Verse := 0;
    BufferVerse := 1;
    BookHasChapters := False;
  end;

  procedure StartVerse(Number: Integer);
  begin
    FlushBuffer;
    BufferVerse := Number;
    Verse := Number;
  end;

  { "Genesis 1:1 text" or "1:1 text" at the start of a line }
  function TryReferenceLine: Boolean;
  var
    Rx: UnicodeString;
    Colon, J, Ch, Vs, BookEnd: Integer;
    BookPart, RestText: string;
    Id: Integer;
  begin
    Result := False;
    Rx := UTF8Decode(Line);
    Colon := Pos(':', Rx);
    if (Colon < 2) or (Colon > 40) then
      Exit;
    J := Colon - 1;
    while (J >= 1) and (Rx[J] >= '0') and (Rx[J] <= '9') do
      Dec(J);
    if J = Colon - 1 then
      Exit;
    Ch := StrToIntDef(UTF8Encode(Copy(Rx, J + 1, Colon - J - 1)), 0);
    BookEnd := J;
    J := Colon + 1;
    while (J <= Length(Rx)) and (Rx[J] >= '0') and (Rx[J] <= '9') do
      Inc(J);
    Vs := StrToIntDef(UTF8Encode(Copy(Rx, Colon + 1, J - Colon - 1)), 0);
    if (Ch <= 0) or (Vs <= 0) then
      Exit;
    { optional verse letter }
    if (J <= Length(Rx)) and (BaseLetter(Rx[J]) >= 'a') and (BaseLetter(Rx[J]) <= 'z') and
       ((J = Length(Rx)) or (Rx[J + 1] = ' ')) then
      Inc(J);
    if (J <= Length(Rx)) and (Rx[J] <> ' ') and (Rx[J] <> #9) then
      Exit;
    RestText := Trim(UTF8Encode(Copy(Rx, J, MaxInt)));
    BookPart := Trim(UTF8Encode(Copy(Rx, 1, BookEnd)));
    if BookPart <> '' then
    begin
      Id := GuessBibleBookID(BookPart);
      if (BookIndex < 0) or (FDetected[BookIndex].Name <> BookPart) then
      begin
        if (Id = 0) and (BookIndex >= 0) then
          Exit;  // "word 3:4" inside a sentence
        StartBook(BookPart);
      end;
    end
    else if BookIndex < 0 then
      Exit;
    if Ch <> Chapter then
    begin
      FlushBuffer;
      Chapter := Ch;
      BookHasChapters := True;
    end;
    StartVerse(Vs);
    Buffer := UTF8Decode(RestText);
    Result := True;
  end;

  procedure ParseText;
  var
    J, D, N, Len, ExplicitChapter, AltChapter, AltVerse: Integer;
    Seg, SubLabel: UnicodeString;
    Letter: WideChar;

    function RaisedDigit(C: WideChar): Integer;
    begin
      Result := SuperDigit(C);
      if Result < 0 then
        Result := SubDigit(C);
    end;

    function ReadRaised(var At: Integer): Integer;
    begin
      Result := 0;
      while (At <= Len) and (RaisedDigit(U[At]) >= 0) do
      begin
        Result := Result * 10 + RaisedDigit(U[At]);
        Inc(At);
      end;
    end;

  begin
    J := 1;
    Len := Length(U);
    { plain number at the start: "12 text", "12. text" }
    if (Len > 0) and (U[1] >= '0') and (U[1] <= '9') then
    begin
      N := 0;
      while (J <= Len) and (U[J] >= '0') and (U[J] <= '9') do
      begin
        N := N * 10 + Ord(U[J]) - Ord('0');
        Inc(J);
      end;
      if (J <= Len) and ((U[J] = '.') or (U[J] = ')')) then
        Inc(J);
      if (J <= Len) and (U[J] = ' ') and (N >= 1) and
         ((N = Verse + 1) or (N = 1)) then
      begin
        if N = 1 then
          if Chapter = 0 then
          begin
            Chapter := 1;
          end;
        StartVerse(N);
        U := Copy(U, J + 1, MaxInt);
      end;
      J := 1;
      Len := Length(U);
    end;

    Seg := '';
    while J <= Len do
    begin
      D := SuperDigit(U[J]);
      if D < 0 then
        D := SubDigit(U[J]);
      if D >= 0 then
      begin
        N := 0;
        while (J <= Len) and ((SuperDigit(U[J]) >= 0) or (SubDigit(U[J]) >= 0)) do
        begin
          D := SuperDigit(U[J]);
          if D < 0 then
            D := SubDigit(U[J]);
          N := N * 10 + D;
          Inc(J);
        end;
        { sub-verse: "¹β " repeats the current verse number + a letter }
        if (N = Verse) and (J + 1 <= Len) and (U[J + 1] = ' ') and
           IsSmallLetter(U[J]) then
        begin
          Letter := U[J];
          Buffer := Buffer + Seg + ' (' + UnicodeString(IntToStr(N)) + Letter + ') ';
          Seg := '';
          Inc(J, 2);
          Continue;
        end;
        Buffer := Buffer + Seg;
        Seg := '';
        { "36:8" = chapter:verse; "23(29)" / "36:8(39:1)" = this edition's
          number followed by the common (KJV) number, which is the one used }
        ExplicitChapter := 0;
        SubLabel := '';
        if (J + 1 <= Len) and (U[J] = ':') and (RaisedDigit(U[J + 1]) >= 0) then
        begin
          Inc(J);
          ExplicitChapter := N;
          N := ReadRaised(J);
        end;
        if (J + 1 <= Len) and (U[J] = '(') and (RaisedDigit(U[J + 1]) >= 0) then
        begin
          K := J + 1;
          AltVerse := ReadRaised(K);
          AltChapter := 0;
          if (K + 1 <= Len) and (U[K] = ':') and (RaisedDigit(U[K + 1]) >= 0) then
          begin
            Inc(K);
            AltChapter := AltVerse;
            AltVerse := ReadRaised(K);
          end;
          SubLabel := '';
          if (K + 1 <= Len) and IsSmallLetter(U[K]) and (U[K + 1] = ')') then
          begin
            { "(38:20a)": part of a verse }
            SubLabel := ' (' + UnicodeString(IntToStr(AltVerse)) + U[K] + ') ';
            Inc(K);
          end;
          if (K <= Len) and (U[K] = ')') then
          begin
            J := K + 1;
            N := AltVerse;
            if AltChapter > 0 then
              ExplicitChapter := AltChapter;
          end
          else
            SubLabel := '';
        end;
        if N = 0 then
          Continue;
        if ExplicitChapter > 0 then
        begin
          if ExplicitChapter <> Chapter then
          begin
            FlushBuffer;
            Chapter := ExplicitChapter;
            Verse := 0;
          end;
          BookHasChapters := True;
        end
        else if Chapter = 0 then
          Chapter := 1
        else if (N = 1) and (Verse > 1) then
        begin
          { verse 1 again without a chapter heading: the next chapter }
          FlushBuffer;
          Inc(Chapter);
          BookHasChapters := True;
          Verse := 0;
        end;
        StartVerse(N);
        Buffer := SubLabel;
        while (J <= Len) and ((U[J] = ' ') or (U[J] = #$00A0)) do
          Inc(J);
        Continue;
      end;
      Seg := Seg + U[J];
      Inc(J);
    end;
    Buffer := Buffer + Seg + ' ';
  end;

begin
  BookIndex := -1;
  Chapter := 0;
  Verse := 0;
  BufferVerse := 1;
  Buffer := '';
  BookHasChapters := False;
  Pending := nil;

  P := 0;
  while P < Paragraphs.Count do
  begin
    Line := Trim(Paragraphs[P]);
    Inc(P);
    if Line = '' then
      Continue;

    ChapterValue := ChapterMarker(Line);
    if (ChapterValue <> 0) and (BookIndex >= 0) then
    begin
      FlushBuffer;
      if (ChapterValue > 0) and (ChapterValue <> Chapter) then
      begin
        Chapter := ChapterValue;
        BookHasChapters := True;
        Verse := 0;
        BufferVerse := 1;
      end
      else if ChapterValue > 0 then
        BookHasChapters := True;
      Continue;
    end;

    if LooksLikeTitle(Line) then
    begin
      NextP := NextNonEmpty(P);
      if (NextP < Paragraphs.Count) and
         ((ChapterMarker(Paragraphs[NextP]) > 0) or
          ((GuessBibleBookID(Line) > 0) and
           ((BookIndex < 0) or (GuessBibleBookID(Line) <> FDetected[BookIndex].BookID)))) then
      begin
        StartBook(Line);
        Continue;
      end;
    end;

    if LooksLikeTitle(Line) then
    begin
      NextP := NextNonEmpty(P);
      if (NextP < Paragraphs.Count) and LooksLikeTitle(Paragraphs[NextP]) and
         (ChapterMarker(Paragraphs[NextP]) = 0) then
        Continue;  // a section heading ("Apocrypha") before a book title
    end;

    if BookIndex < 0 then
    begin
      if not TryReferenceLine then
        Continue;  // text before the first book
      Continue;
    end;

    if TryReferenceLine then
      Continue;

    U := UTF8Decode(Line);
    { a new paragraph without a number continues the current verse, or is
      verse 1 when the chapter has none yet }
    if (Verse = 0) and (Chapter > 0) then
    begin
      BufferVerse := 1;
      Verse := 1;
    end;
    ParseText;
  end;
  CloseBook;
end;

function TBibleImportSource.LoadPlainDocument(const FileName: string;
  out ErrorMessage: string): Boolean;
var
  Paragraphs: TStringList;
  I, LastChapter: Integer;
begin
  Result := False;
  Paragraphs := TStringList.Create;
  try
    if not ReadBibleDocument(FileName, Paragraphs, ErrorMessage) then
      Exit;
    ParseParagraphs(Paragraphs);
  finally
    Paragraphs.Free;
  end;

  for I := 0 to High(FDetected) do
  begin
    FDetected[I].Chapters := 0;
    FDetected[I].Verses := 0;
  end;
  for I := 0 to FDetectedCount - 1 do
    with FDetected[FDetectedVerses[I].Book] do
    begin
      Inc(Verses);
      if FDetectedVerses[I].Chapter > Chapters then
        Chapters := FDetectedVerses[I].Chapter;
    end;
  { drop "books" that got no verses (stray headings) }
  LastChapter := 0;
  for I := 0 to High(FDetected) do
    if FDetected[I].Verses > 0 then
      Inc(LastChapter);
  Result := LastChapter > 0;
  if not Result then
    ErrorMessage := SNoVersesInFile;
end;

function TBibleImportSource.LoadFromFile(const FileName: string;
  out ErrorMessage: string): Boolean;
var
  ModuleFile, Ext: string;
begin
  FFileName := FileName;
  FVerses := nil;
  FVerseCount := 0;
  FDetected := nil;
  FDetectedVerses := nil;
  FDetectedCount := 0;
  FUnplacedVerses := 0;
  FDuplicateVerses := 0;
  FApocryphaNames.Clear;
  FIndex.Clear;
  ErrorMessage := '';
  Ext := LowerCase(ExtractFileExt(FileName));

  if IsPlainBibleDocument(FileName) then
  begin
    FIsPlainDocument := True;
    Result := LoadPlainDocument(FileName, ErrorMessage);
    Exit;
  end;

  FIsPlainDocument := False;
  if (Ext = '.ont') or (Ext = '.ot') or (Ext = '.nt') then
    ModuleFile := FileName
  else if IsConvertibleBibleFile(FileName) then
  begin
    if not PrepareBibleModuleFile(FileName, ModuleFile, ErrorMessage) then
      Exit(False);
  end
  else
  begin
    ErrorMessage := SUnsupportedBibleFile;
    Exit(False);
  end;
  Result := LoadTheWordModule(ModuleFile, ErrorMessage);
  if Result then
    BuildIndex;
end;

procedure TBibleImportSource.SetDetectedBookID(Index, BookID: Integer);
begin
  if (Index >= 0) and (Index <= High(FDetected)) then
    FDetected[Index].BookID := BookID;
end;

procedure TBibleImportSource.ApplyBookMapping;
var
  I, Found: Integer;
  Id: Integer;
  Ref: string;
  Seen: TStringList;
begin
  FVerses := nil;
  FVerseCount := 0;
  FIndex.Clear;
  FApocryphaNames.Clear;
  Seen := TStringList.Create;
  try
    Seen.Sorted := True;
    for I := 0 to FDetectedCount - 1 do
    begin
      Id := FDetected[FDetectedVerses[I].Book].BookID;
      if (Id < 1) or (Id > LAST_IMPORT_BOOK) then
        Continue;
      if (FDetectedVerses[I].Chapter < 1) or (FDetectedVerses[I].Verse < 1) then
        Continue;
      Ref := Format('%d,%d,%d', [Id, FDetectedVerses[I].Chapter,
        FDetectedVerses[I].Verse]);
      Found := Seen.IndexOf(Ref);
      if Found >= 0 then
      begin
        { the same verse number twice: keep one verse, text joined }
        with FVerses[PtrInt(Seen.Objects[Found])] do
          Text := Text + ' ' + FDetectedVerses[I].Text;
        Inc(FDuplicateVerses);
        Continue;
      end;
      Seen.AddObject(Ref, TObject(PtrInt(FVerseCount)));
      AddVerse(Id, FDetectedVerses[I].Chapter, FDetectedVerses[I].Verse,
        FDetectedVerses[I].Text);
    end;
  finally
    Seen.Free;
  end;
  BuildIndex;
end;

procedure TBibleImportSource.BuildIndex;
var
  I: Integer;
  Ref: string;
begin
  FIndex.Clear;
  FIndex.Sorted := False;
  for I := 0 to FVerseCount - 1 do
  begin
    Ref := ReferenceID(I);
    FIndex.AddObject(Ref, TObject(PtrInt(I)));
  end;
  FIndex.Sorted := True;
end;

function TBibleImportSource.GetDetected(Index: Integer): TDetectedBook;
begin
  Result := FDetected[Index];
end;

function TBibleImportSource.GetDetectedCount: Integer;
begin
  Result := Length(FDetected);
end;

function TBibleImportSource.GetVerse(Index: Integer): TImportVerse;
begin
  Result := FVerses[Index];
end;

function TBibleImportSource.BookIDs: TStringList;
var
  I: Integer;
  Present: array[1..LAST_IMPORT_BOOK] of Boolean;
begin
  FillChar(Present, SizeOf(Present), 0);
  for I := 0 to FVerseCount - 1 do
    if (FVerses[I].Book >= 1) and (FVerses[I].Book <= LAST_IMPORT_BOOK) then
      Present[FVerses[I].Book] := True;
  Result := TStringList.Create;
  for I := 1 to LAST_IMPORT_BOOK do
    if Present[I] then
      Result.Add(IntToStr(I));
end;

function TBibleImportSource.ApocryphaOrder: string;
var
  I: Integer;
  Seen: array[1..LAST_IMPORT_BOOK] of Boolean;
begin
  Result := '';
  FillChar(Seen, SizeOf(Seen), 0);
  for I := 0 to FVerseCount - 1 do
    if (FVerses[I].Book >= FIRST_APOCRYPHA_BOOK) and
       (FVerses[I].Book <= LAST_IMPORT_BOOK) and not Seen[FVerses[I].Book] then
    begin
      Seen[FVerses[I].Book] := True;
      if Result <> '' then
        Result := Result + ',';
      Result := Result + IntToStr(FVerses[I].Book);
    end;
end;

procedure TBibleImportSource.BeginCombined(const AName: string);
begin
  FFileName := AName;
  FVerses := nil;
  FVerseCount := 0;
  FDetected := nil;
  FDetectedVerses := nil;
  FDetectedCount := 0;
  FIsPlainDocument := False;
  FUnplacedVerses := 0;
  FDuplicateVerses := 0;
  FApocryphaNames.Clear;
  FIndex.Clear;
end;

procedure TBibleImportSource.AppendBook(Other: TBibleImportSource;
  BookID: Integer);
var
  I: Integer;
  Name: string;
begin
  for I := 0 to Other.VerseCount - 1 do
    if Other.Verses[I].Book = BookID then
      AddVerse(BookID, Other.Verses[I].Chapter, Other.Verses[I].Verse,
        Other.Verses[I].Text);
  Name := Trim(Other.ApocryphaNames.Values[IntToStr(BookID)]);
  if (Name <> '') and (FApocryphaNames.Values[IntToStr(BookID)] = '') then
    FApocryphaNames.Values[IntToStr(BookID)] := Name;
end;

procedure TBibleImportSource.FinishCombined;
begin
  BuildIndex;
end;

function TBibleImportSource.BookVerseCount(BookID: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FVerseCount - 1 do
    if FVerses[I].Book = BookID then
      Inc(Result);
end;

function TBibleImportSource.BookName(BookID: Integer): string;
begin
  Result := Trim(FApocryphaNames.Values[IntToStr(BookID)]);
  if Result = '' then
    Result := AnyBibleBookName(BookID);
end;

function TBibleImportSource.TryGetText(const ReferenceID: string;
  out Text: string): Boolean;
var
  Index: Integer;
begin
  Text := '';
  Index := FIndex.IndexOf(ReferenceID);
  Result := Index >= 0;
  if Result then
    Text := FVerses[PtrInt(FIndex.Objects[Index])].Text;
end;

function TBibleImportSource.ReferenceID(Index: Integer): string;
begin
  Result := Format('%d,%d,%d', [FVerses[Index].Book, FVerses[Index].Chapter,
    FVerses[Index].Verse]);
end;

end.
