unit bible_book_names;

{$mode objfpc}{$H+}

interface

uses
  BibleScope;

function LocalizedBookName(Scope: TEscopoTexto; BookIndex: Integer): string;
function LocalizedBookNameByID(BookID: Integer): string;

implementation

resourcestring
  SGenesis = 'Genesis';
  SExodus = 'Exodus';
  SLeviticus = 'Leviticus';
  SNumbers = 'Numbers';
  SDeuteronomy = 'Deuteronomy';
  SJoshua = 'Joshua';
  SJudges = 'Judges';
  SRuth = 'Ruth';
  S1Samuel = '1 Samuel';
  S2Samuel = '2 Samuel';
  S1Kings = '1 Kings';
  S2Kings = '2 Kings';
  S1Chronicles = '1 Chronicles';
  S2Chronicles = '2 Chronicles';
  SEzra = 'Ezra';
  SNehemiah = 'Nehemiah';
  SEsther = 'Esther';
  SJob = 'Job';
  SPsalms = 'Psalms';
  SProverbs = 'Proverbs';
  SEcclesiastes = 'Ecclesiastes';
  SSongOfSolomon = 'Song of Solomon';
  SIsaiah = 'Isaiah';
  SJeremiah = 'Jeremiah';
  SLamentations = 'Lamentations';
  SEzekiel = 'Ezekiel';
  SDaniel = 'Daniel';
  SHosea = 'Hosea';
  SJoel = 'Joel';
  SAmos = 'Amos';
  SObadiah = 'Obadiah';
  SJonah = 'Jonah';
  SMicah = 'Micah';
  SNahum = 'Nahum';
  SHabakkuk = 'Habakkuk';
  SZephaniah = 'Zephaniah';
  SHaggai = 'Haggai';
  SZechariah = 'Zechariah';
  SMalachi = 'Malachi';
  SMatthew = 'Matthew';
  SMark = 'Mark';
  SLuke = 'Luke';
  SJohn = 'John';
  SActs = 'Acts';
  SRomans = 'Romans';
  S1Corinthians = '1 Corinthians';
  S2Corinthians = '2 Corinthians';
  SGalatians = 'Galatians';
  SEphesians = 'Ephesians';
  SPhilippians = 'Philippians';
  SColossians = 'Colossians';
  S1Thessalonians = '1 Thessalonians';
  S2Thessalonians = '2 Thessalonians';
  S1Timothy = '1 Timothy';
  S2Timothy = '2 Timothy';
  STitus = 'Titus';
  SPhilemon = 'Philemon';
  SHebrews = 'Hebrews';
  SJames = 'James';
  S1Peter = '1 Peter';
  S2Peter = '2 Peter';
  S1John = '1 John';
  S2John = '2 John';
  S3John = '3 John';
  SJude = 'Jude';
  SRevelation = 'Revelation';

function LocalizedBookNameByID(BookID: Integer): string;
begin
  case BookID of
    1: Result := SGenesis;
    2: Result := SExodus;
    3: Result := SLeviticus;
    4: Result := SNumbers;
    5: Result := SDeuteronomy;
    6: Result := SJoshua;
    7: Result := SJudges;
    8: Result := SRuth;
    9: Result := S1Samuel;
    10: Result := S2Samuel;
    11: Result := S1Kings;
    12: Result := S2Kings;
    13: Result := S1Chronicles;
    14: Result := S2Chronicles;
    15: Result := SEzra;
    16: Result := SNehemiah;
    17: Result := SEsther;
    18: Result := SJob;
    19: Result := SPsalms;
    20: Result := SProverbs;
    21: Result := SEcclesiastes;
    22: Result := SSongOfSolomon;
    23: Result := SIsaiah;
    24: Result := SJeremiah;
    25: Result := SLamentations;
    26: Result := SEzekiel;
    27: Result := SDaniel;
    28: Result := SHosea;
    29: Result := SJoel;
    30: Result := SAmos;
    31: Result := SObadiah;
    32: Result := SJonah;
    33: Result := SMicah;
    34: Result := SNahum;
    35: Result := SHabakkuk;
    36: Result := SZephaniah;
    37: Result := SHaggai;
    38: Result := SZechariah;
    39: Result := SMalachi;
    40: Result := SMatthew;
    41: Result := SMark;
    42: Result := SLuke;
    43: Result := SJohn;
    44: Result := SActs;
    45: Result := SRomans;
    46: Result := S1Corinthians;
    47: Result := S2Corinthians;
    48: Result := SGalatians;
    49: Result := SEphesians;
    50: Result := SPhilippians;
    51: Result := SColossians;
    52: Result := S1Thessalonians;
    53: Result := S2Thessalonians;
    54: Result := S1Timothy;
    55: Result := S2Timothy;
    56: Result := STitus;
    57: Result := SPhilemon;
    58: Result := SHebrews;
    59: Result := SJames;
    60: Result := S1Peter;
    61: Result := S2Peter;
    62: Result := S1John;
    63: Result := S2John;
    64: Result := S3John;
    65: Result := SJude;
    66: Result := SRevelation;
  else
    Result := '';
  end;
end;

function LocalizedBookName(Scope: TEscopoTexto; BookIndex: Integer): string;
var
  BookID: Integer;
begin
  case Scope of
    etNT: BookID := BookIndex + 40;
    etOT, etONT: BookID := BookIndex + 1;
  else
    BookID := BookIndex + 1;
  end;
  Result := LocalizedBookNameByID(BookID);
end;

end.
