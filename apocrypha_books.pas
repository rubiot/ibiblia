unit apocrypha_books;

{ Apocrypha / deuterocanonical books (book numbers 67 and up) of the open
  project. 1.1.0's book tables cover the 66 canonical books; these books
  live after them: their verses are extra rows at the end of the project
  (pare_id "b,c,v" with b >= 67), their names are kept in the project info
  key 'apocrypha.books' ("67=Tobit;68=Judith", as in the 1.0.1 mod). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  FIRST_APOCRYPHA_BOOK = 67;

type
  TApocryphaBook = record
    BookID: Integer;
    Name: string;
    VerseCounts: array of Integer; // index 0 = chapter 1
  end;

procedure ClearApocryphaBooks;
{ The order of the books follows the imported source text, not the book
  numbers: 'apocrypha.order' is a list of book numbers ("69,67,70"). Books
  not in it come after, by number. }
procedure SetApocryphaOrder(const Order: string);
{ Merges two order lists: Existing first, then the books of Incoming that are
  not in it yet (in Incoming's order). }
function MergeApocryphaOrder(const Existing, Incoming: string): string;
{ The position of a book in the order (smaller = earlier; same rule as the
  tree and the project rows). }
function ApocryphaRank(BookID: Integer): Integer;
{ Registers verse b,c,v (b >= 67) of the open project. }
procedure AddApocryphaVerse(BookID, Chapter, Verse: Integer);
{ Sets names from an 'apocrypha.books' value. }
procedure SetApocryphaBookNames(const Info: string);
function ApocryphaBookCount: Integer;
function ApocryphaBookAt(Index: Integer): TApocryphaBook;
function IsApocryphaBook(BookID: Integer): Boolean;
function ApocryphaBookName(BookID: Integer): string;
function DefaultApocryphaBookName(BookID: Integer): string;
function CanonicalApocryphaIDByName(const AName: string): Integer;
function FindProjectApocryphaBookIDByName(const AName: string): Integer;
function ApocryphaChapterCount(BookID: Integer): Integer;
function ApocryphaVerseCount(BookID, Chapter: Integer): Integer;
{ Merges two 'apocrypha.books' values (the second wins). }
function MergeApocryphaInfo(const Existing, Incoming: string): string;

implementation

var
  Books: array of TApocryphaBook;
  OrderList: array of Integer;

function ApocryphaRank(BookID: Integer): Integer;
var
  Index: Integer;
begin
  for Index := 0 to High(OrderList) do
    if OrderList[Index] = BookID then
      Exit(Index);
  Result := 1000 + BookID;
end;

procedure SortBooks;
var
  Index, J: Integer;
  Swap: TApocryphaBook;
begin
  for Index := 1 to High(Books) do
  begin
    Swap := Books[Index];
    J := Index - 1;
    while (J >= 0) and (ApocryphaRank(Books[J].BookID) > ApocryphaRank(Swap.BookID)) do
    begin
      Books[J + 1] := Books[J];
      Dec(J);
    end;
    Books[J + 1] := Swap;
  end;
end;

procedure SetApocryphaOrder(const Order: string);
var
  Items: TStringList;
  Index, BookID: Integer;
begin
  OrderList := nil;
  Items := TStringList.Create;
  try
    Items.Delimiter := ',';
    Items.StrictDelimiter := True;
    Items.DelimitedText := Order;
    for Index := 0 to Items.Count - 1 do
    begin
      BookID := StrToIntDef(Trim(Items[Index]), 0);
      if (BookID >= FIRST_APOCRYPHA_BOOK) and (ApocryphaRank(BookID) >= 1000) then
      begin
        SetLength(OrderList, Length(OrderList) + 1);
        OrderList[High(OrderList)] := BookID;
      end;
    end;
  finally
    Items.Free;
  end;
  SortBooks;
end;

function MergeApocryphaOrder(const Existing, Incoming: string): string;
var
  Items, Merged: TStringList;
  Index: Integer;
begin
  Merged := TStringList.Create;
  Items := TStringList.Create;
  try
    Merged.Delimiter := ',';
    Merged.StrictDelimiter := True;
    Merged.DelimitedText := Existing;
    Items.Delimiter := ',';
    Items.StrictDelimiter := True;
    Items.DelimitedText := Incoming;
    for Index := Merged.Count - 1 downto 0 do
      if StrToIntDef(Trim(Merged[Index]), 0) < FIRST_APOCRYPHA_BOOK then
        Merged.Delete(Index);
    for Index := 0 to Items.Count - 1 do
      if (StrToIntDef(Trim(Items[Index]), 0) >= FIRST_APOCRYPHA_BOOK) and
         (Merged.IndexOf(Trim(Items[Index])) < 0) then
        Merged.Add(Trim(Items[Index]));
    Result := Merged.DelimitedText;
  finally
    Items.Free;
    Merged.Free;
  end;
end;

function DefaultName(BookID: Integer): string;
begin
  case BookID of
    67: Result := '1 Esdras';
    68: Result := '2 Esdras';
    69: Result := 'Tobit';
    70: Result := 'Judith';
    71: Result := '1 Maccabees';
    72: Result := '2 Maccabees';
    73: Result := '3 Maccabees';
    74: Result := '4 Maccabees';
    75: Result := 'Prayer of Manasseh';
    76: Result := 'Wisdom';
    77: Result := 'Sirach';
    78: Result := 'Baruch';
    79: Result := 'Letter of Jeremiah';
    80: Result := 'Prayer of Azariah';
    81: Result := 'Susanna';
    82: Result := 'Bel and the Dragon';
    83: Result := 'Additions to Esther';
    84: Result := 'Psalm 151';
    85: Result := 'Psalms of Solomon';
    86: Result := 'Odes';
    87: Result := 'Laodiceans';
  else
    Result := 'Book ' + IntToStr(BookID);
  end;
end;

function DefaultApocryphaBookName(BookID: Integer): string;
begin
  Result := DefaultName(BookID);
end;

function CanonicalApocryphaIDByName(const AName: string): Integer;
var
  S: string;
begin
  S := LowerCase(Trim(AName));
  if S = '' then
    Exit(0);
  if (Pos('psalms of solomon', S) > 0) or (Pos('psalm of solomon', S) > 0) or
     (Pos('psalmsofsolomon', S) > 0) or (S = 'pss') or (S = 'ps3') then
    Result := 85
  else if (Pos('psalm 151', S) > 0) or (Pos('psalm151', S) > 0) or
          (S = 'ps2') or (S = '151') then
    Result := 84
  else if (Pos('sirach', S) > 0) or (Pos('ecclesiasticus', S) > 0) or
          (Pos('ben sira', S) > 0) or (S = 'sir') or (S = 'ecn') then
    Result := 77
  else if (Pos('wisdom', S) > 0) or (S = 'wis') then
    Result := 76
  else if (Pos('2 esdras', S) > 0) or (Pos('2esdras', S) > 0) or
          (Pos('ii esdras', S) > 0) or (Pos('4 esdras', S) > 0) or
          (Pos('4 ezra', S) > 0) or (S = '2es') or (S = '4ez') or
          (S = '5ez') or (S = '6ez') then
    Result := 68
  else if (Pos('1 esdras', S) > 0) or (Pos('1esdras', S) > 0) or
          (Pos('i esdras', S) > 0) or (Pos('3 ezra', S) > 0) or
          (S = '1es') or (S = '3ez') or (S = 'esd') then
    Result := 67
  else if (Pos('tobit', S) > 0) or (Pos('tobias', S) > 0) or (S = 'tob') then
    Result := 69
  else if (Pos('judith', S) > 0) or (S = 'jdt') then
    Result := 70
  else if (Pos('4 macc', S) > 0) or (Pos('4macc', S) > 0) or
          (Pos('iv macc', S) > 0) or (S = '4ma') then
    Result := 74
  else if (Pos('3 macc', S) > 0) or (Pos('3macc', S) > 0) or
          (Pos('iii macc', S) > 0) or (S = '3ma') then
    Result := 73
  else if (Pos('2 macc', S) > 0) or (Pos('2macc', S) > 0) or
          (Pos('ii macc', S) > 0) or (S = '2ma') then
    Result := 72
  else if (Pos('1 macc', S) > 0) or (Pos('1macc', S) > 0) or
          (Pos('i macc', S) > 0) or (S = '1ma') then
    Result := 71
  else if (Pos('manasseh', S) > 0) or (Pos('manasses', S) > 0) or (S = 'man') then
    Result := 75
  else if (Pos('letter of jeremiah', S) > 0) or (Pos('epistle of jeremiah', S) > 0) or
          (Pos('epistle of jeremy', S) > 0) or (S = 'lje') or (S = 'epj') then
    Result := 79
  else if (Pos('baruch', S) > 0) or (S = 'bar') then
    Result := 78
  else if (Pos('azariah', S) > 0) or (Pos('azarias', S) > 0) or
          (Pos('three young', S) > 0) or (Pos('three children', S) > 0) or
          (Pos('song of the three', S) > 0) or (S = 's3y') or (S = 'aza') then
    Result := 80
  else if (Pos('susanna', S) > 0) or (S = 'sus') then
    Result := 81
  else if (Pos('bel and', S) > 0) or (Pos('bel ', S) > 0) or
          (S = 'bel') or (Pos('dragon', S) > 0) then
    Result := 82
  else if (Pos('esther', S) > 0) or (S = 'esg') or (S = 'ade') then
    Result := 83
  else if (Pos('odes', S) > 0) or (S = 'ode') or (S = 'oda') then
    Result := 86
  else if (Pos('laodicean', S) > 0) or (S = 'lao') then
    Result := 87
  else
    Result := 0;
end;

function FindProjectApocryphaBookIDByName(const AName: string): Integer;
var
  I, TargetCanon: Integer;
  Trimmed: string;
begin
  Result := 0;
  Trimmed := Trim(AName);
  if Trimmed = '' then
    Exit;
  for I := 0 to High(Books) do
    if SameText(Trim(Books[I].Name), Trimmed) then
      Exit(Books[I].BookID);
  TargetCanon := CanonicalApocryphaIDByName(Trimmed);
  if TargetCanon > 0 then
    for I := 0 to High(Books) do
      if CanonicalApocryphaIDByName(Books[I].Name) = TargetCanon then
        Exit(Books[I].BookID);
end;

function IndexOfBook(BookID: Integer): Integer;
var
  Index: Integer;
begin
  for Index := 0 to High(Books) do
    if Books[Index].BookID = BookID then
      Exit(Index);
  Result := -1;
end;

procedure ClearApocryphaBooks;
begin
  Books := nil;
  OrderList := nil;
end;

procedure AddApocryphaVerse(BookID, Chapter, Verse: Integer);
var
  Index, Insert: Integer;
begin
  if (BookID < FIRST_APOCRYPHA_BOOK) or (Chapter < 1) or (Verse < 1) then
    Exit;
  Index := IndexOfBook(BookID);
  if Index < 0 then
  begin
    // keep the list in the order of the imported source (else by number)
    Insert := Length(Books);
    while (Insert > 0) and (ApocryphaRank(Books[Insert - 1].BookID) > ApocryphaRank(BookID)) do
      Dec(Insert);
    SetLength(Books, Length(Books) + 1);
    for Index := High(Books) downto Insert + 1 do
      Books[Index] := Books[Index - 1];
    Books[Insert].BookID := BookID;
    Books[Insert].Name := DefaultName(BookID);
    Books[Insert].VerseCounts := nil;
    Index := Insert;
  end;
  if Length(Books[Index].VerseCounts) < Chapter then
    SetLength(Books[Index].VerseCounts, Chapter);
  if Books[Index].VerseCounts[Chapter - 1] < Verse then
    Books[Index].VerseCounts[Chapter - 1] := Verse;
end;

procedure SetApocryphaBookNames(const Info: string);
var
  Items: TStringList;
  Index, BookIndex, BookID: Integer;
begin
  Items := TStringList.Create;
  try
    Items.Delimiter := ';';
    Items.StrictDelimiter := True;
    Items.DelimitedText := Info;
    for Index := 0 to Items.Count - 1 do
    begin
      BookID := StrToIntDef(Trim(Items.Names[Index]), 0);
      BookIndex := IndexOfBook(BookID);
      if (BookIndex >= 0) and (Trim(Items.ValueFromIndex[Index]) <> '') then
        Books[BookIndex].Name := Trim(Items.ValueFromIndex[Index]);
    end;
  finally
    Items.Free;
  end;
end;

function ApocryphaBookCount: Integer;
begin
  Result := Length(Books);
end;

function ApocryphaBookAt(Index: Integer): TApocryphaBook;
begin
  Result := Books[Index];
end;

function IsApocryphaBook(BookID: Integer): Boolean;
begin
  Result := IndexOfBook(BookID) >= 0;
end;

function ApocryphaBookName(BookID: Integer): string;
var
  Index: Integer;
begin
  Index := IndexOfBook(BookID);
  if Index >= 0 then
    Result := Books[Index].Name
  else
    Result := DefaultName(BookID);
end;

function ApocryphaChapterCount(BookID: Integer): Integer;
var
  Index: Integer;
begin
  Index := IndexOfBook(BookID);
  if Index >= 0 then
    Result := Length(Books[Index].VerseCounts)
  else
    Result := 0;
end;

function ApocryphaVerseCount(BookID, Chapter: Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  Index := IndexOfBook(BookID);
  if (Index >= 0) and (Chapter >= 1) and (Chapter <= Length(Books[Index].VerseCounts)) then
    Result := Books[Index].VerseCounts[Chapter - 1];
end;

function MergeApocryphaInfo(const Existing, Incoming: string): string;
var
  Merged, Items: TStringList;
  Index: Integer;
begin
  Merged := TStringList.Create;
  Items := TStringList.Create;
  try
    Merged.Delimiter := ';';
    Merged.StrictDelimiter := True;
    Merged.DelimitedText := Existing;
    Items.Delimiter := ';';
    Items.StrictDelimiter := True;
    Items.DelimitedText := Incoming;
    for Index := 0 to Items.Count - 1 do
      if (Trim(Items.Names[Index]) <> '') and (Trim(Items.ValueFromIndex[Index]) <> '') then
        Merged.Values[Trim(Items.Names[Index])] := Trim(Items.ValueFromIndex[Index]);
    for Index := Merged.Count - 1 downto 0 do
      if Trim(Merged[Index]) = '' then
        Merged.Delete(Index);
    Result := Merged.DelimitedText;
  finally
    Items.Free;
    Merged.Free;
  end;
end;

end.
