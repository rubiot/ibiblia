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
    67: Result := 'Tobit';
    68: Result := 'Judith';
    69: Result := 'Additions to Esther';
    70: Result := 'Wisdom';
    71: Result := 'Sirach';
    72: Result := 'Baruch';
    73: Result := 'Letter of Jeremiah';
    74: Result := 'Prayer of Azariah';
    75: Result := 'Susanna';
    76: Result := 'Bel and the Dragon';
    77: Result := '1 Maccabees';
    78: Result := '2 Maccabees';
    79: Result := '1 Esdras';
    80: Result := 'Prayer of Manasseh';
    81: Result := 'Psalm 151';
    82: Result := '3 Maccabees';
    83: Result := '2 Esdras';
    84: Result := '4 Maccabees';
    85: Result := 'Odes';
    86: Result := 'Psalms of Solomon';
    87: Result := 'Laodiceans';
  else
    Result := 'Book ' + IntToStr(BookID);
  end;
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
