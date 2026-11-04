unit bible_versification;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BibleScope;

type
  TBibleReferenceEnumerator = class
  private
    FScope: TEscopoTexto;
    FBookIndex: Integer;
    FChapterNumber: Integer;
    FVerseNumber: Integer;
    FStarted: Boolean;
    FFinished: Boolean;
    function GetBookID: Integer;
    function GetReferenceID: string;
  public
    constructor Create(AScope: TEscopoTexto);
    function MoveNext: Boolean;
    property BookID: Integer read GetBookID;
    property ChapterNumber: Integer read FChapterNumber;
    property VerseNumber: Integer read FVerseNumber;
    property ReferenceID: string read GetReferenceID;
  end;

function DeclaredBibleVerseCount(Scope: TEscopoTexto): Integer;
function CalculateBibleVerseCount(Scope: TEscopoTexto): Integer;
function TryBibleReferenceAt(Scope: TEscopoTexto; ZeroBasedIndex: Integer;
  out ReferenceID: string): Boolean;

implementation

{$INCLUDE include/bookdefs.inc}

procedure RequireValidScope(Scope: TEscopoTexto);
begin
  if Scope = etNone then
    raise EArgumentException.Create('A Bible scope is required.');
end;

constructor TBibleReferenceEnumerator.Create(AScope: TEscopoTexto);
begin
  inherited Create;
  RequireValidScope(AScope);
  FScope := AScope;
  FBookIndex := 0;
  FChapterNumber := 1;
  FVerseNumber := 1;
  FStarted := False;
  FFinished := False;
end;

function TBibleReferenceEnumerator.GetBookID: Integer;
begin
  if not FStarted or FFinished then
    Exit(0);
  Result := FBookIndex + OffsetLivros[FScope] + 1;
end;

function TBibleReferenceEnumerator.GetReferenceID: string;
begin
  if not FStarted or FFinished then
    Exit('');
  Result := Format('%d,%d,%d', [BookID, FChapterNumber, FVerseNumber]);
end;

function TBibleReferenceEnumerator.MoveNext: Boolean;
var
  VerseCount: Integer;
begin
  if FFinished then
    Exit(False);

  if not FStarted then
    FStarted := True
  else
  begin
    VerseCount := QVersiculos[FScope][
      QCapLivros[FScope][FBookIndex] + FChapterNumber - 2];
    Inc(FVerseNumber);
    if FVerseNumber > VerseCount then
    begin
      FVerseNumber := 1;
      Inc(FChapterNumber);
      if FChapterNumber > QCapitulos[FScope][FBookIndex] then
      begin
        FChapterNumber := 1;
        Inc(FBookIndex);
      end;
    end;
  end;

  if FBookIndex >= QLivros[FScope] then
  begin
    FFinished := True;
    Exit(False);
  end;
  Result := True;
end;

function DeclaredBibleVerseCount(Scope: TEscopoTexto): Integer;
begin
  RequireValidScope(Scope);
  Result := QLinhas[Scope];
end;

function CalculateBibleVerseCount(Scope: TEscopoTexto): Integer;
var
  Enumerator: TBibleReferenceEnumerator;
begin
  Result := 0;
  Enumerator := TBibleReferenceEnumerator.Create(Scope);
  try
    while Enumerator.MoveNext do
      Inc(Result);
  finally
    Enumerator.Free;
  end;
end;

function TryBibleReferenceAt(Scope: TEscopoTexto; ZeroBasedIndex: Integer;
  out ReferenceID: string): Boolean;
var
  Enumerator: TBibleReferenceEnumerator;
  CurrentIndex: Integer;
begin
  ReferenceID := '';
  Result := False;
  if ZeroBasedIndex < 0 then
    Exit;

  CurrentIndex := 0;
  Enumerator := TBibleReferenceEnumerator.Create(Scope);
  try
    while Enumerator.MoveNext do
    begin
      if CurrentIndex = ZeroBasedIndex then
      begin
        ReferenceID := Enumerator.ReferenceID;
        Exit(True);
      end;
      Inc(CurrentIndex);
    end;
  finally
    Enumerator.Free;
  end;
end;

end.
