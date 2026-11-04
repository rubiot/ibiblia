unit navigation_history;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  { TNavigationHistory }

  TNavigationHistory = class
  private
    FEntries: TStringList;
    FCurrentIndex: Integer;
    function GetCount: Integer;
    function GetCurrentReference: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Visit(const ReferenceID: string);
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    function TryGoBack(out ReferenceID: string): Boolean;
    function TryGoForward(out ReferenceID: string): Boolean;
    property Count: Integer read GetCount;
    property CurrentReference: string read GetCurrentReference;
  end;

implementation

constructor TNavigationHistory.Create;
begin
  inherited Create;
  FEntries := TStringList.Create;
  FCurrentIndex := -1;
end;

destructor TNavigationHistory.Destroy;
begin
  FEntries.Free;
  inherited Destroy;
end;

procedure TNavigationHistory.Clear;
begin
  FEntries.Clear;
  FCurrentIndex := -1;
end;

procedure TNavigationHistory.Visit(const ReferenceID: string);
begin
  if ReferenceID = '' then
    Exit;

  if (FCurrentIndex >= 0) and
     (FEntries[FCurrentIndex] = ReferenceID) then
    Exit;

  while FEntries.Count - 1 > FCurrentIndex do
    FEntries.Delete(FEntries.Count - 1);

  FEntries.Add(ReferenceID);
  FCurrentIndex := FEntries.Count - 1;
end;

function TNavigationHistory.CanGoBack: Boolean;
begin
  Result := FCurrentIndex > 0;
end;

function TNavigationHistory.CanGoForward: Boolean;
begin
  Result := (FCurrentIndex >= 0) and
    (FCurrentIndex < FEntries.Count - 1);
end;

function TNavigationHistory.TryGoBack(out ReferenceID: string): Boolean;
begin
  Result := CanGoBack;
  if not Result then
  begin
    ReferenceID := '';
    Exit;
  end;

  Dec(FCurrentIndex);
  ReferenceID := FEntries[FCurrentIndex];
end;

function TNavigationHistory.TryGoForward(out ReferenceID: string): Boolean;
begin
  Result := CanGoForward;
  if not Result then
  begin
    ReferenceID := '';
    Exit;
  end;

  Inc(FCurrentIndex);
  ReferenceID := FEntries[FCurrentIndex];
end;

function TNavigationHistory.GetCount: Integer;
begin
  Result := FEntries.Count;
end;

function TNavigationHistory.GetCurrentReference: string;
begin
  if (FCurrentIndex >= 0) and (FCurrentIndex < FEntries.Count) then
    Result := FEntries[FCurrentIndex]
  else
    Result := '';
end;

end.
