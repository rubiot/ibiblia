unit navigation_history_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, navigation_history;

type
  TNavigationHistoryTests = class(TTestCase)
  private
    FHistory: TNavigationHistory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EmptyHistoryHasNoNavigation;
    procedure VisitsSupportBackAndForward;
    procedure ConsecutiveDuplicateIsIgnored;
    procedure RepeatedNonConsecutiveReferenceIsPreserved;
    procedure NewVisitAfterBackDiscardsForwardBranch;
    procedure ClearResetsHistory;
  end;

implementation

procedure TNavigationHistoryTests.SetUp;
begin
  FHistory := TNavigationHistory.Create;
end;

procedure TNavigationHistoryTests.TearDown;
begin
  FHistory.Free;
end;

procedure TNavigationHistoryTests.EmptyHistoryHasNoNavigation;
var
  ReferenceID: string;
begin
  AssertEquals(0, FHistory.Count);
  AssertEquals('', FHistory.CurrentReference);
  AssertFalse(FHistory.CanGoBack);
  AssertFalse(FHistory.CanGoForward);
  AssertFalse(FHistory.TryGoBack(ReferenceID));
  AssertEquals('', ReferenceID);
  AssertFalse(FHistory.TryGoForward(ReferenceID));
  AssertEquals('', ReferenceID);
end;

procedure TNavigationHistoryTests.VisitsSupportBackAndForward;
var
  ReferenceID: string;
begin
  FHistory.Visit('1,1,1');
  FHistory.Visit('1,1,2');
  FHistory.Visit('1,1,3');

  AssertEquals(3, FHistory.Count);
  AssertTrue(FHistory.CanGoBack);
  AssertFalse(FHistory.CanGoForward);

  AssertTrue(FHistory.TryGoBack(ReferenceID));
  AssertEquals('1,1,2', ReferenceID);
  AssertTrue(FHistory.TryGoBack(ReferenceID));
  AssertEquals('1,1,1', ReferenceID);
  AssertFalse(FHistory.TryGoBack(ReferenceID));
  AssertEquals('', ReferenceID);

  AssertTrue(FHistory.TryGoForward(ReferenceID));
  AssertEquals('1,1,2', ReferenceID);
  AssertTrue(FHistory.TryGoForward(ReferenceID));
  AssertEquals('1,1,3', ReferenceID);
  AssertFalse(FHistory.TryGoForward(ReferenceID));
  AssertEquals('', ReferenceID);
end;

procedure TNavigationHistoryTests.ConsecutiveDuplicateIsIgnored;
begin
  FHistory.Visit('1,1,1');
  FHistory.Visit('1,1,1');

  AssertEquals(1, FHistory.Count);
  AssertEquals('1,1,1', FHistory.CurrentReference);
  AssertFalse(FHistory.CanGoBack);
end;

procedure TNavigationHistoryTests.RepeatedNonConsecutiveReferenceIsPreserved;
var
  ReferenceID: string;
begin
  FHistory.Visit('1,1,1');
  FHistory.Visit('1,1,2');
  FHistory.Visit('1,1,1');

  AssertEquals(3, FHistory.Count);
  AssertTrue(FHistory.TryGoBack(ReferenceID));
  AssertEquals('1,1,2', ReferenceID);
  AssertTrue(FHistory.TryGoBack(ReferenceID));
  AssertEquals('1,1,1', ReferenceID);
end;

procedure TNavigationHistoryTests.NewVisitAfterBackDiscardsForwardBranch;
var
  ReferenceID: string;
begin
  FHistory.Visit('1,1,1');
  FHistory.Visit('1,1,2');
  FHistory.Visit('1,1,3');
  AssertTrue(FHistory.TryGoBack(ReferenceID));
  AssertEquals('1,1,2', ReferenceID);

  FHistory.Visit('2,1,1');

  AssertEquals(3, FHistory.Count);
  AssertEquals('2,1,1', FHistory.CurrentReference);
  AssertFalse(FHistory.CanGoForward);
  AssertTrue(FHistory.TryGoBack(ReferenceID));
  AssertEquals('1,1,2', ReferenceID);
end;

procedure TNavigationHistoryTests.ClearResetsHistory;
begin
  FHistory.Visit('1,1,1');
  FHistory.Visit('1,1,2');

  FHistory.Clear;

  AssertEquals(0, FHistory.Count);
  AssertEquals('', FHistory.CurrentReference);
  AssertFalse(FHistory.CanGoBack);
  AssertFalse(FHistory.CanGoForward);
end;

initialization
  RegisterTest(TNavigationHistoryTests);

end.
