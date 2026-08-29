unit performance_debug_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, performance_debug;

type
  TPerformanceDebugTests = class(TTestCase)
  protected
    procedure TearDown; override;
  published
    procedure DisabledCountersRemainZero;
    procedure EnabledCountersAccumulateAndReset;
  end;

implementation

procedure TPerformanceDebugTests.TearDown;
begin
  IBibliaPerfSetEnabledForTests(False);
  inherited TearDown;
end;

procedure TPerformanceDebugTests.DisabledCountersRemainZero;
var
  Snapshot: TIBibliaPerfCounterValues;
begin
  IBibliaPerfSetEnabledForTests(False);
  IBibliaPerfIncrement(ipcVerseLoads);
  Snapshot := IBibliaPerfSnapshot;
  AssertEquals(0, Int64(Snapshot[ipcVerseLoads]));
end;

procedure TPerformanceDebugTests.EnabledCountersAccumulateAndReset;
var
  Snapshot: TIBibliaPerfCounterValues;
begin
  IBibliaPerfSetEnabledForTests(True);
  IBibliaPerfIncrement(ipcVerseLoads);
  IBibliaPerfIncrement(ipcVerseLoads, 2);
  IBibliaPerfIncrement(ipcWysiwygVisualBuilds, 4);

  Snapshot := IBibliaPerfSnapshot;
  AssertEquals(3, Int64(Snapshot[ipcVerseLoads]));
  AssertEquals(4, Int64(Snapshot[ipcWysiwygVisualBuilds]));

  IBibliaPerfReset;
  Snapshot := IBibliaPerfSnapshot;
  AssertEquals(0, Int64(Snapshot[ipcVerseLoads]));
  AssertEquals(0, Int64(Snapshot[ipcWysiwygVisualBuilds]));
end;

initialization
  RegisterTest(TPerformanceDebugTests);

end.
