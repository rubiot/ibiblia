unit theword_sync_state_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry;

type
  TTheWordSyncStateTests = class(TTestCase)
  published
    procedure EveryDirectionCombinationHasADistinctState;
    procedure StatesRoundTripToDirectionFlags;
    procedure IndependentIniFlagsRestoreEveryState;
  end;

implementation

uses
  SysUtils, IniFiles, theword_sync_state;

procedure TTheWordSyncStateTests.
  EveryDirectionCombinationHasADistinctState;
begin
  AssertEquals(Ord(twssDisabled),
    Ord(ResolveTheWordSyncState(False, False)));
  AssertEquals(Ord(twssIBibliaToTheWord),
    Ord(ResolveTheWordSyncState(True, False)));
  AssertEquals(Ord(twssTheWordToIBiblia),
    Ord(ResolveTheWordSyncState(False, True)));
  AssertEquals(Ord(twssBidirectional),
    Ord(ResolveTheWordSyncState(True, True)));
end;

procedure TTheWordSyncStateTests.StatesRoundTripToDirectionFlags;
var
  IBibliaToTheWord: Boolean;
  State: TTheWordSyncState;
  TheWordToIBiblia: Boolean;
begin
  for State := Low(TTheWordSyncState) to High(TTheWordSyncState) do
  begin
    ExpandTheWordSyncState(State, IBibliaToTheWord, TheWordToIBiblia);
    AssertEquals('expanding and resolving must preserve the state',
      Ord(State), Ord(ResolveTheWordSyncState(IBibliaToTheWord,
        TheWordToIBiblia)));
  end;
end;

procedure TTheWordSyncStateTests.IndependentIniFlagsRestoreEveryState;
var
  FileName: string;
  Ini: TIniFile;
  RestoredState: TTheWordSyncState;
  State: TTheWordSyncState;
begin
  FileName := GetTempFileName(GetTempDir(False), 'tws');
  try
    for State := Low(TTheWordSyncState) to High(TTheWordSyncState) do
    begin
      Ini := TIniFile.Create(FileName);
      try
        WriteTheWordSyncState(Ini, State);
        Ini.UpdateFile;
      finally
        Ini.Free;
      end;

      Ini := TIniFile.Create(FileName);
      try
        RestoredState := ReadTheWordSyncState(Ini);
      finally
        Ini.Free;
      end;
      AssertEquals('the two persisted flags must restore the exact state',
        Ord(State), Ord(RestoredState));
    end;
  finally
    DeleteFile(FileName);
  end;
end;

initialization
  RegisterTest(TTheWordSyncStateTests);

end.
