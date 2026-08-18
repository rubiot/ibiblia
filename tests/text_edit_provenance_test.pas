unit text_edit_provenance_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry;

type
  TTextEditProvenanceTest = class(TTestCase)
  published
    procedure IdentityProjectsOriginalRanges;
    procedure ReplacementPreservesOnlyUntouchedUnits;
    procedure BoundaryInsertionDoesNotTouchAdjacentRanges;
    procedure InteriorInsertionBreaksTheOriginalRange;
    procedure DeleteAndRetypeDoesNotRestoreProvenance;
    procedure RawChangePreservesOnlyCommonPrefixAndSuffix;
    procedure RawUnchangedKeepsExactMode;
    procedure CloneAndResetAreIndependent;
    procedure Utf16PairsAreNeverSplitByRawDiff;
  end;

implementation

uses
  SysUtils, text_edit_provenance;

procedure TTextEditProvenanceTest.IdentityProjectsOriginalRanges;
var
  CurrentStart, OriginalOffset: Integer;
  Map: TTextOriginMap;
begin
  Map := TTextOriginMap.CreateIdentity(6);
  try
    AssertEquals(6, Map.OriginalLength);
    AssertEquals(6, Map.CurrentLength);
    AssertTrue(Map.TryGetOriginalOffset(4, OriginalOffset));
    AssertEquals(4, OriginalOffset);
    AssertTrue(Map.TryProjectOriginalRange(2, 3, CurrentStart));
    AssertEquals(2, CurrentStart);
  finally
    Map.Free;
  end;
end;

procedure TTextEditProvenanceTest.ReplacementPreservesOnlyUntouchedUnits;
var
  CurrentStart: Integer;
  Map: TTextOriginMap;
begin
  Map := TTextOriginMap.CreateIdentity(6);
  try
    Map.Replace(2, 2, 3);
    AssertEquals(7, Map.CurrentLength);
    AssertTrue(Map.TryProjectOriginalRange(0, 2, CurrentStart));
    AssertEquals(0, CurrentStart);
    AssertFalse(Map.TryProjectOriginalRange(2, 1, CurrentStart));
    AssertTrue(Map.TryProjectOriginalRange(4, 2, CurrentStart));
    AssertEquals(5, CurrentStart);
  finally
    Map.Free;
  end;
end;

procedure TTextEditProvenanceTest.
  BoundaryInsertionDoesNotTouchAdjacentRanges;
var
  CurrentStart: Integer;
  Map: TTextOriginMap;
begin
  Map := TTextOriginMap.CreateIdentity(6);
  try
    Map.Replace(3, 0, 2);
    AssertTrue(Map.TryProjectOriginalRange(0, 3, CurrentStart));
    AssertEquals(0, CurrentStart);
    AssertTrue(Map.TryProjectOriginalRange(3, 3, CurrentStart));
    AssertEquals(5, CurrentStart);
  finally
    Map.Free;
  end;
end;

procedure TTextEditProvenanceTest.InteriorInsertionBreaksTheOriginalRange;
var
  CurrentStart: Integer;
  Map: TTextOriginMap;
begin
  Map := TTextOriginMap.CreateIdentity(6);
  try
    Map.Replace(3, 0, 1);
    AssertFalse(Map.TryProjectOriginalRange(1, 4, CurrentStart));
    AssertTrue(Map.TryProjectOriginalRange(1, 2, CurrentStart));
    AssertTrue(Map.TryProjectOriginalRange(3, 2, CurrentStart));
    AssertEquals(4, CurrentStart);
  finally
    Map.Free;
  end;
end;

procedure TTextEditProvenanceTest.
  DeleteAndRetypeDoesNotRestoreProvenance;
var
  CurrentStart: Integer;
  Trace: TVerseEditTrace;
begin
  Trace := TVerseEditTrace.CreateIdentity('repeat', 17);
  try
    AssertTrue(Trace.IsBaselineIdentity);
    Trace.ApplyReplacement(0, 6, 'repeat');
    AssertEquals(UnicodeString('repeat'), Trace.CurrentText);
    AssertFalse('same characters entered manually remain new content',
      Trace.OriginMap.TryProjectOriginalRange(0, 6, CurrentStart));
    AssertFalse(Trace.IsBaselineIdentity);
    AssertEquals(Ord(vetmExact), Ord(Trace.Mode));
  finally
    Trace.Free;
  end;
end;

procedure TTextEditProvenanceTest.
  RawChangePreservesOnlyCommonPrefixAndSuffix;
var
  CurrentStart: Integer;
  Trace: TVerseEditTrace;
begin
  Trace := TVerseEditTrace.CreateIdentity('abc middle xyz', 23);
  try
    Trace.ApplyRawVisibleChange('abc replacement xyz');
    AssertEquals(Ord(vetmRawConservative), Ord(Trace.Mode));
    AssertEquals(UnicodeString('abc replacement xyz'), Trace.CurrentText);
    AssertTrue(Trace.OriginMap.TryProjectOriginalRange(0, 4,
      CurrentStart));
    AssertEquals(0, CurrentStart);
    AssertFalse(Trace.OriginMap.TryProjectOriginalRange(4, 6,
      CurrentStart));
    AssertTrue(Trace.OriginMap.TryProjectOriginalRange(10, 4,
      CurrentStart));
    AssertEquals(15, CurrentStart);
  finally
    Trace.Free;
  end;
end;

procedure TTextEditProvenanceTest.RawUnchangedKeepsExactMode;
var
  CurrentStart: Integer;
  Trace: TVerseEditTrace;
begin
  Trace := TVerseEditTrace.CreateIdentity('same', 9);
  try
    Trace.ApplyRawVisibleChange('same');
    AssertEquals(Ord(vetmExact), Ord(Trace.Mode));
    AssertTrue(Trace.OriginMap.TryProjectOriginalRange(0, 4,
      CurrentStart));
  finally
    Trace.Free;
  end;
end;

procedure TTextEditProvenanceTest.CloneAndResetAreIndependent;
var
  CurrentStart: Integer;
  Clone, Trace: TVerseEditTrace;
begin
  Trace := TVerseEditTrace.CreateIdentity('abcd', 5);
  Clone := Trace.Clone;
  try
    Clone.ApplyReplacement(1, 1, 'X');
    AssertEquals(UnicodeString('abcd'), Trace.CurrentText);
    AssertEquals(UnicodeString('aXcd'), Clone.CurrentText);
    AssertTrue(Trace.OriginMap.TryProjectOriginalRange(1, 1,
      CurrentStart));
    AssertFalse(Clone.OriginMap.TryProjectOriginalRange(1, 1,
      CurrentStart));

    Clone.MarkLegacyFallback;
    AssertEquals(Ord(vetmLegacyFallback), Ord(Clone.Mode));
    Clone.ResetBaseline(Clone.CurrentText, 6);
    AssertEquals(Ord(vetmExact), Ord(Clone.Mode));
    AssertEquals(6, Clone.TokenizerVersion);
    AssertEquals(UnicodeString('aXcd'), Clone.OriginalText);
  finally
    Clone.Free;
    Trace.Free;
  end;
end;

procedure TTextEditProvenanceTest.Utf16PairsAreNeverSplitByRawDiff;
var
  CurrentStart: Integer;
  Emoji, Replacement: UnicodeString;
  Trace: TVerseEditTrace;
begin
  Emoji := UnicodeString(WideChar($D83D)) +
    UnicodeString(WideChar($DE00));
  Replacement := UnicodeString(WideChar($D83D)) +
    UnicodeString(WideChar($DE01));
  Trace := TVerseEditTrace.CreateIdentity('A' + Emoji + 'Z', 1);
  try
    Trace.ApplyRawVisibleChange('A' + Replacement + 'Z');
    AssertFalse('a partially matching surrogate pair is all treated as new',
      Trace.OriginMap.TryProjectOriginalRange(1, 2, CurrentStart));
    AssertTrue(Trace.OriginMap.TryProjectOriginalRange(3, 1,
      CurrentStart));
    AssertEquals(3, CurrentStart);
  finally
    Trace.Free;
  end;
end;

initialization
  RegisterTest(TTextEditProvenanceTest);

end.
