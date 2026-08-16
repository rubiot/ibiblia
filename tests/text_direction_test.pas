unit text_direction_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, text_direction;

type
  TTextDirectionTests = class(TTestCase)
  private
    procedure AssertDirection(const MessageText: string;
      Expected, Actual: TTextDirection);
  published
    procedure MissingAndEmptyR2LPropertiesAreLeftToRight;
    procedure EveryNonEmptyR2LValueIsRightToLeft;
    procedure ParserHandlesCaseWhitespaceAndComments;
    procedure NormalizerRemovesR2LForLeftToRight;
    procedure NormalizerWritesOneCanonicalR2LForRightToLeft;
    procedure NormalizerIsIdempotent;
  end;

implementation

procedure TTextDirectionTests.AssertDirection(const MessageText: string;
  Expected, Actual: TTextDirection);
begin
  AssertEquals(MessageText, Ord(Expected), Ord(Actual));
end;

procedure TTextDirectionTests.MissingAndEmptyR2LPropertiesAreLeftToRight;
begin
  AssertDirection('missing property', tdLeftToRight,
    ParseTheWordR2LProperties('description=Sample'));
  AssertDirection('empty property', tdLeftToRight,
    ParseTheWordR2LProperties('r2l='));
  AssertDirection('whitespace-only property', tdLeftToRight,
    ParseTheWordR2LProperties('r2l =   '));
end;

procedure TTextDirectionTests.EveryNonEmptyR2LValueIsRightToLeft;
begin
  AssertDirection('canonical value', tdRightToLeft,
    ParseTheWordR2LProperties('r2l=1'));
  AssertDirection('zero is still enabled by theWord', tdRightToLeft,
    ParseTheWordR2LProperties('r2l=0'));
  AssertDirection('arbitrary non-empty value', tdRightToLeft,
    ParseTheWordR2LProperties('r2l=false'));
end;

procedure TTextDirectionTests.ParserHandlesCaseWhitespaceAndComments;
begin
  AssertDirection('property names are case-insensitive', tdRightToLeft,
    ParseTheWordR2LProperties('  R2L = 0  # native non-empty semantics'));
  AssertDirection('a comment does not make an empty value non-empty',
    tdLeftToRight, ParseTheWordR2LProperties('r2l= # no value'));
  AssertDirection('a later non-empty duplicate enables RTL', tdRightToLeft,
    ParseTheWordR2LProperties('r2l=' + LineEnding + 'r2l=anything'));
end;

procedure TTextDirectionTests.NormalizerRemovesR2LForLeftToRight;
var
  Properties: string;
begin
  Properties := 'description=Sample' + #10 + 'R2L=1' + #10 +
    'font=Ezra SIL' + #10;
  AssertEquals('other properties and line endings remain intact',
    'description=Sample' + #10 + 'font=Ezra SIL' + #10,
    NormalizeTheWordR2LProperties(Properties, tdLeftToRight));
end;

procedure TTextDirectionTests.NormalizerWritesOneCanonicalR2LForRightToLeft;
var
  Properties: string;
begin
  Properties := 'r2l=' + LineEnding + 'description=Sample' + LineEnding +
    'R2L = 0';
  AssertEquals('all existing variants are replaced by one canonical property',
    'description=Sample' + LineEnding + 'r2l=1',
    NormalizeTheWordR2LProperties(Properties, tdRightToLeft));
  AssertEquals('an empty property block receives the canonical property',
    'r2l=1', NormalizeTheWordR2LProperties('', tdRightToLeft));
end;

procedure TTextDirectionTests.NormalizerIsIdempotent;
var
  Normalized: string;
begin
  Normalized := NormalizeTheWordR2LProperties(
    'description=Sample' + LineEnding + 'r2l=0' + LineEnding,
    tdRightToLeft);
  AssertEquals(Normalized,
    NormalizeTheWordR2LProperties(Normalized, tdRightToLeft));
end;

initialization
  RegisterTest(TTextDirectionTests);

end.
