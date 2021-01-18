unit ibibliatest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Versiculo, Syntagm;

type

  { TVerseTests }

  TVerseTests = class(TTestCase)
  private
    verse1: TVersiculo;
    verse2: TVersiculo;

    procedure Associate(const source: array of const; const dest: array of const);
    procedure AssertSyntagmListEquals(list: TSyntagmList; const values: array of const);
    procedure AssertPairsAndSiblings(syntagm: TSyntagm; const pairs, siblings: array of const);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure SetTexto;
    procedure SyntagmsParsing;
    procedure StrongsParsing;
    procedure MorphologyParsing;
    procedure AssociationOneToOne;
    procedure AssociationOneToMany;
    procedure AssociationManyToOne;
    procedure AssociationManyToMany;
    procedure ReplaceSourceTextSameText;
    procedure ReplaceDestinationTextSameText;
    procedure ReplaceSourceTextChangeBeginning;
    procedure ReplaceDestinationTextChangeBeginning;
    procedure ReplaceSourceTextChangeEnd;
    procedure ReplaceDestinationTextChangeEnd;
    procedure ReplaceSourceTextChangeMiddle;
    procedure ReplaceDestinationTextChangeMiddle;
    procedure ReplaceTextTwice;
    procedure ReplaceSourceTextVerticalBar;
    procedure ReplaceDestinationTextVerticalBar;
    procedure ReplaceSourceTextAddPunctuation;
  end;

implementation

procedure TVerseTests.TestHookUp;
begin
  AssertTrue(verse1.VersiculoPar = verse2);
  AssertTrue(verse2.VersiculoPar = verse1);
end;

procedure TVerseTests.SetTexto;
var
  sample: string;
begin
  sample := 'Sample<WG1><WTPREP> verse<WTPREP l="verse"> <FI>text<Fi>.';
  verse1.Texto := sample;
  AssertEquals(sample, verse1.XML);
  AssertEquals('Sample verse <FI>text<Fi>.', verse1.Texto);
  AssertEquals('Sample verse text.', verse1.TextoSimples);
end;

procedure TVerseTests.SyntagmsParsing;
begin
  verse1.Texto := 'Sample<WG1><WTPREP> verse<WH100><WH11><WTPREP l="verse"> <FI>text<Fi> x|z.';
  AssertTrue(verse1.Sintagmas[0].Text = 'Sample');
  AssertTrue(verse1.Sintagmas[1].Text = ' ');
  AssertTrue(verse1.Sintagmas[2].Text = 'verse');
  AssertTrue(verse1.Sintagmas[3].Text = ' ');
  AssertTrue(verse1.Sintagmas[4].Text = '<FI>');
  AssertTrue(verse1.Sintagmas[5].Text = 'text');
  AssertTrue(verse1.Sintagmas[6].Text = '<Fi>');
  AssertTrue(verse1.Sintagmas[7].Text = ' ');
  AssertTrue(verse1.Sintagmas[8].Text = 'x');
  AssertTrue(verse1.Sintagmas[9].Text = 'z');
  AssertTrue(verse1.Sintagmas[10].Text = '.');
end;

procedure TVerseTests.StrongsParsing;
var
  s: TSyntagm;
begin
  verse1.Texto := 'word';
  s := verse1.Sintagmas.First;
  AssertTrue(s.HasStrongs = false);

  verse1.Texto := 'word<WG1>';
  s := verse1.Sintagmas.First;
  AssertTrue(s.HasStrongs = true);
  AssertEquals(1, s.Strong.Count);
  AssertEquals('G1', s.Strong[0]);

  verse1.Texto := 'verse<WH100><WH11>';
  s := verse1.Sintagmas.First;
  AssertEquals(2, s.Strong.Count);
  AssertEquals('H100', s.Strong[0]);
  AssertEquals('H11', s.Strong[1]);
end;

procedure TVerseTests.MorphologyParsing;
var
  s: TSyntagm;
begin
  verse1.Texto := 'word';
  s := verse1.Sintagmas.First;
  AssertEquals(0, s.Morph.Count);

  verse1.Texto := 'word<WTPREP>';
  s := verse1.Sintagmas.First;
  AssertEquals(1, s.Morph.Count);
  AssertEquals('PREP', s.Morph[0]);

  verse1.Texto := 'word<WTPREP l="prep">';
  s := verse1.Sintagmas.First;
  AssertEquals(1, s.Morph.Count);
  AssertEquals('PREP l="prep"', s.Morph[0]);

  verse1.Texto := 'word<WG1><WTV-AA3S><WG33><WTPREP>';
  s := verse1.Sintagmas.First;
  AssertEquals(2, s.Morph.Count);
  AssertEquals('V-AA3S', s.Morph[0]);
  AssertEquals('PREP', s.Morph[1]);
end;

procedure TVerseTests.AssociationOneToOne;
var
  original, translation: TSyntagm;
begin
  verse1.Texto := 'original';
  verse2.Texto := 'translation';
  Associate([0], [0]);

  original    := verse1.Sintagmas.First;
  translation := verse2.Sintagmas.First;

  AssertPairsAndSiblings(original,    [translation], []);
  AssertPairsAndSiblings(translation, [original   ], []);
end;

procedure TVerseTests.AssociationOneToMany;
var
  original, translation1, translation2: TSyntagm;
begin
  verse1.Texto := 'original';
  verse2.Texto := 'translation1 translation2';
  Associate([0], [0, 2]);

  original     := verse1.Sintagmas[0];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  AssertPairsAndSiblings(original,     [translation1, translation2], []);
  AssertPairsAndSiblings(translation1, [original], [translation2]);
  AssertPairsAndSiblings(translation2, [original], [translation1]);
end;

procedure TVerseTests.AssociationManyToOne;
var
  original1, original2, translation1: TSyntagm;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1';
  Associate([0,2], [0]);

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];

  AssertPairsAndSiblings(original1,    [translation1], [original2]);
  AssertPairsAndSiblings(original2,    [translation1], [original1]);
  AssertPairsAndSiblings(translation1, [original1, original2], []);
end;

procedure TVerseTests.AssociationManyToMany;
var
  original1, original2, translation1, translation2: TSyntagm;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1 translation2';
  Associate([0,2], [0,2]);

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  AssertPairsAndSiblings(original1,    [translation1, translation2], [original2]);
  AssertPairsAndSiblings(original2,    [translation1, translation2], [original1]);
  AssertPairsAndSiblings(translation1, [original1, original2], [translation2]);
  AssertPairsAndSiblings(translation2, [original1, original2], [translation1]);
end;

procedure TVerseTests.ReplaceSourceTextSameText;
var
  original1, original2, translation1, translation2: TSyntagm;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  verse1.AlterarTexto('original1 original2');

  AssertPairsAndSiblings(original1,    [translation1], []);
  AssertPairsAndSiblings(original2,    [translation2], []);
  AssertPairsAndSiblings(translation1, [original1   ], []);
  AssertPairsAndSiblings(translation2, [original2   ], []);
end;

procedure TVerseTests.ReplaceDestinationTextSameText;
var
  original1, original2, translation1, translation2: TSyntagm;
begin
  verse1.Texto := 'translation1 translation2';
  verse2.Texto := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  translation1 := verse1.Sintagmas[0];
  translation2 := verse1.Sintagmas[2];
  original1    := verse2.Sintagmas[0];
  original2    := verse2.Sintagmas[2];

  verse2.AlterarTexto('original1 original2');

  AssertPairsAndSiblings(original1,    [translation1], []);
  AssertPairsAndSiblings(original2,    [translation2], []);
  AssertPairsAndSiblings(translation1, [original1   ], []);
  AssertPairsAndSiblings(translation2, [original2   ], []);
end;

procedure TVerseTests.ReplaceSourceTextChangeBeginning;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  //                   0         2         4
  verse1.AlterarTexto('original3 original1 original2');

  neworiginal3 := verse1.Sintagmas[0];
  neworiginal1 := verse1.Sintagmas[2];
  neworiginal2 := verse1.Sintagmas[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceDestinationTextChangeBeginning;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Texto := 'translation1 translation2';
  verse2.Texto := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse2.Sintagmas[0];
  original2    := verse2.Sintagmas[2];
  translation1 := verse1.Sintagmas[0];
  translation2 := verse1.Sintagmas[2];

  //                   0         2         4
  verse2.AlterarTexto('original3 original1 original2');

  neworiginal3 := verse2.Sintagmas[0];
  neworiginal1 := verse2.Sintagmas[2];
  neworiginal2 := verse2.Sintagmas[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceSourceTextChangeEnd;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  //                   0         2         4
  verse1.AlterarTexto('original1 original2 original3');

  neworiginal1 := verse1.Sintagmas[0];
  neworiginal2 := verse1.Sintagmas[2];
  neworiginal3 := verse1.Sintagmas[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceDestinationTextChangeEnd;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Texto := 'translation1 translation2';
  verse2.Texto := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  translation1 := verse1.Sintagmas[0];
  translation2 := verse1.Sintagmas[2];
  original1    := verse2.Sintagmas[0];
  original2    := verse2.Sintagmas[2];

  //                   0         2         4
  verse2.AlterarTexto('original1 original2 original3');

  neworiginal1 := verse2.Sintagmas[0];
  neworiginal2 := verse2.Sintagmas[2];
  neworiginal3 := verse2.Sintagmas[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceSourceTextChangeMiddle;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1 translation2';
  Associate([0],[0]);
  Associate([2],[2]);

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  //                   0         2         4
  verse1.AlterarTexto('original1 original3 original2');

  neworiginal1 := verse1.Sintagmas[0];
  neworiginal3 := verse1.Sintagmas[2];
  neworiginal2 := verse1.Sintagmas[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceDestinationTextChangeMiddle;
var
  original1, original2, translation1, translation2: TSyntagm;
  neworiginal1, neworiginal2, neworiginal3: TSyntagm;
begin
  verse1.Texto := 'translation1 translation2';
  verse2.Texto := 'original1 original2';
  Associate([0],[0]);
  Associate([2],[2]);

  translation1 := verse1.Sintagmas[0];
  translation2 := verse1.Sintagmas[2];
  original1    := verse2.Sintagmas[0];
  original2    := verse2.Sintagmas[2];

  //                   0         2         4
  verse2.AlterarTexto('original1 original3 original2');

  neworiginal1 := verse2.Sintagmas[0];
  neworiginal3 := verse2.Sintagmas[2];
  neworiginal2 := verse2.Sintagmas[4];

  AssertTrue(neworiginal1 = original1);
  AssertTrue(neworiginal2 = original2);
  AssertPairsAndSiblings(neworiginal1, [translation1], []);
  AssertPairsAndSiblings(neworiginal2, [translation2], []);
  AssertPairsAndSiblings(neworiginal3, [], []);
  AssertPairsAndSiblings(translation1, [neworiginal1], []);
  AssertPairsAndSiblings(translation2, [neworiginal2], []);
end;

procedure TVerseTests.ReplaceTextTwice;
var
  s, d: TSyntagmList;
begin           // 0 2 4 6
  verse1.Texto := 'a b c d';
  verse2.Texto := 'a b c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([4],[4]);
  Associate([6],[6]);
                    // 0 2 4 6 8
  verse2.AlterarTexto('a b e c d');

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[4], [d[6]], []);
  AssertPairsAndSiblings(s[6], [d[8]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[4], [],     []);
  AssertPairsAndSiblings(d[6], [s[4]], []);
  AssertPairsAndSiblings(d[8], [s[6]], []);
                    // 0 2 4 6 8
  verse2.AlterarTexto('a b c e d');

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[6], [d[8]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[6], [    ], []);
  AssertPairsAndSiblings(d[8], [s[6]], []);
end;

procedure TVerseTests.ReplaceSourceTextVerticalBar;
var
  s, d: TSyntagmList;
begin           // 0 2 3 5
  verse1.Texto := 'a b|c d';
                // 0 2 4 6
  verse2.Texto := 'a b c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([3],[4]);
  Associate([5],[6]);

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [d[4]], []);
  AssertPairsAndSiblings(s[4], [],     []);
  AssertPairsAndSiblings(s[5], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[3]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[5]], []);
                    // 0 2 4 6
  verse1.AlterarTexto('a b c d');

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[6]], []);
end;

procedure TVerseTests.ReplaceDestinationTextVerticalBar;
var
  s, d: TSyntagmList;
begin
                // 0 2 4 6
  verse1.Texto := 'a b c d';
                // 0 2 3 5
  verse2.Texto := 'a b|c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([4],[3]);
  Associate([6],[5]);

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[3]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[5]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [s[4]], []);
  AssertPairsAndSiblings(d[4], [],     []);
  AssertPairsAndSiblings(d[5], [s[6]], []);
                    // 0 2 4 6
  verse2.AlterarTexto('a b c d');

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[6]], []);
end;

procedure TVerseTests.ReplaceSourceTextAddPunctuation;
var
  s, d: TSyntagmList;
begin
                // 0 2 4 6
  verse1.Texto := 'a b c d';
  verse2.Texto := 'a b c d';
  Associate([0],[0]);
  Associate([2],[2]);
  Associate([4],[4]);
  Associate([6],[6]);

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [d[4]], []);
  AssertPairsAndSiblings(s[5], [],     []);
  AssertPairsAndSiblings(s[6], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[4]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[6]], []);

                    // 0 2  5 7
  verse1.AlterarTexto('a b, c d');

  s := verse1.Sintagmas;
  d := verse2.Sintagmas;

  AssertPairsAndSiblings(s[0], [d[0]], []);
  AssertPairsAndSiblings(s[1], [],     []);
  AssertPairsAndSiblings(s[2], [d[2]], []);
  AssertPairsAndSiblings(s[3], [],     []);
  AssertPairsAndSiblings(s[4], [],     []);
  AssertPairsAndSiblings(s[5], [d[4]], []);
  AssertPairsAndSiblings(s[6], [],     []);
  AssertPairsAndSiblings(s[7], [d[6]], []);

  AssertPairsAndSiblings(d[0], [s[0]], []);
  AssertPairsAndSiblings(d[1], [],     []);
  AssertPairsAndSiblings(d[2], [s[2]], []);
  AssertPairsAndSiblings(d[3], [],     []);
  AssertPairsAndSiblings(d[4], [s[5]], []);
  AssertPairsAndSiblings(d[5], [],     []);
  AssertPairsAndSiblings(d[6], [s[7]], []);
end;

procedure TVerseTests.Associate(const source: array of const;
  const dest: array of const);
var
  i: integer;
begin
  verse1.LimparSelecao;
  verse2.LimparSelecao;

  for i := 0 to High(source) do
    verse1.Sintagmas[source[i].VInteger].AddToSelection;

  for i := 0 to High(dest) do
    verse2.Sintagmas[dest[i].VInteger].AddToSelection;

  verse1.AssociarSintagmas;
end;

procedure TVerseTests.AssertSyntagmListEquals(list: TSyntagmList; const values: array of const);
var
  i: integer;
begin
  AssertEquals(High(values) + 1, list.Count);
  for i := 0 to High(values) do
    AssertTrue(Format('[%s]=[%s]', [TSyntagm(values[i].VPointer).RawText, list[i].RawText]),
               TSyntagm(values[i].VPointer) = list[i]);
end;

procedure TVerseTests.AssertPairsAndSiblings(syntagm: TSyntagm; const pairs, siblings: array of const);
begin
  AssertSyntagmListEquals(syntagm.Pairs, pairs);
  AssertSyntagmListEquals(syntagm.Siblings, siblings);
end;

procedure TVerseTests.SetUp;
begin
  verse1 := TVersiculo.Criar;
  verse2 := TVersiculo.Criar;
  verse1.VersiculoPar := verse2;
end;

procedure TVerseTests.TearDown;
begin
  verse1.Destroy;
  verse2.Destroy;
end;

initialization

  RegisterTest(TVerseTests);
end.

