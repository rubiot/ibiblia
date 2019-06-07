unit ibibliatest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Versiculo, Sintagma;

type

  { TVerseTests }

  TVerseTests = class(TTestCase)
  private
    verse1: TVersiculo;
    verse2: TVersiculo;

    procedure Associate(const source: array of const; const dest: array of const);
    procedure AssertSyntagmListEquals(list: TSintagmaList; const values: array of const);
    procedure AssertPairsAndSiblings(syntagm: TSintagma; const pairs, siblings: array of const);
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
    procedure ReplaceTextSameText;
    procedure ReplaceTextChangeBeginning;
    procedure ReplaceTextChangeEnd;
    procedure ReplaceTextChangeMiddle;
    procedure ReplaceTextTwice;
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
  verse1.Texto := 'Sample<WG1><WTPREP> verse<WH100><WH11><WTPREP l="verse"> <FI>text<Fi>.';
  AssertTrue(verse1.Sintagmas[0].Texto = 'Sample');
  AssertTrue(verse1.Sintagmas[1].Texto = ' ');
  AssertTrue(verse1.Sintagmas[2].Texto = 'verse');
  AssertTrue(verse1.Sintagmas[3].Texto = ' ');
  AssertTrue(verse1.Sintagmas[4].Texto = '<FI>');
  AssertTrue(verse1.Sintagmas[5].Texto = 'text');
  AssertTrue(verse1.Sintagmas[6].Texto = '<Fi>');
  AssertTrue(verse1.Sintagmas[7].Texto = '.');
end;

procedure TVerseTests.StrongsParsing;
var
  s: TSintagma;
begin
  verse1.Texto := 'word';
  s := verse1.Sintagmas.First;
  AssertTrue(s.TemStrongs = false);

  verse1.Texto := 'word<WG1>';
  s := verse1.Sintagmas.First;
  AssertTrue(s.TemStrongs = true);
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
  s: TSintagma;
begin
  verse1.Texto := 'word';
  s := verse1.Sintagmas.First;
  AssertEquals(0, s.Morf.Count);

  verse1.Texto := 'word<WTPREP>';
  s := verse1.Sintagmas.First;
  AssertEquals(1, s.Morf.Count);
  AssertEquals('PREP', s.Morf[0]);

  verse1.Texto := 'word<WTPREP l="prep">';
  s := verse1.Sintagmas.First;
  AssertEquals(1, s.Morf.Count);
  AssertEquals('PREP l="prep"', s.Morf[0]);

  verse1.Texto := 'word<WG1><WTV-AA3S><WG33><WTPREP>';
  s := verse1.Sintagmas.First;
  AssertEquals(2, s.Morf.Count);
  AssertEquals('V-AA3S', s.Morf[0]);
  AssertEquals('PREP', s.Morf[1]);
end;

procedure TVerseTests.AssociationOneToOne;
var
  original, translation: TSintagma;
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
  original, translation1, translation2: TSintagma;
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
  original1, original2, translation1: TSintagma;
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
  original1, original2, translation1, translation2: TSintagma;
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

procedure TVerseTests.ReplaceTextSameText;
var
  original1, original2, translation1, translation2: TSintagma;
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

procedure TVerseTests.ReplaceTextChangeBeginning;
var
  original1, original2, translation1, translation2: TSintagma;
  neworiginal1, neworiginal2, neworiginal3: TSintagma;
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

procedure TVerseTests.ReplaceTextChangeEnd;
var
  original1, original2, translation1, translation2: TSintagma;
  neworiginal1, neworiginal2, neworiginal3: TSintagma;
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

procedure TVerseTests.ReplaceTextChangeMiddle;
var
  original1, original2, translation1, translation2: TSintagma;
  neworiginal1, neworiginal2, neworiginal3: TSintagma;
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

procedure TVerseTests.ReplaceTextTwice;
var
  s, d: TSintagmaList;
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

procedure TVerseTests.Associate(const source: array of const;
  const dest: array of const);
var
  i: integer;
begin
  verse1.LimparSelecao;
  verse2.LimparSelecao;

  for i := 0 to High(source) do
    verse1.Sintagmas[source[i].VInteger].SelecaoMais;

  for i := 0 to High(dest) do
    verse2.Sintagmas[dest[i].VInteger].SelecaoMais;

  verse1.AssociarSintagmas;
end;

procedure TVerseTests.AssertSyntagmListEquals(list: TSintagmaList; const values: array of const);
var
  i: integer;
begin
  AssertEquals(High(values) + 1, list.Count);
  for i := 0 to High(values) do
    AssertTrue(TSintagma(values[i].VPointer) = list[i]);
end;

procedure TVerseTests.AssertPairsAndSiblings(syntagm: TSintagma; const pairs, siblings: array of const);
begin
  AssertSyntagmListEquals(syntagm.Pares, pairs);
  AssertSyntagmListEquals(syntagm.Irmaos, siblings);
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

