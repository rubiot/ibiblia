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

    procedure MakeTwoAssociatedWords;
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

  original    := verse1.Sintagmas.First;
  translation := verse2.Sintagmas.First;

  original.SelecaoMais;
  translation.SelecaoMais;

  verse1.AssociarSintagmas;

  AssertEquals(1, original.Pares.Count);
  AssertEquals(1, translation.Pares.Count);
  AssertTrue(original.Pares.First = translation);
  AssertTrue(translation.Pares.First = original);
  AssertTrue(original.Irmaos.Empty);
  AssertTrue(translation.Irmaos.Empty);
end;

procedure TVerseTests.AssociationOneToMany;
var
  original, translation1, translation2: TSintagma;
begin
  verse1.Texto := 'original';
  verse2.Texto := 'translation1 translation2';

  original     := verse1.Sintagmas[0];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  original.SelecaoMais;
  translation1.SelecaoMais;
  translation2.SelecaoMais;

  verse1.AssociarSintagmas;

  with original do
  begin
    AssertEquals(2, Pares.Count);
    AssertTrue(Pares[0] = translation1);
    AssertTrue(Pares[1] = translation2);
    AssertEquals(0, Irmaos.Count);
  end;
  with translation1 do
  begin
    AssertEquals(1, Pares.Count);
    AssertTrue(Pares.First = original);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = translation2); // 1 is the space
  end;
  with translation2 do
  begin
    AssertEquals(1, Pares.Count);
    AssertTrue(Pares.First = original);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = translation1);
  end;
end;

procedure TVerseTests.AssociationManyToOne;
var
  original1, original2, translation1: TSintagma;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1';

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];

  original1.SelecaoMais;
  original2.SelecaoMais;
  translation1.SelecaoMais;

  verse1.AssociarSintagmas;

  with original1 do
  begin
    AssertEquals(1, Pares.Count);
    AssertTrue(Pares.First = translation1);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = original2); // 1 is the space
  end;
  with original2 do
  begin
    AssertEquals(1, Pares.Count);
    AssertTrue(Pares.First = translation1);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = original1);
  end;
  with translation1 do
  begin
    AssertEquals(2, Pares.Count);
    AssertTrue(Pares[0] = original1);
    AssertTrue(Pares[1] = original2);
    AssertEquals(0, Irmaos.Count);
  end;
end;

procedure TVerseTests.AssociationManyToMany;
var
  original1, original2, translation1, translation2: TSintagma;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1 translation2';

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  original1.SelecaoMais;
  original2.SelecaoMais;
  translation1.SelecaoMais;
  translation2.SelecaoMais;

  verse1.AssociarSintagmas;

  with original1 do
  begin
    AssertEquals(2, Pares.Count);
    AssertTrue(Pares[0] = translation1);
    AssertTrue(Pares[1] = translation2);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = original2); // 1 is the space
  end;
  with original2 do
  begin
    AssertEquals(2, Pares.Count);
    AssertTrue(Pares[0] = translation1);
    AssertTrue(Pares[1] = translation2);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = original1);
  end;
  with translation1 do
  begin
    AssertEquals(2, Pares.Count);
    AssertTrue(Pares[0] = original1);
    AssertTrue(Pares[1] = original2);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = translation2);
  end;
  with translation2 do
  begin
    AssertEquals(2, Pares.Count);
    AssertTrue(Pares[0] = original1);
    AssertTrue(Pares[1] = original2);
    AssertEquals(1, Irmaos.Count);
    AssertTrue(Irmaos[0] = translation1);
  end;
end;

procedure TVerseTests.ReplaceTextSameText;
var
  original1, original2, translation1, translation2: TSintagma;
begin
  MakeTwoAssociatedWords;

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
  MakeTwoAssociatedWords;

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
  MakeTwoAssociatedWords;

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
  MakeTwoAssociatedWords;

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

procedure TVerseTests.MakeTwoAssociatedWords;
var
  original1, original2, translation1, translation2: TSintagma;
begin
  verse1.Texto := 'original1 original2';
  verse2.Texto := 'translation1 translation2';

  original1    := verse1.Sintagmas[0];
  original2    := verse1.Sintagmas[2];
  translation1 := verse2.Sintagmas[0];
  translation2 := verse2.Sintagmas[2];

  original1.SelecaoMais;
  translation1.SelecaoMais;
  verse1.AssociarSintagmas;

  verse1.LimparSelecao;
  verse2.LimparSelecao;

  original2.SelecaoMais;
  translation2.SelecaoMais;
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

