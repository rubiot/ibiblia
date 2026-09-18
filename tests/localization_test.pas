unit localization_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry;

type
  TLocalizationTests = class(TTestCase)
  published
    procedure PortugueseCatalogLocalizesEveryBibleBook;
    procedure FlowConnectionHintsFollowSelectedLanguage;
  end;

implementation

uses
  SysUtils, LCLTranslator, bible_book_names,
  ai_association_flow, ai_translation_flow;

const
  PORTUGUESE_BOOK_NAMES: array[1..66] of string = (
    'Gênesis', 'Êxodo', 'Levítico', 'Números', 'Deuteronômio', 'Josué',
    'Juízes', 'Rute', '1 Samuel', '2 Samuel', '1 Reis', '2 Reis',
    '1 Crônicas', '2 Crônicas', 'Esdras', 'Neemias', 'Ester', 'Jó',
    'Salmos', 'Provérbios', 'Eclesiastes', 'Cântico dos Cânticos', 'Isaías',
    'Jeremias', 'Lamentações', 'Ezequiel', 'Daniel', 'Oseias', 'Joel',
    'Amós', 'Obadias', 'Jonas', 'Miqueias', 'Naum', 'Habacuque', 'Sofonias',
    'Ageu', 'Zacarias', 'Malaquias', 'Mateus', 'Marcos', 'Lucas', 'João',
    'Atos', 'Romanos', '1 Coríntios', '2 Coríntios', 'Gálatas', 'Efésios',
    'Filipenses', 'Colossenses', '1 Tessalonicenses', '2 Tessalonicenses',
    '1 Timóteo', '2 Timóteo', 'Tito', 'Filemom', 'Hebreus', 'Tiago',
    '1 Pedro', '2 Pedro', '1 João', '2 João', '3 João', 'Judas', 'Apocalipse');

procedure TLocalizationTests.PortugueseCatalogLocalizesEveryBibleBook;
var
  BookID: Integer;
  LanguagesDir: string;
begin
  LanguagesDir := ExpandFileName(IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + '..\languages');
  AssertTrue('Portuguese application catalog must be available to the test',
    FileExists(IncludeTrailingPathDelimiter(LanguagesDir) + 'iBiblia.pt.po'));

  SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  try
    SetDefaultLang('pt', LanguagesDir, 'iBiblia', False);
    for BookID := Low(PORTUGUESE_BOOK_NAMES) to
      High(PORTUGUESE_BOOK_NAMES) do
      AssertEquals('Unexpected localized name for book ' + IntToStr(BookID),
        PORTUGUESE_BOOK_NAMES[BookID], LocalizedBookNameByID(BookID));
  finally
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  end;
end;

procedure TLocalizationTests.FlowConnectionHintsFollowSelectedLanguage;
var
  LanguagesDir: string;
begin
  LanguagesDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\languages');
  SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  try
    SetDefaultLang('pt', LanguagesDir, 'iBiblia', False);
    AssertTrue('Provider hint must refer to the common settings in Portuguese',
      Pos('configurações comuns',
        ai_association_flow.SConfigProviderOverrideDescription) > 0);
    AssertTrue('Model hint must refer to the common settings in Portuguese',
      Pos('configurações comuns',
        ai_association_flow.SConfigModelOverrideDescription) > 0);
    AssertTrue('API key hint must refer to the common settings in Portuguese',
      Pos('configurações comuns',
        ai_association_flow.SConfigApiKeyOverrideDescription) > 0);
    AssertEquals(ai_association_flow.SConfigProviderOverrideDescription,
      ai_translation_flow.SConfigProviderOverrideDescription);
    AssertEquals(ai_association_flow.SConfigModelOverrideDescription,
      ai_translation_flow.SConfigModelOverrideDescription);
    AssertEquals(ai_association_flow.SConfigApiKeyOverrideDescription,
      ai_translation_flow.SConfigApiKeyOverrideDescription);
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
    AssertEquals('Specify a different provider for this flow. Leave empty to use common settings.',
      ai_association_flow.SConfigProviderOverrideDescription);
    AssertEquals('Specify a different model for this flow. Leave empty to use common settings.',
      ai_association_flow.SConfigModelOverrideDescription);
    AssertEquals('Specify a different API key for this flow. Leave empty to use common settings.',
      ai_association_flow.SConfigApiKeyOverrideDescription);
    AssertEquals(ai_association_flow.SConfigProviderOverrideDescription,
      ai_translation_flow.SConfigProviderOverrideDescription);
    AssertEquals(ai_association_flow.SConfigModelOverrideDescription,
      ai_translation_flow.SConfigModelOverrideDescription);
    AssertEquals(ai_association_flow.SConfigApiKeyOverrideDescription,
      ai_translation_flow.SConfigApiKeyOverrideDescription);
  finally
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  end;
end;

initialization
  RegisterTest(TLocalizationTests);

end.
