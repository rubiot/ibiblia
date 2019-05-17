unit MySwordModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, PCRE, gmap, gutil;

type

{ TMySwordModule }

 TMySwordModule = class
  private
    FConnection: TSQLite3Connection;
    FOT: boolean;
    FNT: boolean;
    FStrongs: boolean;
    FInsertVerse: TSQLQuery;
    procedure SetProperties(props: string);
  public
    constructor Create(filename: string; ot: boolean; nt: boolean; strongs: boolean; props: string);
    destructor Destroy; override;
    procedure AddVerse(text: string; book, chapter, verse: integer);
    procedure Commit;
  end;

  TStrLess = specialize TLess<string>;
  TPropsMap = specialize TMap<string, string, TStrLess>;

resourcestring
  SProjectOverrideError = 'A project with this name already exists. Delete it first or choose another name';
  SOverrideError = 'MySword Bible module exists and cannot be overwritten';
  SCreateMySwordBibleError = 'Unable to create new MySword module file';

implementation

{ TMySwordModule }

procedure TMySwordModule.SetProperties(props: string);
var
  strings: TStringList;
  kv, key, value: string;
  reKV: IRegex;
  mtKV: IMatch;
  propMap: TPropsMap;
  FInsert: TSQLQuery;
begin
  strings := TStringList.Create;
  strings.Text := props;
  reKV := RegexCreate('^(.*?)\s*=\s*(.*)\s*$', [rcoUTF8]);
  propMap := TPropsMap.Create;

  // default values
  propMap['title'        ] := '';
  propMap['description'  ] := '';
  propMap['short.title'  ] := '';
  propMap['version.major'] := '0';
  propMap['version.minor'] := '0';
  propMap['version.date' ] := '';
  propMap['publish.date' ] := '';
  propMap['publisher'    ] := '';
  propMap['author'       ] := 'iBiblia';
  propMap['creator'      ] := 'iBiblia';
  propMap['source'       ] := 'iBiblia';
  propMap['lang'         ] := '';
  propMap['rtl'          ] := '0';

  // TODO: handle multiline properties and comments
  for kv in strings do
  begin
    mtKV := reKV.Match(kv);
    if mtKV.Success then
    begin
      key   := mtKV.Groups[1].Value;
      value := mtKV.Groups[2].Value;
      propMap[key] := value;
    end;
  end;

  FInsert := TSQLQuery.Create(nil);
  FInsert.DataBase := FConnection;

  {
  "Title" NVARCHAR(255),
  "Description" TEXT,
  "Abbreviation" NVARCHAR(50),
  "Comments" TEXT,
  "Version" TEXT,
  "VersionDate" DATETIME,
  "PublishDate" DATETIME,
  "Publisher" TEXT,
  "Author" TEXT,
  "Creator" TEXT,
  "Source" TEXT,
  "EditorialComments" TEXT,
  "Language" NVARCHAR(3),
  "RightToLeft" BOOL,
  "OT" BOOL,
  "NT" BOOL,
  "Strong" BOOL,
  "VerseRules" TEXT
  }

  FInsert.SQL.Text :=
    'INSERT INTO Details (' +
    '   title,   description, abbreviation,  ot,  nt,  strong,  version,' +
    '   versiondate,  publishdate,  publisher,  author,  creator,  source,' +
    '   language,  righttoleft' +
    ') VALUES (' +
    '  :title, :description, :abbreviation, :ot, :nt, :strong, :version,' +
    '  :versiondate, :publishdate, :publisher, :author, :creator, :source,' +
    '  :language, :righttoleft' +
    ')';

  FInsert.Prepare;
  FInsert.Params.ParamByName('title'       ).AsString  := propMap['title'];
  FInsert.Params.ParamByName('description' ).AsString  := propMap['description'];
  FInsert.Params.ParamByName('abbreviation').AsString  := propMap['short.title'];
  FInsert.Params.ParamByName('ot'          ).AsBoolean := FOT;
  FInsert.Params.ParamByName('nt'          ).AsBoolean := FNT;
  FInsert.Params.ParamByName('strong'      ).AsBoolean := FStrongs;
  FInsert.Params.ParamByName('version'     ).AsString  := Format('%s.%s', [propMap['version.major'], propMap['version.minor']]);
  FInsert.Params.ParamByName('versiondate' ).AsString  := propMap['version.date'];
  FInsert.Params.ParamByName('publishdate' ).AsString  := propMap['publish.date'];
  FInsert.Params.ParamByName('publisher'   ).AsString  := propMap['publisher'];
  FInsert.Params.ParamByName('author'      ).AsString  := propMap['author'];
  FInsert.Params.ParamByName('creator'     ).AsString  := propMap['creator'];
  FInsert.Params.ParamByName('source'      ).AsString  := propMap['source'];
  FInsert.Params.ParamByName('language'    ).AsString  := propMap['lang'];
  FInsert.Params.ParamByName('righttoleft' ).AsString  := propMap['rtl'];

  FInsert.ExecSQL;
  FInsert.Free;

  propMap.Free;
end;

constructor TMySwordModule.Create(filename: string; ot: boolean; nt: boolean;
  strongs: boolean; props: string);
var
  existed: boolean;
begin
  FOT := ot;
  FNT := nt;
  FStrongs := strongs;
  existed  := FileExists(filename);

  FConnection := TSQLite3Connection.Create(nil);
  FConnection.DatabaseName := filename;
  FConnection.Transaction := TSQLTransaction.Create(nil);
  FConnection.StartTransaction;

  try
    FConnection.Open;
    FConnection.Transaction.Active := true;

    if existed then
    begin
      FConnection.ExecuteDirect('DELETE FROM Bible');
      FConnection.ExecuteDirect('DELETE FROM Details');
    end
    else
    begin
      FConnection.ExecuteDirect('CREATE TABLE "Bible" ("Book" INT,"Chapter" INT,"Verse" INT,"Scripture" TEXT)');
      FConnection.ExecuteDirect('CREATE UNIQUE INDEX "bible_key" ON "Bible" ("Book" ASC, "Chapter" ASC, "Verse" ASC)');
      FConnection.ExecuteDirect('CREATE TABLE "Details" ("Title" NVARCHAR(255), "Description" TEXT, "Abbreviation" NVARCHAR(50), "Comments" TEXT, "Version" TEXT, "VersionDate" DATETIME, "PublishDate" DATETIME, "Publisher" TEXT, "Author" TEXT, "Creator" TEXT, "Source" TEXT, "EditorialComments" TEXT, "Language" NVARCHAR(3), "RightToLeft" BOOL, "OT" BOOL, "NT" BOOL, "Strong" BOOL, "VerseRules" TEXT)');
    end;

    SetProperties(props);
    FConnection.Transaction.Commit;

    FInsertVerse := TSQLQuery.Create(nil);
    FInsertVerse.DataBase := FConnection;
    FInsertVerse.SQL.Text := 'INSERT INTO Bible VALUES (:book, :chapter, :verse, :text)';
    FInsertVerse.Prepare;
  except
    raise Exception.Create(SCreateMySwordBibleError);
  end;
end;

destructor TMySwordModule.Destroy;
begin
  FInsertVerse.Free;
  FConnection.Transaction.Free;
  FConnection.Close;
  FConnection.Free;
end;

procedure TMySwordModule.AddVerse(text: string; book, chapter, verse: integer);
begin
  FInsertVerse.Params.ParamByName('book'   ).AsInteger := book;
  FInsertVerse.Params.ParamByName('chapter').AsInteger := chapter;
  FInsertVerse.Params.ParamByName('verse'  ).AsInteger := verse;
  FInsertVerse.Params.ParamByName('text'   ).AsString  := text;
  FInsertVerse.ExecSQL;
end;

procedure TMySwordModule.Commit;
begin
  FConnection.Transaction.Commit;
end;

end.

