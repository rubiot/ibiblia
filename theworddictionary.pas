unit TheWordDictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sqlite3conn, sqldb, PCRE, gmap, gutil;

type

TStrLess = specialize TLess<string>;
TLemmaMap = specialize TMap<string, string, TStrLess>;

{ TTheWordDictionary }

TTheWordDictionary = class
  private
    FAbbrev: string;
    FAbout: string;
    FAuthor: string;
    FConnection: TSQLite3Connection;
    FDescription: string;
    FEnglishDescription: string;
    FID: string;
    FTitleEnglish: string;
    FTopicID: integer;
    FInsertConfig: TSQLQuery;
    FInsertTopic: TSQLQuery;
    FInsertContent: TSQLQuery;
    FLanguage: string;
    FMorph: boolean;
    FStrong: boolean;
    FTitle: string;
    FVersionDate: string;
    FVersionMajor: string;
    FVersionMinor: string;
    FLemmaMap: TLemmaMap;

    function CreateQuery(statement: string): TSQLQuery;
    procedure SetAbbrev(AValue: string);
    procedure SetAbout(AValue: string);
    procedure SetAuthor(AValue: string);
    procedure SetDescription(AValue: string);
    procedure SetEnglishDescription(AValue: string);
    procedure SetLanguage(AValue: string);
    procedure SetStrong(AValue: boolean);
    procedure SetID(AValue: string);
    procedure SetMorph(AValue: boolean);
    procedure SetProperty(name: string; value: string);
    procedure SetTitle(AValue: string);
    procedure SetTitleEnglish(AValue: string);
    procedure SetVersionDate(AValue: string);
    procedure SetVersionMajor(AValue: string);
    procedure SetVersionMinor(AValue: string);
    function CreateTopic(topic: string): integer;
    procedure CreateContent(topic_id: integer; content: string);
    function GetCurrentTopicID: integer;
    function GetNextTopicID: integer;
    procedure InitLemmas;
  public
    constructor Create(filename: string; strongs: boolean);
    destructor Destroy; override;
    procedure AddEntry(topic: string; content: string);
    procedure Commit;
    function FindLemma(strong: string): string;

    property Abbrev: string read FAbbrev write SetAbbrev;
    property Title: string read FTitle write SetTitle;
    property TitleEnglish: string read FTitleEnglish write SetTitleEnglish;
    property Description: string read FDescription write SetDescription;
    property EnglishDescription: string read FEnglishDescription write SetEnglishDescription;
    property About: string read FAbout write SetAbout;
    property ID: string read FID write SetID;
    property Strong: boolean read FStrong write SetStrong;
    property Morph: boolean read FMorph write SetMorph;
    property VersionMajor: string read FVersionMajor write SetVersionMajor;
    property VersionMinor: string read FVersionMinor write SetVersionMinor;
    property VersionDate: string read FVersionDate write SetVersionDate;
    property Author: string read FAuthor write SetAuthor;
    property Language: string read FLanguage write SetLanguage;
end;

resourcestring
  SProjectOverrideError = 'A project with this name already exists. Delete it first or choose another name';
  SOverrideError = 'A dictionary module exists with this name and cannot be overwritten';
  SCreateTheWordDictionaryError = 'Unable to create new TheWord module file';

implementation

{ TTheWordDictionary }

function TTheWordDictionary.CreateQuery(statement: string): TSQLQuery;
begin
  result := TSQLQuery.Create(nil);
  result.DataBase := FConnection;
  result.SQL.Text := statement;
  result.Prepare;
end;

procedure TTheWordDictionary.SetAbbrev(AValue: string);
begin
  if FAbbrev=AValue then Exit;
  FAbbrev:=AValue;
  SetProperty('abbrev', AValue);
end;

procedure TTheWordDictionary.SetAbout(AValue: string);
begin
  if FAbout=AValue then Exit;
  FAbout:=AValue;
  SetProperty('about', AValue);
end;

procedure TTheWordDictionary.SetAuthor(AValue: string);
begin
  if FAuthor=AValue then Exit;
  FAuthor:=AValue;
  SetProperty('author', AValue);
end;

procedure TTheWordDictionary.SetDescription(AValue: string);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
  SetProperty('description', AValue);
end;

procedure TTheWordDictionary.SetEnglishDescription(AValue: string);
begin
  if FEnglishDescription=AValue then Exit;
  FEnglishDescription:=AValue;
  SetProperty('description.english', AValue);
end;

procedure TTheWordDictionary.SetLanguage(AValue: string);
begin
  if FLanguage=AValue then Exit;
  FLanguage:=AValue;
  SetProperty('lang', AValue);
end;

procedure TTheWordDictionary.SetStrong(AValue: boolean);
begin
  if FStrong=AValue then Exit;
  FStrong:=AValue;
  SetProperty('strong', IfThen(AValue, '1', '0'));
end;

procedure TTheWordDictionary.SetID(AValue: string);
begin
  if FID=AValue then Exit;
  FID:=AValue;
  SetProperty('id', AValue);
end;

procedure TTheWordDictionary.SetMorph(AValue: boolean);
begin
  if FMorph=AValue then Exit;
  FMorph:=AValue;
  SetProperty('morph', IfThen(AValue, '1', '0'));
end;

procedure TTheWordDictionary.SetProperty(name: string; value: string);
begin
  FInsertConfig.Params.ParamByName('name').AsString := name;
  FInsertConfig.Params.ParamByName('value').AsString := value;
  FInsertConfig.ExecSQL;
end;

procedure TTheWordDictionary.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  SetProperty('title', AValue);
end;

procedure TTheWordDictionary.SetTitleEnglish(AValue: string);
begin
  if FTitleEnglish=AValue then Exit;
  FTitleEnglish:=AValue;
  SetProperty('title.english', AValue);
end;

procedure TTheWordDictionary.SetVersionDate(AValue: string);
begin
  if FVersionDate=AValue then Exit;
  FVersionDate:=AValue;
  SetProperty('version.date', AValue);
end;

procedure TTheWordDictionary.SetVersionMajor(AValue: string);
begin
  if FVersionMajor=AValue then Exit;
  FVersionMajor:=AValue;
  SetProperty('version.major', AValue);
end;

procedure TTheWordDictionary.SetVersionMinor(AValue: string);
begin
  if FVersionMinor=AValue then Exit;
  FVersionMinor:=AValue;
  SetProperty('version.minor', AValue);
end;

function TTheWordDictionary.CreateTopic(topic: string): integer;
begin
  FInsertTopic.Params.ParamByName('id').AsInteger := GetNextTopicID;
  FInsertTopic.Params.ParamByName('subject').AsString := topic;
  FInsertTopic.Params.ParamByName('order').AsInteger := GetCurrentTopicID;
  FInsertTopic.Params.ParamByName('lemma').AsString := FindLemma(topic);
  FInsertTopic.ExecSQL;

  result := GetCurrentTopicID;
end;

procedure TTheWordDictionary.CreateContent(topic_id: integer; content: string);
begin
  FInsertContent.Params.ParamByName('topic_id').AsInteger := topic_id;
  FInsertContent.Params.ParamByName('data').AsString := content;
  FInsertContent.ExecSQL;
end;

function TTheWordDictionary.GetCurrentTopicID: integer;
begin
  result := FTopicID;
end;

function TTheWordDictionary.GetNextTopicID: integer;
begin
  inc(FTopicID);
  result := FTopicID;
end;

function TTheWordDictionary.FindLemma(strong: string): string;
begin
  result := '';
  if assigned(FLemmaMap) then
    FLemmaMap.TryGetValue(strong, result);
end;

procedure TTheWordDictionary.InitLemmas;
var
  lines: TStringList;
  line: string;
  rx: IRegex;
  match: IMatch;
begin
  FLemmaMap := TLemmaMap.Create;
  lines := TStringList.Create;
  lines.LoadFromFile('lemmas.dat');
  rx := RegexCreate('^(.*?),(.*?)$', [rcoUTF8]);
  for line in lines do
  begin
    match := rx.Match(line);
    if match.Success then
      FLemmaMap[match.Groups[1].Value] := match.Groups[2].Value;
  end;
  lines.Free;
end;

constructor TTheWordDictionary.Create(filename: string; strongs: boolean);
begin
  if FileExists(filename) then
  begin
    try
      DeleteFile(filename);
    except
      raise Exception.Create(SOverrideError);
    end;
  end;

  FConnection := TSQLite3Connection.Create(nil);
  FConnection.DatabaseName := filename;
  FConnection.Transaction := TSQLTransaction.Create(nil);
  FConnection.StartTransaction;

  try
    FConnection.Open;
    FConnection.Transaction.Active := true;

    FConnection.ExecuteDirect('CREATE TABLE config(name text, value text)');
    FConnection.ExecuteDirect('CREATE TABLE content(topic_id integer primary key, data text, data2 blob)');
    FConnection.ExecuteDirect('CREATE TABLE topics(id integer primary key, pid integer default 0, subject text, rel_order, content_type text, strong_orig_word text)');
    FConnection.ExecuteDirect('CREATE INDEX idx_topics_subject on topics(subject)');

    FInsertConfig  := CreateQuery('INSERT INTO config  VALUES (:name, :value)');
    FInsertContent := CreateQuery('INSERT INTO content VALUES (:topic_id, :data, NULL)');
    FInsertTopic   := CreateQuery('INSERT INTO topics  VALUES (:id, 0, :subject, :order, NULL, :lemma)');

    SetProperty('keywords', 'Strongs');
    SetProperty('type', '1');
    SetProperty('content.type', 'rtf');
    SetProperty('schema.version', '1');
    SetProperty('strong', '1');
  except
    raise Exception.Create(SCreateTheWordDictionaryError);
  end;

  if strongs then
    InitLemmas
  else
    FLemmaMap := nil;

  FTopicID := 0;
end;

destructor TTheWordDictionary.Destroy;
begin
  inherited Destroy;

  if assigned(FLemmaMap) then
    FLemmaMap.Free;

  FInsertConfig.Free;
  FInsertContent.Free;
  FInsertTopic.Free;
  FConnection.Transaction.Free;
  FConnection.Close;
  FConnection.Free;
end;

procedure TTheWordDictionary.AddEntry(topic: string; content: string);
begin
  CreateContent(CreateTopic(topic), content);
end;

procedure TTheWordDictionary.Commit;
begin
  FConnection.Transaction.Commit;
end;

end.

