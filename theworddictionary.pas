unit TheWordDictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sqlite3conn, sqldb, PCRE, gmap, gutil;

function PublishStagedFile(const StagedFile, TargetFile: string): Boolean;

type

{ TTheWordDictionary }

TTheWordDictionary = class
  private
    FAbbrev: string;
    FAbout: string;
    FAuthor: string;
    FTargetFileName: string;
    FStagingFileName: string;
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
    function CreateTopic(topic: string; lemma: string): integer;
    procedure CreateContent(topic_id: integer; content: string);
    function GetCurrentTopicID: integer;
    function GetNextTopicID: integer;
  public
    constructor Create(filename: string; strongs: boolean);
    destructor Destroy; override;
    procedure AddEntry(topic: string; lemma: string; content: string);
    procedure Commit;

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
  SCreateTheWordDictionaryError = 'Unable to create new TheWord module file';

implementation

function PublishStagedFile(const StagedFile, TargetFile: string): Boolean;
var
  srcStream, dstStream, backupStream: TMemoryStream;
  fsOut: TFileStream;
begin
  Result := False;
  if not FileExists(StagedFile) then
    Exit;

  // 1. Fast path: if TargetFile does not exist or can be removed and renamed atomically
  if not FileExists(TargetFile) then
  begin
    if RenameFile(StagedFile, TargetFile) then
    begin
      Result := True;
      Exit;
    end;
  end
  else if SysUtils.DeleteFile(TargetFile) then
  begin
    if RenameFile(StagedFile, TargetFile) then
    begin
      Result := True;
      Exit;
    end;
  end;

  // 2. Fallback for modules currently open with a shared read handle in theWord (v1.1.0 commit 3700ab1):
  //    Back up existing target bytes in memory, overwrite in-place via shared TFileStream, and clean up staging file.
  srcStream := TMemoryStream.Create;
  backupStream := TMemoryStream.Create;
  try
    srcStream.LoadFromFile(StagedFile);
    if FileExists(TargetFile) then
    begin
      dstStream := TMemoryStream.Create;
      try
        try
          dstStream.LoadFromFile(TargetFile);
          backupStream.CopyFrom(dstStream, 0);
        except
          // Target could not be pre-read; continue with stream overwrite attempt
        end;
      finally
        dstStream.Free;
      end;
    end;

    if FileExists(TargetFile) then
      fsOut := TFileStream.Create(TargetFile, fmOpenReadWrite or fmShareDenyNone)
    else
      fsOut := TFileStream.Create(TargetFile, fmCreate or fmOpenWrite or fmShareDenyNone);
    try
      try
        fsOut.Size := 0;
        fsOut.Position := 0;
        if srcStream.Size > 0 then
        begin
          srcStream.Position := 0;
          fsOut.CopyFrom(srcStream, srcStream.Size);
        end;
        Result := True;
      except
        on E: Exception do
        begin
          if backupStream.Size > 0 then
          begin
            try
              fsOut.Size := 0;
              fsOut.Position := 0;
              backupStream.Position := 0;
              fsOut.CopyFrom(backupStream, backupStream.Size);
            except
            end;
          end;
          raise;
        end;
      end;
    finally
      fsOut.Free;
    end;
  finally
    srcStream.Free;
    backupStream.Free;
    if FileExists(StagedFile) then
      SysUtils.DeleteFile(StagedFile);
  end;
end;

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
  FConnection.ExecuteDirect(Format('DELETE FROM config WHERE name = %s', [QuotedStr(name)]));
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

function TTheWordDictionary.CreateTopic(topic: string; lemma: string): integer;
begin
  FInsertTopic.Params.ParamByName('id').AsInteger := GetNextTopicID;
  FInsertTopic.Params.ParamByName('subject').AsString := topic;
  FInsertTopic.Params.ParamByName('order').AsInteger := GetCurrentTopicID;
  FInsertTopic.Params.ParamByName('lemma').AsString := lemma;
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

constructor TTheWordDictionary.Create(filename: string; strongs: boolean);
begin
  FTargetFileName := filename;
  FStagingFileName := filename + '.tmp_staging';
  if FileExists(FStagingFileName) then
    SysUtils.DeleteFile(FStagingFileName);

  FConnection := TSQLite3Connection.Create(nil);
  FConnection.DatabaseName := FStagingFileName;
  FConnection.Transaction := TSQLTransaction.Create(nil);
  FConnection.StartTransaction;

  try
    FConnection.Open;
    FConnection.Transaction.Active := true;

    // Schema conforming to the 2026 theWord Book Module Developer Guide (#quick-build, #topics, #content, #config)
    FConnection.ExecuteDirect('CREATE TABLE config(name text, value text)');
    FConnection.ExecuteDirect('CREATE TABLE content(topic_id integer primary key, data blob, data2 blob)');
    FConnection.ExecuteDirect('CREATE TABLE topics(id integer primary key, pid integer default 0, subject text, rel_order integer, content_type text, strong_orig_word text)');
    FConnection.ExecuteDirect('CREATE INDEX idx_topics_subject on topics(subject)');
    FConnection.ExecuteDirect('CREATE INDEX idx_topics_strong_orig_word on topics(strong_orig_word)');

    FInsertConfig  := CreateQuery('INSERT INTO config VALUES (:name, :value)');
    FInsertContent := CreateQuery('INSERT INTO content VALUES (:topic_id, CAST(:data AS BLOB), NULL)');
    FInsertTopic   := CreateQuery('INSERT INTO topics VALUES (:id, 0, :subject, :order, ''rtf'', :lemma)');

    SetProperty('type', '1');
    SetProperty('schema.version', '1');
    SetProperty('content.type', 'rtf');
    SetProperty('requires', '4.0.0.0');
    SetProperty('version.major', '1');
    SetProperty('version.minor', '0');
    SetProperty('isbn', 'No ISBN');
    SetProperty('keywords', 'Strongs');
    SetProperty('strong', IfThen(strongs, '1', '0'));
  except
    raise Exception.Create(SCreateTheWordDictionaryError);
  end;

  FTopicID := 0;
end;

destructor TTheWordDictionary.Destroy;
var
  hasTarget: Boolean;
begin
  hasTarget := FTargetFileName <> '';
  if Assigned(FInsertConfig) then FInsertConfig.Free;
  if Assigned(FInsertContent) then FInsertContent.Free;
  if Assigned(FInsertTopic) then FInsertTopic.Free;
  if Assigned(FConnection) then
  begin
    if Assigned(FConnection.Transaction) then
      FConnection.Transaction.Free;
    if FConnection.Connected then
      FConnection.Close;
    FConnection.Free;
  end;

  if hasTarget and FileExists(FStagingFileName) then
    PublishStagedFile(FStagingFileName, FTargetFileName);

  inherited Destroy;
end;

procedure TTheWordDictionary.AddEntry(topic: string; lemma: string;
  content: string);
begin
  if content.IsEmpty then
    exit;

  CreateContent(CreateTopic(topic, lemma), content);
end;

procedure TTheWordDictionary.Commit;
begin
  FConnection.Transaction.Commit;
end;

end.
