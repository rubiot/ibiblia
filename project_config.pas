unit project_config;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, project_persistence;

const
  PROJECT_CONFIG_KEY_MARKER = 'marcador';
  PROJECT_CONFIG_KEY_CHAPTER_VIEW_TEXT = 'chapterview.text';
  PROJECT_CONFIG_KEY_CHAPTER_VIEW_MODE = 'chapterview.mode';
  PROJECT_CONFIG_KEY_INTERLINEAR_MODE = 'interlinear.mode';
  PROJECT_CONFIG_KEY_PARAGRAPH_MODE = 'paragraph.mode';
  PROJECT_CONFIG_KEY_TOKENIZER_VERSION = 'tokenizer_version';
  PROJECT_CONFIG_KEY_INTERLINEAR_VERSE_RULES = 'propriedades.interlinearview.verserules';
  PROJECT_CONFIG_KEY_INTRALINEAR_VERSE_RULES = 'propriedades.intralinearview.verserules';

type
  TProjectConfig = class
  private
    FPersistence: TProjectPersistence;
    procedure RequirePersistence;
  public
    constructor Create(APersistence: TProjectPersistence);
    procedure Attach(APersistence: TProjectPersistence);
    class function TextDescriptionKey(textIndex: Integer): string; static;
    function ReadString(const key: string; const defaultValue: string = ''): string;
    procedure WriteString(const key, value: string);
    function ReadInteger(const key: string; defaultValue: Integer): Integer;
    procedure WriteInteger(const key: string; value: Integer);
    function ReadBoolean(const key: string; defaultValue: Boolean = False): Boolean;
    procedure WriteBoolean(const key: string; value: Boolean);
    function ReadMarker(const defaultValue: string = ''): string;
    procedure WriteMarker(const value: string);
    function ReadChapterViewText(defaultValue: Integer): Integer;
    procedure WriteChapterViewText(value: Integer);
    function ReadChapterViewMode(defaultValue: Integer): Integer;
    procedure WriteChapterViewMode(value: Integer);
    function ReadInterlinearMode(defaultValue: Integer): Integer;
    procedure WriteInterlinearMode(value: Integer);
    function ReadParagraphMode(defaultValue: Integer): Integer;
    procedure WriteParagraphMode(value: Integer);
    function ReadTokenizerVersion(defaultValue: Integer): Integer;
    procedure WriteTokenizerVersion(value: Integer);
    function ReadTextDescription(textIndex: Integer; const defaultValue: string = ''): string;
    procedure WriteTextDescription(textIndex: Integer; const value: string);
    function ReadVerseRules(interlinear: Boolean): string;
  end;

implementation

procedure TProjectConfig.RequirePersistence;
begin
  if not Assigned(FPersistence) then
    raise Exception.Create('Project persistence is not attached');
end;

constructor TProjectConfig.Create(APersistence: TProjectPersistence);
begin
  inherited Create;
  FPersistence := APersistence;
end;

procedure TProjectConfig.Attach(APersistence: TProjectPersistence);
begin
  FPersistence := APersistence;
end;

class function TProjectConfig.TextDescriptionKey(textIndex: Integer): string;
begin
  Result := Format('description.text%d', [textIndex]);
end;

function TProjectConfig.ReadString(const key: string; const defaultValue: string): string;
begin
  if not Assigned(FPersistence) then
    Exit(defaultValue);

  Result := FPersistence.ReadInfo(key);
  if Result.IsEmpty then
    Result := defaultValue;
end;

procedure TProjectConfig.WriteString(const key, value: string);
begin
  RequirePersistence;
  if not FPersistence.UpdateInfo(key, value) then
    FPersistence.InsertInfo(key, value);
end;

function TProjectConfig.ReadInteger(const key: string; defaultValue: Integer): Integer;
begin
  Result := StrToIntDef(ReadString(key), defaultValue);
end;

procedure TProjectConfig.WriteInteger(const key: string; value: Integer);
begin
  WriteString(key, IntToStr(value));
end;

function TProjectConfig.ReadBoolean(const key: string; defaultValue: Boolean): Boolean;
var
  value: string;
begin
  value := Trim(LowerCase(ReadString(key)));
  if value.IsEmpty then
    Exit(defaultValue);

  Result := (value = '1') or (value = 'true') or (value = 'yes');
end;

procedure TProjectConfig.WriteBoolean(const key: string; value: Boolean);
begin
  if value then
    WriteString(key, '1')
  else
    WriteString(key, '0');
end;

function TProjectConfig.ReadMarker(const defaultValue: string): string;
begin
  Result := ReadString(PROJECT_CONFIG_KEY_MARKER, defaultValue);
end;

procedure TProjectConfig.WriteMarker(const value: string);
begin
  WriteString(PROJECT_CONFIG_KEY_MARKER, value);
end;

function TProjectConfig.ReadChapterViewText(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_TEXT, defaultValue);
end;

procedure TProjectConfig.WriteChapterViewText(value: Integer);
begin
  WriteInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_TEXT, value);
end;

function TProjectConfig.ReadChapterViewMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_MODE, defaultValue);
end;

procedure TProjectConfig.WriteChapterViewMode(value: Integer);
begin
  WriteInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_MODE, value);
end;

function TProjectConfig.ReadInterlinearMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_INTERLINEAR_MODE, defaultValue);
end;

procedure TProjectConfig.WriteInterlinearMode(value: Integer);
begin
  WriteInteger(PROJECT_CONFIG_KEY_INTERLINEAR_MODE, value);
end;

function TProjectConfig.ReadParagraphMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_PARAGRAPH_MODE, defaultValue);
end;

procedure TProjectConfig.WriteParagraphMode(value: Integer);
begin
  WriteInteger(PROJECT_CONFIG_KEY_PARAGRAPH_MODE, value);
end;

function TProjectConfig.ReadTokenizerVersion(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_TOKENIZER_VERSION, defaultValue);
end;

procedure TProjectConfig.WriteTokenizerVersion(value: Integer);
begin
  WriteInteger(PROJECT_CONFIG_KEY_TOKENIZER_VERSION, value);
end;

function TProjectConfig.ReadTextDescription(textIndex: Integer; const defaultValue: string): string;
begin
  Result := ReadString(TextDescriptionKey(textIndex), defaultValue);
end;

procedure TProjectConfig.WriteTextDescription(textIndex: Integer; const value: string);
begin
  WriteString(TextDescriptionKey(textIndex), value);
end;

function TProjectConfig.ReadVerseRules(interlinear: Boolean): string;
begin
  if interlinear then
    Result := ReadString(PROJECT_CONFIG_KEY_INTERLINEAR_VERSE_RULES)
  else
    Result := ReadString(PROJECT_CONFIG_KEY_INTRALINEAR_VERSE_RULES);
end;

end.
