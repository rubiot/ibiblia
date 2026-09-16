unit project_config;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, project_persistence, text_direction, ai_credentials;

const
  PROJECT_CONFIG_KEY_MARKER = 'marcador';
  PROJECT_CONFIG_KEY_CHAPTER_VIEW_TEXT = 'chapterview.text';
  PROJECT_CONFIG_KEY_CHAPTER_VIEW_MODE = 'chapterview.mode';
  PROJECT_CONFIG_KEY_INTERLINEAR_MODE = 'interlinear.mode';
  PROJECT_CONFIG_KEY_PARAGRAPH_MODE = 'paragraph.mode';
  PROJECT_CONFIG_KEY_TOKENIZER_VERSION = 'tokenizer_version';
  PROJECT_CONFIG_KEY_INTERLINEAR_VERSE_RULES = 'propriedades.interlinearview.verserules';
  PROJECT_CONFIG_KEY_INTRALINEAR_VERSE_RULES = 'propriedades.intralinearview.verserules';
  PROJECT_CONFIG_KEY_SUGGESTION_MODE = 'workflow.suggestion_mode';
  PROJECT_CONFIG_KEY_AUTOMATIC_SUGGESTION = 'workflow.automatic_suggestion';
  PROJECT_CONFIG_KEY_DEFAULT_VERSE_MODE = 'workflow.default_verse_mode';

type
  TProjectConfig = class
  private
    FPersistence: TProjectPersistence;
    FAICredentials: TAIProjectCredentials;
    procedure RequirePersistence;
  public
    constructor Create(APersistence: TProjectPersistence);
    destructor Destroy; override;
    property AICredentials: TAIProjectCredentials read FAICredentials;
    procedure Attach(APersistence: TProjectPersistence);
    class function TextDescriptionKey(textIndex: Integer): string; static;
    class function TextDirectionKey(textIndex: Integer): string; static;
    function HasKey(const key: string): Boolean;
    function ReadString(const key: string; const defaultValue: string = ''): string;
    procedure WriteString(const key, value: string);
    procedure WriteImmediateString(const key, value: string);
    function ReadInteger(const key: string; defaultValue: Integer): Integer;
    procedure WriteInteger(const key: string; value: Integer);
    procedure WriteImmediateInteger(const key: string; value: Integer);
    function ReadBoolean(const key: string; defaultValue: Boolean = False): Boolean;
    procedure WriteBoolean(const key: string; value: Boolean);
    procedure WriteImmediateBoolean(const key: string; value: Boolean);
    function ReadMarker(const defaultValue: string = ''): string;
    procedure WriteMarker(const value: string);
    procedure StageMarkerForCommit(const value: string);
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
    procedure StageTextDescription(textIndex: Integer; const value: string);
    function HasTextDirection(textIndex: Integer): Boolean;
    function ReadTextDirection(textIndex: Integer;
      defaultValue: TTextDirection = tdLeftToRight): TTextDirection;
    procedure WriteTextDirection(textIndex: Integer; value: TTextDirection);
    function ReadVerseRules(interlinear: Boolean): string;
    function HasSuggestionMode: Boolean;
    function ReadSuggestionMode(defaultValue: Integer): Integer;
    procedure WriteSuggestionMode(value: Integer);
    function HasAutomaticSuggestion: Boolean;
    function ReadAutomaticSuggestion(defaultValue: Boolean): Boolean;
    procedure WriteAutomaticSuggestion(value: Boolean);
    function HasDefaultVerseMode: Boolean;
    function ReadDefaultVerseMode(defaultValue: Integer): Integer;
    procedure WriteDefaultVerseMode(value: Integer);
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
  FAICredentials := TAIProjectCredentials.Create(APersistence);
end;

destructor TProjectConfig.Destroy;
begin
  FAICredentials.Free;
  inherited Destroy;
end;

procedure TProjectConfig.Attach(APersistence: TProjectPersistence);
begin
  FPersistence := APersistence;
  FAICredentials.Attach(APersistence);
end;

class function TProjectConfig.TextDescriptionKey(textIndex: Integer): string;
begin
  Result := Format('description.text%d', [textIndex]);
end;

class function TProjectConfig.TextDirectionKey(textIndex: Integer): string;
begin
  Result := Format('direction.text%d', [textIndex]);
end;

function TProjectConfig.HasKey(const key: string): Boolean;
begin
  Result := Assigned(FPersistence) and
    not FPersistence.ReadInfo(key).IsEmpty;
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
  FPersistence.StageInfo(key, value);
end;

procedure TProjectConfig.WriteImmediateString(const key, value: string);
begin
  RequirePersistence;
  FPersistence.WriteInfoImmediate(key, value);
end;

function TProjectConfig.ReadInteger(const key: string; defaultValue: Integer): Integer;
begin
  Result := StrToIntDef(ReadString(key), defaultValue);
end;

procedure TProjectConfig.WriteInteger(const key: string; value: Integer);
begin
  WriteString(key, IntToStr(value));
end;

procedure TProjectConfig.WriteImmediateInteger(const key: string;
  value: Integer);
begin
  WriteImmediateString(key, IntToStr(value));
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

procedure TProjectConfig.WriteImmediateBoolean(const key: string;
  value: Boolean);
begin
  if value then
    WriteImmediateString(key, '1')
  else
    WriteImmediateString(key, '0');
end;

function TProjectConfig.ReadMarker(const defaultValue: string): string;
begin
  Result := ReadString(PROJECT_CONFIG_KEY_MARKER, defaultValue);
end;

procedure TProjectConfig.WriteMarker(const value: string);
begin
  WriteImmediateString(PROJECT_CONFIG_KEY_MARKER, value);
end;

procedure TProjectConfig.StageMarkerForCommit(const value: string);
begin
  FPersistence.StageInfoSilently(PROJECT_CONFIG_KEY_MARKER, value);
end;

function TProjectConfig.ReadChapterViewText(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_TEXT, defaultValue);
end;

procedure TProjectConfig.WriteChapterViewText(value: Integer);
begin
  WriteImmediateInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_TEXT, value);
end;

function TProjectConfig.ReadChapterViewMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_MODE, defaultValue);
end;

procedure TProjectConfig.WriteChapterViewMode(value: Integer);
begin
  WriteImmediateInteger(PROJECT_CONFIG_KEY_CHAPTER_VIEW_MODE, value);
end;

function TProjectConfig.ReadInterlinearMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_INTERLINEAR_MODE, defaultValue);
end;

procedure TProjectConfig.WriteInterlinearMode(value: Integer);
begin
  WriteImmediateInteger(PROJECT_CONFIG_KEY_INTERLINEAR_MODE, value);
end;

function TProjectConfig.ReadParagraphMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_PARAGRAPH_MODE, defaultValue);
end;

procedure TProjectConfig.WriteParagraphMode(value: Integer);
begin
  WriteImmediateInteger(PROJECT_CONFIG_KEY_PARAGRAPH_MODE, value);
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
  WriteImmediateString(TextDescriptionKey(textIndex), value);
end;

procedure TProjectConfig.StageTextDescription(textIndex: Integer;
  const value: string);
begin
  WriteString(TextDescriptionKey(textIndex), value);
end;

function TProjectConfig.HasTextDirection(textIndex: Integer): Boolean;
begin
  Result := not ReadString(TextDirectionKey(textIndex)).IsEmpty;
end;

function TProjectConfig.ReadTextDirection(textIndex: Integer;
  defaultValue: TTextDirection): TTextDirection;
begin
  Result := TextDirectionFromConfigValue(
    ReadString(TextDirectionKey(textIndex)), defaultValue);
end;

procedure TProjectConfig.WriteTextDirection(textIndex: Integer;
  value: TTextDirection);
begin
  WriteString(TextDirectionKey(textIndex),
    TextDirectionToConfigValue(value));
end;

function TProjectConfig.ReadVerseRules(interlinear: Boolean): string;
begin
  if interlinear then
    Result := ReadString(PROJECT_CONFIG_KEY_INTERLINEAR_VERSE_RULES)
  else
    Result := ReadString(PROJECT_CONFIG_KEY_INTRALINEAR_VERSE_RULES);
end;

function TProjectConfig.HasSuggestionMode: Boolean;
begin
  Result := HasKey(PROJECT_CONFIG_KEY_SUGGESTION_MODE);
end;

function TProjectConfig.ReadSuggestionMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_SUGGESTION_MODE, defaultValue);
end;

procedure TProjectConfig.WriteSuggestionMode(value: Integer);
begin
  WriteImmediateInteger(PROJECT_CONFIG_KEY_SUGGESTION_MODE, value);
end;

function TProjectConfig.HasAutomaticSuggestion: Boolean;
begin
  Result := HasKey(PROJECT_CONFIG_KEY_AUTOMATIC_SUGGESTION);
end;

function TProjectConfig.ReadAutomaticSuggestion(defaultValue: Boolean): Boolean;
begin
  Result := ReadBoolean(PROJECT_CONFIG_KEY_AUTOMATIC_SUGGESTION,
    defaultValue);
end;

procedure TProjectConfig.WriteAutomaticSuggestion(value: Boolean);
begin
  WriteImmediateBoolean(PROJECT_CONFIG_KEY_AUTOMATIC_SUGGESTION, value);
end;

function TProjectConfig.HasDefaultVerseMode: Boolean;
begin
  Result := HasKey(PROJECT_CONFIG_KEY_DEFAULT_VERSE_MODE);
end;

function TProjectConfig.ReadDefaultVerseMode(defaultValue: Integer): Integer;
begin
  Result := ReadInteger(PROJECT_CONFIG_KEY_DEFAULT_VERSE_MODE, defaultValue);
end;

procedure TProjectConfig.WriteDefaultVerseMode(value: Integer);
begin
  WriteImmediateInteger(PROJECT_CONFIG_KEY_DEFAULT_VERSE_MODE, value);
end;

end.
