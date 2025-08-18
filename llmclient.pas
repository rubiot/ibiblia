unit llmclient;

{$mode objfpc}{$H+}

// This unit provides an interface to LLMs (OpenAI/DeepSeek/Google) for suggesting associations between words in two verses.

interface

uses
  Classes, SysUtils, fpjson;

type
  { Enum for LLM backends }
  TLLMBackend = (lbOpenAI, lbDeepSeek, lbGoogle);

  TLLMModelInfo = record
    Backend: TLLMBackend;
    SupportsStructOutput: Boolean;
  end;

  TLLMMessage = record
    Role: string;
    Content: string;
  end;

  TLLMResponse = record
    Content: string;
    Reasoning: string;
  end;

  { Base class for LLM clients }
  TBaseLLMClient = class
  protected
    FModel: string;
    FApiKey: string;
    // Utility: Perform HTTP POST and return response as string
    function HttpPostJson(const Url, JsonBody: string; const ExtraHeaders: array of string): string; virtual; abstract;
  public
    constructor Create(const AModel, AApiKey: string);
    function AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse; virtual; abstract;
  end;

  { Base class for OpenAI-compatible backends }
  TOpenAICompatibleBackend = class(TBaseLLMClient)
  protected
    function GetCompletionsUrl: string; virtual; abstract;
    procedure ParseCompletionsResponse(const JsonResponse: TJSONObject; out Response: TLLMResponse); virtual;
  public
    function HttpPostJson(const Url, JsonBody: string; const ExtraHeaders: array of string): string; override;
    function AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse; override;
  end;

  { OpenAI backend implementation }
  TOpenAIBackend = class(TOpenAICompatibleBackend)
  protected
    function GetCompletionsUrl: string; override;
  public
    constructor Create(const AModel, AApiKey: string);
  end;

  { DeepSeek backend implementation }
  TDeepSeekBackend = class(TOpenAICompatibleBackend)
  protected
    function GetCompletionsUrl: string; override;
    procedure ParseCompletionsResponse(const JsonResponse: TJSONObject; out Response: TLLMResponse); override;
  public
    constructor Create(const AModel, AApiKey: string);
  end;

  { Google backend implementation }
  TGoogleBackend = class(TBaseLLMClient)
  public
    constructor Create(const AModel, AApiKey: string);
    function HttpPostJson(const Url, JsonBody: string; const ExtraHeaders: array of string): string; override;
    function AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse; override;
  end;

function CreateBackend(const Model: string; const ApiKey: string): TBaseLLMClient;
function GetModelInfo(const Model: string): TLLMModelInfo;
// Returns the list of supported model names defined in this unit
procedure FillSupportedModelNames(List: TStrings);

// Utility to build a JSON array of messages
function MessagesToJSONArray(const Messages: array of TLLMMessage): TJSONArray;

implementation

uses
  jsonparser, fphttpclient, opensslsockets;

{ TBaseLLMClient }

constructor TBaseLLMClient.Create(const AModel, AApiKey: string);
begin
  FModel := AModel;
  FApiKey := AApiKey;
end;

const
  MODEL_INFO: array[0..9] of record
    Name: string;
    Info: TLLMModelInfo;
  end = (
    (Name: 'gpt-4o-mini';       Info: (Backend: lbOpenAI;   SupportsStructOutput: True)),
    (Name: 'o1';                Info: (Backend: lbOpenAI;   SupportsStructOutput: True)),
    (Name: 'o1-mini';           Info: (Backend: lbOpenAI;   SupportsStructOutput: False)),
    (Name: 'o3-mini';           Info: (Backend: lbOpenAI;   SupportsStructOutput: True)),
    (Name: 'deepseek';          Info: (Backend: lbDeepSeek; SupportsStructOutput: True)),
    (Name: 'deepseek-reasoner'; Info: (Backend: lbDeepSeek; SupportsStructOutput: True)),
    (Name: 'gemini-1.5-flash';  Info: (Backend: lbGoogle;   SupportsStructOutput: True)),
    (Name: 'gemini';            Info: (Backend: lbGoogle;   SupportsStructOutput: True)),
    (Name: 'gemini-1.5';        Info: (Backend: lbGoogle;   SupportsStructOutput: True)),
    (Name: 'gemini-2';          Info: (Backend: lbGoogle;   SupportsStructOutput: True))
  );

procedure FillSupportedModelNames(List: TStrings);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;
  List.BeginUpdate;
  try
    List.Clear;
    for i := Low(MODEL_INFO) to High(MODEL_INFO) do
      List.Add(MODEL_INFO[i].Name);
  finally
    List.EndUpdate;
  end;
end;

function GetModelInfo(const Model: string): TLLMModelInfo;
var
  i: Integer;
  res: TLLMModelInfo;
begin
  for i := Low(MODEL_INFO) to High(MODEL_INFO) do
    if SameText(MODEL_INFO[i].Name, Model) then
      Exit(MODEL_INFO[i].Info);
  // Fallback: try to infer backend by common prefixes and disable structured output by default
  if (Pos('gemini', LowerCase(Model)) = 1) then
  begin
    res.Backend := lbGoogle; res.SupportsStructOutput := False; Exit(res);
  end;
  if (Pos('deepseek', LowerCase(Model)) = 1) then
  begin
    res.Backend := lbDeepSeek; res.SupportsStructOutput := False; Exit(res);
  end;
  // Assume OpenAI-compatible by default (gpt-, o1, o3, etc.)
  Result.Backend := lbOpenAI;
  Result.SupportsStructOutput := False;
end;

function CreateBackend(const Model: string; const ApiKey: string): TBaseLLMClient;
var
  Info: TLLMModelInfo;
begin
  Info := GetModelInfo(Model);
  case Info.Backend of
    lbOpenAI:    Result := TOpenAIBackend.Create(Model, ApiKey);
    lbDeepSeek:  Result := TDeepSeekBackend.Create(Model, ApiKey);
    lbGoogle:    Result := TGoogleBackend.Create(Model, ApiKey);
  else
    raise Exception.Create('Unsupported backend');
  end;
end;

function MessagesToJSONArray(const Messages: array of TLLMMessage): TJSONArray;
var
  arr: TJSONArray;
  i: Integer;
  obj: TJSONObject;
begin
  arr := TJSONArray.Create;
  for i := 0 to High(Messages) do
  begin
    obj := TJSONObject.Create;
    obj.Add('role', Messages[i].Role);
    obj.Add('content', Messages[i].Content);
    arr.Add(obj); // Use Add for FPC compatibility
  end;
  Result := arr;
end;

{ TOpenAICompatibleBackend }

function TOpenAICompatibleBackend.HttpPostJson(const Url, JsonBody: string; const ExtraHeaders: array of string): string;
var
  client: TFPHTTPClient;
  i: Integer;
  stream: TStringStream;
  responseStream: TStringStream;
begin
  client := TFPHTTPClient.Create(nil);
  stream := TStringStream.Create(JsonBody);
  responseStream := TStringStream.Create('');
  try
    client.AddHeader('Content-Type', 'application/json');
    if FApiKey <> '' then
      client.AddHeader('Authorization', 'Bearer ' + FApiKey);
    for i := 0 to High(ExtraHeaders) do
      if ExtraHeaders[i] <> '' then
        client.AddHeader(ExtraHeaders[i], '');
    stream.Position := 0;
    client.RequestBody := stream;
    client.Post(Url, responseStream);
    responseStream.Position := 0;
    Result := responseStream.DataString;
  finally
    stream.Free;
    responseStream.Free;
    client.Free;
  end;
end;

procedure TOpenAICompatibleBackend.ParseCompletionsResponse(const JsonResponse: TJSONObject; out Response: TLLMResponse);
var
  choices: TJSONArray;
  msg: TJSONObject;
begin
  Response.Content := '';
  Response.Reasoning := '';
  if JsonResponse.Find('choices') <> nil then
  begin
    choices := TJSONArray(JsonResponse.Arrays['choices']);
    if (choices.Count > 0) and (choices.Objects[0].Find('message') <> nil) then
    begin
      msg := TJSONObject(choices.Objects[0].Objects['message']);
      Response.Content := msg.Get('content', '');
      Response.Reasoning := ''; // Default reasoning is empty
    end
    else
      Response.Content := JsonResponse.AsString;
  end
  else
    Response.Content := JsonResponse.AsString;
end;

function TOpenAICompatibleBackend.AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse;
var
  url, body, resp: string;
  req, respjson: TJSONObject;
  arr: TJSONArray;
  info: TLLMModelInfo;
begin
  url := GetCompletionsUrl;
  info := GetModelInfo(FModel);
  req := TJSONObject.Create;
  try
    req.Add('model', FModel);
    arr := MessagesToJSONArray(Messages);
    req.Add('messages', arr);
    req.Add('stream', False);
    if info.SupportsStructOutput then
      req.Add('response_format', TJSONObject.Create(['type', 'json_object']));
    body := req.AsJSON;
  finally
    req.Free;
  end;
  try
    resp := HttpPostJson(url, body, []);
    respjson := TJSONObject(GetJSON(resp));
    try
      ParseCompletionsResponse(respjson, Result);
    finally
      respjson.Free;
    end;
  except
    on E: Exception do
    begin
      Result.Content := '{"error": "' + E.Message + '"}';
      Result.Reasoning := '';
    end;
  end;
end;

{ TOpenAIBackend }

constructor TOpenAIBackend.Create(const AModel, AApiKey: string);
begin
  inherited Create(AModel, AApiKey);
end;

function TOpenAIBackend.GetCompletionsUrl: string;
begin
  Result := 'https://api.openai.com/v1/chat/completions';
end;

{ TDeepSeekBackend }

constructor TDeepSeekBackend.Create(const AModel, AApiKey: string);
begin
  inherited Create(AModel, AApiKey);
end;

function TDeepSeekBackend.GetCompletionsUrl: string;
begin
  Result := 'https://api.deepseek.com/v1/chat/completions';
end;

procedure TDeepSeekBackend.ParseCompletionsResponse(const JsonResponse: TJSONObject; out Response: TLLMResponse);
var
  choices: TJSONArray;
  msg: TJSONObject;
begin
  inherited ParseCompletionsResponse(JsonResponse, Response);
  if JsonResponse.Find('choices') <> nil then
  begin
    choices := TJSONArray(JsonResponse.Arrays['choices']);
    if (choices.Count > 0) and (choices.Objects[0].Find('message') <> nil) then
    begin
      msg := TJSONObject(choices.Objects[0].Objects['message']);
      if msg.Find('reasoning_content') <> nil then
        Response.Reasoning := msg.Get('reasoning_content', '');
    end;
  end;
end;

{ TGoogleBackend }

constructor TGoogleBackend.Create(const AModel, AApiKey: string);
begin
  inherited Create(AModel, AApiKey);
end;

function TGoogleBackend.HttpPostJson(const Url, JsonBody: string; const ExtraHeaders: array of string): string;
var
  client: TFPHTTPClient;
  i: Integer;
  stream: TStringStream;
  responseStream: TStringStream;
  postUrl: string;
begin
  client := TFPHTTPClient.Create(nil);
  stream := TStringStream.Create(JsonBody);
  responseStream := TStringStream.Create('');
  postUrl := Url;
  if FApiKey <> '' then
    postUrl := Url + '?key=' + FApiKey;
  try
    client.AddHeader('Content-Type', 'application/json');
    for i := 0 to High(ExtraHeaders) do
      if ExtraHeaders[i] <> '' then
        client.AddHeader(ExtraHeaders[i], '');
    stream.Position := 0;
    client.RequestBody := stream;
    client.Post(postUrl, responseStream);
    responseStream.Position := 0;
    Result := responseStream.DataString;
  finally
    stream.Free;
    responseStream.Free;
    client.Free;
  end;
end;

function TGoogleBackend.AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse;
var
  url, body, resp, combinedContent: string;
  req, respjson, genConfig, part, content: TJSONObject;
  contents, parts: TJSONArray;
  info: TLLMModelInfo;
  i: Integer;
  candidates: TJSONArray;
begin
  url := 'https://generativelanguage.googleapis.com/v1beta/models/' + FModel + ':generateContent';
  info := GetModelInfo(FModel);
  req := TJSONObject.Create;
  try
    contents := TJSONArray.Create;
    content := TJSONObject.Create;
    parts := TJSONArray.Create;
    combinedContent := '';
    for i := 0 to High(Messages) do
    begin
      if combinedContent <> '' then
        combinedContent := combinedContent + #13#10;
      combinedContent := combinedContent + Messages[i].Content;
    end;
    part := TJSONObject.Create;
    part.Add('text', combinedContent);
    parts.Add(part);
    content.Add('parts', parts);
    contents.Add(content);
    req.Add('contents', contents);

    genConfig := TJSONObject.Create;
    if info.SupportsStructOutput then
      genConfig.Add('response_mime_type', 'application/json');
    req.Add('generationConfig', genConfig);

    body := req.AsJSON;
  finally
    req.Free;
  end;
  try
    resp := HttpPostJson(url, body, []);
    respjson := TJSONObject(GetJSON(resp));
    try
      if respjson.Find('candidates') <> nil then
      begin
        candidates := TJSONArray(respjson.Arrays['candidates']);
        if (candidates.Count > 0) and (candidates.Objects[0].Find('content') <> nil) then
        begin
          content := TJSONObject(candidates.Objects[0].Objects['content']);
          if content.Find('parts') <> nil then
          begin
            parts := TJSONArray(content.Arrays['parts']);
            if (parts.Count > 0) and (parts.Objects[0].Find('text') <> nil) then
              Result.Content := parts.Objects[0].Get('text', '')
            else
              Result.Content := resp;
          end
          else
            Result.Content := resp;
        end
        else
          Result.Content := resp;
      end
      else
        Result.Content := resp;
      Result.Reasoning := '';
    finally
      respjson.Free;
    end;
  except
    on E: Exception do
    begin
      Result.Content := '{"error": "' + E.Message + '"}';
      Result.Reasoning := '';
    end;
  end;
end;

end.
