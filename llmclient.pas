unit llmclient;

{$mode objfpc}{$H+}

// This unit provides an interface to LLMs (OpenAI/DeepSeek) for suggesting associations between words in two verses.

interface

uses
  Classes, SysUtils, fpjson;

type
  { Enum for LLM backends }
  TLLMBackend = (lbOpenAI, lbDeepSeek);

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
    // Utility: Perform HTTP POST and return response as string
    function HttpPostJson(const Url, JsonBody, ApiKey: string; const ExtraHeaders: array of string): string;
  public
    function AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse; virtual; abstract;
  end;

  { OpenAI backend implementation (stub, to be implemented with actual HTTP calls) }
  TOpenAIBackend = class(TBaseLLMClient)
  private
    FModel: string;
    FApiKey: string;
  public
    constructor Create(const AModel, AApiKey: string);
    function AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse; override;
  end;

  { DeepSeek backend implementation (stub, to be implemented with actual HTTP calls) }
  TDeepSeekBackend = class(TBaseLLMClient)
  private
    FModel: string;
    FApiKey: string;
  public
    constructor Create(const AModel, AApiKey: string);
    function AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse; override;
  end;

function CreateBackend(const Model: string; const ApiKey: string): TBaseLLMClient;
function GetModelInfo(const Model: string): TLLMModelInfo;

// Utility to build a JSON array of messages
function MessagesToJSONArray(const Messages: array of TLLMMessage): TJSONArray;

implementation

uses
  jsonparser, fphttpclient, opensslsockets;

{ TBaseLLMClient }

function TBaseLLMClient.HttpPostJson(const Url, JsonBody, ApiKey: string; const ExtraHeaders: array of string): string;
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
    if ApiKey <> '' then
      client.AddHeader('Authorization', 'Bearer ' + ApiKey);
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

const
  MODEL_INFO: array[0..5] of record
    Name: string;
    Info: TLLMModelInfo;
  end = (
    (Name: 'gpt-4o-mini';       Info: (Backend: lbOpenAI;   SupportsStructOutput: True)),
    (Name: 'o1';                Info: (Backend: lbOpenAI;   SupportsStructOutput: True)),
    (Name: 'o1-mini';           Info: (Backend: lbOpenAI;   SupportsStructOutput: False)),
    (Name: 'o3-mini';           Info: (Backend: lbOpenAI;   SupportsStructOutput: True)),
    (Name: 'deepseek';          Info: (Backend: lbDeepSeek; SupportsStructOutput: True)),
    (Name: 'deepseek-reasoner'; Info: (Backend: lbDeepSeek; SupportsStructOutput: True))
  );

function GetModelInfo(const Model: string): TLLMModelInfo;
var
  i: Integer;
begin
  for i := Low(MODEL_INFO) to High(MODEL_INFO) do
    if SameText(MODEL_INFO[i].Name, Model) then
      Exit(MODEL_INFO[i].Info);
  raise Exception.Create('Unsupported model backend: ' + Model);
end;

function CreateBackend(const Model: string; const ApiKey: string): TBaseLLMClient;
var
  Info: TLLMModelInfo;
begin
  Info := GetModelInfo(Model);
  case Info.Backend of
    lbOpenAI:    Result := TOpenAIBackend.Create(Model, ApiKey);
    lbDeepSeek:  Result := TDeepSeekBackend.Create(Model, ApiKey);
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

{ TOpenAIBackend }

constructor TOpenAIBackend.Create(const AModel, AApiKey: string);
begin
  FModel := AModel;
  FApiKey := AApiKey;
end;

function TOpenAIBackend.AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse;
var
  url, body, resp: string;
  req, respjson: TJSONObject;
  arr: TJSONArray;
  info: TLLMModelInfo;
  choices: TJSONArray;
  msg: TJSONObject;
begin
  url := 'https://api.openai.com/v1/chat/completions';
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
    resp := HttpPostJson(url, body, FApiKey, []);
    respjson := TJSONObject(GetJSON(resp));
    try
      // Parse response: choices[0].message.content
      if respjson.Find('choices') <> nil then
      begin
        choices := TJSONArray(respjson.Arrays['choices']);
        if (choices.Count > 0) and (choices.Objects[0].Find('message') <> nil) then
        begin
          msg := TJSONObject(choices.Objects[0].Objects['message']);
          Result.Content := msg.Get('content', '');
          // Reasoning is not standard in OpenAI, so leave blank
          Result.Reasoning := '';
        end
        else
          Result.Content := resp;
      end
      else
        Result.Content := resp;
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

{ TDeepSeekBackend }

constructor TDeepSeekBackend.Create(const AModel, AApiKey: string);
begin
  FModel := AModel;
  FApiKey := AApiKey;
end;

function TDeepSeekBackend.AskChatCompletions(const Messages: array of TLLMMessage): TLLMResponse;
var
  url, body, resp: string;
  req, respjson: TJSONObject;
  arr: TJSONArray;
  info: TLLMModelInfo;
  choices: TJSONArray;
  msg: TJSONObject;
begin
  url := 'https://api.deepseek.com/v1/chat/completions';
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
    resp := HttpPostJson(url, body, FApiKey, []);
    respjson := TJSONObject(GetJSON(resp));
    try
      // Parse response: choices[0].message.content, choices[0].message.reasoning_content (if present)
      if respjson.Find('choices') <> nil then
      begin
        choices := TJSONArray(respjson.Arrays['choices']);
        if (choices.Count > 0) and (choices.Objects[0].Find('message') <> nil) then
        begin
          msg := TJSONObject(choices.Objects[0].Objects['message']);
          Result.Content := msg.Get('content', '');
          if msg.Find('reasoning_content') <> nil then
            Result.Reasoning := msg.Get('reasoning_content', '')
          else
            Result.Reasoning := '';
        end
        else
          Result.Content := resp;
      end
      else
        Result.Content := resp;
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
