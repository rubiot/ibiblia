unit FormLlmOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Projeto, llmclient;

type

  { TFrmLLMOptions }

  TFrmLLMOptions = class(TForm)
  btnCancel: TButton;
  btnOK: TButton;
  btnTest: TButton;
  cbModel: TComboBox;
  LabelModel: TLabel;
  LabelPrompt: TLabel;
  leApiKey: TLabeledEdit;
  MemoPrompt: TMemo;
    procedure FormCreate(Sender: TObject);
  procedure btnOKClick(Sender: TObject);
  procedure btnTestClick(Sender: TObject);
  private
  FProjeto: TProjeto;
  procedure UpdateTestEnabled;
  procedure OnFieldsChanged(Sender: TObject);
  procedure LoadFromProjeto;
  procedure SaveToProjeto;
  public
  procedure SetProjeto(AProjeto: TProjeto);
  end;

var
  FrmLLMOptions: TFrmLLMOptions;

resourcestring
  rsLlmOptionsCaption = 'LLM Options';
  rsLabelModel = 'Model';
  rsLabelApiKey = 'API key';
  rsLabelPrompt = 'Prompt';
  rsBtnOK = 'OK';
  rsBtnCancel = 'Cancel';
  rsBtnTest = 'Test';
  rsTestResultTitle = 'Test result';
  rsTestFailedTitle = 'Test failed';
  rsSystemPrompt = 'You are a helpful assistant.';
  rsFallbackTestPrompt = 'Say hello.';

implementation

{ TFrmLLMOptions }

procedure TFrmLLMOptions.FormCreate(Sender: TObject);
begin
  // Populate supported model names
  FillSupportedModelNames(cbModel.Items);
  // Localize captions
  Caption := rsLlmOptionsCaption;
  LabelModel.Caption := rsLabelModel;
  leApiKey.EditLabel.Caption := rsLabelApiKey;
  LabelPrompt.Caption := rsLabelPrompt;
  btnOK.Caption := rsBtnOK;
  btnCancel.Caption := rsBtnCancel;
  btnTest.Caption := rsBtnTest;
  // React to edits and start with proper enabled state
  cbModel.OnChange := @OnFieldsChanged;
  leApiKey.OnChange := @OnFieldsChanged;
  MemoPrompt.OnChange := @OnFieldsChanged;
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.btnOKClick(Sender: TObject);
begin
  SaveToProjeto;
  ModalResult := mrOK;
end;

procedure TFrmLLMOptions.btnTestClick(Sender: TObject);
var
  client: TBaseLLMClient;
  resp: TLLMResponse;
  testPrompt: string;
  msgs: array[0..1] of TLLMMessage;
begin
  // Build a minimal prompt: use current memo if provided, else a tiny fallback
  if Trim(MemoPrompt.Text) <> '' then
    testPrompt := Trim(MemoPrompt.Text)
  else
    testPrompt := rsFallbackTestPrompt;
  try
    client := CreateBackend(cbModel.Text, leApiKey.Text);
    try
  msgs[0].Role := 'system';
  msgs[0].Content := rsSystemPrompt;
  msgs[1].Role := 'user';
  msgs[1].Content := testPrompt;
  resp := client.AskChatCompletions(msgs);
      if Pos('error', LowerCase(resp.Content)) > 0 then
        MessageDlg(rsTestResultTitle, Copy(resp.Content, 1, 1000), mtError, [mbOK], 0)
      else
        MessageDlg(rsTestResultTitle, Copy(resp.Content, 1, 1000), mtInformation, [mbOK], 0);
    finally
      client.Free;
    end;
  except
    on E: Exception do
      MessageDlg(rsTestFailedTitle, E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TFrmLLMOptions.UpdateTestEnabled;
begin
  // Prompt is optional for testing (we use a fallback when empty)
  btnTest.Enabled := (Trim(cbModel.Text) <> '') and (Trim(leApiKey.Text) <> '');
end;

procedure TFrmLLMOptions.OnFieldsChanged(Sender: TObject);
begin
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.LoadFromProjeto;
begin
  if not Assigned(FProjeto) then Exit;
  cbModel.Text := FProjeto.ObterInfo('ai_associator.model');
  leApiKey.Text := FProjeto.ObterInfo('ai_associator.api_key');
  MemoPrompt.Lines.Text := FProjeto.ObterInfo('ai_associator.prompt');
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.SaveToProjeto;
begin
  if not Assigned(FProjeto) then Exit;
  FProjeto.AtribuirInfo('ai_associator.model', cbModel.Text);
  FProjeto.AtribuirInfo('ai_associator.api_key', leApiKey.Text);
  FProjeto.AtribuirInfo('ai_associator.prompt', MemoPrompt.Lines.Text);
end;

procedure TFrmLLMOptions.SetProjeto(AProjeto: TProjeto);
begin
  FProjeto := AProjeto;
  LoadFromProjeto;
end;

initialization
  {$I formllmoptions.lrs}

end.

