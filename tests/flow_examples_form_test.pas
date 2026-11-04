unit flow_examples_form_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ComCtrls,
  fpcunit, testregistry, FormFlowExamples, ai_flow_config;

type
  TFlowExamplesFormTests = class(TTestCase)
  private
    FFlowId, FLanguage, FExpectedTitle, FModalError: string;
    FInspected: Boolean;
    procedure InspectAndCloseDialog(Data: PtrInt);
    procedure CheckDialogLanguages(const FlowId, PortugueseTitle,
      EnglishTitle: string);
    procedure AssertCaption(Form: TForm; ComponentClass: TComponentClass;
      const Expected: string);
    procedure AssertButton(Form: TForm; const ExpectedCaption,
      ExpectedHint: string);
    procedure CaptureIfRequested(Form: TForm);
  published
    procedure AssociationExamplesFollowSelectedLanguage;
    procedure TranslationExamplesFollowSelectedLanguage;
    procedure ConfirmationAndPreviewMessagesFollowSelectedLanguage;
  end;

implementation

uses
  LCLTranslator, ai_association_flow, ai_translation_flow;

procedure TFlowExamplesFormTests.AssertCaption(Form: TForm;
  ComponentClass: TComponentClass; const Expected: string);
var
  I: Integer;
begin
  for I := 0 to Form.ComponentCount - 1 do
    if Form.Components[I].InheritsFrom(ComponentClass) and
      (TControl(Form.Components[I]).Caption = Expected) then
      Exit;
  Fail('Missing ' + ComponentClass.ClassName + ' caption: ' + Expected);
end;

procedure TFlowExamplesFormTests.AssertButton(Form: TForm;
  const ExpectedCaption, ExpectedHint: string);
var
  I: Integer;
  Button: TButton;
begin
  for I := 0 to Form.ComponentCount - 1 do
    if Form.Components[I] is TButton then
    begin
      Button := TButton(Form.Components[I]);
      if Button.Caption = ExpectedCaption then
      begin
        AssertEquals('Hint for ' + ExpectedCaption, ExpectedHint, Button.Hint);
        AssertTrue('Hint must be available for ' + ExpectedCaption, Button.ShowHint);
        Exit;
      end;
    end;
  Fail('Missing button: ' + ExpectedCaption);
end;

procedure TFlowExamplesFormTests.CaptureIfRequested(Form: TForm);
var
  Bitmap: TBitmap;
  Picture: TPortableNetworkGraphic;
begin
  if GetEnvironmentVariable('IBIBLIA_LLM_UI_CAPTURE') = '' then Exit;
  Form.Repaint;
  Bitmap := Form.GetFormImage;
  Picture := TPortableNetworkGraphic.Create;
  try
    Picture.Assign(Bitmap);
    Picture.SaveToFile(IncludeTrailingPathDelimiter(GetTempDir(False)) +
      'ibiblia-flow-examples-' + FFlowId + '-' + FLanguage + '.png');
  finally
    Picture.Free;
    Bitmap.Free;
  end;
end;

procedure TFlowExamplesFormTests.InspectAndCloseDialog(Data: PtrInt);
var
  I, ListCount, MemoCount: Integer;
  Form: TFrmFlowExamples;
  EmptyMessage: string;
begin
  Form := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TFrmFlowExamples then
    begin
      Form := TFrmFlowExamples(Screen.Forms[I]);
      Break;
    end;
  if not Assigned(Form) then
  begin
    FModalError := 'The examples modal dialog was not found';
    if Assigned(Screen.ActiveCustomForm) then
      Screen.ActiveCustomForm.ModalResult := mrCancel;
    Exit;
  end;
  FInspected := True;
  try
    try
      AssertEquals('Examples dialog title', FExpectedTitle, Form.Caption);
      if FLanguage = 'pt' then
      begin
        EmptyMessage := 'Nenhum exemplo salvo.';
        AssertCaption(Form, TLabel, 'Exemplos');
        AssertCaption(Form, TTabSheet, 'Visualização');
        AssertCaption(Form, TTabSheet, 'Resposta do prompt');
        AssertCaption(Form, TLabel, 'Texto de origem');
        AssertCaption(Form, TLabel, 'Destino / tradução');
        AssertButton(Form, 'Adicionar versículo atual',
          'Adicionar ou substituir um exemplo usando os versículos atuais de origem e destino.');
        AssertButton(Form, 'Mover para cima',
          'Mover o exemplo selecionado para uma posição anterior no prompt.');
        AssertButton(Form, 'Mover para baixo',
          'Mover o exemplo selecionado para uma posição posterior no prompt.');
        AssertButton(Form, 'Remover',
          'Remover o exemplo selecionado desta lista de trabalho.');
        AssertButton(Form, 'Limpar',
          'Remover todos os exemplos desta lista de trabalho.');
        AssertButton(Form, 'OK', 'Salvar as alterações dos exemplos.');
        AssertButton(Form, 'Cancelar', 'Fechar sem salvar as alterações dos exemplos.');
      end
      else
      begin
        EmptyMessage := 'No saved examples.';
        AssertCaption(Form, TLabel, 'Examples');
        AssertCaption(Form, TTabSheet, 'Visual preview');
        AssertCaption(Form, TTabSheet, 'Prompt response');
        AssertCaption(Form, TLabel, 'Source text');
        AssertCaption(Form, TLabel, 'Destination / translation');
        AssertButton(Form, 'Add current verse',
          'Add or replace an example using the current source and destination verses.');
        AssertButton(Form, 'Move up', 'Move the selected example earlier in the prompt.');
        AssertButton(Form, 'Move down', 'Move the selected example later in the prompt.');
        AssertButton(Form, 'Remove', 'Remove the selected example from this working list.');
        AssertButton(Form, 'Clear', 'Remove all examples from this working list.');
        AssertButton(Form, 'OK', 'Save these example changes.');
        AssertButton(Form, 'Cancel', 'Close without saving example changes.');
      end;
      AssertCaption(Form, TLabel, EmptyMessage);
      ListCount := 0;
      MemoCount := 0;
      for I := 0 to Form.ComponentCount - 1 do
      begin
        if Form.Components[I] is TListBox then
        begin
          Inc(ListCount);
          AssertEquals('Examples list starts empty', 0,
            TListBox(Form.Components[I]).Items.Count);
        end;
        if Form.Components[I] is TMemo then
        begin
          Inc(MemoCount);
          AssertEquals('The prompt tab also localizes its empty state',
            EmptyMessage, Trim(TMemo(Form.Components[I]).Text));
        end;
      end;
      AssertEquals('One examples list is inspected', 1, ListCount);
      AssertEquals('One prompt response is inspected', 1, MemoCount);
      CaptureIfRequested(Form);
    except
      on E: Exception do
        FModalError := E.ClassName + ': ' + E.Message;
    end;
  finally
    Form.ModalResult := mrCancel;
  end;
end;

procedure TFlowExamplesFormTests.CheckDialogLanguages(const FlowId,
  PortugueseTitle, EnglishTitle: string);
const
  Languages: array[0..1] of string = ('pt', 'en');
var
  LanguagesDir: string;
  Examples: TAIFlowExampleArray;
  I: Integer;
begin
  Forms.Application.Initialize;
  LanguagesDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\languages');
  FFlowId := FlowId;
  Examples := nil;
  SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  try
    for I := Low(Languages) to High(Languages) do
    begin
      FLanguage := Languages[I];
      SetDefaultLang(FLanguage, LanguagesDir, 'iBiblia', False);
      if FLanguage = 'pt' then FExpectedTitle := PortugueseTitle
      else FExpectedTitle := EnglishTitle;
      FInspected := False;
      FModalError := '';
      Forms.Application.QueueAsyncCall(@InspectAndCloseDialog, 0);
      AssertFalse('Inspection cancels without changing examples',
        TFrmFlowExamples.Execute(nil, nil, FlowId, Examples));
      AssertTrue('The real modal must have been inspected', FInspected);
      AssertEquals(FlowId + ' examples in ' + FLanguage, '', FModalError);
      AssertEquals('Cancellation preserves the empty examples', 0, Length(Examples));
    end;
  finally
    Forms.Application.RemoveAsyncCalls(Self);
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  end;
end;

procedure TFlowExamplesFormTests.AssociationExamplesFollowSelectedLanguage;
begin
  CheckDialogLanguages('association', 'Exemplos de Associação', 'Association examples');
end;

procedure TFlowExamplesFormTests.TranslationExamplesFollowSelectedLanguage;
begin
  CheckDialogLanguages('translation', 'Exemplos de Tradução', 'Translation examples');
end;

procedure TFlowExamplesFormTests.ConfirmationAndPreviewMessagesFollowSelectedLanguage;
var
  LanguagesDir: string;
begin
  LanguagesDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\languages');
  SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  try
    SetDefaultLang('pt', LanguagesDir, 'iBiblia', False);
    AssertEquals('Exemplo já existe', rsReplaceExampleTitle);
    AssertEquals('Este versículo já está salvo como exemplo para o fluxo de %s.'#10#10 +
      'Deseja substituí-lo pelo estado atual do versículo?',
      StringReplace(rsReplaceExamplePrompt, #13#10, #10, [rfReplaceAll]));
    AssertEquals('Limpar exemplos', rsClearExamplesTitle);
    AssertEquals('Remover todos os exemplos salvos para o fluxo de %s?',
      rsClearExamplesPrompt);
    AssertEquals('Aviso', rsWarningTitle);
    AssertEquals('Visualização pronta.', rsPreviewReady);
    AssertEquals('Este exemplo não possui associações visuais para exibir.',
      rsPreviewNoVisualAssociations);
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
    AssertEquals('Example already exists', rsReplaceExampleTitle);
    AssertEquals('This verse is already saved as an example for the %s flow.'#10#10 +
      'Do you want to replace it with the current verse state?',
      StringReplace(rsReplaceExamplePrompt, #13#10, #10, [rfReplaceAll]));
    AssertEquals('Clear examples', rsClearExamplesTitle);
    AssertEquals('Remove all saved examples for the %s flow?', rsClearExamplesPrompt);
    AssertEquals('Warning', rsWarningTitle);
    AssertEquals('Visual preview ready.', rsPreviewReady);
    AssertEquals('This example has no visual associations to display.',
      rsPreviewNoVisualAssociations);
  finally
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  end;
end;

initialization
  RegisterTest(TFlowExamplesFormTests);

end.
