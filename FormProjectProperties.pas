unit FormProjectProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ActnList, BibleScope, Projeto, LCLTranslator,
  StrUtils, ONTTokenizer;

type

  TTokenizerVersionChangeAction = (tvcaKeepCurrentVersion, tvcaChangeAnyway, tvcaClearAssociations);

  TFrmProjectProperties = class(TForm)
    ActionChangeTab: TAction;
    ActionClearText: TAction;
    ActionList1: TActionList;
    ActionLoadText: TAction;
    ActionSelectFont: TAction;
    ActionSelectMorphologyDictionary: TAction;
    ActionSelectStrongDictionary: TAction;
    BtnApplyTextDescription: TBitBtn;
    BtnCancel: TBitBtn;
    BtnClearText: TBitBtn;
    BtnLoadText: TBitBtn;
    BtnOk: TBitBtn;
    BtnSelectFont: TButton;
    BtnSelectMorphologyDictionary: TBitBtn;
    BtnSelectStrongDictionary: TBitBtn;
    cbTokenizerVersion: TComboBox;
    EditMorphologyDictionary: TLabeledEdit;
    EditProjectName: TLabeledEdit;
    EditStrongDictionary: TLabeledEdit;
    EditTextDescription: TLabeledEdit;
    FontDialogSelect: TFontDialog;
    GroupBoxProperties: TGroupBox;
    LabelFont: TLabel;
    LabelTokenizerVersion: TLabel;
    MemoTextPreview: TMemo;
    OpenDialogSelectFile: TOpenDialog;
    ProgressBarOperation: TProgressBar;
    TabControlTextType: TTabControl;
    procedure ActionChangeTabExecute(Sender: TObject);
    procedure ActionClearTextExecute(Sender: TObject);
    procedure ActionLoadTextExecute(Sender: TObject);
    procedure ActionSelectFontExecute(Sender: TObject);
    procedure ActionSelectMorphologyDictionaryExecute(Sender: TObject);
    procedure ActionSelectStrongDictionaryExecute(Sender: TObject);
    procedure BtnApplyTextDescriptionClick(Sender: TObject);
    procedure EditMorphologyDictionaryChange(Sender: TObject);
    procedure EditStrongDictionaryChange(Sender: TObject);
    procedure EditTextDescriptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FProject: TProjeto;
    function ConfirmTokenizerVersionChange: TTokenizerVersionChangeAction;
    procedure LoadTokenizerVersions(SelectedVersion: Integer);
    function GetSelectedTokenizerVersion: Integer;
  public
    procedure SetProject(Project: TProjeto);
    procedure ApplyChanges;
    procedure Translate;
  end;

var
  FrmProjectProperties: TFrmProjectProperties;

resourcestring
  SSetDictionary = 'Set';
  SChooseABibleModule = 'Please choose a theWord Bible module file...';
  SChooseAnotherText = 'Replace text';
  SChangeTextConfirmation = 'Are you sure you want to replace this Bible text with a new one?';
  SChangeTextConfirmationAssociation = 'Loading a new Bible text will cause all your existing associations to be discarded. ' +
                                       'But, if you click ''Yes'', iBiblia can try to keep existing associations for you.'#13#10#13#10 +
                                       'Would you like iBiblia to do that?';
  SClearText = 'Clear Bible text';
  SClearTextConfirmation = 'Are you sure you want to clear this Bible text?';
  SClearTextConfirmationAssociation = 'Are you sure you want to clear this Bible text?'#13#10 +
                                      'All your associations will be lost!';
  SChooseDictionary = 'Choose';
  SOpenDictionary = 'Please choose an unencrypted theWord dictionary module...';
  SSourceTab = 'Source';
  SDestinationTab = 'Destination';
  SReference1Tab = 'Reference 1';
  SReference2Tab = 'Reference 2';
  STokenizerVersion = 'Tokenizer version';
  STokenizerVersionChangeTitle = 'Change tokenizer version';
  STokenizerVersionChangeConfirmation = 'Changing the tokenizer version may invalidate your existing associations.'#13#10#13#10 +
                                        'Choose one of the options below:'#13#10 +
                                        'Yes: clear all associations before changing the tokenizer.'#13#10 +
                                        'No: keep the associations and change the tokenizer anyway.'#13#10 +
                                        'Cancel: keep the current tokenizer version.';
  SClearAssociationsAndChange = 'Clear associations';
  SChangeTokenizerAnyway = 'Change anyway';
  SKeepCurrentTokenizer = 'Keep current version';
  SNewTestamentFilter = 'theWord Bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord Bible modules (*.ont, *.ot)|*.ont;*.ot';
  SWholeBibleFilter = 'theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt';
  SDictionaryFilter = 'theWord dictionary modules (*.dct.twm)|*.dct.twm|All files|*.*';
  SNewTextImportedSuccessfully = 'New text imported successfully';

implementation

{ TFrmProjectProperties }

procedure TFrmProjectProperties.FormCreate(Sender: TObject);
begin
  FProject := nil;
  Translate;
end;

procedure TFrmProjectProperties.EditMorphologyDictionaryChange(Sender: TObject);
begin
  BtnSelectMorphologyDictionary.Caption := SSetDictionary;
end;

procedure TFrmProjectProperties.EditStrongDictionaryChange(Sender: TObject);
begin
  BtnSelectStrongDictionary.Caption := SSetDictionary;
end;

procedure TFrmProjectProperties.EditTextDescriptionChange(Sender: TObject);
begin
  BtnApplyTextDescription.Show;
end;

procedure TFrmProjectProperties.Translate;
var
  SelectedVersion: Integer;
begin
  SelectedVersion := GetSelectedTokenizerVersion;
  TabControlTextType.Tabs[0] := SSourceTab;
  TabControlTextType.Tabs[1] := SDestinationTab;
  TabControlTextType.Tabs[2] := SReference1Tab;
  TabControlTextType.Tabs[3] := SReference2Tab;
  LabelTokenizerVersion.Caption := STokenizerVersion;
  LoadTokenizerVersions(SelectedVersion);
end;

procedure TFrmProjectProperties.ActionLoadTextExecute(Sender: TObject);
var
  ReplaceExistingText: Boolean;
  ImportSucceeded: Boolean;
begin
  ReplaceExistingText := False;
  if TTipoTextoBiblico(TabControlTextType.TabIndex) in [tbOrigem, tbDestino] then
  begin
    case MessageDlg(SChooseAnotherText, SChangeTextConfirmationAssociation, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ReplaceExistingText := True;
      mrNo: ReplaceExistingText := False;
      mrCancel: Exit;
    end;
  end else
  begin
    if MessageDlg(SChooseAnotherText, SChangeTextConfirmation, mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then
      Exit;
    ReplaceExistingText := False;
  end;

  case FProject.Escopo of
    etOT: OpenDialogSelectFile.Filter := SOldTestamentFilter;
    etNT: OpenDialogSelectFile.Filter := SNewTestamentFilter;
    etONT: OpenDialogSelectFile.Filter := SWholeBibleFilter;
  end;

  OpenDialogSelectFile.Title := SChooseABibleModule;
  if OpenDialogSelectFile.Execute then
  begin
    Enabled := False;
    ImportSucceeded := FProject.ImportTheWordModule(OpenDialogSelectFile.FileName,
      TTipoTextoBiblico(TabControlTextType.TabIndex), ProgressBarOperation, ReplaceExistingText);
    Enabled := True;
    ActionChangeTabExecute(nil);
    FProject.RefreshUI;
    if ImportSucceeded then
      ShowMessage(SNewTextImportedSuccessfully);
  end;
end;

procedure TFrmProjectProperties.ActionClearTextExecute(Sender: TObject);
var
  Msg: string;
begin
  Msg := IfThen(TTipoTextoBiblico(TabControlTextType.TabIndex) in [tbOrigem, tbDestino],
                SClearTextConfirmationAssociation, SClearTextConfirmation);
  if MessageDlg(SClearText, Msg, mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    FProject.ClearText(TTipoTextoBiblico(TabControlTextType.TabIndex));
    ActionChangeTabExecute(nil);
    FProject.RefreshUI;
  end;
end;

procedure TFrmProjectProperties.ActionChangeTabExecute(Sender: TObject);
begin
  EditTextDescription.Text := FProject.GetTextDescription(TTipoTextoBiblico(TabControlTextType.TabIndex));
  BtnApplyTextDescription.Hide;
  EditStrongDictionary.Text := FProject.GetInfo(Format('dicstrong%d', [TabControlTextType.TabIndex]));
  EditMorphologyDictionary.Text := FProject.GetInfo(Format('dicmorfo%d', [TabControlTextType.TabIndex]));
  BtnSelectStrongDictionary.Caption := SChooseDictionary;
  BtnSelectMorphologyDictionary.Caption := SChooseDictionary;
  MemoTextPreview.Text := FProject.GetPlainVerseText(TTipoTextoBiblico(TabControlTextType.TabIndex));
  MemoTextPreview.Font := FProject.GetTextFont(TTipoTextoBiblico(TabControlTextType.TabIndex));
end;

procedure TFrmProjectProperties.ActionSelectFontExecute(Sender: TObject);
begin
  FontDialogSelect.Font := MemoTextPreview.Font;
  if FontDialogSelect.Execute then
  begin
    MemoTextPreview.Font := FontDialogSelect.Font;
    FProject.AssignTextFont(MemoTextPreview.Font, [TTipoTextoBiblico(TabControlTextType.TabIndex)]);
  end;
end;

procedure TFrmProjectProperties.ActionSelectMorphologyDictionaryExecute(Sender: TObject);
var
  CurrentDirectory: string;
  SelectedPath: string;
begin
  CurrentDirectory := '';
  OpenDialogSelectFile.Filter := SDictionaryFilter;
  OpenDialogSelectFile.FileName := EditMorphologyDictionary.Text;
  OpenDialogSelectFile.Title := SOpenDictionary;
  if (BtnSelectMorphologyDictionary.Caption = SSetDictionary) or OpenDialogSelectFile.Execute then
  begin
    { use a relative path whenever possible }
    GetDir(0, CurrentDirectory);
    if Pos(CurrentDirectory, OpenDialogSelectFile.FileName) = 1 then
      SelectedPath := Format('.%s', [RightStr(OpenDialogSelectFile.FileName,
        Length(OpenDialogSelectFile.FileName) - Length(CurrentDirectory))])
    else
      SelectedPath := OpenDialogSelectFile.FileName;
    if FProject.AssignMorphologyDictionary(SelectedPath, [TTipoTextoBiblico(TabControlTextType.TabIndex)]) then
      EditMorphologyDictionary.Text := SelectedPath;
  end;
  BtnSelectMorphologyDictionary.Caption := SChooseDictionary;
end;

procedure TFrmProjectProperties.ActionSelectStrongDictionaryExecute(Sender: TObject);
var
  CurrentDirectory: string;
  SelectedPath: string;
begin
  CurrentDirectory := '';
  OpenDialogSelectFile.Filter := SDictionaryFilter;
  OpenDialogSelectFile.FileName := EditStrongDictionary.Text;
  OpenDialogSelectFile.Title := SOpenDictionary;
  if (BtnSelectStrongDictionary.Caption = SSetDictionary) or OpenDialogSelectFile.Execute then
  begin
    { use a relative path whenever possible }
    GetDir(0, CurrentDirectory);
    if Pos(CurrentDirectory, OpenDialogSelectFile.FileName) = 1 then
      SelectedPath := Format('.%s', [RightStr(OpenDialogSelectFile.FileName,
        Length(OpenDialogSelectFile.FileName) - Length(CurrentDirectory))])
    else
      SelectedPath := OpenDialogSelectFile.FileName;
    if FProject.AssignStrongDictionary(SelectedPath, [TTipoTextoBiblico(TabControlTextType.TabIndex)]) then
      EditStrongDictionary.Text := SelectedPath;
  end;
  BtnSelectStrongDictionary.Caption := SChooseDictionary;
end;

procedure TFrmProjectProperties.BtnApplyTextDescriptionClick(Sender: TObject);
begin
  FProject.SetTextDescription(TTipoTextoBiblico(TabControlTextType.TabIndex), EditTextDescription.Text);
  BtnApplyTextDescription.Hide;
end;

function TFrmProjectProperties.ConfirmTokenizerVersionChange: TTokenizerVersionChangeAction;
var
  DialogForm: TForm;
  ButtonIndex: Integer;
begin
  Result := tvcaKeepCurrentVersion;
  DialogForm := CreateMessageDialog(STokenizerVersionChangeConfirmation, mtConfirmation, [mbYes, mbNo, mbCancel]);
  try
    DialogForm.Caption := STokenizerVersionChangeTitle;
    for ButtonIndex := 0 to DialogForm.ComponentCount - 1 do
      if DialogForm.Components[ButtonIndex] is TButton then
        case TButton(DialogForm.Components[ButtonIndex]).ModalResult of
          mrYes: TButton(DialogForm.Components[ButtonIndex]).Caption := SClearAssociationsAndChange;
          mrNo: TButton(DialogForm.Components[ButtonIndex]).Caption := SChangeTokenizerAnyway;
          mrCancel: TButton(DialogForm.Components[ButtonIndex]).Caption := SKeepCurrentTokenizer;
        end;

    case DialogForm.ShowModal of
      mrYes: Result := tvcaClearAssociations;
      mrNo: Result := tvcaChangeAnyway;
    else
      Result := tvcaKeepCurrentVersion;
    end;
  finally
    DialogForm.Free;
  end;
end;

procedure TFrmProjectProperties.LoadTokenizerVersions(SelectedVersion: Integer);
var
  VersionIndex: Integer;
begin
  TTokenizerFactory.PopulateVersionCaptions(cbTokenizerVersion.Items);
  cbTokenizerVersion.ItemIndex := -1;

  for VersionIndex := 0 to cbTokenizerVersion.Items.Count - 1 do
    if PtrInt(cbTokenizerVersion.Items.Objects[VersionIndex]) = SelectedVersion then
    begin
      cbTokenizerVersion.ItemIndex := VersionIndex;
      Exit;
    end;

  if cbTokenizerVersion.Items.Count > 0 then
    cbTokenizerVersion.ItemIndex := cbTokenizerVersion.Items.Count - 1;
end;

function TFrmProjectProperties.GetSelectedTokenizerVersion: Integer;
begin
  if cbTokenizerVersion.ItemIndex >= 0 then
    Result := PtrInt(cbTokenizerVersion.Items.Objects[cbTokenizerVersion.ItemIndex])
  else if Assigned(FProject) then
    Result := FProject.TokenizerVersion
  else
    Result := TTokenizerFactory.LATEST_VERSION;
end;

procedure TFrmProjectProperties.SetProject(Project: TProjeto);
begin
  FProject := Project;
  EditProjectName.Caption := Project.GetInfo('descricao');
  LoadTokenizerVersions(Project.TokenizerVersion);
  ActionChangeTab.Execute;
end;

procedure TFrmProjectProperties.ApplyChanges;
var
  CurrentVersion: Integer;
  SelectedVersion: Integer;
begin
  if Assigned(FProject) then
  begin
    FProject.SetInfo('descricao', EditProjectName.Caption);

    CurrentVersion := FProject.TokenizerVersion;
    SelectedVersion := GetSelectedTokenizerVersion;

    if SelectedVersion <> CurrentVersion then
    begin
      if FProject.HasStoredAssociations then
        case ConfirmTokenizerVersionChange of
          tvcaKeepCurrentVersion:
            SelectedVersion := CurrentVersion;
          tvcaClearAssociations:
            FProject.ClearStoredAssociations;
        end;

      if SelectedVersion <> CurrentVersion then
        FProject.TokenizerVersion := SelectedVersion;
    end;
  end;
end;

initialization
{$IFNDEF FPCUNIT}
  {$I FormProjectProperties.lrs}
{$ENDIF}

end.