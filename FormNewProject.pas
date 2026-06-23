unit FormNewProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LCLTranslator, ONTTokenizer, status_catalog,
  FormStatusCatalog;

type

  TFrmNewProject = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    BtnSelectDestinationText: TBitBtn;
    BtnSelectReferenceText1: TBitBtn;
    BtnSelectReferenceText2: TBitBtn;
    BtnSelectSourceText: TBitBtn;
    cbTokenizerVersion: TComboBox;
    GroupBoxProperties: TGroupBox;
    GroupBoxTexts: TGroupBox;
    EditDestinationText: TLabeledEdit;
    EditProjectName: TLabeledEdit;
    EditReferenceText1: TLabeledEdit;
    EditReferenceText2: TLabeledEdit;
    EditSourceText: TLabeledEdit;
    LabelTokenizerVersion: TLabel;
    OpenDialogSelectText: TOpenDialog;
    RadioGroupScope: TRadioGroup;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnSelectDestinationTextClick(Sender: TObject);
    procedure BtnSelectReferenceText1Click(Sender: TObject);
    procedure BtnSelectReferenceText2Click(Sender: TObject);
    procedure BtnSelectSourceTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FStatusDefinitions: TStatusDefinitions;
    FStatusCatalogCustomized: Boolean;
    BtnStatusCatalog: TButton;
    procedure BtnStatusCatalogClick(Sender: TObject);
    procedure CreateStatusCatalogButton;
    function DefaultStatusDefinitions: TStatusDefinitions;
    procedure LoadTokenizerVersions(SelectedVersion: Integer);
    procedure SelectText(Edit: TLabeledEdit);
  public
    function GetTokenizerVersion: Integer;
    function GetStatusDefinitions: TStatusDefinitions;
    function HasCustomStatusCatalog: Boolean;
    procedure Translate;
  end;

var
  FrmNewProject: TFrmNewProject;

resourcestring
  SNewProject = 'New project';
  SChooseAName = 'Please choose a name for your project';
  SChooseAtLeastOneText = 'Please choose a source Bible text';
  SOldTestament = 'Old Testament';
  SNewTestament = 'New Testament';
  SWholeBible = 'Whole Bible';
  STokenizerVersion = 'Tokenizer version';
  SStatusCatalog = 'Status...';
  SStatusNotAssociated = 'Not associated';
  SStatusAssociating = 'Associating';
  SStatusNeedsReview = 'Needs review';
  SStatusAssociated = 'Associated!';
  SNewTestamentFilter = 'theWord Bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord Bible modules (*.ont, *.ot)|*.ont;*.ot';
  SWholeBibleFilter = 'theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt';

implementation

{ TFrmNewProject }

procedure TFrmNewProject.BtnOkClick(Sender: TObject);
begin
  if EditProjectName.Text = '' then
  begin
    ModalResult := mrNone;
    MessageDlg(SNewProject, SChooseAName, mtWarning, [mbOK], 0);
    Exit;
  end;

  if EditSourceText.Text = '' then
  begin
    ModalResult := mrNone;
    MessageDlg(SNewProject, SChooseAtLeastOneText, mtWarning, [mbOK], 0);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TFrmNewProject.FormCreate(Sender: TObject);
begin
  FStatusCatalogCustomized := False;
  FStatusDefinitions := DefaultStatusDefinitions;
  CreateStatusCatalogButton;
  Translate;
end;

function TFrmNewProject.DefaultStatusDefinitions: TStatusDefinitions;
begin
  Result := BuildLegacyStatusDefinitions([
    SStatusNotAssociated,
    SStatusAssociating,
    SStatusNeedsReview,
    SStatusAssociated
  ]);
end;

procedure TFrmNewProject.CreateStatusCatalogButton;
const
  ExtraHeight = 36;
begin
  if Assigned(BtnStatusCatalog) then
    Exit;

  ClientHeight := ClientHeight + ExtraHeight;
  GroupBoxProperties.Height := GroupBoxProperties.Height + ExtraHeight;
  RadioGroupScope.Top := RadioGroupScope.Top + ExtraHeight;
  GroupBoxTexts.Top := GroupBoxTexts.Top + ExtraHeight;
  BtnOk.Top := BtnOk.Top + ExtraHeight;
  BtnCancel.Top := BtnCancel.Top + ExtraHeight;

  BtnStatusCatalog := TButton.Create(Self);
  BtnStatusCatalog.Parent := GroupBoxProperties;
  BtnStatusCatalog.Left := 8;
  BtnStatusCatalog.Top := 106;
  BtnStatusCatalog.Width := 140;
  BtnStatusCatalog.Height := 25;
  BtnStatusCatalog.Caption := SStatusCatalog;
  BtnStatusCatalog.OnClick := @BtnStatusCatalogClick;
end;

procedure TFrmNewProject.BtnStatusCatalogClick(Sender: TObject);
var
  Definitions: TStatusDefinitions;
  Counts: TStatusCounts;
  Remaps: TStatusRemaps;
begin
  Definitions := Copy(FStatusDefinitions, 0, Length(FStatusDefinitions));
  Counts := nil;
  if TFrmStatusCatalog.Execute(Definitions, Counts, Remaps) then
  begin
    NormalizeStatusDefinitions(Definitions, True);
    FStatusDefinitions := Definitions;
    FStatusCatalogCustomized := True;
  end;
end;

procedure TFrmNewProject.BtnSelectSourceTextClick(Sender: TObject);
begin
  SelectText(EditSourceText);
end;

procedure TFrmNewProject.BtnSelectDestinationTextClick(Sender: TObject);
begin
  SelectText(EditDestinationText);
end;

procedure TFrmNewProject.BtnSelectReferenceText1Click(Sender: TObject);
begin
  SelectText(EditReferenceText1);
end;

procedure TFrmNewProject.BtnSelectReferenceText2Click(Sender: TObject);
begin
  SelectText(EditReferenceText2);
end;

procedure TFrmNewProject.SelectText(Edit: TLabeledEdit);
begin
  case RadioGroupScope.ItemIndex of
    0: OpenDialogSelectText.Filter := SOldTestamentFilter;
    1: OpenDialogSelectText.Filter := SNewTestamentFilter;
    2: OpenDialogSelectText.Filter := SWholeBibleFilter;
  end;
  if OpenDialogSelectText.Execute then
    Edit.Text := OpenDialogSelectText.FileName;
end;

procedure TFrmNewProject.LoadTokenizerVersions(SelectedVersion: Integer);
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

function TFrmNewProject.GetTokenizerVersion: Integer;
begin
  if cbTokenizerVersion.ItemIndex >= 0 then
    Result := PtrInt(cbTokenizerVersion.Items.Objects[cbTokenizerVersion.ItemIndex])
  else
    Result := TTokenizerFactory.LATEST_VERSION;
end;

function TFrmNewProject.GetStatusDefinitions: TStatusDefinitions;
begin
  Result := Copy(FStatusDefinitions, 0, Length(FStatusDefinitions));
  NormalizeStatusDefinitions(Result, True);
end;

function TFrmNewProject.HasCustomStatusCatalog: Boolean;
begin
  Result := FStatusCatalogCustomized;
end;

procedure TFrmNewProject.Translate;
var
  SelectedVersion: Integer;
begin
  SelectedVersion := GetTokenizerVersion;
  if not FStatusCatalogCustomized then
    FStatusDefinitions := DefaultStatusDefinitions;
  with RadioGroupScope do
  begin
    Items.Clear;
    Items.Add(SOldTestament);
    Items.Add(SNewTestament);
    Items.Add(SWholeBible);
    ItemIndex := 1;
  end;
  LabelTokenizerVersion.Caption := STokenizerVersion;
  if Assigned(BtnStatusCatalog) then
    BtnStatusCatalog.Caption := SStatusCatalog;
  LoadTokenizerVersions(SelectedVersion);
end;

initialization
{$IFNDEF FPCUNIT}
  {$I FormNewProject.lrs}
{$ENDIF}

end.
