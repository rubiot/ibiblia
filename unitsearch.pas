unit unitsearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Projeto;

type

  { TFormSearch }

  TFormSearch = class(TForm)
  private
    FProject: TProjeto;
    FResults: TVerseSearchResults;
    FCurrentIndex: Integer;

    PanelTop: TPanel;
    PanelBottom: TPanel;

    LabelTermo: TLabel;
    EditTermo: TEdit;
    LabelScope: TLabel;
    ComboScope: TComboBox;
    LabelMatchCount: TLabel;
    ButtonSearch: TButton;

    ListBoxResults: TListBox;

    ButtonGoTo: TButton;
    ButtonNextMatch: TButton;
    ButtonPrevMatch: TButton;
    ButtonClose: TButton;

    procedure DoSearch;
    procedure NavigateToCurrentResult;
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonGoToClick(Sender: TObject);
    procedure ButtonNextMatchClick(Sender: TObject);
    procedure ButtonPrevMatchClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ListBoxResultsDblClick(Sender: TObject);
    procedure ListBoxResultsSelectionChange(Sender: TObject; User: Boolean);
    procedure EditTermoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent; AProject: TProjeto); reintroduce;
    destructor Destroy; override;
    procedure UpdateProject(AProject: TProjeto);
  end;

procedure ShowSearchDialog(AOwner: TComponent; AProject: TProjeto);

var
  FormSearchInstance: TFormSearch = nil;

implementation

uses
  formPrincipal, LCLType;

procedure ShowSearchDialog(AOwner: TComponent; AProject: TProjeto);
begin
  if (AProject = nil) then
    exit;

  if not Assigned(FormSearchInstance) then
    FormSearchInstance := TFormSearch.Create(AOwner, AProject)
  else
    FormSearchInstance.UpdateProject(AProject);

  FormSearchInstance.Show;
  FormSearchInstance.BringToFront;
  FormSearchInstance.EditTermo.SetFocus;
  FormSearchInstance.EditTermo.SelectAll;
end;

{ TFormSearch }

constructor TFormSearch.Create(AOwner: TComponent; AProject: TProjeto);
begin
  inherited CreateNew(AOwner);
  FProject := AProject;
  FCurrentIndex := -1;
  SetLength(FResults, 0);

  Caption := 'Find in Project (Ctrl+F)';
  Width := 660;
  Height := 480;
  Position := poMainFormCenter;
  BorderStyle := bsSizeable;

  // Top Panel
  PanelTop := TPanel.Create(Self);
  PanelTop.Parent := Self;
  PanelTop.Align := alTop;
  PanelTop.Height := 95;
  PanelTop.BevelOuter := bvNone;

  LabelTermo := TLabel.Create(PanelTop);
  LabelTermo.Parent := PanelTop;
  LabelTermo.Left := 12;
  LabelTermo.Top := 8;
  LabelTermo.Caption := 'Find what:';

  EditTermo := TEdit.Create(PanelTop);
  EditTermo.Parent := PanelTop;
  EditTermo.Left := 12;
  EditTermo.Top := 26;
  EditTermo.Width := 460;
  EditTermo.OnKeyDown := @EditTermoKeyDown;

  ButtonSearch := TButton.Create(PanelTop);
  ButtonSearch.Parent := PanelTop;
  ButtonSearch.Left := 480;
  ButtonSearch.Top := 24;
  ButtonSearch.Width := 110;
  ButtonSearch.Height := 30;
  ButtonSearch.Caption := 'Search';
  ButtonSearch.Default := True;
  ButtonSearch.OnClick := @ButtonSearchClick;

  LabelScope := TLabel.Create(PanelTop);
  LabelScope.Parent := PanelTop;
  LabelScope.Left := 12;
  LabelScope.Top := 62;
  LabelScope.Caption := 'Look in:';

  ComboScope := TComboBox.Create(PanelTop);
  ComboScope.Parent := PanelTop;
  ComboScope.Left := 65;
  ComboScope.Top := 58;
  ComboScope.Width := 250;
  ComboScope.Style := csDropDownList;
  ComboScope.Items.Add('Destination translation (Target text)');
  ComboScope.Items.Add('Original source (Hebrew / Greek)');
  ComboScope.Items.Add('Reference text 1');
  ComboScope.Items.Add('Reference text 2');
  ComboScope.Items.Add('Strong tags / Pairs');
  ComboScope.Items.Add('Translator notes / Comments');
  ComboScope.Items.Add('All text fields');
  ComboScope.ItemIndex := 0;

  LabelMatchCount := TLabel.Create(PanelTop);
  LabelMatchCount.Parent := PanelTop;
  LabelMatchCount.Left := 325;
  LabelMatchCount.Top := 62;
  LabelMatchCount.Font.Style := [fsBold];
  LabelMatchCount.Caption := 'Type a word or phrase and click Search';

  // Bottom Panel
  PanelBottom := TPanel.Create(Self);
  PanelBottom.Parent := Self;
  PanelBottom.Align := alBottom;
  PanelBottom.Height := 46;
  PanelBottom.BevelOuter := bvNone;

  ButtonGoTo := TButton.Create(PanelBottom);
  ButtonGoTo.Parent := PanelBottom;
  ButtonGoTo.Left := 12;
  ButtonGoTo.Top := 8;
  ButtonGoTo.Width := 140;
  ButtonGoTo.Height := 30;
  ButtonGoTo.Caption := 'Go to Verse';
  ButtonGoTo.OnClick := @ButtonGoToClick;
  ButtonGoTo.Enabled := False;

  ButtonPrevMatch := TButton.Create(PanelBottom);
  ButtonPrevMatch.Parent := PanelBottom;
  ButtonPrevMatch.Left := 160;
  ButtonPrevMatch.Top := 8;
  ButtonPrevMatch.Width := 110;
  ButtonPrevMatch.Height := 30;
  ButtonPrevMatch.Caption := '< Previous Match';
  ButtonPrevMatch.OnClick := @ButtonPrevMatchClick;
  ButtonPrevMatch.Enabled := False;

  ButtonNextMatch := TButton.Create(PanelBottom);
  ButtonNextMatch.Parent := PanelBottom;
  ButtonNextMatch.Left := 278;
  ButtonNextMatch.Top := 8;
  ButtonNextMatch.Width := 110;
  ButtonNextMatch.Height := 30;
  ButtonNextMatch.Caption := 'Next Match >';
  ButtonNextMatch.OnClick := @ButtonNextMatchClick;
  ButtonNextMatch.Enabled := False;

  ButtonClose := TButton.Create(PanelBottom);
  ButtonClose.Parent := PanelBottom;
  ButtonClose.Left := 530;
  ButtonClose.Top := 8;
  ButtonClose.Width := 90;
  ButtonClose.Height := 30;
  ButtonClose.Caption := 'Close';
  ButtonClose.Cancel := True;
  ButtonClose.OnClick := @ButtonCloseClick;

  // Center ListBox
  ListBoxResults := TListBox.Create(Self);
  ListBoxResults.Parent := Self;
  ListBoxResults.Align := alClient;
  ListBoxResults.Font.Name := 'Segoe UI';
  ListBoxResults.Font.Size := 9;
  ListBoxResults.OnDblClick := @ListBoxResultsDblClick;
  ListBoxResults.OnSelectionChange := @ListBoxResultsSelectionChange;
end;


destructor TFormSearch.Destroy;
begin
  if FormSearchInstance = Self then
    FormSearchInstance := nil;
  inherited Destroy;
end;

procedure TFormSearch.UpdateProject(AProject: TProjeto);
begin
  FProject := AProject;
  FCurrentIndex := -1;
  SetLength(FResults, 0);
  ListBoxResults.Items.Clear;
  LabelMatchCount.Caption := 'Type a word or phrase and click Search';
  ButtonGoTo.Enabled := False;
  ButtonNextMatch.Enabled := False;
  ButtonPrevMatch.Enabled := False;
end;

procedure TFormSearch.DoSearch;
var
  searchTerm: string;
  scope: TSearchScope;
  i: Integer;
  line: string;
begin
  searchTerm := Trim(EditTermo.Text);
  if searchTerm = '' then
  begin
    ShowMessage('Please enter a search term.');
    EditTermo.SetFocus;
    exit;
  end;

  if FProject = nil then
    exit;

  case ComboScope.ItemIndex of
    0: scope := ssDestination;
    1: scope := ssSource;
    2: scope := ssRef1;
    3: scope := ssRef2;
    4: scope := ssStrongs;
    5: scope := ssComments;
  else
    scope := ssAll;
  end;

  LabelMatchCount.Caption := 'Searching...';
  Application.ProcessMessages;

  FResults := FProject.Pesquisar(searchTerm, scope);
  FCurrentIndex := -1;

  ListBoxResults.Items.BeginUpdate;
  try
    ListBoxResults.Items.Clear;
    for i := 0 to High(FResults) do
    begin
      line := Format('%-16s | %s', [FResults[i].FormattedRef,
        StringReplace(StringReplace(FResults[i].PreviewText, #13#10, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll])]);
      ListBoxResults.Items.Add(line);
    end;
  finally
    ListBoxResults.Items.EndUpdate;
  end;

  LabelMatchCount.Caption := Format('%d verses found', [Length(FResults)]);

  ButtonGoTo.Enabled := Length(FResults) > 0;
  ButtonNextMatch.Enabled := Length(FResults) > 1;
  ButtonPrevMatch.Enabled := Length(FResults) > 1;

  if Length(FResults) > 0 then
  begin
    ListBoxResults.ItemIndex := 0;
    FCurrentIndex := 0;
    NavigateToCurrentResult;
  end;
end;

procedure TFormSearch.NavigateToCurrentResult;
begin
  if (FCurrentIndex >= 0) and (FCurrentIndex < Length(FResults)) then
  begin
    if Assigned(FrmPrincipal) then
      FrmPrincipal.NavegarParaVersiculo(FResults[FCurrentIndex].RefID);
  end;
end;

procedure TFormSearch.ButtonSearchClick(Sender: TObject);
begin
  DoSearch;
end;

procedure TFormSearch.ButtonGoToClick(Sender: TObject);
begin
  if ListBoxResults.ItemIndex >= 0 then
  begin
    FCurrentIndex := ListBoxResults.ItemIndex;
    NavigateToCurrentResult;
  end;
end;

procedure TFormSearch.ButtonNextMatchClick(Sender: TObject);
begin
  if Length(FResults) = 0 then exit;
  Inc(FCurrentIndex);
  if FCurrentIndex >= Length(FResults) then
    FCurrentIndex := 0;
  ListBoxResults.ItemIndex := FCurrentIndex;
  NavigateToCurrentResult;
end;

procedure TFormSearch.ButtonPrevMatchClick(Sender: TObject);
begin
  if Length(FResults) = 0 then exit;
  Dec(FCurrentIndex);
  if FCurrentIndex < 0 then
    FCurrentIndex := High(FResults);
  ListBoxResults.ItemIndex := FCurrentIndex;
  NavigateToCurrentResult;
end;

procedure TFormSearch.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormSearch.ListBoxResultsDblClick(Sender: TObject);
begin
  if ListBoxResults.ItemIndex >= 0 then
  begin
    FCurrentIndex := ListBoxResults.ItemIndex;
    NavigateToCurrentResult;
  end;
end;

procedure TFormSearch.ListBoxResultsSelectionChange(Sender: TObject; User: Boolean);
begin
  if User and (ListBoxResults.ItemIndex >= 0) then
  begin
    FCurrentIndex := ListBoxResults.ItemIndex;
    NavigateToCurrentResult;
  end;
end;

procedure TFormSearch.EditTermoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    DoSearch;
  end;
end;

end.
