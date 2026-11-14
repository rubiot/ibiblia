unit FormFindInProject;

{ Find in Project: a modeless window that searches every verse of the open
  project and steps through the matches. Ported from the 1.0.1 mod
  (unitsearch.pas) onto the 1.1.0 project API. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, LCLType,
  ui_style, Project;

type
  TFindNavigateEvent = procedure(const ReferenceID: string) of object;

  { TFrmFindInProject }

  TFrmFindInProject = class(TForm)
  private
    FProject: TProject;
    FResults: TVerseSearchResults;
    FCurrentIndex: Integer;
    FOnNavigate: TFindNavigateEvent;
    FTermLabel: TLabel;
    FTermEdit: TEdit;
    FScopeLabel: TLabel;
    FScopeCombo: TComboBox;
    FCountLabel: TLabel;
    FSearchButton: TButton;
    FResultsList: TListBox;
    FPreviousButton: TButton;
    FNextButton: TButton;
    FCloseButton: TButton;
    procedure BuildControls;
    procedure RefreshCaptions;
    procedure DoSearch;
    procedure ClearResults;
    procedure GoToResult(Index: Integer);
    procedure SearchButtonClick(Sender: TObject);
    procedure PreviousButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure ResultsListSelectionChange(Sender: TObject; User: Boolean);
    procedure TermEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetProject(AProject: TProject);
    property OnNavigate: TFindNavigateEvent read FOnNavigate write FOnNavigate;
  end;

{ Shows the shared Find in Project window for AProject. }
procedure ShowFindInProject(AOwner: TComponent; AProject: TProject;
  ANavigate: TFindNavigateEvent);
{ Call when the open project changes or closes (nil). }
procedure FindInProjectProjectChanged(AProject: TProject);

implementation

resourcestring
  SFindTitle = 'Find in project';
  SFindWhat = 'Find &what:';
  SFindLookIn = '&Look in:';
  SFindButton = '&Search';
  SFindPrevious = '< &Previous';
  SFindNext = '&Next >';
  SFindClose = '&Close';
  SFindPrompt = 'Type a word, phrase or Strong''s number and press Enter.';
  SFindSearching = 'Searching...';
  SFindCountFmt = '%d verses found';
  SFindScopeDestination = 'Destination text';
  SFindScopeSource = 'Source text';
  SFindScopeReference1 = 'Reference text 1';
  SFindScopeReference2 = 'Reference text 2';
  SFindScopeStrongs = 'Strong''s / morphology tags';
  SFindScopeComments = 'Comments';
  SFindScopeAll = 'All fields';

var
  FindInstance: TFrmFindInProject = nil;

procedure ShowFindInProject(AOwner: TComponent; AProject: TProject;
  ANavigate: TFindNavigateEvent);
begin
  if AProject = nil then
    Exit;
  if FindInstance = nil then
    FindInstance := TFrmFindInProject.Create(AOwner);
  FindInstance.OnNavigate := ANavigate;
  FindInstance.SetProject(AProject);
  FindInstance.Show;
  FindInstance.BringToFront;
  if FindInstance.FTermEdit.CanFocus then
    FindInstance.FTermEdit.SetFocus;
  FindInstance.FTermEdit.SelectAll;
end;

procedure FindInProjectProjectChanged(AProject: TProject);
begin
  if FindInstance = nil then
    Exit;
  FindInstance.SetProject(AProject);
  if AProject = nil then
    FindInstance.Hide;
end;

{ TFrmFindInProject }

constructor TFrmFindInProject.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  ApplyFormStyle(Self);
  FCurrentIndex := -1;
  Position := poMainFormCenter;
  BorderStyle := bsSizeToolWin;
  FormStyle := fsStayOnTop;
  ClientWidth := 660;
  ClientHeight := 460;
  Constraints.MinWidth := 480;
  Constraints.MinHeight := 300;
  BuildControls;
  RefreshCaptions;
end;

destructor TFrmFindInProject.Destroy;
begin
  if FindInstance = Self then
    FindInstance := nil;
  inherited Destroy;
end;

procedure TFrmFindInProject.BuildControls;
var
  TopPanel, BottomPanel: TPanel;
begin
  TopPanel := TPanel.Create(Self);
  TopPanel.Parent := Self;
  TopPanel.Align := alTop;
  TopPanel.Height := 96;
  TopPanel.BevelOuter := bvNone;

  FTermLabel := TLabel.Create(Self);
  FTermLabel.Parent := TopPanel;
  FTermLabel.SetBounds(12, 8, 300, 18);

  FTermEdit := TEdit.Create(Self);
  FTermEdit.Parent := TopPanel;
  FTermEdit.SetBounds(12, 28, 500, 28);
  FTermEdit.Anchors := [akLeft, akTop, akRight];
  FTermEdit.OnKeyDown := @TermEditKeyDown;
  FTermLabel.FocusControl := FTermEdit;

  FSearchButton := TButton.Create(Self);
  FSearchButton.Parent := TopPanel;
  FSearchButton.SetBounds(524, 27, 124, 30);
  FSearchButton.Anchors := [akTop, akRight];
  FSearchButton.OnClick := @SearchButtonClick;

  FScopeLabel := TLabel.Create(Self);
  FScopeLabel.Parent := TopPanel;
  FScopeLabel.SetBounds(12, 66, 80, 22);
  FScopeLabel.Layout := tlCenter;

  FScopeCombo := TComboBox.Create(Self);
  FScopeCombo.Parent := TopPanel;
  FScopeCombo.SetBounds(96, 64, 240, 28);
  FScopeCombo.Style := csDropDownList;
  FScopeLabel.FocusControl := FScopeCombo;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Parent := TopPanel;
  FCountLabel.SetBounds(348, 66, 300, 22);
  FCountLabel.Anchors := [akLeft, akTop, akRight];
  FCountLabel.Layout := tlCenter;
  FCountLabel.Font.Style := [fsBold];

  BottomPanel := TPanel.Create(Self);
  BottomPanel.Parent := Self;
  BottomPanel.Align := alBottom;
  BottomPanel.Height := 48;
  BottomPanel.BevelOuter := bvNone;

  FPreviousButton := TButton.Create(Self);
  FPreviousButton.Parent := BottomPanel;
  FPreviousButton.SetBounds(12, 9, 120, 30);
  FPreviousButton.OnClick := @PreviousButtonClick;

  FNextButton := TButton.Create(Self);
  FNextButton.Parent := BottomPanel;
  FNextButton.SetBounds(140, 9, 120, 30);
  FNextButton.OnClick := @NextButtonClick;

  FCloseButton := TButton.Create(Self);
  FCloseButton.Parent := BottomPanel;
  FCloseButton.SetBounds(548, 9, 100, 30);
  FCloseButton.Anchors := [akTop, akRight];
  FCloseButton.Cancel := True;
  FCloseButton.OnClick := @CloseButtonClick;

  FResultsList := TListBox.Create(Self);
  FResultsList.Parent := Self;
  FResultsList.Align := alClient;
  FResultsList.BorderSpacing.Left := 12;
  FResultsList.BorderSpacing.Right := 12;
  FResultsList.OnSelectionChange := @ResultsListSelectionChange;

  ClearResults;
end;

procedure TFrmFindInProject.RefreshCaptions;
var
  Scope: Integer;
begin
  Caption := SFindTitle;
  FTermLabel.Caption := SFindWhat;
  FScopeLabel.Caption := SFindLookIn;
  FSearchButton.Caption := SFindButton;
  FPreviousButton.Caption := SFindPrevious;
  FNextButton.Caption := SFindNext;
  FCloseButton.Caption := SFindClose;
  Scope := FScopeCombo.ItemIndex;
  FScopeCombo.Items.Clear;
  FScopeCombo.Items.Add(SFindScopeDestination);  // ssDestination
  FScopeCombo.Items.Add(SFindScopeSource);       // ssSource
  FScopeCombo.Items.Add(SFindScopeReference1);   // ssReference1
  FScopeCombo.Items.Add(SFindScopeReference2);   // ssReference2
  FScopeCombo.Items.Add(SFindScopeStrongs);      // ssStrongs
  FScopeCombo.Items.Add(SFindScopeAll);          // ssAll
  if (Scope < 0) or (Scope >= FScopeCombo.Items.Count) then
    Scope := 0;
  FScopeCombo.ItemIndex := Scope;
end;

procedure TFrmFindInProject.SetProject(AProject: TProject);
begin
  if FProject <> AProject then
    ClearResults;
  FProject := AProject;
  RefreshCaptions;
end;

procedure TFrmFindInProject.ClearResults;
begin
  SetLength(FResults, 0);
  FCurrentIndex := -1;
  FResultsList.Items.Clear;
  FCountLabel.Caption := SFindPrompt;
  FPreviousButton.Enabled := False;
  FNextButton.Enabled := False;
end;

procedure TFrmFindInProject.DoSearch;
var
  Index: Integer;
  Preview: string;
begin
  if (FProject = nil) or (Trim(FTermEdit.Text) = '') then
    Exit;

  FCountLabel.Caption := SFindSearching;
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  try
    case FScopeCombo.ItemIndex of
      0: FResults := FProject.SearchVerses(Trim(FTermEdit.Text), ssDestination);
      1: FResults := FProject.SearchVerses(Trim(FTermEdit.Text), ssSource);
      2: FResults := FProject.SearchVerses(Trim(FTermEdit.Text), ssReference1);
      3: FResults := FProject.SearchVerses(Trim(FTermEdit.Text), ssReference2);
      4: FResults := FProject.SearchVerses(Trim(FTermEdit.Text), ssStrongs);
    else
      FResults := FProject.SearchVerses(Trim(FTermEdit.Text), ssAll);
    end;
  finally
    Screen.EndWaitCursor;
  end;

  FResultsList.Items.BeginUpdate;
  try
    FResultsList.Items.Clear;
    for Index := 0 to High(FResults) do
    begin
      Preview := StringReplace(FResults[Index].PreviewText, #13, ' ', [rfReplaceAll]);
      Preview := StringReplace(Preview, #10, ' ', [rfReplaceAll]);
      FResultsList.Items.Add(FResults[Index].FormattedRef + '  |  ' + Preview);
    end;
  finally
    FResultsList.Items.EndUpdate;
  end;

  FCountLabel.Caption := Format(SFindCountFmt, [Length(FResults)]);
  FPreviousButton.Enabled := Length(FResults) > 1;
  FNextButton.Enabled := Length(FResults) > 1;
  FCurrentIndex := -1;
  if Length(FResults) > 0 then
    GoToResult(0);
end;

procedure TFrmFindInProject.GoToResult(Index: Integer);
begin
  if (Index < 0) or (Index > High(FResults)) then
    Exit;
  FCurrentIndex := Index;
  if FResultsList.ItemIndex <> Index then
    FResultsList.ItemIndex := Index;
  if Assigned(FOnNavigate) then
    FOnNavigate(FResults[Index].ReferenceID);
end;

procedure TFrmFindInProject.SearchButtonClick(Sender: TObject);
begin
  DoSearch;
end;

procedure TFrmFindInProject.PreviousButtonClick(Sender: TObject);
begin
  if Length(FResults) = 0 then
    Exit;
  if FCurrentIndex <= 0 then
    GoToResult(High(FResults))
  else
    GoToResult(FCurrentIndex - 1);
end;

procedure TFrmFindInProject.NextButtonClick(Sender: TObject);
begin
  if Length(FResults) = 0 then
    Exit;
  if FCurrentIndex >= High(FResults) then
    GoToResult(0)
  else
    GoToResult(FCurrentIndex + 1);
end;

procedure TFrmFindInProject.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmFindInProject.ResultsListSelectionChange(Sender: TObject;
  User: Boolean);
begin
  if User and (FResultsList.ItemIndex >= 0) then
    GoToResult(FResultsList.ItemIndex);
end;

procedure TFrmFindInProject.TermEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    DoSearch;
  end
  else if Key = VK_F3 then
  begin
    Key := 0;
    if ssShift in Shift then
      PreviousButtonClick(Self)
    else
      NextButtonClick(Self);
  end;
end;

end.
