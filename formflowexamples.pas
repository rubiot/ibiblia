unit FormFlowExamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Project, Verse, ai_flow_config;

type
  { TFrmFlowExamples }

  TFrmFlowExamples = class(TForm)
  private
    FProject: TProject;
    FFlowId: string;
    FExamples: TAIFlowExampleArray;
    FSourceVerse: TVerse;
    FTargetVerse: TVerse;
    FListBox: TListBox;
    FBtnAddCurrent: TButton;
    FBtnMoveUp: TButton;
    FBtnMoveDown: TButton;
    FBtnRemove: TButton;
    FBtnClear: TButton;
    FBtnOK: TButton;
    FBtnCancel: TButton;
    FPageControl: TPageControl;
    FTabVisual: TTabSheet;
    FTabExpected: TTabSheet;
    FScrollBoxSource: TScrollBox;
    FScrollBoxTarget: TScrollBox;
    FMemoExpected: TMemo;
    FStatusLabel: TLabel;
    procedure BuildControls;
    procedure CreateVersePreview;
    procedure ConfigureVersePreview;
    procedure SetProjectAndFlow(AProject: TProject; const AFlowId: string;
      const Examples: TAIFlowExampleArray);
    procedure PopulateList;
    procedure SelectExample(Index: Integer);
    procedure LoadSelectedExample;
    procedure UpdateButtons;
    function SourcePreviewText(Index: Integer): string;
    function TargetPreviewText(Index: Integer): string;
    function HasVisualAssociationData(Index: Integer): Boolean;
    procedure OnListSelectionChange(Sender: TObject; User: Boolean);
    procedure OnAddCurrentClick(Sender: TObject);
    procedure OnMoveUpClick(Sender: TObject);
    procedure OnMoveDownClick(Sender: TObject);
    procedure OnRemoveClick(Sender: TObject);
    procedure OnClearClick(Sender: TObject);
    function FlowDisplayName: string;
    function IsAssociationFlow: Boolean;
    procedure ShowPreviewStatus(const AMessage: string; AColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(AOwner: TComponent; AProject: TProject;
      const AFlowId: string; var Examples: TAIFlowExampleArray): Boolean;
  end;

resourcestring
  rsFlowExamplesTitle = '%s examples';
  rsExamplesList = 'Examples';
  rsAddCurrentVerse = 'Add current verse';
  rsMoveExampleUp = 'Move up';
  rsMoveExampleDown = 'Move down';
  rsRemoveExample = 'Remove';
  rsClearExamples = 'Clear';
  rsBtnOK = 'OK';
  rsBtnCancel = 'Cancel';
  rsVisualPreview = 'Visual preview';
  rsPromptResponse = 'Prompt response';
  rsSourceText = 'Source text';
  rsDestinationText = 'Destination / translation';
  rsNoExamples = 'No saved examples.';
  rsReplaceExampleTitle = 'Example already exists';
  rsReplaceExamplePrompt = 'This verse is already saved as an example for the %s flow.'#13#10#13#10 +
                           'Do you want to replace it with the current verse state?';
  rsClearExamplesTitle = 'Clear examples';
  rsClearExamplesPrompt = 'Remove all saved examples for the %s flow?';
  rsWarningTitle = 'Warning';
  rsPreviewReady = 'Visual preview ready.';
  rsPreviewNoVisualAssociations = 'This example has no visual associations to display.';
  rsHintAddCurrentVerse = 'Add or replace an example using the current source and destination verses.';
  rsHintMoveExampleUp = 'Move the selected example earlier in the prompt.';
  rsHintMoveExampleDown = 'Move the selected example later in the prompt.';
  rsHintRemoveExample = 'Remove the selected example from this working list.';
  rsHintClearExamples = 'Remove all examples from this working list.';
  rsHintOK = 'Save these example changes.';
  rsHintCancel = 'Close without saving example changes.';

implementation

uses
  ai_flows;

constructor TFrmFlowExamples.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildControls;
  CreateVersePreview;
end;

destructor TFrmFlowExamples.Destroy;
begin
  FreeAndNil(FSourceVerse);
  FreeAndNil(FTargetVerse);
  inherited Destroy;
end;

class function TFrmFlowExamples.Execute(AOwner: TComponent; AProject: TProject;
  const AFlowId: string; var Examples: TAIFlowExampleArray): Boolean;
var
  Frm: TFrmFlowExamples;
begin
  Frm := TFrmFlowExamples.Create(AOwner);
  try
    Frm.SetProjectAndFlow(AProject, AFlowId, Examples);
    Result := Frm.ShowModal = mrOK;
    if Result then
      Examples := Copy(Frm.FExamples);
  finally
    Frm.Free;
  end;
end;

procedure TFrmFlowExamples.BuildControls;
var
  PanelLeft, PanelButtons, PanelBottom, PanelVisual, PanelSource, PanelTarget: TPanel;
  SplitterLeft, SplitterVerses: TSplitter;
  Lbl: TLabel;
  ButtonLeft, ButtonWidth: Integer;
begin
  Width := 920;
  Height := 650;
  Position := poMainFormCenter;
  Constraints.MinWidth := 720;
  Constraints.MinHeight := 520;

  PanelBottom := TPanel.Create(Self);
  PanelBottom.Parent := Self;
  PanelBottom.Align := alBottom;
  PanelBottom.Height := 44;
  PanelBottom.BevelOuter := bvNone;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := PanelBottom;
  FBtnCancel.Caption := rsBtnCancel;
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Width := 90;
  FBtnCancel.Height := 26;
  FBtnCancel.Left := PanelBottom.ClientWidth - FBtnCancel.Width - 10;
  FBtnCancel.Top := 9;
  FBtnCancel.Anchors := [akRight, akBottom];
  FBtnCancel.Hint := rsHintCancel;
  FBtnCancel.ShowHint := True;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := PanelBottom;
  FBtnOK.Caption := rsBtnOK;
  FBtnOK.ModalResult := mrOK;
  FBtnOK.Default := True;
  FBtnOK.Width := 90;
  FBtnOK.Height := 26;
  FBtnOK.Left := FBtnCancel.Left - FBtnOK.Width - 10;
  FBtnOK.Top := 9;
  FBtnOK.Anchors := [akRight, akBottom];
  FBtnOK.Hint := rsHintOK;
  FBtnOK.ShowHint := True;

  PanelLeft := TPanel.Create(Self);
  PanelLeft.Parent := Self;
  PanelLeft.Align := alLeft;
  PanelLeft.Width := 260;
  PanelLeft.BevelOuter := bvNone;
  PanelLeft.BorderSpacing.Around := 8;

  Lbl := TLabel.Create(Self);
  Lbl.Parent := PanelLeft;
  Lbl.Align := alTop;
  Lbl.Height := 24;
  Lbl.Caption := rsExamplesList;
  Lbl.Layout := tlCenter;

  PanelButtons := TPanel.Create(Self);
  PanelButtons.Parent := PanelLeft;
  PanelButtons.Align := alBottom;
  PanelButtons.Height := 174;
  PanelButtons.BevelOuter := bvNone;
  ButtonLeft := 4;
  ButtonWidth := PanelLeft.Width - 28;

  FBtnAddCurrent := TButton.Create(Self);
  FBtnAddCurrent.Parent := PanelButtons;
  FBtnAddCurrent.Caption := rsAddCurrentVerse;
  FBtnAddCurrent.SetBounds(ButtonLeft, 4, ButtonWidth, 26);
  FBtnAddCurrent.Anchors := [akTop, akLeft, akRight];
  FBtnAddCurrent.Hint := rsHintAddCurrentVerse;
  FBtnAddCurrent.ShowHint := True;
  FBtnAddCurrent.OnClick := @OnAddCurrentClick;

  FBtnMoveUp := TButton.Create(Self);
  FBtnMoveUp.Parent := PanelButtons;
  FBtnMoveUp.Caption := rsMoveExampleUp;
  FBtnMoveUp.SetBounds(ButtonLeft, 38, ButtonWidth, 26);
  FBtnMoveUp.Anchors := [akTop, akLeft, akRight];
  FBtnMoveUp.Hint := rsHintMoveExampleUp;
  FBtnMoveUp.ShowHint := True;
  FBtnMoveUp.OnClick := @OnMoveUpClick;

  FBtnMoveDown := TButton.Create(Self);
  FBtnMoveDown.Parent := PanelButtons;
  FBtnMoveDown.Caption := rsMoveExampleDown;
  FBtnMoveDown.SetBounds(ButtonLeft, 72, ButtonWidth, 26);
  FBtnMoveDown.Anchors := [akTop, akLeft, akRight];
  FBtnMoveDown.Hint := rsHintMoveExampleDown;
  FBtnMoveDown.ShowHint := True;
  FBtnMoveDown.OnClick := @OnMoveDownClick;

  FBtnRemove := TButton.Create(Self);
  FBtnRemove.Parent := PanelButtons;
  FBtnRemove.Caption := rsRemoveExample;
  FBtnRemove.SetBounds(ButtonLeft, 106, ButtonWidth, 26);
  FBtnRemove.Anchors := [akTop, akLeft, akRight];
  FBtnRemove.Hint := rsHintRemoveExample;
  FBtnRemove.ShowHint := True;
  FBtnRemove.OnClick := @OnRemoveClick;

  FBtnClear := TButton.Create(Self);
  FBtnClear.Parent := PanelButtons;
  FBtnClear.Caption := rsClearExamples;
  FBtnClear.SetBounds(ButtonLeft, 140, ButtonWidth, 26);
  FBtnClear.Anchors := [akTop, akLeft, akRight];
  FBtnClear.Hint := rsHintClearExamples;
  FBtnClear.ShowHint := True;
  FBtnClear.OnClick := @OnClearClick;

  FListBox := TListBox.Create(Self);
  FListBox.Parent := PanelLeft;
  FListBox.Align := alClient;
  FListBox.IntegralHeight := False;
  FListBox.OnSelectionChange := @OnListSelectionChange;

  SplitterLeft := TSplitter.Create(Self);
  SplitterLeft.Parent := Self;
  SplitterLeft.Align := alLeft;

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align := alClient;
  FPageControl.BorderSpacing.Around := 8;

  FTabVisual := TTabSheet.Create(Self);
  FTabVisual.PageControl := FPageControl;
  FTabVisual.Caption := rsVisualPreview;

  FTabExpected := TTabSheet.Create(Self);
  FTabExpected.PageControl := FPageControl;
  FTabExpected.Caption := rsPromptResponse;

  PanelVisual := TPanel.Create(Self);
  PanelVisual.Parent := FTabVisual;
  PanelVisual.Align := alClient;
  PanelVisual.BevelOuter := bvNone;

  FStatusLabel := TLabel.Create(Self);
  FStatusLabel.Parent := PanelVisual;
  FStatusLabel.Align := alBottom;
  FStatusLabel.Height := 36;
  FStatusLabel.WordWrap := True;
  FStatusLabel.Layout := tlCenter;
  FStatusLabel.Visible := False;

  PanelTarget := TPanel.Create(Self);
  PanelTarget.Parent := PanelVisual;
  PanelTarget.Align := alClient;
  PanelTarget.BevelOuter := bvNone;

  FScrollBoxTarget := TScrollBox.Create(Self);
  FScrollBoxTarget.Parent := PanelTarget;
  FScrollBoxTarget.Align := alClient;
  FScrollBoxTarget.Constraints.MinHeight := 90;

  Lbl := TLabel.Create(Self);
  Lbl.Parent := PanelTarget;
  Lbl.Align := alTop;
  Lbl.Height := 22;
  Lbl.Caption := rsDestinationText;
  Lbl.Layout := tlCenter;

  SplitterVerses := TSplitter.Create(Self);
  SplitterVerses.Parent := PanelVisual;
  SplitterVerses.Align := alTop;
  SplitterVerses.ResizeAnchor := akTop;

  PanelSource := TPanel.Create(Self);
  PanelSource.Parent := PanelVisual;
  PanelSource.Align := alTop;
  PanelSource.Height := 240;
  PanelSource.Constraints.MinHeight := 100;
  PanelSource.BevelOuter := bvNone;

  FScrollBoxSource := TScrollBox.Create(Self);
  FScrollBoxSource.Parent := PanelSource;
  FScrollBoxSource.Align := alClient;
  FScrollBoxSource.Constraints.MinHeight := 90;

  Lbl := TLabel.Create(Self);
  Lbl.Parent := PanelSource;
  Lbl.Align := alTop;
  Lbl.Height := 22;
  Lbl.Caption := rsSourceText;
  Lbl.Layout := tlCenter;

  FMemoExpected := TMemo.Create(Self);
  FMemoExpected.Parent := FTabExpected;
  FMemoExpected.Align := alClient;
  FMemoExpected.ReadOnly := True;
  FMemoExpected.ScrollBars := ssAutoVertical;
  FMemoExpected.WordWrap := True;
end;

procedure TFrmFlowExamples.CreateVersePreview;
begin
  FSourceVerse := TVerse.Create(FScrollBoxSource);
  FTargetVerse := TVerse.Create(FScrollBoxTarget);
  FSourceVerse.VersiculoPar := FTargetVerse;
  FSourceVerse.ReadOnly := True;
  FTargetVerse.ReadOnly := True;
  FSourceVerse.Ativo := True;
  FTargetVerse.Ativo := True;
end;

procedure TFrmFlowExamples.ConfigureVersePreview;
begin
  if not Assigned(FProject) then
    Exit;

  FSourceVerse.TokenizerVersion := FProject.TokenizerVersion;
  FTargetVerse.TokenizerVersion := FProject.TokenizerVersion;
  if Assigned(FProject.GetTextFont(tbOrigem)) then
    FSourceVerse.Fonte := FProject.GetTextFont(tbOrigem);
  if Assigned(FProject.GetTextFont(tbDestino)) then
    FTargetVerse.Fonte := FProject.GetTextFont(tbDestino);
  FSourceVerse.PalavrasComStrongEmNegrito := FProject.PalavrasComStrongEmNegrito;
  FSourceVerse.StrongsCountMode := FProject.MostrarQtdStrongs;
  FTargetVerse.StrongsCountMode := FProject.MostrarQtdStrongs;
end;

procedure TFrmFlowExamples.SetProjectAndFlow(AProject: TProject;
  const AFlowId: string; const Examples: TAIFlowExampleArray);
begin
  FProject := AProject;
  FFlowId := AFlowId;
  FExamples := Copy(Examples);
  Caption := Format(rsFlowExamplesTitle, [FlowDisplayName]);
  ConfigureVersePreview;
  PopulateList;
  SelectExample(0);
end;

procedure TFrmFlowExamples.PopulateList;
var
  I: Integer;
begin
  FListBox.Items.BeginUpdate;
  try
    FListBox.Items.Clear;
    for I := 0 to High(FExamples) do
      FListBox.Items.Add(FormatFlowExampleSummary(FExamples[I]));
  finally
    FListBox.Items.EndUpdate;
  end;
  UpdateButtons;
end;

procedure TFrmFlowExamples.SelectExample(Index: Integer);
begin
  if Length(FExamples) = 0 then
    FListBox.ItemIndex := -1
  else if Index < 0 then
    FListBox.ItemIndex := 0
  else if Index > High(FExamples) then
    FListBox.ItemIndex := High(FExamples)
  else
    FListBox.ItemIndex := Index;
  LoadSelectedExample;
end;

procedure TFrmFlowExamples.LoadSelectedExample;
var
  Index: Integer;
begin
  Index := FListBox.ItemIndex;
  FMemoExpected.Clear;
  FStatusLabel.Visible := False;

  if (Index < 0) or (Index > High(FExamples)) then
  begin
    FSourceVerse.Texto := '';
    FTargetVerse.Texto := '';
    FMemoExpected.Text := rsNoExamples;
    ShowPreviewStatus(rsNoExamples, clGray);
    UpdateButtons;
    Exit;
  end;

  FSourceVerse.Texto := SourcePreviewText(Index);
  FTargetVerse.Texto := TargetPreviewText(Index);
  FSourceVerse.LimparAssociacoes;
  FMemoExpected.Text := FExamples[Index].ExpectedResponse;

  if IsAssociationFlow then
  begin
    if HasVisualAssociationData(Index) then
    begin
      FSourceVerse.Pares := FExamples[Index].PairsXml;
      ShowPreviewStatus(rsPreviewReady, clGreen);
    end
    else
      ShowPreviewStatus(rsPreviewNoVisualAssociations, clGray);
  end
  else
    ShowPreviewStatus(rsPreviewReady, clGreen);

  UpdateButtons;
end;

function TFrmFlowExamples.SourcePreviewText(Index: Integer): string;
begin
  Result := '';
  if (Index < 0) or (Index > High(FExamples)) then
    Exit;

  Result := FExamples[Index].SourceXml;
  if Result = '' then
    Result := FExamples[Index].SourceText;
end;

function TFrmFlowExamples.TargetPreviewText(Index: Integer): string;
begin
  Result := '';
  if (Index < 0) or (Index > High(FExamples)) then
    Exit;

  if IsAssociationFlow then
  begin
    Result := FExamples[Index].TargetXml;
    if Result = '' then
      Result := FExamples[Index].TargetText;
  end
  else
  begin
    Result := FExamples[Index].TargetXml;
    if Result = '' then
      Result := FExamples[Index].ExpectedResponse;
  end;
end;

function TFrmFlowExamples.HasVisualAssociationData(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index <= High(FExamples)) and
    (Trim(FExamples[Index].PairsXml) <> '') and
    (Trim(FExamples[Index].SourceXml) <> '') and
    (Trim(FExamples[Index].TargetXml) <> '');
end;

procedure TFrmFlowExamples.UpdateButtons;
var
  Index, Count: Integer;
begin
  Count := Length(FExamples);
  Index := FListBox.ItemIndex;
  FBtnMoveUp.Enabled := (Count > 1) and (Index > 0);
  FBtnMoveDown.Enabled := (Count > 1) and (Index >= 0) and (Index < Count - 1);
  FBtnRemove.Enabled := (Count > 0) and (Index >= 0);
  FBtnClear.Enabled := Count > 0;
  FBtnAddCurrent.Enabled := Assigned(FProject);
end;

procedure TFrmFlowExamples.OnListSelectionChange(Sender: TObject; User: Boolean);
begin
  LoadSelectedExample;
end;

procedure TFrmFlowExamples.OnAddCurrentClick(Sender: TObject);
var
  Example: TAIFlowExample;
  ErrorMessage: string;
  Index: Integer;
begin
  if not Assigned(FProject) then
    Exit;

  if not FProject.TryCreateCurrentVerseFlowExample(FFlowId, Example, ErrorMessage) then
  begin
    MessageDlg(rsWarningTitle, ErrorMessage, mtWarning, [mbOK], 0);
    Exit;
  end;

  Index := IndexOfFlowExampleByReference(FExamples, Example.ReferenceId);
  if Index >= 0 then
  begin
    if MessageDlg(rsReplaceExampleTitle,
        Format(rsReplaceExamplePrompt, [FlowDisplayName]),
        mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
    FExamples[Index] := Example;
  end
  else
  begin
    SetLength(FExamples, Length(FExamples) + 1);
    Index := High(FExamples);
    FExamples[Index] := Example;
  end;

  PopulateList;
  SelectExample(Index);
end;

procedure TFrmFlowExamples.OnMoveUpClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := FListBox.ItemIndex;
  if Index <= 0 then
    Exit;
  FExamples := MoveFlowExample(FExamples, Index, Index - 1);
  PopulateList;
  SelectExample(Index - 1);
end;

procedure TFrmFlowExamples.OnMoveDownClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := FListBox.ItemIndex;
  if (Index < 0) or (Index >= High(FExamples)) then
    Exit;
  FExamples := MoveFlowExample(FExamples, Index, Index + 1);
  PopulateList;
  SelectExample(Index + 1);
end;

procedure TFrmFlowExamples.OnRemoveClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := FListBox.ItemIndex;
  if (Index < 0) or (Index > High(FExamples)) then
    Exit;
  FExamples := RemoveFlowExample(FExamples, FExamples[Index].ReferenceId);
  PopulateList;
  SelectExample(Index);
end;

procedure TFrmFlowExamples.OnClearClick(Sender: TObject);
begin
  if Length(FExamples) = 0 then
    Exit;
  if MessageDlg(rsClearExamplesTitle,
      Format(rsClearExamplesPrompt, [FlowDisplayName]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  FExamples := nil;
  PopulateList;
  SelectExample(-1);
end;

function TFrmFlowExamples.FlowDisplayName: string;
var
  FlowConfig: IAIFlowConfig;
begin
  Result := FFlowId;
  try
    FlowConfig := GetFlowConfigById(FFlowId);
    if Assigned(FlowConfig) then
      Result := FlowConfig.GetDisplayName;
  except
    // Keep the flow id as a fallback for unexpected custom flows.
  end;
end;

function TFrmFlowExamples.IsAssociationFlow: Boolean;
begin
  Result := SameText(FFlowId, 'association');
end;

procedure TFrmFlowExamples.ShowPreviewStatus(const AMessage: string; AColor: TColor);
begin
  FStatusLabel.Caption := AMessage;
  FStatusLabel.Font.Color := AColor;
  FStatusLabel.Visible := Trim(AMessage) <> '';
end;

end.
