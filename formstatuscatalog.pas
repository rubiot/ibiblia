unit FormStatusCatalog;

{$mode objfpc}{$H+}

interface

uses
  ui_style, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Grids, status_catalog;

type
  TFrmStatusCatalog = class(TForm)
  private
    FGrid: TStringGrid;
    FInfoLabel: TLabel;
    FAddButton: TButton;
    FRenameButton: TButton;
    FRemoveButton: TButton;
    FColorButton: TButton;
    FDisplayUpButton: TButton;
    FDisplayDownButton: TButton;
    FOkButton: TBitBtn;
    FCancelButton: TBitBtn;
    FColorDialog: TColorDialog;
    FDefinitions: TStatusDefinitions;
    FCounts: TStatusCounts;
    FRemaps: TStatusRemaps;
    constructor CreateEditor(AOwner: TComponent;
      const Definitions: TStatusDefinitions; const Counts: TStatusCounts);
    function CountForStatus(StatusId: Integer): Integer;
    function DefinitionIndexById(StatusId: Integer): Integer;
    function SelectedDefinitionIndex: Integer;
    function PromptRemapStatus(const RemovedStatus: TStatusDefinition;
      out NewStatusId: Integer): Boolean;
    procedure AddRemap(OldStatusId, NewStatusId: Integer);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: string);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure ReadGridCaptions;
    procedure RefreshGrid;
    procedure AddButtonClick(Sender: TObject);
    procedure RenameButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure ColorButtonClick(Sender: TObject);
    procedure DisplayUpButtonClick(Sender: TObject);
    procedure DisplayDownButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure Translate;
  public
    class function Execute(var Definitions: TStatusDefinitions;
      const Counts: TStatusCounts; out Remaps: TStatusRemaps): Boolean;
  public
    procedure AfterConstruction; override;
  end;

resourcestring
  SStatusCatalogWindowTitle = 'Status catalog editor';
  SStatusCatalogInfo =
    'Edit descriptions, colors and display order. Verse status IDs stay stable.';
  SStatusCatalogName = 'Description';
  SStatusCatalogColor = 'Color';
  SStatusCatalogDisplay = 'Display';
  SStatusCatalogUsed = 'Used';
  SStatusCatalogAdd = 'Add';
  SStatusCatalogRename = 'Rename...';
  SStatusCatalogRemove = 'Remove';
  SStatusCatalogChooseColor = 'Color...';
  SStatusCatalogDisplayUp = 'Display up';
  SStatusCatalogDisplayDown = 'Display down';
  SStatusCatalogCannotRemoveLast = 'At least one status is required.';
  SStatusCatalogMaxReached = 'At most %d statuses are allowed.';
  SStatusCatalogRemapTitle = 'Remap status';
  SStatusCatalogRemapMessage =
    'The status "%s" is used by %d verses. Choose another status for those verses before removing it.';
  SStatusCatalogNewStatus = 'Status %d';
  SStatusCatalogRenameTitle = 'Rename status';
  SStatusCatalogRenamePrompt = 'Description:';
  SStatusCatalogNameHint =
    'Status description shown in selectors, menus, legends and tree hints. Editable.';
  SStatusCatalogColorHint =
    'Status color, shown as #RRGGBB. Edit the value, double-click, or use the Color button.';
  SStatusCatalogDisplayHint =
    'Visual order used in selectors, menus, legends and aggregate status bars. Change it with Display up/down.';
  SStatusCatalogUsedHint =
    'Number of verses currently using this status in this project. Read-only.';

implementation

constructor TFrmStatusCatalog.CreateEditor(AOwner: TComponent;
  const Definitions: TStatusDefinitions; const Counts: TStatusCounts);
var
  ButtonPanel: TPanel;
begin
  inherited CreateNew(AOwner, 1);
  ApplyFormStyle(Self);
  Caption := SStatusCatalogWindowTitle;
  Position := poMainFormCenter;
  ClientWidth := 840;
  ClientHeight := 528;
  Constraints.MinWidth := 840;
  Constraints.MinHeight := 528;
  BorderIcons := [biSystemMenu];

  FDefinitions := Copy(Definitions, 0, Length(Definitions));
  FCounts := Copy(Counts, 0, Length(Counts));
  FRemaps := nil;
  NormalizeStatusDefinitions(FDefinitions, False);

  FInfoLabel := TLabel.Create(Self);
  FInfoLabel.Parent := Self;
  FInfoLabel.Left := 16;
  FInfoLabel.Top := 16;
  FInfoLabel.Width := 808;
  FInfoLabel.Anchors := [akTop, akLeft, akRight];
  FInfoLabel.Height := 48;
  FInfoLabel.AutoSize := False;
  FInfoLabel.WordWrap := True;

  FGrid := TStringGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Left := 16;
  FGrid.Top := 80;
  FGrid.Width := 624;
  FGrid.Height := 384;
  FGrid.Anchors := [akTop, akLeft, akRight, akBottom];
  FGrid.ColCount := 4;
  FGrid.FixedCols := 0;
  FGrid.FixedRows := 1;
  FGrid.DefaultDrawing := False;
  FGrid.OnDrawCell := @GridDrawCell;
  FGrid.OnGetCellHint := @GridGetCellHint;
  FGrid.OnMouseDown := @GridMouseDown;
  FGrid.OnSelectEditor := @GridSelectEditor;
  FGrid.ShowHint := True;
  FGrid.CellHintPriority := chpAllNoDefault;
  FGrid.Options := FGrid.Options + [goEditing, goCellHints];
  FGrid.ColWidths[0] := 264;
  FGrid.ColWidths[1] := 144;
  FGrid.ColWidths[2] := 96;
  FGrid.ColWidths[3] := 96;

  ButtonPanel := TPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.Left := 656;
  ButtonPanel.Top := 80;
  ButtonPanel.Width := 168;
  ButtonPanel.Height := 384;
  ButtonPanel.Anchors := [akTop, akRight, akBottom];
  ButtonPanel.BevelOuter := bvNone;

  FAddButton := TButton.Create(Self);
  FAddButton.Parent := ButtonPanel;
  FAddButton.SetBounds(0, 0, 168, 32);
  FAddButton.OnClick := @AddButtonClick;

  FRenameButton := TButton.Create(Self);
  FRenameButton.Parent := ButtonPanel;
  FRenameButton.SetBounds(0, 40, 168, 32);
  FRenameButton.OnClick := @RenameButtonClick;

  FRemoveButton := TButton.Create(Self);
  FRemoveButton.Parent := ButtonPanel;
  FRemoveButton.SetBounds(0, 80, 168, 32);
  FRemoveButton.OnClick := @RemoveButtonClick;

  FColorButton := TButton.Create(Self);
  FColorButton.Parent := ButtonPanel;
  FColorButton.SetBounds(0, 120, 168, 32);
  FColorButton.OnClick := @ColorButtonClick;

  FDisplayUpButton := TButton.Create(Self);
  FDisplayUpButton.Parent := ButtonPanel;
  FDisplayUpButton.SetBounds(0, 176, 168, 32);
  FDisplayUpButton.OnClick := @DisplayUpButtonClick;

  FDisplayDownButton := TButton.Create(Self);
  FDisplayDownButton.Parent := ButtonPanel;
  FDisplayDownButton.SetBounds(0, 216, 168, 32);
  FDisplayDownButton.OnClick := @DisplayDownButtonClick;

  FOkButton := TBitBtn.Create(Self);
  FOkButton.Parent := Self;
  FOkButton.Kind := bkOK;
  FOkButton.Left := 608;
  FOkButton.Top := 480;
  FOkButton.Width := 104;
  FOkButton.Anchors := [akRight, akBottom];
  FOkButton.OnClick := @OkButtonClick;

  FCancelButton := TBitBtn.Create(Self);
  FCancelButton.Parent := Self;
  FCancelButton.Kind := bkCancel;
  FCancelButton.Left := 720;
  FCancelButton.Top := 480;
  FCancelButton.Width := 104;
  FCancelButton.Anchors := [akRight, akBottom];

  FColorDialog := TColorDialog.Create(Self);
  Translate;
  RefreshGrid;
end;

procedure TFrmStatusCatalog.Translate;
begin
  Caption := SStatusCatalogWindowTitle;
  FInfoLabel.Caption := SStatusCatalogInfo;
  FAddButton.Caption := SStatusCatalogAdd;
  FRenameButton.Caption := SStatusCatalogRename;
  FRemoveButton.Caption := SStatusCatalogRemove;
  FColorButton.Caption := SStatusCatalogChooseColor;
  FDisplayUpButton.Caption := SStatusCatalogDisplayUp;
  FDisplayDownButton.Caption := SStatusCatalogDisplayDown;
end;

function TFrmStatusCatalog.CountForStatus(StatusId: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FCounts) do
    if FCounts[I].StatusId = StatusId then
      Exit(FCounts[I].Count);
end;

function TFrmStatusCatalog.DefinitionIndexById(StatusId: Integer): Integer;
begin
  Result := IndexOfStatusId(FDefinitions, StatusId);
end;

function TFrmStatusCatalog.SelectedDefinitionIndex: Integer;
begin
  Result := -1;
  if (FGrid.Row <= 0) or (FGrid.Row > Length(FDefinitions)) then
    Exit;
  Result := FGrid.Row - 1;
end;

procedure TFrmStatusCatalog.AddRemap(OldStatusId, NewStatusId: Integer);
begin
  SetLength(FRemaps, Length(FRemaps) + 1);
  FRemaps[High(FRemaps)].OldStatusId := OldStatusId;
  FRemaps[High(FRemaps)].NewStatusId := NewStatusId;
end;

procedure TFrmStatusCatalog.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  TextLeft: Integer;
  TextTop: Integer;
  SwatchRect: TRect;
  CellText: string;
begin
  if not (Sender is TStringGrid) then
    Exit;

  if gdFixed in aState then
  begin
    FGrid.Canvas.Brush.Color := UISurfaceColor;
    FGrid.Canvas.Font.Color := clWindowText;
  end
  else if gdSelected in aState then
  begin
    FGrid.Canvas.Brush.Color := clHighlight;
    FGrid.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    FGrid.Canvas.Brush.Color := clWindow;
    FGrid.Canvas.Font.Color := clWindowText;
  end;
  FGrid.Canvas.FillRect(aRect);

  TextLeft := aRect.Left + 4;
  CellText := FGrid.Cells[aCol, aRow];

  if (aCol = 1) and (aRow > 0) and (aRow - 1 <= High(FDefinitions)) then
  begin
    SwatchRect := Rect(aRect.Left + 6, aRect.Top + 4, aRect.Left + 24,
      aRect.Bottom - 4);
    FGrid.Canvas.Brush.Color := FDefinitions[aRow - 1].Color;
    FGrid.Canvas.Pen.Color := clGray;
    FGrid.Canvas.Rectangle(SwatchRect);
    TextLeft := aRect.Left + 30;

    if gdSelected in aState then
      FGrid.Canvas.Font.Color := clHighlightText
    else
      FGrid.Canvas.Font.Color := clWindowText;
    FGrid.Canvas.Brush.Style := bsClear;
  end;

  TextTop := aRect.Top + ((aRect.Bottom - aRect.Top -
    FGrid.Canvas.TextHeight(CellText)) div 2);
  FGrid.Canvas.TextRect(aRect, TextLeft, TextTop, CellText);
  FGrid.Canvas.Brush.Style := bsSolid;
  FGrid.Canvas.Pen.Color := clSilver;
  FGrid.Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
  FGrid.Canvas.Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
end;

procedure TFrmStatusCatalog.GridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: string);
begin
  case ACol of
    0: HintText := SStatusCatalogNameHint;
    1: HintText := SStatusCatalogColorHint;
    2: HintText := SStatusCatalogDisplayHint;
    3: HintText := SStatusCatalogUsedHint;
  else
    HintText := '';
  end;
end;

procedure TFrmStatusCatalog.GridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CellCol: Longint;
  CellRow: Longint;
begin
  if (Button <> mbLeft) or not (ssDouble in Shift) then
    Exit;

  FGrid.MouseToCell(X, Y, CellCol, CellRow);
  if (CellCol <> 1) or (CellRow <= 0) or (CellRow > Length(FDefinitions)) then
    Exit;

  FGrid.Col := CellCol;
  FGrid.Row := CellRow;
  ColorButtonClick(Sender);
end;

procedure TFrmStatusCatalog.GridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if (aRow = 0) or not (aCol in [0, 1]) then
    Editor := nil;
end;

procedure TFrmStatusCatalog.ReadGridCaptions;
var
  RowIndex: Integer;
begin
  for RowIndex := 1 to FGrid.RowCount - 1 do
    if RowIndex - 1 <= High(FDefinitions) then
    begin
      FDefinitions[RowIndex - 1].Caption := Trim(FGrid.Cells[0, RowIndex]);
      FDefinitions[RowIndex - 1].Color := StringToStatusColor(
        FGrid.Cells[1, RowIndex], FDefinitions[RowIndex - 1].Color);
    end;
end;

procedure TFrmStatusCatalog.RefreshGrid;
var
  RowIndex: Integer;
begin
  FDefinitions := SortStatusDefinitionsByDisplayOrder(FDefinitions);
  FGrid.RowCount := Length(FDefinitions) + 1;
  FGrid.Cells[0, 0] := SStatusCatalogName;
  FGrid.Cells[1, 0] := SStatusCatalogColor;
  FGrid.Cells[2, 0] := SStatusCatalogDisplay;
  FGrid.Cells[3, 0] := SStatusCatalogUsed;

  for RowIndex := 0 to High(FDefinitions) do
  begin
    FGrid.Cells[0, RowIndex + 1] := FDefinitions[RowIndex].Caption;
    FGrid.Cells[1, RowIndex + 1] := StatusColorToString(FDefinitions[RowIndex].Color);
    FGrid.Cells[2, RowIndex + 1] := IntToStr(RowIndex + 1);
    FGrid.Cells[3, RowIndex + 1] := IntToStr(CountForStatus(FDefinitions[RowIndex].Id));
  end;
end;

procedure TFrmStatusCatalog.AddButtonClick(Sender: TObject);
var
  Index: Integer;
begin
  ReadGridCaptions;
  if Length(FDefinitions) >= STATUS_CATALOG_MAX_COUNT then
  begin
    MessageDlg(SStatusCatalogWindowTitle,
      Format(SStatusCatalogMaxReached, [STATUS_CATALOG_MAX_COUNT]),
      mtWarning, [mbOK], 0);
    Exit;
  end;

  Index := Length(FDefinitions);
  SetLength(FDefinitions, Index + 1);
  FDefinitions[Index].Id := MaxStatusId(FDefinitions) + 1;
  FDefinitions[Index].Caption := Format(SStatusCatalogNewStatus, [Index + 1]);
  FDefinitions[Index].Color := clSilver;
  FDefinitions[Index].DisplayOrder := Index;
  NormalizeStatusDefinitions(FDefinitions, False);
  RefreshGrid;
  FGrid.Row := Length(FDefinitions);
end;

procedure TFrmStatusCatalog.RenameButtonClick(Sender: TObject);
var
  Index: Integer;
  NewCaption: string;
begin
  ReadGridCaptions;
  Index := SelectedDefinitionIndex;
  if Index < 0 then
    Exit;

  NewCaption := FDefinitions[Index].Caption;
  if InputQuery(SStatusCatalogRenameTitle, SStatusCatalogRenamePrompt,
    NewCaption) then
  begin
    FDefinitions[Index].Caption := Trim(NewCaption);
    RefreshGrid;
    FGrid.Row := Index + 1;
    FGrid.Col := 0;
  end;
end;

function TFrmStatusCatalog.PromptRemapStatus(
  const RemovedStatus: TStatusDefinition; out NewStatusId: Integer): Boolean;
var
  DialogForm: TForm;
  MessageLabel: TLabel;
  Combo: TComboBox;
  OkButton: TBitBtn;
  CancelButton: TBitBtn;
  DefIndex: Integer;
begin
  Result := False;
  NewStatusId := FirstStatusId(FDefinitions);

  DialogForm := TForm.CreateNew(Self, 1);
  try
    DialogForm.Caption := SStatusCatalogRemapTitle;
    DialogForm.Position := poMainFormCenter;
    ApplyFormStyle(DialogForm);
    DialogForm.ClientWidth := 560;
    DialogForm.ClientHeight := 208;

    MessageLabel := TLabel.Create(DialogForm);
    MessageLabel.Parent := DialogForm;
    MessageLabel.Left := 16;
    MessageLabel.Top := 16;
    MessageLabel.Width := 528;
    MessageLabel.AutoSize := False;
    MessageLabel.Height := 64;
    MessageLabel.WordWrap := True;
    MessageLabel.Caption := Format(SStatusCatalogRemapMessage,
      [RemovedStatus.Caption, CountForStatus(RemovedStatus.Id)]);

    Combo := TComboBox.Create(DialogForm);
    Combo.Parent := DialogForm;
    Combo.Left := 16;
    Combo.Top := 96;
    Combo.Width := 528;
    Combo.Style := csDropDownList;
    for DefIndex := 0 to High(FDefinitions) do
      if FDefinitions[DefIndex].Id <> RemovedStatus.Id then
        Combo.Items.AddObject(FDefinitions[DefIndex].Caption,
          TObject(PtrUInt(FDefinitions[DefIndex].Id)));
    if Combo.Items.Count > 0 then
      Combo.ItemIndex := 0;

    OkButton := TBitBtn.Create(DialogForm);
    OkButton.Parent := DialogForm;
    OkButton.Kind := bkOK;
    OkButton.Left := 328;
    OkButton.Top := 160;
    OkButton.Width := 90;

    CancelButton := TBitBtn.Create(DialogForm);
    CancelButton.Parent := DialogForm;
    CancelButton.Kind := bkCancel;
    CancelButton.Left := 440;
    CancelButton.Top := 160;
    CancelButton.Width := 90;

    ApplyDialogStyle(DialogForm);
    if (DialogForm.ShowModal = mrOK) and (Combo.ItemIndex >= 0) then
    begin
      DefIndex := DefinitionIndexById(PtrUInt(Combo.Items.Objects[Combo.ItemIndex]));
      if DefIndex >= 0 then
      begin
        NewStatusId := FDefinitions[DefIndex].Id;
        Result := True;
      end;
    end;
  finally
    DialogForm.Free;
  end;
end;

procedure TFrmStatusCatalog.RemoveButtonClick(Sender: TObject);
var
  Index: Integer;
  RemovedStatus: TStatusDefinition;
  NewStatusId: Integer;
begin
  ReadGridCaptions;
  if Length(FDefinitions) <= 1 then
  begin
    MessageDlg(SStatusCatalogWindowTitle, SStatusCatalogCannotRemoveLast,
      mtWarning, [mbOK], 0);
    Exit;
  end;

  Index := SelectedDefinitionIndex;
  if Index < 0 then
    Exit;

  RemovedStatus := FDefinitions[Index];
  if CountForStatus(RemovedStatus.Id) > 0 then
  begin
    if not PromptRemapStatus(RemovedStatus, NewStatusId) then
      Exit;
    AddRemap(RemovedStatus.Id, NewStatusId);
  end;

  for Index := Index to High(FDefinitions) - 1 do
    FDefinitions[Index] := FDefinitions[Index + 1];
  SetLength(FDefinitions, Length(FDefinitions) - 1);
  NormalizeStatusDefinitions(FDefinitions, False);
  RefreshGrid;
end;

procedure TFrmStatusCatalog.ColorButtonClick(Sender: TObject);
var
  Index: Integer;
begin
  ReadGridCaptions;
  Index := SelectedDefinitionIndex;
  if Index < 0 then
    Exit;

  FColorDialog.Color := FDefinitions[Index].Color;
  if FColorDialog.Execute then
  begin
    FDefinitions[Index].Color := FColorDialog.Color;
    RefreshGrid;
  end;
end;

procedure TFrmStatusCatalog.DisplayUpButtonClick(Sender: TObject);
var
  Index: Integer;
  Tmp: Integer;
begin
  ReadGridCaptions;
  Index := SelectedDefinitionIndex;
  if Index <= 0 then
    Exit;
  Tmp := FDefinitions[Index].DisplayOrder;
  FDefinitions[Index].DisplayOrder := FDefinitions[Index - 1].DisplayOrder;
  FDefinitions[Index - 1].DisplayOrder := Tmp;
  NormalizeStatusDefinitions(FDefinitions, False);
  RefreshGrid;
  FGrid.Row := Index;
end;

procedure TFrmStatusCatalog.DisplayDownButtonClick(Sender: TObject);
var
  Index: Integer;
  Tmp: Integer;
begin
  ReadGridCaptions;
  Index := SelectedDefinitionIndex;
  if (Index < 0) or (Index >= High(FDefinitions)) then
    Exit;
  Tmp := FDefinitions[Index].DisplayOrder;
  FDefinitions[Index].DisplayOrder := FDefinitions[Index + 1].DisplayOrder;
  FDefinitions[Index + 1].DisplayOrder := Tmp;
  NormalizeStatusDefinitions(FDefinitions, False);
  RefreshGrid;
  FGrid.Row := Index + 2;
end;

procedure TFrmStatusCatalog.OkButtonClick(Sender: TObject);
var
  ErrorMessage: string;
begin
  ReadGridCaptions;
  NormalizeStatusDefinitions(FDefinitions, False);
  if not ValidateStatusDefinitions(FDefinitions, ErrorMessage) then
  begin
    MessageDlg(SStatusCatalogWindowTitle, ErrorMessage, mtWarning, [mbOK], 0);
    ModalResult := mrNone;
    RefreshGrid;
    Exit;
  end;
  ModalResult := mrOK;
end;

class function TFrmStatusCatalog.Execute(var Definitions: TStatusDefinitions;
  const Counts: TStatusCounts; out Remaps: TStatusRemaps): Boolean;
var
  Form: TFrmStatusCatalog;
begin
  Result := False;
  Remaps := nil;
  Form := TFrmStatusCatalog.CreateEditor(nil, Definitions, Counts);
  try
    if Form.ShowModal = mrOK then
    begin
      Definitions := Copy(Form.FDefinitions, 0, Length(Form.FDefinitions));
      Remaps := Copy(Form.FRemaps, 0, Length(Form.FRemaps));
      Result := True;
    end;
  finally
    Form.Free;
  end;
end;

procedure TFrmStatusCatalog.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
  StyleControl(FInfoLabel, uirHint);
end;


end.
