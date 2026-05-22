unit current_verse_workspace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Graphics,
  Verse, VerseMemo, commentsview, SuggestionManager, syntagm, project_persistence,
  verse_render_buffer;

type
  TCurrentVerseLoadResult = record
    Associations: string;
    Comments: string;
    Status: Integer;
    DestinationTextEmpty: Boolean;
  end;

  TCurrentVerseWorkspace = class
  private
    FOldPairs: TStringList;
    function GetVerseView(const VerseViews: array of TVerse; Index: Integer): TVerse;
    function GetTextFieldIndex(const TextFieldIndexes: array of Integer; Index: Integer): Integer;
    function ReadCommentsText(CommentsControl: TControl): string;
    procedure SaveComments(Persistence: TProjectPersistence; CommentsControl: TControl);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure InitializeVerseRuntime(var VerseViews: array of TVerse;
      var TextFieldIndexes: array of Integer);
    procedure DestroyVerseViews(var VerseViews: array of TVerse);
    procedure CreateVerseView(Owner: TScrollBox; VerseIndex,
      SourceVerseIndex, DestinationVerseIndex: Integer;
      var VerseViews: array of TVerse; OnVerseDoubleClick: TNotifyEvent;
      OnSyntagmClick: TOnSyntagmEvent; OnExportText: TOnExportTextEvent);
    procedure ApplyDisplayTags(const VerseViews: array of TVerse; DisplayTags: Boolean);
    procedure ApplyTokenizerVersion(const VerseViews: array of TVerse;
      TokenizerVersion: Integer);
    procedure ApplyStrongsCountMode(const VerseViews: array of TVerse;
      StrongsCountMode: TStrongsCountMode);
    procedure ApplyBoldStrongs(const VerseViews: array of TVerse; Enabled: Boolean);
    function ApplyEditedVerseText(const VerseViews: array of TVerse;
      EditedVerse: TVerse; const NewText: string): Integer;
    procedure SavePairs(Persistence: TProjectPersistence; SourceVerse: TVerse);
    procedure SaveTexts(Persistence: TProjectPersistence;
      const VerseViews: array of TVerse; const TextFieldIndexes: array of Integer);
    procedure RemoveOldAssociations(Suggester: TSuggestionManager; SourceVerse, DestinationVerse: TVerse);
    procedure SaveAssociations(Suggester: TSuggestionManager; SourceVerse, DestinationVerse: TVerse);
    procedure SaveBeforeScroll(Persistence: TProjectPersistence; Suggester: TSuggestionManager;
      VerseMemo: TVerseMemo; CommentsControl: TControl;
      const VerseViews: array of TVerse; const TextFieldIndexes: array of Integer;
      SourceVerseIndex, DestinationVerseIndex: Integer);
    function LoadCurrentVerse(Persistence: TProjectPersistence;
      const VerseViews: array of TVerse; const TextFieldIndexes: array of Integer;
      const FormattedReference, ReferenceID: string; SourceVerseIndex,
      DestinationVerseIndex: Integer): TCurrentVerseLoadResult;
    procedure SetCurrentVerseText(Persistence: TProjectPersistence;
      RenderBuffer: TVerseRenderBuffer; const TextFieldIndexes: array of Integer;
      const VerseText: string; TextIndex, SourceVerseIndex,
      DestinationVerseIndex: Integer; Replace, ResetStatus: Boolean;
      out CorruptionDetails: string);
    procedure ApplyLoadedAssociations(SourceVerse: TVerse; const Associations: string);
    procedure FinalizeLoadedVerse(const VerseViews: array of TVerse);
  end;

implementation

function TCurrentVerseWorkspace.GetVerseView(const VerseViews: array of TVerse;
  Index: Integer): TVerse;
begin
  if (Index < Low(VerseViews)) or (Index > High(VerseViews)) then
    raise Exception.CreateFmt('Current verse view index out of bounds [%d]', [Index]);

  Result := VerseViews[Index];
end;

function TCurrentVerseWorkspace.GetTextFieldIndex(
  const TextFieldIndexes: array of Integer; Index: Integer): Integer;
begin
  if (Index < Low(TextFieldIndexes)) or (Index > High(TextFieldIndexes)) then
    raise Exception.CreateFmt('Current verse text field index out of bounds [%d]', [Index]);

  Result := TextFieldIndexes[Index];
end;

function TCurrentVerseWorkspace.ReadCommentsText(CommentsControl: TControl): string;
begin
  Result := '';
  if not Assigned(CommentsControl) then
    Exit;

  if CommentsControl is TCommentsView then
    Result := TCommentsView(CommentsControl).Text
  else if CommentsControl is TMemo then
    Result := TMemo(CommentsControl).Text;
end;

procedure TCurrentVerseWorkspace.SaveComments(Persistence: TProjectPersistence;
  CommentsControl: TControl);
var
  CommentsText: string;
begin
  if not Assigned(Persistence) then
    Exit;

  CommentsText := ReadCommentsText(CommentsControl);
  if Persistence.CurrentComments <> CommentsText then
    Persistence.WriteCurrentComments(CommentsText);
end;

destructor TCurrentVerseWorkspace.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCurrentVerseWorkspace.Clear;
begin
  FreeAndNil(FOldPairs);
end;

procedure TCurrentVerseWorkspace.InitializeVerseRuntime(
  var VerseViews: array of TVerse; var TextFieldIndexes: array of Integer);
var
  Index: Integer;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
    VerseViews[Index] := nil;

  for Index := Low(TextFieldIndexes) to High(TextFieldIndexes) do
    TextFieldIndexes[Index] := 0;
end;

procedure TCurrentVerseWorkspace.DestroyVerseViews(
  var VerseViews: array of TVerse);
var
  Index: Integer;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
    if Assigned(VerseViews[Index]) then
    begin
      VerseViews[Index].Destroy;
      VerseViews[Index] := nil;
    end;
end;

procedure TCurrentVerseWorkspace.CreateVerseView(Owner: TScrollBox;
  VerseIndex, SourceVerseIndex, DestinationVerseIndex: Integer;
  var VerseViews: array of TVerse; OnVerseDoubleClick: TNotifyEvent;
  OnSyntagmClick: TOnSyntagmEvent; OnExportText: TOnExportTextEvent);
begin
  if (VerseIndex < Low(VerseViews)) or (VerseIndex > High(VerseViews)) then
    raise Exception.CreateFmt('Current verse view index out of bounds [%d]', [VerseIndex]);

  VerseViews[VerseIndex] := TVerse.Create(Owner);
  Owner.Tag := PtrInt(VerseViews[VerseIndex]);
  Owner.OnDblClick := OnVerseDoubleClick;
  VerseViews[VerseIndex].OnClick := OnSyntagmClick;
  VerseViews[VerseIndex].OnExportText := OnExportText;

  if (VerseIndex <> SourceVerseIndex) and (VerseIndex <> DestinationVerseIndex) then
    VerseViews[VerseIndex].CorDesassociado := clWindowText;

  if (VerseIndex = DestinationVerseIndex) and Assigned(GetVerseView(VerseViews, SourceVerseIndex)) then
    VerseViews[DestinationVerseIndex].VersiculoPar := GetVerseView(VerseViews, SourceVerseIndex)
  else if (VerseIndex = SourceVerseIndex) and Assigned(GetVerseView(VerseViews, DestinationVerseIndex)) then
    VerseViews[SourceVerseIndex].VersiculoPar := GetVerseView(VerseViews, DestinationVerseIndex);
end;

procedure TCurrentVerseWorkspace.ApplyDisplayTags(
  const VerseViews: array of TVerse; DisplayTags: Boolean);
var
  Index: Integer;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
    if Assigned(VerseViews[Index]) then
      if DisplayTags then
        VerseViews[Index].MostrarTags
      else
        VerseViews[Index].OcultarTags;
end;

procedure TCurrentVerseWorkspace.ApplyTokenizerVersion(
  const VerseViews: array of TVerse; TokenizerVersion: Integer);
var
  Index: Integer;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
    if Assigned(VerseViews[Index]) then
      VerseViews[Index].TokenizerVersion := TokenizerVersion;
end;

procedure TCurrentVerseWorkspace.ApplyStrongsCountMode(
  const VerseViews: array of TVerse; StrongsCountMode: TStrongsCountMode);
var
  Index: Integer;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
    if Assigned(VerseViews[Index]) then
      VerseViews[Index].StrongsCountMode := StrongsCountMode;
end;

procedure TCurrentVerseWorkspace.ApplyBoldStrongs(
  const VerseViews: array of TVerse; Enabled: Boolean);
var
  Index: Integer;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
    if Assigned(VerseViews[Index]) then
      VerseViews[Index].PalavrasComStrongEmNegrito := Enabled;
end;

function TCurrentVerseWorkspace.ApplyEditedVerseText(
  const VerseViews: array of TVerse; EditedVerse: TVerse;
  const NewText: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Low(VerseViews) to High(VerseViews) do
    if VerseViews[Index] = EditedVerse then
    begin
      if Assigned(EditedVerse.VersiculoPar) then
        EditedVerse.AlterarTexto(NewText)
      else
        EditedVerse.Texto := NewText;
      Exit(Index);
    end;
end;

procedure TCurrentVerseWorkspace.SavePairs(Persistence: TProjectPersistence;
  SourceVerse: TVerse);
begin
  if not Assigned(Persistence) or not Assigned(SourceVerse) then
    Exit;

  Persistence.WriteCurrentPairs(SourceVerse.Pares);
end;

procedure TCurrentVerseWorkspace.SaveTexts(Persistence: TProjectPersistence;
  const VerseViews: array of TVerse; const TextFieldIndexes: array of Integer);
var
  Index: Integer;
  VerseView: TVerse;
begin
  if not Assigned(Persistence) then
    Exit;

  for Index := Low(VerseViews) to High(VerseViews) do
  begin
    VerseView := VerseViews[Index];
    if Assigned(VerseView) and VerseView.XMLModificado then
    begin
      Persistence.BeginCurrentEdit;
      Persistence.SetCurrentText(GetTextFieldIndex(TextFieldIndexes, Index), VerseView.XML);
      Persistence.PostCurrentEdit;
      VerseView.Modificado := False;
      VerseView.XMLModificado := False;
    end;
  end;
end;

procedure TCurrentVerseWorkspace.RemoveOldAssociations(Suggester: TSuggestionManager;
  SourceVerse, DestinationVerse: TVerse);
var
  PairIndex: Integer;
begin
  if not Assigned(Suggester) or not Assigned(SourceVerse) or not Assigned(DestinationVerse) or
     not Assigned(FOldPairs) then
    Exit;

  for PairIndex := 0 to FOldPairs.Count - 1 do
    if (PairIndex mod 2) = 0 then
      Suggester.RemovePair(FOldPairs.Strings[PairIndex], FOldPairs.Strings[PairIndex + 1]);
end;

procedure TCurrentVerseWorkspace.SaveAssociations(Suggester: TSuggestionManager;
  SourceVerse, DestinationVerse: TVerse);
var
  Pairs: TStringList;
  PairIndex: Integer;
begin
  if not Assigned(Suggester) or not Assigned(SourceVerse) or not Assigned(DestinationVerse) then
    Exit;

  if Assigned(FOldPairs) then
    RemoveOldAssociations(Suggester, SourceVerse, DestinationVerse);

  Pairs := SourceVerse.GetListaPares(plMetaData);
  try
    for PairIndex := 0 to Pairs.Count - 1 do
      if (PairIndex mod 2) = 0 then
        Suggester.AddPair(Pairs.Strings[PairIndex], Pairs.Strings[PairIndex + 1]);
  finally
    Pairs.Free;
  end;
end;

procedure TCurrentVerseWorkspace.SaveBeforeScroll(Persistence: TProjectPersistence;
  Suggester: TSuggestionManager; VerseMemo: TVerseMemo;
  CommentsControl: TControl; const VerseViews: array of TVerse;
  const TextFieldIndexes: array of Integer; SourceVerseIndex,
  DestinationVerseIndex: Integer);
var
  SourceVerse: TVerse;
begin
  SourceVerse := GetVerseView(VerseViews, SourceVerseIndex);
  if not Assigned(SourceVerse) then
    Exit;

  if Assigned(VerseMemo) then
    VerseMemo.Deactivate;

  if SourceVerse.Modificado then
  begin
    SaveAssociations(Suggester, SourceVerse, GetVerseView(VerseViews, DestinationVerseIndex));
    SavePairs(Persistence, SourceVerse);
  end;

  SaveTexts(Persistence, VerseViews, TextFieldIndexes);
  Clear;
  SaveComments(Persistence, CommentsControl);
end;

function TCurrentVerseWorkspace.LoadCurrentVerse(Persistence: TProjectPersistence;
  const VerseViews: array of TVerse; const TextFieldIndexes: array of Integer;
  const FormattedReference, ReferenceID: string; SourceVerseIndex,
  DestinationVerseIndex: Integer): TCurrentVerseLoadResult;
var
  Index: Integer;
  VerseView: TVerse;
begin
  Result.Associations := '';
  Result.Comments := '';
  Result.Status := 0;
  Result.DestinationTextEmpty := False;

  if not Assigned(Persistence) then
    Exit;

  for Index := Low(VerseViews) to High(VerseViews) do
  begin
    VerseView := VerseViews[Index];
    if Assigned(VerseView) then
    begin
      VerseView.Ativo := False;
      VerseView.Texto := Persistence.CurrentText(GetTextFieldIndex(TextFieldIndexes, Index));
      VerseView.Referencia := FormattedReference;
      VerseView.ReferenciaID := ReferenceID;
    end;
  end;

  Result.Associations := Persistence.CurrentPairs;
  Result.Comments := Persistence.CurrentComments;
  Result.Status := Persistence.CurrentStatus;
  Result.DestinationTextEmpty := Persistence.CurrentText(
    GetTextFieldIndex(TextFieldIndexes, DestinationVerseIndex)).IsEmpty;

  if not Assigned(GetVerseView(VerseViews, SourceVerseIndex)) then
    Clear;
end;

procedure TCurrentVerseWorkspace.SetCurrentVerseText(
  Persistence: TProjectPersistence; RenderBuffer: TVerseRenderBuffer;
  const TextFieldIndexes: array of Integer; const VerseText: string; TextIndex,
  SourceVerseIndex, DestinationVerseIndex: Integer; Replace,
  ResetStatus: Boolean; out CorruptionDetails: string);
var
  CurrentPairs: string;
  UpdatedPairs: string;
  ParticipatesInPairs: Boolean;
  SourceTextFieldIndex: Integer;
  DestinationTextFieldIndex: Integer;
begin
  CorruptionDetails := '';
  if not Assigned(Persistence) then
    Exit;

  Persistence.BeginCurrentEdit;

  SourceTextFieldIndex := GetTextFieldIndex(TextFieldIndexes, SourceVerseIndex);
  DestinationTextFieldIndex := GetTextFieldIndex(TextFieldIndexes, DestinationVerseIndex);
  ParticipatesInPairs := (TextIndex = SourceVerseIndex) or (TextIndex = DestinationVerseIndex);
  CurrentPairs := Persistence.CurrentPairs;

  if Replace and ParticipatesInPairs and not CurrentPairs.IsEmpty then
  begin
    if Assigned(RenderBuffer) and RenderBuffer.TryReplaceTextKeepingAssociations(
        Persistence.CurrentText(SourceTextFieldIndex),
        Persistence.CurrentText(DestinationTextFieldIndex),
        CurrentPairs,
        TextIndex = SourceVerseIndex,
        VerseText,
        UpdatedPairs,
        CorruptionDetails) then
      Persistence.SetCurrentFieldAsString('pare_pares', UpdatedPairs);
  end;

  Persistence.SetCurrentText(GetTextFieldIndex(TextFieldIndexes, TextIndex), VerseText);
  if not Replace and ParticipatesInPairs then
  begin
    Persistence.SetCurrentFieldAsString('pare_pares', '');
    if ResetStatus then
      Persistence.SetCurrentFieldAsInteger('pare_situacao', 0);
  end;

  Persistence.PostCurrentEdit;
end;

procedure TCurrentVerseWorkspace.ApplyLoadedAssociations(SourceVerse: TVerse;
  const Associations: string);
begin
  if not Assigned(SourceVerse) then
  begin
    Clear;
    Exit;
  end;

  SourceVerse.Pares := Associations;

  Clear;
  FOldPairs := SourceVerse.GetListaPares(plMetaData);
end;

procedure TCurrentVerseWorkspace.FinalizeLoadedVerse(
  const VerseViews: array of TVerse);
var
  Index: Integer;
  VerseView: TVerse;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
  begin
    VerseView := VerseViews[Index];
    if Assigned(VerseView) then
    begin
      VerseView.Modificado := False;
      VerseView.XMLModificado := False;
      VerseView.Ativo := True;
      VerseView.Renderizar;
    end;
  end;
end;

end.