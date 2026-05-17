unit current_verse_workspace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls,
  versiculo, memoversiculo, commentsview, sugestao, syntagm, project_persistence;

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
    function GetVerseView(const VerseViews: array of TVersiculo; Index: Integer): TVersiculo;
    function GetTextFieldIndex(const TextFieldIndexes: array of Integer; Index: Integer): Integer;
    function ReadCommentsText(CommentsControl: TControl): string;
    procedure SaveComments(Persistence: TProjectPersistence; CommentsControl: TControl);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure SavePairs(Persistence: TProjectPersistence; SourceVerse: TVersiculo);
    procedure SaveTexts(Persistence: TProjectPersistence;
      const VerseViews: array of TVersiculo; const TextFieldIndexes: array of Integer);
    procedure RemoveOldAssociations(Suggester: TGerSugestoes; SourceVerse, DestinationVerse: TVersiculo);
    procedure SaveAssociations(Suggester: TGerSugestoes; SourceVerse, DestinationVerse: TVersiculo);
    procedure SaveBeforeScroll(Persistence: TProjectPersistence; Suggester: TGerSugestoes;
      MemoVersiculo: TMemoVersiculo; CommentsControl: TControl;
      const VerseViews: array of TVersiculo; const TextFieldIndexes: array of Integer;
      SourceVerseIndex, DestinationVerseIndex: Integer);
    function LoadCurrentVerse(Persistence: TProjectPersistence;
      const VerseViews: array of TVersiculo; const TextFieldIndexes: array of Integer;
      const FormattedReference, ReferenceID: string; SourceVerseIndex,
      DestinationVerseIndex: Integer): TCurrentVerseLoadResult;
    procedure ApplyLoadedAssociations(SourceVerse: TVersiculo; const Associations: string);
    procedure FinalizeLoadedVerse(const VerseViews: array of TVersiculo);
  end;

implementation

function TCurrentVerseWorkspace.GetVerseView(const VerseViews: array of TVersiculo;
  Index: Integer): TVersiculo;
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

procedure TCurrentVerseWorkspace.SavePairs(Persistence: TProjectPersistence;
  SourceVerse: TVersiculo);
begin
  if not Assigned(Persistence) or not Assigned(SourceVerse) then
    Exit;

  Persistence.WriteCurrentPairs(SourceVerse.Pares);
end;

procedure TCurrentVerseWorkspace.SaveTexts(Persistence: TProjectPersistence;
  const VerseViews: array of TVersiculo; const TextFieldIndexes: array of Integer);
var
  Index: Integer;
  VerseView: TVersiculo;
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

procedure TCurrentVerseWorkspace.RemoveOldAssociations(Suggester: TGerSugestoes;
  SourceVerse, DestinationVerse: TVersiculo);
var
  PairIndex: Integer;
begin
  if not Assigned(Suggester) or not Assigned(SourceVerse) or not Assigned(DestinationVerse) or
     not Assigned(FOldPairs) then
    Exit;

  for PairIndex := 0 to FOldPairs.Count - 1 do
    if (PairIndex mod 2) = 0 then
      Suggester.RemoverPar(FOldPairs.Strings[PairIndex], FOldPairs.Strings[PairIndex + 1]);
end;

procedure TCurrentVerseWorkspace.SaveAssociations(Suggester: TGerSugestoes;
  SourceVerse, DestinationVerse: TVersiculo);
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
        Suggester.InserirPar(Pairs.Strings[PairIndex], Pairs.Strings[PairIndex + 1]);
  finally
    Pairs.Free;
  end;
end;

procedure TCurrentVerseWorkspace.SaveBeforeScroll(Persistence: TProjectPersistence;
  Suggester: TGerSugestoes; MemoVersiculo: TMemoVersiculo;
  CommentsControl: TControl; const VerseViews: array of TVersiculo;
  const TextFieldIndexes: array of Integer; SourceVerseIndex,
  DestinationVerseIndex: Integer);
var
  SourceVerse: TVersiculo;
begin
  SourceVerse := GetVerseView(VerseViews, SourceVerseIndex);
  if not Assigned(SourceVerse) then
    Exit;

  if Assigned(MemoVersiculo) then
    MemoVersiculo.Desativar;

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
  const VerseViews: array of TVersiculo; const TextFieldIndexes: array of Integer;
  const FormattedReference, ReferenceID: string; SourceVerseIndex,
  DestinationVerseIndex: Integer): TCurrentVerseLoadResult;
var
  Index: Integer;
  VerseView: TVersiculo;
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

procedure TCurrentVerseWorkspace.ApplyLoadedAssociations(SourceVerse: TVersiculo;
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
  const VerseViews: array of TVersiculo);
var
  Index: Integer;
  VerseView: TVersiculo;
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