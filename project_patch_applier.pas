unit project_patch_applier;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazLogger, patchfile, project_persistence;

type
  TPatchIndexArray = array of Integer;
  TPatchSelectionMode = (psmAll, psmSelected);
  TOnMissingPatchReference = function(const Reference: string): Boolean of object;

  TProjectPatchApplier = class
  private
    FPersistence: TProjectPersistence;
    procedure EnsurePersistenceAttached;
    function ShouldApplyIndex(SelectionMode: TPatchSelectionMode;
      const Indexes: TPatchIndexArray; Index: Integer): Boolean;
    function LocatePatchReference(const Reference: string): Boolean;
  public
    constructor Create(APersistence: TProjectPersistence = nil);
    procedure Attach(APersistence: TProjectPersistence);
    procedure ApplyPatchEntry(Patch: TPatchFile; Index: Integer);
    function ApplyPatch(Patch: TPatchFile; SelectionMode: TPatchSelectionMode;
      const Indexes: TPatchIndexArray;
      OnMissingReference: TOnMissingPatchReference = nil): Integer;
    procedure ClearTextFieldForAll(TextFieldIndex, SourceTextFieldIndex,
      DestinationTextFieldIndex: Integer; ResetStatus: Boolean);
  end;

implementation

constructor TProjectPatchApplier.Create(APersistence: TProjectPersistence);
begin
  inherited Create;
  Attach(APersistence);
end;

procedure TProjectPatchApplier.Attach(APersistence: TProjectPersistence);
begin
  FPersistence := APersistence;
end;

procedure TProjectPatchApplier.EnsurePersistenceAttached;
begin
  if not Assigned(FPersistence) then
    raise Exception.Create('Project patch applier persistence is not attached');
end;

function TProjectPatchApplier.ShouldApplyIndex(SelectionMode: TPatchSelectionMode;
  const Indexes: TPatchIndexArray; Index: Integer): Boolean;
var
  SelectedIndex: Integer;
begin
  if SelectionMode = psmAll then
    Exit(True);

  for SelectedIndex in Indexes do
    if SelectedIndex = Index then
      Exit(True);

  Result := False;
end;

function TProjectPatchApplier.LocatePatchReference(const Reference: string): Boolean;
begin
  EnsurePersistenceAttached;

  Result := (FPersistence.CurrentPairId = Reference) or FPersistence.LocateReference(Reference);
  if Result then
    Result := FPersistence.CurrentPairId = Reference;
end;

procedure TProjectPatchApplier.ApplyPatchEntry(Patch: TPatchFile; Index: Integer);
begin
  EnsurePersistenceAttached;

  FPersistence.BeginCurrentEdit;
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', Patch.SourceText[Index]);
  FPersistence.SetCurrentFieldAsString('pare_texto_destino', Patch.DestinationText[Index]);
  FPersistence.SetCurrentFieldAsString('pare_pares', Patch.Pairs[Index]);
  FPersistence.SetCurrentFieldAsString('pare_comentarios', Patch.Comments[Index]);
  FPersistence.SetCurrentFieldAsString('pare_situacao', Patch.Status[Index]);
  FPersistence.PostCurrentEdit;

  DebugLn('%s comments: "%s" situacao: %s', [
    FPersistence.CurrentPairId,
    FPersistence.CurrentComments,
    FPersistence.CurrentFieldAsString('pare_situacao')
  ]);
end;

function TProjectPatchApplier.ApplyPatch(Patch: TPatchFile;
  SelectionMode: TPatchSelectionMode; const Indexes: TPatchIndexArray;
  OnMissingReference: TOnMissingPatchReference): Integer;
var
  PatchIndex: Integer;
  IgnoreMissingWarnings: Boolean;
begin
  Result := 0;
  IgnoreMissingWarnings := False;
  if not Assigned(Patch) then
    Exit;

  for PatchIndex := 0 to Patch.Reference.Count - 1 do
  begin
    if not ShouldApplyIndex(SelectionMode, Indexes, PatchIndex) then
      Continue;

    if not LocatePatchReference(Patch.Reference[PatchIndex]) then
    begin
      if IgnoreMissingWarnings then
        Continue;

      if Assigned(OnMissingReference) then
        IgnoreMissingWarnings := OnMissingReference(Patch.Reference[PatchIndex]);
      Continue;
    end;

    ApplyPatchEntry(Patch, PatchIndex);
    Inc(Result);
  end;
end;

procedure TProjectPatchApplier.ClearTextFieldForAll(TextFieldIndex,
  SourceTextFieldIndex, DestinationTextFieldIndex: Integer; ResetStatus: Boolean);
var
  ClearsAssociations: Boolean;
begin
  EnsurePersistenceAttached;

  ClearsAssociations := (TextFieldIndex = SourceTextFieldIndex) or
                        (TextFieldIndex = DestinationTextFieldIndex);

  FPersistence.First;
  while not FPersistence.EOF do
  begin
    FPersistence.BeginCurrentEdit;
    FPersistence.SetCurrentText(TextFieldIndex, '');
    if ClearsAssociations then
    begin
      FPersistence.SetCurrentFieldAsString('pare_pares', '');
      if ResetStatus then
        FPersistence.SetCurrentFieldAsInteger('pare_situacao', 0);
    end;
    FPersistence.PostCurrentEdit;
    FPersistence.Next;
  end;
end;

end.
