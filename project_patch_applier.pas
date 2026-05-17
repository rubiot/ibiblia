unit project_patch_applier;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazLogger, patchfile, project_persistence;

type
  TPatchIndexArray = array of Integer;
  TOnMissingPatchReference = function(const Reference: string): Boolean of object;

  TProjectPatchApplier = class
  private
    FPersistence: TProjectPersistence;
    function ShouldApplyIndex(const Indexes: TPatchIndexArray; Index: Integer): Boolean;
    function LocatePatchReference(const Reference: string): Boolean;
  public
    constructor Create(APersistence: TProjectPersistence = nil);
    procedure Attach(APersistence: TProjectPersistence);
    procedure ApplyPatchEntry(Patch: TPatchFile; Index: Integer);
    procedure ApplyPatch(Patch: TPatchFile; const Indexes: TPatchIndexArray;
      OnMissingReference: TOnMissingPatchReference = nil);
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

function TProjectPatchApplier.ShouldApplyIndex(const Indexes: TPatchIndexArray;
  Index: Integer): Boolean;
var
  SelectedIndex: Integer;
begin
  if Length(Indexes) = 0 then
    Exit(True);

  for SelectedIndex in Indexes do
    if SelectedIndex = Index then
      Exit(True);

  Result := False;
end;

function TProjectPatchApplier.LocatePatchReference(const Reference: string): Boolean;
begin
  if not Assigned(FPersistence) then
    raise Exception.Create('Project patch applier persistence is not attached');

  Result := (FPersistence.CurrentPairId = Reference) or FPersistence.LocateReference(Reference);
  if Result then
    Result := FPersistence.CurrentPairId = Reference;
end;

procedure TProjectPatchApplier.ApplyPatchEntry(Patch: TPatchFile; Index: Integer);
begin
  if not Assigned(FPersistence) then
    raise Exception.Create('Project patch applier persistence is not attached');

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

procedure TProjectPatchApplier.ApplyPatch(Patch: TPatchFile;
  const Indexes: TPatchIndexArray; OnMissingReference: TOnMissingPatchReference);
var
  PatchIndex: Integer;
  IgnoreMissingWarnings: Boolean;
begin
  IgnoreMissingWarnings := False;
  if not Assigned(Patch) then
    Exit;

  for PatchIndex := 0 to Patch.Reference.Count - 1 do
  begin
    if not ShouldApplyIndex(Indexes, PatchIndex) then
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
  end;
end;

end.