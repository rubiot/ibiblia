unit current_verse_workspace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Graphics,
  Verse, VerseMemo, commentsview, SuggestionManager, syntagm, project_persistence,
  verse_render_buffer, association_memory_service, text_edit_provenance;

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
    FOldAssociations: string;
    FAssociationComparer: TVerseRenderBuffer;
    FLoadedAssociationsInvalid: Boolean;
    function GetVerseView(const VerseViews: array of TVerse; Index: Integer): TVerse;
    function GetTextFieldIndex(const TextFieldIndexes: array of Integer; Index: Integer): Integer;
    function ReadCommentsText(CommentsControl: TControl): string;
    function AssociationsChanged(SourceVerse: TVerse;
      const CurrentAssociations: string): Boolean;
    function SavePairsValue(Persistence: TProjectPersistence;
      const CurrentAssociations: string): Boolean;
    function SaveAssociationsValue(Suggester: TSuggestionManager;
      SourceVerse, DestinationVerse: TVerse;
      MemoryService: TAssociationMemoryService;
      const ReferenceID: string;
      const CurrentAssociations: string): Boolean;
    function SaveComments(Persistence: TProjectPersistence;
      CommentsControl: TControl): Boolean;
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
      EditedVerse: TVerse; const NewText: string;
      const Trace: TVerseEditTrace; out EditedIndex: Integer;
      out Impact: TAssociationEditImpact): Boolean;
    function SavePairs(Persistence: TProjectPersistence;
      SourceVerse: TVerse): Boolean;
    function SaveTexts(Persistence: TProjectPersistence;
      const VerseViews: array of TVerse;
      const TextFieldIndexes: array of Integer): Boolean;
    function HasPendingChanges(Persistence: TProjectPersistence;
      VerseMemo: TVerseMemo; CommentsControl: TControl;
      const VerseViews: array of TVerse;
      const TextFieldIndexes: array of Integer;
      SourceVerseIndex: Integer): Boolean;
    procedure RemoveOldAssociations(Suggester: TSuggestionManager; SourceVerse, DestinationVerse: TVerse);
    function SaveAssociations(Suggester: TSuggestionManager;
      SourceVerse, DestinationVerse: TVerse;
      MemoryService: TAssociationMemoryService = nil;
      const ReferenceID: string = ''): Boolean;
    function SaveBeforeScroll(Persistence: TProjectPersistence; Suggester: TSuggestionManager;
      VerseMemo: TVerseMemo; CommentsControl: TControl;
      const VerseViews: array of TVerse; const TextFieldIndexes: array of Integer;
      SourceVerseIndex, DestinationVerseIndex: Integer;
      MemoryService: TAssociationMemoryService = nil): Boolean;
    function LoadCurrentVerse(Persistence: TProjectPersistence;
      const VerseViews: array of TVerse; const TextFieldIndexes: array of Integer;
      const FormattedReference, ReferenceID: string; SourceVerseIndex,
      DestinationVerseIndex: Integer): TCurrentVerseLoadResult;
    procedure SetCurrentVerseText(Persistence: TProjectPersistence;
      RenderBuffer: TVerseRenderBuffer; const TextFieldIndexes: array of Integer;
      const VerseText: string; TextIndex, SourceVerseIndex,
      DestinationVerseIndex: Integer; Replace, ResetStatus: Boolean;
      out CorruptionDetails: string);
    procedure ApplyLoadedAssociations(SourceVerse: TVerse;
      const Associations: string; RenderBuffer: TVerseRenderBuffer = nil);
    procedure FinalizeLoadedVerse(const VerseViews: array of TVerse);
    property LoadedAssociationsInvalid: Boolean
      read FLoadedAssociationsInvalid;
  end;

implementation

resourcestring
  SAssociationPairListUnmatchedPhrase =
    'Association pair list contains an unmatched phrase';
  SVerseRenderBufferUnavailable =
    'Verse render buffer is not available';
  SAssociationDataNotCompletelyApplied =
    'Association data could not be applied completely to the verse pair';

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

function TCurrentVerseWorkspace.AssociationsChanged(SourceVerse: TVerse;
  const CurrentAssociations: string): Boolean;
begin
  Result := False;
  if not Assigned(SourceVerse) or not Assigned(SourceVerse.Syntagms) then
    Exit;
  if FLoadedAssociationsInvalid then
    Exit(SourceVerse.Modified);

  // Pares represents the complete association graph. Unlike
  // GetPairsList(plMetaData), it includes italic/bracketed syntagms and other
  // elements that are persisted even though they do not feed suggestion
  // memory. The comparer normalizes pair and index order before comparing.
  if Assigned(FAssociationComparer) then
    Result := not FAssociationComparer.AssociationsEquivalent(
      CurrentAssociations, FOldAssociations)
  else
    Result := CurrentAssociations <> FOldAssociations;
end;

function TCurrentVerseWorkspace.SaveComments(Persistence: TProjectPersistence;
  CommentsControl: TControl): Boolean;
var
  CommentsText: string;
begin
  Result := False;
  if not Assigned(Persistence) then
    Exit;

  CommentsText := ReadCommentsText(CommentsControl);
  Result := Persistence.CurrentComments <> CommentsText;
  if Result then
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
  FOldAssociations := '';
  FAssociationComparer := nil;
  FLoadedAssociationsInvalid := False;
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
  { Break reciprocal references while every verse is still alive. Syntagm
    destruction must never inspect a paired verse already freed earlier. }
  for Index := Low(VerseViews) to High(VerseViews) do
    if Assigned(VerseViews[Index]) then
    begin
      VerseViews[Index].MarkDestroying;
      VerseViews[Index].PairedVerse := nil;
    end;
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
    VerseViews[VerseIndex].UnassociatedColor := clWindowText;

  if (VerseIndex = DestinationVerseIndex) and Assigned(GetVerseView(VerseViews, SourceVerseIndex)) then
    VerseViews[DestinationVerseIndex].PairedVerse := GetVerseView(VerseViews, SourceVerseIndex)
  else if (VerseIndex = SourceVerseIndex) and Assigned(GetVerseView(VerseViews, DestinationVerseIndex)) then
    VerseViews[SourceVerseIndex].PairedVerse := GetVerseView(VerseViews, DestinationVerseIndex);
end;

procedure TCurrentVerseWorkspace.ApplyDisplayTags(
  const VerseViews: array of TVerse; DisplayTags: Boolean);
var
  Index: Integer;
begin
  for Index := Low(VerseViews) to High(VerseViews) do
    if Assigned(VerseViews[Index]) then
      if DisplayTags then
        VerseViews[Index].ShowTags
      else
        VerseViews[Index].HideTags;
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
      VerseViews[Index].BoldWordsWithStrong := Enabled;
end;

function TCurrentVerseWorkspace.ApplyEditedVerseText(
  const VerseViews: array of TVerse; EditedVerse: TVerse;
  const NewText: string; const Trace: TVerseEditTrace;
  out EditedIndex: Integer; out Impact: TAssociationEditImpact): Boolean;
var
  Index: Integer;
begin
  Result := False;
  EditedIndex := -1;
  Impact.PreservedMembers := 0;
  Impact.RemovedMembers := 0;
  Impact.UsedLegacyFallback := False;
  Impact.ErrorMessage := 'The edited Verse is not part of the current workspace.';
  for Index := Low(VerseViews) to High(VerseViews) do
    if VerseViews[Index] = EditedVerse then
    begin
      EditedIndex := Index;
      Result := EditedVerse.TryApplyTracedEdit(NewText, Trace, Impact);
      Exit;
    end;
end;

function TCurrentVerseWorkspace.SavePairs(Persistence: TProjectPersistence;
  SourceVerse: TVerse): Boolean;
var
  CurrentAssociations: string;
begin
  Result := False;
  if not Assigned(Persistence) or not Assigned(SourceVerse) or
    not Assigned(SourceVerse.Syntagms) then
    Exit;

  CurrentAssociations := SourceVerse.Pairs;
  Result := SavePairsValue(Persistence, CurrentAssociations);
end;

function TCurrentVerseWorkspace.SavePairsValue(
  Persistence: TProjectPersistence;
  const CurrentAssociations: string): Boolean;
begin
  Result := False;
  if not Assigned(Persistence) then
    Exit;

  Result := Persistence.CurrentPairs <> CurrentAssociations;
  if Result then
    Persistence.WriteCurrentPairs(CurrentAssociations);
end;

function TCurrentVerseWorkspace.SaveTexts(Persistence: TProjectPersistence;
  const VerseViews: array of TVerse;
  const TextFieldIndexes: array of Integer): Boolean;
var
  Index: Integer;
  VerseView: TVerse;
  FieldIndex: Integer;
begin
  Result := False;
  if not Assigned(Persistence) then
    Exit;

  for Index := Low(VerseViews) to High(VerseViews) do
  begin
    VerseView := VerseViews[Index];
    if Assigned(VerseView) then
    begin
      FieldIndex := GetTextFieldIndex(TextFieldIndexes, Index);
      if Persistence.CurrentText(FieldIndex) <> VerseView.XML then
      begin
        Persistence.SetCurrentText(FieldIndex, VerseView.XML);
        Persistence.PostCurrentEdit;
        Result := True;
      end;
      VerseView.Modified := False;
      VerseView.XMLModified := False;
    end;
  end;
end;

function TCurrentVerseWorkspace.HasPendingChanges(
  Persistence: TProjectPersistence; VerseMemo: TVerseMemo;
  CommentsControl: TControl; const VerseViews: array of TVerse;
  const TextFieldIndexes: array of Integer;
  SourceVerseIndex: Integer): Boolean;
var
  Index: Integer;
  VerseView: TVerse;
begin
  Result := False;
  if not Assigned(Persistence) or not Assigned(Persistence.PairsTable) then
    Exit;
  if Persistence.ContentUpdatesPending then
    Exit(True);
  if Assigned(VerseMemo) and VerseMemo.HasPendingChange then
    Exit(True);

  for Index := Low(VerseViews) to High(VerseViews) do
  begin
    VerseView := VerseViews[Index];
    if Assigned(VerseView) and
      (VerseView.XML <> Persistence.CurrentText(
        GetTextFieldIndex(TextFieldIndexes, Index))) then
      Exit(True);
  end;

  if (SourceVerseIndex >= Low(VerseViews)) and
    (SourceVerseIndex <= High(VerseViews)) then
  begin
    VerseView := VerseViews[SourceVerseIndex];
    if Assigned(VerseView) and Assigned(VerseView.Syntagms) and
      AssociationsChanged(VerseView, VerseView.Pairs) then
      Exit(True);
  end;
  if CommentsControl is TCommentsView then
    Result := TCommentsView(CommentsControl).HasPendingChange(
      Persistence.CurrentComments)
  else
    Result := ReadCommentsText(CommentsControl) <>
      Persistence.CurrentComments;
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

  FOldPairs.Clear;
end;

function TCurrentVerseWorkspace.SaveAssociations(
  Suggester: TSuggestionManager; SourceVerse, DestinationVerse: TVerse;
  MemoryService: TAssociationMemoryService; const ReferenceID: string): Boolean;
var
  CurrentAssociations: string;
begin
  Result := False;
  if not Assigned(Suggester) or not Assigned(SourceVerse) or
    not Assigned(DestinationVerse) or not Assigned(SourceVerse.Syntagms) or
    not Assigned(DestinationVerse.Syntagms) then
    Exit;

  // Validate the Verse Pair before taking the stable snapshot. A newly
  // created TVerse has no syntagm list yet, so reading Pares is not valid.
  CurrentAssociations := SourceVerse.Pairs;
  Result := SaveAssociationsValue(Suggester, SourceVerse, DestinationVerse,
    MemoryService, ReferenceID, CurrentAssociations);
end;

function TCurrentVerseWorkspace.SaveAssociationsValue(
  Suggester: TSuggestionManager;
  SourceVerse, DestinationVerse: TVerse;
  MemoryService: TAssociationMemoryService;
  const ReferenceID: string;
  const CurrentAssociations: string): Boolean;
var
  Pairs: TStringList;
  SeenPairs: TStringList;
  PairIndex: Integer;
  Difference: Integer;

  function PairKey(const PairList: TStrings; Index: Integer): string;
  begin
    Result := PairList[Index] + #1 + PairList[Index + 1];
  end;

  function PairCount(const PairList: TStrings; const Source,
    Destination: string): Integer;
  var
    Index: Integer;
  begin
    Result := 0;
    if not Assigned(PairList) then
      Exit;
    for Index := 0 to PairList.Count - 1 do
      if (Index mod 2 = 0) and (PairList[Index] = Source) and
        (PairList[Index + 1] = Destination) then
        Inc(Result);
  end;

  procedure RequireValidPairs(const PairList: TStrings);
  begin
    if Assigned(PairList) and ((PairList.Count mod 2) <> 0) then
      raise Exception.Create(SAssociationPairListUnmatchedPhrase);
  end;
begin
  Result := False;
  if not Assigned(Suggester) or not Assigned(SourceVerse) or
    not Assigned(DestinationVerse) or not Assigned(SourceVerse.Syntagms) or
    not Assigned(DestinationVerse.Syntagms) then
    Exit;

  if FLoadedAssociationsInvalid then
  begin
    // The published cache intentionally contains no contribution from a
    // corrupt canonical row. Once the user explicitly changes that Verse
    // Pair, persist the replacement and rebuild instead of trying to remove
    // semantic pairs that were never accepted into the cache.
    if not SourceVerse.Modified then
      Exit;
    Pairs := SourceVerse.GetPairsList(plMetaData);
    try
      if Assigned(MemoryService) then
        MemoryService.InvalidateReplay;
      FreeAndNil(FOldPairs);
      FOldPairs := Pairs;
      Pairs := nil;
      FOldAssociations := CurrentAssociations;
      FLoadedAssociationsInvalid := False;
      Result := True;
    finally
      Pairs.Free;
    end;
    Exit;
  end;

  Pairs := SourceVerse.GetPairsList(plMetaData);
  SeenPairs := TStringList.Create;
  try
    RequireValidPairs(FOldPairs);
    RequireValidPairs(Pairs);
    Suggester.BeginRebuild;
    try
      if Assigned(FOldPairs) then
        for PairIndex := 0 to FOldPairs.Count - 1 do
          if (PairIndex mod 2 = 0) and
            (SeenPairs.IndexOf(PairKey(FOldPairs, PairIndex)) < 0) then
          begin
            SeenPairs.Add(PairKey(FOldPairs, PairIndex));
            Difference := PairCount(FOldPairs, FOldPairs[PairIndex],
              FOldPairs[PairIndex + 1]) - PairCount(Pairs,
              FOldPairs[PairIndex], FOldPairs[PairIndex + 1]);
            while Difference > 0 do
            begin
              if Assigned(MemoryService) then
                MemoryService.ApplyRemovePair(ReferenceID,
                  FOldPairs[PairIndex],
                  FOldPairs[PairIndex + 1])
              else
                Suggester.RemovePair(FOldPairs[PairIndex],
                  FOldPairs[PairIndex + 1]);
              Dec(Difference);
              Result := True;
            end;
          end;

      SeenPairs.Clear;
      for PairIndex := 0 to Pairs.Count - 1 do
        if (PairIndex mod 2 = 0) and
          (SeenPairs.IndexOf(PairKey(Pairs, PairIndex)) < 0) then
        begin
          SeenPairs.Add(PairKey(Pairs, PairIndex));
          Difference := PairCount(Pairs, Pairs[PairIndex], Pairs[PairIndex + 1]) -
            PairCount(FOldPairs, Pairs[PairIndex], Pairs[PairIndex + 1]);
          while Difference > 0 do
          begin
            if Assigned(MemoryService) then
              MemoryService.ApplyAddPair(ReferenceID, Pairs[PairIndex],
                Pairs[PairIndex + 1])
            else
              Suggester.AddPair(Pairs[PairIndex], Pairs[PairIndex + 1]);
            Dec(Difference);
            Result := True;
          end;
        end;

      Suggester.CommitRebuild;
      FreeAndNil(FOldPairs);
      FOldPairs := Pairs;
      Pairs := nil;
      FOldAssociations := CurrentAssociations;
    except
      Suggester.RollbackRebuild;
      if Assigned(MemoryService) then
        MemoryService.InvalidateReplay;
      raise;
    end;
  finally
    SeenPairs.Free;
    Pairs.Free;
  end;
end;

function TCurrentVerseWorkspace.SaveBeforeScroll(Persistence: TProjectPersistence;
  Suggester: TSuggestionManager; VerseMemo: TVerseMemo;
  CommentsControl: TControl; const VerseViews: array of TVerse;
  const TextFieldIndexes: array of Integer; SourceVerseIndex,
  DestinationVerseIndex: Integer;
  MemoryService: TAssociationMemoryService): Boolean;
var
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
  AssociationWasChanged: Boolean;
  SourceTextChanged: Boolean;
  DestinationTextChanged: Boolean;
  CurrentAssociations: string;
  ReferenceID: string;
begin
  Result := False;
  SourceVerse := GetVerseView(VerseViews, SourceVerseIndex);
  if not Assigned(SourceVerse) or not Assigned(SourceVerse.Syntagms) then
    Exit;
  DestinationVerse := GetVerseView(VerseViews, DestinationVerseIndex);

  if Assigned(VerseMemo) and not VerseMemo.TryDeactivate then
    SysUtils.Abort;

  // Keep one stable serialization for the whole operation. In particular,
  // do not enumerate SourceVerse.Syntagms again after SaveAssociations has
  // updated the SQLite-backed suggestion memory.
  CurrentAssociations := SourceVerse.Pairs;

  SourceTextChanged := Assigned(Persistence) and
    (SourceVerse.XML <> Persistence.CurrentText(
      GetTextFieldIndex(TextFieldIndexes, SourceVerseIndex)));
  DestinationTextChanged := Assigned(Persistence) and
    Assigned(DestinationVerse) and
    (DestinationVerse.XML <> Persistence.CurrentText(
      GetTextFieldIndex(TextFieldIndexes, DestinationVerseIndex)));
  AssociationWasChanged := AssociationsChanged(SourceVerse,
    CurrentAssociations);

  ReferenceID := '';
  if Assigned(Persistence) then
    ReferenceID := Persistence.CurrentPairId;
  if AssociationWasChanged then
    SaveAssociationsValue(Suggester, SourceVerse, DestinationVerse,
      MemoryService, ReferenceID, CurrentAssociations);
  Result := AssociationWasChanged or SourceTextChanged or
    DestinationTextChanged;
  if AssociationWasChanged or SourceTextChanged or DestinationTextChanged then
    SavePairsValue(Persistence, CurrentAssociations);

  SaveTexts(Persistence, VerseViews, TextFieldIndexes);
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
      VerseView.Active := False;
      VerseView.Text := Persistence.CurrentText(GetTextFieldIndex(TextFieldIndexes, Index));
      VerseView.Reference := FormattedReference;
      VerseView.ReferenceID := ReferenceID;
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

  SourceTextFieldIndex := GetTextFieldIndex(TextFieldIndexes, SourceVerseIndex);
  DestinationTextFieldIndex := GetTextFieldIndex(TextFieldIndexes, DestinationVerseIndex);
  ParticipatesInPairs := (TextIndex = SourceVerseIndex) or (TextIndex = DestinationVerseIndex);
  CurrentPairs := Persistence.CurrentPairs;

  if Replace and ParticipatesInPairs and not CurrentPairs.IsEmpty then
  begin
    if not Assigned(RenderBuffer) then
      raise Exception.Create(SVerseRenderBufferUnavailable);
    if not RenderBuffer.TryReplaceTextKeepingAssociations(
      Persistence.CurrentText(SourceTextFieldIndex),
      Persistence.CurrentText(DestinationTextFieldIndex), CurrentPairs,
      TextIndex = SourceVerseIndex, VerseText, UpdatedPairs,
      CorruptionDetails) then
      Exit;
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
  const Associations: string; RenderBuffer: TVerseRenderBuffer);
begin
  // Discard the previous Verse Pair baseline only when a different Verse is
  // actually being loaded. SaveProject also calls SaveBeforeScroll, so
  // clearing it there makes the next navigation treat every association as
  // newly added.
  Clear;
  FAssociationComparer := RenderBuffer;
  if not Assigned(SourceVerse) then
    Exit;

  try
    SourceVerse.Pairs := Associations;
    if Assigned(RenderBuffer) and
      not RenderBuffer.AssociationsEquivalent(SourceVerse.Pairs,
        Associations) then
      raise Exception.Create(SAssociationDataNotCompletelyApplied);
    FOldPairs := SourceVerse.GetPairsList(plMetaData);
    FOldAssociations := SourceVerse.Pairs;
  except
    // SetPares is atomic, but explicitly clear here as well so this recovery
    // remains safe for Verse implementations created by older callers.
    SourceVerse.Pairs := '';
    FOldPairs := TStringList.Create;
    FOldAssociations := '';
    FLoadedAssociationsInvalid := not Associations.IsEmpty;
    raise;
  end;
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
      VerseView.Modified := False;
      VerseView.XMLModified := False;
      VerseView.Active := True;
      VerseView.Render;
    end;
  end;
end;

end.
