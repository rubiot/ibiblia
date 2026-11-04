unit association_memory_service;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SuggestionManager, verse_render_buffer, project_config;

const
  ASSOCIATION_MEMORY_SOURCE_REVISION_KEY = 'association.memory.source_revision';
  ASSOCIATION_MEMORY_BUILT_REVISION_KEY = 'association.memory.built_revision';
  ASSOCIATION_MEMORY_CACHE_VERSION_KEY = 'association.memory.cache_version';
  ASSOCIATION_MEMORY_CACHE_VERSION = '4';

type
  { TAssociationMemoryService }

  TAssociationMemoryService = class
  private
    FConfig: TProjectConfig;
    FManager: TSuggestionManager;
    FRenderBuffer: TVerseRenderBuffer;
    FExpectedPairs: TStringList;
    FExpectedOccurrences: TStringList;
    FDeltaPairs: TStringList;
    FSkippedReferences: TStringList;
    FSourceRevision: string;
    FBuiltRevision: string;
    FCacheVersion: string;
    FPendingDirty: Boolean;
    FCacheMatchesSource: Boolean;
    FCacheValidated: Boolean;
    FRebuildActive: Boolean;
    FFullRebuildPending: Boolean;
    FReplayReady: Boolean;
    FSourceRevisionPersisted: Boolean;
    FCompletedRebuildReady: Boolean;
    function NextRevision: string;
    function GetSkippedAssociationCount: Integer;
    procedure RequireAttached;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(Config: TProjectConfig; Manager: TSuggestionManager;
      RenderBuffer: TVerseRenderBuffer);
    procedure ReloadRevisionState;
    procedure MarkSourceDirty;
    procedure MarkSourceRevisionPersisted;
    procedure MarkCacheSynchronized;
    procedure ApplyAddPair(const Source, Target: string); overload;
    procedure ApplyAddPair(const ReferenceID, Source, Target: string); overload;
    procedure ApplyRemovePair(const Source, Target: string); overload;
    procedure ApplyRemovePair(const ReferenceID, Source, Target: string); overload;
    procedure InvalidateReplay;
    function CanReplayPendingCache: Boolean;
    function CanCommitCompletedRebuild: Boolean;
    procedure CommitSourceOutdated;
    procedure ReplayPendingCache;
    function NeedsRebuild: Boolean;
    function RevisionsMatch: Boolean;
    procedure BeginRebuild;
    procedure AddVerse(const ReferenceID, ReferenceName, OriginalText,
      TranslatedText, Associations: string);
    procedure FinishRebuild;
    procedure AbortRebuild;
    procedure MarkCacheOutdated;
    function StageBuiltRevision: Boolean;
    procedure CommitCompleted;
    procedure ValidateCurrentCache;
    property SourceRevision: string read FSourceRevision;
    property BuiltRevision: string read FBuiltRevision;
    property PendingDirty: Boolean read FPendingDirty;
    property CacheMatchesSource: Boolean read FCacheMatchesSource;
    property CacheValidated: Boolean read FCacheValidated;
    property RebuildActive: Boolean read FRebuildActive;
    property SkippedAssociationCount: Integer
      read GetSkippedAssociationCount;
  end;

implementation

uses
  Syntagm;

resourcestring
  SAssociationMemoryServiceNotAttached =
    'Association memory service is not attached';
  SNoPendingSourceRevision =
    'There is no pending source revision to persist';
  SPendingAssociationMemoryCannotReplay =
    'Pending association memory cannot be replayed';
  SAssociationMemoryDeltaMissingPairFmt =
    'Association memory delta removes a missing pair "%s" -> "%s"';
  SAssociationMemoryRebuildAlreadyActive =
    'Association memory rebuild is already active';
  SAssociationMemoryRebuildNotActive =
    'Association memory rebuild is not active';
  SCannotPublishOutdatedAssociationMemory =
    'Cannot publish an outdated association memory';
  SAssociationMemoryOutdatedFmt =
    'Association memory is outdated (source revision %s, built revision %s)';

constructor TAssociationMemoryService.Create;
begin
  inherited Create;
  FExpectedPairs := TStringList.Create;
  FExpectedOccurrences := TStringList.Create;
  FDeltaPairs := TStringList.Create;
  FSkippedReferences := TStringList.Create;
end;

destructor TAssociationMemoryService.Destroy;
begin
  if FRebuildActive and Assigned(FManager) then
    FManager.RollbackRebuild;
  FExpectedOccurrences.Free;
  FExpectedPairs.Free;
  FDeltaPairs.Free;
  FSkippedReferences.Free;
  inherited Destroy;
end;

function TAssociationMemoryService.GetSkippedAssociationCount: Integer;
begin
  Result := FSkippedReferences.Count;
end;

procedure TAssociationMemoryService.RequireAttached;
begin
  if not Assigned(FConfig) or not Assigned(FManager) or
    not Assigned(FRenderBuffer) then
    raise Exception.Create(SAssociationMemoryServiceNotAttached);
end;

procedure TAssociationMemoryService.Attach(Config: TProjectConfig;
  Manager: TSuggestionManager; RenderBuffer: TVerseRenderBuffer);
begin
  FConfig := Config;
  FManager := Manager;
  FRenderBuffer := RenderBuffer;
  FRebuildActive := False;
  ReloadRevisionState;
end;

procedure TAssociationMemoryService.ReloadRevisionState;
begin
  FPendingDirty := False;
  FRebuildActive := False;
  FExpectedPairs.Clear;
  FExpectedOccurrences.Clear;
  FDeltaPairs.Clear;
  FSkippedReferences.Clear;
  FFullRebuildPending := False;
  FReplayReady := True;
  FSourceRevisionPersisted := False;
  FCompletedRebuildReady := False;
  FCacheValidated := False;
  if not Assigned(FConfig) then
  begin
    FSourceRevision := '';
    FBuiltRevision := '';
    FCacheVersion := '';
    FCacheMatchesSource := False;
    Exit;
  end;

  FSourceRevision := FConfig.ReadString(
    ASSOCIATION_MEMORY_SOURCE_REVISION_KEY, '');
  FBuiltRevision := FConfig.ReadString(
    ASSOCIATION_MEMORY_BUILT_REVISION_KEY, '');
  FCacheVersion := FConfig.ReadString(
    ASSOCIATION_MEMORY_CACHE_VERSION_KEY, '');
  FCacheMatchesSource := RevisionsMatch and
    (FCacheVersion = ASSOCIATION_MEMORY_CACHE_VERSION);
  // A built revision is only published after the rebuilt cache has passed
  // exact validation. Matching durable revisions therefore certify the cache
  // on reopen without an expensive full-table scan on every autosave.
  FCacheValidated := FCacheMatchesSource;
end;

function TAssociationMemoryService.NextRevision: string;
var
  Revision: Int64;
begin
  if not TryStrToInt64(FSourceRevision, Revision) or (Revision < 0) then
    Revision := 0;
  Inc(Revision);
  Result := IntToStr(Revision);
end;

procedure TAssociationMemoryService.MarkSourceDirty;
begin
  RequireAttached;
  if not FPendingDirty then
  begin
    FSourceRevision := NextRevision;
    FConfig.WriteString(ASSOCIATION_MEMORY_SOURCE_REVISION_KEY,
      FSourceRevision);
    FPendingDirty := True;
  end;
  FCacheMatchesSource := False;
  FSourceRevisionPersisted := False;
  FCompletedRebuildReady := False;
end;

procedure TAssociationMemoryService.MarkSourceRevisionPersisted;
begin
  RequireAttached;
  if not FPendingDirty then
    raise Exception.Create(SNoPendingSourceRevision);
  FSourceRevisionPersisted := True;
end;

procedure TAssociationMemoryService.MarkCacheSynchronized;
begin
  RequireAttached;
  FCacheMatchesSource := True;
end;

procedure TAssociationMemoryService.ApplyAddPair(const Source, Target: string);
begin
  ApplyAddPair('', Source, Target);
end;

procedure TAssociationMemoryService.ApplyAddPair(const ReferenceID, Source,
  Target: string);
var
  CanApplyIncrementally: Boolean;
begin
  RequireAttached;
  CanApplyIncrementally := FCacheMatchesSource or
    (FPendingDirty and FReplayReady and not FFullRebuildPending);
  if CanApplyIncrementally then
    FManager.AddPairForReference(ReferenceID, Source, Target);
  MarkSourceDirty;
  if not CanApplyIncrementally then
  begin
    FReplayReady := False;
    Exit;
  end;
  if FFullRebuildPending then
  begin
    FExpectedPairs.Add(Source);
    FExpectedPairs.Add(Target);
    FExpectedOccurrences.Add(ReferenceID);
    FExpectedOccurrences.Add(Source);
    FExpectedOccurrences.Add(Target);
  end
  else
  begin
    FDeltaPairs.Add('+');
    FDeltaPairs.Add(ReferenceID);
    FDeltaPairs.Add(Source);
    FDeltaPairs.Add(Target);
  end;
  FReplayReady := True;
end;

procedure TAssociationMemoryService.ApplyRemovePair(const Source,
  Target: string);
begin
  ApplyRemovePair('', Source, Target);
end;

procedure TAssociationMemoryService.ApplyRemovePair(const ReferenceID, Source,
  Target: string);
var
  CanApplyIncrementally: Boolean;
  PairIndex: Integer;
begin
  RequireAttached;
  CanApplyIncrementally := FCacheMatchesSource or
    (FPendingDirty and FReplayReady and not FFullRebuildPending);
  if CanApplyIncrementally then
    FManager.RemovePairForReference(ReferenceID, Source, Target);
  MarkSourceDirty;
  if not CanApplyIncrementally then
  begin
    FReplayReady := False;
    Exit;
  end;
  if FFullRebuildPending then
  begin
    for PairIndex := 0 to FExpectedPairs.Count - 1 do
      if (PairIndex mod 2 = 0) and (FExpectedPairs[PairIndex] = Source) and
        (FExpectedPairs[PairIndex + 1] = Target) then
      begin
        FExpectedPairs.Delete(PairIndex + 1);
        FExpectedPairs.Delete(PairIndex);
        Break;
      end;
    PairIndex := 0;
    while PairIndex < FExpectedOccurrences.Count do
    begin
      if (FExpectedOccurrences[PairIndex] = ReferenceID) and
         (FExpectedOccurrences[PairIndex + 1] = Source) and
         (FExpectedOccurrences[PairIndex + 2] = Target) then
      begin
        FExpectedOccurrences.Delete(PairIndex + 2);
        FExpectedOccurrences.Delete(PairIndex + 1);
        FExpectedOccurrences.Delete(PairIndex);
        Break;
      end;
      Inc(PairIndex, 3);
    end;
  end
  else
  begin
    FDeltaPairs.Add('-');
    FDeltaPairs.Add(ReferenceID);
    FDeltaPairs.Add(Source);
    FDeltaPairs.Add(Target);
  end;
  FReplayReady := True;
end;

procedure TAssociationMemoryService.InvalidateReplay;
begin
  FReplayReady := False;
  FCacheValidated := False;
end;

function TAssociationMemoryService.CanReplayPendingCache: Boolean;
begin
  Result := FPendingDirty and FReplayReady;
end;

function TAssociationMemoryService.CanCommitCompletedRebuild: Boolean;
begin
  Result := FPendingDirty and FSourceRevisionPersisted and
    FCompletedRebuildReady and FFullRebuildPending and FCacheMatchesSource;
end;

procedure TAssociationMemoryService.CommitSourceOutdated;
begin
  RequireAttached;
  FPendingDirty := False;
  FSourceRevisionPersisted := False;
  FCompletedRebuildReady := False;
  FCacheMatchesSource := False;
  FCacheValidated := False;
  FDeltaPairs.Clear;
  FExpectedPairs.Clear;
  FExpectedOccurrences.Clear;
  FFullRebuildPending := False;
  FReplayReady := True;
end;

procedure TAssociationMemoryService.ReplayPendingCache;
var
  DeltaCounts: TStringList;
  DeltaIndex: Integer;
  ExpectedCount: Integer;
  PairIndex: Integer;
  PairKey: string;
  SeparatorIndex: Integer;
  SourcePhrase: string;
  TargetPhrase: string;
  ErrorMessage: string;
begin
  RequireAttached;
  if not CanReplayPendingCache then
    raise Exception.Create(SPendingAssociationMemoryCannotReplay);

  DeltaCounts := TStringList.Create;
  try
    DeltaCounts.Sorted := True;
    DeltaCounts.CaseSensitive := True;
    DeltaCounts.UseLocale := False;
    FManager.BeginRebuild;
    try
      if FFullRebuildPending then
      begin
        FManager.ClearAssociationDatabase;
        PairIndex := 0;
        while PairIndex < FExpectedOccurrences.Count do
        begin
          FManager.AddPairForReference(FExpectedOccurrences[PairIndex],
            FExpectedOccurrences[PairIndex + 1],
            FExpectedOccurrences[PairIndex + 2]);
          Inc(PairIndex, 3);
        end;
        FManager.PrepareRebuildValidation;
        if not FManager.ValidatePreparedRebuild(ErrorMessage) then
          raise Exception.Create(ErrorMessage);
      end
      else
      begin
        // A previously certified cache remains consistent when its known
        // deltas are applied atomically. Validate the exact final count only
        // for affected phrase pairs instead of scanning the whole database.
        PairIndex := 0;
        while PairIndex < FDeltaPairs.Count do
        begin
          SourcePhrase := FDeltaPairs[PairIndex + 2].ToLower;
          TargetPhrase := FDeltaPairs[PairIndex + 3].ToLower;
          PairKey := SourcePhrase + #1 + TargetPhrase;
          DeltaIndex := DeltaCounts.IndexOf(PairKey);
          if DeltaIndex < 0 then
          begin
            ExpectedCount := FManager.GetPairOccurrenceCount(SourcePhrase,
              TargetPhrase);
            DeltaIndex := DeltaCounts.AddObject(PairKey,
              TObject(PtrInt(ExpectedCount)));
          end
          else
            ExpectedCount := PtrInt(DeltaCounts.Objects[DeltaIndex]);

          if FDeltaPairs[PairIndex] = '+' then
            Inc(ExpectedCount)
          else
            Dec(ExpectedCount);
          if ExpectedCount < 0 then
            raise Exception.CreateFmt(
              SAssociationMemoryDeltaMissingPairFmt,
              [SourcePhrase, TargetPhrase]);
          DeltaCounts.Objects[DeltaIndex] := TObject(PtrInt(ExpectedCount));
          Inc(PairIndex, 4);
        end;

        PairIndex := 0;
        while PairIndex < FDeltaPairs.Count do
        begin
          if FDeltaPairs[PairIndex] = '+' then
            FManager.AddPairForReference(FDeltaPairs[PairIndex + 1],
              FDeltaPairs[PairIndex + 2], FDeltaPairs[PairIndex + 3])
          else
            FManager.RemovePairForReference(FDeltaPairs[PairIndex + 1],
              FDeltaPairs[PairIndex + 2], FDeltaPairs[PairIndex + 3]);
          Inc(PairIndex, 4);
        end;

        for DeltaIndex := 0 to DeltaCounts.Count - 1 do
        begin
          PairKey := DeltaCounts[DeltaIndex];
          SeparatorIndex := Pos(#1, PairKey);
          SourcePhrase := Copy(PairKey, 1, SeparatorIndex - 1);
          TargetPhrase := Copy(PairKey, SeparatorIndex + 1, MaxInt);
          ExpectedCount := PtrInt(DeltaCounts.Objects[DeltaIndex]);
          if not FManager.ValidatePairConsistency(SourcePhrase,
            TargetPhrase, ExpectedCount, ErrorMessage) then
            raise Exception.Create(ErrorMessage);
        end;
      end;
      FManager.CommitRebuild;
      FCacheMatchesSource := True;
      FCacheValidated := True;
    except
      FManager.RollbackRebuild;
      FCacheMatchesSource := False;
      FCacheValidated := False;
      raise;
    end;
  finally
    DeltaCounts.Free;
  end;
end;

function TAssociationMemoryService.NeedsRebuild: Boolean;
begin
  Result := not FCacheMatchesSource;
end;

function TAssociationMemoryService.RevisionsMatch: Boolean;
var
  SourceValue: Int64;
  BuiltValue: Int64;
begin
  Result := TryStrToInt64(FSourceRevision, SourceValue) and
    TryStrToInt64(FBuiltRevision, BuiltValue) and (SourceValue > 0) and
    (SourceValue = BuiltValue);
end;

procedure TAssociationMemoryService.BeginRebuild;
begin
  RequireAttached;
  if FRebuildActive then
    raise Exception.Create(SAssociationMemoryRebuildAlreadyActive);

  if not FPendingDirty then
    MarkSourceDirty;
  FExpectedPairs.Clear;
  FExpectedOccurrences.Clear;
  FDeltaPairs.Clear;
  FSkippedReferences.Clear;
  FFullRebuildPending := True;
  FReplayReady := False;
  FCompletedRebuildReady := False;
  FCacheValidated := False;
  FManager.BeginRebuild;
  FRebuildActive := True;
  try
    FManager.ClearAssociationDatabase;
  except
    AbortRebuild;
    raise;
  end;
end;

procedure TAssociationMemoryService.AddVerse(const ReferenceID, ReferenceName,
  OriginalText, TranslatedText, Associations: string);
var
  Pairs: TStringList;
  CorruptionDetails: string;
  PairIndex: Integer;
  DisplayReference: string;
begin
  if not FRebuildActive then
    raise Exception.Create(SAssociationMemoryRebuildNotActive);
  if Associations.IsEmpty then
    Exit;

  Pairs := nil;
  if not FRenderBuffer.TryExtractPairsList(OriginalText, TranslatedText,
    Associations, plMetaData, Pairs, CorruptionDetails) then
  begin
    DisplayReference := ReferenceName;
    if DisplayReference.IsEmpty then
      DisplayReference := ReferenceID;
    FSkippedReferences.Add(DisplayReference);
    Exit;
  end;

  try
    if (Pairs.Count mod 2) <> 0 then
    begin
      DisplayReference := ReferenceName;
      if DisplayReference.IsEmpty then
        DisplayReference := ReferenceID;
      FSkippedReferences.Add(DisplayReference);
      Exit;
    end;
    FExpectedPairs.AddStrings(Pairs);
    for PairIndex := 0 to Pairs.Count - 1 do
      if (PairIndex mod 2) = 0 then
      begin
        FExpectedOccurrences.Add(ReferenceID);
        FExpectedOccurrences.Add(Pairs[PairIndex]);
        FExpectedOccurrences.Add(Pairs[PairIndex + 1]);
        FManager.AddPairForReference(ReferenceID, Pairs[PairIndex],
          Pairs[PairIndex + 1]);
      end;
  finally
    Pairs.Free;
  end;
end;

procedure TAssociationMemoryService.FinishRebuild;
var
  ErrorMessage: string;
begin
  if not FRebuildActive then
    raise Exception.Create(SAssociationMemoryRebuildNotActive);
  try
    FManager.PrepareRebuildValidation;
    if not FManager.ValidatePreparedRebuild(ErrorMessage) then
      raise Exception.Create(ErrorMessage);
    FManager.CommitRebuild;
    FRebuildActive := False;
    FCacheMatchesSource := True;
    FCacheValidated := True;
    FReplayReady := True;
    FCompletedRebuildReady := True;
  except
    AbortRebuild;
    raise;
  end;
end;

procedure TAssociationMemoryService.AbortRebuild;
begin
  if FRebuildActive and Assigned(FManager) then
    FManager.RollbackRebuild;
  FRebuildActive := False;
  FCacheMatchesSource := False;
  FCacheValidated := False;
  FReplayReady := False;
  FCompletedRebuildReady := False;
end;

procedure TAssociationMemoryService.MarkCacheOutdated;
begin
  FCacheMatchesSource := False;
  FCacheValidated := False;
end;

function TAssociationMemoryService.StageBuiltRevision: Boolean;
begin
  RequireAttached;
  if not FCacheMatchesSource then
    raise Exception.Create(SCannotPublishOutdatedAssociationMemory);
  Result := (FBuiltRevision <> FSourceRevision) or
    (FCacheVersion <> ASSOCIATION_MEMORY_CACHE_VERSION);
  if FBuiltRevision <> FSourceRevision then
    FConfig.WriteString(ASSOCIATION_MEMORY_BUILT_REVISION_KEY,
      FSourceRevision);
  if FCacheVersion <> ASSOCIATION_MEMORY_CACHE_VERSION then
    FConfig.WriteString(ASSOCIATION_MEMORY_CACHE_VERSION_KEY,
      ASSOCIATION_MEMORY_CACHE_VERSION);
end;

procedure TAssociationMemoryService.CommitCompleted;
begin
  FBuiltRevision := FSourceRevision;
  FCacheVersion := ASSOCIATION_MEMORY_CACHE_VERSION;
  FPendingDirty := False;
  FCacheMatchesSource := True;
  FCacheValidated := True;
  FDeltaPairs.Clear;
  FExpectedPairs.Clear;
  FExpectedOccurrences.Clear;
  FFullRebuildPending := False;
  FReplayReady := True;
  FSourceRevisionPersisted := False;
  FCompletedRebuildReady := False;
end;

procedure TAssociationMemoryService.ValidateCurrentCache;
var
  ErrorMessage: string;
begin
  RequireAttached;
  if not FCacheMatchesSource then
    raise Exception.CreateFmt(
      SAssociationMemoryOutdatedFmt,
      [FSourceRevision, FBuiltRevision]);
  if FCacheValidated then
    Exit;
  if not FManager.Validate(ErrorMessage) then
    raise Exception.Create(ErrorMessage);
  FCacheValidated := True;
end;

end.
