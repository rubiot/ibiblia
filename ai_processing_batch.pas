unit ai_processing_batch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, PatchFile;

type
  TAIBatchIndexList = class(TStringList);
  TAIReviewField = (arfSourceText, arfDestinationText, arfAssociations, arfComments, arfStatus);
  TAIReviewFields = set of TAIReviewField;
  TAICommentsDecision = (acdKeepCurrent, acdUseAI, acdMerge);

  { TAIBatchEntry - a single row in the batch; keeps all fields together so
    they can never get out of sync across parallel lists. }
  TAIBatchEntry = record
    Reference: string;
    SourceText: string;
    DestinationText: string;
    Pairs: string;
    Comments: string;
    Status: Integer;
    ProcessingState: string;
    Error: string;
    AcceptedFields: TAIReviewFields;
    CommentsDecision: TAICommentsDecision;
    CommentsDecisionSet: Boolean;
  end;

  TAIProcessingBatch = class
  private
    FEntries: array of TAIBatchEntry;
    FCount: Integer;
    function GetReference(I: Integer): string;
    function GetSourceText(I: Integer): string;
    procedure SetSourceText(I: Integer; const Value: string);
    function GetDestinationText(I: Integer): string;
    procedure SetDestinationText(I: Integer; const Value: string);
    function GetPairs(I: Integer): string;
    procedure SetPairs(I: Integer; const Value: string);
    function GetComments(I: Integer): string;
    procedure SetComments(I: Integer; const Value: string);
    function GetCommentsDecision(I: Integer): TAICommentsDecision;
    procedure SetCommentsDecisionValue(I: Integer; Value: TAICommentsDecision);
    function GetStatus(I: Integer): string;
    function GetProcessingState(I: Integer): string;
    function GetErrors(I: Integer): string;
    function DefaultCommentsDecision(OriginalData: TAIProcessingBatch;
      Index: Integer): TAICommentsDecision;
    procedure EnsureDefaultCommentsDecision(OriginalData: TAIProcessingBatch;
      Index: Integer);
    function HasVerseChanges(OriginalData: TAIProcessingBatch; Index: Integer): Boolean;
    function BuildPatchEntry(OriginalData: TAIProcessingBatch; Index: Integer;
      out Src, Dst, Pairs, Comments: string; out Status: Integer): Boolean;
    function MergeComments(const CurrentComments, AIComments: string): string;
  public
    procedure Add(ref, src, dst, pairs, comments: string; status: integer);
    procedure Clear;
    procedure SetState(index: integer; state: string);
    procedure SetError(index: integer; error: string);
    procedure ApplyAcceptedResult(Index: Integer; ResultObj: TAIFlowResult);
    procedure SetFieldAccepted(Index: Integer; Field: TAIReviewField; Accepted: Boolean);
    procedure AcceptAllChangedFields(OriginalData: TAIProcessingBatch; Index: Integer);
    procedure RejectAllFields(Index: Integer);
    procedure AcceptVerseProposal(OriginalData: TAIProcessingBatch; Index: Integer);
    procedure RejectVerseProposal(Index: Integer);
    function IsFieldAccepted(Index: Integer; Field: TAIReviewField): Boolean;
    function FieldChanged(OriginalData: TAIProcessingBatch; Index: Integer;
      Field: TAIReviewField): Boolean;
    function HasProposalChanges(OriginalData: TAIProcessingBatch; Index: Integer): Boolean;
    function AcceptedFieldChanges(OriginalData: TAIProcessingBatch; Index: Integer): Integer;
    function HasAcceptedChanges(OriginalData: TAIProcessingBatch; Index: Integer): Boolean;
    function CountAcceptedVerses(OriginalData: TAIProcessingBatch;
      SelectedIndexes: TAIBatchIndexList): Integer;
    function EffectiveComments(OriginalData: TAIProcessingBatch;
      Index: Integer): string;
    procedure SetCommentsDecision(Index: Integer; Decision: TAICommentsDecision);
    function Count: integer;
    { Returns a new TStringList with the Reference values; caller must free it. }
    function BuildReferenceList: TStringList;
    function CreatePatch(OriginalData: TAIProcessingBatch; SelectedIndexes: TAIBatchIndexList;
      out AppliedCount: Integer): TPatchFile;

    property Reference[I: Integer]: string read GetReference;
    property SourceText[I: Integer]: string read GetSourceText write SetSourceText;
    property DestinationText[I: Integer]: string read GetDestinationText write SetDestinationText;
    property Pairs[I: Integer]: string read GetPairs write SetPairs;
    property Comments[I: Integer]: string read GetComments write SetComments;
    property CommentsDecision[I: Integer]: TAICommentsDecision read GetCommentsDecision write SetCommentsDecisionValue;
    { Returns status as string (IntToStr) for backward-compatible comparisons. }
    property Status[I: Integer]: string read GetStatus;
    property ProcessingState[I: Integer]: string read GetProcessingState;
    property Errors[I: Integer]: string read GetErrors;
  end;

implementation

{ Accessors }

function TAIProcessingBatch.GetReference(I: Integer): string;
begin Result := FEntries[I].Reference; end;

function TAIProcessingBatch.GetSourceText(I: Integer): string;
begin Result := FEntries[I].SourceText; end;

procedure TAIProcessingBatch.SetSourceText(I: Integer; const Value: string);
begin FEntries[I].SourceText := Value; end;

function TAIProcessingBatch.GetDestinationText(I: Integer): string;
begin Result := FEntries[I].DestinationText; end;

procedure TAIProcessingBatch.SetDestinationText(I: Integer; const Value: string);
begin FEntries[I].DestinationText := Value; end;

function TAIProcessingBatch.GetPairs(I: Integer): string;
begin Result := FEntries[I].Pairs; end;

procedure TAIProcessingBatch.SetPairs(I: Integer; const Value: string);
begin FEntries[I].Pairs := Value; end;

function TAIProcessingBatch.GetComments(I: Integer): string;
begin Result := FEntries[I].Comments; end;

procedure TAIProcessingBatch.SetComments(I: Integer; const Value: string);
begin FEntries[I].Comments := Value; end;

function TAIProcessingBatch.GetCommentsDecision(I: Integer): TAICommentsDecision;
begin
  if (I >= 0) and (I < FCount) then
    Result := FEntries[I].CommentsDecision
  else
    Result := acdKeepCurrent;
end;

procedure TAIProcessingBatch.SetCommentsDecisionValue(I: Integer;
  Value: TAICommentsDecision);
begin
  SetCommentsDecision(I, Value);
end;

function TAIProcessingBatch.GetStatus(I: Integer): string;
begin Result := IntToStr(FEntries[I].Status); end;

function TAIProcessingBatch.GetProcessingState(I: Integer): string;
begin Result := FEntries[I].ProcessingState; end;

function TAIProcessingBatch.GetErrors(I: Integer): string;
begin Result := FEntries[I].Error; end;

{ Core methods }

procedure TAIProcessingBatch.Add(ref, src, dst, pairs, comments: string; status: integer);
var
  entry: TAIBatchEntry;
begin
  entry.Reference := ref;
  entry.SourceText := src;
  entry.DestinationText := dst;
  entry.Pairs := pairs;
  entry.Comments := comments;
  entry.Status := status;
  entry.ProcessingState := 'pending';
  entry.Error := '';
  entry.AcceptedFields := [];
  entry.CommentsDecision := acdKeepCurrent;
  entry.CommentsDecisionSet := False;
  Inc(FCount);
  SetLength(FEntries, FCount);
  FEntries[FCount - 1] := entry;
end;

procedure TAIProcessingBatch.Clear;
begin
  FCount := 0;
  SetLength(FEntries, 0);
end;

procedure TAIProcessingBatch.SetState(index: integer; state: string);
begin
  if (index >= 0) and (index < FCount) then
    FEntries[index].ProcessingState := state;
end;

procedure TAIProcessingBatch.SetError(index: integer; error: string);
begin
  if (index >= 0) and (index < FCount) then
    FEntries[index].Error := error;
end;

procedure TAIProcessingBatch.ApplyAcceptedResult(Index: Integer; ResultObj: TAIFlowResult);
begin
  if (Index < 0) or (Index >= Count) or not Assigned(ResultObj) then
    Exit;

  if ResultObj.AcceptedSourceTextSet then
  begin
    FEntries[Index].SourceText := ResultObj.AcceptedSourceText;
    Include(FEntries[Index].AcceptedFields, arfSourceText);
  end;
  if ResultObj.AcceptedDestinationTextSet then
  begin
    FEntries[Index].DestinationText := ResultObj.AcceptedDestinationText;
    Include(FEntries[Index].AcceptedFields, arfDestinationText);
  end;
  if ResultObj.AcceptedPairsSet then
  begin
    FEntries[Index].Pairs := ResultObj.AcceptedPairs;
    Include(FEntries[Index].AcceptedFields, arfAssociations);
  end;
  if ResultObj.AcceptedCommentsSet then
  begin
    FEntries[Index].Comments := ResultObj.AcceptedComments;
    Include(FEntries[Index].AcceptedFields, arfComments);
  end;
  if ResultObj.AcceptedStatusSet then
  begin
    FEntries[Index].Status := ResultObj.AcceptedStatus;
    Include(FEntries[Index].AcceptedFields, arfStatus);
  end;
end;

procedure TAIProcessingBatch.SetFieldAccepted(Index: Integer; Field: TAIReviewField;
  Accepted: Boolean);
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  if Accepted then
    Include(FEntries[Index].AcceptedFields, Field)
  else
    Exclude(FEntries[Index].AcceptedFields, Field);
end;

procedure TAIProcessingBatch.AcceptAllChangedFields(OriginalData: TAIProcessingBatch;
  Index: Integer);
var
  Field: TAIReviewField;
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  EnsureDefaultCommentsDecision(OriginalData, Index);

  for Field := Low(TAIReviewField) to High(TAIReviewField) do
    if FieldChanged(OriginalData, Index, Field) then
      Include(FEntries[Index].AcceptedFields, Field)
    else
      Exclude(FEntries[Index].AcceptedFields, Field);
end;

procedure TAIProcessingBatch.RejectAllFields(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  FEntries[Index].AcceptedFields := [];
end;

procedure TAIProcessingBatch.AcceptVerseProposal(OriginalData: TAIProcessingBatch;
  Index: Integer);
begin
  AcceptAllChangedFields(OriginalData, Index);
end;

procedure TAIProcessingBatch.RejectVerseProposal(Index: Integer);
begin
  RejectAllFields(Index);
end;

function TAIProcessingBatch.IsFieldAccepted(Index: Integer;
  Field: TAIReviewField): Boolean;
begin
  Result := (Index >= 0) and (Index < Count) and
            (Field in FEntries[Index].AcceptedFields);
end;

function TAIProcessingBatch.FieldChanged(OriginalData: TAIProcessingBatch;
  Index: Integer; Field: TAIReviewField): Boolean;
begin
  Result := True;
  if (Index < 0) or (Index >= Count) then
    Exit(False);
  if not Assigned(OriginalData) or (Index < 0) or (Index >= OriginalData.Count) then
    Exit(True);

  case Field of
    arfSourceText:
      Result := FEntries[Index].SourceText <> OriginalData.FEntries[Index].SourceText;
    arfDestinationText:
      Result := FEntries[Index].DestinationText <> OriginalData.FEntries[Index].DestinationText;
    arfAssociations:
      Result := FEntries[Index].Pairs <> OriginalData.FEntries[Index].Pairs;
    arfComments:
      Result := EffectiveComments(OriginalData, Index) <> OriginalData.FEntries[Index].Comments;
    arfStatus:
      Result := FEntries[Index].Status <> OriginalData.FEntries[Index].Status;
  end;
end;

function TAIProcessingBatch.HasProposalChanges(OriginalData: TAIProcessingBatch;
  Index: Integer): Boolean;
begin
  Result := HasVerseChanges(OriginalData, Index);
end;

function TAIProcessingBatch.AcceptedFieldChanges(OriginalData: TAIProcessingBatch;
  Index: Integer): Integer;
var
  Field: TAIReviewField;
begin
  Result := 0;
  if (Index < 0) or (Index >= Count) then
    Exit;

  for Field := Low(TAIReviewField) to High(TAIReviewField) do
    if IsFieldAccepted(Index, Field) and FieldChanged(OriginalData, Index, Field) then
      Inc(Result);
end;

function TAIProcessingBatch.HasAcceptedChanges(OriginalData: TAIProcessingBatch;
  Index: Integer): Boolean;
begin
  Result := AcceptedFieldChanges(OriginalData, Index) > 0;
end;

function TAIProcessingBatch.CountAcceptedVerses(OriginalData: TAIProcessingBatch;
  SelectedIndexes: TAIBatchIndexList): Integer;
var
  I: Integer;
  Index: Integer;
begin
  Result := 0;

  if Assigned(SelectedIndexes) then
  begin
    for I := 0 to SelectedIndexes.Count - 1 do
    begin
      Index := StrToIntDef(SelectedIndexes[I], -1);
      if HasAcceptedChanges(OriginalData, Index) then
        Inc(Result);
    end;
    Exit;
  end;

  for Index := 0 to Count - 1 do
    if HasAcceptedChanges(OriginalData, Index) then
      Inc(Result);
end;

function TAIProcessingBatch.MergeComments(const CurrentComments,
  AIComments: string): string;
begin
  if Trim(CurrentComments) = '' then
    Exit(AIComments);
  if Trim(AIComments) = '' then
    Exit(CurrentComments);
  if CurrentComments = AIComments then
    Exit(CurrentComments);

  Result := CurrentComments + '<br/><br/>' + AIComments;
end;

function TAIProcessingBatch.DefaultCommentsDecision(OriginalData: TAIProcessingBatch;
  Index: Integer): TAICommentsDecision;
begin
  Result := acdKeepCurrent;
  if (Index < 0) or (Index >= Count) then
    Exit;

  if not Assigned(OriginalData) or (Index < 0) or (Index >= OriginalData.Count) then
  begin
    if Trim(FEntries[Index].Comments) <> '' then
      Result := acdUseAI;
    Exit;
  end;

  if Trim(FEntries[Index].Comments) = '' then
    Result := acdKeepCurrent
  else if FEntries[Index].Comments = OriginalData.FEntries[Index].Comments then
    Result := acdKeepCurrent
  else if Trim(OriginalData.FEntries[Index].Comments) = '' then
    Result := acdUseAI
  else
    Result := acdMerge;
end;

procedure TAIProcessingBatch.EnsureDefaultCommentsDecision(
  OriginalData: TAIProcessingBatch; Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  if not FEntries[Index].CommentsDecisionSet then
  begin
    FEntries[Index].CommentsDecision := DefaultCommentsDecision(OriginalData, Index);
    FEntries[Index].CommentsDecisionSet := True;
  end;
end;

function TAIProcessingBatch.EffectiveComments(OriginalData: TAIProcessingBatch;
  Index: Integer): string;
var
  Decision: TAICommentsDecision;
begin
  Result := '';
  if (Index < 0) or (Index >= Count) then
    Exit;

  if not Assigned(OriginalData) or (Index < 0) or (Index >= OriginalData.Count) then
    Exit(FEntries[Index].Comments);

  EnsureDefaultCommentsDecision(OriginalData, Index);
  Decision := FEntries[Index].CommentsDecision;
  case Decision of
    acdKeepCurrent:
      Result := OriginalData.FEntries[Index].Comments;
    acdUseAI:
      Result := FEntries[Index].Comments;
    acdMerge:
      Result := MergeComments(OriginalData.FEntries[Index].Comments,
        FEntries[Index].Comments);
  end;
end;

procedure TAIProcessingBatch.SetCommentsDecision(Index: Integer;
  Decision: TAICommentsDecision);
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  FEntries[Index].CommentsDecision := Decision;
  FEntries[Index].CommentsDecisionSet := True;
end;

function TAIProcessingBatch.Count: integer;
begin
  Result := FCount;
end;

function TAIProcessingBatch.BuildReferenceList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FCount - 1 do
    Result.Add(FEntries[i].Reference);
end;

function TAIProcessingBatch.HasVerseChanges(OriginalData: TAIProcessingBatch; Index: Integer): Boolean;
begin
  Result := not Assigned(OriginalData) or
            (Index < 0) or
            (Index >= OriginalData.Count) or
            FieldChanged(OriginalData, Index, arfSourceText) or
            FieldChanged(OriginalData, Index, arfDestinationText) or
            FieldChanged(OriginalData, Index, arfAssociations) or
            FieldChanged(OriginalData, Index, arfComments) or
            FieldChanged(OriginalData, Index, arfStatus);
end;

function TAIProcessingBatch.BuildPatchEntry(OriginalData: TAIProcessingBatch;
  Index: Integer; out Src, Dst, Pairs, Comments: string; out Status: Integer): Boolean;
var
  HasOriginal: Boolean;
begin
  Result := False;
  if (Index < 0) or (Index >= Count) then
    Exit;

  HasOriginal := Assigned(OriginalData) and (Index >= 0) and (Index < OriginalData.Count);
  if not HasOriginal then
  begin
    Src := FEntries[Index].SourceText;
    Dst := FEntries[Index].DestinationText;
    Pairs := FEntries[Index].Pairs;
    Comments := FEntries[Index].Comments;
    Status := FEntries[Index].Status;
    Exit(True);
  end;

  if arfSourceText in FEntries[Index].AcceptedFields then
    Src := FEntries[Index].SourceText
  else
    Src := OriginalData.FEntries[Index].SourceText;

  if arfDestinationText in FEntries[Index].AcceptedFields then
    Dst := FEntries[Index].DestinationText
  else
    Dst := OriginalData.FEntries[Index].DestinationText;

  if arfAssociations in FEntries[Index].AcceptedFields then
    Pairs := FEntries[Index].Pairs
  else
    Pairs := OriginalData.FEntries[Index].Pairs;

  if arfComments in FEntries[Index].AcceptedFields then
    Comments := EffectiveComments(OriginalData, Index)
  else
    Comments := OriginalData.FEntries[Index].Comments;

  if arfStatus in FEntries[Index].AcceptedFields then
    Status := FEntries[Index].Status
  else
    Status := OriginalData.FEntries[Index].Status;

  Result := (Src <> OriginalData.FEntries[Index].SourceText) or
            (Dst <> OriginalData.FEntries[Index].DestinationText) or
            (Pairs <> OriginalData.FEntries[Index].Pairs) or
            (Comments <> OriginalData.FEntries[Index].Comments) or
            (Status <> OriginalData.FEntries[Index].Status);
end;

function TAIProcessingBatch.CreatePatch(OriginalData: TAIProcessingBatch;
  SelectedIndexes: TAIBatchIndexList; out AppliedCount: Integer): TPatchFile;
var
  I: Integer;
  Index: Integer;
  SrcValue, DstValue, PairsValue, CommentsValue: string;
  StatusValue: Integer;
begin
  Result := TPatchFile.Create;
  AppliedCount := 0;

  if not Assigned(SelectedIndexes) then
  begin
    for Index := 0 to Count - 1 do
      if BuildPatchEntry(OriginalData, Index, SrcValue, DstValue, PairsValue,
        CommentsValue, StatusValue) then
      begin
        Result.Add(FEntries[Index].Reference, SrcValue, DstValue, PairsValue,
          CommentsValue, StatusValue);
        Inc(AppliedCount);
      end;
    Exit;
  end;

  for I := 0 to SelectedIndexes.Count - 1 do
  begin
    Index := StrToIntDef(SelectedIndexes[I], -1);
    if (Index < 0) or (Index >= Count) then
      Continue;
    if not BuildPatchEntry(OriginalData, Index, SrcValue, DstValue, PairsValue,
      CommentsValue, StatusValue) then
      Continue;

    Result.Add(FEntries[Index].Reference, SrcValue, DstValue, PairsValue,
      CommentsValue, StatusValue);
    Inc(AppliedCount);
  end;
end;

end.
