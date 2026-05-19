unit ai_processing_batch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, PatchFile;

type
  TAIBatchIndexList = class(TStringList);

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
    function GetStatus(I: Integer): string;
    function GetProcessingState(I: Integer): string;
    function GetErrors(I: Integer): string;
    function HasVerseChanges(OriginalData: TAIProcessingBatch; Index: Integer): Boolean;
  public
    procedure Add(ref, src, dst, pairs, comments: string; status: integer);
    procedure Clear;
    procedure SetState(index: integer; state: string);
    procedure SetError(index: integer; error: string);
    procedure ApplyAcceptedResult(Index: Integer; ResultObj: TAIFlowResult);
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
    FEntries[Index].SourceText := ResultObj.AcceptedSourceText;
  if ResultObj.AcceptedDestinationTextSet then
    FEntries[Index].DestinationText := ResultObj.AcceptedDestinationText;
  if ResultObj.AcceptedPairsSet then
    FEntries[Index].Pairs := ResultObj.AcceptedPairs;
  if ResultObj.AcceptedCommentsSet then
    FEntries[Index].Comments := ResultObj.AcceptedComments;
  if ResultObj.AcceptedStatusSet then
    FEntries[Index].Status := ResultObj.AcceptedStatus;
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
            (FEntries[Index].Reference       <> OriginalData.FEntries[Index].Reference) or
            (FEntries[Index].SourceText      <> OriginalData.FEntries[Index].SourceText) or
            (FEntries[Index].DestinationText <> OriginalData.FEntries[Index].DestinationText) or
            (FEntries[Index].Pairs           <> OriginalData.FEntries[Index].Pairs) or
            (FEntries[Index].Comments        <> OriginalData.FEntries[Index].Comments) or
            (FEntries[Index].Status          <> OriginalData.FEntries[Index].Status);
end;

function TAIProcessingBatch.CreatePatch(OriginalData: TAIProcessingBatch;
  SelectedIndexes: TAIBatchIndexList; out AppliedCount: Integer): TPatchFile;
var
  I: Integer;
  Index: Integer;
begin
  Result := TPatchFile.Create;
  AppliedCount := 0;

  if not Assigned(SelectedIndexes) then
  begin
    for Index := 0 to Count - 1 do
      if HasVerseChanges(OriginalData, Index) then
      begin
        Result.Add(FEntries[Index].Reference, FEntries[Index].SourceText,
          FEntries[Index].DestinationText, FEntries[Index].Pairs,
          FEntries[Index].Comments, FEntries[Index].Status);
        Inc(AppliedCount);
      end;
    Exit;
  end;

  for I := 0 to SelectedIndexes.Count - 1 do
  begin
    Index := StrToIntDef(SelectedIndexes[I], -1);
    if (Index < 0) or (Index >= Count) then
      Continue;
    if not HasVerseChanges(OriginalData, Index) then
      Continue;

    Result.Add(FEntries[Index].Reference, FEntries[Index].SourceText,
      FEntries[Index].DestinationText, FEntries[Index].Pairs,
      FEntries[Index].Comments, FEntries[Index].Status);
    Inc(AppliedCount);
  end;
end;

end.
