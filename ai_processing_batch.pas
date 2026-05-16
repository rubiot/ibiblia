unit ai_processing_batch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ai_flows, PatchFile;

type
  TAIBatchIndexList = class(TStringList);

  TAIProcessingBatch = class
  private
    FReference: TStringList;
    FSrcText: TStringList;
    FDstText: TStringList;
    FPairs: TStringList;
    FComments: TStringList;
    FStatus: TStringList;
    FErrors: TStringList;
    FProcessingState: TStringList;
    function HasVerseChanges(OriginalData: TAIProcessingBatch; Index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ref, src, dst, pairs, comments: string; status: integer);
    procedure SetState(index: integer; state: string);
    procedure SetError(index: integer; error: string);
    procedure ApplyAcceptedResult(Index: Integer; ResultObj: TAIFlowResult);
    function Count: integer;
    function CreatePatch(OriginalData: TAIProcessingBatch; SelectedIndexes: TAIBatchIndexList;
      out AppliedCount: Integer): TPatchFile;

    property Reference: TStringList read FReference;
    property SourceText: TStringList read FSrcText;
    property DestinationText: TStringList read FDstText;
    property Pairs: TStringList read FPairs;
    property Comments: TStringList read FComments;
    property Status: TStringList read FStatus;
    property ProcessingState: TStringList read FProcessingState;
    property Errors: TStringList read FErrors;
  end;

implementation

constructor TAIProcessingBatch.Create;
begin
  FReference := TStringList.Create;
  FSrcText := TStringList.Create;
  FDstText := TStringList.Create;
  FPairs := TStringList.Create;
  FComments := TStringList.Create;
  FStatus := TStringList.Create;
  FErrors := TStringList.Create;
  FProcessingState := TStringList.Create;
end;

destructor TAIProcessingBatch.Destroy;
begin
  FReference.Free;
  FSrcText.Free;
  FDstText.Free;
  FPairs.Free;
  FComments.Free;
  FStatus.Free;
  FErrors.Free;
  FProcessingState.Free;
  inherited Destroy;
end;

procedure TAIProcessingBatch.Add(ref, src, dst, pairs, comments: string; status: integer);
begin
  FReference.Add(ref);
  FSrcText.Add(src);
  FDstText.Add(dst);
  FPairs.Add(pairs);
  FComments.Add(comments);
  FStatus.Add(IntToStr(status));
  FErrors.Add('');
  FProcessingState.Add('pending');
end;

procedure TAIProcessingBatch.SetState(index: integer; state: string);
begin
  if (index >= 0) and (index < FProcessingState.Count) then
    FProcessingState[index] := state;
end;

procedure TAIProcessingBatch.SetError(index: integer; error: string);
begin
  if (index >= 0) and (index < FErrors.Count) then
    FErrors[index] := error;
end;

procedure TAIProcessingBatch.ApplyAcceptedResult(Index: Integer; ResultObj: TAIFlowResult);
begin
  if (Index < 0) or (Index >= Count) or not Assigned(ResultObj) then
    Exit;

  if ResultObj.AcceptedSourceTextSet then
    FSrcText[Index] := ResultObj.AcceptedSourceText;
  if ResultObj.AcceptedDestinationTextSet then
    FDstText[Index] := ResultObj.AcceptedDestinationText;
  if ResultObj.AcceptedPairsSet then
    FPairs[Index] := ResultObj.AcceptedPairs;
  if ResultObj.AcceptedCommentsSet then
    FComments[Index] := ResultObj.AcceptedComments;
  if ResultObj.AcceptedStatusSet then
    FStatus[Index] := IntToStr(ResultObj.AcceptedStatus);
end;

function TAIProcessingBatch.Count: integer;
begin
  Result := FReference.Count;
end;

function TAIProcessingBatch.HasVerseChanges(OriginalData: TAIProcessingBatch; Index: Integer): Boolean;
begin
  Result := not Assigned(OriginalData) or
            (Index < 0) or
            (Index >= OriginalData.Count) or
            (FReference[Index] <> OriginalData.Reference[Index]) or
            (FSrcText[Index] <> OriginalData.SourceText[Index]) or
            (FDstText[Index] <> OriginalData.DestinationText[Index]) or
            (FPairs[Index] <> OriginalData.Pairs[Index]) or
            (FComments[Index] <> OriginalData.Comments[Index]) or
            (FStatus[Index] <> OriginalData.Status[Index]);
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
        Result.Add(FReference[Index], FSrcText[Index], FDstText[Index], FPairs[Index],
          FComments[Index], StrToIntDef(FStatus[Index], 0));
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

    Result.Add(FReference[Index], FSrcText[Index], FDstText[Index], FPairs[Index],
      FComments[Index], StrToIntDef(FStatus[Index], 0));
    Inc(AppliedCount);
  end;
end;

end.
