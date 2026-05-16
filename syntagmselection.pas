unit SyntagmSelection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, Syntagm;

type
  TSyntagmIndexArray = array of Integer;

function ResolveSyntagmSelectionIndexes(Syntagms: TSyntagmList; Words: TStrings;
  Partial: Boolean = False; JoinConnected: Boolean = True): TSyntagmIndexArray;

implementation

function IsAssociatedSyntagm(SyntagmNode: TSyntagm): Boolean;
begin
  Result := Assigned(SyntagmNode) and Assigned(SyntagmNode.Pairs) and
            (SyntagmNode.Pairs.Count > 0);
end;

function IsAvailableWordSyntagm(SyntagmNode: TSyntagm; const WordToFind: string): Boolean;
begin
  Result := Assigned(SyntagmNode) and (SyntagmNode.Kind = tsSintagma) and
            AnsiSameText(SyntagmNode.Text, WordToFind) and
            not IsAssociatedSyntagm(SyntagmNode);
end;

procedure AddIndex(var Indexes: TSyntagmIndexArray; Index: Integer);
var
  LengthBefore: Integer;
begin
  if Index < 0 then
    Exit;

  LengthBefore := Length(Indexes);
  SetLength(Indexes, LengthBefore + 1);
  Indexes[LengthBefore] := Index;
end;

function BuildRequestedWords(Words: TStrings): TStringList;
var
  I: Integer;
  WordToFind: string;
begin
  Result := TStringList.Create;
  if not Assigned(Words) then
    Exit;

  for I := 0 to Words.Count - 1 do
  begin
    WordToFind := Trim(Words[I]);
    if WordToFind <> '' then
      Result.Add(WordToFind);
  end;
end;

function TryResolveExactSequence(Syntagms: TSyntagmList; RequestedWords: TStrings): TSyntagmIndexArray;
var
  StartIndex: Integer;
  CandidateIndex: Integer;
  RequestedIndex: Integer;
  ScanIndex: Integer;
  Candidate: TSyntagmIndexArray;
  ScanNode: TSyntagm;
  SequenceMatched: Boolean;
begin
  SetLength(Result, 0);
  if not Assigned(Syntagms) or not Assigned(RequestedWords) or (RequestedWords.Count = 0) then
    Exit;

  for StartIndex := 0 to Syntagms.Count - 1 do
  begin
    if not IsAvailableWordSyntagm(Syntagms[StartIndex], RequestedWords[0]) then
      Continue;

    SetLength(Candidate, 0);
    AddIndex(Candidate, StartIndex);
    CandidateIndex := StartIndex;
    SequenceMatched := True;

    for RequestedIndex := 1 to RequestedWords.Count - 1 do
    begin
      ScanIndex := CandidateIndex + 1;
      CandidateIndex := -1;

      while ScanIndex < Syntagms.Count do
      begin
        ScanNode := Syntagms[ScanIndex];
        case ScanNode.Kind of
          tsEspaco, tsPontuacao:
            begin
              Inc(ScanIndex);
              Continue;
            end;
          tsSintagma:
            begin
              if IsAvailableWordSyntagm(ScanNode, RequestedWords[RequestedIndex]) then
                CandidateIndex := ScanIndex;
              Break;
            end;
        else
          Break;
        end;
      end;

      if CandidateIndex = -1 then
      begin
        SequenceMatched := False;
        Break;
      end;

      AddIndex(Candidate, CandidateIndex);
    end;

    if SequenceMatched and (Length(Candidate) = RequestedWords.Count) then
      Exit(Candidate);
  end;
end;

function ResolveOrderedFallback(Syntagms: TSyntagmList; RequestedWords: TStrings;
  Partial: Boolean): TSyntagmIndexArray;
var
  RequestedIndex: Integer;
  ScanIndex: Integer;
  CandidateScanIndex: Integer;
  CandidateIndex: Integer;
begin
  SetLength(Result, 0);
  if not Assigned(Syntagms) or not Assigned(RequestedWords) or (RequestedWords.Count = 0) then
    Exit;

  ScanIndex := 0;
  for RequestedIndex := 0 to RequestedWords.Count - 1 do
  begin
    CandidateIndex := -1;
    CandidateScanIndex := ScanIndex;
    while CandidateScanIndex < Syntagms.Count do
    begin
      if IsAvailableWordSyntagm(Syntagms[CandidateScanIndex], RequestedWords[RequestedIndex]) then
      begin
        CandidateIndex := CandidateScanIndex;
        Break;
      end;
      Inc(CandidateScanIndex);
    end;

    if CandidateIndex = -1 then
    begin
      if not Partial then
      begin
        SetLength(Result, 0);
        Exit;
      end;
      Continue;
    end;

    AddIndex(Result, CandidateIndex);
    ScanIndex := CandidateIndex + 1;
  end;
end;

function ExpandConnectedPunctuation(Syntagms: TSyntagmList;
  BaseIndexes: TSyntagmIndexArray): TSyntagmIndexArray;
var
  I: Integer;
  IntermediateIndex: Integer;
  CurrentIndex: Integer;
  NextIndex: Integer;
  HasBlockingWord: Boolean;
  HasPunctuation: Boolean;
  IntermediateNode: TSyntagm;
begin
  SetLength(Result, 0);
  if not Assigned(Syntagms) or (Length(BaseIndexes) = 0) then
    Exit(BaseIndexes);

  for I := 0 to High(BaseIndexes) do
  begin
    AddIndex(Result, BaseIndexes[I]);
    if I = High(BaseIndexes) then
      Continue;

    CurrentIndex := BaseIndexes[I];
    NextIndex := BaseIndexes[I + 1];
    if (CurrentIndex < 0) or (NextIndex >= Syntagms.Count) or (CurrentIndex >= NextIndex) then
      Continue;

    HasBlockingWord := False;
    HasPunctuation := False;
    for IntermediateIndex := CurrentIndex + 1 to NextIndex - 1 do
    begin
      IntermediateNode := Syntagms[IntermediateIndex];
      case IntermediateNode.Kind of
        tsPontuacao:
          HasPunctuation := True;
        tsTag, tsMetaDado:
          begin
          end;
      else
        begin
          HasBlockingWord := True;
          Break;
        end;
      end;
    end;

    if HasBlockingWord or not HasPunctuation then
      Continue;

    for IntermediateIndex := CurrentIndex + 1 to NextIndex - 1 do
      if Syntagms[IntermediateIndex].Kind = tsPontuacao then
        AddIndex(Result, IntermediateIndex);
  end;
end;

function ResolveSyntagmSelectionIndexes(Syntagms: TSyntagmList; Words: TStrings;
  Partial: Boolean; JoinConnected: Boolean): TSyntagmIndexArray;
var
  RequestedWords: TStringList;
begin
  SetLength(Result, 0);
  RequestedWords := BuildRequestedWords(Words);
  try
    if RequestedWords.Count = 0 then
      Exit;

    Result := TryResolveExactSequence(Syntagms, RequestedWords);
    if Length(Result) <> RequestedWords.Count then
      Result := ResolveOrderedFallback(Syntagms, RequestedWords, Partial);

    if JoinConnected and (Length(Result) > 1) then
      Result := ExpandConnectedPunctuation(Syntagms, Result);
  finally
    RequestedWords.Free;
  end;
end;

end.