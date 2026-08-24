unit SyntagmSelection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, Syntagm;

type
  TSyntagmIndexArray = array of Integer;
  TSyntagmStyleRequirement = (ssrAny, ssrRegular, ssrItalic);
  TSyntagmStyleRequirementArray = array of TSyntagmStyleRequirement;

function ResolveSyntagmSelectionIndexes(Syntagms: TSyntagmList; Words: TStrings;
  Partial: Boolean = False; JoinConnected: Boolean = True): TSyntagmIndexArray; overload;
function ResolveSyntagmSelectionIndexes(Syntagms: TSyntagmList; Words: TStrings;
  const StyleRequirements: TSyntagmStyleRequirementArray;
  Partial: Boolean = False; JoinConnected: Boolean = True): TSyntagmIndexArray; overload;

implementation

function IsAssociatedSyntagm(SyntagmNode: TSyntagm): Boolean;
begin
  Result := Assigned(SyntagmNode) and Assigned(SyntagmNode.Pairs) and
            (SyntagmNode.Pairs.Count > 0);
end;

function StyleMatches(SyntagmNode: TSyntagm;
  StyleRequirement: TSyntagmStyleRequirement): Boolean;
begin
  case StyleRequirement of
    ssrRegular: Result := not SyntagmNode.IsItalic;
    ssrItalic: Result := SyntagmNode.IsItalic;
  else
    Result := True;
  end;
end;

function IsAvailableWordSyntagm(SyntagmNode: TSyntagm; const WordToFind: string;
  StyleRequirement: TSyntagmStyleRequirement): Boolean;
begin
  Result := Assigned(SyntagmNode) and (SyntagmNode.Kind = skSyntagm) and
            AnsiSameText(SyntagmNode.Text, WordToFind) and
            not IsAssociatedSyntagm(SyntagmNode) and
            StyleMatches(SyntagmNode, StyleRequirement);
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

function BuildRequestedStyleRequirements(Words: TStrings;
  const StyleRequirements: TSyntagmStyleRequirementArray): TSyntagmStyleRequirementArray;
var
  I: Integer;
  Requirement: TSyntagmStyleRequirement;
begin
  SetLength(Result, 0);
  if not Assigned(Words) then
    Exit;

  for I := 0 to Words.Count - 1 do
  begin
    if Trim(Words[I]) = '' then
      Continue;

    Requirement := ssrAny;
    if I <= High(StyleRequirements) then
      Requirement := StyleRequirements[I];
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Requirement;
  end;
end;

function RequirementAt(const StyleRequirements: TSyntagmStyleRequirementArray;
  Index: Integer): TSyntagmStyleRequirement;
begin
  Result := ssrAny;
  if (Index >= 0) and (Index <= High(StyleRequirements)) then
    Result := StyleRequirements[Index];
end;

function TryResolveExactSequence(Syntagms: TSyntagmList; RequestedWords: TStrings;
  const StyleRequirements: TSyntagmStyleRequirementArray): TSyntagmIndexArray;
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
    if not IsAvailableWordSyntagm(Syntagms[StartIndex], RequestedWords[0],
      RequirementAt(StyleRequirements, 0)) then
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
          skWhitespace, skPunctuation:
            begin
              Inc(ScanIndex);
              Continue;
            end;
          skSyntagm:
            begin
              if IsAvailableWordSyntagm(ScanNode, RequestedWords[RequestedIndex],
                RequirementAt(StyleRequirements, RequestedIndex)) then
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
  const StyleRequirements: TSyntagmStyleRequirementArray;
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
      if IsAvailableWordSyntagm(Syntagms[CandidateScanIndex],
        RequestedWords[RequestedIndex],
        RequirementAt(StyleRequirements, RequestedIndex)) then
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
        skPunctuation:
          HasPunctuation := True;
        skTag, skMetadata:
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
      if Syntagms[IntermediateIndex].Kind = skPunctuation then
        AddIndex(Result, IntermediateIndex);
  end;
end;

function ResolveSyntagmSelectionIndexes(Syntagms: TSyntagmList; Words: TStrings;
  Partial: Boolean; JoinConnected: Boolean): TSyntagmIndexArray;
var
  NoStyleRequirements: TSyntagmStyleRequirementArray;
begin
  SetLength(NoStyleRequirements, 0);
  Result := ResolveSyntagmSelectionIndexes(Syntagms, Words,
    NoStyleRequirements, Partial, JoinConnected);
end;

function ResolveSyntagmSelectionIndexes(Syntagms: TSyntagmList; Words: TStrings;
  const StyleRequirements: TSyntagmStyleRequirementArray;
  Partial: Boolean; JoinConnected: Boolean): TSyntagmIndexArray;
var
  RequestedWords: TStringList;
  RequestedStyles: TSyntagmStyleRequirementArray;
begin
  SetLength(Result, 0);
  RequestedWords := BuildRequestedWords(Words);
  try
    if RequestedWords.Count = 0 then
      Exit;

    RequestedStyles := BuildRequestedStyleRequirements(Words, StyleRequirements);
    Result := TryResolveExactSequence(Syntagms, RequestedWords, RequestedStyles);
    if Length(Result) <> RequestedWords.Count then
      Result := ResolveOrderedFallback(Syntagms, RequestedWords,
        RequestedStyles, Partial);

    if JoinConnected and (Length(Result) > 1) then
      Result := ExpandConnectedPunctuation(Syntagms, Result);
  finally
    RequestedWords.Free;
  end;
end;

end.