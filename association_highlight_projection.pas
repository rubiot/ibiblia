unit association_highlight_projection;

{$mode objfpc}{$H+}

interface

uses
  ConcordanceExport, Verse, SuggestionManager;

procedure BuildVisibleDestinationProjection(ADestinationVerse: TVerse;
  out APlainText: string;
  out ASyntagmRanges: TConcordanceHighlightRangeArray);

function TryNormalizeAssociationHighlightRanges(const AContext: string;
  const ARanges: TConcordanceHighlightRangeArray;
  out ANormalized: TConcordanceHighlightRangeArray): Boolean;

function TryProjectAssociationHighlightRanges(
  const AData: TAssociationPairData; const AContext: string;
  const ASyntagmRanges: TConcordanceHighlightRangeArray;
  out AHighlightRanges: TConcordanceHighlightRangeArray): Boolean;

function TryUnionAssociationHighlightRanges(const AContext: string;
  const ALeft, ARight: TConcordanceHighlightRangeArray;
  out AUnion: TConcordanceHighlightRangeArray): Boolean;

function TryProjectMatchingAssociationHighlightRanges(
  const AAssociationData: TAssociationPairDataArray;
  const AMatches: TTranslationPairMatchArray; const AContext: string;
  const ASyntagmRanges: TConcordanceHighlightRangeArray;
  out AHighlightRanges: TConcordanceHighlightRangeArray): Boolean;

implementation

uses
  SysUtils, StrUtils, Math, syntagm, ONTTokenizer, rf_note_scanner;

procedure BuildVisibleDestinationProjection(ADestinationVerse: TVerse;
  out APlainText: string;
  out ASyntagmRanges: TConcordanceHighlightRangeArray);
var
  FirstVisibleByte: Integer;
  InRFNote: Boolean;
  InTitle: Boolean;
  Index: Integer;
  LastVisibleByte: Integer;
  Syntagm: TSyntagm;
begin
  APlainText := '';
  ASyntagmRanges := nil;
  if not Assigned(ADestinationVerse) or
     not Assigned(ADestinationVerse.Syntagms) then
    Exit;

  SetLength(ASyntagmRanges, ADestinationVerse.Syntagms.Count);
  InRFNote := False;
  InTitle := False;
  for Index := 0 to ADestinationVerse.Syntagms.Count - 1 do
  begin
    Syntagm := ADestinationVerse.Syntagms[Index];
    if Syntagm.Kind = skTag then
    begin
      if InRFNote then
      begin
        if IsRFClosingTag(Syntagm.Text) then
          InRFNote := False;
      end
      else if InTitle then
      begin
        if SameText(Syntagm.Text, '<Ts>') then
          InTitle := False;
      end
      else if IsRFOpeningTag(Syntagm.Text) then
        InRFNote := True
      else if AnsiStartsText('<TS', Syntagm.Text) then
        InTitle := True;
      Continue;
    end;

    if InRFNote or InTitle or
       not (Syntagm.Kind in [skWhitespace, skSyntagm, skPunctuation]) then
      Continue;

    ASyntagmRanges[Index].StartIndex := Length(APlainText) + 1;
    ASyntagmRanges[Index].Length := Length(Syntagm.Text);
    APlainText := APlainText + Syntagm.Text;
  end;

  FirstVisibleByte := 1;
  while (FirstVisibleByte <= Length(APlainText)) and
        (APlainText[FirstVisibleByte] <= ' ') do
    Inc(FirstVisibleByte);
  LastVisibleByte := Length(APlainText);
  while (LastVisibleByte >= FirstVisibleByte) and
        (APlainText[LastVisibleByte] <= ' ') do
    Dec(LastVisibleByte);

  if (FirstVisibleByte > 1) or
     (LastVisibleByte < Length(APlainText)) then
  begin
    APlainText := Copy(APlainText, FirstVisibleByte,
      LastVisibleByte - FirstVisibleByte + 1);
    for Index := 0 to High(ASyntagmRanges) do
      if ASyntagmRanges[Index].Length > 0 then
      begin
        Dec(ASyntagmRanges[Index].StartIndex, FirstVisibleByte - 1);
        if (ASyntagmRanges[Index].StartIndex < 1) or
           (ASyntagmRanges[Index].StartIndex +
            ASyntagmRanges[Index].Length - 1 > Length(APlainText)) then
        begin
          ASyntagmRanges[Index].StartIndex := 0;
          ASyntagmRanges[Index].Length := 0;
        end;
      end;
  end;
end;

function TryNormalizeAssociationHighlightRanges(const AContext: string;
  const ARanges: TConcordanceHighlightRangeArray;
  out ANormalized: TConcordanceHighlightRangeArray): Boolean;
var
  CurrentEnd: Integer;
  Index: Integer;
  InsertIndex: Integer;
  NormalizedIndex: Integer;
  RangeValue: TConcordanceHighlightRange;
begin
  Result := False;
  ANormalized := nil;
  if AContext.IsEmpty or (Length(ARanges) = 0) then
    Exit;

  SetLength(ANormalized, Length(ARanges));
  for Index := 0 to High(ARanges) do
  begin
    if (ARanges[Index].StartIndex < 1) or
       (ARanges[Index].Length <= 0) or
       (ARanges[Index].StartIndex > Length(AContext)) or
       (ARanges[Index].Length >
        Length(AContext) - ARanges[Index].StartIndex + 1) then
    begin
      ANormalized := nil;
      Exit;
    end;
    ANormalized[Index] := ARanges[Index];
  end;

  for Index := 1 to High(ANormalized) do
  begin
    RangeValue := ANormalized[Index];
    InsertIndex := Index - 1;
    while (InsertIndex >= 0) and
          (ANormalized[InsertIndex].StartIndex > RangeValue.StartIndex) do
    begin
      ANormalized[InsertIndex + 1] := ANormalized[InsertIndex];
      Dec(InsertIndex);
    end;
    ANormalized[InsertIndex + 1] := RangeValue;
  end;

  NormalizedIndex := 0;
  for Index := 1 to High(ANormalized) do
  begin
    CurrentEnd := ANormalized[NormalizedIndex].StartIndex +
      ANormalized[NormalizedIndex].Length - 1;
    if ANormalized[Index].StartIndex <= CurrentEnd then
    begin
      CurrentEnd := Max(CurrentEnd, ANormalized[Index].StartIndex +
        ANormalized[Index].Length - 1);
      ANormalized[NormalizedIndex].Length := CurrentEnd -
        ANormalized[NormalizedIndex].StartIndex + 1;
    end
    else
    begin
      Inc(NormalizedIndex);
      ANormalized[NormalizedIndex] := ANormalized[Index];
    end;
  end;
  SetLength(ANormalized, NormalizedIndex + 1);
  Result := True;
end;

function TryProjectAssociationHighlightRanges(
  const AData: TAssociationPairData; const AContext: string;
  const ASyntagmRanges: TConcordanceHighlightRangeArray;
  out AHighlightRanges: TConcordanceHighlightRangeArray): Boolean;
var
  CandidateRanges: TConcordanceHighlightRangeArray;
  Index: Integer;
  SyntagmIndex: Integer;
begin
  Result := False;
  AHighlightRanges := nil;
  if Length(AData.TargetSyntagmIndexes) = 0 then
    Exit;

  SetLength(CandidateRanges, Length(AData.TargetSyntagmIndexes));
  for Index := 0 to High(AData.TargetSyntagmIndexes) do
  begin
    SyntagmIndex := AData.TargetSyntagmIndexes[Index];
    if (SyntagmIndex < 0) or
       (SyntagmIndex > High(ASyntagmRanges)) or
       (ASyntagmRanges[SyntagmIndex].Length <= 0) then
      Exit;
    CandidateRanges[Index] := ASyntagmRanges[SyntagmIndex];
  end;

  Result := TryNormalizeAssociationHighlightRanges(AContext,
    CandidateRanges, AHighlightRanges);
end;

function TryUnionAssociationHighlightRanges(const AContext: string;
  const ALeft, ARight: TConcordanceHighlightRangeArray;
  out AUnion: TConcordanceHighlightRangeArray): Boolean;
var
  Combined: TConcordanceHighlightRangeArray;
  Index: Integer;
begin
  AUnion := nil;
  SetLength(Combined, Length(ALeft) + Length(ARight));
  for Index := 0 to High(ALeft) do
    Combined[Index] := ALeft[Index];
  for Index := 0 to High(ARight) do
    Combined[Length(ALeft) + Index] := ARight[Index];
  Result := TryNormalizeAssociationHighlightRanges(AContext, Combined,
    AUnion);
end;

function AssociationMatches(const AData: TAssociationPairData;
  const AMatches: TTranslationPairMatchArray): Boolean;
var
  MatchIndex: Integer;
begin
  Result := False;
  for MatchIndex := 0 to High(AMatches) do
    if AnsiSameText(AData.SourcePhrase,
         AMatches[MatchIndex].SourcePhrase) and
       AnsiSameText(AData.TargetPhrase,
         AMatches[MatchIndex].TargetPhrase) then
      Exit(True);
end;

function TryProjectMatchingAssociationHighlightRanges(
  const AAssociationData: TAssociationPairDataArray;
  const AMatches: TTranslationPairMatchArray; const AContext: string;
  const ASyntagmRanges: TConcordanceHighlightRangeArray;
  out AHighlightRanges: TConcordanceHighlightRangeArray): Boolean;
var
  AssociationIndex: Integer;
  AssociationRanges: TConcordanceHighlightRangeArray;
  CombinedRanges: TConcordanceHighlightRangeArray;
begin
  Result := False;
  AHighlightRanges := nil;
  AssociationRanges := nil;
  CombinedRanges := nil;
  for AssociationIndex := 0 to High(AAssociationData) do
    if AssociationMatches(AAssociationData[AssociationIndex], AMatches) then
    begin
      if not TryProjectAssociationHighlightRanges(
          AAssociationData[AssociationIndex], AContext, ASyntagmRanges,
          AssociationRanges) or
         not TryUnionAssociationHighlightRanges(AContext,
          AHighlightRanges, AssociationRanges, CombinedRanges) then
      begin
        AHighlightRanges := nil;
        Exit(False);
      end;
      AHighlightRanges := CombinedRanges;
      Result := True;
    end;
end;

end.
