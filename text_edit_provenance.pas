unit text_edit_provenance;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Math, SysUtils;

const
  TEXT_ORIGIN_INSERTED = -1;

type
  ETextEditProvenance = class(Exception);

  TVerseEditTraceMode = (
    vetmExact,
    vetmRawConservative,
    vetmLegacyFallback
  );

  { Maps every current UTF-16 code unit to its zero-based offset in the
    original visible text. Inserted units use TEXT_ORIGIN_INSERTED. }
  TTextOriginMap = class
  private
    FOriginalLength: Integer;
    FOrigins: array of Integer;
    function GetCurrentLength: Integer;
  public
    constructor CreateIdentity(AOriginalLength: Integer);
    function Clone: TTextOriginMap;
    function IsIdentity: Boolean;
    procedure Replace(AStartUTF16, ALengthUTF16,
      AReplacementLengthUTF16: Integer);
    function TryGetOriginalOffset(ACurrentOffsetUTF16: Integer;
      out AOriginalOffsetUTF16: Integer): Boolean;
    function TryProjectOriginalRange(AOriginalStartUTF16,
      AOriginalLengthUTF16: Integer;
      out ACurrentStartUTF16: Integer): Boolean;
    property CurrentLength: Integer read GetCurrentLength;
    property OriginalLength: Integer read FOriginalLength;
  end;

  { Immutable to consumers except through edit operations. Clone returns a
    caller-owned deep copy suitable for a save/reconciliation boundary. }
  TVerseEditTrace = class
  private
    FCurrentText: UnicodeString;
    FMode: TVerseEditTraceMode;
    FOriginMap: TTextOriginMap;
    FOriginalText: UnicodeString;
    FTokenizerVersion: Integer;
  public
    constructor CreateIdentity(const AOriginalText: UnicodeString;
      ATokenizerVersion: Integer);
    destructor Destroy; override;
    function Clone: TVerseEditTrace;
    function IsBaselineIdentity: Boolean;
    procedure ApplyReplacement(AStartUTF16, ALengthUTF16: Integer;
      const AReplacement: UnicodeString);
    procedure ApplyRawVisibleChange(const ANewVisibleText: UnicodeString);
    procedure MarkLegacyFallback;
    procedure ResetBaseline(const AText: UnicodeString;
      ATokenizerVersion: Integer);
    property CurrentText: UnicodeString read FCurrentText;
    property Mode: TVerseEditTraceMode read FMode;
    property OriginMap: TTextOriginMap read FOriginMap;
    property OriginalText: UnicodeString read FOriginalText;
    property TokenizerVersion: Integer read FTokenizerVersion;
  end;

implementation

function IsHighSurrogate(AValue: WideChar): Boolean;
begin
  Result := (Ord(AValue) >= $D800) and (Ord(AValue) <= $DBFF);
end;

function IsLowSurrogate(AValue: WideChar): Boolean;
begin
  Result := (Ord(AValue) >= $DC00) and (Ord(AValue) <= $DFFF);
end;

function IsUTF16Boundary(const AText: UnicodeString;
  AOffsetUTF16: Integer): Boolean;
begin
  Result := (AOffsetUTF16 >= 0) and (AOffsetUTF16 <= Length(AText));
  if not Result then
    Exit;
  Result := (AOffsetUTF16 = 0) or (AOffsetUTF16 = Length(AText)) or
    not (IsHighSurrogate(AText[AOffsetUTF16]) and
      IsLowSurrogate(AText[AOffsetUTF16 + 1]));
end;

function IsValidUTF16(const AText: UnicodeString): Boolean;
var
  Index: Integer;
begin
  Index := 1;
  while Index <= Length(AText) do
  begin
    if IsHighSurrogate(AText[Index]) then
    begin
      if (Index = Length(AText)) or not IsLowSurrogate(AText[Index + 1]) then
        Exit(False);
      Inc(Index, 2);
    end
    else
    begin
      if IsLowSurrogate(AText[Index]) then
        Exit(False);
      Inc(Index);
    end;
  end;
  Result := True;
end;

procedure RequireValidUTF16(const AText: UnicodeString;
  const AContext: string);
begin
  if not IsValidUTF16(AText) then
    raise ETextEditProvenance.CreateFmt('%s contains malformed UTF-16',
      [AContext]);
end;

constructor TTextOriginMap.CreateIdentity(AOriginalLength: Integer);
var
  Index: Integer;
begin
  inherited Create;
  if AOriginalLength < 0 then
    raise ETextEditProvenance.CreateFmt(
      'Original UTF-16 length cannot be negative: %d', [AOriginalLength]);
  FOriginalLength := AOriginalLength;
  SetLength(FOrigins, AOriginalLength);
  for Index := 0 to High(FOrigins) do
    FOrigins[Index] := Index;
end;

function TTextOriginMap.GetCurrentLength: Integer;
begin
  Result := Length(FOrigins);
end;

function TTextOriginMap.Clone: TTextOriginMap;
var
  Index: Integer;
begin
  Result := TTextOriginMap.CreateIdentity(FOriginalLength);
  SetLength(Result.FOrigins, Length(FOrigins));
  for Index := 0 to High(FOrigins) do
    Result.FOrigins[Index] := FOrigins[Index];
end;

function TTextOriginMap.IsIdentity: Boolean;
var
  Index: Integer;
begin
  Result := Length(FOrigins) = FOriginalLength;
  if not Result then
    Exit;
  for Index := 0 to High(FOrigins) do
    if FOrigins[Index] <> Index then
      Exit(False);
end;

procedure TTextOriginMap.Replace(AStartUTF16, ALengthUTF16,
  AReplacementLengthUTF16: Integer);
var
  Index, NewLength, OldFinish, TailLength: Integer;
  Updated: array of Integer;
begin
  if (AStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (AReplacementLengthUTF16 < 0) or
     (AStartUTF16 > Length(FOrigins)) or
     (ALengthUTF16 > Length(FOrigins) - AStartUTF16) then
    raise ETextEditProvenance.CreateFmt(
      'Invalid current UTF-16 replacement [%d,%d) -> %d for length %d',
      [AStartUTF16, AStartUTF16 + ALengthUTF16,
       AReplacementLengthUTF16, Length(FOrigins)]);

  OldFinish := AStartUTF16 + ALengthUTF16;
  TailLength := Length(FOrigins) - OldFinish;
  NewLength := AStartUTF16 + AReplacementLengthUTF16 + TailLength;
  SetLength(Updated, NewLength);

  for Index := 0 to AStartUTF16 - 1 do
    Updated[Index] := FOrigins[Index];
  for Index := AStartUTF16 to
    AStartUTF16 + AReplacementLengthUTF16 - 1 do
    Updated[Index] := TEXT_ORIGIN_INSERTED;
  for Index := 0 to TailLength - 1 do
    Updated[AStartUTF16 + AReplacementLengthUTF16 + Index] :=
      FOrigins[OldFinish + Index];
  FOrigins := Updated;
end;

function TTextOriginMap.TryGetOriginalOffset(
  ACurrentOffsetUTF16: Integer; out AOriginalOffsetUTF16: Integer): Boolean;
begin
  AOriginalOffsetUTF16 := TEXT_ORIGIN_INSERTED;
  Result := (ACurrentOffsetUTF16 >= 0) and
    (ACurrentOffsetUTF16 < Length(FOrigins));
  if not Result then
    Exit;
  AOriginalOffsetUTF16 := FOrigins[ACurrentOffsetUTF16];
  Result := AOriginalOffsetUTF16 <> TEXT_ORIGIN_INSERTED;
end;

function TTextOriginMap.TryProjectOriginalRange(AOriginalStartUTF16,
  AOriginalLengthUTF16: Integer;
  out ACurrentStartUTF16: Integer): Boolean;
var
  CurrentIndex, LeftIndex, Offset, RightIndex: Integer;
begin
  ACurrentStartUTF16 := -1;
  Result := (AOriginalStartUTF16 >= 0) and
    (AOriginalLengthUTF16 >= 0) and
    (AOriginalStartUTF16 <= FOriginalLength) and
    (AOriginalLengthUTF16 <= FOriginalLength - AOriginalStartUTF16);
  if not Result then
    Exit;

  if AOriginalLengthUTF16 = 0 then
  begin
    { Inserted units at a boundary make its affinity ambiguous. Only adjacent
      surviving original units define one exact zero-width projection. }
    if FOriginalLength = 0 then
    begin
      Result := Length(FOrigins) = 0;
      if Result then
        ACurrentStartUTF16 := 0;
      Exit;
    end;
    LeftIndex := -1;
    RightIndex := -1;
    for CurrentIndex := 0 to High(FOrigins) do
    begin
      if FOrigins[CurrentIndex] = AOriginalStartUTF16 - 1 then
        LeftIndex := CurrentIndex;
      if FOrigins[CurrentIndex] = AOriginalStartUTF16 then
        RightIndex := CurrentIndex;
    end;
    if AOriginalStartUTF16 = 0 then
      Result := RightIndex = 0
    else if AOriginalStartUTF16 = FOriginalLength then
      Result := LeftIndex = High(FOrigins)
    else
      Result := (LeftIndex >= 0) and (RightIndex = LeftIndex + 1);
    if Result then
    begin
      if AOriginalStartUTF16 = FOriginalLength then
        ACurrentStartUTF16 := Length(FOrigins)
      else
        ACurrentStartUTF16 := RightIndex;
    end;
    Exit;
  end;

  for CurrentIndex := 0 to High(FOrigins) do
    if FOrigins[CurrentIndex] = AOriginalStartUTF16 then
    begin
      if AOriginalLengthUTF16 > Length(FOrigins) - CurrentIndex then
        Exit(False);
      for Offset := 0 to AOriginalLengthUTF16 - 1 do
        if FOrigins[CurrentIndex + Offset] <>
          AOriginalStartUTF16 + Offset then
          Exit(False);
      ACurrentStartUTF16 := CurrentIndex;
      Exit(True);
    end;
  Result := False;
end;

constructor TVerseEditTrace.CreateIdentity(const AOriginalText: UnicodeString;
  ATokenizerVersion: Integer);
begin
  inherited Create;
  FOriginMap := nil;
  ResetBaseline(AOriginalText, ATokenizerVersion);
end;

destructor TVerseEditTrace.Destroy;
begin
  FOriginMap.Free;
  inherited Destroy;
end;

function TVerseEditTrace.Clone: TVerseEditTrace;
begin
  Result := TVerseEditTrace.CreateIdentity(FOriginalText,
    FTokenizerVersion);
  Result.FCurrentText := FCurrentText;
  Result.FMode := FMode;
  Result.FOriginMap.Free;
  Result.FOriginMap := FOriginMap.Clone;
end;

function TVerseEditTrace.IsBaselineIdentity: Boolean;
begin
  Result := (FMode = vetmExact) and
    (FCurrentText = FOriginalText) and FOriginMap.IsIdentity;
end;

procedure TVerseEditTrace.ApplyReplacement(AStartUTF16,
  ALengthUTF16: Integer; const AReplacement: UnicodeString);
begin
  RequireValidUTF16(AReplacement, 'Replacement');
  if (AStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (AStartUTF16 > Length(FCurrentText)) or
     (ALengthUTF16 > Length(FCurrentText) - AStartUTF16) or
     not IsUTF16Boundary(FCurrentText, AStartUTF16) or
     not IsUTF16Boundary(FCurrentText, AStartUTF16 + ALengthUTF16) then
    raise ETextEditProvenance.CreateFmt(
      'Invalid visible UTF-16 replacement [%d,%d) for length %d',
      [AStartUTF16, AStartUTF16 + ALengthUTF16,
       Length(FCurrentText)]);

  FOriginMap.Replace(AStartUTF16, ALengthUTF16, Length(AReplacement));
  FCurrentText := Copy(FCurrentText, 1, AStartUTF16) + AReplacement +
    Copy(FCurrentText, AStartUTF16 + ALengthUTF16 + 1, MaxInt);
end;

procedure TVerseEditTrace.ApplyRawVisibleChange(
  const ANewVisibleText: UnicodeString);
var
  CommonPrefix, CommonSuffix, MaximumCommon: Integer;
begin
  RequireValidUTF16(ANewVisibleText, 'Raw visible text');
  { A Raw edit can change lexical annotations without changing visible text. }
  if FMode = vetmExact then
    FMode := vetmRawConservative;
  if ANewVisibleText = FCurrentText then
    Exit;

  MaximumCommon := Min(Length(FCurrentText), Length(ANewVisibleText));
  CommonPrefix := 0;
  while (CommonPrefix < MaximumCommon) and
    (FCurrentText[CommonPrefix + 1] = ANewVisibleText[CommonPrefix + 1]) do
    Inc(CommonPrefix);
  while (CommonPrefix > 0) and
    (not IsUTF16Boundary(FCurrentText, CommonPrefix) or
     not IsUTF16Boundary(ANewVisibleText, CommonPrefix)) do
    Dec(CommonPrefix);

  MaximumCommon := Min(Length(FCurrentText) - CommonPrefix,
    Length(ANewVisibleText) - CommonPrefix);
  CommonSuffix := 0;
  while (CommonSuffix < MaximumCommon) and
    (FCurrentText[Length(FCurrentText) - CommonSuffix] =
     ANewVisibleText[Length(ANewVisibleText) - CommonSuffix]) do
    Inc(CommonSuffix);
  while (CommonSuffix > 0) and
    (not IsUTF16Boundary(FCurrentText,
       Length(FCurrentText) - CommonSuffix) or
     not IsUTF16Boundary(ANewVisibleText,
       Length(ANewVisibleText) - CommonSuffix)) do
    Dec(CommonSuffix);

  FOriginMap.Replace(CommonPrefix,
    Length(FCurrentText) - CommonPrefix - CommonSuffix,
    Length(ANewVisibleText) - CommonPrefix - CommonSuffix);
  FCurrentText := ANewVisibleText;
end;

procedure TVerseEditTrace.MarkLegacyFallback;
begin
  FMode := vetmLegacyFallback;
end;

procedure TVerseEditTrace.ResetBaseline(const AText: UnicodeString;
  ATokenizerVersion: Integer);
begin
  RequireValidUTF16(AText, 'Trace baseline');
  FOriginMap.Free;
  FOriginMap := TTextOriginMap.CreateIdentity(Length(AText));
  FOriginalText := AText;
  FCurrentText := AText;
  FTokenizerVersion := ATokenizerVersion;
  FMode := vetmExact;
end;

end.
