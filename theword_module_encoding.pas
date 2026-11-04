unit theword_module_encoding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTheWordModuleEncodingKind = (
    twmekUtf8Bom,
    twmekUtf8,
    twmekLegacyOrInvalid
  );

  TTheWordModuleEncodingInfo = record
    Kind: TTheWordModuleEncodingKind;
    HasUtf8Bom: Boolean;
    IsStrictUtf8: Boolean;
    InvalidByteOffset: SizeInt;
    RequiresConfirmation: Boolean;
  end;

function IsStrictUtf8(const ABytes: RawByteString;
  out AInvalidByteOffset: SizeInt): Boolean;
function InspectTheWordModuleEncoding(
  const ABytes: RawByteString): TTheWordModuleEncodingInfo;
function InspectTheWordModuleEncodingFile(
  const AFileName: string): TTheWordModuleEncodingInfo;
function NormalizeTheWordModuleLineBreaks(const ABytes: RawByteString): RawByteString;
procedure LoadTheWordModuleLines(const AFileName: string; ALines: TStrings);

implementation

const
  UTF8_BOM: RawByteString = #$EF#$BB#$BF;

function IsContinuationByte(AValue: Byte): Boolean; inline;
begin
  Result := (AValue and $C0) = $80;
end;

function IsStrictUtf8(const ABytes: RawByteString;
  out AInvalidByteOffset: SizeInt): Boolean;
var
  B0, B1, B2, B3: Byte;
  Index, ByteCount: SizeInt;
begin
  AInvalidByteOffset := 0;
  ByteCount := Length(ABytes);
  Index := 1;
  if Copy(ABytes, 1, Length(UTF8_BOM)) = UTF8_BOM then
    Inc(Index, Length(UTF8_BOM));

  while Index <= ByteCount do
  begin
    B0 := Byte(ABytes[Index]);
    if B0 <= $7F then
    begin
      Inc(Index);
      Continue;
    end;

    if (B0 >= $C2) and (B0 <= $DF) then
    begin
      if Index + 1 > ByteCount then
        Break;
      B1 := Byte(ABytes[Index + 1]);
      if not IsContinuationByte(B1) then
        Break;
      Inc(Index, 2);
      Continue;
    end;

    if (B0 >= $E0) and (B0 <= $EF) then
    begin
      if Index + 2 > ByteCount then
        Break;
      B1 := Byte(ABytes[Index + 1]);
      B2 := Byte(ABytes[Index + 2]);
      if not IsContinuationByte(B2) then
        Break;
      if (B0 = $E0) and ((B1 < $A0) or (B1 > $BF)) then
        Break;
      if (B0 = $ED) and ((B1 < $80) or (B1 > $9F)) then
        Break;
      if (B0 <> $E0) and (B0 <> $ED) and
         not IsContinuationByte(B1) then
        Break;
      Inc(Index, 3);
      Continue;
    end;

    if (B0 >= $F0) and (B0 <= $F4) then
    begin
      if Index + 3 > ByteCount then
        Break;
      B1 := Byte(ABytes[Index + 1]);
      B2 := Byte(ABytes[Index + 2]);
      B3 := Byte(ABytes[Index + 3]);
      if not IsContinuationByte(B2) or not IsContinuationByte(B3) then
        Break;
      if (B0 = $F0) and ((B1 < $90) or (B1 > $BF)) then
        Break;
      if (B0 = $F4) and ((B1 < $80) or (B1 > $8F)) then
        Break;
      if (B0 <> $F0) and (B0 <> $F4) and
         not IsContinuationByte(B1) then
        Break;
      Inc(Index, 4);
      Continue;
    end;

    Break;
  end;

  Result := Index > ByteCount;
  if not Result then
    AInvalidByteOffset := Index;
end;

function InspectTheWordModuleEncoding(
  const ABytes: RawByteString): TTheWordModuleEncodingInfo;
begin
  Result.HasUtf8Bom := Copy(ABytes, 1, Length(UTF8_BOM)) = UTF8_BOM;
  Result.IsStrictUtf8 := IsStrictUtf8(ABytes, Result.InvalidByteOffset);
  Result.RequiresConfirmation := not Result.IsStrictUtf8;
  if not Result.IsStrictUtf8 then
    Result.Kind := twmekLegacyOrInvalid
  else if Result.HasUtf8Bom then
    Result.Kind := twmekUtf8Bom
  else
    Result.Kind := twmekUtf8;
end;

function ReadAllBytes(const AFileName: string): RawByteString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

function InspectTheWordModuleEncodingFile(
  const AFileName: string): TTheWordModuleEncodingInfo;
begin
  Result := InspectTheWordModuleEncoding(ReadAllBytes(AFileName));
end;

{ Some editors save modules with CR CR LF line endings. TStrings treats the
  lone CR as a line break too, which inserts an empty line after every verse
  and shifts all verses after the first. Collapse any run of CRs to one. }
function NormalizeTheWordModuleLineBreaks(const ABytes: RawByteString): RawByteString;
begin
  Result := ABytes;
  if Pos(#13#13, Result) = 0 then
    Exit;
  repeat
    Result := StringReplace(Result, #13#13, #13, [rfReplaceAll]);
  until Pos(#13#13, Result) = 0;
end;

{ Same as ALines.LoadFromFile, but with CR CR LF line endings normalized. }
procedure LoadTheWordModuleLines(const AFileName: string; ALines: TStrings);
var
  Bytes: RawByteString;
  Stream: TStringStream;
begin
  Bytes := ReadAllBytes(AFileName);
  if Pos(#13#13, Bytes) = 0 then
  begin
    ALines.LoadFromFile(AFileName);
    Exit;
  end;
  Stream := TStringStream.Create(NormalizeTheWordModuleLineBreaks(Bytes));
  try
    ALines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.
