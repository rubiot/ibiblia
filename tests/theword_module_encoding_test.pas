unit theword_module_encoding_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, theword_module_encoding;

type
  TTheWordModuleEncodingTests = class(TTestCase)
  private
    FTempFileName: string;
    procedure WriteFixture(const ABytes: RawByteString);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Utf8BomWithLegacyCharsetDoesNotRequireConfirmation;
    procedure StrictUtf8WithoutBomDoesNotRequireConfirmation;
    procedure AsciiWithCharsetDoesNotRequireConfirmation;
    procedure Windows1252RequiresConfirmation;
    procedure MalformedUtf8SequencesRequireConfirmation;
    procedure FileInspectionPreservesBinaryEncodingEvidence;
  end;

implementation

const
  UTF8_BOM: RawByteString = #$EF#$BB#$BF;

procedure TTheWordModuleEncodingTests.WriteFixture(
  const ABytes: RawByteString);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FTempFileName, fmCreate);
  try
    if Length(ABytes) > 0 then
      Stream.WriteBuffer(ABytes[1], Length(ABytes));
  finally
    Stream.Free;
  end;
end;

procedure TTheWordModuleEncodingTests.SetUp;
begin
  FTempFileName := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia-encoding-%x-%x.ont', [PtrUInt(Self), GetTickCount64]);
end;

procedure TTheWordModuleEncodingTests.TearDown;
begin
  if FileExists(FTempFileName) then
    DeleteFile(FTempFileName);
end;

procedure TTheWordModuleEncodingTests.
  Utf8BomWithLegacyCharsetDoesNotRequireConfirmation;
var
  Bytes: RawByteString;
  Info: TTheWordModuleEncodingInfo;
begin
  Bytes := UTF8_BOM + 'In the beginning' + #13#10 +
    'description=KJV' + #13#10 + 'charset=0' + #13#10;

  Info := InspectTheWordModuleEncoding(Bytes);

  AssertEquals(Ord(twmekUtf8Bom), Ord(Info.Kind));
  AssertTrue(Info.HasUtf8Bom);
  AssertTrue(Info.IsStrictUtf8);
  AssertFalse(Info.RequiresConfirmation);
  AssertEquals(0, Info.InvalidByteOffset);
end;

procedure TTheWordModuleEncodingTests.
  StrictUtf8WithoutBomDoesNotRequireConfirmation;
var
  Bytes: RawByteString;
  Info: TTheWordModuleEncodingInfo;
begin
  Bytes := 'a' + RawByteString(#$C3#$A7) + RawByteString(#$C3#$A3) +
    'o' + #10 + 'charset=windows-1252' + #10;

  Info := InspectTheWordModuleEncoding(Bytes);

  AssertEquals(Ord(twmekUtf8), Ord(Info.Kind));
  AssertFalse(Info.HasUtf8Bom);
  AssertTrue(Info.IsStrictUtf8);
  AssertFalse(Info.RequiresConfirmation);
end;

procedure TTheWordModuleEncodingTests.
  AsciiWithCharsetDoesNotRequireConfirmation;
var
  Info: TTheWordModuleEncodingInfo;
begin
  Info := InspectTheWordModuleEncoding(
    'plain' + #10 + 'charset=0' + #10);

  AssertEquals(Ord(twmekUtf8), Ord(Info.Kind));
  AssertTrue(Info.IsStrictUtf8);
  AssertFalse(Info.RequiresConfirmation);
end;

procedure TTheWordModuleEncodingTests.Windows1252RequiresConfirmation;
var
  Info: TTheWordModuleEncodingInfo;
begin
  Info := InspectTheWordModuleEncoding(
    'caf' + RawByteString(#$E9) + #10 + 'charset=windows-1252' + #10);

  AssertEquals(Ord(twmekLegacyOrInvalid), Ord(Info.Kind));
  AssertFalse(Info.IsStrictUtf8);
  AssertTrue(Info.RequiresConfirmation);
  AssertEquals(4, Info.InvalidByteOffset);
end;

procedure TTheWordModuleEncodingTests.
  MalformedUtf8SequencesRequireConfirmation;
const
  INVALID_SEQUENCES: array[0..5] of RawByteString = (
    #$C0#$AF,
    #$E0#$80#$AF,
    #$ED#$A0#$80,
    #$F0#$80#$80#$AF,
    #$F4#$90#$80#$80,
    #$E2#$82
  );
var
  Index: Integer;
  Info: TTheWordModuleEncodingInfo;
begin
  for Index := Low(INVALID_SEQUENCES) to High(INVALID_SEQUENCES) do
  begin
    Info := InspectTheWordModuleEncoding('x' + INVALID_SEQUENCES[Index]);
    AssertFalse('sequence ' + IntToStr(Index) + ' must be rejected',
      Info.IsStrictUtf8);
    AssertTrue('sequence ' + IntToStr(Index) + ' requires confirmation',
      Info.RequiresConfirmation);
    AssertEquals('invalid sequence offset ' + IntToStr(Index), 2,
      Info.InvalidByteOffset);
  end;
end;

procedure TTheWordModuleEncodingTests.
  FileInspectionPreservesBinaryEncodingEvidence;
var
  Bytes: RawByteString;
  Info: TTheWordModuleEncodingInfo;
begin
  Bytes := UTF8_BOM + 'Jo' + RawByteString(#$C3#$A3) + 'o' + #10 +
    'charset=0' + #10;
  WriteFixture(Bytes);

  Info := InspectTheWordModuleEncodingFile(FTempFileName);

  AssertEquals(Ord(twmekUtf8Bom), Ord(Info.Kind));
  AssertTrue(Info.IsStrictUtf8);
  AssertFalse(Info.RequiresConfirmation);
end;

initialization
  RegisterTest(TTheWordModuleEncodingTests);

end.
