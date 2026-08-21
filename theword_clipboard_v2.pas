unit theword_clipboard_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, theword_ont_codec_v2;

const
  THEWORD_CLIPBOARD_FORMAT_V2 =
    'iBiblia.TheWord.SemanticVerseFragment.v1';
  THEWORD_CLIPBOARD_SCHEMA_V2 = 'ibiblia.theword-semantic-fragment';
  THEWORD_CLIPBOARD_VERSION_V2 = 1;

type
  ETheWordClipboardV2 = class(Exception);

  TTheWordClipboardObjectKindV2 = (
    twcokLexical,
    twcokOpaque,
    twcokStructural
  );

  TTheWordClipboardMarkV2 = record
    Kind: TTheWordSemanticKind;
    StartOffset: Integer;
    EndOffset: Integer;
  end;

  TTheWordClipboardObjectV2 = record
    ObjectKind: TTheWordClipboardObjectKindV2;
    SubKind: Integer;
    Offset: Integer;
    RawLexeme: UnicodeString;
    SourceOrder: Integer;
  end;

  { This is a transport object. Offsets are relative UTF-16 code-unit
    offsets into Text. }
  TTheWordClipboardFragmentV2 = class
  private
    FText: UnicodeString;
    FMarks: array of TTheWordClipboardMarkV2;
    FObjects: array of TTheWordClipboardObjectV2;
    function GetMark(AIndex: Integer): TTheWordClipboardMarkV2;
    function GetMarkCount: Integer;
    function GetObject(AIndex: Integer): TTheWordClipboardObjectV2;
    function GetObjectCount: Integer;
    procedure AddMark(const AValue: TTheWordClipboardMarkV2);
    procedure AddObject(const AValue: TTheWordClipboardObjectV2);
    procedure ClearObjects;
  public
    property Text: UnicodeString read FText;
    property MarkCount: Integer read GetMarkCount;
    property Marks[AIndex: Integer]: TTheWordClipboardMarkV2 read GetMark;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[AIndex: Integer]: TTheWordClipboardObjectV2
      read GetObject;
  end;

  TTheWordClipboardCodecV2 = class
  public
    class function EncodeSelection(ADocument: TTheWordVerseDocument;
      AStartUTF16, ALengthUTF16: Integer; out APlainText: UnicodeString;
      out APayload: string): Boolean; static;
    class function TryDecodePayload(const APayload: string;
      out AFragment: TTheWordClipboardFragmentV2): Boolean; static;
    class function NewPlainFragment(const AText: UnicodeString):
      TTheWordClipboardFragmentV2; static;
    class function BuildPasteRaw(ADocument: TTheWordVerseDocument;
      AStartUTF16, ALengthUTF16: Integer;
      AFragment: TTheWordClipboardFragmentV2;
      out ARaw: UnicodeString): Boolean; static;
    class function BuildCutRaw(ADocument: TTheWordVerseDocument;
      AStartUTF16, ALengthUTF16: Integer;
      out ARaw: UnicodeString): Boolean; static;
  end;

function NormalizeTheWordClipboardText(
  const AValue: UnicodeString): UnicodeString;

implementation

uses
  Math, fpjson, jsonparser;

const
  MAX_CLIPBOARD_PAYLOAD_BYTES = 16 * 1024 * 1024;
  MAX_CLIPBOARD_ITEMS = 10000;

type
  TBooleanArray = array of Boolean;

function IsHighSurrogate(AValue: WideChar): Boolean;
begin
  Result := (Ord(AValue) >= $D800) and (Ord(AValue) <= $DBFF);
end;

function IsLowSurrogate(AValue: WideChar): Boolean;
begin
  Result := (Ord(AValue) >= $DC00) and (Ord(AValue) <= $DFFF);
end;

function IsUtf16Boundary(const AText: UnicodeString;
  AOffset: Integer): Boolean;
begin
  Result := (AOffset >= 0) and (AOffset <= Length(AText));
  if Result and (AOffset > 0) and (AOffset < Length(AText)) then
    Result := not (IsHighSurrogate(AText[AOffset]) and
      IsLowSurrogate(AText[AOffset + 1]));
end;

function IsValidRange(const AText: UnicodeString; AStartUTF16,
  ALengthUTF16: Integer): Boolean;
begin
  Result := (AStartUTF16 >= 0) and (ALengthUTF16 >= 0) and
    (AStartUTF16 <= Length(AText)) and
    (ALengthUTF16 <= Length(AText) - AStartUTF16) and
    IsUtf16Boundary(AText, AStartUTF16) and
    IsUtf16Boundary(AText, AStartUTF16 + ALengthUTF16);
end;

function NormalizeTheWordClipboardText(
  const AValue: UnicodeString): UnicodeString;
var
  Index: Integer;
begin
  Result := '';
  Index := 1;
  while Index <= Length(AValue) do
  begin
    if AValue[Index] = #13 then
    begin
      Result := Result + ' ';
      if (Index < Length(AValue)) and (AValue[Index + 1] = #10) then
        Inc(Index);
    end
    else if AValue[Index] = #10 then
      Result := Result + ' '
    else
      Result := Result + AValue[Index];
    Inc(Index);
  end;
end;

function TTheWordClipboardFragmentV2.GetMark(
  AIndex: Integer): TTheWordClipboardMarkV2;
begin
  Result := FMarks[AIndex];
end;

function TTheWordClipboardFragmentV2.GetMarkCount: Integer;
begin
  Result := Length(FMarks);
end;

function TTheWordClipboardFragmentV2.GetObject(
  AIndex: Integer): TTheWordClipboardObjectV2;
begin
  Result := FObjects[AIndex];
end;

function TTheWordClipboardFragmentV2.GetObjectCount: Integer;
begin
  Result := Length(FObjects);
end;

procedure TTheWordClipboardFragmentV2.AddMark(
  const AValue: TTheWordClipboardMarkV2);
begin
  SetLength(FMarks, Length(FMarks) + 1);
  FMarks[High(FMarks)] := AValue;
end;

procedure TTheWordClipboardFragmentV2.AddObject(
  const AValue: TTheWordClipboardObjectV2);
begin
  SetLength(FObjects, Length(FObjects) + 1);
  FObjects[High(FObjects)] := AValue;
end;

procedure TTheWordClipboardFragmentV2.ClearObjects;
begin
  SetLength(FObjects, 0);
end;

function SemanticKindName(AKind: TTheWordSemanticKind): string;
begin
  Result := '';
  case AKind of
    twsmItalic: Result := 'FI';
    twsmWordsOfJesus: Result := 'FR';
    twsmOldTestamentQuote: Result := 'FO';
    twsmUnderline: Result := 'FU';
    twsmStrikethrough: Result := 'S';
    twsmSuperscript: Result := 'SUP';
    twsmSubscript: Result := 'SUB';
  end;
end;

function SemanticKindFromName(const AName: string): TTheWordSemanticKind;
begin
  if AName = 'FI' then Exit(twsmItalic);
  if AName = 'FR' then Exit(twsmWordsOfJesus);
  if AName = 'FO' then Exit(twsmOldTestamentQuote);
  if AName = 'FU' then Exit(twsmUnderline);
  if AName = 'S' then Exit(twsmStrikethrough);
  if AName = 'SUP' then Exit(twsmSuperscript);
  if AName = 'SUB' then Exit(twsmSubscript);
  raise ETheWordClipboardV2.CreateFmt('Unknown semantic kind: %s', [AName]);
end;

function ObjectKindName(AKind: TTheWordClipboardObjectKindV2): string;
begin
  Result := '';
  case AKind of
    twcokLexical: Result := 'lexical';
    twcokOpaque: Result := 'opaque';
    twcokStructural: Result := 'structural';
  end;
end;

function ObjectKindFromName(const AName: string):
  TTheWordClipboardObjectKindV2;
begin
  if AName = 'lexical' then Exit(twcokLexical);
  if AName = 'opaque' then Exit(twcokOpaque);
  if AName = 'structural' then Exit(twcokStructural);
  raise ETheWordClipboardV2.CreateFmt('Unknown object type: %s', [AName]);
end;

function SubKindName(AObjectKind: TTheWordClipboardObjectKindV2;
  ASubKind: Integer): string;
begin
  Result := '';
  case AObjectKind of
    twcokLexical:
      case TTheWordLexicalKind(ASubKind) of
        twlkStrongGreek: Result := 'strongGreek';
        twlkStrongHebrew: Result := 'strongHebrew';
        twlkMorphology: Result := 'morphology';
        twlkWordTokenEnd: Result := 'wordTokenEnd';
      end;
    twcokOpaque:
      case TTheWordOpaqueKind(ASubKind) of
        twokUnknownTag: Result := 'unknownTag';
        twokMalformedTag: Result := 'malformedTag';
      end;
    twcokStructural:
      case TTheWordStructuralKind(ASubKind) of
        twstSyntagmBoundary: Result := 'syntagmBoundary';
      end;
  end;
end;

function SubKindFromName(AObjectKind: TTheWordClipboardObjectKindV2;
  const AName: string): Integer;
begin
  case AObjectKind of
    twcokLexical:
      begin
        if AName = 'strongGreek' then Exit(Ord(twlkStrongGreek));
        if AName = 'strongHebrew' then Exit(Ord(twlkStrongHebrew));
        if AName = 'morphology' then Exit(Ord(twlkMorphology));
        if AName = 'wordTokenEnd' then Exit(Ord(twlkWordTokenEnd));
      end;
    twcokOpaque:
      begin
        if AName = 'unknownTag' then Exit(Ord(twokUnknownTag));
        if AName = 'malformedTag' then Exit(Ord(twokMalformedTag));
      end;
    twcokStructural:
      if AName = 'syntagmBoundary' then
        Exit(Ord(twstSyntagmBoundary));
  end;
  raise ETheWordClipboardV2.CreateFmt('Unknown object kind: %s', [AName]);
end;

function CompareMarks(const ALeft, ARight: TTheWordClipboardMarkV2): Integer;
begin
  Result := ALeft.StartOffset - ARight.StartOffset;
  if Result = 0 then
    Result := ARight.EndOffset - ALeft.EndOffset;
  if Result = 0 then
    Result := Ord(ALeft.Kind) - Ord(ARight.Kind);
end;

function CompareObjects(const ALeft,
  ARight: TTheWordClipboardObjectV2): Integer;
begin
  Result := ALeft.Offset - ARight.Offset;
  if Result = 0 then
    Result := ALeft.SourceOrder - ARight.SourceOrder;
  if Result = 0 then
    Result := Ord(ALeft.ObjectKind) - Ord(ARight.ObjectKind);
end;

procedure SortFragment(AFragment: TTheWordClipboardFragmentV2);
var
  I, J: Integer;
  Mark: TTheWordClipboardMarkV2;
  Obj: TTheWordClipboardObjectV2;
begin
  for I := 0 to AFragment.MarkCount - 2 do
    for J := I + 1 to AFragment.MarkCount - 1 do
      if CompareMarks(AFragment.FMarks[I], AFragment.FMarks[J]) > 0 then
      begin
        Mark := AFragment.FMarks[I];
        AFragment.FMarks[I] := AFragment.FMarks[J];
        AFragment.FMarks[J] := Mark;
      end;
  for I := 0 to AFragment.ObjectCount - 2 do
    for J := I + 1 to AFragment.ObjectCount - 1 do
      if CompareObjects(AFragment.FObjects[I], AFragment.FObjects[J]) > 0 then
      begin
        Obj := AFragment.FObjects[I];
        AFragment.FObjects[I] := AFragment.FObjects[J];
        AFragment.FObjects[J] := Obj;
      end;
end;

procedure RequireFields(AObject: TJSONObject;
  const AAllowed: array of string; const AContext: string);
var
  I, J, K: Integer;
  Found: Boolean;
begin
  for I := 0 to AObject.Count - 1 do
  begin
    for J := 0 to I - 1 do
      if AObject.Names[J] = AObject.Names[I] then
        raise ETheWordClipboardV2.CreateFmt('Duplicate field "%s" in %s',
          [AObject.Names[I], AContext]);
    Found := False;
    for K := Low(AAllowed) to High(AAllowed) do
      if AObject.Names[I] = AAllowed[K] then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      raise ETheWordClipboardV2.CreateFmt('Unknown field "%s" in %s',
        [AObject.Names[I], AContext]);
  end;
end;

function RequireData(AObject: TJSONObject; const AName,
  AContext: string; AType: TJSONType): TJSONData;
begin
  Result := AObject.Find(AName);
  if not Assigned(Result) then
    raise ETheWordClipboardV2.CreateFmt('Missing field "%s" in %s',
      [AName, AContext]);
  if Result.JSONType <> AType then
    raise ETheWordClipboardV2.CreateFmt('Wrong type for "%s" in %s',
      [AName, AContext]);
end;

function RequireString(AObject: TJSONObject; const AName,
  AContext: string): string;
begin
  Result := RequireData(AObject, AName, AContext, jtString).AsString;
end;

function RequireInteger(AObject: TJSONObject; const AName,
  AContext: string): Integer;
var
  Value: Int64;
begin
  if not TryStrToInt64(RequireData(AObject, AName, AContext,
    jtNumber).AsJSON, Value) or (Value < Low(Integer)) or
    (Value > High(Integer)) then
    raise ETheWordClipboardV2.CreateFmt('Invalid integer "%s" in %s',
      [AName, AContext]);
  Result := Integer(Value);
end;

function SerializeFragment(AFragment: TTheWordClipboardFragmentV2): string;
var
  Root, Item: TJSONObject;
  Marks, Objects: TJSONArray;
  I: Integer;
begin
  SortFragment(AFragment);
  Root := TJSONObject.Create;
  try
    Root.Add('schema', THEWORD_CLIPBOARD_SCHEMA_V2);
    Root.Add('version', THEWORD_CLIPBOARD_VERSION_V2);
    Root.Add('text', UTF8Encode(AFragment.Text));
    Marks := TJSONArray.Create;
    Root.Add('marks', Marks);
    for I := 0 to AFragment.MarkCount - 1 do
    begin
      Item := TJSONObject.Create;
      Item.Add('kind', SemanticKindName(AFragment.Marks[I].Kind));
      Item.Add('startUTF16', AFragment.Marks[I].StartOffset);
      Item.Add('endUTF16', AFragment.Marks[I].EndOffset);
      Marks.Add(Item);
    end;
    Objects := TJSONArray.Create;
    Root.Add('objects', Objects);
    for I := 0 to AFragment.ObjectCount - 1 do
    begin
      Item := TJSONObject.Create;
      Item.Add('type', ObjectKindName(AFragment.Objects[I].ObjectKind));
      Item.Add('kind', SubKindName(AFragment.Objects[I].ObjectKind,
        AFragment.Objects[I].SubKind));
      Item.Add('offsetUTF16', AFragment.Objects[I].Offset);
      Item.Add('raw', UTF8Encode(AFragment.Objects[I].RawLexeme));
      Item.Add('order', AFragment.Objects[I].SourceOrder);
      Objects.Add(Item);
    end;
    Result := Root.AsJSON;
  finally
    Root.Free;
  end;
end;

function DeserializeFragment(const APayload: string):
  TTheWordClipboardFragmentV2;
var
  Data: TJSONData;
  Root, Item: TJSONObject;
  Marks, Objects: TJSONArray;
  Mark: TTheWordClipboardMarkV2;
  Obj: TTheWordClipboardObjectV2;
  I: Integer;
begin
  if (APayload = '') or (Length(APayload) > MAX_CLIPBOARD_PAYLOAD_BYTES) then
    raise ETheWordClipboardV2.Create('Clipboard payload is empty or too large');
  Data := nil;
  Result := nil;
  try
    Data := GetJSON(APayload);
    if Data.JSONType <> jtObject then
      raise ETheWordClipboardV2.Create('Clipboard payload root must be an object');
    Root := TJSONObject(Data);
    RequireFields(Root, ['schema', 'version', 'text', 'marks', 'objects'],
      'clipboard payload');
    if RequireString(Root, 'schema', 'clipboard payload') <>
       THEWORD_CLIPBOARD_SCHEMA_V2 then
      raise ETheWordClipboardV2.Create('Unknown clipboard schema');
    if RequireInteger(Root, 'version', 'clipboard payload') <>
       THEWORD_CLIPBOARD_VERSION_V2 then
      raise ETheWordClipboardV2.Create('Unsupported clipboard version');

    Result := TTheWordClipboardFragmentV2.Create;
    Result.FText := UTF8Decode(RequireString(Root, 'text', 'clipboard payload'));
    Marks := TJSONArray(RequireData(Root, 'marks', 'clipboard payload', jtArray));
    Objects := TJSONArray(RequireData(Root, 'objects', 'clipboard payload', jtArray));
    if (Marks.Count > MAX_CLIPBOARD_ITEMS) or
       (Objects.Count > MAX_CLIPBOARD_ITEMS) then
      raise ETheWordClipboardV2.Create('Clipboard payload has too many items');
    for I := 0 to Marks.Count - 1 do
    begin
      if Marks.Items[I].JSONType <> jtObject then
        raise ETheWordClipboardV2.Create('Clipboard mark must be an object');
      Item := TJSONObject(Marks.Items[I]);
      RequireFields(Item, ['kind', 'startUTF16', 'endUTF16'],
        'clipboard mark');
      Mark.Kind := SemanticKindFromName(RequireString(Item, 'kind',
        'clipboard mark'));
      Mark.StartOffset := RequireInteger(Item, 'startUTF16', 'clipboard mark');
      Mark.EndOffset := RequireInteger(Item, 'endUTF16', 'clipboard mark');
      if (Mark.StartOffset < 0) or (Mark.EndOffset <= Mark.StartOffset) or
         (Mark.EndOffset > Length(Result.Text)) or
         not IsUtf16Boundary(Result.Text, Mark.StartOffset) or
         not IsUtf16Boundary(Result.Text, Mark.EndOffset) then
        raise ETheWordClipboardV2.Create('Clipboard mark has an invalid range');
      Result.AddMark(Mark);
    end;
    for I := 0 to Objects.Count - 1 do
    begin
      if Objects.Items[I].JSONType <> jtObject then
        raise ETheWordClipboardV2.Create('Clipboard object must be an object');
      Item := TJSONObject(Objects.Items[I]);
      RequireFields(Item, ['type', 'kind', 'offsetUTF16', 'raw', 'order'],
        'clipboard object');
      Obj.ObjectKind := ObjectKindFromName(RequireString(Item, 'type',
        'clipboard object'));
      Obj.SubKind := SubKindFromName(Obj.ObjectKind, RequireString(Item,
        'kind', 'clipboard object'));
      Obj.Offset := RequireInteger(Item, 'offsetUTF16', 'clipboard object');
      Obj.RawLexeme := UTF8Decode(RequireString(Item, 'raw',
        'clipboard object'));
      Obj.SourceOrder := RequireInteger(Item, 'order', 'clipboard object');
      if (Obj.Offset < 0) or (Obj.Offset > Length(Result.Text)) or
         not IsUtf16Boundary(Result.Text, Obj.Offset) or
         (Obj.RawLexeme = '') or (Obj.SourceOrder < 0) then
        raise ETheWordClipboardV2.Create('Clipboard object is invalid');
      Result.AddObject(Obj);
    end;
    SortFragment(Result);
  except
    Result.Free;
    Result := nil;
    Data.Free;
    raise;
  end;
  Data.Free;
end;

function FindUniqueMarker(const ARaw: UnicodeString): UnicodeString;
var
  Code: Integer;
begin
  for Code := $E000 to $F8FF do
  begin
    Result := UnicodeString(WideChar(Code));
    if Pos(Result, ARaw) = 0 then
      Exit;
  end;
  raise ETheWordClipboardV2.Create('Unable to reserve an internal marker');
end;

function InjectObjectGroups(const ARaw: UnicodeString;
  AFragment: TTheWordClipboardFragmentV2; ABaseOffset: Integer;
  out AResult: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  Doc: TTheWordVerseDocument;
  GroupRaw, Marker, Temporary: UnicodeString;
  GroupOffset, I, MarkerPos: Integer;
begin
  Result := False;
  AResult := ARaw;
  Codec := TTheWordOntCodecV2.Create;
  I := 0;
  try
    while I < AFragment.ObjectCount do
    begin
      GroupOffset := AFragment.Objects[I].Offset;
      GroupRaw := '';
      while (I < AFragment.ObjectCount) and
            (AFragment.Objects[I].Offset = GroupOffset) do
      begin
        GroupRaw := GroupRaw + AFragment.Objects[I].RawLexeme;
        Inc(I);
      end;
      Doc := Codec.DecodeVerse(AResult);
      try
        Marker := FindUniqueMarker(AResult);
        Doc.ReplaceVisibleText(ABaseOffset + GroupOffset, 0, Marker);
        Temporary := Codec.EncodeVerse(Doc);
      finally
        Doc.Free;
      end;
      MarkerPos := Pos(Marker, Temporary);
      if MarkerPos = 0 then
        Exit;
      if Pos(Marker, Copy(Temporary, MarkerPos + Length(Marker), MaxInt)) > 0 then
        Exit;
      AResult := Copy(Temporary, 1, MarkerPos - 1) + GroupRaw +
        Copy(Temporary, MarkerPos + Length(Marker), MaxInt);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function FragmentMatchesDocument(AFragment: TTheWordClipboardFragmentV2;
  ADocument: TTheWordVerseDocument): Boolean;
var
  UsedMarks: TBooleanArray;
  I, J, TotalObjects: Integer;
  Mark: TTheWordSemanticMark;
  Obj: TTheWordClipboardObjectV2;
  Found: Boolean;
begin
  Result := ADocument.VisibleText = AFragment.Text;
  if not Result then Exit;
  SetLength(UsedMarks, ADocument.MarkCount);
  for I := 0 to AFragment.MarkCount - 1 do
  begin
    Found := False;
    for J := 0 to ADocument.MarkCount - 1 do
    begin
      Mark := ADocument.Marks[J];
      if not UsedMarks[J] and (Mark.Kind = AFragment.Marks[I].Kind) and
         (Mark.StartOffset = AFragment.Marks[I].StartOffset) and
         (Mark.EndOffset = AFragment.Marks[I].EndOffset) then
      begin
        UsedMarks[J] := True;
        Found := True;
        Break;
      end;
    end;
    if not Found then Exit(False);
  end;
  if ADocument.MarkCount <> AFragment.MarkCount then Exit(False);

  TotalObjects := ADocument.LexicalItemCount + ADocument.OpaqueItemCount +
    ADocument.StructuralItemCount;
  if TotalObjects <> AFragment.ObjectCount then Exit(False);
  for I := 0 to AFragment.ObjectCount - 1 do
  begin
    Obj := AFragment.Objects[I];
    Found := False;
    case Obj.ObjectKind of
      twcokLexical:
        for J := 0 to ADocument.LexicalItemCount - 1 do
          if (ADocument.LexicalItems[J].Offset = Obj.Offset) and
             (Ord(ADocument.LexicalItems[J].Kind) = Obj.SubKind) and
             (ADocument.LexicalItems[J].RawLexeme = Obj.RawLexeme) then
          begin Found := True; Break; end;
      twcokOpaque:
        for J := 0 to ADocument.OpaqueItemCount - 1 do
          if (ADocument.OpaqueItems[J].Offset = Obj.Offset) and
             (Ord(ADocument.OpaqueItems[J].Kind) = Obj.SubKind) and
             (ADocument.OpaqueItems[J].RawLexeme = Obj.RawLexeme) then
          begin Found := True; Break; end;
      twcokStructural:
        for J := 0 to ADocument.StructuralItemCount - 1 do
          if (ADocument.StructuralItems[J].Offset = Obj.Offset) and
             (Ord(ADocument.StructuralItems[J].Kind) = Obj.SubKind) and
             (ADocument.StructuralItems[J].RawLexeme = Obj.RawLexeme) then
          begin Found := True; Break; end;
    end;
    if not Found then Exit(False);
  end;
end;

function BuildFragmentRaw(AFragment: TTheWordClipboardFragmentV2;
  out ARaw: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  Doc, Check: TTheWordVerseDocument;
  InjectedRaw: UnicodeString;
  I: Integer;
begin
  Result := False;
  ARaw := '';
  if not Assigned(AFragment) then Exit;
  Codec := TTheWordOntCodecV2.Create;
  try
    Doc := Codec.DecodeVerse(AFragment.Text);
    try
      if (Doc.VisibleText <> AFragment.Text) or (Doc.MarkCount <> 0) or
         (Doc.LexicalItemCount <> 0) or (Doc.OpaqueItemCount <> 0) or
         (Doc.StructuralItemCount <> 0) then
        Exit;
      for I := 0 to AFragment.MarkCount - 1 do
        Doc.AddSemanticMark(AFragment.Marks[I].Kind,
          AFragment.Marks[I].StartOffset, AFragment.Marks[I].EndOffset);
      ARaw := Codec.EncodeVerse(Doc);
    finally
      Doc.Free;
    end;
    if not InjectObjectGroups(ARaw, AFragment, 0, InjectedRaw) then Exit;
    ARaw := InjectedRaw;
    Check := Codec.DecodeVerse(ARaw);
    try
      Result := FragmentMatchesDocument(AFragment, Check);
    finally
      Check.Free;
    end;
  except
    Result := False;
  end;
end;

procedure AddDocumentObjectsForSelection(ADocument: TTheWordVerseDocument;
  AStart, ALength: Integer; AFragment: TTheWordClipboardFragmentV2);
var
  I, Finish, WordStart: Integer;
  Obj: TTheWordClipboardObjectV2;
  TitleRanges: TTheWordTitleRangeArray;

  function IsTitleAnchor(AOrdinal: Integer): Boolean;
  var
    TitleIndex: Integer;
  begin
    for TitleIndex := 0 to High(TitleRanges) do
      if (TitleRanges[TitleIndex].OpenOrdinal = AOrdinal) or
         (TitleRanges[TitleIndex].CloseOrdinal = AOrdinal) then
        Exit(True);
    Result := False;
  end;
begin
  Finish := AStart + ALength;
  CollectTheWordTitleRanges(ADocument, TitleRanges);
  for I := 0 to ADocument.LexicalItemCount - 1 do
  begin
    WordStart := ADocument.LexicalProtectedStart(I);
    if (AStart <= WordStart) and
       (Finish >= ADocument.LexicalItems[I].Offset) then
    begin
      Obj.ObjectKind := twcokLexical;
      Obj.SubKind := Ord(ADocument.LexicalItems[I].Kind);
      Obj.Offset := ADocument.LexicalItems[I].Offset - AStart;
      Obj.RawLexeme := ADocument.LexicalItems[I].RawLexeme;
      Obj.SourceOrder := ADocument.LexicalItems[I].Ordinal;
      AFragment.AddObject(Obj);
    end;
  end;
  for I := 0 to ADocument.OpaqueItemCount - 1 do
    if not IsTitleAnchor(ADocument.OpaqueItems[I].Ordinal) and
       (ADocument.OpaqueItems[I].Offset >= AStart) and
       (ADocument.OpaqueItems[I].Offset < Finish) then
    begin
      Obj.ObjectKind := twcokOpaque;
      Obj.SubKind := Ord(ADocument.OpaqueItems[I].Kind);
      Obj.Offset := ADocument.OpaqueItems[I].Offset - AStart;
      Obj.RawLexeme := ADocument.OpaqueItems[I].RawLexeme;
      Obj.SourceOrder := ADocument.OpaqueItems[I].Ordinal;
      AFragment.AddObject(Obj);
    end;
  for I := 0 to ADocument.StructuralItemCount - 1 do
    if (ADocument.StructuralItems[I].Offset >= AStart) and
       (ADocument.StructuralItems[I].Offset < Finish) then
    begin
      Obj.ObjectKind := twcokStructural;
      Obj.SubKind := Ord(ADocument.StructuralItems[I].Kind);
      Obj.Offset := ADocument.StructuralItems[I].Offset - AStart;
      Obj.RawLexeme := ADocument.StructuralItems[I].RawLexeme;
      Obj.SourceOrder := ADocument.StructuralItems[I].Ordinal;
      AFragment.AddObject(Obj);
    end;
end;

class function TTheWordClipboardCodecV2.EncodeSelection(
  ADocument: TTheWordVerseDocument; AStartUTF16, ALengthUTF16: Integer;
  out APlainText: UnicodeString; out APayload: string): Boolean;
var
  Fragment: TTheWordClipboardFragmentV2;
  Mark: TTheWordSemanticMark;
  ClipboardMark: TTheWordClipboardMarkV2;
  Raw: UnicodeString;
  I, Finish, MarkStart, MarkEnd: Integer;
begin
  APlainText := '';
  APayload := '';
  Result := Assigned(ADocument) and (ALengthUTF16 > 0) and
    IsValidRange(ADocument.VisibleText, AStartUTF16, ALengthUTF16);
  if not Result then Exit;
  Finish := AStartUTF16 + ALengthUTF16;
  APlainText := Copy(ADocument.VisibleText, AStartUTF16 + 1, ALengthUTF16);
  Fragment := TTheWordClipboardFragmentV2.Create;
  try
    Fragment.FText := APlainText;
    for I := 0 to ADocument.MarkCount - 1 do
    begin
      Mark := ADocument.Marks[I];
      MarkStart := Max(Mark.StartOffset, AStartUTF16);
      MarkEnd := Min(Mark.EndOffset, Finish);
      if MarkEnd <= MarkStart then Continue;
      ClipboardMark.Kind := Mark.Kind;
      ClipboardMark.StartOffset := MarkStart - AStartUTF16;
      ClipboardMark.EndOffset := MarkEnd - AStartUTF16;
      Fragment.AddMark(ClipboardMark);
    end;
    AddDocumentObjectsForSelection(ADocument, AStartUTF16, ALengthUTF16,
      Fragment);
    SortFragment(Fragment);
    if not BuildFragmentRaw(Fragment, Raw) then
    begin
      Fragment.ClearObjects;
      if not BuildFragmentRaw(Fragment, Raw) then
        Exit(True);
    end;
    APayload := SerializeFragment(Fragment);
  finally
    Fragment.Free;
  end;
end;

class function TTheWordClipboardCodecV2.TryDecodePayload(
  const APayload: string; out AFragment: TTheWordClipboardFragmentV2): Boolean;
var
  Raw: UnicodeString;
begin
  AFragment := nil;
  try
    AFragment := DeserializeFragment(APayload);
    Result := BuildFragmentRaw(AFragment, Raw);
    if not Result then FreeAndNil(AFragment);
  except
    FreeAndNil(AFragment);
    Result := False;
  end;
end;

class function TTheWordClipboardCodecV2.NewPlainFragment(
  const AText: UnicodeString): TTheWordClipboardFragmentV2;
begin
  Result := TTheWordClipboardFragmentV2.Create;
  Result.FText := NormalizeTheWordClipboardText(AText);
end;

class function TTheWordClipboardCodecV2.BuildPasteRaw(
  ADocument: TTheWordVerseDocument; AStartUTF16, ALengthUTF16: Integer;
  AFragment: TTheWordClipboardFragmentV2;
  out ARaw: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  Working, Check: TTheWordVerseDocument;
  ExpectedText: UnicodeString;
  InjectedRaw: UnicodeString;
  I: Integer;
begin
  Result := False;
  ARaw := '';
  if not Assigned(ADocument) or not Assigned(AFragment) or
     not ADocument.CanReplaceVisibleText(AStartUTF16, ALengthUTF16) then
    Exit;
  Codec := TTheWordOntCodecV2.Create;
  try
    Working := Codec.DecodeVerse(Codec.EncodeVerse(ADocument));
    try
      Working.ReplaceVisibleText(AStartUTF16, ALengthUTF16, AFragment.Text);
      for I := 0 to AFragment.MarkCount - 1 do
        Working.AddSemanticMark(AFragment.Marks[I].Kind,
          AStartUTF16 + AFragment.Marks[I].StartOffset,
          AStartUTF16 + AFragment.Marks[I].EndOffset);
      ARaw := Codec.EncodeVerse(Working);
    finally
      Working.Free;
    end;
    if not InjectObjectGroups(ARaw, AFragment, AStartUTF16,
      InjectedRaw) then Exit;
    ARaw := InjectedRaw;
    ExpectedText := Copy(ADocument.VisibleText, 1, AStartUTF16) +
      AFragment.Text + Copy(ADocument.VisibleText,
      AStartUTF16 + ALengthUTF16 + 1, MaxInt);
    Check := Codec.DecodeVerse(ARaw);
    try
      Result := Check.VisibleText = ExpectedText;
    finally
      Check.Free;
    end;
  except
    Result := False;
  end;
end;

function LexicalTouchesRange(ADocument: TTheWordVerseDocument;
  AIndex, AStart, AFinish: Integer): Boolean;
var
  Anchor, WordStart: Integer;
begin
  Anchor := ADocument.LexicalItems[AIndex].Offset;
  WordStart := ADocument.LexicalProtectedStart(AIndex);
  Result := ((AStart < Anchor) and (AFinish > WordStart)) or
    ((Anchor = WordStart) and (Anchor >= AStart) and (Anchor < AFinish));
end;

function ShiftObjectOffset(AOffset, AStart, AFinish: Integer): Integer;
begin
  if AOffset >= AFinish then
    Result := AOffset - (AFinish - AStart)
  else
    Result := AOffset;
end;

class function TTheWordClipboardCodecV2.BuildCutRaw(
  ADocument: TTheWordVerseDocument; AStartUTF16, ALengthUTF16: Integer;
  out ARaw: UnicodeString): Boolean;
var
  Fragment: TTheWordClipboardFragmentV2;
  Mark: TTheWordSemanticMark;
  ClipboardMark: TTheWordClipboardMarkV2;
  Obj: TTheWordClipboardObjectV2;
  Finish, I, RangeIndex, WordStart: Integer;
  TitleRanges: TTheWordTitleRangeArray;

  function IsTitleAnchor(AOrdinal: Integer): Boolean;
  var
    TitleIndex: Integer;
  begin
    for TitleIndex := 0 to High(TitleRanges) do
      if (TitleRanges[TitleIndex].OpenOrdinal = AOrdinal) or
         (TitleRanges[TitleIndex].CloseOrdinal = AOrdinal) then
        Exit(True);
    Result := False;
  end;
begin
  Result := False;
  ARaw := '';
  if not Assigned(ADocument) or (ALengthUTF16 <= 0) or
     not IsValidRange(ADocument.VisibleText, AStartUTF16, ALengthUTF16) then
    Exit;
  Finish := AStartUTF16 + ALengthUTF16;
  CollectTheWordTitleRanges(ADocument, TitleRanges);
  for RangeIndex := 0 to High(TitleRanges) do
    if (AStartUTF16 < TitleRanges[RangeIndex].EndOffset) and
       (Finish > TitleRanges[RangeIndex].StartOffset) then
      Exit;

  { A lexical object may be cut only with its complete protected word. }
  for I := 0 to ADocument.LexicalItemCount - 1 do
    if LexicalTouchesRange(ADocument, I, AStartUTF16, Finish) then
    begin
      WordStart := ADocument.LexicalProtectedStart(I);
      if (AStartUTF16 > WordStart) or
         (Finish < ADocument.LexicalItems[I].Offset) then
        Exit;
    end;

  Fragment := TTheWordClipboardFragmentV2.Create;
  try
    Fragment.FText := Copy(ADocument.VisibleText, 1, AStartUTF16) +
      Copy(ADocument.VisibleText, Finish + 1, MaxInt);
    for I := 0 to ADocument.MarkCount - 1 do
    begin
      Mark := ADocument.Marks[I];
      ClipboardMark.Kind := Mark.Kind;
      ClipboardMark.StartOffset := Mark.StartOffset;
      ClipboardMark.EndOffset := Mark.EndOffset;
      if Mark.EndOffset <= AStartUTF16 then
        { unchanged }
      else if Mark.StartOffset >= Finish then
      begin
        Dec(ClipboardMark.StartOffset, ALengthUTF16);
        Dec(ClipboardMark.EndOffset, ALengthUTF16);
      end
      else
      begin
        if ClipboardMark.StartOffset >= AStartUTF16 then
          ClipboardMark.StartOffset := AStartUTF16;
        if ClipboardMark.EndOffset <= Finish then
          ClipboardMark.EndOffset := AStartUTF16
        else
          Dec(ClipboardMark.EndOffset, ALengthUTF16);
      end;
      if ClipboardMark.EndOffset > ClipboardMark.StartOffset then
        Fragment.AddMark(ClipboardMark);
    end;

    for I := 0 to ADocument.LexicalItemCount - 1 do
      if not LexicalTouchesRange(ADocument, I, AStartUTF16, Finish) then
      begin
        Obj.ObjectKind := twcokLexical;
        Obj.SubKind := Ord(ADocument.LexicalItems[I].Kind);
        Obj.Offset := ShiftObjectOffset(ADocument.LexicalItems[I].Offset,
          AStartUTF16, Finish);
        Obj.RawLexeme := ADocument.LexicalItems[I].RawLexeme;
        Obj.SourceOrder := ADocument.LexicalItems[I].Ordinal;
        Fragment.AddObject(Obj);
    end;
    for I := 0 to ADocument.OpaqueItemCount - 1 do
      if IsTitleAnchor(ADocument.OpaqueItems[I].Ordinal) or
         not ((ADocument.OpaqueItems[I].Offset >= AStartUTF16) and
              (ADocument.OpaqueItems[I].Offset < Finish)) then
      begin
        Obj.ObjectKind := twcokOpaque;
        Obj.SubKind := Ord(ADocument.OpaqueItems[I].Kind);
        Obj.Offset := ShiftObjectOffset(ADocument.OpaqueItems[I].Offset,
          AStartUTF16, Finish);
        Obj.RawLexeme := ADocument.OpaqueItems[I].RawLexeme;
        Obj.SourceOrder := ADocument.OpaqueItems[I].Ordinal;
        Fragment.AddObject(Obj);
      end;
    for I := 0 to ADocument.StructuralItemCount - 1 do
      if not ((ADocument.StructuralItems[I].Offset >= AStartUTF16) and
              (ADocument.StructuralItems[I].Offset < Finish)) then
      begin
        Obj.ObjectKind := twcokStructural;
        Obj.SubKind := Ord(ADocument.StructuralItems[I].Kind);
        Obj.Offset := ShiftObjectOffset(ADocument.StructuralItems[I].Offset,
          AStartUTF16, Finish);
        Obj.RawLexeme := ADocument.StructuralItems[I].RawLexeme;
        Obj.SourceOrder := ADocument.StructuralItems[I].Ordinal;
        Fragment.AddObject(Obj);
      end;
    SortFragment(Fragment);
    Result := BuildFragmentRaw(Fragment, ARaw);
  finally
    Fragment.Free;
  end;
end;

end.
