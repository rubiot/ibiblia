unit bible_document_reader;

{ Reads plain documents (.txt, .rtf, .odt, .docx) as a list of paragraphs
  (UTF-8). Digits written in superscript or subscript style are returned as
  Unicode superscript / subscript digits, so a verse number like a raised
  "12" survives as "¹²" and can be recognised later. This unit has no
  user-interface code. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ True for the extensions this unit reads. }
function IsPlainBibleDocument(const FileName: string): Boolean;
{ Fills Paragraphs with the document's paragraphs. False + ErrorMessage on
  failure. }
function ReadBibleDocument(const FileName: string; Paragraphs: TStrings;
  out ErrorMessage: string): Boolean;

resourcestring
  SDocumentReadError = 'Could not read the document: %s';
  SDocumentEmpty = 'The document has no text.';

implementation

uses
  LazUTF8, LConvEncoding, Zipper, DOM, XMLRead, StrUtils, Math;

const
  SUPER_DIGITS: array['0'..'9'] of string = (
    #$E2#$81#$B0, #$C2#$B9, #$C2#$B2, #$C2#$B3, #$E2#$81#$B4,
    #$E2#$81#$B5, #$E2#$81#$B6, #$E2#$81#$B7, #$E2#$81#$B8, #$E2#$81#$B9);
  SUB_DIGITS: array['0'..'9'] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84,
    #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89);

type
  TRunPosition = (rpNormal, rpSuper, rpSub);

{ Turns the ASCII digits of a raised / lowered run into Unicode digits. }
function StyleDigits(const S: string; Position: TRunPosition): string;
var
  I: Integer;
begin
  if Position = rpNormal then
    Exit(S);
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then
    begin
      if Position = rpSuper then
        Result := Result + SUPER_DIGITS[S[I]]
      else
        Result := Result + SUB_DIGITS[S[I]];
    end
    else
      Result := Result + S[I];
end;

function IsPlainBibleDocument(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.txt') or (Ext = '.rtf') or (Ext = '.odt') or
    (Ext = '.docx');
end;

procedure AddParagraph(Paragraphs: TStrings; const Text: string);
var
  S: string;
begin
  S := Trim(StringReplace(Text, #9, ' ', [rfReplaceAll]));
  while Pos('  ', S) > 0 do
    S := StringReplace(S, '  ', ' ', [rfReplaceAll]);
  if S <> '' then
    Paragraphs.Add(S);
end;

{ ---------- .txt ---------- }

function ReadTextFile(const FileName: string; Paragraphs: TStrings): Boolean;
var
  Stream: TFileStream;
  Raw: RawByteString;
  U: UnicodeString;
  Lines: TStringList;
  I: Integer;
  Text: string;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Raw, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Raw[1], Stream.Size);
  finally
    Stream.Free;
  end;

  if (Length(Raw) >= 2) and (Raw[1] = #$FF) and (Raw[2] = #$FE) then
  begin
    SetLength(U, (Length(Raw) - 2) div 2);
    if Length(U) > 0 then
      Move(Raw[3], U[1], Length(U) * 2);
    Text := UTF8Encode(U);
  end
  else if (Length(Raw) >= 2) and (Raw[1] = #$FE) and (Raw[2] = #$FF) then
  begin
    SetLength(U, (Length(Raw) - 2) div 2);
    for I := 1 to Length(U) do
      U[I] := WideChar((Ord(Raw[1 + I * 2]) shl 8) or Ord(Raw[2 + I * 2]));
    Text := UTF8Encode(U);
  end
  else
  begin
    if (Length(Raw) >= 3) and (Raw[1] = #$EF) and (Raw[2] = #$BB) and
      (Raw[3] = #$BF) then
      Delete(Raw, 1, 3);
    Text := Raw;
    if FindInvalidUTF8Codepoint(PChar(Text), Length(Text)) >= 0 then
      Text := ConvertEncoding(Raw, GuessEncoding(Raw), EncodingUTF8);
  end;

  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    for I := 0 to Lines.Count - 1 do
      AddParagraph(Paragraphs, Lines[I]);
  finally
    Lines.Free;
  end;
  Result := True;
end;

{ ---------- .rtf ---------- }

type
  TRtfState = record
    Skip: Boolean;
    Position: TRunPosition;
    UcSkip: Integer;
  end;

function ReadRtfFile(const FileName: string; Paragraphs: TStrings): Boolean;
var
  Raw: RawByteString;
  Stream: TFileStream;
  I, N, J, Param, PendingSkip: Integer;
  C: Char;
  Word, NumText: string;
  HasParam, Negative: Boolean;
  State: TRtfState;
  Stack: array of TRtfState;
  Current, Run: string;
  CodePage: string;
  AnsiBytes: RawByteString;

  procedure FlushAnsi;
  begin
    if AnsiBytes = '' then
      Exit;
    Run := Run + ConvertEncoding(AnsiBytes, CodePage, EncodingUTF8);
    AnsiBytes := '';
  end;

  procedure FlushRun;
  begin
    FlushAnsi;
    if Run <> '' then
      Current := Current + StyleDigits(Run, State.Position);
    Run := '';
  end;

  procedure EmitText(const S: string);
  begin
    if State.Skip then
      Exit;
    FlushAnsi;
    Run := Run + S;
  end;

  procedure EndParagraph;
  begin
    FlushRun;
    AddParagraph(Paragraphs, Current);
    Current := '';
  end;

  procedure SetPosition(P: TRunPosition);
  begin
    if State.Position <> P then
    begin
      FlushRun;
      State.Position := P;
    end;
  end;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Raw, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Raw[1], Stream.Size);
  finally
    Stream.Free;
  end;

  CodePage := 'cp1252';
  State.Skip := False;
  State.Position := rpNormal;
  State.UcSkip := 1;
  Stack := nil;
  Current := '';
  Run := '';
  AnsiBytes := '';
  PendingSkip := 0;
  N := Length(Raw);
  I := 1;
  while I <= N do
  begin
    C := Raw[I];
    if PendingSkip > 0 then
    begin
      { characters after \uN that stand in for it }
      if C = '\' then
      begin
        if (I < N) and (Raw[I + 1] = '''') then
          Inc(I, 4)
        else
        begin
          Dec(PendingSkip);
          Continue;
        end;
      end
      else if (C = '{') or (C = '}') then
      begin
        PendingSkip := 0;
        Continue;
      end
      else
        Inc(I);
      Dec(PendingSkip);
      Continue;
    end;

    case C of
      '{':
        begin
          SetLength(Stack, Length(Stack) + 1);
          Stack[High(Stack)] := State;
          Inc(I);
          if (I + 1 <= N) and (Raw[I] = '\') and (Raw[I + 1] = '*') then
          begin
            FlushRun;
            State.Skip := True;
          end;
        end;
      '}':
        begin
          FlushRun;
          if Length(Stack) > 0 then
          begin
            State := Stack[High(Stack)];
            SetLength(Stack, Length(Stack) - 1);
          end;
          Inc(I);
        end;
      '\':
        begin
          Inc(I);
          if I > N then
            Break;
          C := Raw[I];
          if C = '''' then
          begin
            if not State.Skip then
            begin
              FlushRun;
              AnsiBytes := AnsiBytes + Chr(StrToIntDef('$' + Copy(Raw, I + 1, 2), 32));
            end;
            Inc(I, 3);
            Continue;
          end;
          if C in ['\', '{', '}'] then
          begin
            EmitText(C);
            Inc(I);
            Continue;
          end;
          if C = '~' then
          begin
            EmitText(' ');
            Inc(I);
            Continue;
          end;
          if C in ['-', '_'] then
          begin
            Inc(I);
            Continue;
          end;
          if not (C in ['a'..'z', 'A'..'Z']) then
          begin
            Inc(I);
            Continue;
          end;
          J := I;
          while (J <= N) and (Raw[J] in ['a'..'z', 'A'..'Z']) do
            Inc(J);
          Word := Copy(Raw, I, J - I);
          I := J;
          Negative := (I <= N) and (Raw[I] = '-');
          if Negative then
            Inc(I);
          J := I;
          while (J <= N) and (Raw[J] in ['0'..'9']) do
            Inc(J);
          NumText := Copy(Raw, I, J - I);
          HasParam := NumText <> '';
          Param := StrToIntDef(NumText, 0);
          if Negative then
            Param := -Param;
          I := J;
          if (I <= N) and (Raw[I] = ' ') then
            Inc(I);

          if (Word = 'fonttbl') or (Word = 'colortbl') or
             (Word = 'stylesheet') or (Word = 'info') or (Word = 'pict') or
             (Word = 'header') or (Word = 'footer') or (Word = 'headerl') or
             (Word = 'headerr') or (Word = 'footerl') or (Word = 'footerr') or
             (Word = 'footnote') or (Word = 'object') or
             (Word = 'listtable') or (Word = 'listoverridetable') or
             (Word = 'rsidtbl') or (Word = 'generator') or
             (Word = 'xmlnstbl') or (Word = 'themedata') or
             (Word = 'colorschememapping') or (Word = 'latentstyles') or
             (Word = 'datastore') or (Word = 'pgdsctbl') then
          begin
            FlushRun;
            State.Skip := True;
          end
          else if Word = 'ansicpg' then
            CodePage := 'cp' + IntToStr(Param)
          else if (Word = 'par') or (Word = 'sect') or (Word = 'page') then
          begin
            if not State.Skip then
              EndParagraph;
          end
          else if (Word = 'line') or (Word = 'tab') or (Word = 'emspace') or
            (Word = 'enspace') then
            EmitText(' ')
          else if Word = 'super' then
            SetPosition(rpSuper)
          else if Word = 'sub' then
            SetPosition(rpSub)
          else if Word = 'nosupersub' then
            SetPosition(rpNormal)
          else if (Word = 'up') then
          begin
            if Param > 0 then SetPosition(rpSuper) else SetPosition(rpNormal);
          end
          else if (Word = 'dn') then
          begin
            if Param > 0 then SetPosition(rpSub) else SetPosition(rpNormal);
          end
          else if Word = 'plain' then
            SetPosition(rpNormal)
          else if Word = 'uc' then
            State.UcSkip := Param
          else if (Word = 'u') and HasParam then
          begin
            if Param < 0 then
              Inc(Param, 65536);
            EmitText(UTF8Encode(UnicodeString(WideChar(Param))));
            PendingSkip := State.UcSkip;
          end
          else if Word = 'ldblquote' then EmitText(#$E2#$80#$9C)
          else if Word = 'rdblquote' then EmitText(#$E2#$80#$9D)
          else if Word = 'lquote' then EmitText(#$E2#$80#$98)
          else if Word = 'rquote' then EmitText(#$E2#$80#$99)
          else if Word = 'emdash' then EmitText(#$E2#$80#$94)
          else if Word = 'endash' then EmitText(#$E2#$80#$93)
          else if Word = 'bullet' then EmitText(#$E2#$80#$A2);
        end;
      #13, #10:
        Inc(I);
    else
      begin
        if not State.Skip then
        begin
          if Ord(C) >= 128 then
            AnsiBytes := AnsiBytes + C
          else
            EmitText(C);
        end;
        Inc(I);
      end;
    end;
  end;
  EndParagraph;
  Result := True;
end;

{ ---------- zip helpers ---------- }

function ExtractZipEntry(const ZipFile, EntryName, TargetFolder: string): string;
var
  UnZipper: TUnZipper;
  Entries: TStringList;
begin
  Result := '';
  UnZipper := TUnZipper.Create;
  Entries := TStringList.Create;
  try
    UnZipper.FileName := ZipFile;
    UnZipper.OutputPath := TargetFolder;
    Entries.Add(EntryName);
    UnZipper.UnZipFiles(Entries);
    Result := IncludeTrailingPathDelimiter(TargetFolder) +
      StringReplace(EntryName, '/', PathDelim, [rfReplaceAll]);
    if not FileExists(Result) then
      Result := '';
  finally
    Entries.Free;
    UnZipper.Free;
  end;
end;

function TempFolderFor(const FileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'iBiblia' +
    PathDelim + 'docread' + PathDelim +
    IntToHex(GetTickCount64, 8) + '_' + ChangeFileExt(ExtractFileName(FileName), '') +
    PathDelim;
  ForceDirectories(Result);
end;

procedure RemoveFolder(const Folder: string);
var
  Info: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Folder) + '*', faAnyFile, Info) = 0 then
  try
    repeat
      if (Info.Name = '.') or (Info.Name = '..') then
        Continue;
      if (Info.Attr and faDirectory) <> 0 then
        RemoveFolder(IncludeTrailingPathDelimiter(Folder) + Info.Name)
      else
        DeleteFile(IncludeTrailingPathDelimiter(Folder) + Info.Name);
    until FindNext(Info) <> 0;
  finally
    FindClose(Info);
  end;
  RemoveDir(Folder);
end;

function LocalName(Node: TDOMNode): string;
var
  P: Integer;
begin
  Result := UTF8Encode(Node.NodeName);
  P := Pos(':', Result);
  if P > 0 then
    Delete(Result, 1, P);
end;

function AttrValue(Node: TDOMNode; const Name: string): string;
var
  I: Integer;
  Attrs: TDOMNamedNodeMap;
begin
  Result := '';
  Attrs := Node.Attributes;
  if not Assigned(Attrs) then
    Exit;
  for I := 0 to Attrs.Length - 1 do
    if UTF8Encode(Attrs.Item[I].NodeName) = Name then
      Exit(UTF8Encode(Attrs.Item[I].NodeValue));
end;

{ ---------- .docx ---------- }

function ReadDocxFile(const FileName: string; Paragraphs: TStrings): Boolean;
var
  Folder, XmlFile: string;
  Doc: TXMLDocument;
  Current: string;

  function RunPosition(RunNode: TDOMNode): TRunPosition;
  var
    Props, Child: TDOMNode;
    Value: string;
  begin
    Result := rpNormal;
    Props := RunNode.FirstChild;
    while Assigned(Props) and (LocalName(Props) <> 'rPr') do
      Props := Props.NextSibling;
    if not Assigned(Props) then
      Exit;
    Child := Props.FirstChild;
    while Assigned(Child) do
    begin
      if LocalName(Child) = 'vertAlign' then
      begin
        Value := AttrValue(Child, 'w:val');
        if Value = 'superscript' then
          Exit(rpSuper)
        else if Value = 'subscript' then
          Exit(rpSub);
      end;
      Child := Child.NextSibling;
    end;
  end;

  procedure WalkRun(RunNode: TDOMNode);
  var
    Child: TDOMNode;
    Name, Text: string;
    Position: TRunPosition;
  begin
    Position := RunPosition(RunNode);
    Text := '';
    Child := RunNode.FirstChild;
    while Assigned(Child) do
    begin
      Name := LocalName(Child);
      if (Name = 't') then
        Text := Text + UTF8Encode(Child.TextContent)
      else if (Name = 'tab') or (Name = 'br') or (Name = 'cr') then
        Text := Text + ' ';
      Child := Child.NextSibling;
    end;
    Current := Current + StyleDigits(Text, Position);
  end;

  procedure Walk(Node: TDOMNode);
  var
    Child: TDOMNode;
    Name: string;
  begin
    Child := Node.FirstChild;
    while Assigned(Child) do
    begin
      Name := LocalName(Child);
      if Name = 'p' then
      begin
        Current := '';
        Walk(Child);
        AddParagraph(Paragraphs, Current);
        Current := '';
      end
      else if Name = 'r' then
        WalkRun(Child)
      else if (Name = 'footnoteReference') or (Name = 'endnoteReference') or
        (Name = 'pPr') or (Name = 'rPr') or (Name = 'sectPr') or
        (Name = 'instrText') then
        { nothing }
      else if Child.HasChildNodes then
        Walk(Child);
      Child := Child.NextSibling;
    end;
  end;

begin
  Result := False;
  Folder := TempFolderFor(FileName);
  try
    XmlFile := ExtractZipEntry(FileName, 'word/document.xml', Folder);
    if XmlFile = '' then
      Exit;
    ReadXMLFile(Doc, XmlFile);
    try
      Current := '';
      Walk(Doc.DocumentElement);
    finally
      Doc.Free;
    end;
    Result := True;
  finally
    RemoveFolder(Folder);
  end;
end;

{ ---------- .odt ---------- }

function ReadOdtFile(const FileName: string; Paragraphs: TStrings): Boolean;
var
  Folder, ContentFile, StylesFile: string;
  Doc, StylesDoc: TXMLDocument;
  RaisedStyles, LoweredStyles: TStringList;
  Current: string;

  procedure CollectStyles(Root: TDOMNode);
  var
    Child, Props: TDOMNode;
    StyleName, Position: string;
  begin
    if not Assigned(Root) then
      Exit;
    Child := Root.FirstChild;
    while Assigned(Child) do
    begin
      if LocalName(Child) = 'style' then
      begin
        StyleName := AttrValue(Child, 'style:name');
        Props := Child.FirstChild;
        while Assigned(Props) do
        begin
          if LocalName(Props) = 'text-properties' then
          begin
            Position := LowerCase(Trim(AttrValue(Props, 'style:text-position')));
            if (Position <> '') and (StyleName <> '') then
            begin
              if AnsiStartsStr('super', Position) or
                 ((Position[1] in ['1'..'9']) and (Position <> '0%')) then
                RaisedStyles.Add(StyleName)
              else if AnsiStartsStr('sub', Position) or (Position[1] = '-') then
                LoweredStyles.Add(StyleName);
            end;
          end;
          Props := Props.NextSibling;
        end;
      end
      else if Child.HasChildNodes then
        CollectStyles(Child);
      Child := Child.NextSibling;
    end;
  end;

  function StylePosition(Node: TDOMNode; AInherited: TRunPosition): TRunPosition;
  var
    StyleName: string;
  begin
    Result := AInherited;
    StyleName := AttrValue(Node, 'text:style-name');
    if StyleName = '' then
      Exit;
    if RaisedStyles.IndexOf(StyleName) >= 0 then
      Result := rpSuper
    else if LoweredStyles.IndexOf(StyleName) >= 0 then
      Result := rpSub;
  end;

  procedure WalkInline(Node: TDOMNode; Position: TRunPosition);
  var
    Child: TDOMNode;
    Name: string;
  begin
    Child := Node.FirstChild;
    while Assigned(Child) do
    begin
      if Child.NodeType = TEXT_NODE then
        Current := Current + StyleDigits(UTF8Encode(Child.NodeValue), Position)
      else
      begin
        Name := LocalName(Child);
        if Name = 's' then
          Current := Current + StringOfChar(' ',
            Max(1, StrToIntDef(AttrValue(Child, 'text:c'), 1)))
        else if (Name = 'tab') or (Name = 'line-break') then
          Current := Current + ' '
        else if (Name = 'note') or (Name = 'annotation') or
          (Name = 'bookmark') or (Name = 'soft-page-break') then
          { skip }
        else if Name = 'span' then
          WalkInline(Child, StylePosition(Child, Position))
        else
          WalkInline(Child, Position);
      end;
      Child := Child.NextSibling;
    end;
  end;

  procedure WalkBlocks(Node: TDOMNode);
  var
    Child: TDOMNode;
    Name: string;
  begin
    Child := Node.FirstChild;
    while Assigned(Child) do
    begin
      Name := LocalName(Child);
      if (Name = 'p') or (Name = 'h') then
      begin
        Current := '';
        WalkInline(Child, StylePosition(Child, rpNormal));
        AddParagraph(Paragraphs, Current);
        Current := '';
      end
      else if (Name = 'automatic-styles') or (Name = 'font-face-decls') or
        (Name = 'scripts') or (Name = 'tracked-changes') then
        { skip }
      else if Child.HasChildNodes then
        WalkBlocks(Child);
      Child := Child.NextSibling;
    end;
  end;

begin
  Result := False;
  Folder := TempFolderFor(FileName);
  RaisedStyles := TStringList.Create;
  LoweredStyles := TStringList.Create;
  RaisedStyles.Sorted := True;
  LoweredStyles.Sorted := True;
  RaisedStyles.Duplicates := dupIgnore;
  LoweredStyles.Duplicates := dupIgnore;
  try
    ContentFile := ExtractZipEntry(FileName, 'content.xml', Folder);
    if ContentFile = '' then
      Exit;
    StylesFile := ExtractZipEntry(FileName, 'styles.xml', Folder);
    if StylesFile <> '' then
    begin
      ReadXMLFile(StylesDoc, StylesFile);
      try
        CollectStyles(StylesDoc.DocumentElement);
      finally
        StylesDoc.Free;
      end;
    end;
    ReadXMLFile(Doc, ContentFile);
    try
      CollectStyles(Doc.DocumentElement);
      Current := '';
      WalkBlocks(Doc.DocumentElement);
    finally
      Doc.Free;
    end;
    Result := True;
  finally
    RaisedStyles.Free;
    LoweredStyles.Free;
    RemoveFolder(Folder);
  end;
end;

function ReadBibleDocument(const FileName: string; Paragraphs: TStrings;
  out ErrorMessage: string): Boolean;
var
  Ext: string;
begin
  ErrorMessage := '';
  Paragraphs.Clear;
  Ext := LowerCase(ExtractFileExt(FileName));
  try
    if Ext = '.txt' then
      Result := ReadTextFile(FileName, Paragraphs)
    else if Ext = '.rtf' then
      Result := ReadRtfFile(FileName, Paragraphs)
    else if Ext = '.docx' then
      Result := ReadDocxFile(FileName, Paragraphs)
    else if Ext = '.odt' then
      Result := ReadOdtFile(FileName, Paragraphs)
    else
      Result := False;
    if not Result then
      ErrorMessage := Format(SDocumentReadError, [ExtractFileName(FileName)]);
  except
    on E: Exception do
    begin
      Result := False;
      ErrorMessage := Format(SDocumentReadError, [E.Message]);
    end;
  end;
  if Result and (Paragraphs.Count = 0) then
  begin
    Result := False;
    ErrorMessage := SDocumentEmpty;
  end;
end;

end.
