unit sword_module;

{ SWORD Project (CrossWire) modules for iBiblia: Bible texts (zText, zText4,
  RawText, RawText4) and dictionaries / lexicons (zLD, RawLD, RawLD4).

  A module is a .conf file in a "mods.d" folder next to a "modules" folder
  (a downloaded .zip holds the same). The Bible is converted, once, to the
  theWord verse lines iBiblia already imports (Strong's numbers and
  morphology become <WG...>/<WT...> tags, notes <RF>, words of Jesus <FR>,
  words added by the translators <FI>). A dictionary is converted to a theWord
  .dct.twm that the popup dictionaries use. Locked modules (CipherKey) are
  refused. Verse counts of the SWORD numbering systems: sword_canons.pas.
  zText / zLD blocks are zlib compressed (the usual CompressType); bzip2 and
  xz blocks are unpacked with the bzip2 / xz program when it is installed. }

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, esword_usfm;

{ True for a SWORD .conf file that describes a Bible. }
function IsSwordBibleConf(const FileName: string): Boolean;
{ True for a SWORD .conf file that describes a dictionary / lexicon. }
function IsSwordDictionaryConf(const FileName: string): Boolean;
{ Bible, dictionary, commentary, book, or '' when the file is no SWORD .conf;
  Reason says why a module cannot be opened (locked, unsupported). }
function SwordModuleKind(const ConfFile: string; out Reason: string): string;

function LoadSwordBible(const ConfFile: string; Modulo: TStringList;
  out ExtraVerses: TESwordExtraVerseArray; out ApocryphaBooksInfo: string;
  out HasOT, HasNT: Boolean; out ErrorMessage: string): Boolean;

{ Writes the dictionary as a theWord .dct.twm to OutFile. }
function ConvertSwordDictionary(const ConfFile, OutFile: string;
  out ErrorMessage: string): Boolean;
{ Folder for converted dictionaries and the file name used for one. }
function SwordDictionaryTarget(const ConfFile: string): string;

resourcestring
  SSwordLocked = 'This SWORD module is locked (CipherKey). iBiblia does not open locked modules.';
  SSwordUnsupported = 'This kind of SWORD module (%s) is not supported.';
  SSwordNoDriver = 'This .conf names no module type (ModDrv).';
  SSwordFilesMissing = 'The data files of this SWORD module are missing (looked in %s).';
  SSwordNoVerses = 'No Bible verses were found in this SWORD module.';
  SSwordNeedTool = 'This module is compressed with %s. Install the "%s" program and try again.';
  SSwordNotBible = 'This SWORD module is not a Bible.';
  SSwordNotDictionary = 'This SWORD module is not a dictionary.';
  SSwordNoEntries = 'No dictionary entries were found in this SWORD module.';

implementation

uses
  StrUtils, zstream, Process, LazUTF8, LConvEncoding, sqlite3conn, sqldb,
  sword_canons;

const
  ONT_LINES = 31102;
  OT_LINES = 23145;

type
  TIntArray = array of Integer;

  { TSwordConf }

  TSwordConf = class
  public
    Name: string;
    Keys, Vals: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Load(const FileName: string): Boolean;
    function Get(const Key: string; const Default: string = ''): string;
    function Has(const Key: string): Boolean;
    function Joined(const Key: string): string;
  end;

constructor TSwordConf.Create;
begin
  Keys := TStringList.Create;
  Vals := TStringList.Create;
end;

destructor TSwordConf.Destroy;
begin
  Keys.Free;
  Vals.Free;
  inherited Destroy;
end;

function TSwordConf.Load(const FileName: string): Boolean;
var
  Lines: TStringList;
  I, P: Integer;
  Ln, K, V: string;
begin
  Result := False;
  Name := '';
  Lines := TStringList.Create;
  try
    try
      Lines.LoadFromFile(FileName);
    except
      Exit;
    end;
    I := 0;
    while I < Lines.Count do
    begin
      Ln := Lines[I];
      if (I = 0) and (Copy(Ln, 1, 3) = #$EF#$BB#$BF) then
        Delete(Ln, 1, 3);
      Inc(I);
      Ln := TrimRight(Ln);
      if Name = '' then
      begin
        P := Pos('[', Ln);
        if (P > 0) and (Pos(']', Ln) > P) and (Trim(Copy(Ln, 1, P - 1)) = '') then
          Name := Trim(Copy(Ln, P + 1, Pos(']', Ln) - P - 1));
        Continue;
      end;
      if (Trim(Ln) = '') or (Trim(Ln)[1] in ['#', ';']) then
        Continue;
      while (Ln <> '') and (Ln[Length(Ln)] = '\') and (I < Lines.Count) do
      begin
        Ln := Copy(Ln, 1, Length(Ln) - 1) + #10 + TrimRight(Lines[I]);
        Inc(I);
      end;
      P := Pos('=', Ln);
      if P < 2 then
        Continue;
      K := Trim(Copy(Ln, 1, P - 1));
      V := Trim(Copy(Ln, P + 1, MaxInt));
      Keys.Add(K);
      Vals.Add(V);
    end;
    Result := Name <> '';
  finally
    Lines.Free;
  end;
end;

function TSwordConf.Get(const Key: string; const Default: string): string;
var
  I: Integer;
begin
  for I := 0 to Keys.Count - 1 do
    if SameText(Keys[I], Key) then
      Exit(Vals[I]);
  Result := Default;
end;

function TSwordConf.Has(const Key: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to Keys.Count - 1 do
    if SameText(Keys[I], Key) then
      Exit(True);
  Result := False;
end;

function TSwordConf.Joined(const Key: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Keys.Count - 1 do
    if SameText(Keys[I], Key) then
      Result := Result + ' ' + Vals[I];
end;

{ ---------- files ---------- }

function FindFileCI(const Dir, Name: string): string;
var
  SR: TSearchRec;
  D: string;
begin
  D := IncludeTrailingPathDelimiter(Dir);
  if FileExists(D + Name) then
    Exit(D + Name);
  Result := '';
  if FindFirst(D + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if SameText(SR.Name, Name) then
        Exit(D + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

function ReadWholeFile(const FileName: string): TBytes;
var
  F: TFileStream;
begin
  Result := nil;
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, F.Size);
    if F.Size > 0 then
      F.ReadBuffer(Result[0], F.Size);
  finally
    F.Free;
  end;
end;

function ReadPart(F: TStream; Pos, Len: Int64): TBytes;
begin
  Result := nil;
  if (Pos < 0) or (Len < 0) or (Pos + Len > F.Size) then
    Exit;
  SetLength(Result, Len);
  if Len > 0 then
  begin
    F.Position := Pos;
    F.ReadBuffer(Result[0], Len);
  end;
end;

function U16(const B: TBytes; Off: Integer): Cardinal;
begin
  if (Off < 0) or (Off + 2 > Length(B)) then
    Exit(0);
  Result := B[Off] or (B[Off + 1] shl 8);
end;

function U32(const B: TBytes; Off: Integer): Cardinal;
begin
  if (Off < 0) or (Off + 4 > Length(B)) then
    Exit(0);
  Result := Cardinal(B[Off]) or (Cardinal(B[Off + 1]) shl 8) or
    (Cardinal(B[Off + 2]) shl 16) or (Cardinal(B[Off + 3]) shl 24);
end;

{ ---------- decompression ---------- }

function InflateBlock(const Raw: TBytes): TBytes;
var
  Src, Dst: TMemoryStream;
  Z: TDecompressionStream;
  Buf: array[0..65535] of Byte;
  N: Integer;
begin
  Result := nil;
  if Length(Raw) = 0 then
    Exit;
  Src := TMemoryStream.Create;
  Dst := TMemoryStream.Create;
  try
    Src.WriteBuffer(Raw[0], Length(Raw));
    Src.Position := 0;
    Z := TDecompressionStream.Create(Src);
    try
      try
        repeat
          N := Z.Read(Buf, SizeOf(Buf));
          if N > 0 then
            Dst.WriteBuffer(Buf, N);
        until N <= 0;
      except
        // a damaged block keeps what was unpacked
      end;
    finally
      Z.Free;
    end;
    SetLength(Result, Dst.Size);
    if Dst.Size > 0 then
    begin
      Dst.Position := 0;
      Dst.ReadBuffer(Result[0], Dst.Size);
    end;
  finally
    Dst.Free;
    Src.Free;
  end;
end;

function FindProgram(const Name: string): string;
begin
  Result := FileSearch(Name {$IFDEF WINDOWS} + '.exe' {$ENDIF},
    GetEnvironmentVariable('PATH'));
end;

{ bzip2 / xz blocks: unpacked by the program (bzip2 reports the zero padding
  SWORD puts after the stream, but still writes the text). }
function UnpackWithProgram(const Raw: TBytes; const ProgName: string;
  out Data: TBytes; out Err: string): Boolean;
var
  Exe, Tmp: string;
  P: TProcess;
  Out_: TMemoryStream;
  Buf: array[0..65535] of Byte;
  N: Integer;
  F: TFileStream;
begin
  Result := False;
  Data := nil;
  Err := '';
  Exe := FindProgram(ProgName);
  if Exe = '' then
  begin
    Err := Format(SSwordNeedTool, [ProgName, ProgName]);
    Exit;
  end;
  Tmp := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'ibiblia_sword_' +
    IntToStr(GetProcessID) + '.blk';
  F := TFileStream.Create(Tmp, fmCreate);
  try
    if Length(Raw) > 0 then
      F.WriteBuffer(Raw[0], Length(Raw));
  finally
    F.Free;
  end;
  P := TProcess.Create(nil);
  Out_ := TMemoryStream.Create;
  try
    P.Executable := Exe;
    P.Parameters.Add('-dc');
    P.Parameters.Add(Tmp);
    P.Options := [poUsePipes, poStderrToOutPut];
    P.ShowWindow := swoHide;
    P.Execute;
    repeat
      N := P.Output.Read(Buf, SizeOf(Buf));
      if N > 0 then
        Out_.WriteBuffer(Buf, N);
    until N <= 0;
    P.WaitOnExit;
    SetLength(Data, Out_.Size);
    if Out_.Size > 0 then
    begin
      Out_.Position := 0;
      Out_.ReadBuffer(Data[0], Out_.Size);
    end;
    Result := True;
  finally
    Out_.Free;
    P.Free;
    DeleteFile(Tmp);
  end;
end;

{ ---------- text ---------- }

function DecodeSwordText(const B: TBytes; Latin: Boolean): string;
var
  S: string;
begin
  if Length(B) = 0 then
    Exit('');
  SetString(S, PAnsiChar(@B[0]), Length(B));
  if (not Latin) and (FindInvalidUTF8Codepoint(PChar(S), Length(S), False) < 0) then
    Result := S
  else
    Result := CP1252ToUTF8(S);
end;

function CodepointToUTF8(CP: Integer): string;
begin
  if (CP <= 0) or (CP > $10FFFF) then
    Exit('');
  Result := UnicodeToUTF8(CP);
end;

function DecodeEntities(const S: string): string;
var
  I, J: Integer;
  E: string;
  N: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    if (S[I] = '&') then
    begin
      J := PosEx(';', S, I + 1);
      if (J > 0) and (J - I <= 10) then
      begin
        E := LowerCase(Copy(S, I + 1, J - I - 1));
        if E = 'amp' then Result := Result + '&'
        else if E = 'lt' then Result := Result + #$E2#$80#$B9
        else if E = 'gt' then Result := Result + #$E2#$80#$BA
        else if E = 'quot' then Result := Result + '"'
        else if E = 'apos' then Result := Result + ''''
        else if E = 'nbsp' then Result := Result + ' '
        else if (Length(E) > 1) and (E[1] = '#') then
        begin
          if (E[2] = 'x') then
            N := StrToIntDef('$' + Copy(E, 3, MaxInt), 0)
          else
            N := StrToIntDef(Copy(E, 2, MaxInt), 0);
          if N = 60 then Result := Result + #$E2#$80#$B9
          else if N = 62 then Result := Result + #$E2#$80#$BA
          else Result := Result + CodepointToUTF8(N);
        end
        else
        begin
          Result := Result + Copy(S, I, J - I + 1);
        end;
        I := J + 1;
        Continue;
      end;
    end;
    Result := Result + S[I];
    Inc(I);
  end;
end;

function AttrValue(const Tag, Name: string): string;
var
  L: string;
  P, Q: Integer;
  Quote: Char;
begin
  Result := '';
  L := LowerCase(Tag);
  P := 1;
  while True do
  begin
    P := PosEx(LowerCase(Name), L, P);
    if P = 0 then
      Exit;
    if ((P = 1) or (L[P - 1] in [' ', #9, #10, #13, '<'])) then
    begin
      Q := P + Length(Name);
      while (Q <= Length(Tag)) and (Tag[Q] in [' ', #9]) do
        Inc(Q);
      if (Q <= Length(Tag)) and (Tag[Q] = '=') then
      begin
        Inc(Q);
        while (Q <= Length(Tag)) and (Tag[Q] in [' ', #9]) do
          Inc(Q);
        if (Q <= Length(Tag)) and (Tag[Q] in ['"', '''']) then
        begin
          Quote := Tag[Q];
          Inc(Q);
          P := Q;
          while (Q <= Length(Tag)) and (Tag[Q] <> Quote) do
            Inc(Q);
          Exit(DecodeEntities(Copy(Tag, P, Q - P)));
        end;
      end;
    end;
    Inc(P);
  end;
end;

function StripTags(const S: string): string;
var
  I: Integer;
  InTag: Boolean;
begin
  Result := '';
  InTag := False;
  for I := 1 to Length(S) do
    if S[I] = '<' then InTag := True
    else if S[I] = '>' then InTag := False
    else if not InTag then Result := Result + S[I];
end;

function CollapseSpaces(const S: string): string;
var
  I: Integer;
  Prev: Boolean;
begin
  Result := '';
  Prev := True;
  for I := 1 to Length(S) do
    if S[I] in [' ', #9, #10, #13] then
    begin
      if not Prev then
        Result := Result + ' ';
      Prev := True;
    end
    else
    begin
      Result := Result + S[I];
      Prev := False;
    end;
  Result := TrimRight(Result);
end;

{ Strong's numbers (G976, H430) in a lemma / strong attribute. }
procedure CollectStrongs(const S: string; List: TStrings);
var
  Tokens: TStringList;
  T, D: string;
  I, J: Integer;
begin
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ' ';
    Tokens.StrictDelimiter := True;
    Tokens.DelimitedText := StringReplace(S, #9, ' ', [rfReplaceAll]);
    for I := 0 to Tokens.Count - 1 do
    begin
      T := Trim(Tokens[I]);
      if T = '' then
        Continue;
      if (Pos('lemma', LowerCase(T)) = 1) then
        Continue;
      J := Pos(':', T);
      if J > 0 then
      begin
        if Pos('|' + LowerCase(Copy(T, 1, J - 1)) + '|', '|strong|x-strongs|x-strong|sn|strongs|') = 0 then
          Continue;
        T := Copy(T, J + 1, MaxInt);
      end;
      if (Length(T) > 1) and (UpCase(T[1]) in ['G', 'H']) then
      begin
        D := '';
        J := 2;
        while (J <= Length(T)) and (T[J] in ['0'..'9']) do
        begin
          D := D + T[J];
          Inc(J);
        end;
        if (D <> '') and ((J > Length(T)) or (T[J] in ['a'..'z'])) then
          List.Add(UpCase(T[1]) + IntToStr(StrToIntDef(D, 0)));
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

function LemmaOf(const S: string): string;
var
  P, Q: Integer;
  L: string;
begin
  Result := '';
  L := LowerCase(S);
  P := Pos('lemma', L);
  while P > 0 do
  begin
    Q := P + 5;
    while (Q <= Length(S)) and not (S[Q] in [':', ' ']) do
      Inc(Q);
    if (Q <= Length(S)) and (S[Q] = ':') then
    begin
      P := Q + 1;
      Q := P;
      while (Q <= Length(S)) and not (S[Q] in [' ', #9]) do
        Inc(Q);
      Exit(StringReplace(Copy(S, P, Q - P), '"', '', [rfReplaceAll]));
    end;
    P := PosEx('lemma', L, P + 5);
  end;
end;

procedure CollectMorph(const S: string; List: TStrings);
var
  Tokens: TStringList;
  T: string;
  I, J: Integer;
begin
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ' ';
    Tokens.StrictDelimiter := True;
    Tokens.DelimitedText := S;
    for I := 0 to Tokens.Count - 1 do
    begin
      T := Trim(Tokens[I]);
      J := Pos(':', T);
      if J > 0 then
        T := Copy(T, J + 1, MaxInt);
      if T <> '' then
        List.Add(T);
    end;
  finally
    Tokens.Free;
  end;
end;

function WordTags(const LemmaAttr, StrongAttr, MorphAttr: string): string;
var
  St, Mo: TStringList;
  I: Integer;
  Lem: string;
begin
  Result := '';
  St := TStringList.Create;
  Mo := TStringList.Create;
  try
    CollectStrongs(LemmaAttr, St);
    CollectStrongs(StrongAttr, St);
    CollectMorph(MorphAttr, Mo);
    Lem := LemmaOf(LemmaAttr);
    for I := 0 to St.Count - 1 do
      Result := Result + '<W' + St[I] + '>';
    for I := 0 to Mo.Count - 1 do
      if (I = 0) and (Lem <> '') then
        Result := Result + '<WT' + Mo[I] + ' l="' + Lem + '">'
      else
        Result := Result + '<WT' + Mo[I] + '>';
  finally
    Mo.Free;
    St.Free;
  end;
end;

type
  TOpen = record
    Name: string;
    Kind: Integer;   // 0 pass, 1 word, 2 note, 3 added, 4 jesus, 5 drop, 6 bold, 7 italic
    Start: Integer;
    Attrs: string;
  end;

{ OSIS / ThML / TEI / GBF / plain verse text to a theWord verse line.
  Woj: words of Jesus are open at the end of the verse (carried to the next). }
function MarkupToONT(const Raw, SrcType: string; var Woj: Boolean): string;
var
  Out_: string;
  Stack: array of TOpen;
  Sp: Integer;
  I, J: Integer;
  Part, Tag, Name, Rest: string;
  Closing, SelfClose: Boolean;
  Mode: string;
  SkipHeading: Boolean;

  procedure Push(const AName: string; AKind: Integer; const AAttrs: string);
  begin
    if Sp >= Length(Stack) then
      SetLength(Stack, Sp + 16);
    Stack[Sp].Name := AName;
    Stack[Sp].Kind := AKind;
    Stack[Sp].Start := Length(Out_);
    Stack[Sp].Attrs := AAttrs;
    Inc(Sp);
  end;

  procedure AddSpace;
  begin
    if (Out_ <> '') and (Out_[Length(Out_)] <> ' ') then
      Out_ := Out_ + ' ';
  end;

  procedure EndItem(const It: TOpen);
  var
    Inner, Plain: string;
  begin
    case It.Kind of
      1:
        begin
          Inner := Copy(Out_, It.Start + 1, MaxInt);
          SetLength(Out_, It.Start);
          Out_ := Out_ + Inner + WordTags(AttrValue('<' + It.Attrs, 'lemma') + ' ' +
            AttrValue('<' + It.Attrs, 'savlm'), AttrValue('<' + It.Attrs, 'strong'),
            AttrValue('<' + It.Attrs, 'morph'));
        end;
      2:
        begin
          Inner := Copy(Out_, It.Start + 1, MaxInt);
          SetLength(Out_, It.Start);
          Plain := CollapseSpaces(StripTags(Inner));
          if Plain <> '' then
            Out_ := Out_ + '<RF>' + Plain + '<Rf>';
        end;
      3: Out_ := Out_ + '<Fi>';
      4: if Woj then begin Out_ := Out_ + '<Fr>'; Woj := False; end;
      5:
        begin
          // headings are not kept inside a verse, except a Psalm's own title
          if AttrValue('<' + It.Attrs, 'canonical') <> 'true' then
            SetLength(Out_, It.Start);
        end;
    end;
  end;

  procedure Pop(const AName: string);
  var
    K: Integer;
  begin
    for K := Sp - 1 downto 0 do
      if Stack[K].Name = AName then
      begin
        while Sp > K + 1 do
        begin
          Dec(Sp);
          EndItem(Stack[Sp]);
        end;
        Dec(Sp);
        EndItem(Stack[Sp]);
        Exit;
      end;
  end;

begin
  Out_ := '';
  Sp := 0;
  Mode := UpperCase(SrcType);
  SkipHeading := False;
  if Woj then
    Out_ := '<FR>';

  if (Mode = 'GBF') then
  begin
    I := 1;
    while I <= Length(Raw) do
    begin
      if Raw[I] = '<' then
      begin
        J := PosEx('>', Raw, I);
        if J = 0 then
          Break;
        Tag := Copy(Raw, I + 1, J - I - 1);
        I := J + 1;
        if Tag = 'FR' then begin Out_ := Out_ + '<FR>'; Woj := True; end
        else if Tag = 'Fr' then begin if Woj then Out_ := Out_ + '<Fr>'; Woj := False; end
        else if (Tag = 'FI') or (Tag = 'Fi') or (Tag = 'RF') or (Tag = 'Rf') then
          Out_ := Out_ + '<' + Tag + '>'
        else if (Tag = 'TS') or (Pos('TS', Tag) = 1) and (Length(Tag) = 3) then SkipHeading := True
        else if Tag = 'Ts' then SkipHeading := False
        else if (Tag = 'CM') or (Tag = 'CL') or (Tag = 'CI') then AddSpace
        else if (Length(Tag) > 2) and (Tag[1] = 'W') and (Tag[2] in ['G', 'H']) and
          (Tag[3] in ['0'..'9']) then
          Out_ := Out_ + '<' + Tag + '>'
        else if (Length(Tag) > 2) and (Copy(Tag, 1, 2) = 'WT') then
          Out_ := Out_ + '<' + Tag + '>';
        Continue;
      end;
      J := PosEx('<', Raw, I);
      if J = 0 then
        J := Length(Raw) + 1;
      if not SkipHeading then
        Out_ := Out_ + DecodeEntities(Copy(Raw, I, J - I));
      I := J;
    end;
  end
  else if (Mode = 'PLAIN') and (Pos('<', Raw) = 0) then
    Out_ := Out_ + DecodeEntities(Raw)
  else
  begin
    I := 1;
    while I <= Length(Raw) do
    begin
      if Raw[I] = '<' then
      begin
        J := PosEx('>', Raw, I);
        if J = 0 then
          Break;
        Tag := Trim(Copy(Raw, I + 1, J - I - 1));
        I := J + 1;
        if Tag = '' then
          Continue;
        Closing := Tag[1] = '/';
        if Closing then
          Tag := Trim(Copy(Tag, 2, MaxInt));
        SelfClose := (Tag <> '') and (Tag[Length(Tag)] = '/');
        if SelfClose then
          Tag := Trim(Copy(Tag, 1, Length(Tag) - 1));
        J := 1;
        while (J <= Length(Tag)) and not (Tag[J] in [' ', #9, #10, #13]) do
          Inc(J);
        Name := LowerCase(Copy(Tag, 1, J - 1));
        Rest := Copy(Tag, J, MaxInt);
        if Closing then
        begin
          Pop(Name);
          if (Name = 'p') or (Name = 'l') or (Name = 'item') then
            AddSpace;
          Continue;
        end;
        if (Name = 'w') then Push(Name, 1, Rest)
        else if (Name = 'note') then Push(Name, 2, Rest)
        else if (Name = 'q') then
        begin
          if (Pos('jesus', LowerCase(AttrValue('<' + Rest, 'who'))) > 0) or
             (Pos('christ', LowerCase(AttrValue('<' + Rest, 'who'))) > 0) then
          begin
            if Pos('eid', LowerCase(Rest)) > 0 then
            begin
              if Woj then begin Out_ := Out_ + '<Fr>'; Woj := False; end;
            end
            else if Pos('sid', LowerCase(Rest)) > 0 then
            begin
              if not Woj then begin Out_ := Out_ + '<FR>'; Woj := True; end;
            end
            else if not SelfClose then
            begin
              if not Woj then begin Out_ := Out_ + '<FR>'; Woj := True; end;
              Push(Name, 4, Rest);
            end;
          end
          else if not SelfClose then
            Push(Name, 0, Rest);
        end
        else if (Name = 'transchange') or (Name = 'added') or (Name = 'insertion') then
        begin
          if (Name <> 'transchange') or (Pos('added', LowerCase(Rest)) > 0) or
             (Pos('supplied', LowerCase(Rest)) > 0) or (AttrValue('<' + Rest, 'type') = '') then
          begin
            if not SelfClose then
            begin
              Out_ := Out_ + '<FI>';
              Push(Name, 3, Rest);
            end;
          end
          else if not SelfClose then
            Push(Name, 0, Rest);
        end
        else if (Name = 'title') or (Name = 'head') then
        begin
          if not SelfClose then
            Push(Name, 5, Rest);
        end
        else if (Name = 'div') and (Pos('title', LowerCase(Rest)) + Pos('head', LowerCase(Rest)) > 0) then
        begin
          if not SelfClose then
            Push(Name, 5, Rest);
        end
        else if (Name = 'lb') or (Name = 'br') or (Name = 'milestone') or (Name = 'p') or
          (Name = 'l') or (Name = 'item') then
        begin
          AddSpace;
          if (not SelfClose) and ((Name = 'p') or (Name = 'l') or (Name = 'item')) then
            Push(Name, 0, Rest);
        end
        else if (Name = 'sync') then
        begin
          if Pos('strong', LowerCase(AttrValue('<' + Rest, 'type'))) > 0 then
            Out_ := Out_ + WordTags('', AttrValue('<' + Rest, 'value'), '')
          else if Pos('morph', LowerCase(AttrValue('<' + Rest, 'type'))) > 0 then
            Out_ := Out_ + '<WT' + AttrValue('<' + Rest, 'value') + '>';
        end
        else if not SelfClose then
          Push(Name, 0, Rest);
        Continue;
      end;
      J := PosEx('<', Raw, I);
      if J = 0 then
        J := Length(Raw) + 1;
      Out_ := Out_ + DecodeEntities(Copy(Raw, I, J - I));
      I := J;
    end;
    while Sp > 0 do
    begin
      Dec(Sp);
      EndItem(Stack[Sp]);
    end;
  end;

  if Woj then
    Out_ := Out_ + '<Fr>';
  Out_ := StringReplace(Out_, #13, ' ', [rfReplaceAll]);
  Out_ := StringReplace(Out_, #10, ' ', [rfReplaceAll]);
  Out_ := StringReplace(Out_, '<FR><Fr>', '', [rfReplaceAll]);
  Out_ := StringReplace(Out_, '<FI><Fi>', '', [rfReplaceAll]);
  Result := CollapseSpaces(Out_);
  while Pos(' <', Result) = 1 do
    Delete(Result, 1, 1);
  Result := Trim(Result);
end;

{ ---------- the modules ---------- }

function ConfRoot(const ConfFile: string): string;
begin
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(ConfFile)));
end;

function ConfDataPath(const Conf: TSwordConf): string;
begin
  Result := StringReplace(Conf.Get('DataPath'), '\', '/', [rfReplaceAll]);
  while Copy(Result, 1, 2) = './' do
    Delete(Result, 1, 2);
  Result := StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
end;

function DriverKind(const Driver: string): string;
var
  D: string;
begin
  D := LowerCase(Driver);
  if (D = 'ztext') or (D = 'ztext4') or (D = 'rawtext') or (D = 'rawtext4') then
    Result := 'Bible'
  else if (D = 'zld') or (D = 'rawld') or (D = 'rawld4') then
    Result := 'dictionary'
  else if (D = 'zcom') or (D = 'zcom4') or (D = 'rawcom') or (D = 'rawcom4') then
    Result := 'commentary'
  else if D = 'rawgenbook' then
    Result := 'book'
  else
    Result := '';
end;

function SwordModuleKind(const ConfFile: string; out Reason: string): string;
var
  C: TSwordConf;
begin
  Result := '';
  Reason := '';
  if not SameText(ExtractFileExt(ConfFile), '.conf') then
    Exit;
  C := TSwordConf.Create;
  try
    if not C.Load(ConfFile) then
      Exit;
    if C.Has('CipherKey') and (Trim(C.Get('CipherKey')) <> '') then
    begin
      Reason := SSwordLocked;
      Exit;
    end;
    if C.Get('ModDrv') = '' then
    begin
      Reason := SSwordNoDriver;
      Exit;
    end;
    Result := DriverKind(C.Get('ModDrv'));
    if Result = '' then
      Reason := Format(SSwordUnsupported, [C.Get('ModDrv')]);
  finally
    C.Free;
  end;
end;

function IsSwordBibleConf(const FileName: string): Boolean;
var
  R: string;
begin
  Result := SameText(ExtractFileExt(FileName), '.conf') and FileExists(FileName) and
    (SwordModuleKind(FileName, R) = 'Bible');
end;

function IsSwordDictionaryConf(const FileName: string): Boolean;
var
  R: string;
begin
  Result := SameText(ExtractFileExt(FileName), '.conf') and FileExists(FileName) and
    (SwordModuleKind(FileName, R) = 'dictionary');
end;

const
  OSIS66: array[1..66] of string = (
    'Gen', 'Exod', 'Lev', 'Num', 'Deut', 'Josh', 'Judg', 'Ruth', '1Sam', '2Sam',
    '1Kgs', '2Kgs', '1Chr', '2Chr', 'Ezra', 'Neh', 'Esth', 'Job', 'Ps', 'Prov',
    'Eccl', 'Song', 'Isa', 'Jer', 'Lam', 'Ezek', 'Dan', 'Hos', 'Joel', 'Amos',
    'Obad', 'Jonah', 'Mic', 'Nah', 'Hab', 'Zeph', 'Hag', 'Zech', 'Mal',
    'Matt', 'Mark', 'Luke', 'John', 'Acts', 'Rom', '1Cor', '2Cor', 'Gal', 'Eph',
    'Phil', 'Col', '1Thess', '2Thess', '1Tim', '2Tim', 'Titus', 'Phlm', 'Heb',
    'Jas', '1Pet', '2Pet', '1John', '2John', '3John', 'Jude', 'Rev');

{ iBiblia book number for an OSIS book id (apocrypha through their USFM code). }
function OsisBookNumber(const Osis: string): Integer;
var
  I: Integer;
  Code: string;
begin
  for I := 1 to 66 do
    if SameText(OSIS66[I], Osis) then
      Exit(I);
  Code := '';
  if SameText(Osis, 'Tob') then Code := 'TOB'
  else if SameText(Osis, 'Jdt') then Code := 'JDT'
  else if SameText(Osis, 'AddEsth') or SameText(Osis, 'EsthGr') then Code := 'ESG'
  else if SameText(Osis, 'Wis') then Code := 'WIS'
  else if SameText(Osis, 'Sir') then Code := 'SIR'
  else if SameText(Osis, 'Bar') then Code := 'BAR'
  else if SameText(Osis, 'EpJer') then Code := 'LJE'
  else if SameText(Osis, 'PrAzar') then Code := 'S3Y'
  else if SameText(Osis, 'Sus') then Code := 'SUS'
  else if SameText(Osis, 'Bel') then Code := 'BEL'
  else if SameText(Osis, '1Macc') then Code := '1MA'
  else if SameText(Osis, '2Macc') then Code := '2MA'
  else if SameText(Osis, '3Macc') then Code := '3MA'
  else if SameText(Osis, '4Macc') then Code := '4MA'
  else if SameText(Osis, '1Esd') then Code := '1ES'
  else if SameText(Osis, '2Esd') then Code := '2ES'
  else if SameText(Osis, 'PrMan') then Code := 'MAN'
  else if SameText(Osis, 'AddPs') then Code := 'PS2'
  else if SameText(Osis, 'PssSol') then Code := 'PSS'
  else if SameText(Osis, 'Odes') then Code := 'ODA'
  else if SameText(Osis, 'EpLao') then Code := 'LAO';
  if Code = '' then
    Exit(0);
  Result := TESwordModule.USFMBookCodeToID(Code);
end;

function ApocryphaName(Book: Integer): string;
begin
  case Book of
    67: Result := '1 Esdras';
    68: Result := '2 Esdras';
    69: Result := 'Tobit';
    70: Result := 'Judith';
    71: Result := '1 Maccabees';
    72: Result := '2 Maccabees';
    73: Result := '3 Maccabees';
    74: Result := '4 Maccabees';
    75: Result := 'Prayer of Manasseh';
    76: Result := 'Wisdom of Solomon';
    77: Result := 'Sirach';
    78: Result := 'Baruch';
    79: Result := 'Letter of Jeremiah';
    80: Result := 'Prayer of Azariah';
    81: Result := 'Susanna';
    82: Result := 'Bel and the Dragon';
    83: Result := 'Additions to Esther';
    84: Result := 'Psalm 151';
    85: Result := 'Psalms of Solomon';
    86: Result := 'Odes';
    87: Result := 'Laodiceans';
  else
    Result := 'Book ' + IntToStr(Book);
  end;
end;

type
  TCanonBook = record
    Osis: string;
    Counts: TIntArray;
  end;
  TCanonBooks = array of TCanonBook;

procedure ParseCanon(const Spec: string; out Books: TCanonBooks);
var
  Items, Nums: TStringList;
  I, J, P: Integer;
begin
  Books := nil;
  Items := TStringList.Create;
  Nums := TStringList.Create;
  try
    Items.Delimiter := ';';
    Items.StrictDelimiter := True;
    Items.DelimitedText := Spec;
    for I := 0 to Items.Count - 1 do
    begin
      P := Pos('=', Items[I]);
      if P < 2 then
        Continue;
      SetLength(Books, Length(Books) + 1);
      Books[High(Books)].Osis := Copy(Items[I], 1, P - 1);
      Nums.Delimiter := ',';
      Nums.StrictDelimiter := True;
      Nums.DelimitedText := Copy(Items[I], P + 1, MaxInt);
      SetLength(Books[High(Books)].Counts, Nums.Count);
      for J := 0 to Nums.Count - 1 do
        Books[High(Books)].Counts[J] := StrToIntDef(Nums[J], 0);
    end;
  finally
    Nums.Free;
    Items.Free;
  end;
end;

function CanonTotal(const Books: TCanonBooks): Integer;
var
  B, C: Integer;
begin
  Result := 2;
  for B := 0 to High(Books) do
  begin
    Inc(Result, Length(Books[B].Counts) + 1);
    for C := 0 to High(Books[B].Counts) do
      Inc(Result, Books[B].Counts[C]);
  end;
end;

var
  KjvCh: array[1..66] of Integer;
  KjvReady: Boolean = False;

function KjvChapters(Book: Integer): Integer;
var
  B: TCanonBooks;
  I, N: Integer;
begin
  if not KjvReady then
  begin
    N := 0;
    ParseCanon(SwordCanonBooks('kjv', 'ot'), B);
    for I := 0 to High(B) do
      if N < 66 then
      begin
        Inc(N);
        KjvCh[N] := Length(B[I].Counts);
      end;
    ParseCanon(SwordCanonBooks('kjv', 'nt'), B);
    for I := 0 to High(B) do
      if N < 66 then
      begin
        Inc(N);
        KjvCh[N] := Length(B[I].Counts);
      end;
    KjvReady := True;
  end;
  if (Book >= 1) and (Book <= 66) then
    Result := KjvCh[Book]
  else
    Result := 0;
end;

type
  TTest = record
    Present: Boolean;
    V, S, Z: TFileStream;
    VBuf: TBytes;
    RecSize, Records: Integer;
    CacheN: Integer;
    CacheData: TBytes;
  end;

function LoadSwordBible(const ConfFile: string; Modulo: TStringList;
  out ExtraVerses: TESwordExtraVerseArray; out ApocryphaBooksInfo: string;
  out HasOT, HasNT: Boolean; out ErrorMessage: string): Boolean;
var
  Conf: TSwordConf;
  Kind, Reason, Driver, Compress, BlockLetter, DataDir, Declared, Sys, SrcType: string;
  Compressed, Wide, Latin: Boolean;
  T: array[0..1] of TTest;
  TName: array[0..1] of string = ('ot', 'nt');
  Canon: array[0..1] of TCanonBooks;
  I, K, Fit, BestGap: Integer;
  Sizes: array[0..1] of Integer;
  AllVerses: array of string;
  ExtraCount, NonEmpty: Integer;
  Woj: Boolean;
  ApocPresent: array[67..150] of Boolean;

  function GapOf(const SysName: string): Integer;
  var
    Ti, D: Integer;
    Spec: string;
  begin
    Result := 0;
    for Ti := 0 to 1 do
    begin
      if not T[Ti].Present then
        Continue;
      Spec := SwordCanonBooks(SysName, TName[Ti]);
      if Spec = '' then
        Exit(-1);
      ParseCanon(Spec, Canon[Ti]);
      D := CanonTotal(Canon[Ti]) - T[Ti].Records;
      if (D < 0) or (D > 2) then
        Exit(-1);
      Inc(Result, D);
    end;
  end;

  procedure SetRecSize(RS: Integer);
  var
    Ti: Integer;
  begin
    for Ti := 0 to 1 do
      if T[Ti].Present then
      begin
        T[Ti].RecSize := RS;
        T[Ti].Records := Length(T[Ti].VBuf) div RS;
      end;
  end;

  function BlockBytes(Ti, N: Integer): TBytes;
  var
    Rec, Raw, Data: TBytes;
    Off, Size: Cardinal;
    Err: string;
  begin
    Result := nil;
    if T[Ti].CacheN = N then
      Exit(T[Ti].CacheData);
    Rec := ReadPart(T[Ti].S, Int64(N) * 12, 12);
    if Length(Rec) < 8 then
      Exit;
    Off := U32(Rec, 0);
    Size := U32(Rec, 4);
    Raw := ReadPart(T[Ti].Z, Off, Size);
    if Length(Raw) = 0 then
      Exit;
    if Compress = 'ZIP' then
      Data := InflateBlock(Raw)
    else if Compress = 'XZ' then
    begin
      if not UnpackWithProgram(Raw, 'xz', Data, Err) then
        raise Exception.Create(Err);
    end
    else if not UnpackWithProgram(Raw, 'bzip2', Data, Err) then
      raise Exception.Create(Err);
    T[Ti].CacheN := N;
    T[Ti].CacheData := Data;
    Result := Data;
  end;

  function RawVerse(Ti, Idx: Integer): string;
  var
    O: ^TTest;
    P, Blk, Raw: TBytes;
    N, Start, Len: Cardinal;
  begin
    Result := '';
    O := @T[Ti];
    if (Idx < 0) or (Idx >= O^.Records) then
      Exit;
    SetLength(P, O^.RecSize);
    Move(O^.VBuf[Idx * O^.RecSize], P[0], O^.RecSize);
    if Compressed then
    begin
      N := U32(P, 0);
      Start := U32(P, 4);
      if O^.RecSize > 10 then Len := U32(P, 8) else Len := U16(P, 8);
      if Len = 0 then
        Exit;
      Blk := BlockBytes(Ti, N);
      if (Blk = nil) or (Start + Len > Cardinal(Length(Blk))) then
        Exit;
      SetLength(Raw, Len);
      Move(Blk[Start], Raw[0], Len);
    end
    else
    begin
      Start := U32(P, 0);
      if O^.RecSize > 6 then Len := U32(P, 4) else Len := U16(P, 4);
      if Len = 0 then
        Exit;
      Raw := ReadPart(O^.Z, Start, Len);
      if Raw = nil then
        Exit;
    end;
    Result := DecodeSwordText(Raw, Latin);
  end;

  procedure AddVerse(Book, Chap, Verse: Integer; const Text: string);
  var
    LineIdx, MaxV: Integer;
    PareID: string;
  begin
    if Text = '' then
      Exit;
    Inc(NonEmpty);
    if (Book <= 66) and (Chap >= 1) and (Chap <= KjvChapters(Book)) then
    begin
      LineIdx := TESwordModule.GetCanonicalLineIndex(Book, Chap, Verse, MaxV);
      if LineIdx >= 0 then
      begin
        if Book <= 39 then HasOT := True else HasNT := True;
        if AllVerses[LineIdx] <> '' then
          AllVerses[LineIdx] := AllVerses[LineIdx] + ' ' + Text
        else
          AllVerses[LineIdx] := Text;
      end;
    end
    else if ((Book <= 66) and (Chap > KjvChapters(Book))) or
      ((Book >= 67) and (Book <= 150)) then
    begin
      if (Book <= 39) or (Book >= 67) then HasOT := True else HasNT := True;
      PareID := Format('%d,%d,%d', [Book, Chap, Verse]);
      if (ExtraCount > 0) and (ExtraVerses[ExtraCount - 1].PareID = PareID) then
        ExtraVerses[ExtraCount - 1].ONTText := ExtraVerses[ExtraCount - 1].ONTText + ' ' + Text
      else
      begin
        if ExtraCount >= Length(ExtraVerses) then
          SetLength(ExtraVerses, ExtraCount + 1024);
        ExtraVerses[ExtraCount].Book := Book;
        ExtraVerses[ExtraCount].Chapter := Chap;
        ExtraVerses[ExtraCount].Verse := Verse;
        ExtraVerses[ExtraCount].PareID := PareID;
        ExtraVerses[ExtraCount].ONTText := Text;
        Inc(ExtraCount);
      end;
      if Book >= 67 then
        ApocPresent[Book] := True;
    end;
  end;

  procedure ReadAll;
  var
    Ti, B, C, V, Start, Before, Idx, Book: Integer;
    Text: string;
  begin
    for Ti := 0 to 1 do
    begin
      if not T[Ti].Present then
        Continue;
      Start := 2;
      for B := 0 to High(Canon[Ti]) do
      begin
        Book := OsisBookNumber(Canon[Ti][B].Osis);
        Before := 0;
        if Book > 0 then
          for C := 1 to Length(Canon[Ti][B].Counts) do
          begin
            for V := 1 to Canon[Ti][B].Counts[C - 1] do
            begin
              Idx := Start + 1 + C + Before + (V - 1);
              Text := RawVerse(Ti, Idx);
              if Text <> '' then
                AddVerse(Book, C, V, MarkupToONT(Text, SrcType, Woj));
            end;
            Inc(Before, Canon[Ti][B].Counts[C - 1]);
          end;
        Inc(Start, Length(Canon[Ti][B].Counts) + 1);
        for C := 0 to High(Canon[Ti][B].Counts) do
          Inc(Start, Canon[Ti][B].Counts[C]);
      end;
    end;
  end;

begin
  Result := False;
  HasOT := False;
  HasNT := False;
  ErrorMessage := '';
  ExtraVerses := nil;
  ApocryphaBooksInfo := '';
  ExtraCount := 0;
  NonEmpty := 0;
  Woj := False;
  for I := 67 to 150 do
    ApocPresent[I] := False;
  for I := 0 to 1 do
  begin
    T[I].Present := False;
    T[I].V := nil;
    T[I].S := nil;
    T[I].Z := nil;
    T[I].CacheN := -1;
  end;
  Kind := SwordModuleKind(ConfFile, Reason);
  if Kind <> 'Bible' then
  begin
    if Reason <> '' then ErrorMessage := Reason else ErrorMessage := SSwordNotBible;
    Exit;
  end;
  Conf := TSwordConf.Create;
  try
    Conf.Load(ConfFile);
    Driver := LowerCase(Conf.Get('ModDrv'));
    Compress := UpperCase(Conf.Get('CompressType', 'ZIP'));
    Compressed := Copy(Driver, 1, 1) = 'z';
    Wide := Copy(Driver, Length(Driver), 1) = '4';
    if Compressed and (Compress = 'LZSS') then
    begin
      ErrorMessage := Format(SSwordUnsupported, ['LZSS']);
      Exit;
    end;
    case UpperCase(Conf.Get('BlockType', 'BOOK'))[1] of
      'C': BlockLetter := 'c';
      'V': BlockLetter := 'v';
    else
      BlockLetter := 'b';
    end;
    SrcType := Conf.Get('SourceType', 'Plain');
    Latin := (Pos('latin', LowerCase(Conf.Get('Encoding'))) > 0) or
      (Pos('1252', Conf.Get('Encoding')) > 0) or (Pos('8859-1', Conf.Get('Encoding')) > 0);
    DataDir := ConfRoot(ConfFile) + ConfDataPath(Conf);
    if not DirectoryExists(DataDir) then
      DataDir := ExtractFilePath(DataDir);
    try
      for I := 0 to 1 do
      begin
        if Compressed then
        begin
          T[I].V := nil;
          K := 0;
          if (FindFileCI(DataDir, TName[I] + '.' + BlockLetter + 'zv') <> '') and
             (FindFileCI(DataDir, TName[I] + '.' + BlockLetter + 'zs') <> '') and
             (FindFileCI(DataDir, TName[I] + '.' + BlockLetter + 'zz') <> '') then
          begin
            T[I].V := TFileStream.Create(FindFileCI(DataDir, TName[I] + '.' + BlockLetter + 'zv'), fmOpenRead or fmShareDenyNone);
            T[I].S := TFileStream.Create(FindFileCI(DataDir, TName[I] + '.' + BlockLetter + 'zs'), fmOpenRead or fmShareDenyNone);
            T[I].Z := TFileStream.Create(FindFileCI(DataDir, TName[I] + '.' + BlockLetter + 'zz'), fmOpenRead or fmShareDenyNone);
            T[I].Present := True;
          end;
        end
        else if (FindFileCI(DataDir, TName[I] + '.vss') <> '') and
          (FindFileCI(DataDir, TName[I]) <> '') then
        begin
          T[I].V := TFileStream.Create(FindFileCI(DataDir, TName[I] + '.vss'), fmOpenRead or fmShareDenyNone);
          T[I].Z := TFileStream.Create(FindFileCI(DataDir, TName[I]), fmOpenRead or fmShareDenyNone);
          T[I].Present := True;
        end;
        if T[I].Present then
        begin
          SetLength(T[I].VBuf, T[I].V.Size);
          if T[I].V.Size > 0 then
          begin
            T[I].V.Position := 0;
            T[I].V.ReadBuffer(T[I].VBuf[0], T[I].V.Size);
          end;
        end;
      end;
      if not (T[0].Present or T[1].Present) then
      begin
        ErrorMessage := Format(SSwordFilesMissing, [DataDir]);
        Exit;
      end;

      // record width: the size that fits one of the numbering systems wins
      if Compressed then
      begin
        if Wide then begin Sizes[0] := 12; Sizes[1] := 10; end
        else begin Sizes[0] := 10; Sizes[1] := 12; end;
      end
      else
      begin
        if Wide then begin Sizes[0] := 8; Sizes[1] := 6; end
        else begin Sizes[0] := 6; Sizes[1] := 8; end;
      end;
      Fit := -1;
      for K := 0 to 1 do
      begin
        SetRecSize(Sizes[K]);
        for I := 0 to WordCount(SwordCanonNames, [',']) - 1 do
          if GapOf(ExtractWord(I + 1, SwordCanonNames, [','])) >= 0 then
          begin
            Fit := K;
            Break;
          end;
        if Fit >= 0 then
          Break;
      end;
      if Fit < 0 then
        Fit := 0;
      SetRecSize(Sizes[Fit]);

      // which numbering system: the .conf names it, the verse index confirms it
      Declared := LowerCase(Conf.Get('Versification', 'KJV'));
      Sys := '';
      if (SwordCanonBooks(Declared, 'ot') <> '') and (GapOf(Declared) >= 0) then
        Sys := Declared
      else
      begin
        BestGap := MaxInt;
        for I := 0 to WordCount(SwordCanonNames, [',']) - 1 do
        begin
          K := GapOf(ExtractWord(I + 1, SwordCanonNames, [',']));
          if (K >= 0) and (K < BestGap) then
          begin
            BestGap := K;
            Sys := ExtractWord(I + 1, SwordCanonNames, [',']);
          end;
        end;
      end;
      if Sys = '' then
        Sys := 'kjv';
      GapOf(Sys);   // fills Canon[]

      SetLength(AllVerses, ONT_LINES);
      ReadAll;
    finally
      for I := 0 to 1 do
      begin
        T[I].V.Free;
        T[I].S.Free;
        T[I].Z.Free;
      end;
    end;

    SetLength(ExtraVerses, ExtraCount);
    if (NonEmpty = 0) or not (HasOT or HasNT) then
    begin
      ErrorMessage := SSwordNoVerses;
      Exit;
    end;
    for I := 67 to 150 do
      if ApocPresent[I] then
      begin
        if ApocryphaBooksInfo <> '' then
          ApocryphaBooksInfo := ApocryphaBooksInfo + ';';
        ApocryphaBooksInfo := ApocryphaBooksInfo + IntToStr(I) + '=' + ApocryphaName(I);
      end;

    Modulo.BeginUpdate;
    try
      Modulo.Clear;
      if HasOT and HasNT then
        for I := 0 to ONT_LINES - 1 do
          Modulo.Add(AllVerses[I])
      else if HasOT then
        for I := 0 to OT_LINES - 1 do
          Modulo.Add(AllVerses[I])
      else
        for I := OT_LINES to ONT_LINES - 1 do
          Modulo.Add(AllVerses[I]);
      Modulo.Add('description=' + StringReplace(Conf.Get('Description', Conf.Name), #10, ' ', [rfReplaceAll]));
      Modulo.Add('short.title=' + Copy(Conf.Name, 1, 15));
      Modulo.Add('title=' + StringReplace(Conf.Get('Description', Conf.Name), #10, ' ', [rfReplaceAll]));
    finally
      Modulo.EndUpdate;
    end;
    Result := True;
  finally
    Conf.Free;
  end;
end;

{ ---------- dictionaries ---------- }

function RtfEscape(const S: string): string;
var
  P: PChar;
  Len, CP, CLen: Integer;
  U: Word;
begin
  Result := '';
  P := PChar(S);
  Len := Length(S);
  while Len > 0 do
  begin
    CP := Integer(UTF8CodepointToUnicode(P, CLen));
    if CLen <= 0 then
      CLen := 1;
    if CLen > Len then
      CLen := Len;
    Dec(Len, CLen);
    Inc(P, CLen);
    if CP = 92 then Result := Result + '\\'
    else if CP = 123 then Result := Result + '\{'
    else if CP = 125 then Result := Result + '\}'
    else if CP = 10 then Result := Result + '\par '
    else if (CP < 32) then
    else if CP < 127 then Result := Result + Chr(CP)
    else
    begin
      if CP > $FFFF then
      begin
        Dec(CP, $10000);
        Result := Result + '\u' + IntToStr(SmallInt($D800 + (CP shr 10))) + '?' +
          '\u' + IntToStr(SmallInt($DC00 + (CP and $3FF))) + '?';
      end
      else
      begin
        U := CP;
        Result := Result + '\u' + IntToStr(SmallInt(U)) + '?';
      end;
    end;
  end;
end;

{ Dictionary entry (ThML / OSIS / plain) to RTF. }
function MarkupToRtf(const Raw: string): string;
var
  I, J: Integer;
  Tag, Name: string;
  Closing: Boolean;
  Body, Chunk: string;
  Stack: array[0..63] of string;
  Sp: Integer;
begin
  Body := '';
  Sp := 0;
  I := 1;
  while I <= Length(Raw) do
  begin
    if Raw[I] = '<' then
    begin
      J := PosEx('>', Raw, I);
      if J = 0 then
        Break;
      Tag := Trim(Copy(Raw, I + 1, J - I - 1));
      I := J + 1;
      if Tag = '' then
        Continue;
      Closing := Tag[1] = '/';
      if Closing then
        Tag := Copy(Tag, 2, MaxInt);
      J := 1;
      while (J <= Length(Tag)) and not (Tag[J] in [' ', #9, #10, #13, '/']) do
        Inc(J);
      Name := LowerCase(Copy(Tag, 1, J - 1));
      if Closing then
      begin
        if (Sp > 0) and (Stack[Sp - 1] = Name) then
        begin
          Dec(Sp);
          Body := Body + '}';
        end
        else if (Name = 'p') or (Name = 'l') or (Name = 'div') or (Name = 'item') then
          Body := Body + #10;
        Continue;
      end;
      if (Name = 'br') or (Name = 'lb') then
        Body := Body + #10
      else if ((Name = 'p') or (Name = 'div') or (Name = 'l') or (Name = 'item')) and
        (Body <> '') and (Body[Length(Body)] <> #10) then
        Body := Body + #10
      else if (Tag[Length(Tag)] <> '/') and (Sp < 64) and
        ((Name = 'orth') or (Name = 'b') or (Name = 'strong')) then
      begin
        Body := Body + '{\b ';
        Stack[Sp] := Name;
        Inc(Sp);
      end
      else if (Tag[Length(Tag)] <> '/') and (Sp < 64) and
        ((Name = 'pron') or (Name = 'i') or (Name = 'em') or (Name = 'foreign') or
         (Name = 'orig') or (Name = 'hi') or (Name = 'emphasis')) then
      begin
        Body := Body + '{\i ';
        Stack[Sp] := Name;
        Inc(Sp);
      end;
      Continue;
    end;
    J := PosEx('<', Raw, I);
    if J = 0 then
      J := Length(Raw) + 1;
    Chunk := DecodeEntities(Copy(Raw, I, J - I));
    Body := Body + RtfEscape(Chunk);
    I := J;
  end;
  while Sp > 0 do
  begin
    Dec(Sp);
    Body := Body + '}';
  end;
  Body := Trim(Body);
  while Pos(#10#10#10, Body) > 0 do
    Body := StringReplace(Body, #10#10#10, #10#10, [rfReplaceAll]);
  while (Body <> '') and (Body[1] in [#10, ' ']) do
    Delete(Body, 1, 1);
  Result := '{\rtf1\ansi\deff0{\fonttbl{\f0\fnil Segoe UI;}}\f0\fs22 ' +
    StringReplace(Body, #10, '\par ', [rfReplaceAll]) + '}';
end;

function SwordDictionaryTarget(const ConfFile: string): string;
var
  C: TSwordConf;
  N: string;
begin
  N := ChangeFileExt(ExtractFileName(ConfFile), '');
  C := TSwordConf.Create;
  try
    if C.Load(ConfFile) then
      N := C.Name;
  finally
    C.Free;
  end;
  Result := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'dictionaries';
  ForceDirectories(Result);
  Result := IncludeTrailingPathDelimiter(Result) + 'SWORD_' + N + '.dct.twm';
end;

function ConvertSwordDictionary(const ConfFile, OutFile: string;
  out ErrorMessage: string): Boolean;
var
  Conf: TSwordConf;
  Kind, Reason, Driver, Compress, Feat, FullPath, Dir, Pre, StrongsLetter: string;
  Zipped, Latin: Boolean;
  Idx, Dat, Zdx, Zdt: TBytes;
  RS, N, I, K, NL, TopicID, Count: Integer;
  Off, Sz: Cardinal;
  E, Raw, Key, Subject, Text, Rtf: string;
  Keys, Texts: TStringList;
  BlkNum, EntryNum: array of Cardinal;
  BlkCache: array of TBytes;
  BlkLoaded: array of Boolean;
  Conn: TSQLite3Connection;
  Tr: TSQLTransaction;
  QCfg, QTop, QCon: TSQLQuery;
  Err: string;

  function FileBytes(const Ext: string): TBytes;
  var
    F: string;
  begin
    Result := nil;
    F := FindFileCI(Dir, Pre + '.' + Ext);
    if F <> '' then
      Result := ReadWholeFile(F);
  end;

  function BlockOf(NBlock: Cardinal): TBytes;
  var
    O, S: Cardinal;
    Raw2, Data: TBytes;
    Er: string;
  begin
    Result := nil;
    if (Cardinal(NBlock) * 8 + 8) > Cardinal(Length(Zdx)) then
      Exit;
    if NBlock < Cardinal(Length(BlkLoaded)) then
      if BlkLoaded[NBlock] then
        Exit(BlkCache[NBlock]);
    O := U32(Zdx, NBlock * 8);
    S := U32(Zdx, NBlock * 8 + 4);
    if O + S > Cardinal(Length(Zdt)) then
      Exit;
    SetLength(Raw2, S);
    if S > 0 then
      Move(Zdt[O], Raw2[0], S);
    if Compress = 'ZIP' then
      Data := InflateBlock(Raw2)
    else if Compress = 'XZ' then
    begin
      if not UnpackWithProgram(Raw2, 'xz', Data, Er) then
        raise Exception.Create(Er);
    end
    else if not UnpackWithProgram(Raw2, 'bzip2', Data, Er) then
      raise Exception.Create(Er);
    if NBlock < Cardinal(Length(BlkLoaded)) then
    begin
      BlkLoaded[NBlock] := True;
      BlkCache[NBlock] := Data;
    end;
    Result := Data;
  end;

  function EntryText(Ix: Integer): string;
  var
    B, Raw2: TBytes;
    Cnt, O, S: Cardinal;
  begin
    Result := '';
    if not Zipped then
    begin
      Result := Texts[Ix];
      Exit;
    end;
    B := BlockOf(BlkNum[Ix]);
    if B = nil then
      Exit;
    Cnt := U32(B, 0);
    if EntryNum[Ix] >= Cnt then
      Exit;
    O := U32(B, 4 + EntryNum[Ix] * 8);
    S := U32(B, 8 + EntryNum[Ix] * 8);
    if O + S > Cardinal(Length(B)) then
      Exit;
    SetLength(Raw2, S);
    if S > 0 then
      Move(B[O], Raw2[0], S);
    Result := DecodeSwordText(Raw2, Latin);
    while (Result <> '') and (Result[Length(Result)] in [#0, #10, #13]) do
      Delete(Result, Length(Result), 1);
  end;

  procedure SetCfg(const AName, AValue: string);
  begin
    QCfg.Params.ParamByName('name').AsString := AName;
    QCfg.Params.ParamByName('value').AsString := AValue;
    QCfg.ExecSQL;
  end;

begin
  Result := False;
  ErrorMessage := '';
  Kind := SwordModuleKind(ConfFile, Reason);
  if Kind <> 'dictionary' then
  begin
    if Reason <> '' then ErrorMessage := Reason else ErrorMessage := SSwordNotDictionary;
    Exit;
  end;
  Conf := TSwordConf.Create;
  Keys := TStringList.Create;
  Texts := TStringList.Create;
  try
    Conf.Load(ConfFile);
    Driver := LowerCase(Conf.Get('ModDrv'));
    Compress := UpperCase(Conf.Get('CompressType', 'ZIP'));
    Zipped := Driver = 'zld';
    Latin := (Pos('latin', LowerCase(Conf.Get('Encoding'))) > 0) or
      (Pos('1252', Conf.Get('Encoding')) > 0) or (Pos('8859-1', Conf.Get('Encoding')) > 0);
    Feat := Conf.Joined('Feature');
    if Pos('greekdef', LowerCase(Feat)) > 0 then StrongsLetter := 'G'
    else if Pos('hebrewdef', LowerCase(Feat)) > 0 then StrongsLetter := 'H'
    else StrongsLetter := '';
    FullPath := ConfRoot(ConfFile) + ConfDataPath(Conf);
    Dir := ExtractFilePath(FullPath);
    Pre := ExtractFileName(FullPath);
    Idx := FileBytes('idx');
    Dat := FileBytes('dat');
    if (Idx = nil) or (Dat = nil) then
    begin
      ErrorMessage := Format(SSwordFilesMissing, [Dir]);
      Exit;
    end;
    if Zipped then
    begin
      Zdx := FileBytes('zdx');
      Zdt := FileBytes('zdt');
      if (Zdx = nil) or (Zdt = nil) then
      begin
        ErrorMessage := Format(SSwordFilesMissing, [Dir]);
        Exit;
      end;
      SetLength(BlkCache, Length(Zdx) div 8);
      SetLength(BlkLoaded, Length(Zdx) div 8);
    end;
    if Zipped or (Driver = 'rawld4') then RS := 8 else RS := 6;
    N := Length(Idx) div RS;
    SetLength(BlkNum, N);
    SetLength(EntryNum, N);
    for I := 0 to N - 1 do
    begin
      Off := U32(Idx, I * RS);
      if RS = 6 then Sz := U16(Idx, I * RS + 4) else Sz := U32(Idx, I * RS + 4);
      if Off + Sz > Cardinal(Length(Dat)) then
        Continue;
      NL := -1;
      for K := 0 to Integer(Sz) - 1 do
        if Dat[Off + Cardinal(K)] = 10 then
        begin
          NL := K;
          Break;
        end;
      if NL < 0 then
        Continue;
      SetString(Raw, PAnsiChar(@Dat[Off]), NL);
      Key := Trim(StringReplace(DecodeSwordText(BytesOf(Raw), Latin), #13, '', [rfReplaceAll]));
      if Key = '' then
        Continue;
      if Zipped then
      begin
        if Integer(Sz) - NL - 1 >= 8 then
        begin
          BlkNum[Keys.Count] := U32(Dat, Off + Sz - 8);
          EntryNum[Keys.Count] := U32(Dat, Off + Sz - 4);
        end;
        Keys.Add(Key);
        Texts.Add('');
      end
      else
      begin
        SetString(E, PAnsiChar(@Dat[Off + Cardinal(NL) + 1]), Integer(Sz) - NL - 1);
        Keys.Add(Key);
        Texts.Add(DecodeSwordText(BytesOf(E), Latin));
      end;
    end;
    if Keys.Count = 0 then
    begin
      ErrorMessage := SSwordNoEntries;
      Exit;
    end;

    if FileExists(OutFile) then
      DeleteFile(OutFile);
    Conn := TSQLite3Connection.Create(nil);
    Tr := TSQLTransaction.Create(nil);
    try
      Conn.DatabaseName := OutFile;
      Conn.Transaction := Tr;
      Conn.Open;
      Tr.Active := True;
      Conn.ExecuteDirect('CREATE TABLE config(name text, value text)');
      Conn.ExecuteDirect('CREATE TABLE content(topic_id integer primary key, data text, data2 blob)');
      Conn.ExecuteDirect('CREATE TABLE topics(id integer primary key, pid integer default 0, subject text, rel_order, content_type text, strong_orig_word text)');
      Conn.ExecuteDirect('CREATE INDEX idx_topics_subject on topics(subject)');
      QCfg := TSQLQuery.Create(nil);
      QTop := TSQLQuery.Create(nil);
      QCon := TSQLQuery.Create(nil);
      try
        QCfg.DataBase := Conn;
        QCfg.SQL.Text := 'INSERT INTO config VALUES (:name, :value)';
        QCfg.Prepare;
        QTop.DataBase := Conn;
        QTop.SQL.Text := 'INSERT INTO topics VALUES (:id, 0, :subject, :order, NULL, '''')';
        QTop.Prepare;
        QCon.DataBase := Conn;
        QCon.SQL.Text := 'INSERT INTO content VALUES (:id, :data, NULL)';
        QCon.Prepare;
        SetCfg('type', '1');
        SetCfg('id', 'sword.' + LowerCase(Conf.Name));
        SetCfg('title', Conf.Get('Description', Conf.Name));
        SetCfg('abbrev', Conf.Name);
        SetCfg('description', Conf.Get('Description', Conf.Name));
        SetCfg('content.type', 'rtf');
        SetCfg('schema.version', '1');
        if Conf.Get('Lang') <> '' then
          SetCfg('lang', Conf.Get('Lang'));
        if Conf.Get('DistributionLicense') <> '' then
          SetCfg('about', RtfEscape(Conf.Get('DistributionLicense') + ' ' + Conf.Get('Copyright')));
        if StrongsLetter <> '' then
        begin
          SetCfg('strong', '1');
          if StrongsLetter = 'G' then SetCfg('strong.g', '1') else SetCfg('strong.h', '1');
        end
        else if (Pos('morph', LowerCase(Conf.Name)) > 0) or
          (Pos('robinson', LowerCase(Conf.Name)) > 0) or
          (Pos('packard', LowerCase(Conf.Name)) > 0) then
          SetCfg('morph', '1');
        TopicID := 0;
        Count := 0;
        for I := 0 to Keys.Count - 1 do
        begin
          Text := EntryText(I);
          // an entry that only points at another one (@LINK key)
          K := 0;
          while (Pos('@LINK', Trim(Text)) = 1) and (K < 5) do
          begin
            Key := Trim(Copy(Trim(Text), 6, MaxInt));
            Text := '';
            for NL := 0 to Keys.Count - 1 do
              if SameText(Keys[NL], Key) then
              begin
                Text := EntryText(NL);
                Break;
              end;
            Inc(K);
          end;
          if Trim(Text) = '' then
            Continue;
          Subject := Keys[I];
          if (StrongsLetter <> '') and (StrToIntDef(Subject, -1) >= 0) then
            Subject := StrongsLetter + IntToStr(StrToInt(Subject));
          Rtf := MarkupToRtf(Text);
          Inc(TopicID);
          QTop.Params.ParamByName('id').AsInteger := TopicID;
          QTop.Params.ParamByName('subject').AsString := Subject;
          QTop.Params.ParamByName('order').AsInteger := TopicID;
          QTop.ExecSQL;
          QCon.Params.ParamByName('id').AsInteger := TopicID;
          QCon.Params.ParamByName('data').AsString := Rtf;
          QCon.ExecSQL;
          Inc(Count);
        end;
      finally
        QCfg.Free;
        QTop.Free;
        QCon.Free;
      end;
      Tr.Commit;
    finally
      Conn.Close;
      Tr.Free;
      Conn.Free;
    end;
    Result := Count > 0;
    if not Result then
      ErrorMessage := SSwordNoEntries;
  except
    on Ex: Exception do
    begin
      Err := Ex.Message;
      ErrorMessage := Err;
      Result := False;
    end;
  end;
  Texts.Free;
  Keys.Free;
  Conf.Free;
end;

end.
