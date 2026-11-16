unit app_updater;

{ Self-update engine for iBiblia (Windows and Linux).

  Flow: ask GitHub for the latest release -> pick the asset for this platform
  -> download it -> verify its SHA-256 against the release's SHA256SUMS.txt ->
  unpack into a staging folder -> swap the files into the install folder.

  The swap renames each existing file to "<name>.old-update" before the new one
  is copied in. Renaming a file that is currently running or loaded is allowed
  on both Windows and Linux, so the update can be installed while iBiblia is
  open; leftovers are deleted at the next start. If anything fails half-way the
  renamed files are put back. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  UpdateRepoOwner = 'SalitaNgDiyos';
  UpdateRepoName = 'ibiblia';
  UpdateReleasesPage = 'https://github.com/SalitaNgDiyos/ibiblia/releases';
  UpdateOldSuffix = '.old-update';
  {$IFDEF WINDOWS}
  UpdateAssetSuffix = '-windows-x64.zip';
  {$ELSE}
  UpdateAssetSuffix = '-linux-x86_64.zip';
  {$ENDIF}
  UpdateSumsAsset = 'SHA256SUMS.txt';

type
  TUpdateInfo = record
    Available: Boolean;
    Version: string;      // e.g. 1.1.0.027
    Tag: string;
    Notes: string;        // release body (markdown/plain)
    PageUrl: string;
    AssetName: string;
    AssetUrl: string;
    AssetSize: Int64;
    SumsUrl: string;
  end;

  TUpdateProgressEvent = procedure(ADone, ATotal: Int64) of object;

{ Version of the running program (from the executable's version resource). }
function CurrentAppVersion: string;

{ -1 if A < B, 0 if equal, 1 if A > B. Compares dotted numbers; text is ignored. }
function CompareVersions(const A, B: string): Integer;

{ "v.1.1.0.27" -> "1.1.0.027" (for display). }
function NormalizeVersion(const S: string): string;

{ True when this build may update itself; Reason says why not otherwise. }
function UpdatesSupported(out Reason: string): Boolean;

{ Contact GitHub. Returns False (with Err) if the check itself failed; when it
  succeeds Info.Available tells whether a newer release exists. }
function CheckForUpdate(out Info: TUpdateInfo; out Err: string): Boolean;

{ Download + verify + unpack. StageDir receives the folder holding the new files. }
function DownloadAndStage(const Info: TUpdateInfo; AProgress: TUpdateProgressEvent;
  out StageDir, Err: string): Boolean;

{ Copy the staged files over the install folder (with rollback). }
function ApplyStagedUpdate(const StageDir: string; out Err: string): Boolean;

{ Start a fresh copy of the program with the original command line. }
function LaunchNewInstance: Boolean;

{ Delete "*.old-update" files left behind by a previous update. }
procedure CleanupOldUpdateFiles;

{ Linux: make sure iBiblia is listed in the applications menu (writes
  ~/.local/share/applications/ibiblia.desktop when it is missing or points to
  another folder). Does nothing on Windows, in a system install under /usr, or
  when IBIBLIA_NO_MENU is set. }
procedure EnsureLinuxMenuEntry;

{ Remove a staging folder and the downloaded archive next to it. }
procedure DiscardStagedUpdate(const StageDir: string);

function Sha256OfFile(const FileName: string): string;
function Sha256OfString(const S: string): string;

implementation

uses
  fileinfo, elfreader, {$IFDEF WINDOWS}winpeimagereader,{$ENDIF}
  fpjson, jsonparser, zipper, process, LazFileUtils, FileUtil,
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  tls_runtime;

{ ---------------------------------------------------------------- SHA-256 -- }

type
  TSha256Ctx = record
    State: array[0..7] of LongWord;
    Buf: array[0..63] of Byte;
    BufLen: Integer;
    TotalLen: QWord;
  end;

const
  K256: array[0..63] of LongWord = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
    $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
    $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
    $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
    $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

function Ror(X: LongWord; N: Integer): LongWord; inline;
begin
  Result := (X shr N) or (X shl (32 - N));
end;

procedure Sha256Init(var C: TSha256Ctx);
begin
  C.State[0] := $6a09e667; C.State[1] := $bb67ae85; C.State[2] := $3c6ef372; C.State[3] := $a54ff53a;
  C.State[4] := $510e527f; C.State[5] := $9b05688c; C.State[6] := $1f83d9ab; C.State[7] := $5be0cd19;
  C.BufLen := 0;
  C.TotalLen := 0;
end;

procedure Sha256Block(var C: TSha256Ctx; const P: PByte);
var
  W: array[0..63] of LongWord;
  a, b, cc, d, e, f, g, h, t1, t2: LongWord;
  i: Integer;
begin
  for i := 0 to 15 do
    W[i] := (LongWord(P[i * 4]) shl 24) or (LongWord(P[i * 4 + 1]) shl 16) or
            (LongWord(P[i * 4 + 2]) shl 8) or LongWord(P[i * 4 + 3]);
  for i := 16 to 63 do
    W[i] := (Ror(W[i - 2], 17) xor Ror(W[i - 2], 19) xor (W[i - 2] shr 10)) + W[i - 7] +
            (Ror(W[i - 15], 7) xor Ror(W[i - 15], 18) xor (W[i - 15] shr 3)) + W[i - 16];
  a := C.State[0]; b := C.State[1]; cc := C.State[2]; d := C.State[3];
  e := C.State[4]; f := C.State[5]; g := C.State[6]; h := C.State[7];
  for i := 0 to 63 do
  begin
    t1 := h + (Ror(e, 6) xor Ror(e, 11) xor Ror(e, 25)) + ((e and f) xor ((not e) and g)) + K256[i] + W[i];
    t2 := (Ror(a, 2) xor Ror(a, 13) xor Ror(a, 22)) + ((a and b) xor (a and cc) xor (b and cc));
    h := g; g := f; f := e; e := d + t1; d := cc; cc := b; b := a; a := t1 + t2;
  end;
  Inc(C.State[0], a); Inc(C.State[1], b); Inc(C.State[2], cc); Inc(C.State[3], d);
  Inc(C.State[4], e); Inc(C.State[5], f); Inc(C.State[6], g); Inc(C.State[7], h);
end;

procedure Sha256Update(var C: TSha256Ctx; const Data; Len: PtrInt);
var
  P: PByte;
  N: Integer;
begin
  P := @Data;
  Inc(C.TotalLen, Len);
  while Len > 0 do
  begin
    if (C.BufLen = 0) and (Len >= 64) then
    begin
      Sha256Block(C, P);
      Inc(P, 64);
      Dec(Len, 64);
    end
    else
    begin
      N := 64 - C.BufLen;
      if N > Len then
        N := Len;
      Move(P^, C.Buf[C.BufLen], N);
      Inc(C.BufLen, N);
      Inc(P, N);
      Dec(Len, N);
      if C.BufLen = 64 then
      begin
        Sha256Block(C, @C.Buf[0]);
        C.BufLen := 0;
      end;
    end;
  end;
end;

function Sha256Final(var C: TSha256Ctx): string;
var
  Bits: QWord;
  Pad: array[0..127] of Byte;
  PadLen, i: Integer;
begin
  Bits := C.TotalLen * 8;
  FillChar(Pad, SizeOf(Pad), 0);
  Pad[0] := $80;
  if C.BufLen < 56 then
    PadLen := 56 - C.BufLen
  else
    PadLen := 120 - C.BufLen;
  Sha256Update(C, Pad, PadLen);
  for i := 0 to 7 do
    Pad[i] := Byte(Bits shr (56 - 8 * i));
  Sha256Update(C, Pad, 8);
  Result := '';
  for i := 0 to 7 do
    Result := Result + LowerCase(IntToHex(C.State[i], 8));
end;

function Sha256OfString(const S: string): string;
var
  C: TSha256Ctx;
begin
  Sha256Init(C);
  if Length(S) > 0 then
    Sha256Update(C, S[1], Length(S));
  Result := Sha256Final(C);
end;

function Sha256OfFile(const FileName: string): string;
var
  C: TSha256Ctx;
  F: TFileStream;
  Buf: array[0..65535] of Byte;
  N: Integer;
begin
  Sha256Init(C);
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    repeat
      N := F.Read(Buf, SizeOf(Buf));
      if N > 0 then
        Sha256Update(C, Buf, N);
    until N <= 0;
  finally
    F.Free;
  end;
  Result := Sha256Final(C);
end;

{ ---------------------------------------------------------------- versions -- }

function CurrentAppVersion: string;
var
  Info: TFileVersionInfo;
begin
  Result := '';
  try
    Info := TFileVersionInfo.Create(nil);
    try
      Info.ReadFileInfo;
      Result := Info.VersionStrings.Values['FileVersion'];
    finally
      Info.Free;
    end;
  except
    Result := '';
  end;
end;

{ Splits "v.1.1.0.027" / "1.1.0.27-beta" into numbers. }
procedure ParseVersion(const S: string; out Parts: array of Integer);
var
  i, k, N: Integer;
  Started: Boolean;
begin
  for k := 0 to High(Parts) do
    Parts[k] := 0;
  k := 0;
  N := -1;
  Started := False;
  for i := 1 to Length(S) + 1 do
  begin
    if (i <= Length(S)) and (S[i] in ['0'..'9']) then
    begin
      Started := True;
      if N < 0 then
        N := 0;
      if N < 100000 then
        N := N * 10 + Ord(S[i]) - Ord('0');
    end
    else
    begin
      if N >= 0 then
      begin
        if k <= High(Parts) then
          Parts[k] := N;
        Inc(k);
        N := -1;
      end;
      // anything other than '.' after the number ends the version ("-beta")
      if Started and ((i > Length(S)) or (S[i] <> '.')) then
        Break;
    end;
  end;
end;

function CompareVersions(const A, B: string): Integer;
var
  PA, PB: array[0..3] of Integer;
  i: Integer;
begin
  ParseVersion(A, PA);
  ParseVersion(B, PB);
  Result := 0;
  for i := 0 to 3 do
    if PA[i] <> PB[i] then
    begin
      if PA[i] < PB[i] then
        Result := -1
      else
        Result := 1;
      Exit;
    end;
end;

function NormalizeVersion(const S: string): string;
var
  P: array[0..3] of Integer;
begin
  ParseVersion(S, P);
  Result := Format('%d.%d.%d.%.3d', [P[0], P[1], P[2], P[3]]);
end;

{ ------------------------------------------------------------- environment -- }

function InstallDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(0))));
end;

function WorkRoot: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'iBiblia-update-' +
    {$IFDEF UNIX}IntToStr(FpGetUid){$ELSE}'user'{$ENDIF};
end;

function UpdatesSupported(out Reason: string): Boolean;
var
  Probe: string;
  H: TFileStream;
begin
  Result := False;
  Reason := '';
  if GetEnvironmentVariable('IBIBLIA_NO_UPDATE') <> '' then
  begin
    Reason := 'Updates are disabled by IBIBLIA_NO_UPDATE.';
    Exit;
  end;
  if (CompareVersions(CurrentAppVersion, '1.1.0.0') <= 0) then
  begin
    Reason := 'This build has no version number, so it cannot be compared with releases.';
    Exit;
  end;
  Probe := InstallDir + '.update-write-test';
  try
    H := TFileStream.Create(Probe, fmCreate);
    H.Free;
    DeleteFile(Probe);
  except
    Reason := 'The program folder is not writable (' + InstallDir +
      '). Download the new version from ' + UpdateReleasesPage + ' instead.';
    Exit;
  end;
  Result := True;
end;

{ ---------------------------------------------------------------- network -- }

function NewClient: TIBibliaHTTPSClient;
begin
  Result := TIBibliaHTTPSClient.CreateForRuntime(nil, InstallDir);
  Result.AllowRedirect := True;
  Result.ConnectTimeout := 15000;
  Result.IOTimeout := 30000;
  Result.AddHeader('User-Agent', 'iBiblia-updater/' + CurrentAppVersion);
  Result.AddHeader('Accept', 'application/vnd.github+json');
end;

type
  TDownloadTracker = class
    OnProgress: TUpdateProgressEvent;
    procedure DataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
  end;

procedure TDownloadTracker.DataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  if Assigned(OnProgress) then
    OnProgress(CurrentPos, ContentLength);
end;

{ IBIBLIA_UPDATE_FEED lets a test point the updater at a local copy of the
  GitHub "latest release" JSON; normally it is the real releases feed. }
function FeedUrl: string;
begin
  Result := GetEnvironmentVariable('IBIBLIA_UPDATE_FEED');
  if Result = '' then
    Result := 'https://api.github.com/repos/' + UpdateRepoOwner + '/' +
      UpdateRepoName + '/releases/latest';
end;

function CheckForUpdate(out Info: TUpdateInfo; out Err: string): Boolean;
var
  Client: TIBibliaHTTPSClient;
  Body: string;
  Root, A: TJSONData;
  Assets: TJSONArray;
  i: Integer;
  Name: string;
begin
  Result := False;
  Err := '';
  Initialize(Info);
  Info.Available := False;
  try
    Client := NewClient;
    try
      Body := Client.Get(FeedUrl);
    finally
      Client.Free;
    end;
    Root := GetJSON(Body);
    try
      if Root.JSONType <> jtObject then
        raise Exception.Create('Unexpected reply from GitHub.');
      Info.Tag := TJSONObject(Root).Get('tag_name', '');
      Info.Version := NormalizeVersion(Info.Tag);
      Info.Notes := TJSONObject(Root).Get('body', '');
      Info.PageUrl := TJSONObject(Root).Get('html_url', UpdateReleasesPage);
      Assets := TJSONObject(Root).Get('assets', TJSONArray(nil));
      if Assigned(Assets) then
        for i := 0 to Assets.Count - 1 do
        begin
          A := Assets[i];
          if A.JSONType <> jtObject then
            Continue;
          Name := TJSONObject(A).Get('name', '');
          if (Length(Name) > Length(UpdateAssetSuffix)) and
             SameText(Copy(Name, Length(Name) - Length(UpdateAssetSuffix) + 1, MaxInt), UpdateAssetSuffix) then
          begin
            Info.AssetName := Name;
            Info.AssetUrl := TJSONObject(A).Get('browser_download_url', '');
            Info.AssetSize := TJSONObject(A).Get('size', Int64(0));
          end
          else if SameText(Name, UpdateSumsAsset) then
            Info.SumsUrl := TJSONObject(A).Get('browser_download_url', '');
        end;
    finally
      Root.Free;
    end;
    Info.Available := (Info.Tag <> '') and (Info.AssetUrl <> '') and
      (CompareVersions(Info.Version, CurrentAppVersion) > 0);
    Result := True;
  except
    on E: Exception do
      Err := E.Message;
  end;
end;

{ Finds "<hash>  <name>" in a sha256sum-style listing. }
function HashFromSums(const Sums, FileName: string): string;
var
  L: TStringList;
  i, p: Integer;
  Line, Nm: string;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.Text := Sums;
    for i := 0 to L.Count - 1 do
    begin
      Line := Trim(L[i]);
      p := Pos(' ', Line);
      if p < 65 then
        Continue;
      Nm := Trim(Copy(Line, p + 1, MaxInt));
      if (Nm <> '') and (Nm[1] = '*') then
        Delete(Nm, 1, 1);
      if SameText(Nm, FileName) then
      begin
        Result := LowerCase(Copy(Line, 1, p - 1));
        Exit;
      end;
    end;
  finally
    L.Free;
  end;
end;

function UnsafeArchiveName(const S: string): Boolean;
var
  N: string;
begin
  N := StringReplace(S, '\', '/', [rfReplaceAll]);
  Result := (N = '') or (N[1] = '/') or (Pos('..', N) > 0) or ((Length(N) > 1) and (N[2] = ':'));
end;

function DownloadAndStage(const Info: TUpdateInfo; AProgress: TUpdateProgressEvent;
  out StageDir, Err: string): Boolean;
var
  Client: TIBibliaHTTPSClient;
  Tracker: TDownloadTracker;
  Root, Zip, Sums, Want, Got: string;
  F: TFileStream;
  UnZ: TUnZipper;
  i: Integer;
begin
  Result := False;
  Err := '';
  StageDir := '';
  Tracker := nil;
  try
    if (Info.AssetUrl = '') or (Info.SumsUrl = '') then
      raise Exception.Create('The release has no ' + UpdateSumsAsset +
        ' or no package for this platform, so it cannot be verified.');
    Root := IncludeTrailingPathDelimiter(WorkRoot) + Info.Version + PathDelim;
    if DirectoryExists(Root) then
      DeleteDirectory(Root, False);
    ForceDirectories(Root);
    Zip := Root + Info.AssetName;
    StageDir := Root + 'files' + PathDelim;

    // checksum list first (small), then the package
    Client := NewClient;
    Tracker := TDownloadTracker.Create;
    try
      Sums := Client.Get(Info.SumsUrl);
      Client.OnDataReceived := @Tracker.DataReceived;
      Tracker.OnProgress := AProgress;
      F := TFileStream.Create(Zip, fmCreate);
      try
        Client.Get(Info.AssetUrl, F);
      finally
        F.Free;
      end;
    finally
      Client.Free;
    end;

    Want := HashFromSums(Sums, Info.AssetName);
    if Want = '' then
      raise Exception.Create('No checksum for ' + Info.AssetName + ' in ' + UpdateSumsAsset + '.');
    Got := Sha256OfFile(Zip);
    if not SameText(Want, Got) then
      raise Exception.Create('Download is corrupt (checksum mismatch); it was discarded.');

    ForceDirectories(StageDir);
    UnZ := TUnZipper.Create;
    try
      UnZ.FileName := Zip;
      UnZ.OutputPath := StageDir;
      UnZ.Examine;
      for i := 0 to UnZ.Entries.Count - 1 do
        if UnsafeArchiveName(UnZ.Entries[i].ArchiveFileName) then
          raise Exception.Create('Package contains an unsafe path: ' + UnZ.Entries[i].ArchiveFileName);
      UnZ.UnZipAllFiles;
    finally
      UnZ.Free;
    end;

    {$IFDEF WINDOWS}
    if not FileExists(StageDir + 'iBiblia.exe') then
    {$ELSE}
    if not FileExists(StageDir + 'iBiblia') then
    {$ENDIF}
      raise Exception.Create('The package does not contain the iBiblia program.');
    Result := True;
  except
    on E: Exception do
    begin
      Err := E.Message;
      if StageDir <> '' then
        DiscardStagedUpdate(StageDir);
      StageDir := '';
    end;
  end;
  Tracker.Free;
end;

procedure DiscardStagedUpdate(const StageDir: string);
var
  Root: string;
begin
  if StageDir = '' then
    Exit;
  Root := ExtractFilePath(ExcludeTrailingPathDelimiter(StageDir));
  if (Pos('iBiblia-update-', Root) > 0) then
    DeleteDirectory(ExcludeTrailingPathDelimiter(Root), False);
end;

{ ------------------------------------------------------------------ apply -- }

procedure ListFiles(const Base, Sub: string; Into: TStrings);
var
  SR: TSearchRec;
begin
  if FindFirst(Base + Sub + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      if (SR.Attr and faDirectory) <> 0 then
        ListFiles(Base, Sub + SR.Name + PathDelim, Into)
      else
        Into.Add(Sub + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

function ApplyStagedUpdate(const StageDir: string; out Err: string): Boolean;
var
  Files, Moved, Created: TStringList;
  i: Integer;
  Rel, Dest, Old, Src, Inst: string;
begin
  Result := False;
  Err := '';
  Inst := InstallDir;
  Files := TStringList.Create;
  Moved := TStringList.Create;    // renamed originals
  Created := TStringList.Create;  // new files written
  try
    ListFiles(StageDir, '', Files);
    if Files.Count = 0 then
    begin
      Err := 'Nothing to install.';
      Exit;
    end;
    try
      for i := 0 to Files.Count - 1 do
      begin
        Rel := Files[i];
        // never touch the user's settings or projects
        if SameText(ExtractFileName(Rel), 'iBiblia.ini') or
           SameText(ExtractFileExt(Rel), '.bib') then
          Continue;
        Src := StageDir + Rel;
        Dest := Inst + Rel;
        ForceDirectories(ExtractFilePath(Dest));
        if FileExists(Dest) then
        begin
          Old := Dest + UpdateOldSuffix;
          if FileExists(Old) then
            DeleteFile(Old);
          if not RenameFile(Dest, Old) then
            raise Exception.Create('Cannot replace ' + Rel);
          Moved.Add(Rel);
        end;
        if not CopyFile(Src, Dest, [cffOverwriteFile]) then
          raise Exception.Create('Cannot write ' + Rel);
        Created.Add(Rel);
        {$IFDEF UNIX}
        if (ExtractFileName(Rel) = 'iBiblia') or SameText(ExtractFileExt(Rel), '.sh') then
          FpChmod(Dest, &755);
        {$ENDIF}
      end;
      Result := True;
    except
      on E: Exception do
      begin
        Err := E.Message;
        // roll back: remove what we wrote, restore what we moved aside
        for i := Created.Count - 1 downto 0 do
          DeleteFile(Inst + Created[i]);
        for i := Moved.Count - 1 downto 0 do
          RenameFile(Inst + Moved[i] + UpdateOldSuffix, Inst + Moved[i]);
      end;
    end;
  finally
    Created.Free;
    Moved.Free;
    Files.Free;
  end;
end;

procedure CleanupDir(const Dir: string; Depth: Integer);
var
  SR: TSearchRec;
begin
  if FindFirst(Dir + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      if (SR.Attr and faDirectory) <> 0 then
      begin
        if Depth > 0 then
          CleanupDir(Dir + SR.Name + PathDelim, Depth - 1);
      end
      else if SameText(ExtractFileExt(SR.Name), UpdateOldSuffix) then
        DeleteFile(Dir + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure CleanupOldUpdateFiles;
begin
  try
    CleanupDir(InstallDir, 2);
  except
    // best effort only
  end;
end;

procedure EnsureLinuxMenuEntry;
{$IFDEF UNIX}
var
  Exe, Icon, Dir, FileName, Wanted, Current: string;
  L: TStringList;
begin
  try
    if GetEnvironmentVariable('IBIBLIA_NO_MENU') <> '' then Exit;
    Exe := ExpandFileName(ParamStr(0));
    if (Copy(Exe, 1, 5) = '/usr/') or (Copy(Exe, 1, 5) = '/opt/') then Exit;
    if GetEnvironmentVariable('HOME') = '' then Exit;
    Dir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
      '.local/share/applications';
    FileName := Dir + '/ibiblia.desktop';
    Icon := ExtractFilePath(Exe) + 'iBiblia.png';
    Wanted := 'Exec="' + Exe + '" %f';
    if FileExists(FileName) then
    begin
      L := TStringList.Create;
      try
        L.LoadFromFile(FileName);
        Current := L.Values['Exec'];
        if ('Exec=' + Current = Wanted) or (Pos(Exe, L.Text) > 0) then Exit;
      finally
        L.Free;
      end;
    end;
    if not ForceDirectories(Dir) then Exit;
    L := TStringList.Create;
    try
      L.Add('[Desktop Entry]');
      L.Add('Type=Application');
      L.Add('Name=iBiblia');
      L.Add('Comment=Associate Strong''s numbers and morphology between Bible texts');
      L.Add(Wanted);
      if FileExists(Icon) then L.Add('Icon=' + Icon);
      L.Add('Terminal=false');
      L.Add('Categories=Education;Office;');
      L.Add('MimeType=application/x-ibiblia-project;');
      L.SaveToFile(FileName);
    finally
      L.Free;
    end;
  except
    // best effort only
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

function LaunchNewInstance: Boolean;
var
  P: TProcess;
  i: Integer;
begin
  Result := False;
  try
    P := TProcess.Create(nil);
    try
      {$IFDEF UNIX}
      { Wait (up to 20 s) until this process has really exited before the new
        copy starts: while the old one is still running it holds iBiblia.ini
        and the new one fails with "Unable to open file ... Try again". }
      P.Executable := '/bin/sh';
      P.Parameters.Add('-c');
      P.Parameters.Add('p=$1; shift; n=0; while kill -0 "$p" 2>/dev/null && ' +
        '[ "$n" -lt 100 ]; do sleep 0.2; n=$((n+1)); done; exec "$@"');
      P.Parameters.Add('ibiblia-restart');
      P.Parameters.Add(IntToStr(fpGetPid));
      P.Parameters.Add(ExpandFileName(ParamStr(0)));
      {$ELSE}
      P.Executable := ExpandFileName(ParamStr(0));
      {$ENDIF}
      for i := 1 to ParamCount do
        P.Parameters.Add(ParamStr(i));
      P.CurrentDirectory := InstallDir;
      P.Options := [poNoConsole];
      P.Execute;
      Result := True;
    finally
      P.Free;
    end;
  except
    Result := False;
  end;
end;

end.
