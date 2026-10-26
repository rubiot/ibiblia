unit unittexttospeech;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  IniFiles,
  {$IFDEF WINDOWS}
  comobj, Variants,
  {$ENDIF}
  LCLType;

type
  TTTSTriggerMode = (ttsmHover, ttsmCtrlHover, ttsmAltHover);

  { TTTSManager }

  TTTSManager = class
  private
    FEnabled: Boolean;
    FSourceVoiceIndex: Integer;
    FDestVoiceIndex: Integer;
    FSourceVoiceName: string;
    FDestVoiceName: string;
    FRate: Integer;
    FVolume: Integer;
    FTriggerMode: TTTSTriggerMode;
    FVoiceList: TStringList;
    FLastSpokenWord: string;
    {$IFDEF WINDOWS}
    FSpVoice: OleVariant;
    FOneCoreCat: OleVariant;
    FVoiceTokens: array of OleVariant;
    FCurrentVoiceIndex: Integer;
    FInitialized: Boolean;
    {$ENDIF}
    procedure SetEnabled(AValue: Boolean);
    procedure SetSourceVoiceIndex(AValue: Integer);
    procedure SetDestVoiceIndex(AValue: Integer);
    function GetVoiceList: TStringList;
    procedure ReconcileVoiceNames;
    {$IFDEF WINDOWS}
    procedure InitSAPI;
    procedure PopulateVoiceList;
    procedure SetVoiceByIndex(AIndex: Integer);
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Speak(const AText: string; AIsSource: Boolean = False);
    procedure SpeakSyntagm(const AText: string; AIsSource: Boolean = False);
    procedure Stop;
    procedure TestVoice(AIsSource: Boolean);
    procedure SpeakTest(AVoiceIndex: Integer; const AText: string; ARate, AVolume: Integer);
    function CleanWord(const AText: string): string;
    procedure LoadFromIni(AIni: TIniFile);
    procedure SaveToIni(AIni: TIniFile);

    property Enabled: Boolean read FEnabled write SetEnabled;
    property SourceVoiceIndex: Integer read FSourceVoiceIndex write SetSourceVoiceIndex;
    property DestVoiceIndex: Integer read FDestVoiceIndex write SetDestVoiceIndex;
    property SourceVoiceName: string read FSourceVoiceName write FSourceVoiceName;
    property DestVoiceName: string read FDestVoiceName write FDestVoiceName;
    property Rate: Integer read FRate write FRate;
    property Volume: Integer read FVolume write FVolume;
    property TriggerMode: TTTSTriggerMode read FTriggerMode write FTriggerMode;
    property VoiceList: TStringList read GetVoiceList;
  end;

var
  TTSManager: TTTSManager;

procedure InitTTS;
procedure FreeTTS;

implementation

procedure InitTTS;
begin
  if TTSManager = nil then
    TTSManager := TTTSManager.Create;
end;

procedure FreeTTS;
begin
  if TTSManager <> nil then
  begin
    TTSManager.Free;
    TTSManager := nil;
  end;
end;

{ TTTSManager }

procedure TTTSManager.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
  {$IFDEF WINDOWS}
  if FEnabled and (not FInitialized) then
    InitSAPI;
  {$ENDIF}
end;

function TTTSManager.GetVoiceList: TStringList;
begin
  {$IFDEF WINDOWS}
  if not FInitialized then
    InitSAPI;
  {$ENDIF}
  Result := FVoiceList;
end;

procedure TTTSManager.ReconcileVoiceNames;
var
  i: Integer;
begin
  if FSourceVoiceName <> '' then
  begin
    for i := 0 to FVoiceList.Count - 1 do
      if FVoiceList[i] = FSourceVoiceName then
      begin
        FSourceVoiceIndex := i;
        break;
      end;
  end;

  if FDestVoiceName <> '' then
  begin
    for i := 0 to FVoiceList.Count - 1 do
      if FVoiceList[i] = FDestVoiceName then
      begin
        FDestVoiceIndex := i;
        break;
      end;
  end;
end;

procedure TTTSManager.SetSourceVoiceIndex(AValue: Integer);
begin
  FSourceVoiceIndex := AValue;
  if (AValue >= 0) and (AValue < FVoiceList.Count) then
    FSourceVoiceName := FVoiceList[AValue];
end;

procedure TTTSManager.SetDestVoiceIndex(AValue: Integer);
begin
  FDestVoiceIndex := AValue;
  if (AValue >= 0) and (AValue < FVoiceList.Count) then
    FDestVoiceName := FVoiceList[AValue];
end;

constructor TTTSManager.Create;
begin
  inherited Create;
  FEnabled := False;
  FSourceVoiceIndex := 0;
  FDestVoiceIndex := 0;
  FSourceVoiceName := '';
  FDestVoiceName := '';
  FRate := 0;
  FVolume := 100;
  FTriggerMode := ttsmHover;
  FVoiceList := TStringList.Create;
  FLastSpokenWord := '';

  {$IFDEF WINDOWS}
  FInitialized := False;
  // Lazy SAPI initialization: InitSAPI is called only when TTS is enabled or VoiceList is requested
  {$ENDIF}
end;

destructor TTTSManager.Destroy;
begin
  Stop;
  FVoiceList.Free;
  {$IFDEF WINDOWS}
  SetLength(FVoiceTokens, 0);
  if FInitialized then
  begin
    try
      FSpVoice := Unassigned;
    except
      // ignore
    end;
  end;
  {$ENDIF}
  inherited Destroy;
end;

{$IFDEF WINDOWS}
procedure TTTSManager.InitSAPI;
var
  i: Integer;
  vName: string;
begin
  if FInitialized then exit;
  try
    FSpVoice := CreateOleObject('SAPI.SpVoice');
    FInitialized := True;
    PopulateVoiceList;

    // Auto-detect Greek or Hebrew voice for source original text
    for i := 0 to FVoiceList.Count - 1 do
    begin
      vName := LowerCase(FVoiceList[i]);
      if (Pos('greek', vName) > 0) or (Pos('el-gr', vName) > 0) or
         (Pos('stefanos', vName) > 0) or (Pos('athina', vName) > 0) or
         (Pos('kalypso', vName) > 0) or (Pos('hebrew', vName) > 0) or
         (Pos('asaf', vName) > 0) or (Pos('he-il', vName) > 0) then
      begin
        FSourceVoiceIndex := i;
        FSourceVoiceName := FVoiceList[i];
        break;
      end;
    end;

    // Auto-detect English voice for destination translation text
    if FDestVoiceName = '' then
    begin
      for i := 0 to FVoiceList.Count - 1 do
      begin
        vName := LowerCase(FVoiceList[i]);
        if (Pos('david', vName) > 0) or (Pos('zira', vName) > 0) or
           (Pos('english', vName) > 0) or (Pos('en-us', vName) > 0) then
        begin
          FDestVoiceIndex := i;
          FDestVoiceName := FVoiceList[i];
          break;
        end;
      end;
    end;

    ReconcileVoiceNames;
  except
    FInitialized := False;
  end;
end;

procedure TTTSManager.PopulateVoiceList;
var
  tokens, oneCoreTokens: OleVariant;
  cat: OleVariant;
  i, count: Integer;
  desc: string;
  tokenObj: OleVariant;
begin
  FVoiceList.Clear;
  SetLength(FVoiceTokens, 0);
  if not FInitialized then exit;

  // 1. Enumerate standard SAPI 5 Desktop voices
  try
    tokens := FSpVoice.GetVoices;
    count := tokens.Count;
    for i := 0 to count - 1 do
    begin
      tokenObj := tokens.Item(i);
      desc := tokenObj.GetDescription;
      FVoiceList.Add(desc);
      SetLength(FVoiceTokens, Length(FVoiceTokens) + 1);
      FVoiceTokens[High(FVoiceTokens)] := tokenObj;
    end;
  except
    // ignore
  end;

  // 2. Enumerate modern Windows 10/11 OneCore voices (Greek, Hebrew, and other installed language packs)
  try
    cat := CreateOleObject('SAPI.SpObjectTokenCategory');
    cat.SetId('HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Speech_OneCore\Voices', False);
    oneCoreTokens := cat.EnumerateTokens;
    count := oneCoreTokens.Count;
    for i := 0 to count - 1 do
    begin
      tokenObj := oneCoreTokens.Item(i);
      desc := tokenObj.GetDescription;
      if FVoiceList.IndexOf(desc) < 0 then
      begin
        FVoiceList.Add(desc);
        SetLength(FVoiceTokens, Length(FVoiceTokens) + 1);
        FVoiceTokens[High(FVoiceTokens)] := tokenObj;
      end;
    end;
  except
    // ignore
  end;

  // 3. Fallback for WOW6432Node (32-bit / 64-bit registry redirection)
  try
    cat := CreateOleObject('SAPI.SpObjectTokenCategory');
    cat.SetId('HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\Speech_OneCore\Voices', False);
    oneCoreTokens := cat.EnumerateTokens;
    count := oneCoreTokens.Count;
    for i := 0 to count - 1 do
    begin
      tokenObj := oneCoreTokens.Item(i);
      desc := tokenObj.GetDescription;
      if FVoiceList.IndexOf(desc) < 0 then
      begin
        FVoiceList.Add(desc);
        SetLength(FVoiceTokens, Length(FVoiceTokens) + 1);
        FVoiceTokens[High(FVoiceTokens)] := tokenObj;
      end;
    end;
  except
    // ignore
  end;
end;

procedure TTTSManager.SetVoiceByIndex(AIndex: Integer);
begin
  if not FInitialized then exit;

  try
    if (AIndex >= 0) and (AIndex < Length(FVoiceTokens)) then
      FSpVoice.Voice := FVoiceTokens[AIndex];
  except
    // ignore
  end;
end;
{$ENDIF}

function TTTSManager.CleanWord(const AText: string): string;
var
  i: Integer;
  wCode: Word;
  w: WideString;
  res: WideString;
begin
  w := UTF8Decode(AText);
  res := '';
  for i := 1 to Length(w) do
  begin
    wCode := Word(w[i]);
    // Skip ASCII control characters and punctuation
    if (wCode <= 47) or ((wCode >= 58) and (wCode <= 64)) or
       ((wCode >= 91) and (wCode <= 96)) or ((wCode >= 123) and (wCode <= 126)) then
      Continue;
    // Skip common Unicode typography: guillemets, Greek ano teleia, dashes, smart quotes
    if (wCode = $00AB) or (wCode = $00BB) or (wCode = $00B7) or
       ((wCode >= $2010) and (wCode <= $2027)) or (wCode = $22C5) then
      Continue;

    res := res + w[i];
  end;
  Result := UTF8Encode(res);
end;

procedure TTTSManager.SpeakTest(AVoiceIndex: Integer; const AText: string; ARate, AVolume: Integer);
var
  wText: WideString;
begin
  if AText = '' then exit;

  {$IFDEF WINDOWS}
  if not FInitialized then
    InitSAPI;

  if not FInitialized then exit;

  try
    SetVoiceByIndex(AVoiceIndex);
    FSpVoice.Rate := ARate;
    FSpVoice.Volume := AVolume;

    wText := UTF8Decode(AText);
    // SVSFlagsAsync = 1, SVSFPurgeBeforeSpeak = 2 -> 3
    FSpVoice.Speak(wText, 3);
  except
    // ignore
  end;
  {$ENDIF}
end;

procedure TTTSManager.Speak(const AText: string; AIsSource: Boolean);
var
  wText: WideString;
  targetIdx: Integer;
  targetName: string;
  i: Integer;
begin
  if not FEnabled or (AText = '') then exit;

  {$IFDEF WINDOWS}
  if not FInitialized then
    InitSAPI;

  if not FInitialized then exit;

  try
    if AIsSource then
    begin
      targetIdx := FSourceVoiceIndex;
      targetName := FSourceVoiceName;
    end
    else
    begin
      targetIdx := FDestVoiceIndex;
      targetName := FDestVoiceName;
    end;

    // If targetIdx is out of bounds, try finding it by targetName
    if (targetIdx < 0) or (targetIdx >= FVoiceList.Count) then
    begin
      if targetName <> '' then
      begin
        for i := 0 to FVoiceList.Count - 1 do
          if FVoiceList[i] = targetName then
          begin
            targetIdx := i;
            break;
          end;
      end;
    end;

    // Ensure valid index within bounds
    if (targetIdx < 0) or (targetIdx >= FVoiceList.Count) then
      targetIdx := 0;

    SetVoiceByIndex(targetIdx);
    FSpVoice.Rate := FRate;
    FSpVoice.Volume := FVolume;

    wText := UTF8Decode(AText);
    // SVSFlagsAsync = 1, SVSFPurgeBeforeSpeak = 2 -> 1 or 2 = 3
    FSpVoice.Speak(wText, 3);
  except
    // Non-fatal
  end;
  {$ENDIF}
end;

procedure TTTSManager.SpeakSyntagm(const AText: string; AIsSource: Boolean);
var
  clean: string;
begin
  clean := CleanWord(AText);
  if (clean = '') or (clean = FLastSpokenWord) then
    exit;

  FLastSpokenWord := clean;
  Speak(clean, AIsSource);
end;

procedure TTTSManager.Stop;
begin
  {$IFDEF WINDOWS}
  if FInitialized then
  begin
    try
      // SVSFPurgeBeforeSpeak = 2
      FSpVoice.Speak('', 2);
    except
      // ignore
    end;
  end;
  {$ENDIF}
  FLastSpokenWord := '';
end;

procedure TTTSManager.TestVoice(AIsSource: Boolean);
begin
  if AIsSource then
    Speak('Ἐν ἀρχῇ ἦν ὁ λόγος', True)
  else
    Speak('In the beginning was the Word', False);
end;

procedure TTTSManager.LoadFromIni(AIni: TIniFile);
begin
  if AIni = nil then exit;
  FEnabled := AIni.ReadBool('tts', 'enabled', false);
  FTriggerMode := TTTSTriggerMode(AIni.ReadInteger('tts', 'triggermode', Integer(ttsmHover)));
  FSourceVoiceName := AIni.ReadString('tts', 'sourcevoicename', FSourceVoiceName);
  FDestVoiceName := AIni.ReadString('tts', 'destvoicename', FDestVoiceName);
  FSourceVoiceIndex := AIni.ReadInteger('tts', 'sourcevoiceindex', FSourceVoiceIndex);
  FDestVoiceIndex := AIni.ReadInteger('tts', 'destvoiceindex', FDestVoiceIndex);
  FRate := AIni.ReadInteger('tts', 'rate', 0);
  FVolume := AIni.ReadInteger('tts', 'volume', 100);

  {$IFDEF WINDOWS}
  if FEnabled and (not FInitialized) then
    InitSAPI
  else if FInitialized then
    ReconcileVoiceNames;
  {$ELSE}
  ReconcileVoiceNames;
  {$ENDIF}
end;

procedure TTTSManager.SaveToIni(AIni: TIniFile);
begin
  if AIni = nil then exit;
  AIni.WriteBool('tts', 'enabled', FEnabled);
  AIni.WriteInteger('tts', 'triggermode', Integer(FTriggerMode));
  AIni.WriteInteger('tts', 'sourcevoiceindex', FSourceVoiceIndex);
  AIni.WriteInteger('tts', 'destvoiceindex', FDestVoiceIndex);
  AIni.WriteString('tts', 'sourcevoicename', FSourceVoiceName);
  AIni.WriteString('tts', 'destvoicename', FDestVoiceName);
  AIni.WriteInteger('tts', 'rate', FRate);
  AIni.WriteInteger('tts', 'volume', FVolume);
  AIni.UpdateFile;
end;

end.
