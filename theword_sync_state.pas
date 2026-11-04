unit theword_sync_state;

{$mode objfpc}{$H+}

interface

uses
  IniFiles;

const
  THEWORD_SYNC_INI_SECTION = 'opcoes';
  THEWORD_SYNC_IBIBLIA_TO_THEWORD_KEY = 'synctheword';
  THEWORD_SYNC_THEWORD_TO_IBIBLIA_KEY = 'syncibiblia';

type
  TTheWordSyncState = (
    twssDisabled,
    twssIBibliaToTheWord,
    twssTheWordToIBiblia,
    twssBidirectional
  );

function ResolveTheWordSyncState(AIBibliaToTheWord,
  ATheWordToIBiblia: Boolean): TTheWordSyncState;
procedure ExpandTheWordSyncState(AState: TTheWordSyncState;
  out AIBibliaToTheWord, ATheWordToIBiblia: Boolean);
function ReadTheWordSyncState(AIni: TCustomIniFile): TTheWordSyncState;
procedure WriteTheWordSyncState(AIni: TCustomIniFile;
  AState: TTheWordSyncState);

implementation

function ResolveTheWordSyncState(AIBibliaToTheWord,
  ATheWordToIBiblia: Boolean): TTheWordSyncState;
begin
  if AIBibliaToTheWord then
  begin
    if ATheWordToIBiblia then
      Result := twssBidirectional
    else
      Result := twssIBibliaToTheWord;
  end
  else if ATheWordToIBiblia then
    Result := twssTheWordToIBiblia
  else
    Result := twssDisabled;
end;

procedure ExpandTheWordSyncState(AState: TTheWordSyncState;
  out AIBibliaToTheWord, ATheWordToIBiblia: Boolean);
begin
  AIBibliaToTheWord := AState in
    [twssIBibliaToTheWord, twssBidirectional];
  ATheWordToIBiblia := AState in
    [twssTheWordToIBiblia, twssBidirectional];
end;

function ReadTheWordSyncState(AIni: TCustomIniFile): TTheWordSyncState;
begin
  Result := ResolveTheWordSyncState(
    AIni.ReadBool(THEWORD_SYNC_INI_SECTION,
      THEWORD_SYNC_IBIBLIA_TO_THEWORD_KEY, False),
    AIni.ReadBool(THEWORD_SYNC_INI_SECTION,
      THEWORD_SYNC_THEWORD_TO_IBIBLIA_KEY, False));
end;

procedure WriteTheWordSyncState(AIni: TCustomIniFile;
  AState: TTheWordSyncState);
var
  IBibliaToTheWord: Boolean;
  TheWordToIBiblia: Boolean;
begin
  ExpandTheWordSyncState(AState, IBibliaToTheWord, TheWordToIBiblia);
  AIni.WriteBool(THEWORD_SYNC_INI_SECTION,
    THEWORD_SYNC_IBIBLIA_TO_THEWORD_KEY, IBibliaToTheWord);
  AIni.WriteBool(THEWORD_SYNC_INI_SECTION,
    THEWORD_SYNC_THEWORD_TO_IBIBLIA_KEY, TheWordToIBiblia);
end;

end.
