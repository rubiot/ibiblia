unit twAutomate;

interface

type
  TBCV_A = packed record
    case Integer of
      0: (span: byte; vi: byte; ci: byte; bi: byte);
      1: (val: Cardinal);
  end;

  TCopyData_Op_GotoVerse = packed record
    bcv: TBCV_A;
  end;

  //the phrase array should be zero-filled: when theWord receives theWord
  //it treates it as a PWideChar so it needs the terminating WideChar(0) to
  //know where the string ends
  TCopyData_Op_DctWordLookup = packed record
    phrase: array[1..255] of WideChar;
  end;

  TWAutomateUtils = class
    class function IsTwRunning: THandle;
  end;
const

  //the classname of the main theWord window.
  MAINFORM_CLASSNAME = 'theWord.0f2ba8a0-906d-11e1-b0c4-0800200c9a66';

  //the value of COPYDATASTRUCT.dwData
  COPYDATA_OP_FIRST = $ffff0000;

  //Operation go-to-verse
  COPYDATA_OP_GOTOVERSE = COPYDATA_OP_FIRST + 1;
  COPYDATA_OP_DCTWORDLOOKUP = COPYDATA_OP_FIRST + 2;

implementation
uses Windows;

{------------------------------------------------------------------------------}
{ Return 0 if tw is not running, else the HWND of the running instance }
class function TWAutomateUtils.IsTwRunning: THandle;
var
  PrevWindow: THandle;
begin
  PrevWindow := FindWindow(MAINFORM_CLASSNAME + '.UnicodeClass', nil);
  if IsWindow(PrevWindow) then begin
    Result := PrevWindow
  end
  else begin
    Result := 0;
  end;
end;

end.
