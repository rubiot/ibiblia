unit ai_debug;

{$mode objfpc}{$H+}

{ Debug utilities for AI flows }

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF};

function IsAIDebugEnabled: Boolean;
function IsAIDebugVerbose: Boolean;
procedure AIDebugLog(const Msg: string);
procedure AIDebugLogFmt(const Fmt: string; const Args: array of const);
procedure AIDebugLogVerbose(const Msg: string);
procedure AIDebugLogVerboseFmt(const Fmt: string; const Args: array of const);

implementation

var
  GAIDebugEnabled: Boolean = True;
  GAIDebugVerbose: Boolean = False;
  GAIDebugChecked: Boolean = False;

function IsAIDebugEnabled: Boolean;
var
  EnvValue: string;
begin
  if not GAIDebugChecked then
  begin
    EnvValue := SysUtils.GetEnvironmentVariable('IBIBLIA_AI_DEBUG');
    GAIDebugEnabled := (EnvValue = '1') or (UpperCase(EnvValue) = 'TRUE') or (UpperCase(EnvValue) = 'YES');

    EnvValue := SysUtils.GetEnvironmentVariable('IBIBLIA_AI_DEBUG_VERBOSE');
    GAIDebugVerbose := (EnvValue = '1') or (UpperCase(EnvValue) = 'TRUE') or (UpperCase(EnvValue) = 'YES');

    GAIDebugChecked := True;
  end;
  Result := GAIDebugEnabled;
end;

function IsAIDebugVerbose: Boolean;
begin
  if not GAIDebugChecked then IsAIDebugEnabled;
  Result := GAIDebugVerbose;
end;

procedure AIDebugLog(const Msg: string);
begin
  if IsAIDebugEnabled then
  begin
    {$IFDEF WINDOWS}
    OutputDebugString(PChar('AI_DEBUG: ' + Msg));
    {$ELSE}
    WriteLn('AI_DEBUG: ', Msg);
    {$ENDIF}
  end;
end;

procedure AIDebugLogFmt(const Fmt: string; const Args: array of const);
begin
  if IsAIDebugEnabled then
    AIDebugLog(Format(Fmt, Args));
end;

procedure AIDebugLogVerbose(const Msg: string);
begin
  if IsAIDebugVerbose then
    AIDebugLog(Msg);
end;

procedure AIDebugLogVerboseFmt(const Fmt: string; const Args: array of const);
begin
  if IsAIDebugVerbose then
    AIDebugLog(Format(Fmt, Args));
end;

end.
