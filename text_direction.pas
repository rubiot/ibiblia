unit text_direction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTextDirection = (
    tdLeftToRight,
    tdRightToLeft
  );

function TextDirectionToConfigValue(Direction: TTextDirection): string;
function TextDirectionFromConfigValue(const Value: string;
  DefaultDirection: TTextDirection = tdLeftToRight): TTextDirection;
function ParseTheWordR2LProperties(const Properties: string): TTextDirection;
function NormalizeTheWordR2LProperties(const Properties: string;
  Direction: TTextDirection): string;

implementation

function TryReadR2LProperty(const Line: string; out Value: string): Boolean;
var
  CommentPosition: Integer;
  EqualsPosition: Integer;
  PropertyName: string;
begin
  Result := False;
  Value := '';
  EqualsPosition := Pos('=', Line);
  if EqualsPosition = 0 then
    Exit;

  PropertyName := Trim(Copy(Line, 1, EqualsPosition - 1));
  if not SameText(PropertyName, 'r2l') then
    Exit;

  Value := Copy(Line, EqualsPosition + 1, MaxInt);
  CommentPosition := Pos('#', Value);
  if CommentPosition > 0 then
    Value := Copy(Value, 1, CommentPosition - 1);
  Value := Trim(Value);
  Result := True;
end;

function DetectLineBreak(const Text: string): string;
begin
  if Pos(#13#10, Text) > 0 then
    Exit(#13#10);
  if Pos(#10, Text) > 0 then
    Exit(#10);
  if Pos(#13, Text) > 0 then
    Exit(#13);
  Result := LineEnding;
end;

function EndsWithLineBreak(const Text: string): Boolean;
begin
  Result := (Text <> '') and (Text[Length(Text)] in [#10, #13]);
end;

function JoinPropertyLines(Lines: TStrings; const LineBreak: string;
  PreserveTrailingLineBreak: Boolean): string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Lines.Count - 1 do
  begin
    if Index > 0 then
      Result := Result + LineBreak;
    Result := Result + Lines[Index];
  end;
  if PreserveTrailingLineBreak and (Lines.Count > 0) then
    Result := Result + LineBreak;
end;

function TextDirectionToConfigValue(Direction: TTextDirection): string;
begin
  case Direction of
    tdRightToLeft: Result := 'rtl';
  else
    Result := 'ltr';
  end;
end;

function TextDirectionFromConfigValue(const Value: string;
  DefaultDirection: TTextDirection): TTextDirection;
var
  NormalizedValue: string;
begin
  NormalizedValue := Trim(Value);
  if NormalizedValue = '' then
    Exit(DefaultDirection);
  if SameText(NormalizedValue, 'rtl') then
    Exit(tdRightToLeft);
  if SameText(NormalizedValue, 'ltr') then
    Exit(tdLeftToRight);
  Result := DefaultDirection;
end;

function ParseTheWordR2LProperties(const Properties: string): TTextDirection;
var
  Index: Integer;
  Lines: TStringList;
  Value: string;
begin
  Result := tdLeftToRight;
  Lines := TStringList.Create;
  try
    Lines.Text := Properties;
    for Index := 0 to Lines.Count - 1 do
      if TryReadR2LProperty(Lines[Index], Value) and (Value <> '') then
        Exit(tdRightToLeft);
  finally
    Lines.Free;
  end;
end;

function NormalizeTheWordR2LProperties(const Properties: string;
  Direction: TTextDirection): string;
var
  Index: Integer;
  InputLines: TStringList;
  LineBreak: string;
  OutputLines: TStringList;
  PreserveTrailingLineBreak: Boolean;
  Value: string;
begin
  LineBreak := DetectLineBreak(Properties);
  PreserveTrailingLineBreak := EndsWithLineBreak(Properties);
  InputLines := TStringList.Create;
  OutputLines := TStringList.Create;
  try
    InputLines.Text := Properties;
    for Index := 0 to InputLines.Count - 1 do
      if not TryReadR2LProperty(InputLines[Index], Value) then
        OutputLines.Add(InputLines[Index]);

    if Direction = tdRightToLeft then
      OutputLines.Add('r2l=1');

    Result := JoinPropertyLines(OutputLines, LineBreak,
      PreserveTrailingLineBreak);
  finally
    OutputLines.Free;
    InputLines.Free;
  end;
end;

end.
