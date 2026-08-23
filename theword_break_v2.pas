unit theword_break_v2;

{$mode objfpc}{$H+}

interface

type
  TTheWordBreakKind = (
    twbkParagraph,
    twbkSoftLine
  );

  { A break is a zero-width ONT object. Offset alone is not an identity:
    several CM/CL objects may share the same UTF-16 anchor, so the imported
    event ordinal is carried through the visual layer and edit transaction. }
  TTheWordBreakIdentity = record
    Kind: TTheWordBreakKind;
    OffsetUTF16: Integer;
    Ordinal: Integer;
  end;

  TTheWordBreakIdentityArray = array of TTheWordBreakIdentity;

function TheWordBreakLexeme(AKind: TTheWordBreakKind): UnicodeString;
function TheWordBreakVisualText(AKind: TTheWordBreakKind): UnicodeString;
function TryTheWordBreakKindFromLexeme(const ALexeme: UnicodeString;
  out AKind: TTheWordBreakKind): Boolean;
function SameTheWordBreak(const ALeft,
  ARight: TTheWordBreakIdentity): Boolean;

implementation

function TheWordBreakLexeme(AKind: TTheWordBreakKind): UnicodeString;
begin
  case AKind of
    twbkParagraph: Result := '<CM>';
    twbkSoftLine: Result := '<CL>';
  else
    Result := '';
  end;
end;

function TheWordBreakVisualText(AKind: TTheWordBreakKind): UnicodeString;
begin
  case AKind of
    twbkParagraph: Result := UnicodeString(WideChar($00B6));
    twbkSoftLine: Result := UnicodeString(WideChar($21B5)) + #10;
  else
    Result := '';
  end;
end;

function TryTheWordBreakKindFromLexeme(const ALexeme: UnicodeString;
  out AKind: TTheWordBreakKind): Boolean;
begin
  if ALexeme = '<CM>' then
  begin
    AKind := twbkParagraph;
    Exit(True);
  end;
  if ALexeme = '<CL>' then
  begin
    AKind := twbkSoftLine;
    Exit(True);
  end;
  AKind := twbkParagraph;
  Result := False;
end;

function SameTheWordBreak(const ALeft,
  ARight: TTheWordBreakIdentity): Boolean;
begin
  Result := (ALeft.Kind = ARight.Kind) and
    (ALeft.OffsetUTF16 = ARight.OffsetUTF16) and
    (ALeft.Ordinal = ARight.Ordinal);
end;

end.
