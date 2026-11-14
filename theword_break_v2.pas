unit theword_break_v2;

{$mode objfpc}{$H+}

interface

type
  TTheWordBreakKind = (
    twbkParagraph,      // <CM> spaced paragraph
    twbkSoftLine,       // <CL> new line in the same paragraph
    twbkTightParagraph  // <CI> paragraph without the extra space
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
{ True for the zero-width break lexemes <CM>, <CI> and <CL>. }
function IsTheWordBreakLexeme(const ALexeme: UnicodeString): Boolean;
{ True for the paragraph kinds (<CM> and <CI>). }
function IsTheWordParagraphBreak(AKind: TTheWordBreakKind): Boolean;

implementation

function TheWordBreakLexeme(AKind: TTheWordBreakKind): UnicodeString;
begin
  case AKind of
    twbkParagraph: Result := '<CM>';
    twbkSoftLine: Result := '<CL>';
    twbkTightParagraph: Result := '<CI>';
  else
    Result := '';
  end;
end;

function TheWordBreakVisualText(AKind: TTheWordBreakKind): UnicodeString;
begin
  case AKind of
    twbkParagraph: Result := UnicodeString(WideChar($00B6));
    twbkSoftLine: Result := UnicodeString(WideChar($21B5)) + #10;
    twbkTightParagraph: Result := UnicodeString(WideChar($204B)); // reversed pilcrow
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
  if ALexeme = '<CI>' then
  begin
    AKind := twbkTightParagraph;
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

function IsTheWordBreakLexeme(const ALexeme: UnicodeString): Boolean;
begin
  Result := (ALexeme = '<CM>') or (ALexeme = '<CI>') or (ALexeme = '<CL>');
end;

function IsTheWordParagraphBreak(AKind: TTheWordBreakKind): Boolean;
begin
  Result := AKind in [twbkParagraph, twbkTightParagraph];
end;

end.
