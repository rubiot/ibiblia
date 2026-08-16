unit verse_visual_adapter;

{$mode objfpc}{$H+}

interface

uses
  Graphics, ONTTokenizer, Syntagm, verse_visual_document;

function BuildVerseVisualDocument(Syntagms: TSyntagmList;
  StrongCount: TSyntagm; BaseDirection: TTextDirection; Revision: QWord;
  AssociatedColor, UnassociatedColor: TColor;
  BoldWordsWithStrong: Boolean): IVerseVisualDocument;

implementation

uses
  SysUtils;

function SyntagmStyle(Syntagm: TSyntagm; AssociatedColor,
  UnassociatedColor: TColor; BoldWordsWithStrong: Boolean): TVisualTextStyle;
var
  Foreground: TColor;
begin
  Result := DefaultVisualTextStyle;
  if Syntagm.IsItalic then
    Include(Result.Flags, vtsfItalic);
  if Syntagm.IsSuperscript then
    Include(Result.Flags, vtsfSuperscript);
  if BoldWordsWithStrong and Syntagm.HasStrongs then
    Include(Result.Flags, vtsfBold);
  if Syntagm.IsSelected then
  begin
    Include(Result.Flags, vtsfSelected);
    Result.HasBackgroundColor := True;
    Result.BackgroundColor := LongInt(clYellow);
  end;
  if Syntagm.IsAssociated then
    Include(Result.Flags, vtsfAssociated);
  if Syntagm.IsPointed then
  begin
    Include(Result.Flags, vtsfPointed);
    Include(Result.Flags, vtsfUnderline);
    if not Syntagm.IsSelected then
    begin
      Result.HasBackgroundColor := True;
      Result.BackgroundColor := LongInt(clHighlight);
    end;
  end;
  if Syntagm.IsStrongHighlighted then
  begin
    Include(Result.Flags, vtsfStrongHighlight);
    Include(Result.Flags, vtsfUnderline);
  end;

  if Syntagm.TextColor <> clDefault then
    Foreground := Syntagm.TextColor
  else if Syntagm.IsAssociated then
    Foreground := AssociatedColor
  else
    Foreground := UnassociatedColor;
  Result.HasForegroundColor := True;
  Result.ForegroundColor := LongInt(Foreground);
end;

procedure AddSyntagm(Builder: TVerseVisualDocumentBuilder; LogicalIndex: Integer;
  Syntagm: TSyntagm; AssociatedColor, UnassociatedColor: TColor;
  BoldWordsWithStrong: Boolean);
var
  Annotation: UnicodeString;
  DisplayText: string;
begin
  if not Assigned(Syntagm) then
  begin
    Builder.AddHidden(LogicalIndex);
    Exit;
  end;

  if Syntagm.Text = '<FI>' then
    DisplayText := '['
  else if Syntagm.Text = '<Fi>' then
    DisplayText := ']'
  else if Syntagm.Kind in [skSyntagm, skWhitespace, skPunctuation,
    skStrongCount] then
    DisplayText := Syntagm.Text
  else
  begin
    Builder.AddMarkup(LogicalIndex, UTF8Decode(Syntagm.Text));
    Exit;
  end;

  Annotation := '';
  if Syntagm.HasStrongs then
    Annotation := UTF8Decode(Syntagm.Strong.CommaText);

  Builder.AddText(LogicalIndex, UTF8Decode(DisplayText),
    SyntagmStyle(Syntagm, AssociatedColor, UnassociatedColor,
      BoldWordsWithStrong),
    Syntagm.Kind in [skSyntagm, skPunctuation],
    Annotation);
end;

function BuildVerseVisualDocument(Syntagms: TSyntagmList;
  StrongCount: TSyntagm; BaseDirection: TTextDirection; Revision: QWord;
  AssociatedColor, UnassociatedColor: TColor;
  BoldWordsWithStrong: Boolean): IVerseVisualDocument;
var
  Builder: TVerseVisualDocumentBuilder;
  Index: Integer;
begin
  Builder := TVerseVisualDocumentBuilder.Create(BaseDirection);
  try
    if Assigned(Syntagms) then
      for Index := 0 to Syntagms.Count - 1 do
        AddSyntagm(Builder, Index, Syntagms[Index], AssociatedColor,
          UnassociatedColor, BoldWordsWithStrong);
    if Assigned(StrongCount) then
      if Assigned(Syntagms) then
        AddSyntagm(Builder, Syntagms.Count, StrongCount, AssociatedColor,
          UnassociatedColor, BoldWordsWithStrong)
      else
        AddSyntagm(Builder, 0, StrongCount, AssociatedColor,
          UnassociatedColor, BoldWordsWithStrong);
    Result := Builder.Build(Revision);
  finally
    Builder.Free;
  end;
end;

end.
