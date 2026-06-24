unit VerseMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType, SynEdit, SynEditKeyCmds,
  {$IFNDEF UNIX}
  Windows,
  {$ENDIF}
  Dialogs, Verse, Math, theword_syntax_highlighter;

type

  TVerseMemo = class;
  TVerseTextModifiedEvent = procedure (Sender: TVerseMemo) of object;

  TVerseMemo = class
  private
    FModified: Boolean;
    FMemo: TSynEdit;
    FContainer: TScrollBox;
    FVerse: TVerse;
    FOnVerseTextModified: TVerseTextModifiedEvent;
    function GetText: TCaption;
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ApplyTagToSelection(const StartTag, EndTag: string);
    function HandleEditorCommandKey(var Key: Word; Shift: TShiftState): Boolean;
  protected
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnChange(Sender: TObject);
    procedure ConfirmChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Activate(Container: TScrollBox; Verse: TVerse);
    procedure Deactivate;
    property Text: TCaption read GetText;
    property Verse: TVerse read FVerse;
    property OnVerseTextModified: TVerseTextModifiedEvent read FOnVerseTextModified write FOnVerseTextModified;
  end;

resourcestring
  SChangeVerse = 'Change verse text?';
  SChangeVerseConfirmation = 'Are you sure you want to change the verse text? Some associations may be lost.';

implementation

function RemoveFinalLineBreak(const Value: string): string;
begin
  Result := Value;
  if (Length(Result) >= 2) and
    (Copy(Result, Length(Result) - 1, 2) = #13#10) then
    SetLength(Result, Length(Result) - 2)
  else if (Length(Result) >= 1) and
    (Result[Length(Result)] in [#10, #13]) then
    SetLength(Result, Length(Result) - 1);
end;

function TVerseMemo.GetText: TCaption;
begin
  Result := RemoveFinalLineBreak(FMemo.Text);
end;

procedure TVerseMemo.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    FMemo.Font.Size := FMemo.Font.Size + Sign(WheelDelta);
end;

procedure TVerseMemo.ApplyTagToSelection(const StartTag, EndTag: string);
var
  SelectedText: string;
begin
  SelectedText := FMemo.SelText;
  if SelectedText <> '' then
    FMemo.SelText := StartTag + SelectedText + EndTag;
end;

function TVerseMemo.HandleEditorCommandKey(var Key: Word;
  Shift: TShiftState): Boolean;
var
  Command: TSynEditorCommand;
  BasicShift: TShiftState;
begin
  Result := False;

  if not Assigned(FMemo) or FMemo.ReadOnly then
    Exit;

  BasicShift := Shift * [ssShift, ssCtrl];
  if Shift <> BasicShift then
    Exit;

  Command := ecNone;
  case Key of
    VK_BACK:
      if (BasicShift = []) or (BasicShift = [ssShift]) then
        Command := ecDeleteLastChar
      else if BasicShift = [ssCtrl] then
        Command := ecDeleteLastWord;
    VK_DELETE:
      if BasicShift = [] then
        Command := ecDeleteChar
      else if BasicShift = [ssShift] then
        Command := ecCut;
    VK_C:
      if BasicShift = [ssCtrl] then
        Command := ecCopy;
    VK_V:
      if BasicShift = [ssCtrl] then
        Command := ecPaste;
    VK_X:
      if BasicShift = [ssCtrl] then
        Command := ecCut;
    VK_LEFT:
      if BasicShift = [] then
        Command := ecLeft
      else if BasicShift = [ssShift] then
        Command := ecSelLeft
      else if BasicShift = [ssCtrl] then
        Command := ecWordLeft
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelWordLeft;
    VK_RIGHT:
      if BasicShift = [] then
        Command := ecRight
      else if BasicShift = [ssShift] then
        Command := ecSelRight
      else if BasicShift = [ssCtrl] then
        Command := ecWordRight
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelWordRight;
    VK_UP:
      if BasicShift = [] then
        Command := ecUp
      else if BasicShift = [ssShift] then
        Command := ecSelUp;
    VK_DOWN:
      if BasicShift = [] then
        Command := ecDown
      else if BasicShift = [ssShift] then
        Command := ecSelDown;
    VK_HOME:
      if BasicShift = [] then
        Command := ecLineStart
      else if BasicShift = [ssShift] then
        Command := ecSelLineStart
      else if BasicShift = [ssCtrl] then
        Command := ecEditorTop
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelEditorTop;
    VK_END:
      if BasicShift = [] then
        Command := ecLineEnd
      else if BasicShift = [ssShift] then
        Command := ecSelLineEnd
      else if BasicShift = [ssCtrl] then
        Command := ecEditorBottom
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelEditorBottom;
  end;

  Result := Command <> ecNone;
  if Result then
  begin
    FMemo.CommandProcessor(Command, #0, nil);
    Key := 0;
  end;
end;

procedure TVerseMemo.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (ssShift in Shift) and
    (Key in [VK_I, VK_E, VK_O]) then
  begin
    case Key of
      VK_I:
        begin
          ApplyTagToSelection('<FI>', '<Fi>');
          Key := 0;
        end;
      VK_E:
        begin
          ApplyTagToSelection('<FR>', '<Fr>');
          Key := 0;
        end;
      VK_O:
        begin
          ApplyTagToSelection('<FO>', '<Fo>');
          Key := 0;
        end;
    end;
  end
  else if Key = VK_ESCAPE then
  begin
    ConfirmChange;
  end
  else if Key = VK_RETURN then
    Key := 0
  else
    HandleEditorCommandKey(Key, Shift);
end;

procedure TVerseMemo.OnChange(Sender: TObject);
begin
  if Assigned(TSynEdit(Sender).Parent) and TSynEdit(Sender).Visible then
    FModified := True;
end;

procedure TVerseMemo.ConfirmChange;
begin
  {
  if FModified then
  begin
    case MessageDlg(SChangeVerse, SChangeVerseConfirmation, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if Assigned(FOnVerseTextModified) then
          FOnVerseTextModified(Self);
      mrCancel: exit;
    end;
  end;
  }
  Deactivate;
end;

constructor TVerseMemo.Create;
begin
  FMemo := TSynEdit.Create(nil);
  FMemo.Visible := False;
  FMemo.Align := alNone;
  ConfigureTheWordSyntaxEditor(FMemo, False);
  FMemo.OnKeyDown := @OnKeyDown;
  FMemo.OnChange := @OnChange;
  FMemo.OnMouseWheel := @OnMouseWheel;
  FModified := False;
  FContainer := nil;
  FVerse := nil;
  FOnVerseTextModified := nil;
end;

destructor TVerseMemo.Destroy;
begin
  FMemo.Free;
  inherited Destroy;
end;

procedure TVerseMemo.Activate(Container: TScrollBox; Verse: TVerse);
begin
  if FMemo.Visible and FModified and (FContainer <> Container) then
  begin
    ConfirmChange;
    Exit;
  end;

  ApplyTheWordSyntaxEditorFont(FMemo);
  FModified := False;
  FContainer := Container;
  FVerse := Verse;
  FMemo.Parent := nil;
  FMemo.Text := Verse.XML;

  if Assigned(Container.Parent) then
  begin
    FMemo.Align := alNone;
    FMemo.Parent := Container.Parent;
    FMemo.SetBounds(Container.Left, Container.Top, Container.Width,
      Container.Height);
    FMemo.Anchors := Container.Anchors;
  end
  else
  begin
    FMemo.Align := alClient;
    FMemo.Parent := Container;
  end;

  FMemo.BringToFront;
  FMemo.Visible := True;
  FMemo.SetFocus;
end;

procedure TVerseMemo.Deactivate;
begin
  if not FMemo.Visible then
    Exit;

  FMemo.Visible := False;
  if Assigned(FOnVerseTextModified) and FModified then
    FOnVerseTextModified(Self);
  FContainer := nil;
end;

end.
