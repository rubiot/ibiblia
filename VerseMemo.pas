unit VerseMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Forms, LCLType,
  {$IFNDEF UNIX}
  Windows,
  {$ENDIF}
  Dialogs, Verse, Math;

type

  TVerseMemo = class;
  TVerseTextModifiedEvent = procedure (Sender: TVerseMemo) of object;

  TVerseMemo = class
  private
    FModified: Boolean;
    FMemo: TMemo;
    FVerse: TVerse;
    FOnVerseTextModified: TVerseTextModifiedEvent;
    function GetText: TCaption;
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ApplyTagToSelection(const StartTag, EndTag: string);
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

function TVerseMemo.GetText: TCaption;
begin
  Result := FMemo.Text;
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

procedure TVerseMemo.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (ssShift in Shift) then
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
    FVerse.Font.Size := FMemo.Font.Size;
  end
  else if Key = VK_RETURN then
    Key := 0;
end;

procedure TVerseMemo.OnChange(Sender: TObject);
begin
  if Assigned(TMemo(Sender).Parent) and TMemo(Sender).Visible then
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
  FMemo := TMemo.Create(nil);
  FMemo.Visible := False;
  FMemo.Align := alClient;
  FMemo.ScrollBars := ssAutoVertical;
  FMemo.OnKeyDown := @OnKeyDown;
  FMemo.OnChange := @OnChange;
  FMemo.OnMouseWheel := @OnMouseWheel;
  FMemo.ParentFont := False;
  FMemo.Font.Name := 'Courier New';
  FModified := False;
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
  if FMemo.Visible and FModified and (FMemo.Parent <> Container) then
  begin
    ConfirmChange;
    Exit;
  end;

  FMemo.Font.Size := Verse.Font.Size;
  FModified := False;
  FVerse := Verse;
  FMemo.Parent := nil;
  FMemo.Text := Verse.XML;
  FMemo.Parent := Container;
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
end;

end.