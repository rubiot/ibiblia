unit FormGoToVerse;

{ Go to verse: a small lookup box, like theWord's. Type a reference such as
  "jn 3 16", "1co 13.4" or "ps 23" and press Enter to jump there. The line
  under the box shows what the text is understood as while typing.
  The parsing lives in verse_lookup.pas. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, LCLType,
  ui_style, Project;

type
  TGoToVerseNavigateEvent = procedure(const ReferenceID: string) of object;

  { TFrmGoToVerse }

  TFrmGoToVerse = class(TForm)
  private
    FProject: TProject;
    FOnNavigate: TGoToVerseNavigateEvent;
    FTargetID: string;
    FPromptLabel: TLabel;
    FEdit: TEdit;
    FResultLabel: TLabel;
    FExamplesLabel: TLabel;
    procedure BuildControls;
    procedure RefreshCaptions;
    procedure UpdatePreview;
    procedure EditChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetProject(AProject: TProject);
    procedure StartTyping(const InitialText: string);
    property OnNavigate: TGoToVerseNavigateEvent read FOnNavigate write FOnNavigate;
  end;

{ Shows the lookup box for AProject. InitialText is put in the box with the
  cursor after it (used when the box is opened by typing a letter). }
procedure ShowGoToVerse(AOwner: TComponent; AProject: TProject;
  ANavigate: TGoToVerseNavigateEvent; const InitialText: string);
{ Call when the open project changes or closes (nil). }
procedure GoToVerseProjectChanged(AProject: TProject);

implementation

uses
  verse_lookup;

resourcestring
  SGoToTitle = 'Go to verse';
  SGoToPrompt = 'Type a &reference and press Enter:';
  SGoToExamples = 'Examples:  jn 3 16    1co 13.4    ps 23    3 16    :16';
  SGoToWaiting = 'A space or a period works as the colon.';
  SGoToResultFmt = 'Go to:  %s %d:%d';
  SGoToUnknownBook = 'No book matches what you typed.';
  SGoToBookNotInProjectFmt = '%s is not in this project.';
  SGoToNoChapterFmt = '%s has no chapter %d.';
  SGoToNoVerseFmt = '%s %d has no verse %d.';

var
  GoToInstance: TFrmGoToVerse = nil;

procedure ShowGoToVerse(AOwner: TComponent; AProject: TProject;
  ANavigate: TGoToVerseNavigateEvent; const InitialText: string);
begin
  if AProject = nil then
    Exit;
  if GoToInstance = nil then
    GoToInstance := TFrmGoToVerse.Create(AOwner);
  GoToInstance.OnNavigate := ANavigate;
  GoToInstance.SetProject(AProject);
  GoToInstance.Show;
  GoToInstance.BringToFront;
  GoToInstance.StartTyping(InitialText);
end;

procedure GoToVerseProjectChanged(AProject: TProject);
begin
  if GoToInstance = nil then
    Exit;
  GoToInstance.SetProject(AProject);
  if AProject = nil then
    GoToInstance.Hide;
end;

{ TFrmGoToVerse }

constructor TFrmGoToVerse.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  ApplyFormStyle(Self);
  FTargetID := '';
  Position := poMainFormCenter;
  BorderStyle := bsToolWindow;
  FormStyle := fsStayOnTop;
  ClientWidth := 420;
  ClientHeight := 124;
  BuildControls;
  RefreshCaptions;
end;

destructor TFrmGoToVerse.Destroy;
begin
  if GoToInstance = Self then
    GoToInstance := nil;
  inherited Destroy;
end;

procedure TFrmGoToVerse.BuildControls;
begin
  FPromptLabel := TLabel.Create(Self);
  FPromptLabel.Parent := Self;
  FPromptLabel.SetBounds(12, 10, 396, 18);

  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.SetBounds(12, 32, 396, 28);
  FEdit.OnChange := @EditChange;
  FEdit.OnKeyDown := @EditKeyDown;
  FPromptLabel.FocusControl := FEdit;

  FResultLabel := TLabel.Create(Self);
  FResultLabel.Parent := Self;
  FResultLabel.SetBounds(12, 68, 396, 20);
  FResultLabel.Font.Style := [fsBold];

  FExamplesLabel := TLabel.Create(Self);
  FExamplesLabel.Parent := Self;
  FExamplesLabel.SetBounds(12, 96, 396, 18);
end;

procedure TFrmGoToVerse.RefreshCaptions;
begin
  Caption := SGoToTitle;
  FPromptLabel.Caption := SGoToPrompt;
  FExamplesLabel.Caption := SGoToExamples;
  UpdatePreview;
end;

procedure TFrmGoToVerse.SetProject(AProject: TProject);
begin
  FProject := AProject;
  UpdatePreview;
end;

procedure TFrmGoToVerse.StartTyping(const InitialText: string);
begin
  FEdit.Text := InitialText;
  if FEdit.CanFocus then
    FEdit.SetFocus;
  { Cursor after the text, so the next key continues it. }
  FEdit.SelStart := Length(FEdit.Text);
  FEdit.SelLength := 0;
  UpdatePreview;
end;

procedure TFrmGoToVerse.UpdatePreview;
var
  BookID, Chapter, Verse: Integer;
  BookName: string;
begin
  FTargetID := '';
  if (FProject = nil) or (FEdit = nil) or (FResultLabel = nil) then
    Exit;

  case ParseVerseLookup(FEdit.Text, FProject.Escopo, FProject.BookID,
    FProject.Chapter, BookID, Chapter, Verse) of
    vlsOk:
      begin
        FTargetID := Format('%d,%d,%d', [BookID, Chapter, Verse]);
        FResultLabel.Caption := Format(SGoToResultFmt,
          [VerseLookupBookName(BookID), Chapter, Verse]);
      end;
    vlsUnknownBook:
      FResultLabel.Caption := SGoToUnknownBook;
    vlsBookNotInProject:
      begin
        BookName := VerseLookupBookName(BookID);
        FResultLabel.Caption := Format(SGoToBookNotInProjectFmt, [BookName]);
      end;
    vlsNoSuchChapter:
      begin
        BookName := VerseLookupBookName(BookID);
        FResultLabel.Caption := Format(SGoToNoChapterFmt, [BookName, Chapter]);
      end;
    vlsNoSuchVerse:
      begin
        BookName := VerseLookupBookName(BookID);
        FResultLabel.Caption := Format(SGoToNoVerseFmt,
          [BookName, Chapter, Verse]);
      end;
  else
    FResultLabel.Caption := SGoToWaiting;
  end;
end;

procedure TFrmGoToVerse.EditChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TFrmGoToVerse.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Target: string;
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    UpdatePreview;
    if FTargetID = '' then
      Exit; // nothing valid yet: stay open so the text can be corrected
    Target := FTargetID;
    Close;
    if Assigned(FOnNavigate) then
      FOnNavigate(Target);
  end
  else if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

end.
