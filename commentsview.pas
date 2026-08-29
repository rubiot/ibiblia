unit CommentsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, HTMLRenderer, StdCtrls, Graphics;

function CommentsTextForWysiwygEditor(const Value: string): string;
function CommentsTextFromWysiwygEditor(const Value: string): string;
function CommentsTextForPreview(const Value: string): string;

type
  { TCommentsView }

  TCommentsView = class(THTMLRenderer)
  private
    FFontName: string;
    FFontSize: integer;
    FPreviewText: string;
    FRenderGeneration: QWord;
    FText: string;
    FReadOnly: boolean;
    FOnTextChanged: TNotifyEvent;
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleDoubleClick(Sender: TObject);
    procedure RenderComments;
    function GetText: string;
    procedure SetText(const AValue: string);
    procedure SetFontName(const AValue: string);
    procedure SetFontSize(const AValue: integer);
  protected
    procedure SetupDefaultStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetCommentsText(const CommentsText: string);
    function HasPendingChange(const Baseline: string): Boolean;
    procedure DiscardEdit;

    property FontSize: integer read FFontSize write SetFontSize;
    property FontName: string read FFontName write SetFontName;
    property Text: string read GetText write SetText;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;
    {$IFDEF FPCUNIT}
    function RenderGenerationFixture: QWord;
    {$ENDIF}
  end;

implementation

uses
  Math, theword_translator_note_editor;

resourcestring
  SEditorialCommentsTitle = 'Editorial comments';

function CommentsTextForWysiwygEditor(const Value: string): string;
begin
  Result := StringReplace(Value, #13#10, '<p>', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '<p>', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '<p>', [rfReplaceAll]);
  Result := StringReplace(Result, '<crlf/>', '<p>',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<lf/>', '<p>',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<br/>', '<CL>',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<br>', '<CL>',
    [rfReplaceAll, rfIgnoreCase]);
end;

function CommentsTextFromWysiwygEditor(const Value: string): string;
begin
  Result := CommentsTextForWysiwygEditor(Value);
end;

function CommentsTextForPreview(const Value: string): string;
begin
  Result := StringReplace(Value, #13#10, '<br/>', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '<br/>', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '<br/>', [rfReplaceAll]);
  Result := StringReplace(Result, '<crlf/>', '<br/>',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<lf/>', '<br/>',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<p>', '<br/>',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<CL>', '<br/>',
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<br>', '<br/>',
    [rfReplaceAll, rfIgnoreCase]);
end;

{ TCommentsView }

constructor TCommentsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFontName := 'default';
  FFontSize := 10;
  FReadOnly := False;
  FOnTextChanged := nil;
  OnMouseWheel := @HandleMouseWheel;
  OnDblClick := @HandleDoubleClick;
  ScrollBars := ssAutoVertical;
end;

destructor TCommentsView.Destroy;
begin
  inherited Destroy;
end;

procedure TCommentsView.SetupDefaultStyle;
begin
  inherited SetupDefaultStyle;
  TextStyle.Font.Name := FFontName;
  TextStyle.Font.Size := FFontSize;
end;

procedure TCommentsView.SetCommentsText(const CommentsText: string);
var
  PreviewText: string;
begin
  if FText = CommentsText then
    Exit;
  FText := CommentsText;
  PreviewText := CommentsTextForPreview(CommentsText);
  if FPreviewText = PreviewText then
    Exit;
  FPreviewText := PreviewText;
  RenderComments;
end;

procedure TCommentsView.RenderComments;
begin
  Inc(FRenderGeneration);
  RenderHTML(FPreviewText);
end;

{$IFDEF FPCUNIT}
function TCommentsView.RenderGenerationFixture: QWord;
begin
  Result := FRenderGeneration;
end;
{$ENDIF}

function TCommentsView.HasPendingChange(const Baseline: string): Boolean;
begin
  Result := GetText <> Baseline;
end;

procedure TCommentsView.DiscardEdit;
begin
  { Editing is modal and only updates FText after Save. }
end;

function TCommentsView.GetText: string;
begin
  Result := FText;
end;

procedure TCommentsView.SetText(const AValue: string);
begin
  SetCommentsText(AValue);
end;

procedure TCommentsView.SetFontName(const AValue: string);
begin
  if SameText(FFontName, AValue) then
  begin
    FFontName := AValue;
    Exit;
  end;
  FFontName := AValue;
  SetupDefaultStyle;
  if FPreviewText <> '' then
    RenderComments;
end;

procedure TCommentsView.SetFontSize(const AValue: integer);
begin
  if FFontSize = AValue then Exit;
  FFontSize := AValue;
  SetupDefaultStyle;
  if FPreviewText <> '' then
    RenderComments;
end;

procedure TCommentsView.HandleDoubleClick(Sender: TObject);
var
  AnchorRect: TRect;
  EditedBody, InitialBody: UnicodeString;
  NewText: string;
  ScreenOrigin: TPoint;
begin
  if FReadOnly then
    Exit;

  ScreenOrigin := ClientToScreen(Point(0, 0));
  AnchorRect := Rect(ScreenOrigin.X, ScreenOrigin.Y,
    ScreenOrigin.X + ClientWidth, ScreenOrigin.Y + ClientHeight);
  InitialBody := UTF8Decode(CommentsTextForWysiwygEditor(FText));
  if not ExecuteTheWordBodyEditor(Self, AnchorRect,
    SEditorialCommentsTitle, InitialBody, EditedBody) then
    Exit;

  NewText := CommentsTextFromWysiwygEditor(UTF8Encode(EditedBody));
  if NewText = FText then
    Exit;
  SetCommentsText(NewText);
  if Assigned(FOnTextChanged) then
    FOnTextChanged(Self);
end;

procedure TCommentsView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  NewFontSize: Integer;
begin
  if ssCtrl in Shift then
  begin
    Handled := True;
    NewFontSize := Max(6, FFontSize + Sign(WheelDelta));
    if NewFontSize = FFontSize then
      Exit;
    FFontSize := NewFontSize;
    SetupDefaultStyle;
    RenderComments;
  end;
end;

{$IFDEF FPC}
initialization
  { Component registration not needed for FPC/Lazarus runtime. }
{$ENDIF}

end.
