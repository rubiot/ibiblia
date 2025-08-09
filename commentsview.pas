unit CommentsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, HTMLRenderer, StdCtrls, Graphics;

type
  TCommentsView = class;

  { TRawCommentsMemo }

  TRawCommentsMemo = class(TMemo)
  private
    FCommentsView: TCommentsView;
    procedure HandleOnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure HandleOnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent; CommentsView: TCommentsView); overload;
    destructor Destroy; override;

    procedure Show(aText: string);
    procedure Hide;
  end;

  { TCommentsView }
  
  TCommentsView = class(THTMLRenderer)
  private
    FFontName: string;
    FFontSize: integer;
    FText: string;
    FReadOnly: boolean;
    FRawMemo: TRawCommentsMemo;
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; 
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleDoubleClick(Sender: TObject);
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
    
    property FontSize: integer read FFontSize write SetFontSize;
    property FontName: string read FFontName write SetFontName;
    property Text: string read GetText write SetText;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
  end;

implementation

uses
  Math;

{ TRawCommentsMemo }

procedure TRawCommentsMemo.HandleOnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 27 then // VK_ESCAPE
    Hide;
end;

procedure TRawCommentsMemo.HandleOnMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Font.Size := Font.Size + Sign(WheelDelta) * 1;
end;

constructor TRawCommentsMemo.Create(AOwner: TComponent; CommentsView: TCommentsView);
begin
  inherited Create(AOwner);

  Visible    := False;
  Parent     := CommentsView.Parent; // Use the same parent as CommentsView
  FCommentsView := CommentsView;
  
  // Position exactly over the CommentsView
  Left       := CommentsView.Left;
  Top        := CommentsView.Top;
  Width      := CommentsView.Width;
  Height     := CommentsView.Height;
  Anchors    := CommentsView.Anchors;
  
  ScrollBars := ssAutoVertical;
  ParentFont := False;
  Font.Name  := 'Courier New';
  Color      := clWindow; // Ensure it has a solid background
  ReadOnly   := false; // Allow editing
  OnKeyDown  := @HandleOnKeyDown;
  OnMouseWheel := @HandleOnMouseWheel;
end;

destructor TRawCommentsMemo.Destroy;
begin
  inherited Destroy;
end;

procedure TRawCommentsMemo.Show(aText: string);
begin
  Font.Size := FCommentsView.FontSize;
  Text := aText;
  
  // Position exactly over the CommentsView
  Left   := FCommentsView.Left;
  Top    := FCommentsView.Top;
  Width  := FCommentsView.Width;
  Height := FCommentsView.Height;
  
  // Hide the CommentsView and show this memo
  FCommentsView.Visible := False;
  Visible := True;
  BringToFront;
  SetFocus;
end;

procedure TRawCommentsMemo.Hide;
begin
  Visible := false;
  
  // Update CommentsView text with any changes and show it again
  if Assigned(FCommentsView) then
  begin
    FCommentsView.SetCommentsText(Text);
    FCommentsView.Visible := True;
  end;
end;

{ TCommentsView }

constructor TCommentsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFontName := 'default';
  FFontSize := 10;
  FReadOnly := False; // Allow editing by default
  OnMouseWheel := @HandleMouseWheel;
  OnDblClick := @HandleDoubleClick;
  ScrollBars := ssAutoVertical;
  FRawMemo := nil; // Will be created when needed
end;

destructor TCommentsView.Destroy;
begin
  if Assigned(FRawMemo) then
    FRawMemo.Free;
  inherited Destroy;
end;

procedure TCommentsView.SetupDefaultStyle;
begin
  inherited SetupDefaultStyle;
  TextStyle.Font.Name := FFontName;
  TextStyle.Font.Size := FFontSize;
end;

procedure TCommentsView.SetCommentsText(const CommentsText: string);
begin
  FText := CommentsText;
  RenderHTML(CommentsText);
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
  if FFontName = AValue then Exit;
  FFontName := AValue;
  SetupDefaultStyle;
  if FText <> '' then
    RenderHTML(FText);
end;

procedure TCommentsView.SetFontSize(const AValue: integer);
begin
  if FFontSize = AValue then Exit;
  FFontSize := AValue;
  SetupDefaultStyle;
  if FText <> '' then
    RenderHTML(FText);
end;

procedure TCommentsView.HandleDoubleClick(Sender: TObject);
begin
  // Don't allow editing if ReadOnly is true
  if FReadOnly then
    Exit;
    
  // Create the raw memo if it doesn't exist yet
  if not Assigned(FRawMemo) then
    FRawMemo := TRawCommentsMemo.Create(Self, Self);
    
  // Show the raw memo for editing (it will handle hiding this component)
  FRawMemo.Show(FText);
end;

procedure TCommentsView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    FFontSize := Max(6, FFontSize + Sign(WheelDelta));
    SetupDefaultStyle;
    RenderHTML(FText); // Re-render with new font size
    Handled := true;
  end;
end;

end.

{$IFDEF FPC}
initialization
  // Component registration not needed for FPC/Lazarus runtime
{$ENDIF}
