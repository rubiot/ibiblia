unit KChapterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ONTTokenizer, LCLType, HTMLColors,
  LazUTF8, Projeto, PCRE, Controls, Menus, fgl, Dialogs, Math, Contnrs, KMemo,
  KEditCommon, KGraphics;

type

  TBlockRange = record
    first, last: integer;
  end;

  { TNoteInfo }

  TNoteInfo = class
  private
    FId: integer;
    FLinkText: string;
    FVerse: integer;
    FText: string;
    FRange: TBlockRange;
  public
    constructor Create(id: integer; verse: integer; text: string; linktext: string);
    //destructor Destroy; override;
    property Id: integer read FId write FId;
    property LinkText: string read FLinkText write FLinkText;
    property Verse: integer read FVerse write FVerse;
    property Text: string read FText write FText;
    property Range: TBlockRange read FRange write FRange;
    property FirstBlock: integer read FRange.first write FRange.first;
    property LastBlock: integer read FRange.last write FRange.last;
  end;

  TKChapterView = class;

  { TNoteWindow }

  TNoteWindow = class(THintWindow)
  private
    FNoteView: TKChapterView;
    procedure InitNoteView;
    procedure SetNoteText(const AText: string; const AStyle: TKMemoTextStyle);
    procedure HandlePopupMouseEnter(Sender: TObject);
    procedure HandlePopupMouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(const AHint: string; AStyle: TKMemoTextStyle); overload;
  end;

  TViewMode = (vmParagraph, vmVersePerLine);
  TNoteList = specialize TFPGList<TNoteInfo>;

  { TKChapterView }

  TKChapterView = class(TKMemo)
  private
    FFontName: string;
    FFontSize: integer;
    FProject: TProjeto;
    FRxVerseHeading: IRegex;
    FNoteID: integer;
    FHint: TNoteWindow;
    FFontItem: TMenuItem;
    FParagraphModeItem: TMenuItem;
    FVersePerLineItem: TMenuItem;
    FVerseMode: TViewMode;
    FBibleText: TTipoTextoBiblico;
    FStyleStack: TObjectStack;
    FVerseRanges: array of TBlockRange;
    FCurrentVerse: integer;
    FNotes: TNoteList;
    FHideVerseNumber: boolean;

    function GetCurrentStyle: TKMemoTextStyle;
    procedure RenderChapter(verses: TStringList; current: integer);
    procedure RenderChapterHeader;
    procedure RenderChapterFooter;
    procedure RenderVerse(txt: string);
    procedure RenderSpan(txt: string);
    procedure RenderNote(link: string; note: string);
    procedure RenderNotes;
    procedure HandleReferenceClick(sender: TObject);
    procedure HandleNoteLinkClick(sender: TObject);
    procedure HandleNoteClick(sender: TObject);
    procedure HandleVerseChange(Sender: TProjeto);
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandlePopupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleSetFont(Sender: TObject);
    procedure HandleParagraphMode(Sender: TObject);
    procedure HandleVersePerLineMode(Sender: TObject);
    procedure SetBibleText(AValue: TTipoTextoBiblico);
    procedure SetProject(AValue: TProjeto);
    procedure InitPopupMenu;
    procedure SetVerseMode(AValue: TViewMode);
    procedure SetFontSize(AValue: integer);
    procedure SetFontName(AValue: string);
    function PushNewStyle: TKMemoTextStyle;
    function PushInheritedStyle: TKMemoTextStyle;
    procedure PopStyle;
    procedure HightlightRange(range: TBlockRange; bgcolor: TColor);
    procedure ResetStyleStack;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Project: TProjeto read FProject write SetProject;
    property VerseMode: TViewMode read FVerseMode write SetVerseMode;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontName: string read FFontName write SetFontName;
    property BibleText: TTipoTextoBiblico read FBibleText write SetBibleText default tbDestino;
    property CurrentStyle: TKMemoTextStyle read GetCurrentStyle;
    property HideVerseNumber: boolean read FHideVerseNumber write FHideVerseNumber default false;
  end;

const
  NonBibleTextColor = $be7c41;
  NoteLinkColor = $4080ff;
  ChapterNumberColor = $2cba7d;

resourcestring
  SSetFont = 'Choose &font...';
  SParagraphMode = '&Paragraph mode';
  SVersePerLineMode = '&Verse per line mode';

implementation

{ TNoteWindow }

procedure TNoteWindow.InitNoteView;
begin
  InsertControl(TKChapterView.Create(nil));
  FNoteView := Controls[0] as TKChapterView;
  with FNoteView do
  begin
    PopupMenu.Free;
    PopupMenu   := nil;
    OnKeyDown   := @HandlePopupKeyDown;
    OnMouseEnter:= @HandlePopupMouseEnter;
    OnMouseLeave:= @HandlePopupMouseLeave;
    Align       := alClient;
    BorderStyle := bsNone;
  end;
end;

procedure TNoteWindow.SetNoteText(const AText: string;
  const AStyle: TKMemoTextStyle);
begin
  FNoteView.Blocks.LockUpdate;
  Width := 500;
  FNoteView.Clear(false);
  FNoteView.TextStyle.Assign(AStyle);
  FNoteView.TextStyle.Font.Size := FNoteView.TextStyle.Font.Size - 1;
  FNoteView.RenderSpan(AText);
  FNoteView.Blocks.UnLockUpdate;
end;

procedure TNoteWindow.HandlePopupMouseEnter(Sender: TObject);
begin
  AutoHide := false;
end;

procedure TNoteWindow.HandlePopupMouseLeave(Sender: TObject);
begin
  Hide;
end;

constructor TNoteWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Color := clWindow;
  FNoteView:= nil;
end;

destructor TNoteWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TNoteWindow.ActivateHint(const AHint: string; AStyle: TKMemoTextStyle
  );
var
  Rect: TRect;
  Pos: TPoint;
begin
  if not Assigned(FNoteView) then
    InitNoteView;

  AutoHide := true;
  SetNoteText(AHint, AStyle);

  Rect := FNoteView.ContentRect;
  Pos := Mouse.CursorPos;
  Rect.Left := Pos.X+10;
  Rect.Top := Pos.Y+5;
  Rect.Right := Rect.Left + {Rect.Right} 500 + 5;
  Rect.Bottom := Rect.Top + Rect.Bottom + 5;
  ActivateHint(Rect, '');
end;

{ TNoteInfo }

constructor TNoteInfo.Create(id: integer; verse: integer; text: string;
  linktext: string);
begin
  FId    := id;
  FVerse := verse;
  FText  := text;
  FLinkText := linktext;
end;

{ TKChapterView }

procedure TKChapterView.RenderVerse(txt: string);
var
  mtHeading: IMatch;
  heading: string;
begin
  if (FCurrentVerse > 1) and not (Blocks.LastBlock is TKMemoParagraph) then
    Blocks.AddTextBlock(' '); // space between verses

  mtHeading := FRxVerseHeading.Match(txt);
  if not mtHeading.Success then
    raise Exception.Create(SysUtils.Format('Unmatched verse: <%s>', [txt]));
  heading := mtHeading.Groups[1].Value;
  txt     := mtHeading.Groups[2].Value;

  { rendering titles before the verse number }
  if not heading.IsEmpty then
    RenderSpan(heading);

  { rendering verse number }
  if not FHideVerseNumber then
  begin
    with Blocks.AddHyperlink(Format('%d ', [FCurrentVerse]), Format('%d,%d,%d', [FProject.BookID, FProject.Chapter, FCurrentVerse])) do
    begin
      OnClick := @HandleReferenceClick;
      TextStyle.Font.Name  := 'default';
      TextStyle.Font.Size  := FFontSize+2;
      TextStyle.Font.Style := [fsBold];
      TextStyle.Font.Color := NonBibleTextColor;
      TextStyle.ScriptPosition:= tpoSuperscript;
    end;
  end;

  { rendering the verse text }
  FVerseRanges[FCurrentVerse].first := Blocks.Count;
  RenderSpan(txt);
  FVerseRanges[FCurrentVerse].last := Blocks.Count-1;

  if FVerseMode = vmVersePerLine then
    Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
end;

function TKChapterView.GetCurrentStyle: TKMemoTextStyle;
begin
  result := TKMemoTextStyle(FStyleStack.Peek);
end;

procedure TKChapterView.RenderSpan(txt: string);
var
  FTokenizer: TONTTokenizer;
  token: TTagSintagma;
  prop: string;
  chunk: string;
  linktext: string;
  size: integer;
begin
  chunk := '';
  FTokenizer := TONTTokenizer.Criar(txt);

  Blocks.LockUpdate;
  while FTokenizer.LerSintagma(token) <> tsNulo do
  begin
    case token.tipo of
      tsMetaDado:
        ;
      tsEspaco:
        if token.valor <> '|' then
          chunk := chunk + token.valor;
      tsPontuacao, tsSintagma:
        chunk := chunk + token.valor;
      tsTag:
      begin
        if chunk.Length > 0 then
        begin
          Blocks.AddTextBlock(chunk).TextStyle.Assign(CurrentStyle);
          chunk := '';
        end;
        if token.valor.StartsWith('<TS') then
        begin
          if not (Blocks[Blocks.Count-1] is TKMemoParagraph) then
            Blocks.AddParagraph().ParaStyle.FirstIndent := IfThen(FVerseMode = vmParagraph, 20);

          with PushNewStyle do
          begin
            case token.valor[4] of
              '>','0':
              begin
                Font.Size := Font.Size + 2;
                Font.Style := [fsBold];
                Font.Color := $424242;
              end;
              '1':
              begin
                Font.Size := Font.Size + 1;
                Font.Style := [fsBold, fsItalic];
                Font.Color := clDefault;
              end;
              '2':
              begin
                Font.Style := [fsBold, fsItalic];
                Font.Color := clDefault;
              end;
              '3':
              begin
                Font.Style := [fsItalic];
                Font.Color := clDefault;
              end;
            end;
          end;
        end else if token.valor = '<Ts>' then
        begin
          ResetStyleStack;
          with Blocks.AddParagraph().ParaStyle do
          begin
            FirstIndent := 0;
            TopPadding := IfThen(Blocks.GetNearestParagraphBlock(Blocks.Count-1).BottomPadding > 0, 0, 10);
          end;
        end else if token.valor.StartsWith('<RF') then
        begin
          linktext := FTokenizer.LerPropriedadeTag('q', token);
          token.valor := '';
          FTokenizer.LerAteTag(token, '<Rf>');
          token.valor := token.valor.Replace('<Rf>', '');
          RenderNote(linktext, token.valor);
        end
        else if token.valor = '<FI>' then
        begin
          with PushInheritedStyle do
          begin
            Font.Style := Font.Style + [fsItalic];
            Font.Color := clGrayText;
          end;
        end else if token.valor = '<FR>' then
        begin
          PushInheritedStyle.Font.Color := clRed
        end else if token.valor = '<FO>' then
        begin
          PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsBold]
        end
        else if token.valor = '<CM>' then
        begin
          if FVerseMode = vmParagraph then
            Blocks.AddParagraph().ParaStyle.FirstIndent := 20;
        end else if token.valor = '<CL>' then
        begin
          if FVerseMode = vmParagraph then
            Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
            //Blocks.InsertNewLine(Text.Length);
        end else
        begin
          token.valor := token.valor.ToLower;
          if token.valor.StartsWith('<font ') then
          begin
            if token.valor.Contains(' color=') then
              PushInheritedStyle.Font.Color := HTML2Color(FTokenizer.LerPropriedadeTag('color', token));
            if token.valor.Contains(' size=') then
            begin
              prop := FTokenizer.LerPropriedadeTag('size', token);
              if prop.Chars[0] in ['-', '+'] then
                size := CurrentStyle.Font.Size + prop.ToInteger
              else
                size := prop.ToInteger;
              PushInheritedStyle.Font.Size := size;
            end;
          end else if token.valor = '<b>' then
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsBold]
          else if token.valor = '<i>' then
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsItalic]
          else if token.valor = '<u>' then
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsUnderline]
          else if token.valor = '<s>' then
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsStrikeOut]
          else if token.valor = '<sup>' then
            PushInheritedStyle.ScriptPosition := tpoSuperscript
          else if token.valor = '<sub>' then
            PushInheritedStyle.ScriptPosition := tpoSubscript
          else if token.valor = '<br/>' then
            Blocks.AddParagraph().ParaStyle.FirstIndent := 0
          else if (token.valor = '</sub>')
                   or (token.valor = '</sup>')
                   or (token.valor = '</s>')
                   or (token.valor = '</u>')
                   or (token.valor = '</i>')
                   or (token.valor = '</b>')
                   or (token.valor = '<fo>')
                   or (token.valor = '<fr>')
                   or (token.valor = '<fi>')
                   or (token.valor = '</font>') then
            PopStyle;
        end;
      end;
    end;
  end;

  if chunk.Length > 0 then
  begin
    Blocks.AddTextBlock(chunk).TextStyle.Assign(CurrentStyle);
    chunk := '';
  end;

  FreeAndNil(FTokenizer);
  Blocks.UnLockUpdate;
end;

procedure TKChapterView.RenderNote(link: string; note: string);
begin
  if link.IsEmpty then
    link := FNoteID.ToString;

  FNotes.Add(TNoteInfo.Create(FNoteID, FCurrentVerse, note, link));
  if not Blocks.LastBlock.Text.EndsWith(' ') then
    link := ' ' + link;

  with Blocks.AddHyperlink(link, (FNoteID-1).ToString) do
  begin
    TextStyle.Assign(TextStyle);
    TextStyle.Font.Color := NoteLinkColor;
    TextStyle.Font.Style := [fsItalic];
    TextStyle.ScriptPosition := tpoSuperscript;
    OnClick := @HandleNoteLinkClick;
  end;
  Inc(FNoteID);
end;

procedure TKChapterView.RenderNotes;
var
  note: TNoteInfo;
begin
  if FNotes.Count = 0 then
    exit;

  ResetStyleStack;
  CurrentStyle.Font.Size := max(FFontSize-2, 2); // using smaller font

  Blocks.AddParagraph();
  Blocks.AddParagraph();

  for note in FNotes do
  begin
    with Blocks.AddHyperlink(SysUtils.Format('[%s] ', [note.LinkText]), (note.Id-1).ToString) do
    begin
      TextStyle.Font.Color := NoteLinkColor;
      TextStyle.Font.Style := [fsBold];
      TextStyle.Font.Size  := Max(FFontSize - 3, 2);
      OnClick := @HandleNoteClick;
    end;
    note.FirstBlock := Blocks.Count-1;
    RenderSpan(note.Text);
    note.LastBlock := Blocks.Count-1;
    Blocks.AddParagraph();
  end;
end;

procedure TKChapterView.HandleReferenceClick(sender: TObject);
begin
  FProject.IrPara(TKMemoHyperlink(Sender).URL);
end;

procedure TKChapterView.HandleNoteLinkClick(sender: TObject);
var
  //p: TPoint;
  note: TNoteInfo;
  {rect: TRect;
  pos: TPoint;
  memo: TKChapterView;}
begin
  note := FNotes[TKMemoHyperlink(Sender).URL.ToInteger];
  FHint.ActivateHint(note.Text, TextStyle);

  {
  if FHint.ControlCount = 0 then // first time? create the memo
  begin
    FHint.InsertControl(TKChapterView.Create(nil));
    with (FHint.Controls[0] as TKChapterView) do
    begin
      PopupMenu.Free;
      PopupMenu  := nil;
      OnKeyDown  := @HandlePopupKeyDown;
      OnExit     := @HandlePopupExit;
      Align      := alClient;
      BorderStyle:= bsNone;
    end;
  end;

  memo := FHint.Controls[0] as TKChapterView;
  with memo do
  begin
    memo.Blocks.LockUpdate;
    TextStyle.Assign(self.TextStyle);
    TextStyle.Font.Size := TextStyle.Font.Size - 1;
    FHint.Width := 500;
    memo.Clear(false);
    memo.RenderSpan(note.Text);
    memo.Blocks.UnLockUpdate;
  end;

  Rect := memo.ContentRect; //FHint.CalcHintRect(600, memo.Text, nil);
  Pos := Mouse.CursorPos;
  Rect.Left := Pos.X+10;
  Rect.Top := Pos.Y+5;
  Rect.Right := Rect.Left + {Rect.Right} 500 + 5;
  Rect.Bottom := Rect.Top + Rect.Bottom + 5;
  FHint.ActivateHint(Rect, '');}

  {p := Blocks[note.FirstBlock].BoundsRect.TopLeft;
  ExecuteCommand(ecGotoXY, @p);
  ExecuteCommand(ecDown, nil);
  HightlightRange(note.Range, clSilver);
  //ExecuteCommand(ecScrollCenter, nil);
  }
end;

procedure TKChapterView.HandleNoteClick(sender: TObject);
var
  note: TNoteInfo;
begin
  note := FNotes[TKMemoHyperlink(Sender).URL.ToInteger];

  HightlightRange(note.Range, clDefault); // un-highlight current note text
  HightlightRange(FVerseRanges[note.Verse], clSilver); // un-highlight current note text
end;

procedure TKChapterView.HandleVerseChange(Sender: TProjeto);
var
  verses: TStringList;
begin
  if not assigned(FProject) or (Width < 2) then
    exit;

  verses := Sender.GetChapterText(FBibleText);
  try
    RenderChapter(verses, Sender.Verse);
  finally
    FreeAndNil(verses);
  end;
end;

procedure TKChapterView.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key in [VK_SUBTRACT, VK_OEM_MINUS]) and (FFontSize > 1) then
  begin
    FFontSize := FFontSize-1;
    HandleVerseChange(FProject);
  end
  else if (ssCtrl in Shift) and (Key in [VK_ADD, VK_OEM_PLUS]) then
  begin
    FFontSize := FFontSize+1;
    HandleVerseChange(FProject);
  end;
end;

procedure TKChapterView.HandlePopupKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    (Parent as THintWindow).Hide;
end;

procedure TKChapterView.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
{var
  iCharIndex, iCharOffset, i, j: Integer;
  s, note: string;
  attr: TFontParams;
  rect: TRect;
  pos: TPoint;}
begin
  {
  iCharIndex := CharAtPos(X, Y);
  if iCharIndex < 0
    then Exit;

  if not GetTextAttributes(iCharIndex, attr) or
     (attr.VScriptPos <> vpSuperScript) then
  begin
    FHint.Hide;
    exit;
  end;

  { GetText causes the whole memo to scroll to the cursor position }
  s := GetText(Max(iCharIndex - 3, 0), 6);

  iCharOffset := iCharIndex - Max(iCharIndex - 3, 0);
  i := iCharOffset + 1;
  while (i > 0) and IsDigit(s[i]) do Dec(i);
  j := iCharOffset + 1;
  while (j <= Length(s)) and IsDigit(s[j]) do Inc(j);
  note := Copy(s, i, j - i);

  if note.IsEmpty then
  begin
    FHint.Hide;
    exit;
  end;

  if FHint.Visible then
    exit;

  note := FChapterNotes[note.ToInteger - 1];
  Rect := FHint.CalcHintRect(500, note, nil);
  Pos := Mouse.CursorPos;
  Rect.Left := Pos.X+10;
  Rect.Top := Pos.Y+5;
  Rect.Right := Rect.Left + Rect.Right;
  Rect.Bottom := Rect.Top + Rect.Bottom;
  FHint.ActivateHint(Rect, note);}
end;

procedure TKChapterView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    SetFontSize(FFontSize + Sign(WheelDelta)*1);
    Handled := true;
  end;
end;

procedure TKChapterView.HandleSetFont(Sender: TObject);
var
  dialog: TFontDialog;
begin
  dialog := TFontDialog.Create(self);
  dialog.Options := [fdNoStyleSel];
  dialog.Font.Name := FFontName;
  dialog.Font.Size := FFontSize;
  if dialog.Execute then
  begin
    FontName := dialog.Font.Name;
    FontSize := dialog.Font.Size;
    HandleVerseChange(FProject);
  end;
  dialog.Free;
end;

procedure TKChapterView.HandleParagraphMode(Sender: TObject);
begin
  VerseMode := vmParagraph;
end;

procedure TKChapterView.HandleVersePerLineMode(Sender: TObject);
begin
  VerseMode := vmVersePerLine;
end;

procedure TKChapterView.SetBibleText(AValue: TTipoTextoBiblico);
begin
  if FBibleText=AValue then Exit;
  FBibleText:=AValue;
  HandleVerseChange(FProject);
end;

procedure TKChapterView.SetProject(AValue: TProjeto);
begin
  if FProject = AValue then Exit;
  FProject := AValue;
  FProject.OnNewVerseSubscribe(@HandleVerseChange);
end;

procedure TKChapterView.InitPopupMenu;
begin
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Parent := Self;

  FFontItem := TMenuItem.Create(PopupMenu);
  with FFontItem do
  begin
    Caption := SSetFont;
    OnClick := @HandleSetFont;
    Enabled := True;
  end;
  PopupMenu.Items.Add(FFontItem);

  PopupMenu.Items.AddSeparator;

  FParagraphModeItem := TMenuItem.Create(PopupMenu);
  with FParagraphModeItem do
  begin
    Caption := SParagraphMode;
    OnClick := @HandleParagraphMode;
    Enabled := False;
  end;
  PopupMenu.Items.Add(FParagraphModeItem);

  FVersePerLineItem := TMenuItem.Create(PopupMenu);
  with FVersePerLineItem do
  begin
    Caption := SVersePerLineMode;
    OnClick := @HandleVersePerLineMode;
    Enabled := True;
  end;
  PopupMenu.Items.Add(FVersePerLineItem);
end;

procedure TKChapterView.SetVerseMode(AValue: TViewMode);
begin
  if FVerseMode = AValue
    then Exit;

  FVerseMode := AValue;
  FParagraphModeItem.Enabled := FVerseMode <> vmParagraph;
  FVersePerLineItem.Enabled  := FVerseMode <> vmVersePerLine;
  HandleVerseChange(FProject);
end;

procedure TKChapterView.SetFontSize(AValue: integer);
begin
  if (FFontSize = AValue) or (AValue < 1) then Exit;
  FFontSize := AValue;
  TextStyle.Font.Size := FFontSize;
  FHint.Font.Assign(TextStyle.Font);

  HandleVerseChange(FProject);
end;

procedure TKChapterView.SetFontName(AValue: string);
begin
  if (FFontName = AValue) then Exit;
  FFontName := AValue;
  TextStyle.Font.Name := FFontName;
  FHint.Font.Assign(TextStyle.Font);

  HandleVerseChange(FProject);
end;

function TKChapterView.PushNewStyle: TKMemoTextStyle;
begin
  result := TKMemoTextStyle(FStyleStack.Push(TKMemoTextStyle.Create));
  result.Assign(TextStyle);
end;

function TKChapterView.PushInheritedStyle: TKMemoTextStyle;
var
  current: TKMemoTextStyle;
begin
  current := CurrentStyle;
  result := PushNewStyle;
  result.Assign(current);
end;

procedure TKChapterView.PopStyle;
begin
  FStyleStack.Pop.Free;
end;

procedure TKChapterView.HightlightRange(range: TBlockRange; bgcolor: TColor);
var
  i: integer;
begin
  for i:=range.first to range.last do
    if Blocks[i].ClassName = 'TKMemoTextBlock' then
      with TKMemoTextBlock(Blocks[i]) do
        TextStyle.Brush.Color := bgcolor;
end;

procedure TKChapterView.ResetStyleStack;
begin
  while FStyleStack.Count > 0 do
    PopStyle;
  TextStyle.Font.Size := FFontSize;
  TextStyle.Font.Name := FFontName;
  TextStyle.ScriptPosition := tpoNormal;
  ParaStyle.FirstIndent := 0;
  ParaStyle.LineSpacingFactor := 1.2;
  PushNewStyle; // pushing default style onto stack
end;

procedure TKChapterView.RenderChapterHeader;
var
  c: integer;
begin
  { current chapter }
  with Blocks.AddTextBlock(SysUtils.Format('%s %d', [FProject.Book, FProject.Chapter])).TextStyle.Font do
  begin
    Name := 'default';
    Size := Self.TextStyle.Font.Size+2;
    Bold := true;
    Color := NonBibleTextColor;
  end;
  Blocks.AddParagraph(); //.ParaStyle.HAlign := halLeft;

  { chapter list }
  for c:=1 to QCapitulosONT[FProject.BookID] do
  begin
    if c > 1 then
      Blocks.AddTextBlock(' ');

    if c = FProject.Chapter then
      with Blocks.AddTextBlock(c.ToString).TextStyle.Font do
      begin
        Name := 'default';
        Size := Self.TextStyle.Font.Size-2;
        Bold := true;
        Color := NonBibleTextColor;
      end
    else
      with Blocks.AddHyperlink(c.ToString, Format('%d,%d,1', [FProject.BookID, c])) do
      begin
        OnClick := @HandleReferenceClick;
        TextStyle.Font.Name  := 'default';
        TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
        TextStyle.Font.Bold  := false;
        TextStyle.Font.Color := ChapterNumberColor;
      end;
  end;
  Blocks.AddParagraph().ParaStyle.BottomPadding := 10;
end;

procedure TKChapterView.RenderChapterFooter;
  function GetFirstBook: integer;
  begin
    if FProject.Escopo = etNT then
      Result := 40
    else
      Result := 1;
  end;

  procedure GetPreviousChapter(var book: integer; var chapter: integer);
  begin
    if (FProject.BookID = GetFirstBook) and (FProject.Chapter = 1) then
       exit;

    if FProject.Chapter = 1 then
    begin
      book := FProject.BookID - 1;
      chapter := QCapitulosONT[book];
    end else
    begin
      book := FProject.BookID;
      chapter := FProject.Chapter - 1;
    end;
  end;

  function GetLastBook: integer;
  begin
    if FProject.Escopo = etOT then
      Result := 39
    else
      Result := 66;
  end;

  procedure GetNextChapter(var book: integer; var chapter: integer);
  begin
    if (FProject.BookID = GetLastBook) and (FProject.Chapter = QCapitulosONT[FProject.BookID]) then
       exit;

    if FProject.Chapter = QCapitulosONT[FProject.BookID] then
    begin
      book := FProject.BookID + 1;
      chapter := 1;
    end else
    begin
      book := FProject.BookID;
      chapter := FProject.Chapter + 1;
    end;
  end;

var
  book, chapter: integer;
begin
  Blocks.AddParagraph().ParaStyle.BottomPadding := 10;

  book := 0;
  chapter := 0;
  GetPreviousChapter(book, chapter);

  if book <> 0 then
    with Blocks.AddHyperlink(Format('%s %d << ', [NLivrosONT[book], chapter]), Format('%d,%d,1', [book, chapter])) do
    begin
      OnClick := @HandleReferenceClick;
      TextStyle.Font.Name  := 'default';
      TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
      TextStyle.Font.Bold  := false;
      TextStyle.Font.Color := ChapterNumberColor;
    end;

  book := 0;
  chapter := 0;
  GetNextChapter(book, chapter);

  if book <> 0 then
    with Blocks.AddHyperlink(Format(' >> %s %d', [NLivrosONT[book], chapter]), Format('%d,%d,1', [book, chapter])) do
    begin
      OnClick := @HandleReferenceClick;
      TextStyle.Font.Name  := 'default';
      TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
      TextStyle.Font.Bold  := false;
      TextStyle.Font.Color := ChapterNumberColor;
    end;

  with Blocks.AddParagraph().ParaStyle do
  begin
    BottomPadding := 10;
    HAlign := halCenter;
  end;
end;

procedure TKChapterView.SetEnabled(Value: Boolean);
begin
  if assigned(FProject) and not Value and Enabled then
  begin
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);
    FProject := nil;
  end;

  inherited SetEnabled(Value);
end;

constructor TKChapterView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent       := TWinControl(AOwner);
  OnKeyDown    := @HandleKeyDown;
  ReadOnly     := true;
  OnMouseWheel := @HandleMouseWheel;
  { TRichMemo.GetText causes unsolicited scrolling, disabling it }
  //OnMouseMove  := @HandleMouseMove;
  FFontName       := 'default';
  FFontSize       := 10;
  FProject        := nil;
  FVerseMode      := vmParagraph;
  FRxVerseHeading := RegexCreate('^((?:<TS[0-3]?>.*?<Ts>)*)(.*?)$', [rcoUTF8]);
  FNotes          := TNoteList.Create;
  FHint           := TNoteWindow.Create(Self);
  InitPopupMenu;

  FStyleStack := TObjectStack.Create;
end;

destructor TKChapterView.Destroy;
begin
  FNotes.Free;
  FHint.Free;
  FStyleStack.Free;
  if assigned(FProject) then
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);

  inherited Destroy;
end;

procedure TKChapterView.RenderChapter(verses: TStringList; current: integer);
var
  verse: string;
  p: TPoint;
begin
  SetLength(FVerseRanges, verses.Count + 1);

  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;

    FNoteID := 1;
    FNotes.Clear;

    RenderChapterHeader;

    FCurrentVerse := 0;
    for verse in verses do
    begin
      Inc(FCurrentVerse);
      RenderVerse(verse);
    end;

    RenderChapterFooter;
    HightlightRange(FVerseRanges[current], clSilver);
    //RenderNotes;
  finally
    Blocks.UnLockUpdate;
  end;

  p := Blocks[FVerseRanges[current].first].BoundsRect.TopLeft;
  ExecuteCommand(ecGotoXY, @p);
  ExecuteCommand(ecDown, nil);
  ExecuteCommand(ecScrollCenter, nil);
end;

end.

