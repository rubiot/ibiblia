unit ChapterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, RichView, RVStyle, Graphics, ONTTokenizer, LCLType,
  LazUTF8, Projeto, PCRE, Controls, Menus, fgl, Dialogs;

type

  TViewMode = (vmParagraph, vmVersePerLine);
  TIntegerList = specialize TFPGList<Integer>;

  { TChapterView }

  TChapterView = class(TRichView)
  private
    FFontName: string;
    FFontSize: integer;
    FProject: TProjeto;
    FRxVerseHeading: IRegex;
    FChapterNotes: TStringList;
    FNoteID: integer;
    FHint: THintWindow;
    FFontItem: TMenuItem;
    FParagraphModeItem: TMenuItem;
    FVersePerLineItem: TMenuItem;
    FVerseMode: TViewMode;
    FStyles: TRVStyle;
    FVerseJumps: TIntegerList;
    FNoteJumps: TIntegerList;
    FJumps: integer;
    FFromNewLine: boolean;

    procedure RenderVerse(txt: string; verse: integer; isCurrent: boolean);
    procedure RenderSpan(txt: string);
    procedure RenderNotes;
    procedure HandleJump(Sender: TObject; id: Integer);
    procedure HandleVerseChange(Sender: TProjeto);
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleSetFont(Sender: TObject);
    procedure HandleParagraphMode(Sender: TObject);
    procedure HandleVersePerLineMode(Sender: TObject);
    procedure SetProject(AValue: TProjeto);
    procedure InitPopupMenu;
    procedure InitStyles;
    procedure SetVerseMode(AValue: TViewMode);
    procedure AppendText(s: string; StyleNo: Integer);
    procedure SetFontSize(AValue: integer);
    procedure SetFontName(AValue: string);
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderChapter(verses: TStringList; current: integer);
    property Project: TProjeto read FProject write SetProject;
    property VerseMode: TViewMode read FVerseMode write SetVerseMode;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontName: string read FFontName write SetFontName;
  end;

var
  rvsFI: integer;
  rvsFO: integer;
  rvsFR: integer;
  rvsBold: integer;
  rvsItalic: integer;
  rvsUnderline: integer;
  rvsSuperscript: integer;
  rvsSubscript: integer;
  rvsBoldItalic: integer;
  rvsTS0: integer;
  rvsTS1: integer;
  rvsTS2: integer;
  rvsTS3: integer;
  rvsStrikeout: integer;
  rvsVerseNumber: integer;
  rvsCurrentVerseNumber: integer;

resourcestring
  SSetFont = 'Choose &font...';
  SParagraphMode = '&Paragraph mode';
  SVersePerLineMode = '&Verse per line mode';

implementation

{ TChapterView }

procedure TChapterView.RenderVerse(txt: string; verse: integer;
  isCurrent: boolean);
var
  mtHeading: IMatch;
begin
  if (FVerseMode = vmParagraph) and (verse > 1) then
    Add(' ', rvsNormal);

  mtHeading := FRxVerseHeading.Match(txt);
  if not mtHeading.Success then
    raise Exception.Create(SysUtils.Format('Unmatched verse: <%s>', [txt]));

  { rendering titles before the verse number }
  RenderSpan(mtHeading.Groups[1].Value);
  txt := mtHeading.Groups[2].Value;

  {if isCurrent then
  begin
    //fmt.BkColor  := clLtGray;
    //fmt.HasBkClr := true;
  end;}

  { rendering verse number }
  AppendText(SysUtils.Format('%d ', [verse]), rvsJump1);
  FVerseJumps.Add(FJumps); Inc(FJumps);

  { rendering the verse text }
  RenderSpan(txt);

  if FVerseMode = vmVersePerLine then
    FFromNewLine := true;
end;

procedure TChapterView.RenderSpan(txt: string);
var
  FTokenizer: TONTTokenizer;
  token: TTagSintagma;
  fmt: Integer;
begin
  fmt := rvsNormal;
  FTokenizer := TONTTokenizer.Criar(txt);
  while FTokenizer.LerSintagma(token) <> tsNulo do
  begin
    case token.tipo of
      tsMetaDado:
        token.valor := '';
      tsEspaco:
        if token.valor = '|' then
          token.valor := '';
      tsPontuacao, tsSintagma:
        ;
      tsTag:
        if token.valor.StartsWith('<TS') then
        begin
          case token.valor[4] of
            '>','0':
              fmt := rvsTS0;
            '1':
              fmt := rvsTS1;
            '2':
              fmt := rvsTS2;
            '3':
              fmt := rvsTS3;
          end;
          token.valor := '';
          FFromNewLine := true;
        end else if token.valor = '<Ts>' then
        begin
          fmt := rvsNormal;
          token.valor := '';
          FFromNewLine:=true;
        end
        else if token.valor.StartsWith('<RF') then
        begin
          token.valor := '';
          FTokenizer.LerAteTag(token, '<Rf>');
          token.valor := token.valor.Replace('<Rf>',  '');
          FChapterNotes.Add(token.valor);

          AddText(SysUtils.Format(' %d ', [FNoteID]), rvsJump2);
          FNoteJumps.Add(FJumps); Inc(FJumps);
          inc(FNoteID);
          token.valor := '';
        end
        else if token.valor = '<FI>' then
        begin
          fmt := rvsFI;
          token.valor := '';
        end else if token.valor = '<FR>' then
        begin
          fmt := rvsFR;
          token.valor := '';
        end else if token.valor = '<FO>' then
        begin
          fmt := rvsFO;
          token.valor := '';
        end else if token.valor.ToLower = '<b>' then
        begin
          fmt := rvsBold;
          token.valor := '';
        end else if token.valor.ToLower = '<i>' then
        begin
          fmt := rvsItalic;
          token.valor := '';
        end else if token.valor.ToLower = '<u>' then
        begin
          fmt := rvsUnderline;
          token.valor := '';
        end else if token.valor.ToLower = '<s>' then
        begin
          fmt := rvsStrikeout;
          token.valor := '';
        end else if token.valor.ToLower = '<sup>' then
        begin
          fmt := rvsSuperscript;
          token.valor := '';
        end else if token.valor.ToLower = '<sub>' then
        begin
          fmt := rvsSubscript;
          token.valor := '';
        end else if token.valor = '<CM>' then
        begin
          if FVerseMode = vmParagraph then
            FFromNewLine := true;
          token.valor := '';
        end else if (token.valor = '</sub>')
                 or (token.valor = '</sup>')
                 or (token.valor = '</s>')
                 or (token.valor = '</u>')
                 or (token.valor = '</i>')
                 or (token.valor = '</b>')
                 or (token.valor = '<Fo>')
                 or (token.valor = '<Fr>')
                 or (token.valor = '<Fi>')
                 or (token.valor = '</b>') then
        begin
          fmt := rvsNormal;
          token.valor := '';
        end else if token.valor.ToLower = '<br/>' then
        begin
          token.valor := ' ';
        end else
          token.valor := '';
    end;
    if not token.valor.IsEmpty then
      AppendText(token.valor, fmt);
  end;
  FreeAndNil(FTokenizer);
end;

procedure TChapterView.HandleJump(Sender: TObject; id: Integer);
var
  i: Integer;
begin
  if id >= FJumps then { notes }
  begin
    ScrollTo(GetJumpPointY(FNoteJumps[id-FJumps]));
    exit;
  end;

  i := FVerseJumps.IndexOf(id);
  if i >= 0 then { verse }
  begin
    FProject.IrPara(SysUtils.Format('%d,%d,%d', [FProject.BookID, FProject.Chapter, i + 1]));
    exit;
  end;

  { note link }
  i := FNoteJumps.IndexOf(id);
  ScrollTo(GetJumpPointY(FJumps + i));
end;

procedure TChapterView.RenderNotes;
var
  i: integer;
begin
  if FChapterNotes.Count = 0 then
    exit;

  AddBreak;
  FFromNewLine := false;
  for i:=0 to FChapterNotes.Count-1 do
  begin
    AddFromNewLine(SysUtils.Format('%d ', [i + 1]), rvsJump2);
    RenderSpan(FChapterNotes[i]);
  end;
end;

procedure TChapterView.HandleVerseChange(Sender: TProjeto);
var
  verses: TStringList;
begin
  if not assigned(FProject) or (Width < 2) then
    exit;

  verses := Sender.ChapterText;
  try
    RenderChapter(verses, Sender.Verse);
  finally
    FreeAndNil(verses);
  end;
end;

procedure TChapterView.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  s: integer;
begin
  if (ssCtrl in Shift) and (Key in [VK_SUBTRACT, VK_OEM_MINUS]) and (Style.TextStyles[rvsNormal].Size > 1) then
  begin
    for s:=0 to Style.TextStyles.Count-1 do
      Style.TextStyles[s].Size := Style.TextStyles[s].Size-1;
    Format;
    Repaint;
  end
  else if (ssCtrl in Shift) and (Key in [VK_ADD, VK_OEM_PLUS]) then
  begin
    for s:=0 to Style.TextStyles.Count-1 do
      Style.TextStyles[s].Size := Style.TextStyles[s].Size+1;
    Format;
    Repaint;
  end;
end;

procedure TChapterView.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
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

procedure TChapterView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  {if ssCtrl in Shift then
  begin
    TRichMemo(Sender).ZoomFactor := Max(TRichMemo(Sender).ZoomFactor + Sign(WheelDelta)*0.1, 0.1);
    Handled := true;
  end;}
end;

procedure TChapterView.HandleSetFont(Sender: TObject);
var
  dialog: TFontDialog;
begin
  dialog := TFontDialog.Create(self);
  dialog.Font.Name := FFontName;
  dialog.Font.Size := FFontSize;
  if dialog.Execute then
  begin
    FontName := dialog.Font.Name;
    FontSize := dialog.Font.Size;
  end;
  dialog.Free;
end;

procedure TChapterView.HandleParagraphMode(Sender: TObject);
begin
  VerseMode := vmParagraph;
end;

procedure TChapterView.HandleVersePerLineMode(Sender: TObject);
begin
  VerseMode := vmVersePerLine;
end;

procedure TChapterView.SetProject(AValue: TProjeto);
begin
  if FProject = AValue then Exit;
  FProject := AValue;
  FProject.OnNewVerseSubscribe(@HandleVerseChange);
end;

procedure TChapterView.InitPopupMenu;
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

procedure TChapterView.InitStyles;
begin
  Style := TRVStyle.Create(Self);

  Style.TextStyles[rvsNormal].Size := FFontSize;

  rvsFI := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsItalic];
  Style.TextStyles[Style.TextStyles.Count-1].Color := clGrayText;

  rvsFO := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold];

  rvsFR := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Color := clRed;

  rvsBold := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold];

  rvsItalic := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsItalic];

  rvsUnderline := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsUnderline];

  rvsSuperscript := Style.AddTextStyle();

  rvsSubscript := Style.AddTextStyle();

  rvsBoldItalic := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold, fsItalic];

  rvsTS0 := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold, fsItalic];
  Style.TextStyles[Style.TextStyles.Count-1].Color := clGrayText;

  rvsTS1 := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold, fsItalic];

  rvsTS2 := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold, fsItalic];

  rvsTS3 := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsItalic];

  rvsStrikeout := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsStrikeOut];

  rvsVerseNumber := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold];
  Style.TextStyles[Style.TextStyles.Count-1].Color := clMaroon;

  rvsCurrentVerseNumber := Style.AddTextStyle();
  Style.TextStyles[Style.TextStyles.Count-1].Style := [fsBold, fsUnderline];
  Style.TextStyles[Style.TextStyles.Count-1].Color := clMaroon;

  Style.TextStyles[rvsJump1].Color := clMaroon;
  Style.TextStyles[rvsJump1].Style := [fsBold];
end;

procedure TChapterView.SetVerseMode(AValue: TViewMode);
begin
  if FVerseMode = AValue
    then Exit;

  FVerseMode := AValue;
  FParagraphModeItem.Enabled := FVerseMode <> vmParagraph;
  FVersePerLineItem.Enabled  := FVerseMode <> vmVersePerLine;
  HandleVerseChange(FProject);
end;

procedure TChapterView.AppendText(s: string; StyleNo: Integer);
begin
  if FFromNewLine then
    AddTextFromNewLine(s, StyleNo)
  else
    AddText(s, StyleNo);
  FFromNewLine:=false;
end;

procedure TChapterView.SetFontSize(AValue: integer);
var
  s: integer;
begin
  if (FFontSize = AValue) or (AValue < 1) then Exit;
  FFontSize := AValue;

  for s:=0 to Style.TextStyles.Count-1 do
    Style.TextStyles[s].Size := FFontSize;

  Format;
  Repaint;
end;

procedure TChapterView.SetFontName(AValue: string);
var
  s: integer;
begin
  if (FFontName = AValue) then Exit;
  FFontName := AValue;

  for s:=0 to Style.TextStyles.Count-1 do
    Style.TextStyles[s].FontName := FFontName;

  Format;
  Repaint;
end;

procedure TChapterView.SetEnabled(Value: Boolean);
begin
  if assigned(FProject) and not Value and Enabled then
  begin
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);
    FProject := nil;
  end;

  inherited SetEnabled(Value);
end;

constructor TChapterView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent       := TWinControl(AOwner);
  //OnKeyDown    := @HandleKeyDown;
  //OnMouseWheel := @HandleMouseWheel;
  { TRichMemo.GetText causes unsolicited scrolling, disabling it }
  //OnMouseMove  := @HandleMouseMove;
  OnJump          := @HandleJump;
  FFontName       := 'default';
  FFontSize       := 10;
  FProject        := nil;
  FVerseMode      := vmParagraph;
  FRxVerseHeading := RegexCreate('^((?:<TS[0-7]?>.*?<Ts>)*)(.*?)$', [rcoUTF8]);
  FChapterNotes   := TStringList.Create;
  FVerseJumps     := TIntegerList.Create;
  FNoteJumps      := TIntegerList.Create;
  FHint           := THintWindow.Create(Self);
  FHint.Font.Size := 10;
  FHint.Color     := $E0FFFF;
  InitPopupMenu;
  InitStyles;
end;

destructor TChapterView.Destroy;
begin
  FChapterNotes.Free;
  FVerseJumps.Free;
  FNoteJumps.Free;
  FHint.Free;
  FStyles.Free;
  if assigned(FProject) then
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);

  inherited Destroy;
end;

procedure TChapterView.RenderChapter(verses: TStringList; current: integer);
var
  verse: string;
  v: integer;
begin
  Clear;

  FirstJumpNo := 0;
  FNoteID := 1;
  FChapterNotes.Clear;
  FVerseJumps.Clear;
  FNoteJumps.Clear;
  FJumps := 0;

  FFromNewLine := false;

  v := 0;
  for verse in verses do
  begin
    Inc(v);
    RenderVerse(verse, v, v = current);
  end;

  RenderNotes;

  Format;
  Repaint;

  if current > 1 then
    ScrollTo(GetJumpPointY(FVerseJumps[current-1]));
end;

end.

