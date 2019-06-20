unit ChapterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, RichMemo, RichMemoUtils, Graphics, StrUtils,
  ONTTokenizer, LCLType, LazUTF8, Projeto, PCRE, Math, Character, Controls;

type

  { TChapterView }

  TChapterView = class(TRichMemo)
  private
    FProject: TProjeto;
    FRxVerseHeading: IRegex;
    FChapterNotes: TStringList;
    FNoteID: integer;
    FHint: THintWindow;

    procedure RenderVerse(txt: string; verse: string; isCurrent: boolean);
    procedure RenderSpan(txt: string; fmt: TFontParams);
    procedure RenderNotes;
    procedure HandleVerseChange(Sender: TProjeto);
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleLinkAction(Sender: TObject; ALinkAction: TLinkAction; const info: TLinkMouseInfo; LinkStart, LinkLen: Integer);
    procedure SetProject(AValue: TProjeto);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderChapter(verses: TStringList; current: integer);
    property Project: TProjeto read FProject write SetProject;
  end;

implementation

{ TChapterView }

procedure TChapterView.RenderVerse(txt: string; verse: string;
  isCurrent: boolean);
var
  fmt: TFontParams;
  pos: PtrInt;
  mtHeading: IMatch;
begin
  fmt := GetFontParams(Font);
  fmt.Size := 10;

  if Lines.Count > 0 then
    InsertFontText(Self, ' ', fmt);

  mtHeading := FRxVerseHeading.Match(txt);
  if not mtHeading.Success then
    raise Exception.Create(Format('Unmatched verse: <%s>', [txt]));

  { rendering titles before the verse number }
  RenderSpan(mtHeading.Groups[1].Value, fmt);
  txt := mtHeading.Groups[2].Value;

  if isCurrent then
  begin
    fmt.BkColor  := clLtGray;
    fmt.HasBkClr := true;
  end;

  { rendering verse number and making it a link }
  InsertFontText(Self, verse + ' ', fmt);
  pos := UTF8Length(Text) - UTF8Length(verse) - 1;
  SetLink(pos, UTF8Length(verse), true, verse);
  { rendering the verse text }
  RenderSpan(txt, fmt);
end;

procedure TChapterView.RenderSpan(txt: string; fmt: TFontParams);
var
  FTokenizer: TONTTokenizer;
  token: TTagSintagma;
begin
  FTokenizer := TONTTokenizer.Criar(txt);
  while FTokenizer.LerSintagma(token) <> tsNulo do
  begin
    case token.tipo of
      tsMetaDado:
        token.valor := '';
      tsEspaco, tsPontuacao, tsSintagma:
        ;
      tsTag:
        if token.valor.StartsWith('<TS') then
        begin
          fmt.Style := [fsBold, fsItalic];
          token.valor := IfThen(Lines.Count > 0, #13#10, '');
        end else if token.valor = '<Ts>' then
        begin
          fmt.Style := [];
          token.valor := #13;
        end
        else if token.valor.StartsWith('<RF') then
        begin
          token.valor := '';
          FTokenizer.LerAteTag(token, '<Rf>');
          token.valor := token.valor.Replace('<Rf>',  '');
          FChapterNotes.Add(token.valor);

          fmt.VScriptPos := vpSuperScript;
          InsertFontText(Self, Format(' %d ', [FNoteID]), fmt);
          fmt.VScriptPos := vpNormal;

          inc(FNoteID);
          token.valor := '';
        end
        else if token.valor = '<FI>' then
        begin
          fmt.Style := [fsItalic];
          fmt.Color := clGray;
          token.valor := '';
        end else if token.valor = '<Fi>' then
        begin
          fmt.Style := [];
          fmt.Color := clBlack;
          token.valor := '';
        end else if token.valor = '<FR>' then
        begin
          fmt.Color := clRed;
          token.valor := '';
        end else if token.valor = '<Fi>' then
        begin
          fmt.Color := clBlack;
          token.valor := '';
        end
        else if token.valor = '<FO>' then
        begin
          fmt.Style := [fsBold];
          token.valor := '';
        end else if token.valor = '<Fo>' then
        begin
          fmt.Style := [];
          token.valor := '';
        end
        else if token.valor.ToLower = '<b>' then
        begin
          Include(fmt.Style, fsBold);
          token.valor := '';
        end else if token.valor.ToLower = '</b>' then
        begin
          Exclude(fmt.Style, fsBold);
          token.valor := '';
        end
        else if token.valor.ToLower = '<i>' then
        begin
          Include(fmt.Style, fsItalic);
          token.valor := '';
        end else if token.valor.ToLower = '</i>' then
        begin
          Exclude(fmt.Style, fsItalic);
          token.valor := '';
        end
        else if token.valor.ToLower = '<u>' then
        begin
          Include(fmt.Style, fsUnderline);
          token.valor := '';
        end else if token.valor.ToLower = '</u>' then
        begin
          Exclude(fmt.Style, fsUnderline);
          token.valor := '';
        end
        else if token.valor.ToLower = '<s>' then
        begin
          Include(fmt.Style, fsStrikeOut);
          token.valor := '';
        end else if token.valor.ToLower = '</s>' then
        begin
          Exclude(fmt.Style, fsStrikeOut);
          token.valor := '';
        end
        else if token.valor.ToLower = '<sup>' then
        begin
          fmt.VScriptPos := vpSuperScript;
          token.valor := '';
        end else if token.valor.ToLower = '</sup>' then
        begin
          fmt.VScriptPos := vpNormal;
          token.valor := '';
        end
        else if token.valor.ToLower = '<sub>' then
        begin
          fmt.VScriptPos := vpSubScript;
          token.valor := '';
        end else if token.valor.ToLower = '</sub>' then
        begin
          fmt.VScriptPos := vpNormal;
          token.valor := '';
        end
        else if token.valor = '<CM>' then
        begin
          token.valor := #13#10;
        end else if token.valor.ToLower = '<br/>' then
        begin
          token.valor := ' ';
        end else
          token.valor := '';
    end;
    if not token.valor.IsEmpty then
      InsertFontText(Self, token.valor, fmt);
  end;
  FreeAndNil(FTokenizer);
end;

procedure TChapterView.RenderNotes;
var
  i: integer;
  fmt: TFontParams;
  par: TParaNumbering;
  start: PtrInt;
begin
  fmt := GetFontParams(Font);
  fmt.Size := 9;

  InsertFontText(Self, #13#10#13#10, fmt);

  GetParaNumbering(0, par);
  par.NumberStart := 1;
  par.Indent      := 10;
  par.Style       := pnNumber;

  for i:=0 to FChapterNotes.Count-1 do
  begin
    start := UTF8Length(Text) + 1;
    RenderSpan(Format('%s<CM>', [FChapterNotes[i]]), fmt);
    SetParaNumbering(start, UTF8Length(Text) - start, par);
  end;
end;

procedure TChapterView.HandleVerseChange(Sender: TProjeto);
var
  verses: TStringList;
begin
  if {not assigned(FProject) or} Width < 2 then
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
begin
  if (ssCtrl in Shift) and (Key in [VK_SUBTRACT, VK_OEM_MINUS]) and (ZoomFactor > 0.1) then
    ZoomFactor := ZoomFactor - 0.1
  else if (ssCtrl in Shift) and (Key in [VK_ADD, VK_OEM_PLUS]) then
    ZoomFactor := ZoomFactor + 0.1
  else if (ssCtrl in Shift) and (Key in [VK_A]) then
  begin
    SelectAll;
    Key := 0;
  end;
end;

procedure TChapterView.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  iCharIndex, iCharOffset, i, j: Integer;
  s, note: string;
  attr: TFontParams;
  rect: TRect;
  pos: TPoint;
begin
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
  FHint.ActivateHint(Rect, note);
end;

procedure TChapterView.HandleLinkAction(Sender: TObject;
  ALinkAction: TLinkAction; const info: TLinkMouseInfo; LinkStart,
  LinkLen: Integer);
var
  Verse: string;
begin
  Verse := GetText(LinkStart, LinkLen);
  FProject.IrPara(Format('%d,%d,%s', [FProject.BookID, FProject.Chapter, Verse]));
end;

procedure TChapterView.SetProject(AValue: TProjeto);
begin
  if FProject = AValue then Exit;
  FProject := AValue;
  FProject.OnNovoVersiculo := @HandleVerseChange;
end;

constructor TChapterView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent       := TWinControl(AOwner);
  ReadOnly     := true;
  OnKeyDown    := @HandleKeyDown;
  OnLinkAction := @HandleLinkAction;
  //OnMouseMove  := @HandleMouseMove;  { TRichMemo.GetText causes unsolicited scrolling, disabling it }
  FRxVerseHeading := RegexCreate('^((?:<TS[0-7]?>.*?<Ts>)*)(.*?)$', [rcoUTF8]);
  FChapterNotes := TStringList.Create;
  FHint := THintWindow.Create(Self);
  FHint.Font.Size := 10;
  FHint.Color := $E0FFFF;
end;

destructor TChapterView.Destroy;
begin
  FChapterNotes.Free;
  FHint.Free;
  inherited Destroy;
end;

procedure TChapterView.RenderChapter(verses: TStringList; current: integer);
var
  verse: string;
  v: integer;
begin
  Lines.BeginUpdate;
  Rtf := ''; { RichMemo1.Lines.Clear doesn't work on Linux }

  FNoteID := 1;
  FChapterNotes.Clear;
  FChapterNotes := TStringList.Create;

  v := 0;
  for verse in verses do
  begin
    Application.ProcessMessages;
    Inc(v);
    RenderVerse(verse, v.ToString, v = current);
  end;
  RenderNotes;
  Lines.EndUpdate;

  { TRichMemo bug: Current zoom factor doesn't apply automatically }
  ZoomFactor := ZoomFactor;
end;

end.

