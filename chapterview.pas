unit ChapterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Graphics, ONTTokenizer, LCLType, HTMLColors,
  LazUTF8, Projeto, PCRE, Controls, Menus, fgl, Dialogs, Math, Contnrs, KMemo,
  KEditCommon, KGraphics, KDialogs, formInterlinearVerseRules,
  LCLTranslator, LazLogger, StdCtrls, unitskins;

type

  TBlockRange = record
    first, last: integer;
  end;

  TChapterView = class;

  { TRawChapterMemo }

  TRawChapterMemo = class(TMemo)
  private
    FChapterView: TChapterView;
    procedure HandleOnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure HandleOnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent; ChapterView: TChapterView); overload;
    destructor Destroy; override;

    procedure Show(aText: string);
    procedure Hide;
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
    property Id: integer read FId write FId;
    property LinkText: string read FLinkText write FLinkText;
    property Verse: integer read FVerse write FVerse;
    property Text: string read FText write FText;
    property Range: TBlockRange read FRange write FRange;
    property FirstBlock: integer read FRange.first write FRange.first;
    property LastBlock: integer read FRange.last write FRange.last;
  end;

  { TNoteWindow }

  TNoteWindow = class(THintWindow)
  private
    FNoteView: TChapterView;
    function GetScrollBarsVisible: boolean;
    procedure InitNoteView;
    procedure SetNoteText(const AText: string; const AStyle: TKMemoTextStyle);
    procedure HandlePopupMouseEnter(Sender: TObject);
    procedure HandlePopupMouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(const AHint: string; AStyle: TKMemoTextStyle); overload;
    property ScrollBarsVisible: boolean read GetScrollBarsVisible;
  end;

  TNoteList = specialize TFPGList<TNoteInfo>;

  { TChapterView }

  TChapterView = class(TKMemo)
  private
    FLoadingChapter: boolean;
    FFontName: string;
    FFontSize: integer;
    FPrintMode: boolean;
    FProject: TProjeto;
    FRxVerseHeading: IRegex;
    FNoteID: integer;
    FHint: TNoteWindow;
    FSetFontItem: TMenuItem;
    FParagraphModeItem: TMenuItem;
    FVerseRulesItem: TMenuItem;
    FInterlinearModeItem: TMenuItem;
    FInterlinearModeInterItem: TMenuItem;
    FInterlinearModeIntraItem: TMenuItem;
    FPrintChapterItem: TMenuItem;
    FStyleStack: TObjectStack;
    FVerseRanges: array of TBlockRange;
    FCurrentVerse: integer;
    FNotes: TNoteList;
    FHideVerseNumber: boolean;
    FRawMemo: TRawChapterMemo;
    FParaLeftIndent: LongInt;
    FParaFirstIndent: LongInt;
    FParaTopPadding: LongInt;
    FParaBottomPadding: LongInt;
    FParaHAlign: TKHAlign;
    FJustClosedHeading: Boolean;

    function GetCurrentStyle: TKMemoTextStyle;
    function GetInterlinearMode: TInterlinearMode;
    function GetParagraphMode: TParagraphMode;
    function GetTextType: TTipoTextoBiblico;
    procedure CloseParagraphBlock(cont: TKMemoBlocks);
    procedure ExtractLeadingParagraphPrefix(const AText: string; out ALeadingPrefix, ARemaining: string);
    procedure RenderChapter(verses: TStringList; current: integer);
    procedure RenderChapterHeader;
    procedure RenderChapterFooter;
    procedure RenderVerse(txt: string);
    procedure RenderVerseNumber(number: integer);
    procedure RenderSpan(cont: TKMemoBlocks; txt: string);
    procedure RenderNote(link: string; note: string);
    procedure RenderNotes;
    procedure RenderInterlinearBlock(cont: TKMemoBlocks; blockText: string);
    procedure RenderInterlinearUnit(cont: TKMemoBlocks; translation: string);
    procedure HandleReferenceClick(sender: TObject);
    procedure HandleNoteLinkClick(sender: TObject);
    procedure HandleNoteClick(sender: TObject);
    procedure HandleVerseChange(Sender: TProjeto);
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandlePopupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleDoubleClick(Sender: TObject);
    procedure HandleContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure HandleSetFont(Sender: TObject);
    procedure HandlePrintChapter(Sender: TObject);
    procedure HandleParagraphMode(Sender: TObject);
    procedure HandleVerseRules(Sender: TObject);
    procedure HandleInterlinearMode(Sender: TObject);
    procedure HandleIntralinearMode(Sender: TObject);
    procedure SetInterlinearMode(AValue: TInterlinearMode);
    procedure SetPrintMode(AValue: boolean);
    procedure SetProject(AValue: TProjeto);
    procedure InitPopupMenu;
    procedure SetParagraphMode(AValue: TParagraphMode);
    procedure SetFontSize(AValue: integer);
    procedure SetFontName(AValue: string);
    function PushNewStyle: TKMemoTextStyle;
    function PushInheritedStyle: TKMemoTextStyle;
    procedure PopStyle;
    procedure HighlightRange(range: TBlockRange; bgcolor: TColor);
    procedure ResetStyleStack;
    function GetPreviousParagraphBlock: TKMemoParagraph;
    procedure SetTextType(AValue: TTipoTextoBiblico);
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Translate;
    procedure LoadChapter;
    procedure ApplySkin(const ASkin: TSkinColors);
    property Project: TProjeto read FProject write SetProject;
    property TextType: TTipoTextoBiblico read GetTextType write SetTextType;
    property ParagraphMode: TParagraphMode read GetParagraphMode write SetParagraphMode;
    property InterlinearMode: TInterlinearMode read GetInterlinearMode write SetInterlinearMode;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontName: string read FFontName write SetFontName;
    property CurrentStyle: TKMemoTextStyle read GetCurrentStyle;
    property HideVerseNumber: boolean read FHideVerseNumber write FHideVerseNumber default false;
    property PrintMode: boolean read FPrintMode write SetPrintMode;
  end;

const
  NonBibleTextColor = $be7c41;
  NoteLinkColor = $4080ff;
  ChapterNumberColor = $2cba7d;
  DefaultFirstIndent = 20;

resourcestring
  SSetFont = 'Choose &font...';
  SParagraphMode = '&Paragraph mode';
  SPrintChapter = 'Print chapter';
  SVerseRules = 'Verse &rules...';
  SInterlinearMode = 'Interlinear layout...';
  SInterlinearModeInter = 'Interlinear';
  SInterlinearModeIntra = 'Intralinear';
  SPreviousChapter = 'Previous chapter';
  SNextChapter = 'Next chapter';
  SChapterBeginning = 'Beginning of chapter';

implementation

{ TRawChapterMemo }

procedure TRawChapterMemo.HandleOnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Hide;
end;

procedure TRawChapterMemo.HandleOnMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Font.Size := Font.Size + Sign(WheelDelta) * 1;
end;

constructor TRawChapterMemo.Create(AOwner: TComponent; ChapterView: TChapterView);
begin
  inherited Create(nil);

  Visible    := False;
  Parent     := TWinControl(AOwner);
  FChapterView := ChapterView;
  Align      := alClient;
  ScrollBars := ssAutoVertical;
  ParentFont := False;
  Font.Name  := 'Courier New';
  ReadOnly   := true;
  OnKeyDown  := @HandleOnKeyDown;
  OnMouseWheel := @HandleOnMouseWheel;
end;

destructor TRawChapterMemo.Destroy;
begin
  inherited Destroy;
end;

procedure TRawChapterMemo.Show(aText: string);
begin
  Font.Size := FChapterView.FontSize;
  Color := CurrentSkin.WindowBg;
  Font.Color := CurrentSkin.WindowText;
  Text := aText;
  BringToFront;
  Visible := True;
  SetFocus;
end;

procedure TRawChapterMemo.Hide;
begin
  Visible := false;
end;

{ TNoteWindow }

procedure TNoteWindow.InitNoteView;
begin
  InsertControl(TChapterView.Create(nil));
  FNoteView := Controls[0] as TChapterView;
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

function TNoteWindow.GetScrollBarsVisible: boolean;
begin
  result := ((FNoteView.Height - FNoteView.ClientRect.Bottom) > 10) or
            ((FNoteView.Width - FNoteView.ClientRect.Right) > 10) or
            (FNoteView.ClientRect.Height = 10);
end;

procedure TNoteWindow.SetNoteText(const AText: string;
  const AStyle: TKMemoTextStyle);
begin
  FNoteView.Blocks.LockUpdate;
  Width := 500;
  FNoteView.Clear(false);
  FNoteView.TextStyle.Assign(AStyle);
  FNoteView.TextStyle.Font.Size := FNoteView.TextStyle.Font.Size - 1;
  FNoteView.RenderSpan(FNoteView.Blocks, AText);
  FNoteView.Background.Color := CurrentSkin.NoteBg;
  FNoteView.TextStyle.Font.Color := CurrentSkin.NoteText;
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

procedure TNoteWindow.ActivateHint(const AHint: string; AStyle: TKMemoTextStyle);
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
  Rect.Right := Rect.Left + Rect.Right + 5;
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

{ TChapterView }

procedure TChapterView.CloseParagraphBlock(cont: TKMemoBlocks);
var
  par: TKMemoParagraph;
begin
  par := cont.AddParagraph();
  par.ParaStyle.LeftPadding := FParaLeftIndent;
  par.ParaStyle.FirstIndent := FParaFirstIndent;
  par.ParaStyle.TopPadding := FParaTopPadding;
  par.ParaStyle.BottomPadding := FParaBottomPadding;
  par.ParaStyle.HAlign := FParaHAlign;
end;

procedure TChapterView.ExtractLeadingParagraphPrefix(const AText: string;
  out ALeadingPrefix, ARemaining: string);
var
  rest: string;
  pEnd: Integer;
begin
  ALeadingPrefix := '';
  rest := AText;

  while Length(rest) > 0 do
  begin
    if AnsiStartsText('<TS', rest) then
    begin
      pEnd := Pos('<Ts>', rest);
      if pEnd = 0 then pEnd := Pos('<TS>', UpperCase(rest));
      if pEnd > 0 then
      begin
        ALeadingPrefix := ALeadingPrefix + Copy(rest, 1, pEnd + 3);
        Delete(rest, 1, pEnd + 3);
        if AnsiStartsText('<_CM>', rest) then
        begin
          ALeadingPrefix := ALeadingPrefix + Copy(rest, 1, 5);
          Delete(rest, 1, 5);
        end;
        continue;
      end;
    end;

    if AnsiStartsText('<CM>', rest) or AnsiStartsText('<CI>', rest) or AnsiStartsText('<CL>', rest) then
    begin
      ALeadingPrefix := ALeadingPrefix + Copy(rest, 1, 4);
      Delete(rest, 1, 4);
      continue;
    end;

    if AnsiStartsText('<_CM>', rest) then
    begin
      ALeadingPrefix := ALeadingPrefix + Copy(rest, 1, 5);
      Delete(rest, 1, 5);
      continue;
    end;

    if (Length(rest) >= 5) and (AnsiStartsText('<PI', rest) or AnsiStartsText('<PF', rest)) and
       (rest[4] in ['0'..'7']) and (rest[5] = '>') then
    begin
      ALeadingPrefix := ALeadingPrefix + Copy(rest, 1, 5);
      Delete(rest, 1, 5);
      continue;
    end;

    if AnsiStartsText('<PI>', rest) then
    begin
      ALeadingPrefix := ALeadingPrefix + Copy(rest, 1, 4);
      Delete(rest, 1, 4);
      continue;
    end;

    if AnsiStartsText('<center>', rest) then
    begin
      ALeadingPrefix := ALeadingPrefix + Copy(rest, 1, 8);
      Delete(rest, 1, 8);
      continue;
    end;

    break;
  end;

  ARemaining := rest;
end;

procedure TChapterView.RenderVerse(txt: string);
var
  leadingPrefix, remainingTxt: string;
begin
  ResetStyleStack;

  ExtractLeadingParagraphPrefix(txt, leadingPrefix, remainingTxt);

  { Render leading headings, paragraph breaks (<CM>/<CI>/<CL>), and indents (<PI>/<PF>/<center>) before verse number }
  if not leadingPrefix.IsEmpty then
    RenderSpan(Blocks, leadingPrefix);

  if Assigned(Blocks.LastBlock) and (Blocks.LastBlock.ClassName <> 'TKMemoParagraph') then
    Blocks.AddTextBlock(' '); // space between verses in same paragraph

  FVerseRanges[FCurrentVerse].first := Blocks.Count;

  { rendering verse number }
  RenderVerseNumber(FCurrentVerse);

  { rendering the verse text }
  RenderSpan(Blocks, remainingTxt);

  FVerseRanges[FCurrentVerse].last := Max(FVerseRanges[FCurrentVerse].first, Blocks.Count - 1);

  if Assigned(FProject) and (FProject.ParagraphMode = pmNoParagraphs) then
  begin
    if Assigned(Blocks.LastBlock) and (Blocks.LastBlock.ClassName <> 'TKMemoParagraph') then
      CloseParagraphBlock(Blocks);
    FParaTopPadding := 0;
    FParaBottomPadding := 0;
    FParaLeftIndent := 0;
    FParaFirstIndent := 0;
    FParaHAlign := halLeft;
  end;
end;

procedure TChapterView.RenderVerseNumber(number: integer);
var
  b: TKMemoBlocks;
begin
  if FHideVerseNumber or not Assigned(FProject) then exit;

  if (FProject.ChapterViewText = tbInterlinear) and (FProject.InterlinearMode = imInterlinear) then
    with Blocks.AddContainer do
    begin
      Position := mbpText;
      FixedWidth := true;
      BlockStyle.RightPadding := 5;
      b := Blocks;
    end
  else
    b := Blocks;

  with b.AddHyperlink(Format('%d ', [number]), Format('%d,%d,%d', [FProject.BookID, FProject.Chapter, number])) do
  begin
    OnClick := @HandleReferenceClick;
    TextStyle.Font.Name  := 'default';
    TextStyle.Font.Size  := Max(6, FFontSize - 2);
    TextStyle.Font.Style := [fsBold];
    TextStyle.Font.Color := CurrentSkin.VerseNumColor;
  end;
end;

function TChapterView.GetCurrentStyle: TKMemoTextStyle;
begin
  result := TKMemoTextStyle(FStyleStack.Peek);
end;

function TChapterView.GetInterlinearMode: TInterlinearMode;
begin
  result := FProject.InterlinearMode;
end;

function TChapterView.GetParagraphMode: TParagraphMode;
begin
  if Assigned(FProject) then
    result := FProject.ParagraphMode
  else
    result := pmParagraph;
end;

function TChapterView.GetTextType: TTipoTextoBiblico;
begin
  result := FProject.ChapterViewText;
end;

procedure TChapterView.RenderSpan(cont: TKMemoBlocks; txt: string);
var
  FTokenizer: TONTTokenizer;
  token: TTagSintagma;
  prop, rawTag, lowerTag, rxRef: string;
  chunk: string;
  size, lvl: integer;
begin
  chunk := '';
  FTokenizer := TTokenizerFactory.CreatePreferredTokenizer(txt);

  cont.LockUpdate;
  try
    while FTokenizer.LerSintagma(token) <> tsNulo do
    begin
      case token.tipo of
        tsMetaDado:
          ;
        tsEspaco:
          if token.valor <> '|' then
          begin
            chunk := chunk + token.valor;
            FJustClosedHeading := False;
          end;
        tsPontuacao, tsSintagma:
        begin
          chunk := chunk + token.valor;
          FJustClosedHeading := False;
        end;
        tsTag:
        begin
          if chunk.Length > 0 then
          begin
            cont.AddTextBlock(chunk).TextStyle.Assign(CurrentStyle);
            chunk := '';
          end;

          rawTag := token.valor;
          lowerTag := LowerCase(rawTag);

          if AnsiStartsText('<TS', rawTag) and (lowerTag <> '<ts>') and (rawTag <> '<Ts>') then { section heading beginning }
          begin
            if assigned(cont.LastBlock) and (cont.LastBlock.ClassName <> 'TKMemoParagraph') then
              CloseParagraphBlock(cont);

            FParaTopPadding := 8;
            FParaBottomPadding := 4;
            FParaFirstIndent := 0;
            FParaLeftIndent := 0;
            FParaHAlign := halLeft;
            FJustClosedHeading := False;

            with PushNewStyle do
            begin
              case rawTag[4] of
                '>','0','1':
                begin
                  Font.Size := Font.Size + 2;
                  Font.Style := [fsBold];
                  Font.Color := CurrentSkin.HeaderColor;
                end;
                '2':
                begin
                  Font.Size := Font.Size + 1;
                  Font.Style := [fsBold, fsItalic];
                  Font.Color := CurrentSkin.HeaderColor;
                end;
                '3':
                begin
                  Font.Style := [fsBold, fsItalic];
                  Font.Color := CurrentSkin.WindowTextDim;
                end;
              end;
            end;
          end
          else if (rawTag = '<Ts>') or (lowerTag = '<ts>') then { section heading end }
          begin
            ResetStyleStack;
            CloseParagraphBlock(cont);
            FParaTopPadding := 0;
            FParaBottomPadding := 0;
            FParaFirstIndent := IfThen(GetParagraphMode = pmParagraph, DefaultFirstIndent, 0);
            FParaLeftIndent := 0;
            FParaHAlign := halLeft;
            FJustClosedHeading := True;
          end
          else if SameText(rawTag, '<_CM>') then { heading paragraph fallback }
          begin
            if FJustClosedHeading then
              FJustClosedHeading := False
            else
            begin
              if assigned(cont.LastBlock) and (cont.LastBlock.ClassName <> 'TKMemoParagraph') then
                CloseParagraphBlock(cont);
              FParaTopPadding := 8;
              FParaBottomPadding := 0;
              FParaLeftIndent := 0;
              FParaFirstIndent := 0;
              FParaHAlign := halLeft;
            end;
          end
          else if SameText(rawTag, '<CM>') then { spaced paragraph break (Double Return) }
          begin
            FJustClosedHeading := False;
            if assigned(cont.LastBlock) and (cont.LastBlock.ClassName <> 'TKMemoParagraph') then
              CloseParagraphBlock(cont);
            FParaTopPadding := 8;
            FParaBottomPadding := 0;
            FParaLeftIndent := 0;
            FParaFirstIndent := 0;
            FParaHAlign := halLeft;
          end
          else if SameText(rawTag, '<CI>') then { tight paragraph break (Single Return) }
          begin
            FJustClosedHeading := False;
            if assigned(cont.LastBlock) and (cont.LastBlock.ClassName <> 'TKMemoParagraph') then
              CloseParagraphBlock(cont);
            FParaTopPadding := 0;
            FParaBottomPadding := 0;
            FParaLeftIndent := 0;
            FParaFirstIndent := 0;
            FParaHAlign := halLeft;
          end
          else if SameText(rawTag, '<CL>') or SameText(rawTag, '<br>') or SameText(rawTag, '<br/>') then { line break inside paragraph }
          begin
            FJustClosedHeading := False;
            if assigned(cont.LastBlock) and (cont.LastBlock.ClassName <> 'TKMemoParagraph') then
              CloseParagraphBlock(cont);
            FParaTopPadding := 0;
            FParaBottomPadding := 0;
            FParaFirstIndent := 0;
            // Preserves FParaLeftIndent and FParaHAlign inside the same paragraph!
          end
          else if (rawTag = '<Pi>') or SameText(rawTag, '<PI0>') then { reset whole-paragraph indent }
          begin
            FParaLeftIndent := 0;
          end
          else if SameText(rawTag, '<PI>') or SameText(rawTag, '<PI1>') then
          begin
            FParaLeftIndent := 24;
            if FParaFirstIndent = DefaultFirstIndent then
              FParaFirstIndent := 0;
          end
          else if (Length(rawTag) = 5) and AnsiStartsText('<PI', rawTag) and (rawTag[4] in ['2'..'7']) and (rawTag[5] = '>') then
          begin
            lvl := Ord(rawTag[4]) - Ord('0');
            FParaLeftIndent := lvl * 22;
            if FParaFirstIndent = DefaultFirstIndent then
              FParaFirstIndent := 0;
          end
          else if (Length(rawTag) = 5) and AnsiStartsText('<PF', rawTag) and (rawTag[4] in ['0'..'7']) and (rawTag[5] = '>') then
          begin
            lvl := Ord(rawTag[4]) - Ord('0');
            FParaFirstIndent := lvl * 20;
          end
          else if SameText(rawTag, '<center>') then
          begin
            FParaHAlign := halCenter;
            if FParaFirstIndent = DefaultFirstIndent then
              FParaFirstIndent := 0;
          end
          else if rawTag.StartsWith('<RF') then { translator note }
            RenderNote(FTokenizer.LerPropriedadeTag('q', token), FTokenizer.ReadUntilExclusive('<Rf>'))
          else if AnsiStartsText('<RX ', rawTag) then { cross-reference }
          begin
            rxRef := Trim(Copy(rawTag, 5, Length(rawTag) - 5));
            if Pos(' ', rxRef) > 0 then
              rxRef := Copy(rxRef, 1, Pos(' ', rxRef) - 1);
            with cont.AddTextBlock(' [xref: ' + rxRef + '] ') do
            begin
              TextStyle.Assign(CurrentStyle);
              TextStyle.Font.Color := NoteLinkColor;
              TextStyle.Font.Size := Max(6, FFontSize - 2);
              TextStyle.ScriptPosition := tpoSuperscript;
            end;
          end
          else if rawTag = '<Q>' then { interlinear block }
            RenderInterlinearBlock(cont, FTokenizer.ReadUntilExclusive('<q>'))
          else if rawTag = '<E>' then { interlinear English block }
            RenderInterlinearUnit(cont, FTokenizer.ReadUntilExclusive('<e>'))
          else if rawTag = '<T>' then { interlinear translation block }
            RenderInterlinearUnit(cont, FTokenizer.ReadUntilExclusive('<t>'))
          else if rawTag = '<X>' then { interlinear transliteration block }
            RenderInterlinearUnit(cont, FTokenizer.ReadUntilExclusive('<x>'))
          else if rawTag = '<H>' then { interlinear Hebrew block }
            RenderInterlinearUnit(cont, FTokenizer.ReadUntilExclusive('<h>'))
          else if rawTag = '<G>' then { interlinear Greek block }
            RenderInterlinearUnit(cont, FTokenizer.ReadUntilExclusive('<g>'))
          else if rawTag = '<FI>' then { added word(s) }
            with PushInheritedStyle do
            begin
              Font.Style := Font.Style + [fsItalic];
              Font.Color := CurrentSkin.WindowTextDim;
            end
          else if rawTag = '<FR>' then { Jesus word(s) }
          begin
            if CurrentSkin.IsDark then
              PushInheritedStyle.Font.Color := MakeRGB(244, 92, 92)
            else
              PushInheritedStyle.Font.Color := clRed;
          end
          else if rawTag = '<FO>' then { Old Testament quotation }
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsBold]
          else if rawTag = '<FU>' then { theWord Underline }
            PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsUnderline]
          else if (rawTag = '<Fi>') or (rawTag = '<Fr>') or (rawTag = '<Fo>') or (rawTag = '<Fu>') then
            PopStyle
          else
          begin
            if lowerTag.StartsWith('<font ') then { font change }
            begin
              PushInheritedStyle;
              if lowerTag.Contains(' color=') then
                CurrentStyle.Font.Color := HTML2Color(FTokenizer.LerPropriedadeTag('color', token));
              if lowerTag.Contains(' size=') then
              begin
                prop := FTokenizer.LerPropriedadeTag('size', token);
                if (Length(prop) > 0) and (prop.Chars[0] in ['-', '+']) then
                  size := CurrentStyle.Font.Size + StrToIntDef(prop, 0)
                else
                  size := StrToIntDef(prop, CurrentStyle.Font.Size);
                CurrentStyle.Font.Size := Max(6, size);
              end;
            end
            else if lowerTag = '<b>' then { bold }
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsBold]
            else if lowerTag = '<i>' then { italic }
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsItalic]
            else if lowerTag = '<u>' then { underline }
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsUnderline]
            else if lowerTag = '<s>' then { strikeout }
              PushInheritedStyle.Font.Style := CurrentStyle.Font.Style + [fsStrikeOut]
            else if lowerTag = '<sup>' then { superscript }
              PushInheritedStyle.ScriptPosition := tpoSuperscript
            else if lowerTag = '<sub>' then { subscript }
              PushInheritedStyle.ScriptPosition := tpoSubscript
            else if (lowerTag = '</sub>')
                 or (lowerTag = '</sup>')
                 or (lowerTag = '</s>')
                 or (lowerTag = '</u>')
                 or (lowerTag = '</i>')
                 or (lowerTag = '</b>')
                 or (lowerTag = '<fo>')
                 or (lowerTag = '<fr>')
                 or (lowerTag = '<fi>')
                 or (lowerTag = '<fu>')
                 or (lowerTag = '</font>') then
              PopStyle;
          end;
        end;
      end;
    end;

    if chunk.Length > 0 then
      cont.AddTextBlock(chunk).TextStyle.Assign(CurrentStyle);
  finally
    FreeAndNil(FTokenizer);
    cont.UnLockUpdate;
  end;
end;

procedure TChapterView.RenderNote(link: string; note: string);
begin
  if link.IsEmpty then
    link := FNoteID.ToString;

  FNotes.Add(TNoteInfo.Create(FNoteID, FCurrentVerse, note, link));
  if assigned(Blocks.LastBlock) and not Blocks.LastBlock.Text.EndsWith(' ') then
    link := ' ' + link;

  with Blocks.AddHyperlink(link, (FNoteID-1).ToString) do
  begin
    TextStyle.Assign(TextStyle);
    TextStyle.Font.Color := NoteLinkColor;
    TextStyle.Font.Style := [fsItalic];
    TextStyle.Font.Size := TextStyle.Font.Size + 1;
    TextStyle.ScriptPosition := tpoSuperscript;
    OnClick := @HandleNoteLinkClick;
  end;
  Inc(FNoteID);
end;

procedure TChapterView.RenderNotes;
var
  note: TNoteInfo;
begin
  if FNotes.Count = 0 then
    exit;

  ResetStyleStack;
  CurrentStyle.Font.Size := max(FFontSize-2, 2);

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
    RenderSpan(Blocks, note.Text);
    note.LastBlock := Blocks.Count-1;
    Blocks.AddParagraph();
  end;
end;

procedure TChapterView.RenderInterlinearBlock(cont: TKMemoBlocks;
  blockText: string);
var
  b: TKMemoContainer;
begin
  b := cont.AddContainer();
  b.Position := mbpText;
  b.FixedWidth := true;
  b.BlockStyle.RightPadding := 5;

  RenderSpan(b.Blocks, stringReplace(blockText, ' ', #8239, [rfReplaceAll]));
end;

procedure TChapterView.RenderInterlinearUnit(cont: TKMemoBlocks;
  translation: string);
begin
  if cont.Count > 0 then
    cont.AddParagraph();
  RenderSpan(cont, '<font color="#417cbe">' + translation + '</font>');
end;

procedure TChapterView.HandleReferenceClick(sender: TObject);
begin
  FProject.GoToReference(TKMemoHyperlink(Sender).URL);
end;

procedure TChapterView.HandleNoteLinkClick(sender: TObject);
var
  note: TNoteInfo;
  c: integer = 0;
begin
  note := FNotes[TKMemoHyperlink(Sender).URL.ToInteger];
  repeat
    inc(c);
    FHint.ActivateHint(note.Text, TextStyle);
  until not FHint.ScrollBarsVisible or (c = 2);
end;

procedure TChapterView.HandleNoteClick(sender: TObject);
var
  note: TNoteInfo;
begin
  note := FNotes[TKMemoHyperlink(Sender).URL.ToInteger];

  if CurrentSkin.IsDark then
    HighlightRange(note.Range, CurrentSkin.NoteBg)
  else
    HighlightRange(note.Range, clDefault);
  HighlightRange(FVerseRanges[note.Verse], CurrentSkin.VerseHighlightBg);
end;

procedure TChapterView.HandleVerseChange(Sender: TProjeto);
var
  verses: TStringList;
begin
  if not assigned(FProject) or (Width < 2) or FLoadingChapter then
    exit;

  FLoadingChapter := true;
  try
    verses := Sender.GetChapterText;
    try
      RenderChapter(verses, Sender.Verse);
    finally
      FreeAndNil(verses);
    end;
  finally
    FLoadingChapter := false;
  end;
end;

procedure TChapterView.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key in [VK_SUBTRACT, VK_OEM_MINUS]) and (FFontSize > 1) then
  begin
    FFontSize := FFontSize-1;
    LoadChapter;
  end
  else if (ssCtrl in Shift) and (Key in [VK_ADD, VK_OEM_PLUS]) then
  begin
    FFontSize := FFontSize+1;
    LoadChapter;
  end;
end;

procedure TChapterView.HandlePopupKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    (Parent as THintWindow).Hide;
end;

procedure TChapterView.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TChapterView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    Blocks.LockUpdate;
    FontSize := FFontSize + Sign(WheelDelta) * 1;
    LoadChapter;
    Blocks.UnlockUpdate;
    Handled := true;
  end;
end;

procedure TChapterView.HandleDoubleClick(Sender: TObject);
var
  verses: TStringList;
begin
  if not assigned(FProject) then exit;

  verses := FProject.GetChapterText;
  try
    FRawMemo.Show(verses.Text);
  finally
    verses.Free;
  end;
end;

procedure TChapterView.HandleContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  if not assigned(FProject) then
  begin
    Handled := true;
    exit;
  end;

  FVerseRulesItem.Visible      := GetTextType = tbInterlinear;
  FInterlinearModeItem.Visible := GetTextType = tbInterlinear;
end;

procedure TChapterView.HandleSetFont(Sender: TObject);
var
  dialog: TFontDialog;
begin
  dialog := TFontDialog.Create(self);
  dialog.Options := [fdNoStyleSel];
  dialog.Font.Name := FFontName;
  dialog.Font.Size := FFontSize;
  if dialog.Execute then
  begin
    Blocks.LockUpdate;
    FontName := dialog.Font.Name;
    FontSize := dialog.Font.Size;
    LoadChapter;
    Blocks.UnlockUpdate;
  end;
  dialog.Free;
end;

procedure TChapterView.HandlePrintChapter(Sender: TObject);
var
  setup: TKPrintSetupDialog;
begin
  setup := TKPrintSetupDialog.Create(nil);
  try
    PrintMode := true;
    setup.Control := self;
    setup.Execute;
  finally
    setup.Free;
    PrintMode := false;
  end;
end;

procedure TChapterView.HandleParagraphMode(Sender: TObject);
begin
  if ParagraphMode = pmParagraph then
    ParagraphMode := pmNoParagraphs
  else
    ParagraphMode := pmParagraph;
end;

procedure TChapterView.HandleVerseRules(Sender: TObject);
begin
  frmInterlinearVerseRules.Project := FProject;
  if frmInterlinearVerseRules.ShowModal = mrOK then
    LoadChapter;
end;

procedure TChapterView.HandleInterlinearMode(Sender: TObject);
begin
  InterlinearMode := imInterlinear;
end;

procedure TChapterView.HandleIntralinearMode(Sender: TObject);
begin
  InterlinearMode := imIntralinear;
end;

procedure TChapterView.SetInterlinearMode(AValue: TInterlinearMode);
begin
  FInterlinearModeInterItem.Checked := AValue = imInterlinear;
  FInterlinearModeIntraItem.Checked := AValue = imIntralinear;

  if FProject.InterlinearMode = AValue then Exit;
  FProject.InterlinearMode := AValue;

  LoadChapter;
end;

procedure TChapterView.SetPrintMode(AValue: boolean);
begin
  if FPrintMode=AValue then Exit;
  FPrintMode:=AValue;
  HandleVerseChange(FProject);
end;

procedure TChapterView.SetProject(AValue: TProjeto);
begin
  if FProject = AValue then Exit;
  FProject := AValue;

  if assigned(FProject) then
  begin
    SetParagraphMode(FProject.ParagraphMode);
    SetInterlinearMode(FProject.InterlinearMode);
    FProject.OnNewVerseSubscribe(@HandleVerseChange);
    LoadChapter;
  end;
end;

procedure TChapterView.InitPopupMenu;
begin
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Parent := Self;

  { Set font }
  FSetFontItem := TMenuItem.Create(PopupMenu);
  with FSetFontItem do
  begin
    Caption := SSetFont;
    OnClick := @HandleSetFont;
  end;
  PopupMenu.Items.Add(FSetFontItem);

  { Paragraph mode }
  FParagraphModeItem := TMenuItem.Create(PopupMenu);
  with FParagraphModeItem do
  begin
    Caption := SParagraphMode;
    OnClick := @HandleParagraphMode;
  end;
  PopupMenu.Items.Add(FParagraphModeItem);

  { Interlinear mode }
  FInterlinearModeItem := TMenuItem.Create(PopupMenu);
  FInterlinearModeItem.Caption := SInterlinearMode;
  PopupMenu.Items.Add(FInterlinearModeItem);

  { Interlinear mode -> Interlinear }
  FInterlinearModeInterItem := TMenuItem.Create(PopupMenu);
  with FInterlinearModeInterItem do
  begin
    Caption := SInterlinearModeInter;
    OnClick := @HandleInterlinearMode;
  end;
  FInterlinearModeItem.Add(FInterlinearModeInterItem);

  { Interlinear mode -> Intralinear }
  FInterlinearModeIntraItem := TMenuItem.Create(PopupMenu);
  with FInterlinearModeIntraItem do
  begin
    Caption := SInterlinearModeIntra;
    OnClick := @HandleIntralinearMode;
  end;
  FInterlinearModeItem.Add(FInterlinearModeIntraItem);

  { Verse rules }
  FVerseRulesItem := TMenuItem.Create(PopupMenu);
  with FVerseRulesItem do
  begin
    Caption := SVerseRules;
    OnClick := @HandleVerseRules;
  end;
  PopupMenu.Items.Add(FVerseRulesItem);

  { Print }
  FPrintChapterItem := TMenuItem.Create(PopupMenu);
  with FPrintChapterItem do
  begin
    Caption := SPrintChapter;
    OnClick := @HandlePrintChapter;
  end;
  PopupMenu.Items.Add(FPrintChapterItem);
end;

procedure TChapterView.SetParagraphMode(AValue: TParagraphMode);
begin
  FParagraphModeItem.Checked := AValue = pmParagraph;

  if FProject.ParagraphMode = AValue then exit;
  FProject.ParagraphMode := AValue;

  LoadChapter;
end;

procedure TChapterView.SetFontSize(AValue: integer);
begin
  if (FFontSize = AValue) or (AValue < 1) then Exit;

  Blocks.LockUpdate;
  FFontSize := AValue;
  TextStyle.Font.Size := FFontSize;
  FHint.Font.Assign(TextStyle.Font);
  Blocks.UnlockUpdate;
end;

procedure TChapterView.SetFontName(AValue: string);
begin
  if (FFontName = AValue) then Exit;

  Blocks.LockUpdate;
  FFontName := AValue;
  TextStyle.Font.Name := FFontName;
  FHint.Font.Assign(TextStyle.Font);
  Blocks.UnlockUpdate;
end;

function TChapterView.PushNewStyle: TKMemoTextStyle;
begin
  result := TKMemoTextStyle(FStyleStack.Push(TKMemoTextStyle.Create));
  result.Assign(TextStyle);
end;

function TChapterView.PushInheritedStyle: TKMemoTextStyle;
var
  current: TKMemoTextStyle;
begin
  current := CurrentStyle;
  result := PushNewStyle;
  result.Assign(current);
end;

procedure TChapterView.PopStyle;
begin
  if FStyleStack.Count > 1 then
    FStyleStack.Pop.Free;
end;

procedure TChapterView.HighlightRange(range: TBlockRange; bgcolor: TColor);
var
  i: integer;
begin
  if (range.first < 0) or (range.last < range.first) then Exit;
  for i := Max(0, range.first) to Min(Blocks.Count - 1, range.last) do
  begin
    if Blocks[i].ClassName = 'TKMemoTextBlock' then
    begin
      TKMemoTextBlock(Blocks[i]).TextStyle.Brush.Style := bsSolid;
      TKMemoTextBlock(Blocks[i]).TextStyle.Brush.Color := bgcolor;
    end
    else if Blocks[i].ClassName = 'TKMemoContainer' then
      TKMemoContainer(Blocks[i]).BlockStyle.Brush.Color := bgcolor;
  end;
end;

procedure TChapterView.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.WindowBg;
  ApplyDarkModeToControl(Self, ASkin.IsDark);
  Background.Color := ASkin.WindowBg;
  TextStyle.Font.Color := ASkin.WindowText;
  TextStyle.Brush.Color := ASkin.WindowBg;
  TextStyle.Brush.Style := bsClear;
  if assigned(FHint) and assigned(FHint.FNoteView) then
  begin
    FHint.FNoteView.Color := ASkin.NoteBg;
    FHint.FNoteView.Background.Color := ASkin.NoteBg;
    FHint.FNoteView.TextStyle.Font.Color := ASkin.NoteText;
    FHint.FNoteView.TextStyle.Brush.Color := ASkin.NoteBg;
  end;
  LoadChapter;
end;

procedure TChapterView.ResetStyleStack;
begin
  while FStyleStack.Count > 0 do
    FStyleStack.Pop.Free;
  TextStyle.Font.Size := FFontSize;
  TextStyle.Font.Name := FFontName;
  TextStyle.Font.Color := CurrentSkin.WindowText;
  TextStyle.Brush.Color := CurrentSkin.WindowBg;
  TextStyle.Brush.Style := bsClear;
  TextStyle.ScriptPosition := tpoNormal;
  ParaStyle.FirstIndent := 0;
  ParaStyle.LineSpacingFactor := 1.15;
  PushNewStyle; // pushing default style onto stack
end;

function TChapterView.GetPreviousParagraphBlock: TKMemoParagraph;
var
  b: TKMemoBlockIndex;
begin
  Result := nil;
  for b := Blocks.Count-1 downto 0 do
    if Blocks[b] is TKMemoParagraph then
    begin
      Result := Blocks[b] as TKMemoParagraph;
      break;
    end;
end;

procedure TChapterView.SetTextType(AValue: TTipoTextoBiblico);
begin
  if GetTextType = AValue then Exit;
  FProject.ChapterViewText := AValue;
  LoadChapter;
end;

procedure TChapterView.RenderChapterHeader;
var
  c: integer;
begin
  with Blocks.AddTextBlock(SysUtils.Format('%s %d', [FProject.Book, FProject.Chapter])).TextStyle.Font do
  begin
    Name := 'default';
    Size := Self.TextStyle.Font.Size+2;
    Bold := true;
    Color := CurrentSkin.HeaderColor;
  end;

  if not FPrintMode then
  begin
    Blocks.AddParagraph();

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
  end;

  with Blocks.AddParagraph().ParaStyle do
  begin
    BottomPadding := 10;
    FirstIndent := 0;
  end;
end;

procedure TChapterView.RenderChapterFooter;
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
  pbook, nbook, pchapter, nchapter: integer;
begin
  if FPrintMode then exit;

  pbook := 0;
  pchapter := 0;
  GetPreviousChapter(pbook, pchapter);

  if pbook <> 0 then
    with Blocks.AddHyperlink(Format('<< %s %d', [NLivrosONT[pbook], pchapter]), Format('%d,%d,1', [pbook, pchapter])) do
    begin
      OnClick := @HandleReferenceClick;
      TextStyle.Font.Name  := 'default';
      TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
      TextStyle.Font.Bold  := false;
      TextStyle.Font.Color := ChapterNumberColor;
    end;

  nbook := 0;
  nchapter := 0;
  GetNextChapter(nbook, nchapter);

  if pbook <> 0 then
    Blocks.AddTextBlock(' | ');

  with Blocks.AddHyperlink(Format('%s', [SChapterBeginning]), Format('%d,%d,1', [FProject.BookID, FProject.Chapter])) do
  begin
    OnClick := @HandleReferenceClick;
    TextStyle.Font.Name  := 'default';
    TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
    TextStyle.Font.Bold  := false;
    TextStyle.Font.Color := ChapterNumberColor;
  end;

  if nbook <> 0 then
  begin
    Blocks.AddTextBlock(' | ');
    with Blocks.AddHyperlink(Format('%s %d >>', [NLivrosONT[nbook], nchapter]), Format('%d,%d,1', [nbook, nchapter])) do
    begin
      OnClick := @HandleReferenceClick;
      TextStyle.Font.Name  := 'default';
      TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
      TextStyle.Font.Bold  := false;
      TextStyle.Font.Color := ChapterNumberColor;
    end;
  end;

  with Blocks.AddParagraph().ParaStyle do
  begin
    TopPadding := 10;
    HAlign := halCenter;
  end;
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
  FLoadingChapter := false;
  Parent       := TWinControl(AOwner);
  ReadOnly     := true;
  OnKeyDown    := @HandleKeyDown;
  OnMouseWheel := @HandleMouseWheel;
  OnDblClick   := @HandleDoubleClick;
  OnContextPopup := @HandleContextPopup;
  FFontName       := 'default';
  FFontSize       := 10;
  FProject        := nil;
  FRxVerseHeading := RegexCreate('^((?:<TS[0-3]?>.*?<Ts>)*)(.*?)$', [rcoUTF8]);
  FNotes          := TNoteList.Create;
  FHint           := TNoteWindow.Create(Self);
  FHideVerseNumber:= false;
  FPrintMode      := false;
  FParaLeftIndent := 0;
  FParaFirstIndent := 0;
  FParaTopPadding := 0;
  FParaBottomPadding := 0;
  FParaHAlign := halLeft;
  FJustClosedHeading := False;
  InitPopupMenu;

  FStyleStack := TObjectStack.Create;
  FRawMemo    := TRawChapterMemo.Create(Parent, Self);
  Color       := CurrentSkin.WindowBg;
  Background.Color := CurrentSkin.WindowBg;
  TextStyle.Font.Color := CurrentSkin.WindowText;
  TextStyle.Brush.Color := CurrentSkin.WindowBg;
  TextStyle.Brush.Style := bsClear;
end;

destructor TChapterView.Destroy;
begin
  FRawMemo.Free;
  FNotes.Free;
  FHint.Free;
  while FStyleStack.Count > 0 do
    FStyleStack.Pop.Free;
  FStyleStack.Free;
  if assigned(FProject) then
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);

  inherited Destroy;
end;

procedure TChapterView.Translate;
begin
  FSetFontItem.Caption              := SSetFont;
  FParagraphModeItem.Caption        := SParagraphMode;
  FInterlinearModeItem.Caption      := SInterlinearMode;
  FInterlinearModeInterItem.Caption := SInterlinearModeInter;
  FInterlinearModeIntraItem.Caption := SInterlinearModeIntra;
  FVerseRulesItem.Caption           := SVerseRules;
  FPrintChapterItem.Caption         := SPrintChapter;
  LoadChapter;
end;

procedure TChapterView.LoadChapter;
begin
  HandleVerseChange(FProject);
end;

procedure TChapterView.RenderChapter(verses: TStringList; current: integer);
var
  verse: string;
begin
  SetLength(FVerseRanges, verses.Count + 1);
  FRawMemo.Hide;

  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;

    FNoteID := 1;
    FNotes.Clear;

    RenderChapterHeader;

    FParaLeftIndent := 0;
    FParaFirstIndent := IfThen(GetParagraphMode = pmParagraph, DefaultFirstIndent, 0);
    FParaTopPadding := 0;
    FParaBottomPadding := 0;
    FParaHAlign := halLeft;
    FJustClosedHeading := False;

    FCurrentVerse := 0;
    for verse in verses do
    begin
      Inc(FCurrentVerse);
      RenderVerse(verse);
    end;

    if Assigned(Blocks.LastBlock) and (Blocks.LastBlock.ClassName <> 'TKMemoParagraph') then
      CloseParagraphBlock(Blocks);

    RenderChapterFooter;
    if (current >= Low(FVerseRanges)) and (current <= High(FVerseRanges)) then
      HighlightRange(FVerseRanges[current], CurrentSkin.VerseHighlightBg);
  finally
    Blocks.UnLockUpdate;
  end;

  { scroll current verse to center }
  if (current >= Low(FVerseRanges)) and (current <= High(FVerseRanges)) and
     (FVerseRanges[current].first >= 0) and (FVerseRanges[current].first < Blocks.Count) then
  begin
    self.SelectBlock(Blocks[FVerseRanges[current].first], sgpTopLeft);
    self.SelLength := 0;
    ExecuteCommand(ecScrollCenter);
  end;
end;

end.
