unit Syntagm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONTTokenizer, StdCtrls, Controls, Graphics, Forms, Math,
  LazUTF8, LazLogger;

type
  TSyntagm = class;
  TSyntagmList = class;

  TPairsListType =
  (
    plMetaData, // only metadata, e.g. "<WG1><WTADJ-NSM>"
    plStrong,   // only strong#, e.g. "<WG1>"
    plText,     // only text, e.g. "Jesus"
    plAll       // text and metadata, e.g.  "Jesus<WG1088><WTNPI>"
  );

  { TSyntagmListEnumerator }

  TSyntagmListEnumerator = class(TListEnumerator)
  private
    function GetCurrentSintagma: TSyntagm;
  public
    property Current: TSyntagm read GetCurrentSintagma;
  end;

  { TSyntagmListReverseEnumerator }

  TSyntagmListReverseEnumerator = class
  private
    FList: TSyntagmList;
    FPosition: integer;
    function GetCurrent: TSyntagm;
  public
    constructor Create(AList: TSyntagmList);
    function MoveNext: Boolean;
    property Current: TSyntagm read GetCurrent;
    function GetEnumerator: TSyntagmListReverseEnumerator;
  end;

  { TSyntagmList }

  TSyntagmList = class(TList)
  private
    function GetEmpty: boolean;
  protected
    function GetS(Index: Integer): TSyntagm;
    procedure PutS(Index: Integer; Item: TSyntagm);
  public
    function First: TSyntagm;
    function GetEnumerator: TSyntagmListEnumerator;
    function GetReverseEnumerator: TSyntagmListReverseEnumerator;
    property Itens[Index: Integer]: TSyntagm read GetS write PutS; default;
    property Empty: boolean read GetEmpty;
    procedure AddList(AList : TSyntagmList); //override;
  end;

  { TSyntagm }

  TSyntagm = class
  private
    FKind: TTipoSintagma;
    FText: string;
    FRawText: string;
    FPairs: TSyntagmList;
    FSiblings: TSyntagmList;
    FLabel: TLabel;
    FSelected: boolean;
    FVerse: TObject; // TVersiculo; avoiding circular reference
    FStrong: TStringList;
    FMorph: TStringList;
    FColor: TColor;
    FSuperscript: boolean;
    FItalic: boolean;
    procedure DoOnDblClick(Sender: TObject);
    procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure DoOnMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure DoOnMouseEnter(Sender: TObject);
    procedure DoOnMouseLeave(Sender: TObject);
    procedure DoOnLeftClick(Sender: TObject; Shift: TShiftState);
    procedure DoOnRightClick(Sender: TObject; Shift: TShiftState);
    procedure DoOnMiddleClick(Sender: TObject; Shift: TShiftState);
    function GetIsAssociated: boolean;
    function GetPairsHaveStrongs: boolean;
    function GetStrongsCount: integer;
    function GetStrongWordsCount: integer;
    function GetTags: string;
    function GetHasStrongs: boolean;
    function GetHasMorph: boolean;
    procedure SetPointed(const AValue: boolean);
    procedure SetAssociated(const AValue: boolean);
    function GetGist: string;
  public
    constructor Create(s: TTagSintagma; owner: TObject);
    procedure Render;
    destructor Destroy; override;
    procedure AddToSelection;
    procedure RemoveFromSelection;
    procedure InvertSelection;
    procedure Disassociate;
    procedure DisassociatePairs;
    procedure HighlightStrong(strong: string);
    procedure ToggleStrongHighlight(enable: boolean);
    function GetNext: TSyntagm;
    function GetNextVisible: TSyntagm;
    function GetNextUnassociated: TSyntagm;
    function GetPrevVisible: TSyntagm;
    function GetSuggestionKey(t: TPairsListType): string;
    function IsEqualTo(other : TSyntagm): boolean;
    function ContainedInRect(rect: TRect): boolean;
    property Strong: TStringList read FStrong write FStrong;
    property Morph: TStringList read FMorph write FMorph;
    property Tags: string read GetTags;
    property LabelRef: TLabel read FLabel write FLabel;
    property Pairs: TSyntagmList read FPairs write FPairs;
    property Siblings: TSyntagmList read FSiblings write FSiblings;
    property VerseRef: TObject{TVersiculo} read FVerse write FVerse;
    property IsSelected: boolean read FSelected;
    property IsAssociated: boolean read GetIsAssociated write SetAssociated;
    property IsPointed: boolean write SetPointed;
    property Text: string read FText write FText;
    property RawText: string read FRawText write FRawText;
    property Kind: TTipoSintagma read FKind write FKind;
    property IsItalic: boolean read FItalic;
    property IsSuperscript: boolean read FSuperscript;
    property HasStrongs: boolean read GetHasStrongs;
    property StrongsCount: integer read GetStrongsCount;
    property StrongsWordsCount: integer read GetStrongWordsCount;
    property HasMorph: boolean read GetHasMorph;
    property PairsHaveStrongs: boolean read GetPairsHaveStrongs;
    property Gist: string read GetGist;
  published

  end;

implementation

uses Versiculo;

var Hint: THintWindow;

{ TSyntagmListReverseEnumerator }

function TSyntagmListReverseEnumerator.GetCurrent: TSyntagm;
begin
  Result := FList[FPosition];
end;

constructor TSyntagmListReverseEnumerator.Create(AList: TSyntagmList);
begin
  inherited Create;
  FList := AList;
  FPosition := FList.Count;
end;

function TSyntagmListReverseEnumerator.MoveNext: Boolean;
begin
  Dec(FPosition);
  Result := (FPosition >= 0) and (FPosition < FList.Count);
end;

function TSyntagmListReverseEnumerator.GetEnumerator: TSyntagmListReverseEnumerator;
begin
  Result := Self;
end;

{ TSyntagmListEnumerator }

function TSyntagmListEnumerator.GetCurrentSintagma: TSyntagm;
begin
  result := TSyntagm(GetCurrent);
end;

{ TSyntagm }

procedure TSyntagm.DoOnLeftClick(Sender: TObject; Shift: TShiftState);
//var
//  d: string;
var
  verse: TVersiculo;
begin
  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VerseRef.Selecao.Count, VerseRef.VersiculoPar.Selecao.Count, Siblings.Count, Pairs.Count]);
  verse := TVersiculo(VerseRef);

  if verse.ReadOnly then
    Shift := [];

  if (ssShift in Shift) and Assigned(verse.VersiculoPar) then
  begin
    DisassociatePairs;
    if (verse.Selecao.Count > 0) then
      verse.Selecao[0].DisassociatePairs;
  end;
  if not (ssCtrl in Shift) then // Ctrl não pressionado
    verse.LimparSelecao;

  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VerseRef.Selecao.Count, VerseRef.VersiculoPar.Selecao.Count, Siblings.Count, Pairs.Count]);
  InvertSelection;
  //d := format('selecao: %d, ' + 'sel par: %d, ' + 'irmaos : %d, ' + 'pares  : %d', [VerseRef.Selecao.Count, VerseRef.VersiculoPar.Selecao.Count, Siblings.Count, Pairs.Count]);

  if (ssShift in Shift) and (Assigned(verse.VersiculoPar)) and
     (verse.VersiculoPar.Selecao.Count > 0) and
     (verse.Selecao.Count > 0) then // Shift pressionado
  begin
    verse.AssociarSintagmas;
  end
  else if Assigned(verse.VersiculoPar) then // Shift não pressionado
  begin
    verse.VersiculoPar.LimparSelecao;
    if Pairs.Count > 0 then
    begin
      //VerseRef.SelecionarSintagmas(Pairs[0].Pairs);
      verse.SelecionarSintagmas(Siblings);
      verse.VersiculoPar.SelecionarSintagmas(Pairs);
    end;
  end;

  verse.Renderizar;
  if  assigned(verse.VersiculoPar) then
    verse.VersiculoPar.Renderizar;

  if assigned(verse.OnClick) then
    verse.OnClick(self);
end;

procedure TSyntagm.DoOnDblClick(Sender: TObject);
begin
  {
  if (TVersiculo(FVerse).Ativo) then
  begin
    //LabelRef.Font.StrikeTrough := not LabelRef.Font.StrikeTrough;
    with TVersiculo(FVerse).Edit do
    begin
      Tag       := PtrInt(Self);
      Top       := FLabel.Top;
      Left      := FLabel.Left;
      Height    := FLabel.Height + 5;
      Width     := FLabel.Width + 15;
      Text      := FLabel.Caption;
      Visible   := true;
      SetFocus;
    end;
  end;
  }
end;

procedure TSyntagm.DoOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TVersiculo(FVerse).Edit.OnExit(Self);
  if Button = mbRight then
  begin
    TVersiculo(FVerse).OnMouseDownVerse(Sender, Button, Shift, FLabel.Left+x, FLabel.Top+y);
    DoOnRightClick(Sender, Shift)
  end
  else if Button = mbLeft then
    DoOnLeftClick(Sender, Shift)
  else if Button = mbMiddle then
    DoOnMiddleClick(Sender, Shift);
end;

procedure TSyntagm.DoOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    TVersiculo(FVerse).OnMouseUpVerse(Sender, Button, Shift, FLabel.Left+x, FLabel.Top+y);
end;

procedure TSyntagm.DoOnMouseEnter(Sender: TObject);
var
  rect: TRect;
  pos: TPoint;
  txt: string;
  i: smallint;
  verse: TVersiculo;
begin
  SetPointed(true);

  for i:=0 to Pairs.Count-1 do
    Pairs[i].SetPointed(true);
  for i:=0 to Siblings.Count-1 do
    Siblings[i].SetPointed(true);

  verse := TVersiculo(VerseRef);
  if assigned(verse.OnMouseEnter) then
    verse.OnMouseEnter(self);

  if not verse.MostrarDicas then exit;

  if not assigned(Hint) then
    Hint := THintWindow.Create(Self.LabelRef);

  //if FStrong.Count > 0 then
  //begin
    txt := '';
    //if assigned(VerseRef.OnStrong) then
    //  txt := VerseRef.OnStrong(FStrong); //FStrong + #13#10 + FMorph;

    txt := 'Pares:';
    for i:=0 to Pairs.Count-1 do
      txt := Concat(txt, #13#10, Pairs[i].LabelRef.Caption);
    txt := Concat(txt, #13#10#13#10'Irmãos:');
    for i:=0 to Siblings.Count-1 do
      txt := Concat(txt, #13#10, Siblings[i].LabelRef.Caption);

    if length(txt) > 0 then
    begin
      Rect := Hint.CalcHintRect(0, txt, nil);  // no maxwidth
      Pos := Mouse.CursorPos;
      Rect.Left := Pos.X+10;
      Rect.Top := Pos.Y+5;
      Rect.Right := Rect.Left + Rect.Right;
      Rect.Bottom := Rect.Top + Rect.Bottom;

      Hint.ActivateHint(Rect, txt);
    end;
  //end;
end;

procedure TSyntagm.DoOnMouseLeave(Sender: TObject);
var
  i: smallint;
begin
  SetPointed(False);
  for i:=0 to Pairs.Count-1 do
    Pairs[i].SetPointed(False);
  for i:=0 to Siblings.Count-1 do
    Siblings[i].SetPointed(False);

  if assigned(Hint) then
  begin
    Hint.Destroy;
    Hint := nil;
  end;

  if assigned(TVersiculo(VerseRef).OnMouseLeave) then
    TVersiculo(VerseRef).OnMouseLeave(self);
end;

procedure TSyntagm.DoOnRightClick(Sender: TObject; Shift: TShiftState);
var
  LastSelected: TSyntagm;
  verse: TVersiculo;
begin
  {if ssShift in Shift then
  begin
    TVersiculo(FVerse).OnSintagmaPopupMenu(self);
    exit;
  end;
  }

  verse := TVersiculo(VerseRef);
  if not assigned(verse.VersiculoPar) then
    exit;

  if verse.ReadOnly then
    exit;

  DisassociatePairs;
  if (verse.Selecao.Count > 0) then
    verse.Selecao[0].DisassociatePairs;

  if not (ssCtrl in Shift) then // Ctrl não pressionado
    verse.LimparSelecao;

  InvertSelection;

  if Assigned(verse.VersiculoPar) and
     (verse.VersiculoPar.Selecao.Count > 0) and
     (verse.Selecao.Count > 0) then
  begin
    verse.AssociarSintagmas;
  end;

  if not (ssCtrl in Shift) and (verse.VersiculoPar.Selecao.Count > 0) then
  begin
    LastSelected := verse.VersiculoPar.Selecao[verse.VersiculoPar.Selecao.Count-1];
    verse.LimparSelecao;
    verse.VersiculoPar.LimparSelecao;
    if Assigned(LastSelected) and Assigned(LastSelected.GetNextUnassociated) then
      LastSelected.GetNextUnassociated.AddToSelection;
  end;

  verse.Renderizar;
end;

procedure TSyntagm.DoOnMiddleClick(Sender: TObject; Shift: TShiftState);
begin
  TVersiculo(FVerse).OnSintagmaPopupMenu(self);
end;

function TSyntagm.GetSuggestionKey(t: TPairsListType): string;
var
  s: smallint;
begin
  result := '';

  if t in [plText, plAll] then
  begin
    result := Text;
    with TVersiculo(FVerse) do
    begin
      s := Sintagmas.IndexOf(Self);
      if (s >= 0) and (s < (Sintagmas.Count-1)) and
         (Sintagmas[s+1].Kind = tsSintagma) and
         Sintagmas[s+1].Text.StartsWith('-') then
        result := result + '-';
    end;
  end;

  if t <> plText then
  begin
    if assigned(Strong) and assigned(Morph) then
    begin
      for s:=0 to max(Strong.Count, Morph.Count) do
      begin
        if s < Strong.Count then
          result := format('%s<W%s>', [result, Strong.Strings[s]]);
        if (t <> plStrong) and (s < Morph.Count) then
          result := format('%s<WT%s>', [result, Morph.Strings[s]]);
      end;
      if result = '' then
        result := Text;
    end;
  end;

  result := UTF8LowerCase(result);
end;

function TSyntagm.GetIsAssociated: boolean;
begin
  result := TVersiculo(FVerse).Ativo and assigned(Pairs) and (Pairs.Count > 0){ (not FLabel.Font.Italic)};
end;

function TSyntagm.GetPairsHaveStrongs: boolean;
var
  p: TSyntagm;
begin
  if not assigned(Pairs) then
    exit;

  for p in Pairs do
    if p.HasStrongs then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TSyntagm.GetStrongsCount: integer;
var
  s: TSyntagm;
begin
  result := 0;
  if HasStrongs then
    result := FStrong.Count
  else if PairsHaveStrongs then
    for s in Pairs do
      inc(result, s.StrongsCount);
end;

function TSyntagm.GetStrongWordsCount: integer;
var
  s: TSyntagm;
begin
  result := 0;
  if HasStrongs then
    result := 1
  else if PairsHaveStrongs then
    for s in Pairs do
      if s.HasStrongs then
        inc(result);
end;

function TSyntagm.GetTags: string;
var
  tagsstr: TStringStream;
  t: string;
begin
  result := '';
  try
    tagsstr := TStringStream.Create('');
    if assigned(FStrong) then
      for t in FStrong do
        tagsstr.WriteString(format('<W%s>', [t]));
    if assigned(FMorph) then
      for t in FMorph do
        tagsstr.WriteString(format('<W%s>', [t]));
  finally
    result := tagsstr.DataString;
    tagsstr.Destroy;
  end;
end;

function TSyntagm.GetHasStrongs: boolean;
begin
  result := assigned(FStrong) and (FStrong.Count > 0);
end;

function TSyntagm.GetHasMorph: boolean;
begin
    result := assigned(FMorph) and (FMorph.Count > 0);
end;

procedure TSyntagm.SetPointed(const AValue: boolean);
begin
  //TVersiculo(FVerse).Painel.Refresh; // ocultando tags, se estiverem sendo exibidas

  if not assigned(FLabel) then
    exit;

  if AValue then
  begin // IsPointed
    FLabel.Font.Underline := true;
    if not IsSelected then
      FLabel.Color:= clHighlight;
  end
  else
  begin // não IsPointed
    FLabel.Font.Underline := false;
    if IsSelected then
      FLabel.Color:= clYellow
    else
      FLabel.Color:= clDefault;
  end;
end;

procedure TSyntagm.SetAssociated(const AValue: boolean);
begin
  if (not assigned(FVerse)) or
     (not TVersiculo(FVerse).Ativo) or
     (not assigned(FLabel)) then
    exit;

  if AValue then // IsAssociated
  begin
    //FLabel.Font.Italic := false;
    //FLabel.Font.Color := FColor; //clWindowText;
    if FColor = clDefault then
      FLabel.Font.Color := TVersiculo(FVerse).CorAssociado //clGrayText
    else
      FLabel.Font.Color := FColor;
  end else // nao IsAssociated
  begin
    //FLabel.Font.Italic := true;
    if FColor = clDefault then
      FLabel.Font.Color := TVersiculo(FVerse).CorDesassociado //clGrayText
    else
      FLabel.Font.Color := FColor;
  end;
end;

function TSyntagm.GetGist: string;
  function KindToStr(t: TTipoSintagma): string;
  begin
    result := '';
    case t of
      tsNulo:     result := 'tsNulo';
      tsSintagma: result := 'tsSintagma';
      tsTag:      result := 'tsTag';
      tsEspaco:   result := 'tsEspaco';
      tsPontuacao:result := 'tsPontuacao';
      tsMetaDado: result := 'tsMetaDado';
      tsStrongCount: result := 'tsStrongCount';
    end;
  end;
begin
  result := format('{index:%d,kind:%s,text:"%s"}',
    [TVersiculo(FVerse).Sintagmas.IndexOf(self), KindToStr(FKind), FText]);
end;

procedure TSyntagm.ToggleStrongHighlight(enable: boolean);
begin
  if not assigned(FLabel) then
    exit;

  FLabel.Font.Underline := enable;
end;

function TSyntagm.GetNext: TSyntagm;
var
  i: Integer;
begin
  result := nil;
  with TVersiculo(FVerse) do
  begin
    i := Sintagmas.IndexOf(Self);
    if (i >= 0) and (i < (Sintagmas.Count-1)) then
       result := Sintagmas[i+1];
  end;
end;

function TSyntagm.GetNextVisible: TSyntagm;
var
  i: Integer;
begin
  result := nil;
  with TVersiculo(FVerse) do
  begin
    i := Sintagmas.IndexOf(Self);
    if (i >= 0) and (i < Sintagmas.Count) then
    begin
      for i := i+1 to Sintagmas.Count-1 do
      begin
        if assigned(Sintagmas[i].FLabel)then
        begin
          result := Sintagmas[i];
          break;
        end;
      end;
    end;
  end;
end;

function TSyntagm.IsEqualTo(other: TSyntagm): boolean;
begin
  result := (FKind = other.FKind) and
            (FText = other.FText) and
            (FColor = other.FColor) and
            (FSuperscript = other.FSuperscript) and
            (FItalic = other.FItalic);
end;

function TSyntagm.ContainedInRect(rect: TRect): boolean;
var
  topLeft, bottomRight: TPoint;
begin
  result := false;
  if assigned(FLabel) then
  begin
    topLeft.x := FLabel.Left;
    topLeft.y := FLabel.Top;
    bottomRight.x := FLabel.Left + FLabel.Width;
    bottomRight.y := FLabel.Top + FLabel.Height;
    result := rect.Contains(topLeft) or rect.Contains(bottomRight);
  end;
end;

{ creates a basic instance of a syntagm, no label, no event handlers, etc. }
constructor TSyntagm.Create(s: TTagSintagma; owner: TObject);
begin
  FVerse       := owner;
  FText        := s.valor;
  FRawText     := s.valor;
  FKind        := s.tipo;
  FColor       := s.cor;
  FSuperscript := s.sobrescrito;
  FItalic      := s.italico;
  FLabel       := nil;
  FPairs       := TSyntagmList.Create;
  FSiblings    := TSyntagmList.Create;
  FStrong      := TStringList.Create;
  FMorph       := TStringList.Create;
end;

{ instantiates the visual representation of the syntagm (its label) }
procedure TSyntagm.Render;
begin
  if (FKind in [tsSintagma, tsEspaco, tsPontuacao, tsStrongCount]) or
     (FText = '<FI>') or (FText = '<Fi>') then
  begin
    if not assigned(FLabel) then
      FLabel := TLabel.Create(TVersiculo(FVerse).Painel);

    FLabel.Caption     := FText;
    if FText = '<FI>' then
      FLabel.Caption := '['
    else if FText = '<Fi>' then
      FLabel.Caption := ']';
    FLabel.ParentColor := false;
    FLabel.Font.Color  := FColor;
    FLabel.Parent      := TVersiculo(FVerse).Painel;
    FLabel.ParentFont  := FKind <> tsStrongCount;
    FLabel.AutoSize    := true;
    if FSuperscript then
      FLabel.Font.Size := round(FLabel.Font.Size * 0.7);
    FLabel.Font.Bold := FLabel.Font.Bold or (TVersiculo(FVerse).PalavrasComStrongEmNegrito and HasStrongs);
    IsAssociated     := FPairs.Count > 0;
  end else
    FLabel := nil;

  if FKind in [tsSintagma, tsPontuacao] then
  begin
    //FLabel.OnClick     := @DoOnClick;
    //FLabel.OnDblClick   := @DoOnDblClick;
    FLabel.OnMouseDown  := @DoOnMouseDown;
    FLabel.OnMouseUp    := @DoOnMouseUp;
    FLabel.OnMouseEnter := @DoOnMouseEnter;
    FLabel.OnMouseLeave := @DoOnMouseLeave;
  end else
  begin
    FStrong := nil;
    FMorph   := nil;
  end;
end;

destructor TSyntagm.Destroy;
begin
  DisassociatePairs;
  if assigned(FLabel) then
  begin
    FLabel.Destroy;
    FLabel := nil;
  end;
  if assigned(FPairs) then
  begin
    FPairs.Destroy;
    FPairs := nil;
  end;
  if assigned(FSiblings) then
  begin
    FSiblings.Destroy;
    FSiblings := nil;
  end;
  if assigned(FStrong) then
  begin
     FStrong.Destroy;
     FStrong := nil;
  end;
  if assigned(FMorph) then
  begin
    FMorph.Destroy;
    FMorph := nil;
  end;
end;

procedure TSyntagm.AddToSelection;
begin
  FSelected := true;
  if TVersiculo(FVerse).Ativo and
     assigned(FLabel) and
     (not TVersiculo(FVerse).IsDragging or (FKind = tsSintagma)) then
    FLabel.Color := clYellow;
  TVersiculo(FVerse).Selecao.Add(Self);
end;

procedure TSyntagm.RemoveFromSelection;
begin
  FSelected := false;
  if TVersiculo(FVerse).Ativo and assigned(FLabel) then
    FLabel.Color:= clWindow;
  TVersiculo(FVerse).Selecao.Remove(Self);
end;

procedure TSyntagm.InvertSelection;
begin
  if IsSelected then
    RemoveFromSelection
  else
    AddToSelection;
end;

procedure TSyntagm.DisassociatePairs;
var
  s: TSyntagm;
begin
  for s in Siblings do
    s.Disassociate;

  if not Pairs.Empty then
  begin
    if assigned(Pairs[0].Siblings) then
      for s in Pairs[0].Siblings do
        s.Disassociate;
    Pairs[0].Disassociate;
  end;

  Disassociate;
end;

procedure TSyntagm.HighlightStrong(strong: string);
var
  s: string;
begin
  if not assigned(FStrong) then
    exit;

  for s in FStrong do
    if s = strong then
      ToggleStrongHighlight(true);
end;

function TSyntagm.GetNextUnassociated: TSyntagm;
var
  i: Integer;
begin
  result := nil;
  with TVersiculo(FVerse) do
  begin
    i := Sintagmas.IndexOf(Self);
    if (i >= 0) and (i < Sintagmas.Count) then
    begin
      for i := i+1 to Sintagmas.Count-1 do
      begin
        if (Sintagmas[i].Kind = tsSintagma) and
           (Sintagmas[i].HasStrongs or Sintagmas[i].HasMorph) and
           (not Sintagmas[i].IsAssociated) then
        begin
          result := Sintagmas[i];
          break;
        end;
      end;
    end;
  end;
end;

function TSyntagm.GetPrevVisible: TSyntagm;
var
  i: Integer;
begin
  result := nil;
  with TVersiculo(FVerse) do
  begin
    i := Sintagmas.IndexOf(Self);
    if (i >= 0) and (i > 0) then
    begin
      for i := i-1 to 0 do
      begin
        if assigned(Sintagmas[i].FLabel)then
        begin
          result := Sintagmas[i];
          break;
        end;
      end;
    end;
  end;
end;

procedure TSyntagm.Disassociate;
begin
  if not (FKind in [tsSintagma, tsPontuacao]) then
    exit;

  IsAssociated := false;
  IsPointed := false;
  Siblings.Clear;
  Pairs.Clear;
end;

{ TSyntagmList }

function TSyntagmList.GetEmpty: boolean;
begin
  result := Count = 0;
end;

function TSyntagmList.GetS(Index: Integer): TSyntagm;
begin
  result := TSyntagm(Get(Index));
end;

procedure TSyntagmList.PutS(Index: Integer; Item: TSyntagm);
begin
  if IndexOf(Item) = -1 then  // avoiding duplicates
    Put(Index, Item);
end;

function TSyntagmList.First: TSyntagm;
begin
  Result := GetS(0);
end;

function TSyntagmList.GetEnumerator: TSyntagmListEnumerator;
begin
  Result := TSyntagmListEnumerator.Create(Self);
end;

function TSyntagmList.GetReverseEnumerator: TSyntagmListReverseEnumerator;
begin
  Result := TSyntagmListReverseEnumerator.Create(Self);
end;

procedure TSyntagmList.AddList(AList: TSyntagmList);
var
  s: TSyntagm;
begin
  for s in AList do
    if IndexOf(s) = -1 then
       Add(s);
end;

end.

