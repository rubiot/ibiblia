unit ONTMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, KMemo, KGraphics, Graphics, ONTParser;

type

  { TONTBlock }

  TONTBlock = class(TKMemoTextBlock)
    FData: string;
  public
    constructor Create(AText: string); overload; virtual;
  end;

  { TFIBlock }

  TFIBlock = class(TONTBlock)
  public
    constructor Create(AText: string); override;
  end;

  { TFRBlock }

  TFRBlock = class(TONTBlock)
  public
    constructor Create(AText: string); override;
  end;

  { TFOBlock }

  TFOBlock = class(TONTBlock)
  public
    constructor Create(AText: string); override;
  end;

  { TInterlinearBlock }

  TInterlinearBlock = class(TKMemoContainer)
  private
    FBlocks: TKMemoBlocks;
  public
    constructor Create(ABlocks: TKMemoBlocks); overload;
    destructor Destroy; override;
  end;

  { TONTMemo }

  TONTMemo = class(TKCustomMemo)
  private
    FVerseNumber: integer;
    FNoteNumber: integer;
    FNewVerse: boolean;
    FONTText: string;
    procedure SetONTText(AValue: string);
    procedure CheckNewVerse;
  public
    constructor Create(AOwner: TComponent); override;
    function AddTextBlock(const AText: string; At: TKMemoBlockIndex = -1): TONTBlock;
    function AddFIBlock(const AText: string; At: TKMemoBlockIndex = -1): TFIBlock;
    function AddFRBlock(const AText: string; At: TKMemoBlockIndex = -1): TFRBlock;
    function AddFOBlock(const AText: string; At: TKMemoBlockIndex = -1): TFOBlock;
    procedure AddTitle(const AText: string; Level: integer);
    function AddCMBlock(At: TKMemoBlockIndex = -1): TKMemoParagraph;
    procedure AddInterlinearBlock(const AText: string);
    procedure AddNote(anchor: string; const Note: string);

    property ONT: string read FONTText write SetONTText;
  end;

implementation

{ TInterlinearBlock }

constructor TInterlinearBlock.Create(ABlocks: TKMemoBlocks);
begin
  inherited Create;
  FBlocks := TKMemoBlocks.Create;
  FBlocks.Assign(ABlocks);
end;

destructor TInterlinearBlock.Destroy;
begin
  FBlocks.Free;
  inherited Destroy;
end;

{ TFOBlock }

constructor TFOBlock.Create(AText: string);
begin
  inherited Create(AText);
  TextStyle.Font.Bold := true;
end;

{ TONTBlock }

constructor TONTBlock.Create(AText: string);
begin
  inherited Create;
  Text := AText;
end;

{ TFIBlock }

constructor TFIBlock.Create(AText: string);
begin
  inherited Create(AText);
  TextStyle.Font.Italic := true;
  TextStyle.Font.Color := clGray;
end;

{ TFRBlock }

constructor TFRBlock.Create(AText: string);
begin
  inherited Create(AText);
  TextStyle.Font.Color := clRed;
end;

{ TONTMemo }

procedure TONTMemo.SetONTText(AValue: string);
var
  parser: TONTParser;
  token: TONTToken;
begin
  if FONTText = AValue then Exit;
  FONTText := AValue;

  FVerseNumber := 0;
  FNoteNumber  := 0;
  FNewVerse    := true;

  Blocks.LockUpdate;
  Blocks.Clear;
  parser := TONTParser.Create(FONTText);
  try
    repeat
      token := parser.ReadNext;
      case token.Kind of
        otText, otPunctuation:
          AddTextBlock(token.Text + token.Punctuation);
        otParagraph:
          Blocks.AddParagraph();
        otAddedWords:
          AddFIBlock(token.Text);
        otTitle:
          AddTitle(token.Text, token.Level);
        otNote:
          AddNote(token.q, token.Text);
        otInterlinearBlock:
          AddInterlinearBlock(token.Text);
        otEOL:
          FNewVerse := true;
        otEOF:
          ; // do nothing
        otMetaData:
          ; // TODO
        otTranslationBlock:
          ; // TODO
        otStrong, otMorphology:
          ; // TODO handle astray strong/morpho tags
        else
          raise Exception.Create(Format('unhandled token type in TONTMemo.SetONTText(): [%s]', [token.Text]));
      end;
    until token.Kind = otEOF;
  finally
    parser.Free;
    Blocks.UnLockUpdate;
  end;
end;

constructor TONTMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentFont := true;
  ReadOnly := true;
  Blocks.Clear; // clearing default empty paragraph
  Blocks.DefaultParaStyle.HAlign := halLeft;
end;

function TONTMemo.AddTextBlock(const AText: string; At: TKMemoBlockIndex
  ): TONTBlock;
begin
  CheckNewVerse;
  Result := TONTBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

function TONTMemo.AddFIBlock(const AText: string; At: TKMemoBlockIndex
  ): TFIBlock;
begin
  CheckNewVerse;
  Result := TFIBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

function TONTMemo.AddFRBlock(const AText: string; At: TKMemoBlockIndex
  ): TFRBlock;
begin
  CheckNewVerse;
  Result := TFRBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

function TONTMemo.AddFOBlock(const AText: string; At: TKMemoBlockIndex
  ): TFOBlock;
begin
  CheckNewVerse;
  Result := TFOBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

procedure TONTMemo.AddTitle(const AText: string; Level: integer);
begin
  if not Blocks.Empty then
    AddCMBlock();

  Blocks.AddTextBlock(AText).TextStyle.Font.Bold := true;
  AddCMBlock();
end;

function TONTMemo.AddCMBlock(At: TKMemoBlockIndex): TKMemoParagraph;
begin
  result := Blocks.AddParagraph(At);
end;

procedure TONTMemo.AddInterlinearBlock(const AText: string);
var
  parser: TONTParser;
  token: TONTToken;
  cont: TKMemoContainer;
begin
  CheckNewVerse;
  cont := Blocks.AddContainer();
  cont.Position := mbpText;
  cont.FixedWidth := true;
  //cont.DefaultParaStyle.HAlign := halCenter;
  parser := TONTParser.Create(AText);
  try
    repeat
      token := parser.ReadNext;
      case token.Kind of
        otText:
        begin
          token.Punctuation := StringReplace(token.Punctuation, ' ', #8239, [rfReplaceAll]);
          if assigned(cont.Blocks.LastBlock) and cont.Blocks.LastBlock.ClassNameIs('TKMemoTextBlock') then
            cont.Blocks.InsertString(cont.Blocks.Text.Length, true, token.Text + token.Punctuation)
          else
            cont.Blocks.AddTextBlock(token.Text + token.Punctuation);

          {if Length(token.Strongs) > 0 then
          begin
            cont.Blocks.AddParagraph();
            cont.Blocks.AddTextBlock(token.Strongs).TextStyle.Font.Color := clBlue;
          end;
          if Length(token.Morpho) > 0 then
          begin
            cont.Blocks.AddParagraph(); //.ParaStyle.HAlign := halCenter;
            cont.Blocks.AddTextBlock(token.Morpho).TextStyle.Font.Color := clGreen;
          end;
          if Length(token.Lemma) > 0 then
          begin
            cont.Blocks.AddParagraph(); //.ParaStyle.HAlign := halCenter;
            cont.Blocks.AddTextBlock(token.Lemma).TextStyle.Font.Color := clBlack;
          end;}
        end;
        otTranslationBlock, otTransliterationBlock:
        begin
          if cont.Blocks.Count > 0 then
            cont.Blocks.AddParagraph();
          token.Text := StringReplace(token.Text, ' ', #8239, [rfReplaceAll]);
          cont.Blocks.AddTextBlock(token.Text).TextStyle.Font.Color := $be7c41;
        end;
      end;
    until token.Kind = otEOF;

    cont.BlockStyle.BottomPadding := 10;
    cont.BlockStyle.RightPadding := 5;

  finally
    parser.Free;
  end;
end;

procedure TONTMemo.AddNote(anchor: string; const Note: string);
begin
  if anchor.IsEmpty then
  begin
    Inc(FNoteNumber);
    anchor := FNoteNumber.ToString;
  end;

  with Blocks.AddHyperlink(anchor, '').TextStyle do
  begin
    ScriptPosition := tpoSuperscript;
  end;
end;

procedure TONTMemo.CheckNewVerse;
begin
  if not FNewVerse then exit;

  Inc(FVerseNumber);
  if (FVerseNumber > 1) and (Blocks.LastBlock.ClassName <> 'TKMemoParagraph') then
     Blocks.AddTextBlock(' ');

  with Blocks.AddTextBlock(Format('%d ', [FVerseNumber])).TextStyle do
  begin
    ScriptPosition := tpoSuperscript;
    Font.Bold := true;
  end;

  FNewVerse := false;
end;

end.

