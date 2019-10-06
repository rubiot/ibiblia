unit ONTMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, KMemo, KGraphics, Graphics, ONTParser;

type

  { TONTBlock }

  TONTBlock = class(TKMemoTextBlock)
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
    FONTText: string;
    procedure SetONTText(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function AddTextBlock(const AText: string; At: TKMemoBlockIndex = -1): TONTBlock;
    function AddFIBlock(const AText: string; At: TKMemoBlockIndex = -1): TFIBlock;
    function AddFRBlock(const AText: string; At: TKMemoBlockIndex = -1): TFRBlock;
    function AddFOBlock(const AText: string; At: TKMemoBlockIndex = -1): TFOBlock;
    procedure AddTitle(const AText: string; Level: integer);
    procedure AddCMBlock(At: TKMemoBlockIndex = -1);
    procedure AddInterlinearBlock(const AText: string);

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

  LockUpdate;
  parser := TONTParser.Create(FONTText);
  try
    repeat
      token := parser.ReadNext;
      case token.Kind of
        otText:
          AddTextBlock(token.Text);
        otParagraph:
          Blocks.AddParagraph();
        otAddedWords:
          AddFIBlock(token.Text);
        otTitle:
          AddTitle(token.Text, token.Level);
        otNote:;
        otInterlinearBlock:
          AddInterlinearBlock(token.Text);
      end;
    until token.Kind = otNull;
  finally
    parser.Free;
    UnlockUpdate;
  end;
end;

constructor TONTMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Blocks.Clear; // clearing default empty paragraph
  Blocks.DefaultParaStyle.HAlign := halLeft;
end;

function TONTMemo.AddTextBlock(const AText: string; At: TKMemoBlockIndex
  ): TONTBlock;
begin
  Result := TONTBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

function TONTMemo.AddFIBlock(const AText: string; At: TKMemoBlockIndex
  ): TFIBlock;
begin
  Result := TFIBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

function TONTMemo.AddFRBlock(const AText: string; At: TKMemoBlockIndex
  ): TFRBlock;
begin
  Result := TFRBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

function TONTMemo.AddFOBlock(const AText: string; At: TKMemoBlockIndex
  ): TFOBlock;
begin
  Result := TFOBlock.Create(AText);
  Blocks.AddAt(Result, At);
end;

procedure TONTMemo.AddTitle(const AText: string; Level: integer);
begin
  AddFOBlock(AText);
  AddCMBlock();
end;

procedure TONTMemo.AddCMBlock(At: TKMemoBlockIndex);
begin
  Blocks.AddParagraph(At);
end;

procedure TONTMemo.AddInterlinearBlock(const AText: string);
var
  parser: TONTParser;
  token: TONTToken;
  cont: TKMemoContainer;
  //maxWidth: integer;
begin
  //maxWidth := 0;
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
          //if (cont.Blocks.Count > 0) and (cont.Blocks.LastBlock.ClassName <> 'TKMemoParagraph') then
          //  cont.Blocks.AddParagraph();
          //maxWidth := Max(maxWidth, cont.Blocks.AddTextBlock(token.Text).Width);
          cont.Blocks.AddTextBlock(token.Text)
        end;
        otTranslationBlock, otTransliterationBlock:
        begin
          if cont.Blocks.Count > 0 then
            cont.Blocks.AddParagraph();
          //maxWidth := Max(maxWidth, cont.Blocks.AddTextBlock(token.Text).BoundsRect.Right);
          cont.Blocks.AddTextBlock(token.Text);
        end;
      end;
    until token.Kind = otNull;
  finally
    //cont.RequiredWidth := 100;
    //cont.Blocks.AddParagraph();
    cont.BlockStyle.BottomPadding := 10;
    parser.Free;
  end;
end;

end.

