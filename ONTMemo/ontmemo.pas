unit ONTMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, KMemo, Graphics;

type

  { TONTBlock }

  TONTBlock = class(TKMemoTextBlock)
  public
    constructor Create(AText: string);
  end;

  { TFIBlock }

  TFIBlock = class(TONTBlock)
  public
    constructor Create(AText: string);// override;
  end;

  { TFRBlock }

  TFRBlock = class(TONTBlock)
  public
    constructor Create(AText: string);// override;
  end;

  { TFOBlock }

  TFOBlock = class(TONTBlock)
  public
    constructor Create(AText: string);// override;
  end;

  { TONTMemo }

  TONTMemo = class(TKCustomMemo)
  private
    FONTText: string;

    procedure SetONTText(AValue: string);
  public
    function AddTextBlock(const AText: string; At: TKMemoBlockIndex = -1): TONTBlock;
    function AddFIBlock(const AText: string; At: TKMemoBlockIndex = -1): TFIBlock;
    function AddFRBlock(const AText: string; At: TKMemoBlockIndex = -1): TFRBlock;
    function AddFOBlock(const AText: string; At: TKMemoBlockIndex = -1): TFOBlock;
    procedure AddCMBlock(At: TKMemoBlockIndex = -1);

    property ONT: string read FONTText write SetONTText;
  end;

implementation

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
begin
  if FONTText = AValue then Exit;
  FONTText := AValue;
  { TODO: Parse ont text and create blocks }
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

procedure TONTMemo.AddCMBlock(At: TKMemoBlockIndex);
begin
  Blocks.AddParagraph(At);
end;

end.

