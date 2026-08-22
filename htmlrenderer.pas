unit HTMLRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, KMemo, KEditCommon, ONTTokenizer, HTMLColors,
  Graphics, Contnrs, Math, rf_note_scanner, theword_ont_codec_v2,
  theword_effective_inline_style_v2;

type
  { THTMLRenderer }
  
  THTMLRenderer = class(TKMemo)
  private
    FStyleStack: TObjectStack;
    FTokenizerVersion: Integer;
  protected
    function GetCurrentStyle: TKMemoTextStyle;
    function PushNewStyle: TKMemoTextStyle;
    function PushInheritedStyle: TKMemoTextStyle;
    procedure PopStyle;
    procedure ResetStyleStack;
    procedure SetupDefaultStyle; virtual;
    function ResolveInlineProjection(const ARaw: UnicodeString):
      TTheWordEffectiveInlineProjectionV2; virtual;
    function TryResolveInlineProjection(const ARaw: UnicodeString;
      out AProjection: TTheWordEffectiveInlineProjectionV2): Boolean;
    procedure DiscardInlineProjection(
      var AProjection: TTheWordEffectiveInlineProjectionV2;
      var ARunIndex: Integer);
    procedure AdvanceProjectionForConsumedRaw(const ARaw: UnicodeString;
      var AProjection: TTheWordEffectiveInlineProjectionV2;
      var AVisibleOffsetUTF16, ARunIndex: Integer);
    procedure ApplyProjectedInlineStyle(ABlock: TKMemoTextBlock;
      ABaseStyle: TKMemoTextStyle;
      const AEffective: TTheWordEffectiveInlineStyleV2); virtual;
    procedure RenderProjectedChunk(AContainer: TKMemoBlocks;
      const AChunk: string;
      AProjection: TTheWordEffectiveInlineProjectionV2;
      var AVisibleOffsetUTF16, ARunIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure RenderHTML(const HTMLText: string);
    procedure RenderSpan(cont: TKMemoBlocks; txt: string); virtual;
    
    property CurrentStyle: TKMemoTextStyle read GetCurrentStyle;
    property TokenizerVersion: Integer read FTokenizerVersion write FTokenizerVersion;
  end;

implementation

{ THTMLRenderer }

constructor THTMLRenderer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := true;
  BorderStyle := bsNone;
  FStyleStack := TObjectStack.Create;
  FTokenizerVersion := TTokenizerFactory.LATEST_VERSION;
end;

destructor THTMLRenderer.Destroy;
begin
  FStyleStack.Free;
  inherited Destroy;
end;

function THTMLRenderer.GetCurrentStyle: TKMemoTextStyle;
begin
  if FStyleStack.Count > 0 then
    result := TKMemoTextStyle(FStyleStack.Peek)
  else
    result := TextStyle;
end;

function THTMLRenderer.PushNewStyle: TKMemoTextStyle;
begin
  result := TKMemoTextStyle(FStyleStack.Push(TKMemoTextStyle.Create));
  result.Assign(TextStyle);
end;

function THTMLRenderer.PushInheritedStyle: TKMemoTextStyle;
var
  current: TKMemoTextStyle;
begin
  current := CurrentStyle;
  result := PushNewStyle;
  result.Assign(current);
end;

procedure THTMLRenderer.PopStyle;
begin
  if FStyleStack.Count > 0 then
    FStyleStack.Pop.Free;
end;

procedure THTMLRenderer.ResetStyleStack;
begin
  while FStyleStack.Count > 0 do
    PopStyle;
  SetupDefaultStyle;
  PushNewStyle; // pushing default style onto stack
end;

procedure THTMLRenderer.SetupDefaultStyle;
begin
  // Override in subclasses to set specific default styling
end;

function THTMLRenderer.ResolveInlineProjection(const ARaw: UnicodeString):
  TTheWordEffectiveInlineProjectionV2;
begin
  Result := TTheWordEffectiveInlineProjectionV2.Resolve(ARaw);
end;

function THTMLRenderer.TryResolveInlineProjection(const ARaw: UnicodeString;
  out AProjection: TTheWordEffectiveInlineProjectionV2): Boolean;
begin
  AProjection := nil;
  try
    AProjection := ResolveInlineProjection(ARaw);
    Result := True;
  except
    on ETheWordOntCodec do
      Result := False;
  end;
end;

procedure THTMLRenderer.DiscardInlineProjection(
  var AProjection: TTheWordEffectiveInlineProjectionV2;
  var ARunIndex: Integer);
begin
  FreeAndNil(AProjection);
  ARunIndex := 0;
end;

procedure THTMLRenderer.AdvanceProjectionForConsumedRaw(
  const ARaw: UnicodeString;
  var AProjection: TTheWordEffectiveInlineProjectionV2;
  var AVisibleOffsetUTF16, ARunIndex: Integer);
var
  ConsumedProjection: TTheWordEffectiveInlineProjectionV2;
begin
  if not Assigned(AProjection) then
    Exit;
  ConsumedProjection := nil;
  if not TryResolveInlineProjection(ARaw, ConsumedProjection) then
  begin
    DiscardInlineProjection(AProjection, ARunIndex);
    Exit;
  end;
  try
    Inc(AVisibleOffsetUTF16, Length(ConsumedProjection.VisibleText));
  finally
    ConsumedProjection.Free;
  end;
end;

procedure THTMLRenderer.ApplyProjectedInlineStyle(ABlock: TKMemoTextBlock;
  ABaseStyle: TKMemoTextStyle;
  const AEffective: TTheWordEffectiveInlineStyleV2);
var
  FontStyle: TFontStyles;
begin
  ABlock.TextStyle.Assign(ABaseStyle);
  FontStyle := ABlock.TextStyle.Font.Style;
  if tweifv2Bold in AEffective.Flags then
    Include(FontStyle, fsBold);
  if tweifv2Italic in AEffective.Flags then
    Include(FontStyle, fsItalic);
  if tweifv2Underline in AEffective.Flags then
    Include(FontStyle, fsUnderline);
  if tweifv2Strikeout in AEffective.Flags then
    Include(FontStyle, fsStrikeOut);
  ABlock.TextStyle.Font.Style := FontStyle;
  if AEffective.HasSize then
    ABlock.TextStyle.Font.Size := ABaseStyle.Font.Size +
      AEffective.FontSizeDeltaPoints;
  if AEffective.HasExplicitColor then
    ABlock.TextStyle.Font.Color := AEffective.ExplicitColor;
  if tweifv2Superscript in AEffective.Flags then
    ABlock.TextStyle.ScriptPosition := tpoSuperscript
  else if tweifv2Subscript in AEffective.Flags then
    ABlock.TextStyle.ScriptPosition := tpoSubscript;
end;

procedure THTMLRenderer.RenderProjectedChunk(AContainer: TKMemoBlocks;
  const AChunk: string;
  AProjection: TTheWordEffectiveInlineProjectionV2;
  var AVisibleOffsetUTF16, ARunIndex: Integer);
var
  BaseStyle: TKMemoTextStyle;
  Effective: TTheWordEffectiveInlineStyleV2;
  Run: TTheWordEffectiveInlineRunV2;
  DecodedChunk, Piece: UnicodeString;
  At, Take, RunEnd: Integer;
  Block: TKMemoTextBlock;
begin
  if AChunk = '' then
    Exit;
  DecodedChunk := UTF8Decode(AChunk);
  BaseStyle := TKMemoTextStyle.Create;
  try
    BaseStyle.Assign(CurrentStyle);
    At := 1;
    while At <= Length(DecodedChunk) do
    begin
      Effective := DefaultTheWordEffectiveInlineStyleV2;
      Take := Length(DecodedChunk) - At + 1;
      if Assigned(AProjection) then
      begin
        while ARunIndex < AProjection.RunCount do
        begin
          Run := AProjection.Runs[ARunIndex];
          RunEnd := Run.VisibleStartUTF16 + Run.VisibleLengthUTF16;
          if RunEnd > AVisibleOffsetUTF16 then
            Break;
          Inc(ARunIndex);
        end;
        if ARunIndex < AProjection.RunCount then
        begin
          Run := AProjection.Runs[ARunIndex];
          if Run.VisibleStartUTF16 > AVisibleOffsetUTF16 then
            Take := Min(Take, Run.VisibleStartUTF16 - AVisibleOffsetUTF16)
          else
          begin
            Effective := Run.Style;
            Take := Min(Take, Run.VisibleStartUTF16 +
              Run.VisibleLengthUTF16 - AVisibleOffsetUTF16);
          end;
        end;
      end;
      Piece := Copy(DecodedChunk, At, Take);
      Block := AContainer.AddTextBlock(UTF8Encode(Piece));
      ApplyProjectedInlineStyle(Block, BaseStyle, Effective);
      Inc(At, Take);
      Inc(AVisibleOffsetUTF16, Take);
    end;
  finally
    BaseStyle.Free;
  end;
end;

procedure THTMLRenderer.RenderHTML(const HTMLText: string);
begin
  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;
    RenderSpan(Blocks, HTMLText);
  finally
    Blocks.UnLockUpdate;
  end;
end;

procedure THTMLRenderer.RenderSpan(cont: TKMemoBlocks; txt: string);
var
  FTokenizer: TONTTokenizer;
  token: TSyntagmToken;
  chunk: string;
  OpeningTag, Body, ClosingTag: string;
  Projection: TTheWordEffectiveInlineProjectionV2;
  VisibleOffsetUTF16, RunIndex: Integer;
begin
  chunk := '';
  FTokenizer := nil;
  Projection := nil;
  VisibleOffsetUTF16 := 0;
  RunIndex := 0;
  try
    FTokenizer := TTokenizerFactory.CreateTokenizer(FTokenizerVersion, txt);
    TryResolveInlineProjection(UTF8Decode(txt), Projection);

    cont.LockUpdate;
    try
      while FTokenizer.ReadSyntagm(token) <> skNull do
    begin
      case token.Kind of
        skMetadata:
          ;
        skWhitespace:
          if token.Value <> '|' then
            chunk := chunk + token.Value;
        skPunctuation, skSyntagm:
          chunk := chunk + token.Value;
        skTag:
        begin
          if chunk.Length > 0 then
          begin
            RenderProjectedChunk(cont, chunk, Projection,
              VisibleOffsetUTF16, RunIndex);
            chunk := '';
          end;

          OpeningTag := token.Value;
          if IsRFOpeningTag(OpeningTag) then
          begin
            ReadRFNoteBody(FTokenizer, Body, ClosingTag);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + Body +
              ClosingTag), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<Q>' then
          begin
            Body := FTokenizer.ReadUntilExclusive('<q>');
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + Body + '<q>'),
              Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<E>' then
          begin
            Body := FTokenizer.ReadUntilExclusive('<e>');
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + Body + '<e>'),
              Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<T>' then
          begin
            Body := FTokenizer.ReadUntilExclusive('<t>');
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + Body + '<t>'),
              Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<X>' then
          begin
            Body := FTokenizer.ReadUntilExclusive('<x>');
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + Body + '<x>'),
              Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<H>' then
          begin
            Body := FTokenizer.ReadUntilExclusive('<h>');
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + Body + '<h>'),
              Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<G>' then
          begin
            Body := FTokenizer.ReadUntilExclusive('<g>');
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + Body + '<g>'),
              Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if LowerCase(OpeningTag) = '<br/>' then
            cont.AddParagraph().ParaStyle.FirstIndent := 0;
        end;
      end;
    end;

    if chunk.Length > 0 then
      RenderProjectedChunk(cont, chunk, Projection,
        VisibleOffsetUTF16, RunIndex);
    finally
      cont.UnLockUpdate;
    end;
  finally
    FreeAndNil(FTokenizer);
    FreeAndNil(Projection);
  end;
end;

{$IFDEF FPC}
initialization
  // Component registration not needed for FPC/Lazarus runtime
{$ENDIF}

end.
