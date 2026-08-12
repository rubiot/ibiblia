unit ai_batch_work_item;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Verse;

type
  TVerseData = record
    ReferenceID: string;
    ReferenceName: string;
    OriginalText: string;
    TranslatedText: string;
    Associations: string;
    Comments: string;
    Status: Integer;
  end;

  TAIBatchWorkItem = class
  private
    FTokenizerVersion: Integer;
    FVerseData: TVerseData;
  public
    constructor Create(ATokenizerVersion: Integer; const AVerseData: TVerseData);
    procedure CreateTempVersePair(out Verse: TVerse; out VersePair: TVerse);
    property VerseData: TVerseData read FVerseData;
  end;

  TAIBatchWorkItemList = class(TList)
  private
    function GetItem(Index: Integer): TAIBatchWorkItem;
  public
    destructor Destroy; override;
    procedure AddItem(Item: TAIBatchWorkItem);
    function ExtractItem(Index: Integer): TAIBatchWorkItem;
    property Items[Index: Integer]: TAIBatchWorkItem read GetItem; default;
  end;

implementation

constructor TAIBatchWorkItem.Create(ATokenizerVersion: Integer; const AVerseData: TVerseData);
begin
  inherited Create;
  FTokenizerVersion := ATokenizerVersion;
  FVerseData := AVerseData;
end;

procedure TAIBatchWorkItem.CreateTempVersePair(out Verse: TVerse; out VersePair: TVerse);
begin
  Verse := TVerse.Create();
  VersePair := TVerse.Create();
  Verse.TokenizerVersion := FTokenizerVersion;
  VersePair.TokenizerVersion := FTokenizerVersion;
  Verse.Text := FVerseData.OriginalText;
  VersePair.Text := FVerseData.TranslatedText;
  Verse.Reference := FVerseData.ReferenceName;
  Verse.ReferenceID := FVerseData.ReferenceID;
  Verse.PairedVerse := VersePair;
end;

function TAIBatchWorkItemList.GetItem(Index: Integer): TAIBatchWorkItem;
begin
  Result := TAIBatchWorkItem(Get(Index));
end;

destructor TAIBatchWorkItemList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TAIBatchWorkItem(Get(I)).Free;
  inherited Destroy;
end;

procedure TAIBatchWorkItemList.AddItem(Item: TAIBatchWorkItem);
begin
  Add(Item);
end;

function TAIBatchWorkItemList.ExtractItem(Index: Integer): TAIBatchWorkItem;
begin
  Result := GetItem(Index);
  Delete(Index);
end;

end.
