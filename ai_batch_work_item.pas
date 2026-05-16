unit ai_batch_work_item;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, versiculo;

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
    FProject: TObject;
    FVerseData: TVerseData;
  public
    constructor Create(AProject: TObject; const AVerseData: TVerseData);
    procedure CreateTempVersePair(out Verse: TVersiculo; out VersePair: TVersiculo);
    property Project: TObject read FProject;
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

constructor TAIBatchWorkItem.Create(AProject: TObject; const AVerseData: TVerseData);
begin
  inherited Create;
  FProject := AProject;
  FVerseData := AVerseData;
end;

procedure TAIBatchWorkItem.CreateTempVersePair(out Verse: TVersiculo; out VersePair: TVersiculo);
begin
  if not Assigned(FProject) then
    raise Exception.CreateFmt('TAIBatchWorkItem.CreateTempVersePair: missing project for %s', [FVerseData.ReferenceID]);

  Verse := TVersiculo.Criar(FProject);
  VersePair := TVersiculo.Criar(FProject);
  Verse.Texto := FVerseData.OriginalText;
  VersePair.Texto := FVerseData.TranslatedText;
  Verse.Referencia := FVerseData.ReferenceName;
  Verse.ReferenciaID := FVerseData.ReferenceID;
  Verse.VersiculoPar := VersePair;
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
