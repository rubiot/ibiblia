unit PatchFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream, ComCtrls, Forms;

type

  { TPatchFile }

  TPatchFile = class
  private
    FReference: TStringList;
    FSrcText: TStringList;
    FDstText: TStringList;
    FPairs: TStringList;
    FComments: TStringList;
    FStatus: TStringList;
    FProject: TObject;
    FIgnoredVerses: integer;
    procedure LoadFromFile(patchfile: string);
    function FormatComments(comments: string): string;
  public
    constructor Create;
    constructor Create(patchFile: string; project: TObject); overload;
    destructor Destroy; override;
    procedure Add(ref, src, dst, pairs, comments: string; status: integer);
    procedure Save(patchfile: string);

    property Reference: TStringList read FReference;
    property SourceText: TStringList read FSrcText;
    property DestinationText: TStringList read FDstText;
    property Pairs: TStringList read FPairs;
    property Comments: TStringList read FComments;
    property Status: TStringList read FStatus;
    property IgnoredVerses: integer read FIgnoredVerses;
  end;

implementation

uses Projeto;

{ TPatchFile }

constructor TPatchFile.Create;
begin
  FReference := TStringList.Create();
  FSrcText   := TStringList.Create();
  FDstText   := TStringList.Create();
  FPairs     := TStringList.Create();
  FComments  := TStringList.Create();
  FStatus    := TStringList.Create();
end;

constructor TPatchFile.Create(patchFile: string; project: TObject);
begin
  Create;
  FProject := project;
  FIgnoredVerses := 0;
  LoadFromFile(patchFile);
end;

destructor TPatchFile.Destroy;
begin
  FReference.Free;
  FSrcText.Free;
  FDstText.Free;
  FPairs.Free;
  FComments.Free;
  FStatus.Free;
end;

procedure TPatchFile.Add(ref, src, dst, pairs, comments: string; status: integer
  );
begin
  FReference.Add(ref);
  FSrcText.Add(src);
  FDstText.Add(dst);
  FPairs.Add(pairs);
  FComments.Add(FormatComments(comments));
  FStatus.Add(status.ToString);
end;

procedure TPatchFile.LoadFromFile(patchfile: string);

  function VersesAreEqual(patch: TStringArray; project: TProjeto): boolean;
  begin
    result := false;
    try
      project.IrPara(patch[0]);
    except
      exit;
    end;
    if patch[1] <> project.ObterTextoVersiculo(tbOrigem) then exit;
    if patch[2] <> project.ObterTextoVersiculo(tbDestino) then exit;
    if patch[3] <> project.Pairs then exit;
    if patch[4] <> FormatComments(project.Comentarios) then exit;
    if patch[5].ToInteger <> project.Situacao then exit;
    result := true;
  end;
var
  decompressor: TDecompressionStream;
  patch: TMemoryStream;
  lines: TStringList;
  line: string;
  fields: TStringArray;
  project: TProjeto;
begin
  FReference.Clear;
  FSrcText.Clear;
  FDstText.Clear;
  FPairs.Clear;
  FComments.Clear;
  FStatus.Clear;

  project := TProjeto(FProject);
  project.StartScrollingSession;

  lines := TStringList.Create;
  patch := TMemoryStream.Create;
  try
    patch.LoadFromFile(PatchFile);
    decompressor := TDecompressionStream.Create(patch);
    lines.LoadFromStream(decompressor);

    for line in lines do
    begin
      fields := line.Split(#9);

      if assigned(project) and VersesAreEqual(fields, project) then
      begin
         inc(FIgnoredVerses);
         continue;
      end;

      FReference.Add(fields[0]);
      FSrcText.  Add(fields[1]);
      FDstText.  Add(fields[2]);
      FPairs.    Add(fields[3]);
      FComments. Add(fields[4].Replace('<crlf/>', #13#10).Replace('<lf/>', #10));
      FStatus.   Add(fields[5]);
    end;

  finally
    lines.Free;
    decompressor.Free;
    patch.Free;
    project.FinishScrollingSession;
  end;
end;

function TPatchFile.FormatComments(comments: string): string;
begin
  result := comments.Replace(#13#10, '<crlf/>').Replace(#10, '<lf/>')
end;

procedure TPatchFile.Save(patchfile: string);
var
  i: integer;
  patch: TStringList;
  zpatch: TMemoryStream; // compressed patch
  compressor: TCompressionStream;
begin
  patch := TStringList.Create;
  zpatch := TMemoryStream.Create;
  compressor := TCompressionStream.create(clMax, zpatch);

  try
    for i:=0 to FReference.Count-1 do
      patch.Add(Format('%s'#9'%s'#9'%s'#9'%s'#9'%s'#9'%s', [FReference[i], FSrcText[i], FDstText[i], FPairs[i], FComments[i], FStatus[i]]));

    patch.SaveToStream(compressor);
    compressor.flush;
    zpatch.SaveToFile(patchfile);
  finally
    compressor.Free;
    zpatch.Free;
    patch.Free;
  end;
end;

end.

