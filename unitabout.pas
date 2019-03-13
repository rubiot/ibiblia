unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, fileinfo;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  FileVerInfo.ReadFileInfo;
  LabelVersion.Caption := FileVerInfo.VersionStrings.Values['FileVersion'];
  FileVerInfo.Free;
end;

initialization
  {$I unitabout.lrs}

end.

